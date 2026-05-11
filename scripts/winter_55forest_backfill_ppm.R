# Backfill numeric ppm/organic_matter on the 85 Winter 55 Forest rows that were
# pushed to prod with categorical descriptors only (ids 425-509, created_by
# wintereric22). Uses Bugbee 2026 CAES Morgan bin-anchor values:
#   NO3-N High        = 25   ppm
#   NH4-N Very Low    = 6    ppm
#   P     High        = 100  ppm
#   K     Low         = 60   ppm
#   Ca    High        = 1600 ppm
#   Mg    Medium      = 25   ppm (Rear Shade)
#   Mg    Medium High = 50   ppm (Front Island)
#   Mg    High        = 125  ppm (Living Room)
#   OM    High        = 13.0 % (midpoint of 11-15)
#
# Also rewrites notes to remove now-redundant categorical strings; keeps zone
# name + CAES provenance.
#
# Dry-run by default. Set env APPLY=yes to actually write.

suppressPackageStartupMessages({
  library(DBI); library(RPostgres)
})

if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

ERIC_UID <- "7b9df450-79de-48e9-9960-c9627422b6c2"
DRY_RUN  <- !identical(tolower(Sys.getenv("APPLY")), "yes")

# Per-zone payload: notes prefix to match on + new ppm values + cleaned notes.
zones <- list(
  list(
    match_prefix = "Front Island Garden",
    sample_id    = 550,
    mg_ppm       = 50,
    cleaned_note = "Front Island Garden (redbud bed). Soil tested by CAES (Morgan method, sample 550)."
  ),
  list(
    match_prefix = "Living-Room Garden",
    sample_id    = 548,
    mg_ppm       = 125,
    cleaned_note = "Living-Room Garden. Soil tested by CAES (Morgan method, sample 548)."
  ),
  list(
    match_prefix = "Rear Shade Garden",
    sample_id    = 547,
    mg_ppm       = 25,
    cleaned_note = "Rear Shade Garden. Soil tested by CAES (Morgan method, sample 547)."
  )
)

# Shared ppm values across all zones (the only variable is Mg).
SHARED <- list(
  nitrate_ppm    = 25,
  ammonium_ppm   = 6,
  phosphorus_ppm = 100,
  potassium_ppm  = 60,
  calcium_ppm    = 1600,
  organic_matter = 13.0
)

con <- dbConnect(Postgres(),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname   = Sys.getenv("POSTGRES_DB"),
  user     = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", "require")
)
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

cat("DB host:", Sys.getenv("POSTGRES_HOST"), "\n")
cat("Mode:   ", if (DRY_RUN) "DRY RUN (no writes)" else "APPLY (writes will commit)", "\n\n")

# ---- Pre-flight: confirm the 85 rows look like we expect --------------------
pre <- dbGetQuery(con, "
  SELECT split_part(notes, '.', 1) AS zone, COUNT(*)::int AS n,
         MIN(id) AS min_id, MAX(id) AS max_id,
         COUNT(nitrate_ppm)::int    AS n_nitrate_set,
         COUNT(phosphorus_ppm)::int AS n_p_set,
         COUNT(organic_matter)::int AS n_om_set
  FROM soil_samples
  WHERE created_by = $1
  GROUP BY 1 ORDER BY 1", params = list(ERIC_UID))
cat("=== Pre-flight state by zone ===\n"); print(pre); cat("\n")

total_pre <- sum(pre$n)
if (total_pre != 85) {
  stop(sprintf("Expected 85 rows for Eric, found %d. Aborting.", total_pre))
}

# Sanity: ppm columns should be NULL on all rows (push script never set them).
if (any(pre$n_nitrate_set > 0) || any(pre$n_p_set > 0) || any(pre$n_om_set > 0)) {
  cat("WARNING: some ppm/OM columns already populated. Update guards on _ppm IS NULL.\n\n")
}

# ---- Build UPDATE per zone -------------------------------------------------
UPDATE_SQL <- "
  UPDATE soil_samples
  SET nitrate_ppm    = $1,
      ammonium_ppm   = $2,
      phosphorus_ppm = $3,
      potassium_ppm  = $4,
      calcium_ppm    = $5,
      magnesium_ppm  = $6,
      organic_matter = $7,
      notes          = $8
  WHERE created_by = $9
    AND notes LIKE $10
    AND nitrate_ppm    IS NULL
    AND phosphorus_ppm IS NULL
    AND potassium_ppm  IS NULL
    AND calcium_ppm    IS NULL
    AND magnesium_ppm  IS NULL
"

run_zone <- function(z) {
  affected <- dbExecute(con, UPDATE_SQL, params = list(
    SHARED$nitrate_ppm, SHARED$ammonium_ppm, SHARED$phosphorus_ppm,
    SHARED$potassium_ppm, SHARED$calcium_ppm, z$mg_ppm,
    SHARED$organic_matter, z$cleaned_note,
    ERIC_UID, paste0(z$match_prefix, "%")
  ))
  cat(sprintf("  %-30s  Mg=%-4d  rows updated: %d\n",
              z$match_prefix, z$mg_ppm, affected))
  affected
}

if (DRY_RUN) {
  cat("Dry run — would issue 3 UPDATEs:\n")
  for (z in zones) {
    expected <- pre$n[grepl(paste0("^", z$match_prefix), pre$zone)]
    cat(sprintf("  %-30s  Mg=%-4d  would update ~%d rows  (notes -> '%s')\n",
                z$match_prefix, z$mg_ppm, ifelse(length(expected) == 1, expected, NA),
                z$cleaned_note))
  }
  cat("\n*** DRY RUN — set env APPLY=yes to actually update. ***\n")
  quit(status = 0)
}

# ---- Apply in a transaction ------------------------------------------------
dbBegin(con)
ok <- tryCatch({
  total_updated <- 0L
  cat("Applying updates:\n")
  for (z in zones) total_updated <- total_updated + run_zone(z)
  cat(sprintf("\nTotal rows updated: %d\n", total_updated))
  if (total_updated != 85) {
    stop(sprintf("Expected to update 85 rows, updated %d. Rolling back.",
                 total_updated))
  }
  TRUE
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  FALSE
})

if (ok) {
  dbCommit(con)
  cat("Committed.\n\n")
} else {
  dbRollback(con)
  cat("Rolled back. No rows changed.\n")
  quit(status = 1)
}

# ---- Post-verify -----------------------------------------------------------
cat("=== Post-update state by zone ===\n")
print(dbGetQuery(con, "
  SELECT split_part(notes, '.', 1) AS zone, COUNT(*)::int AS n,
         AVG(nitrate_ppm)::numeric(8,2)    AS no3,
         AVG(ammonium_ppm)::numeric(8,2)   AS nh4,
         AVG(phosphorus_ppm)::numeric(8,2) AS p,
         AVG(potassium_ppm)::numeric(8,2)  AS k,
         AVG(calcium_ppm)::numeric(8,2)    AS ca,
         AVG(magnesium_ppm)::numeric(8,2)  AS mg,
         AVG(organic_matter)::numeric(5,2) AS om
  FROM soil_samples
  WHERE created_by = $1
  GROUP BY 1 ORDER BY 1", params = list(ERIC_UID)))

cat("\n=== Sample row (post-update) ===\n")
print(dbGetQuery(con, "
  SELECT id, species, ph, organic_matter,
         nitrate_ppm, ammonium_ppm, phosphorus_ppm, potassium_ppm,
         calcium_ppm, magnesium_ppm, notes
  FROM soil_samples WHERE created_by = $1 ORDER BY id LIMIT 3",
  params = list(ERIC_UID)))
