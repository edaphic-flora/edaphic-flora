# Backfill ecoregion_l4 / ecoregion_l4_code for any soil_samples rows that have
# lat/long but are missing ecoregion. Uses the precomputed grid lookup
# (app/data/ecoregion_grid.rds) — same path the prod app uses, no sf required.
#
# Dry-run by default. APPLY=yes to commit.

suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

DRY_RUN <- !identical(tolower(Sys.getenv("APPLY")), "yes")

GRID_PATH <- "app/data/ecoregion_grid.rds"
RESOLUTION <- 0.1
TOLERANCE  <- 0.001

grid <- readRDS(GRID_PATH)
stopifnot(all(c("lat","lon","ecoregion_name","ecoregion_code") %in% colnames(grid)))
cat("Loaded grid:", nrow(grid), "cells\n\n")

lookup_one <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return(list(name = NA_character_, code = NA_character_))
  grid_lat <- round(lat / RESOLUTION) * RESOLUTION
  grid_lon <- round(lon / RESOLUTION) * RESOLUTION
  idx <- which(abs(grid$lat - grid_lat) < TOLERANCE &
               abs(grid$lon - grid_lon) < TOLERANCE)
  if (length(idx) > 0) list(name = grid$ecoregion_name[idx[1]], code = grid$ecoregion_code[idx[1]])
  else list(name = NA_character_, code = NA_character_)
}

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
cat("Mode:   ", if (DRY_RUN) "DRY RUN" else "APPLY", "\n\n")

rows <- dbGetQuery(con, "
  SELECT id, location_lat::double precision AS lat,
             location_long::double precision AS lon
  FROM soil_samples
  WHERE location_lat IS NOT NULL
    AND location_long IS NOT NULL
    AND ecoregion_l4 IS NULL
  ORDER BY id")

cat("=== Rows needing backfill:", nrow(rows), "===\n")
if (nrow(rows) == 0) {
  cat("Nothing to do.\n"); quit(status = 0)
}

resolved <- t(mapply(lookup_one, rows$lat, rows$lon))
rows$eco_name <- unlist(resolved[, "name"])
rows$eco_code <- unlist(resolved[, "code"])

cat("\n=== Resolved distinct (lat, lon) -> ecoregion ===\n")
agg <- aggregate(id ~ lat + lon + eco_name + eco_code, data = rows, FUN = length)
colnames(agg)[ncol(agg)] <- "n_rows"
print(agg)

n_unresolved <- sum(is.na(rows$eco_name))
if (n_unresolved > 0) {
  cat(sprintf("\nWARNING: %d rows resolved to NA (no grid cell match — may be off-CONUS)\n",
              n_unresolved))
}

if (DRY_RUN) {
  cat("\n*** DRY RUN — set env APPLY=yes to write. ***\n")
  quit(status = 0)
}

dbBegin(con)
ok <- tryCatch({
  updated <- 0L
  for (i in seq_len(nrow(rows))) {
    if (is.na(rows$eco_name[i])) next
    n <- dbExecute(con, "
      UPDATE soil_samples
      SET ecoregion_l4 = $1, ecoregion_l4_code = $2
      WHERE id = $3 AND ecoregion_l4 IS NULL",
      params = list(rows$eco_name[i], rows$eco_code[i], rows$id[i]))
    updated <- updated + n
  }
  cat(sprintf("\nUpdated %d rows\n", updated))
  TRUE
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n"); FALSE
})

if (ok) {
  dbCommit(con); cat("Committed.\n\n")
} else {
  dbRollback(con); cat("Rolled back.\n"); quit(status = 1)
}

cat("=== Post-backfill site-wide ecoregion summary ===\n")
print(dbGetQuery(con, "
  SELECT ecoregion_l4, ecoregion_l4_code, COUNT(*)::int AS n,
         COUNT(DISTINCT created_by)::int AS users
  FROM soil_samples
  WHERE (flagged IS NULL OR flagged = FALSE)
  GROUP BY 1,2 ORDER BY n DESC"))
