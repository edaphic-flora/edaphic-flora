# Push Winter 55 Forest Ave soil samples (3 zones, 85 plants) to prod soil_samples
# table under wintereric22@gmail.com (Polished user_uid 7b9df450-79de-48e9-9960-c9627422b6c2).
# Wraps every insert in a single transaction; rolls back on any error.

suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(readr); library(dplyr); library(stringr)
})

if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

# ---- Constants -------------------------------------------------------------
ERIC_UID       <- "7b9df450-79de-48e9-9960-c9627422b6c2"
SAMPLE_DATE    <- as.Date("2026-05-05")
WYOMING_OH_LAT <- 39.2342
WYOMING_OH_LNG <- -84.4777
DRY_RUN        <- !identical(tolower(Sys.getenv("APPLY")), "yes")  # gate writes

upload_dir <- "C:/Users/toddt/OneDrive/Desktop/edaphic consulting/01_Clients/Winter_Eric_Wyoming_2026/edaphic_uploads"

# Per-zone soil context (CAES Morgan method, 5 May 2026)
zones <- list(
  list(
    csv          = file.path(upload_dir, "sample_550_front_island.csv"),
    sample_id    = 550, zone_name = "Front Island Garden (redbud bed)",
    ph           = 7.4, om_class = "High", texture_class = "Organic",
    nutrients    = "NO3 High, NH4 Very Low, P High, K Low, Ca High, Mg Medium High"
  ),
  list(
    csv          = file.path(upload_dir, "sample_548_living_room.csv"),
    sample_id    = 548, zone_name = "Living-Room Garden",
    ph           = 7.4, om_class = "High", texture_class = "Organic",
    nutrients    = "NO3 High, NH4 Very Low, P High, K Low, Ca High, Mg High"
  ),
  list(
    csv          = file.path(upload_dir, "sample_547_rear_shade.csv"),
    sample_id    = 547, zone_name = "Rear Shade Garden",
    ph           = 7.3, om_class = "High", texture_class = "Organic",
    nutrients    = "NO3 High, NH4 Very Low, P High, K Low, Ca High, Mg Medium"
  )
)

# ---- Build full insert payload ---------------------------------------------
all_rows <- bind_rows(lapply(zones, function(z) {
  csv <- read_csv(z$csv, show_col_types = FALSE, na = c("", "NA"))
  csv |>
    transmute(
      species, cultivar, outcome, sun_exposure, site_hydrology,
      ph             = z$ph,
      organic_matter_class = z$om_class,
      texture_class  = z$texture_class,
      date           = SAMPLE_DATE,
      location_lat   = WYOMING_OH_LAT,
      location_long  = WYOMING_OH_LNG,
      created_by     = ERIC_UID,
      notes          = sprintf(
        "%s. Soil tested by CAES (Morgan method, sample %d, 5 May 2026). Categorical nutrients: %s. pH %.1f, organic matter %s, texture %s.",
        z$zone_name, z$sample_id, z$nutrients, z$ph, z$om_class, z$texture_class
      )
    )
}))

cat(sprintf("Built %d insert rows across %d zones\n", nrow(all_rows), length(zones)))
cat("By zone:\n")
print(all_rows |> count(notes |> str_extract("^[^.]+\\.")) |> rename(zone = 1))
cat("\nFirst row preview:\n")
print(as.list(all_rows[1, ]))

# ---- Connect ---------------------------------------------------------------
con <- dbConnect(Postgres(),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname   = Sys.getenv("POSTGRES_DB"),
  user     = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", "require")
)
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

# Prove we're against the expected DB
cat("\nDB host:", Sys.getenv("POSTGRES_HOST"), "  user:",
    Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")), "\n")
before <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM soil_samples WHERE created_by = $1",
                     params = list(ERIC_UID))
cat("Existing rows for Eric:", before$n, "\n")

if (DRY_RUN) {
  cat("\n*** DRY RUN — set env APPLY=yes to actually insert. No rows written. ***\n")
  quit(status = 0)
}

# ---- Insert in a transaction ----------------------------------------------
INSERT_SQL <- "
  INSERT INTO soil_samples (
    species, cultivar, outcome, sun_exposure, site_hydrology,
    ph, organic_matter_class, texture_class, date,
    location_lat, location_long, created_by, notes
  ) VALUES (
    $1, $2, $3, $4, $5,
    $6, $7, $8, $9,
    $10, $11, $12, $13
  )
  RETURNING id
"

dbBegin(con)
inserted_ids <- integer(0)
ok <- tryCatch({
  for (i in seq_len(nrow(all_rows))) {
    r <- all_rows[i, ]
    res <- dbGetQuery(con, INSERT_SQL, params = list(
      r$species, r$cultivar %||% NA, r$outcome, r$sun_exposure, r$site_hydrology,
      r$ph, r$organic_matter_class, r$texture_class, r$date,
      r$location_lat, r$location_long, r$created_by, r$notes
    ))
    inserted_ids <- c(inserted_ids, res$id)
  }
  TRUE
}, error = function(e) {
  cat("ERROR during insert:", conditionMessage(e), "\n")
  FALSE
})

if (ok) {
  dbCommit(con)
  cat(sprintf("\nCommitted. Inserted %d rows. ID range: %d..%d\n",
              length(inserted_ids), min(inserted_ids), max(inserted_ids)))
  after <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM soil_samples WHERE created_by = $1",
                      params = list(ERIC_UID))
  cat("Eric now has", after$n, "rows in soil_samples (was", before$n, ").\n")
} else {
  dbRollback(con)
  cat("\nRolled back. No rows committed.\n")
  quit(status = 1)
}
