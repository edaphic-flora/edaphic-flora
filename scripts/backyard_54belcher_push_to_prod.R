# Push 54 Belcher backyard soil samples (3 zones, 35 plants) to prod soil_samples
# under todd@edaphicgardenconsulting.com (Polished user_uid 3882df3d-cf87-433a-9238-258b3a6b8742).
# CAES Morgan-method report 2023-07-11 — older format that includes inline ppm values
# next to each categorical descriptor, so numeric nutrient columns are populated.
# Single transaction, rolls back on any error. Set APPLY=yes to commit.

suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(dplyr)
})

if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

TODD_UID    <- "3882df3d-cf87-433a-9238-258b3a6b8742"
SAMPLE_DATE <- as.Date("2023-07-11")
WETHERSFIELD_LAT <- 41.7136   # 54 Belcher Rd, Wethersfield, CT 06109
WETHERSFIELD_LNG <- -72.6650
DRY_RUN <- !identical(tolower(Sys.getenv("APPLY")), "yes")

# ---- Per-sample soil context (CAES Morgan, 2023-07-11) --------------------
# All three samples: Sandy Loam, OM Medium High, all numeric ppm from lab report
soil_514 <- list(
  zone = "Back garage low (rain garden + adjacent)", sample_id = 514,
  ph = 5.1, om_class = "Medium High", om_pct = 9.5, texture_class = "Sandy Loam",
  nitrate_ppm = 1,   nitrate_class   = "Very Low",
  ammonium_ppm = 12, ammonium_class  = "Low",
  phosphorus_ppm = 19, phosphorus_class = "Medium Low",
  potassium_ppm = 120, potassium_class  = "Medium",
  calcium_ppm = 1200, calcium_class    = "Medium High",
  magnesium_ppm = 25, magnesium_class  = "Medium"
)
soil_515 <- list(
  zone = "Fence line high", sample_id = 515,
  ph = 5.0, om_class = "Medium High", om_pct = 9.5, texture_class = "Sandy Loam",
  nitrate_ppm = 1,   nitrate_class   = "Very Low",
  ammonium_ppm = 6,  ammonium_class  = "Very Low",
  phosphorus_ppm = 25, phosphorus_class = "Medium",
  potassium_ppm = 120, potassium_class  = "Medium",
  calcium_ppm = 1200, calcium_class    = "Medium High",
  magnesium_ppm = 25, magnesium_class  = "Medium"
)
soil_516 <- list(
  zone = "Front garage low", sample_id = 516,
  ph = 5.1, om_class = "Medium High", om_pct = 9.5, texture_class = "Sandy Loam",
  nitrate_ppm = 6,   nitrate_class   = "Medium",
  ammonium_ppm = 24, ammonium_class  = "Medium Low",
  phosphorus_ppm = 25, phosphorus_class = "Medium",
  potassium_ppm = 180, potassium_class  = "Medium High",
  calcium_ppm = 1200, calcium_class    = "Medium High",
  magnesium_ppm = 25, magnesium_class  = "Medium"
)

# ---- Per-zone plant lists --------------------------------------------------
make_note <- function(s, extra = NULL) {
  base <- sprintf("%s (CAES sample %d, 11 Jul 2023). pH %.1f, %s, OM %s. Nutrients: NO3 %s (%g ppm), NH4 %s (%g ppm), P %s (%g ppm), K %s (%g ppm), Ca %s (%g ppm), Mg %s (%g ppm).",
                  s$zone, s$sample_id, s$ph, s$texture_class, s$om_class,
                  s$nitrate_class, s$nitrate_ppm,
                  s$ammonium_class, s$ammonium_ppm,
                  s$phosphorus_class, s$phosphorus_ppm,
                  s$potassium_class, s$potassium_ppm,
                  s$calcium_class, s$calcium_ppm,
                  s$magnesium_class, s$magnesium_ppm)
  if (!is.null(extra) && !is.na(extra) && nzchar(extra)) base <- paste0(base, " ", extra)
  base
}

# Sample 516 — Front garage low — 10 plants, all Full Sun, Mesic, Thriving
plants_516 <- tibble::tribble(
  ~species,                    ~cultivar,    ~outcome,    ~sun_exposure, ~site_hydrology, ~extra_note,
  "Echinacea purpurea",        NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Clethra alnifolia",         "Ruby Spice", "Thriving",  "Full Sun",    "Mesic",          NA,
  "Allium cernuum",            NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Penstemon digitalis",       NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Pycnanthemum tenuifolium",  NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Coreopsis rosea",           NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Fragaria virginiana",       NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Liatris spicata",           NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Schizachyrium scoparium",   NA,           "Thriving",  "Full Sun",    "Mesic",          NA,
  "Glechoma hederacea",        NA,           "Thriving",  "Full Sun",    "Mesic",          "Volunteer weed in this bed (not deliberately planted)."
) |> mutate(soil_idx = "516")

# Sample 514 — Back garage low — 8 rain garden + 2 outside RG
plants_514 <- tibble::tribble(
  ~species,                       ~cultivar,      ~outcome,     ~sun_exposure, ~site_hydrology, ~extra_note,
  "Hibiscus moscheutos",          NA,             "Thriving",   "Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Vernonia noveboracensis",      NA,             "Thriving",   "Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Eutrochium maculatum",         NA,             "Thriving",   "Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Cephalanthus occidentalis",    NA,             "Thriving",   "Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Gentiana andrewsii",           NA,             "Thriving",   "Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Asclepias incarnata",          NA,             "Thriving",   "Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Carex muskingumensis",         "Little Midge", "Established","Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Chelone glabra",               NA,             "Struggling", "Full Sun",    "Mesic",          "Rain garden — periodically inundated.",
  "Solidago speciosa",            NA,             "Thriving",   "Full Sun",    "Mesic",          "Adjacent to rain garden but outside the inundation zone.",
  "Symphyotrichum novae-angliae", NA,             "Thriving",   "Full Sun",    "Mesic",          "Adjacent to rain garden but outside the inundation zone."
) |> mutate(soil_idx = "514")

# Sample 515 — Fence line high — 15 plants
plants_515 <- tibble::tribble(
  ~species,                       ~cultivar,            ~outcome,      ~sun_exposure, ~site_hydrology, ~extra_note,
  "Aronia melanocarpa",           NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Aronia arbutifolia",           NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Amelanchier laevis",           NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Solidago rugosa",              "Fireworks",          "Thriving",    "Full Sun",    "Mesic",          NA,
  "Chionanthus virginicus",       NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Monarda fistulosa",            "Claire Grace",       "Thriving",    "Full Sun",    "Mesic",          NA,
  "Rhus typhina",                 "Tiger Eyes",         "Thriving",    "Full Sun",    "Mesic",          NA,
  "Antennaria neglecta",          NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Symphyotrichum oblongifolium", "Raydon's Favorite",  "Thriving",    "Full Sun",    "Mesic",          NA,
  "Ratibida pinnata",             NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Veronicastrum virginicum",     NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Amsonia tabernaemontana",      NA,                   "Thriving",    "Full Sun",    "Mesic",          NA,
  "Cercis canadensis",            NA,                   "Failed/Died", "Full Sun",    "Mesic",          "Did not establish.",
  "Rudbeckia deamii",             NA,                   "Thriving",    "Part Sun",    "Mesic",          NA,
  "Echinacea pallida",            NA,                   "Established", "Part Sun",    "Mesic",          NA
) |> mutate(soil_idx = "515")

# ---- Build full row payload -----------------------------------------------
soil_lookup <- list("514" = soil_514, "515" = soil_515, "516" = soil_516)

build_zone <- function(plant_df) {
  s <- soil_lookup[[plant_df$soil_idx[1]]]
  plant_df |>
    rowwise() |>
    mutate(
      ph                   = s$ph,
      organic_matter       = s$om_pct,
      texture_class        = s$texture_class,
      nitrate_ppm          = s$nitrate_ppm,
      ammonium_ppm         = s$ammonium_ppm,
      phosphorus_ppm       = s$phosphorus_ppm,
      potassium_ppm        = s$potassium_ppm,
      calcium_ppm          = s$calcium_ppm,
      magnesium_ppm        = s$magnesium_ppm,
      date                 = SAMPLE_DATE,
      location_lat         = WETHERSFIELD_LAT,
      location_long        = WETHERSFIELD_LNG,
      created_by           = TODD_UID,
      notes                = make_note(s, extra_note)
    ) |> ungroup() |>
    select(species, cultivar, outcome, sun_exposure, site_hydrology,
           ph, organic_matter, texture_class,
           nitrate_ppm, ammonium_ppm, phosphorus_ppm, potassium_ppm,
           calcium_ppm, magnesium_ppm, date, location_lat, location_long,
           created_by, notes)
}

all_rows <- bind_rows(
  build_zone(plants_516),
  build_zone(plants_514),
  build_zone(plants_515)
)
cat(sprintf("Built %d rows total (516: %d, 514: %d, 515: %d)\n",
            nrow(all_rows), nrow(plants_516), nrow(plants_514), nrow(plants_515)))
cat("\nOutcome breakdown:\n"); print(table(all_rows$outcome))
cat("\nSun breakdown:\n");      print(table(all_rows$sun_exposure))
cat("\nFirst row preview:\n");  print(as.list(all_rows[1, ]))

# ---- Connect & sanity ------------------------------------------------------
con <- dbConnect(Postgres(),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname   = Sys.getenv("POSTGRES_DB"),
  user     = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", "require")
)
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
cat("\nDB host:", Sys.getenv("POSTGRES_HOST"), "  user:",
    Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")), "\n")
before <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM soil_samples WHERE created_by = $1",
                     params = list(TODD_UID))
cat("Existing rows for Todd:", before$n, "\n")

if (DRY_RUN) {
  cat("\n*** DRY RUN — set env APPLY=yes to actually insert. No rows written. ***\n")
  quit(status = 0)
}

# ---- Insert in a transaction ----------------------------------------------
INSERT_SQL <- "
  INSERT INTO soil_samples (
    species, cultivar, outcome, sun_exposure, site_hydrology,
    ph, organic_matter, texture_class,
    nitrate_ppm, ammonium_ppm, phosphorus_ppm, potassium_ppm,
    calcium_ppm, magnesium_ppm, date,
    location_lat, location_long, created_by, notes
  ) VALUES (
    $1, $2, $3, $4, $5,
    $6, $7, $8,
    $9, $10, $11, $12,
    $13, $14, $15,
    $16, $17, $18, $19
  )
  RETURNING id
"

dbBegin(con)
inserted_ids <- integer(0)
ok <- tryCatch({
  for (i in seq_len(nrow(all_rows))) {
    r <- all_rows[i, ]
    res <- dbGetQuery(con, INSERT_SQL, params = list(
      r$species, r$cultivar, r$outcome, r$sun_exposure, r$site_hydrology,
      r$ph, r$organic_matter, r$texture_class,
      r$nitrate_ppm, r$ammonium_ppm, r$phosphorus_ppm, r$potassium_ppm,
      r$calcium_ppm, r$magnesium_ppm, r$date,
      r$location_lat, r$location_long, r$created_by, r$notes
    ))
    inserted_ids <- c(inserted_ids, res$id)
  }
  TRUE
}, error = function(e) {
  cat("ERROR during insert:", conditionMessage(e), "\n"); FALSE
})

if (ok) {
  dbCommit(con)
  cat(sprintf("\nCommitted. Inserted %d rows. ID range: %d..%d\n",
              length(inserted_ids), min(inserted_ids), max(inserted_ids)))
  after <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM soil_samples WHERE created_by = $1",
                      params = list(TODD_UID))
  cat("Todd now has", after$n, "rows in soil_samples (was", before$n, ").\n")
} else {
  dbRollback(con)
  cat("\nRolled back. No rows committed.\n")
  quit(status = 1)
}
