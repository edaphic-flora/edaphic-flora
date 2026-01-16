# R/db.R - Database connection and helper functions

library(DBI)
library(RPostgres)
library(pool)

# ---------------------------
# Database Connection
# ---------------------------

pool <- dbPool(
  drv      = Postgres(),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname   = Sys.getenv("POSTGRES_DB"),
  user     = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", unset = "require"),
  minSize  = 1,
  maxSize  = 25
)

# ---------------------------
# Schema Migration (idempotent)
# ---------------------------

db_migrate <- function() {
  tryCatch({
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS soil_samples (
        id SERIAL PRIMARY KEY,
        species VARCHAR(255),
        cultivar VARCHAR(255),
        ph NUMERIC(4,2),
        organic_matter NUMERIC(5,2),
        nitrate_ppm NUMERIC,
        ammonium_ppm NUMERIC,
        phosphorus_ppm NUMERIC,
        potassium_ppm NUMERIC,
        calcium_ppm NUMERIC,
        magnesium_ppm NUMERIC,
        soluble_salts_ppm NUMERIC,
        texture_sand NUMERIC(5,2),
        texture_silt NUMERIC(5,2),
        texture_clay NUMERIC(5,2),
        texture_class VARCHAR(50),
        location_lat NUMERIC(10,6),
        location_long NUMERIC(10,6),
        date DATE,
        ecoregion_l4 VARCHAR(255),
        ecoregion_l4_code VARCHAR(50),
        notes TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")

    # Add columns (safe if already exist)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS created_by TEXT")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS photo_url TEXT")

    # Indices
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_species ON soil_samples(species)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_date ON soil_samples(date)")

    TRUE
  }, error = function(e) {
    message("DB migration error: ", e$message)
    FALSE
  })
}

# ---------------------------
# Query Functions
# ---------------------------

db_get_all_samples <- function() {
  tryCatch({
    dbGetQuery(pool, "SELECT * FROM soil_samples ORDER BY created_at DESC")
  }, error = function(e) {
    message("Error fetching samples: ", e$message)
    data.frame()
  })
}

db_get_species_data <- function(species) {
  if (is.null(species) || !nzchar(trimws(species))) return(data.frame())
  tryCatch({
    dbGetQuery(pool, "SELECT * FROM soil_samples WHERE species = $1 ORDER BY created_at DESC",
               params = list(species))
  }, error = function(e) {
    message("Error fetching species data: ", e$message)
    data.frame()
  })
}

db_get_unique_species <- function() {
  tryCatch({
    res <- dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")
    res$species
  }, error = function(e) {
    message("Error fetching unique species: ", e$message)
    character()
  })
}

db_add_sample <- function(sample_data) {
  tryCatch({
    if ("date" %in% names(sample_data)) {
      sample_data$date <- as.character(as.Date(sample_data$date))
    }
    fields <- names(sample_data)
    values <- as.list(unname(sample_data))
    placeholders <- paste0("$", seq_along(fields))
    sql <- sprintf("INSERT INTO soil_samples (%s) VALUES (%s) RETURNING id",
                   paste(fields, collapse = ", "), paste(placeholders, collapse = ", "))
    ret <- dbGetQuery(pool, sql, params = values)
    ret$id[1]
  }, error = function(e) {
    message("db_add_sample error: ", e$message)
    NULL
  })
}
