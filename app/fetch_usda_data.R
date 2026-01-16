# =============================================================================
# USDA Reference Data Fetcher - Standalone Script
# =============================================================================
# Run this script to fetch USDA plant characteristics for species in your database.
#
# Usage:
#   1. Make sure your .Renviron is loaded (restart R if you just added it)
#   2. Set working directory to app folder: setwd("app")
#   3. Source this script: source("fetch_usda_data.R")
#   4. Run: fetch_usda_batch(limit = 100)  # Fetch 100 species
#
# The script will:
#   - Find species in ref_taxon that are missing USDA characteristics
#   - Fetch data from USDA PLANTS API
#   - Cache responses locally (won't re-fetch already cached)
#   - Store characteristics in database
# =============================================================================

library(httr2)
library(jsonlite)
library(DBI)
library(RPostgres)
library(dplyr)

# --- Database connection ---
get_db_connection <- function() {
  dbConnect(
    Postgres(),
    host = Sys.getenv("POSTGRES_HOST"),
    port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD"),
    sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
  )
}

# --- HTTP fetch with caching ---
fetch_usda_json <- function(url, cache_file) {
  # Return cached if exists
  if (file.exists(cache_file)) {
    return(list(
      data = fromJSON(readLines(cache_file, warn = FALSE), simplifyVector = TRUE),
      cached = TRUE
    ))
  }

  # Fetch from API
  resp <- tryCatch({
    request(url) |>
      req_user_agent("EdaphicFlora/1.0 (research)") |>
      req_timeout(30) |>
      req_perform()
  }, error = function(e) NULL)

  if (is.null(resp) || resp_status(resp) >= 400) {
    return(list(data = NULL, cached = FALSE))
  }

  body <- resp_body_string(resp)

  # Cache the response
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(body, cache_file)

  list(data = fromJSON(body, simplifyVector = TRUE), cached = FALSE)
}

# --- Extract characteristics from USDA API response ---
parse_characteristics <- function(char_data) {
  if (is.null(char_data) || !is.data.frame(char_data)) return(list())

  # USDA returns PlantCharacteristicName / PlantCharacteristicValue pairs
  if (!all(c("PlantCharacteristicName", "PlantCharacteristicValue") %in% names(char_data))) {
    return(list())
  }

  result <- list()

  for (i in seq_len(nrow(char_data))) {
    name <- tolower(trimws(char_data$PlantCharacteristicName[i]))
    value <- char_data$PlantCharacteristicValue[i]

    if (is.na(value) || !nzchar(value)) next

    # Map USDA field names to our database columns
    if (grepl("^ph.*min", name)) result$soil_ph_min <- as.numeric(gsub("[^0-9.]", "", value))
    else if (grepl("^ph.*max", name)) result$soil_ph_max <- as.numeric(gsub("[^0-9.]", "", value))
    else if (grepl("precip.*min", name)) result$precip_min_in <- as.numeric(gsub("[^0-9.]", "", value))
    else if (grepl("precip.*max", name)) result$precip_max_in <- as.numeric(gsub("[^0-9.]", "", value))
    else if (grepl("temp.*min", name)) result$temp_min_f <- as.numeric(gsub("[^0-9.-]", "", value))
    else if (grepl("shade.*tol", name)) result$shade_tolerance <- value
    else if (grepl("drought.*tol", name)) result$drought_tolerance <- value
    else if (grepl("salinity.*tol", name)) result$salinity_tolerance <- value
    else if (grepl("moisture.*use", name)) result$moisture_use <- value
    else if (grepl("bloom.*period", name)) result$bloom_period <- value
    else if (grepl("growth.*rate", name)) result$growth_rate <- value
    else if (grepl("anaerobic.*tol", name)) result$anaerobic_tolerance <- value
    else if (grepl("caco3.*tol", name)) result$caco3_tolerance <- value
  }

  result
}

# --- Fetch data for a single species ---
fetch_single_species <- function(symbol, taxon_id, cache_dir, con) {
  # 1. Fetch profile to get plant ID
  profile_cache <- file.path(cache_dir, paste0("profile_", symbol, ".json"))
  profile <- fetch_usda_json(
    sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantProfile?symbol=%s", URLencode(symbol)),
    profile_cache
  )

  if (is.null(profile$data)) {
    return(list(success = FALSE, reason = "profile_failed"))
  }

  # Extract plant ID from profile
  plant_id <- profile$data$Id[1]
  if (is.null(plant_id) || is.na(plant_id)) {
    return(list(success = FALSE, reason = "no_plant_id"))
  }

  # Extract basic info from profile
  duration <- profile$data$Duration[1]
  growth_habit <- if (!is.null(profile$data$GrowthHabits)) {
    paste(profile$data$GrowthHabits, collapse = ", ")
  } else NA
  native_status <- profile$data$NativeStatus[1]

  # 2. Fetch characteristics
  Sys.sleep(0.3)  # Rate limiting
  char_cache <- file.path(cache_dir, paste0("char_", plant_id, ".json"))
  chars <- fetch_usda_json(
    sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantCharacteristics/%s", plant_id),
    char_cache
  )

  # Parse characteristics
  char_data <- parse_characteristics(chars$data)

  # Add profile data
  char_data$duration <- duration
  char_data$growth_habit <- growth_habit
  char_data$native_status <- native_status
  char_data$usda_symbol <- symbol
  char_data$taxon_id <- taxon_id

  # Upsert to database
  tryCatch({
    # Check if record exists
    existing <- dbGetQuery(con,
      "SELECT taxon_id FROM ref_usda_traits WHERE taxon_id = $1",
      params = list(taxon_id))

    if (nrow(existing) == 0) {
      # Insert new record
      dbExecute(con, "
        INSERT INTO ref_usda_traits (taxon_id, usda_symbol, duration, growth_habit, native_status,
          soil_ph_min, soil_ph_max, precip_min_in, precip_max_in, temp_min_f,
          shade_tolerance, drought_tolerance, salinity_tolerance, moisture_use, bloom_period)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
      ", params = list(
        char_data$taxon_id, char_data$usda_symbol,
        char_data$duration %||% NA, char_data$growth_habit %||% NA, char_data$native_status %||% NA,
        char_data$soil_ph_min %||% NA, char_data$soil_ph_max %||% NA,
        char_data$precip_min_in %||% NA, char_data$precip_max_in %||% NA, char_data$temp_min_f %||% NA,
        char_data$shade_tolerance %||% NA, char_data$drought_tolerance %||% NA,
        char_data$salinity_tolerance %||% NA, char_data$moisture_use %||% NA, char_data$bloom_period %||% NA
      ))
    } else {
      # Update existing (only non-null values)
      sets <- c()
      params <- list()
      param_idx <- 1

      for (col in c("soil_ph_min", "soil_ph_max", "precip_min_in", "precip_max_in", "temp_min_f",
                    "shade_tolerance", "drought_tolerance", "salinity_tolerance", "moisture_use",
                    "bloom_period", "duration", "growth_habit", "native_status")) {
        val <- char_data[[col]]
        if (!is.null(val) && !is.na(val)) {
          sets <- c(sets, sprintf("%s = $%d", col, param_idx))
          params[[param_idx]] <- val
          param_idx <- param_idx + 1
        }
      }

      if (length(sets) > 0) {
        params[[param_idx]] <- taxon_id
        sql <- sprintf("UPDATE ref_usda_traits SET %s, updated_at = NOW() WHERE taxon_id = $%d",
                      paste(sets, collapse = ", "), param_idx)
        dbExecute(con, sql, params = params)
      }
    }

    list(success = TRUE, cached = profile$cached && chars$cached, traits = length(char_data))
  }, error = function(e) {
    list(success = FALSE, reason = paste("db_error:", e$message))
  })
}

# --- Main batch fetcher ---
#' Fetch USDA data for species missing characteristics
#' @param limit Maximum number of species to fetch (default 100)
#' @param cache_dir Directory to cache API responses
#' @param only_missing If TRUE, only fetch species without existing data
fetch_usda_batch <- function(limit = 100, cache_dir = "data/cache/usda_char", only_missing = TRUE) {
  message("=== USDA Reference Data Fetcher ===\n")

  # Connect to database
  con <- tryCatch(get_db_connection(), error = function(e) {
    stop("Database connection failed. Check your .Renviron settings.\n", e$message)
  })
  on.exit(dbDisconnect(con), add = TRUE)

  # Get species to fetch
  if (only_missing) {
    sql <- "
      SELECT t.id, t.usda_symbol
      FROM ref_taxon t
      LEFT JOIN ref_usda_traits r ON r.taxon_id = t.id
      WHERE t.usda_symbol IS NOT NULL
        AND t.usda_symbol != ''
        AND (r.taxon_id IS NULL
             OR r.soil_ph_min IS NULL
             OR r.soil_ph_max IS NULL)
      LIMIT $1
    "
  } else {
    sql <- "
      SELECT id, usda_symbol
      FROM ref_taxon
      WHERE usda_symbol IS NOT NULL AND usda_symbol != ''
      LIMIT $1
    "
  }

  species <- dbGetQuery(con, sql, params = list(limit))

  if (nrow(species) == 0) {
    message("No species to fetch. All species have USDA data or no USDA symbols in database.")
    return(invisible(NULL))
  }

  message(sprintf("Fetching USDA data for %d species...\n", nrow(species)))

  # Progress tracking
  success <- 0
  failed <- 0
  cached <- 0

  for (i in seq_len(nrow(species))) {
    symbol <- species$usda_symbol[i]
    taxon_id <- species$id[i]

    # Progress indicator
    if (i %% 10 == 0 || i == nrow(species)) {
      message(sprintf("[%d/%d] Processing %s... (success: %d, cached: %d, failed: %d)",
                     i, nrow(species), symbol, success, cached, failed))
    }

    result <- fetch_single_species(symbol, taxon_id, cache_dir, con)

    if (isTRUE(result$success)) {
      success <- success + 1
      if (isTRUE(result$cached)) cached <- cached + 1
    } else {
      failed <- failed + 1
    }

    # Rate limiting between API calls (skip if we used cache)
    if (!isTRUE(result$cached)) Sys.sleep(0.5)
  }

  message(sprintf("\n=== Complete ==="))
  message(sprintf("Processed: %d species", nrow(species)))
  message(sprintf("Success: %d (cached: %d, fresh: %d)", success, cached, success - cached))
  message(sprintf("Failed: %d", failed))

  invisible(list(processed = nrow(species), success = success, cached = cached, failed = failed))
}

# Null coalescing operator
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

message("USDA fetcher loaded. Run: fetch_usda_batch(limit = 100)")
