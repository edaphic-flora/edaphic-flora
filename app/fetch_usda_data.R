# =============================================================================
# USDA Reference Data Fetcher - Standalone Script
# =============================================================================
# Run this script to fetch USDA plant characteristics for species in your database.
#
# Usage:
#   1. Make sure your .Renviron is loaded (restart R if you just added it)
#   2. Set working directory to app folder: setwd("app")
#   3. Source this script: source("fetch_usda_data.R")
#   4. Run: usda_stats()                    # Check progress
#   5. Run: fetch_usda_batch(limit = 100)   # Fetch 100 species
#
# The script will:
#   - Find species in ref_taxon that haven't been attempted yet
#   - Fetch data from USDA PLANTS API
#   - Cache responses locally (won't re-fetch already cached)
#   - Store characteristics in database (even if empty, to track attempts)
#   - Track fetch status: 'success', 'no_data', 'api_error', 'no_symbol'
# =============================================================================

library(httr2)
library(jsonlite)
library(DBI)
library(RPostgres)
library(dplyr)

# Null coalescing operator
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

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

# --- Ensure fetch_status column exists ---
ensure_schema <- function(con) {
  # Add fetch_status column if it doesn't exist
  tryCatch({
    dbExecute(con, "
      ALTER TABLE ref_usda_traits
      ADD COLUMN IF NOT EXISTS fetch_status TEXT DEFAULT NULL,
      ADD COLUMN IF NOT EXISTS fetch_attempted_at TIMESTAMPTZ DEFAULT NULL
    ")
  }, error = function(e) {
    # Column might already exist, that's fine
    if (!grepl("already exists", e$message, ignore.case = TRUE)) {
      warning("Schema update warning: ", e$message)
    }
  })
}

# --- Progress statistics ---
#' Show USDA fetch progress statistics
usda_stats <- function() {
  con <- tryCatch(get_db_connection(), error = function(e) {
    stop("Database connection failed. Check your .Renviron settings.\n", e$message)
  })
  on.exit(dbDisconnect(con), add = TRUE)

  ensure_schema(con)

  stats <- dbGetQuery(con, "
    WITH taxon_counts AS (
      SELECT
        COUNT(*) AS total_taxa,
        COUNT(*) FILTER (WHERE usda_symbol IS NOT NULL AND usda_symbol != '') AS with_usda_symbol
      FROM ref_taxon
    ),
    trait_counts AS (
      SELECT
        COUNT(*) AS attempted,
        COUNT(*) FILTER (WHERE fetch_status = 'success') AS success,
        COUNT(*) FILTER (WHERE fetch_status = 'no_data') AS no_data,
        COUNT(*) FILTER (WHERE fetch_status IN ('api_error', 'no_symbol', 'profile_failed', 'no_plant_id')) AS failed,
        COUNT(*) FILTER (WHERE fetch_status IS NULL) AS legacy,
        COUNT(*) FILTER (WHERE soil_ph_min IS NOT NULL) AS has_ph,
        COUNT(*) FILTER (WHERE shade_tolerance IS NOT NULL) AS has_shade,
        COUNT(*) FILTER (WHERE drought_tolerance IS NOT NULL) AS has_drought
      FROM ref_usda_traits
    ),
    remaining AS (
      SELECT COUNT(*) AS not_attempted
      FROM ref_taxon t
      LEFT JOIN ref_usda_traits r ON r.taxon_id = t.id
      WHERE t.usda_symbol IS NOT NULL
        AND t.usda_symbol != ''
        AND r.taxon_id IS NULL
    )
    SELECT
      tc.total_taxa,
      tc.with_usda_symbol,
      COALESCE(trc.attempted, 0) AS attempted,
      COALESCE(trc.success, 0) AS success,
      COALESCE(trc.no_data, 0) AS no_data,
      COALESCE(trc.failed, 0) AS failed,
      COALESCE(trc.legacy, 0) AS legacy,
      COALESCE(trc.has_ph, 0) AS has_ph,
      COALESCE(trc.has_shade, 0) AS has_shade,
      COALESCE(trc.has_drought, 0) AS has_drought,
      COALESCE(r.not_attempted, 0) AS not_attempted
    FROM taxon_counts tc
    CROSS JOIN trait_counts trc
    CROSS JOIN remaining r
  ")

  # Convert to integers for display
  s <- lapply(stats, as.integer)

  message("\n=== USDA Data Fetch Progress ===\n")
  message(sprintf("Total taxa in database:     %6d", s$total_taxa))
  message(sprintf("Taxa with USDA symbols:     %6d (fetchable)", s$with_usda_symbol))
  message("")
  message(sprintf("Already attempted:          %6d", s$attempted))
  message(sprintf("  - Success (has data):     %6d", s$success))
  message(sprintf("  - No data from USDA:      %6d", s$no_data))
  message(sprintf("  - API/fetch errors:       %6d", s$failed))
  message(sprintf("  - Legacy (status unknown):%6d", s$legacy))
  message("")
  message(sprintf("Remaining to fetch:         %6d", s$not_attempted))
  message("")

  pct_complete <- if (s$with_usda_symbol > 0) {
    round(100 * s$attempted / s$with_usda_symbol, 1)
  } else 0

  message(sprintf("Progress: %.1f%% complete (%d/%d)",
                  pct_complete, s$attempted, s$with_usda_symbol))
  message("")
  message("Data quality (of attempted):")
  message(sprintf("  - Has pH data:            %6d (%.1f%%)",
                  s$has_ph, 100 * s$has_ph / max(s$attempted, 1)))
  message(sprintf("  - Has shade tolerance:    %6d (%.1f%%)",
                  s$has_shade, 100 * s$has_shade / max(s$attempted, 1)))
  message(sprintf("  - Has drought tolerance:  %6d (%.1f%%)",
                  s$has_drought, 100 * s$has_drought / max(s$attempted, 1)))

  invisible(stats)
}

# --- HTTP fetch with caching ---
fetch_usda_json <- function(url, cache_file) {
  # Return cached if exists
  if (file.exists(cache_file)) {
    return(list(
      data = tryCatch(
        fromJSON(readLines(cache_file, warn = FALSE), simplifyVector = TRUE),
        error = function(e) NULL
      ),
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

  list(data = tryCatch(fromJSON(body, simplifyVector = TRUE), error = function(e) NULL), cached = FALSE)
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

# --- Helper to safely extract scalar value from potentially nested data ---
safe_scalar <- function(x, collapse = ", ") {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if (length(x) == 0) return(NA_character_)
  if (length(x) > 1) {
    # Multiple values - collapse to string
    x <- x[!is.na(x) & nzchar(as.character(x))]
    if (length(x) == 0) return(NA_character_)
    return(paste(unique(x), collapse = collapse))
  }
  if (is.na(x) || !nzchar(as.character(x))) return(NA_character_)
  as.character(x)
}

safe_numeric <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(NA_real_)
  x <- x[1]
  if (is.na(x)) return(NA_real_)
  as.numeric(x)
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
    # Record the failed attempt
    record_attempt(con, taxon_id, symbol, list(), "profile_failed")
    return(list(success = FALSE, reason = "profile_failed", cached = profile$cached))
  }

  # Extract plant ID from profile (handle potential list/array)
  plant_id <- safe_numeric(profile$data$Id)
  if (is.na(plant_id)) {
    record_attempt(con, taxon_id, symbol, list(), "no_plant_id")
    return(list(success = FALSE, reason = "no_plant_id", cached = profile$cached))
  }

  # Extract basic info from profile - safely handle arrays/lists
  duration <- safe_scalar(profile$data$Duration)
  growth_habit <- safe_scalar(profile$data$GrowthHabits)
  native_status <- safe_scalar(profile$data$NativeStatus)

  # 2. Fetch characteristics
  if (!isTRUE(profile$cached)) Sys.sleep(0.3)  # Rate limiting only for fresh requests

  char_cache <- file.path(cache_dir, paste0("char_", as.integer(plant_id), ".json"))
  chars <- fetch_usda_json(
    sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantCharacteristics/%s", as.integer(plant_id)),
    char_cache
  )

  # Parse characteristics
  char_data <- parse_characteristics(chars$data)

  # Add profile data (already safe scalars)
  char_data$duration <- duration
  char_data$growth_habit <- growth_habit
  char_data$native_status <- native_status

  # Determine status based on what data we got
  has_any_data <- length(char_data) > 0 || !is.na(duration) || !is.na(native_status)
  status <- if (has_any_data) "success" else "no_data"

  # Record the attempt (success or no_data)
  record_attempt(con, taxon_id, symbol, char_data, status)

  list(
    success = TRUE,
    status = status,
    cached = isTRUE(profile$cached) && isTRUE(chars$cached),
    traits = length(char_data)
  )
}

# --- Record fetch attempt in database ---
record_attempt <- function(con, taxon_id, symbol, char_data, status) {
  # Helper to ensure value is scalar for DB insertion
  to_db <- function(x, numeric = FALSE) {
    if (is.null(x)) return(NA)
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    if (length(x) == 0) return(NA)
    if (length(x) > 1) x <- paste(x[!is.na(x)], collapse = ", ")
    x <- x[1]
    if (is.na(x)) return(NA)
    if (numeric) return(as.numeric(x))
    as.character(x)
  }

  tryCatch({
    # Check if record exists
    existing <- dbGetQuery(con,
      "SELECT taxon_id FROM ref_usda_traits WHERE taxon_id = $1",
      params = list(as.integer(taxon_id)))

    if (nrow(existing) == 0) {
      # Insert new record with status - ensure all values are scalar
      dbExecute(con, "
        INSERT INTO ref_usda_traits (
          taxon_id, usda_symbol, duration, growth_habit, native_status,
          soil_ph_min, soil_ph_max, precip_min_in, precip_max_in, temp_min_f,
          shade_tolerance, drought_tolerance, salinity_tolerance, moisture_use, bloom_period,
          fetch_status, fetch_attempted_at
        )
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, NOW())
      ", params = list(
        as.integer(taxon_id),
        to_db(symbol),
        to_db(char_data$duration),
        to_db(char_data$growth_habit),
        to_db(char_data$native_status),
        to_db(char_data$soil_ph_min, numeric = TRUE),
        to_db(char_data$soil_ph_max, numeric = TRUE),
        to_db(char_data$precip_min_in, numeric = TRUE),
        to_db(char_data$precip_max_in, numeric = TRUE),
        to_db(char_data$temp_min_f, numeric = TRUE),
        to_db(char_data$shade_tolerance),
        to_db(char_data$drought_tolerance),
        to_db(char_data$salinity_tolerance),
        to_db(char_data$moisture_use),
        to_db(char_data$bloom_period),
        to_db(status)
      ))
    } else {
      # Update existing record
      sets <- c("fetch_status = $1", "fetch_attempted_at = NOW()")
      params <- list(to_db(status))
      param_idx <- 2

      for (col in c("soil_ph_min", "soil_ph_max", "precip_min_in", "precip_max_in", "temp_min_f",
                    "shade_tolerance", "drought_tolerance", "salinity_tolerance", "moisture_use",
                    "bloom_period", "duration", "growth_habit", "native_status")) {
        val <- char_data[[col]]
        db_val <- to_db(val, numeric = col %in% c("soil_ph_min", "soil_ph_max", "precip_min_in", "precip_max_in", "temp_min_f"))
        if (!is.na(db_val)) {
          sets <- c(sets, sprintf("%s = $%d", col, param_idx))
          params[[param_idx]] <- db_val
          param_idx <- param_idx + 1
        }
      }

      params[[param_idx]] <- as.integer(taxon_id)
      sql <- sprintf("UPDATE ref_usda_traits SET %s WHERE taxon_id = $%d",
                    paste(sets, collapse = ", "), param_idx)
      dbExecute(con, sql, params = params)
    }
    TRUE
  }, error = function(e) {
    warning("DB error for taxon ", taxon_id, ": ", e$message)
    FALSE
  })
}

# --- Main batch fetcher ---
#' Fetch USDA data for species not yet attempted
#' @param limit Maximum number of species to fetch (default 100)
#' @param cache_dir Directory to cache API responses
#' @param retry_errors If TRUE, retry species that previously had API errors
#' @param delay Seconds to wait between API calls (default 0.5, use 2.5-3 for overnight runs)
fetch_usda_batch <- function(limit = 100, cache_dir = "data/cache/usda_char", retry_errors = FALSE, delay = 0.5) {
  message("=== USDA Reference Data Fetcher ===\n")

  # Connect to database
  con <- tryCatch(get_db_connection(), error = function(e) {
    stop("Database connection failed. Check your .Renviron settings.\n", e$message)
  })
  on.exit(dbDisconnect(con), add = TRUE)

  # Ensure schema is up to date
  ensure_schema(con)

  # Get species to fetch - only those NOT in ref_usda_traits yet
  if (retry_errors) {
    # Retry species with API errors
    sql <- "
      SELECT t.id, t.usda_symbol
      FROM ref_taxon t
      JOIN ref_usda_traits r ON r.taxon_id = t.id
      WHERE t.usda_symbol IS NOT NULL
        AND t.usda_symbol != ''
        AND r.fetch_status IN ('api_error', 'profile_failed', 'no_plant_id')
      LIMIT $1
    "
  } else {
    # Normal mode: only fetch species we haven't attempted at all
    sql <- "
      SELECT t.id, t.usda_symbol
      FROM ref_taxon t
      LEFT JOIN ref_usda_traits r ON r.taxon_id = t.id
      WHERE t.usda_symbol IS NOT NULL
        AND t.usda_symbol != ''
        AND r.taxon_id IS NULL
      LIMIT $1
    "
  }

  species <- dbGetQuery(con, sql, params = list(limit))

  if (nrow(species) == 0) {
    message("No species to fetch!")
    if (!retry_errors) {
      message("All species with USDA symbols have been attempted.")
      message("Run usda_stats() to see progress, or use retry_errors=TRUE to retry failures.")
    }
    return(invisible(NULL))
  }

  message(sprintf("Fetching USDA data for %d species...", nrow(species)))
  est_hours <- round((nrow(species) * (delay + 0.5)) / 3600, 1)
  message(sprintf("Estimated time: %.1f hours (with %.1fs delay between calls)\n", est_hours, delay))

  # Progress tracking
  success_with_data <- 0
  success_no_data <- 0
  failed <- 0
  cached <- 0

  for (i in seq_len(nrow(species))) {
    symbol <- species$usda_symbol[i]
    taxon_id <- species$id[i]

    # Progress indicator
    if (i %% 10 == 0 || i == nrow(species)) {
      message(sprintf("[%d/%d] %s (data: %d, no_data: %d, failed: %d, cached: %d)",
                     i, nrow(species), symbol, success_with_data, success_no_data, failed, cached))
    }

    result <- fetch_single_species(symbol, taxon_id, cache_dir, con)

    if (isTRUE(result$success)) {
      if (result$status == "success") {
        success_with_data <- success_with_data + 1
      } else {
        success_no_data <- success_no_data + 1
      }
      if (isTRUE(result$cached)) cached <- cached + 1
    } else {
      failed <- failed + 1
    }

    # Rate limiting between API calls (skip if we used cache)
    if (!isTRUE(result$cached)) Sys.sleep(delay)
  }

  message(sprintf("\n=== Complete ==="))
  message(sprintf("Processed: %d species", nrow(species)))
  message(sprintf("Success with data: %d", success_with_data))
  message(sprintf("Success (no USDA data available): %d", success_no_data))
  message(sprintf("Failed (API errors): %d", failed))
  message(sprintf("From cache: %d", cached))
  message("\nRun usda_stats() to see overall progress.")

  invisible(list(
    processed = nrow(species),
    success_with_data = success_with_data,
    success_no_data = success_no_data,
    failed = failed,
    cached = cached
  ))
}

# --- Mark legacy records ---
#' Update legacy records (those without fetch_status) to have proper status
mark_legacy_records <- function() {
  con <- tryCatch(get_db_connection(), error = function(e) {
    stop("Database connection failed.\n", e$message)
  })
  on.exit(dbDisconnect(con), add = TRUE)

  ensure_schema(con)

  # Mark records that have data as 'success'
  n1 <- dbExecute(con, "
    UPDATE ref_usda_traits
    SET fetch_status = 'success', fetch_attempted_at = COALESCE(updated_at, NOW())
    WHERE fetch_status IS NULL
      AND (soil_ph_min IS NOT NULL OR shade_tolerance IS NOT NULL OR duration IS NOT NULL)
  ")

  # Mark records with no data as 'no_data'
  n2 <- dbExecute(con, "
    UPDATE ref_usda_traits
    SET fetch_status = 'no_data', fetch_attempted_at = COALESCE(updated_at, NOW())
    WHERE fetch_status IS NULL
  ")


  message(sprintf("Updated %d legacy records as 'success'", n1))
  message(sprintf("Updated %d legacy records as 'no_data'", n2))

  invisible(list(success = n1, no_data = n2))
}

# --- Diagnose data issues ---
#' Check for mismatches between ref_taxon and ref_usda_traits
usda_diagnose <- function(cache_dir = "data/cache/usda_char") {
  con <- tryCatch(get_db_connection(), error = function(e) {
    stop("Database connection failed.\n", e$message)
  })
  on.exit(dbDisconnect(con), add = TRUE)

  message("\n=== USDA Data Diagnosis ===\n")

  # Check taxon_id alignment
  alignment <- dbGetQuery(con, "
    SELECT
      (SELECT COUNT(*) FROM ref_usda_traits) AS traits_total,
      (SELECT COUNT(*) FROM ref_usda_traits r
       JOIN ref_taxon t ON t.id = r.taxon_id) AS traits_matched,
      (SELECT COUNT(*) FROM ref_usda_traits r
       LEFT JOIN ref_taxon t ON t.id = r.taxon_id
       WHERE t.id IS NULL) AS traits_orphaned
  ")

  message(sprintf("ref_usda_traits records:      %d", as.integer(alignment$traits_total)))
  message(sprintf("  - Matched to ref_taxon:     %d", as.integer(alignment$traits_matched)))
  message(sprintf("  - Orphaned (no match):      %d", as.integer(alignment$traits_orphaned)))

  # Check cache files
  profile_files <- list.files(cache_dir, pattern = "^profile_.*\\.json$", full.names = FALSE)
  char_files <- list.files(cache_dir, pattern = "^char_.*\\.json$", full.names = FALSE)

  # Extract symbols from profile filenames
  cached_symbols <- gsub("^profile_|\\.json$", "", profile_files)

  message("\nCache files:")
  message(sprintf("  - Profile files:            %d", as.integer(length(profile_files))))
  message(sprintf("  - Characteristic files:     %d", as.integer(length(char_files))))

  # Check how many cached symbols exist in ref_taxon
  if (length(cached_symbols) > 0) {
    # Sample check - take first 1000
    sample_symbols <- head(cached_symbols, 1000)
    in_taxon <- dbGetQuery(con,
      sprintf("SELECT COUNT(*) as n FROM ref_taxon WHERE usda_symbol IN (%s)",
              paste(sprintf("'%s'", sample_symbols), collapse = ",")))

    message(sprintf("  - Cached symbols in ref_taxon: %d/%d (sampled)",
                    as.integer(in_taxon$n), as.integer(length(sample_symbols))))
  }

  # Check what the query actually returns
  next_batch <- dbGetQuery(con, "
    SELECT t.id, t.usda_symbol
    FROM ref_taxon t
    LEFT JOIN ref_usda_traits r ON r.taxon_id = t.id
    WHERE t.usda_symbol IS NOT NULL
      AND t.usda_symbol != ''
      AND r.taxon_id IS NULL
    LIMIT 10
  ")

  message("\nNext 10 species to fetch:")
  for (i in seq_len(nrow(next_batch))) {
    sym <- next_batch$usda_symbol[i]
    has_cache <- file.exists(file.path(cache_dir, paste0("profile_", sym, ".json")))
    message(sprintf("  %s (id=%d) - cache: %s", sym, as.integer(next_batch$id[i]), if(has_cache) "YES" else "no"))
  }

  invisible(list(
    alignment = alignment,
    cached_symbols = length(cached_symbols)
  ))
}

# --- Sync cache to database ---
#' Process existing cache files and ensure they have database records
#' This fixes the issue where cache exists but no database record
sync_cache_to_db <- function(cache_dir = "data/cache/usda_char", limit = 500) {
  con <- tryCatch(get_db_connection(), error = function(e) {
    stop("Database connection failed.\n", e$message)
  })
  on.exit(dbDisconnect(con), add = TRUE)

  ensure_schema(con)

  message("=== Syncing Cache to Database ===\n")

  # Get species that have cache files but no database record
  # First, find which symbols are in ref_taxon but not in ref_usda_traits
  missing <- dbGetQuery(con, "
    SELECT t.id, t.usda_symbol
    FROM ref_taxon t
    LEFT JOIN ref_usda_traits r ON r.taxon_id = t.id
    WHERE t.usda_symbol IS NOT NULL
      AND t.usda_symbol != ''
      AND r.taxon_id IS NULL
  ")

  if (nrow(missing) == 0) {
    message("All species already have database records!")
    return(invisible(NULL))
  }

  # Filter to those with cache files
  missing$has_cache <- sapply(missing$usda_symbol, function(sym) {
    file.exists(file.path(cache_dir, paste0("profile_", sym, ".json")))
  })

  to_sync <- missing[missing$has_cache, ]
  to_sync <- head(to_sync, limit)

  if (nrow(to_sync) == 0) {
    message("No cached species need syncing.")
    message(sprintf("(%d species need fetching from API)", as.integer(nrow(missing))))
    return(invisible(NULL))
  }

  message(sprintf("Found %d species with cache but no DB record", as.integer(sum(missing$has_cache))))
  message(sprintf("Processing %d of them...\n", as.integer(nrow(to_sync))))

  synced <- 0
  for (i in seq_len(nrow(to_sync))) {
    symbol <- to_sync$usda_symbol[i]
    taxon_id <- to_sync$id[i]

    if (i %% 50 == 0 || i == nrow(to_sync)) {
      message(sprintf("[%d/%d] Syncing %s...", as.integer(i), as.integer(nrow(to_sync)), symbol))
    }

    # Process cached data (this won't hit the API since cache exists)
    result <- fetch_single_species(symbol, taxon_id, cache_dir, con)
    if (isTRUE(result$success)) synced <- synced + 1
  }

  message("\n=== Sync Complete ===")
  message(sprintf("Synced: %d species", as.integer(synced)))
  message(sprintf("Remaining without cache: %d (will need API fetch)",
                  as.integer(nrow(missing) - sum(missing$has_cache))))

  invisible(list(synced = synced, remaining = nrow(missing) - sum(missing$has_cache)))
}

message("USDA fetcher loaded. Available commands:")
message("  usda_stats()                  - Show fetch progress")
message("  usda_diagnose()               - Check for data issues")
message("  sync_cache_to_db(limit=500)   - Sync cached files to database")
message("  fetch_usda_batch(limit=100)   - Fetch next batch from API")
message("  mark_legacy_records()         - Update old records with status")
