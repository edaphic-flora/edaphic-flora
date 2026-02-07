# R/db.R - Database connection and helper functions

library(DBI)
library(RPostgres)
library(pool)

# Note: %||% operator defined in R/helpers.R (canonical definition)

# ---------------------------
# Column Whitelist
# ---------------------------
SOIL_SAMPLE_COLUMNS <- c(
  "species", "cultivar", "ph", "organic_matter", "organic_matter_class",
  "nitrate_ppm", "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
  "calcium_ppm", "magnesium_ppm", "sulfur_ppm", "iron_ppm", "manganese_ppm",
  "zinc_ppm", "boron_ppm", "copper_ppm", "soluble_salts_ppm",
  "cec_meq", "texture_sand", "texture_silt", "texture_clay", "texture_class",
  "location_lat", "location_long", "date", "ecoregion_l4", "ecoregion_l4_code",
  "ecoregion_l3", "ecoregion_l3_code", "ecoregion_l2", "ecoregion_l2_code",
  "notes", "created_by", "outcome", "sun_exposure", "site_hydrology"
)

SOIL_SAMPLE_SELECT <- paste("id,", paste(SOIL_SAMPLE_COLUMNS, collapse = ", "), ", created_at")

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
  maxSize  = as.integer(Sys.getenv("DB_POOL_SIZE", "5"))  # Lower default for memory-constrained envs
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

    # Per-species metadata columns (added 2025-01)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS inat_url TEXT")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS sun_exposure VARCHAR(50)")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS site_hydrology VARCHAR(50)")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS outcome VARCHAR(50)")

    # Additional soil parameters (added 2025-01)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS sulfur_ppm NUMERIC")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS iron_ppm NUMERIC")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS manganese_ppm NUMERIC")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS zinc_ppm NUMERIC")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS boron_ppm NUMERIC")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS copper_ppm NUMERIC")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS cec_meq NUMERIC")

    # Qualitative organic matter class (added 2025-01)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS organic_matter_class VARCHAR(50)")

    # Level III ecoregion columns (added 2025-01)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l3 VARCHAR(255)")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l3_code VARCHAR(50)")

    # Level II ecoregion columns (added 2025-01)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l2 VARCHAR(255)")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l2_code VARCHAR(50)")

    # Indices
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_species ON soil_samples(species)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_date ON soil_samples(date)")

    # PDF extraction rate limiting table
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS pdf_extractions (
        id SERIAL PRIMARY KEY,
        user_id TEXT NOT NULL,
        extracted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        tokens_used INTEGER,
        filename TEXT
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_pdf_extractions_user ON pdf_extractions(user_id)")

    # Audit log table for tracking changes
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS audit_log (
        id SERIAL PRIMARY KEY,
        action VARCHAR(50) NOT NULL,
        table_name VARCHAR(100),
        record_id INTEGER,
        user_id TEXT,
        details TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_audit_log_user ON audit_log(user_id)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_audit_log_action ON audit_log(action)")

    # State distribution table for native status lookup
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS ref_state_distribution (
        id SERIAL PRIMARY KEY,
        taxon_id INTEGER REFERENCES ref_taxon(id) ON DELETE CASCADE,
        state_code VARCHAR(2) NOT NULL,
        native_status VARCHAR(20) NOT NULL,
        source VARCHAR(50) DEFAULT 'USDA',
        updated_at TIMESTAMPTZ DEFAULT now(),
        UNIQUE(taxon_id, state_code)
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_state_dist_taxon ON ref_state_distribution(taxon_id)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_state_dist_state ON ref_state_distribution(state_code)")

    # User preferences table (home location for native status lookups)
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS user_preferences (
        user_id TEXT PRIMARY KEY,
        home_zipcode VARCHAR(10),
        home_state VARCHAR(2),
        home_city TEXT,
        home_lat NUMERIC(10,6),
        home_long NUMERIC(10,6),
        created_at TIMESTAMPTZ DEFAULT now(),
        updated_at TIMESTAMPTZ DEFAULT now()
      )
    ")

    # Experience level column for user preferences
    dbExecute(pool, "ALTER TABLE user_preferences ADD COLUMN IF NOT EXISTS experience_level VARCHAR(20) DEFAULT 'casual'")

    # Noxious/invasive species reference table
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS ref_noxious_invasive (
        id SERIAL PRIMARY KEY,
        taxon_id INTEGER REFERENCES ref_taxon(id) ON DELETE CASCADE,
        state_code VARCHAR(2),
        designation TEXT NOT NULL,
        source TEXT,
        source_url TEXT,
        updated_at TIMESTAMPTZ DEFAULT now(),
        UNIQUE(taxon_id, state_code, designation)
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_noxious_taxon ON ref_noxious_invasive(taxon_id)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_noxious_state ON ref_noxious_invasive(state_code)")

    TRUE
  }, error = function(e) {
    # Ignore permission errors - schema likely already exists in production
    if (!grepl("permission denied", e$message, ignore.case = TRUE)) {
      message("DB migration error: ", e$message)
    }
    FALSE
  })
}

# ---------------------------
# Query Functions
# ---------------------------

db_get_all_samples <- function(limit = NULL) {
  tryCatch({
    sql <- paste("SELECT", SOIL_SAMPLE_SELECT, "FROM soil_samples ORDER BY created_at DESC")
    if (!is.null(limit) && is.numeric(limit) && limit > 0) {
      sql <- paste(sql, "LIMIT", as.integer(limit))
    }
    dbGetQuery(pool, sql)
  }, error = function(e) {
    message("Error fetching samples: ", e$message)
    data.frame()
  })
}

db_get_species_data <- function(species, limit = NULL) {
  if (is.null(species) || !nzchar(trimws(species))) return(data.frame())
  tryCatch({
    sql <- paste("SELECT", SOIL_SAMPLE_SELECT, "FROM soil_samples WHERE species = $1 ORDER BY created_at DESC")
    if (!is.null(limit) && is.numeric(limit) && limit > 0) {
      sql <- paste(sql, "LIMIT", as.integer(limit))
    }
    dbGetQuery(pool, sql, params = list(species))
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
    # Filter through column whitelist
    sample_data <- sample_data[names(sample_data) %in% SOIL_SAMPLE_COLUMNS]
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

# ---------------------------
# Edit/Delete Functions
# ---------------------------

db_get_sample_by_id <- function(id) {
  tryCatch({
    dbGetQuery(pool, paste("SELECT", SOIL_SAMPLE_SELECT, "FROM soil_samples WHERE id = $1"), params = list(id))
  }, error = function(e) {
    message("Error fetching sample by id: ", e$message)
    data.frame()
  })
}

db_get_user_samples <- function(user_id) {
  if (is.null(user_id) || !nzchar(trimws(user_id))) return(data.frame())
  tryCatch({
    dbGetQuery(pool, paste("SELECT", SOIL_SAMPLE_SELECT, "FROM soil_samples WHERE created_by = $1 ORDER BY created_at DESC"),
               params = list(user_id))
  }, error = function(e) {
    message("Error fetching user samples: ", e$message)
    data.frame()
  })
}

db_update_sample <- function(id, sample_data, user_id, is_admin = FALSE) {
  tryCatch({
    # Verify ownership (unless admin)
    existing <- dbGetQuery(pool, "SELECT created_by FROM soil_samples WHERE id = $1", params = list(id))
    if (nrow(existing) == 0) {
      message("Sample not found: ", id)
      return(FALSE)
    }
    if (!is_admin && (is.na(existing$created_by[1]) || existing$created_by[1] != user_id)) {
      message("User does not own this sample")
      return(FALSE)
    }

    # Format date if present
    if ("date" %in% names(sample_data)) {
      sample_data$date <- as.character(as.Date(sample_data$date))
    }

    # Filter through column whitelist
    sample_data <- sample_data[names(sample_data) %in% SOIL_SAMPLE_COLUMNS]

    # Build UPDATE statement
    fields <- names(sample_data)
    set_clauses <- paste0(fields, " = $", seq_along(fields))
    values <- as.list(unname(sample_data))
    values <- c(values, list(id))  # Add id as last parameter

    sql <- sprintf("UPDATE soil_samples SET %s WHERE id = $%d",
                   paste(set_clauses, collapse = ", "), length(fields) + 1)
    dbExecute(pool, sql, params = values)
    TRUE
  }, error = function(e) {
    message("db_update_sample error: ", e$message)
    FALSE
  })
}

db_delete_sample <- function(id, user_id, is_admin = FALSE) {
  tryCatch({
    # Verify ownership (unless admin)
    existing <- dbGetQuery(pool, "SELECT created_by FROM soil_samples WHERE id = $1", params = list(id))
    if (nrow(existing) == 0) {
      message("Sample not found: ", id)
      return(FALSE)
    }
    if (!is_admin && (is.na(existing$created_by[1]) || existing$created_by[1] != user_id)) {
      message("User does not own this sample")
      return(FALSE)
    }

    dbExecute(pool, "DELETE FROM soil_samples WHERE id = $1", params = list(id))
    TRUE
  }, error = function(e) {
    message("db_delete_sample error: ", e$message)
    FALSE
  })
}

# ---------------------------
# PDF Extraction Rate Limiting
# ---------------------------

db_get_extraction_count_today <- function(user_id) {
  tryCatch({
    result <- dbGetQuery(pool,
      "SELECT COUNT(*) as count FROM pdf_extractions
       WHERE user_id = $1 AND extracted_at >= CURRENT_DATE",
      params = list(user_id))
    as.integer(result$count[1])
  }, error = function(e) {
    message("Error getting extraction count: ", e$message)
    # Fail closed: return high count to prevent unlimited extractions on DB error
    Inf
  })
}

db_log_extraction <- function(user_id, filename = NULL, tokens_used = NULL) {
  tryCatch({
    dbExecute(pool,
      "INSERT INTO pdf_extractions (user_id, filename, tokens_used) VALUES ($1, $2, $3)",
      params = list(user_id, filename, tokens_used))
    TRUE
  }, error = function(e) {
    message("Error logging extraction: ", e$message)
    FALSE
  })
}

db_can_extract <- function(user_id, daily_limit = 5) {
  count <- db_get_extraction_count_today(user_id)
  count < daily_limit
}

db_get_remaining_extractions <- function(user_id, daily_limit = 5) {
  count <- db_get_extraction_count_today(user_id)
  # If count is Inf (error case), return 0 remaining
  if (is.infinite(count)) return(0L)
  as.integer(max(0, daily_limit - count))
}

# ---------------------------
# Audit Logging
# ---------------------------

#' Log an action to the audit trail
#' @param action Action type (e.g., "create", "update", "delete", "login", "export")
#' @param table_name Table affected (e.g., "soil_samples")
#' @param record_id ID of the affected record
#' @param user_id User who performed the action
#' @param details Additional details (JSON or text)
db_audit_log <- function(action, table_name = NULL, record_id = NULL, user_id = NULL, details = NULL) {
  tryCatch({
    dbExecute(pool,
      "INSERT INTO audit_log (action, table_name, record_id, user_id, details) VALUES ($1, $2, $3, $4, $5)",
      params = list(action, table_name, record_id, user_id, details))
    # Also log to console for server logs
    message(sprintf("[AUDIT] %s | %s | record:%s | user:%s | %s",
                    action, table_name %||% "-", record_id %||% "-",
                    substr(user_id %||% "-", 1, 8), details %||% ""))
    TRUE
  }, error = function(e) {
    message("Audit log error: ", e$message)
    FALSE
  })
}

#' Get recent audit log entries
#' @param limit Number of entries to return
db_get_audit_log <- function(limit = 100) {
  tryCatch({
    dbGetQuery(pool,
      "SELECT * FROM audit_log ORDER BY created_at DESC LIMIT $1",
      params = list(limit))
  }, error = function(e) {
    message("Error fetching audit log: ", e$message)
    data.frame()
  })
}

# ---------------------------
# Reuse Previous Soil Data
# ---------------------------

#' Get user's recent unique soil profiles for reuse
#' Groups by soil chemistry to show distinct soil tests (not every species entry)
#' @param user_id Firebase UID
#' @param limit Max number of profiles to return (default 10)
#' @return Data frame with unique soil profiles
db_get_user_soil_profiles <- function(user_id, limit = 10) {
  if (is.null(user_id) || !nzchar(user_id)) return(data.frame())

  tryCatch({
    # Get distinct soil profiles (group by key soil properties)
    # Use the most recent entry for each unique soil test
    query <- "
      SELECT DISTINCT ON (ph, organic_matter, texture_class)
        id, date, created_at, ph, organic_matter, organic_matter_class,
        texture_class, texture_sand, texture_silt, texture_clay,
        nitrate_ppm, ammonium_ppm, phosphorus_ppm, potassium_ppm,
        calcium_ppm, magnesium_ppm, sulfur_ppm, cec_meq, soluble_salts_ppm,
        iron_ppm, manganese_ppm, zinc_ppm, copper_ppm, boron_ppm,
        location_lat, location_long
      FROM soil_samples
      WHERE created_by = $1
        AND (ph IS NOT NULL OR organic_matter IS NOT NULL)
      ORDER BY ph, organic_matter, texture_class, created_at DESC
      LIMIT $2
    "

    dbGetQuery(pool, query, params = list(user_id, limit))
  }, error = function(e) {
    message("Error fetching user soil profiles: ", e$message)
    data.frame()
  })
}

#' Get soil data from a specific entry for reuse
#' @param entry_id The soil_samples.id to get data from
#' @return Named list of soil chemistry fields, or NULL if not found
db_get_soil_data_by_id <- function(entry_id) {
  if (is.null(entry_id) || is.na(entry_id)) return(NULL)

  tryCatch({
    query <- "
      SELECT ph, organic_matter, organic_matter_class,
             texture_class, texture_sand, texture_silt, texture_clay,
             nitrate_ppm, ammonium_ppm, phosphorus_ppm, potassium_ppm,
             calcium_ppm, magnesium_ppm, sulfur_ppm, cec_meq, soluble_salts_ppm,
             iron_ppm, manganese_ppm, zinc_ppm, copper_ppm, boron_ppm,
             date as source_date
      FROM soil_samples
      WHERE id = $1
    "
    result <- dbGetQuery(pool, query, params = list(entry_id))

    if (nrow(result) == 0) return(NULL)

    as.list(result[1, ])
  }, error = function(e) {
    message("Error fetching soil data by id: ", e$message)
    NULL
  })
}

# ---------------------------
# User Preferences
# ---------------------------

#' Get user preferences (home location for native status lookups)
#' @param user_id Firebase UID
#' @param pool Database connection pool (optional, uses global if not provided)
#' @return Named list with home_zipcode, home_state, home_city, home_lat, home_long, or NULL if not set
db_get_user_prefs <- function(user_id, pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(NULL)
  if (is.null(pool)) pool <- get("pool", envir = globalenv())

  tryCatch({
    result <- dbGetQuery(pool, "
      SELECT home_zipcode, home_state, home_city, home_lat, home_long,
             COALESCE(experience_level, 'casual') AS experience_level
      FROM user_preferences
      WHERE user_id = $1
    ", params = list(user_id))

    if (nrow(result) == 0) return(NULL)

    as.list(result[1, ])
  }, error = function(e) {
    message("Error fetching user preferences: ", e$message)
    NULL
  })
}

#' Set user preferences (home location)
#' Uses upsert to insert or update existing preferences
#' @param user_id Firebase UID
#' @param zipcode 5-digit US zip code
#' @param city City name (optional, derived from zipcode lookup)
#' @param state Two-letter state code (optional, derived from zipcode lookup)
#' @param lat Latitude (optional, derived from zipcode lookup)
#' @param lon Longitude (optional, derived from zipcode lookup)
#' @param pool Database connection pool (optional, uses global if not provided)
#' @return TRUE on success, FALSE on failure
db_set_user_prefs <- function(user_id, zipcode, city = NULL, state = NULL, lat = NULL, lon = NULL, pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(FALSE)
  if (is.null(pool)) pool <- get("pool", envir = globalenv())

  tryCatch({
    dbExecute(pool, "
      INSERT INTO user_preferences (user_id, home_zipcode, home_state, home_city, home_lat, home_long, updated_at)
      VALUES ($1, $2, $3, $4, $5, $6, now())
      ON CONFLICT (user_id)
      DO UPDATE SET
        home_zipcode = EXCLUDED.home_zipcode,
        home_state = EXCLUDED.home_state,
        home_city = EXCLUDED.home_city,
        home_lat = EXCLUDED.home_lat,
        home_long = EXCLUDED.home_long,
        updated_at = now()
    ", params = list(user_id, zipcode, state, city, lat, lon))
    TRUE
  }, error = function(e) {
    message("Error setting user preferences: ", e$message)
    FALSE
  })
}

#' Set experience level for a user
#' @param user_id Firebase UID
#' @param level "casual" or "enthusiast"
#' @param pool Database connection pool (optional, uses global if not provided)
#' @return TRUE on success, FALSE on failure
db_set_experience_level <- function(user_id, level = "casual", pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(FALSE)
  if (is.null(pool)) pool <- get("pool", envir = globalenv())
  if (!level %in% c("casual", "enthusiast")) level <- "casual"

  tryCatch({
    dbExecute(pool, "
      INSERT INTO user_preferences (user_id, experience_level, updated_at)
      VALUES ($1, $2, now())
      ON CONFLICT (user_id)
      DO UPDATE SET experience_level = EXCLUDED.experience_level, updated_at = now()
    ", params = list(user_id, level))
    TRUE
  }, error = function(e) {
    message("Error setting experience level: ", e$message)
    FALSE
  })
}

#' Clear user preferences
#' @param user_id Firebase UID
#' @param pool Database connection pool (optional, uses global if not provided)
#' @return TRUE on success, FALSE on failure
db_clear_user_prefs <- function(user_id, pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(FALSE)
  if (is.null(pool)) pool <- get("pool", envir = globalenv())

  tryCatch({
    dbExecute(pool, "DELETE FROM user_preferences WHERE user_id = $1", params = list(user_id))
    TRUE
  }, error = function(e) {
    message("Error clearing user preferences: ", e$message)
    FALSE
  })
}

# ---------------------------
# Stats Gating
# ---------------------------

#' Check if a species meets the threshold for public stats display
#' @param species Species name
#' @param pool Database connection pool
#' @return List with meets_threshold, n_samples, n_contributors, status_label
db_check_species_stats_threshold <- function(species, pool) {
  if (is.null(species) || !nzchar(species)) {
    return(list(meets_threshold = FALSE, n_samples = 0L, n_contributors = 0L,
                status_label = "No species selected"))
  }

  tryCatch({
    result <- dbGetQuery(pool, "
      SELECT COUNT(*)::int AS n_samples,
             COUNT(DISTINCT created_by)::int AS n_contributors
      FROM soil_samples
      WHERE species = $1
    ", params = list(species))

    n_samples <- result$n_samples[1]
    n_contributors <- result$n_contributors[1]

    meets <- n_samples >= MIN_SAMPLES_FOR_PUBLIC_STATS &&
             n_contributors >= MIN_CONTRIBUTORS_FOR_PUBLIC_STATS

    label <- if (meets) {
      "Community Data"
    } else {
      sprintf("Early Access (%d/%d samples, %d/%d contributors)",
              n_samples, MIN_SAMPLES_FOR_PUBLIC_STATS,
              n_contributors, MIN_CONTRIBUTORS_FOR_PUBLIC_STATS)
    }

    list(
      meets_threshold = meets,
      n_samples = n_samples,
      n_contributors = n_contributors,
      status_label = label
    )
  }, error = function(e) {
    message("Error checking species stats threshold: ", e$message)
    list(meets_threshold = FALSE, n_samples = 0L, n_contributors = 0L,
         status_label = "Error checking stats")
  })
}

#' Get site-wide statistics for gating welcome page stats
#' @param pool Database connection pool
#' @return List with total_samples, total_species, total_contributors, meets_site_threshold
db_get_site_stats <- function(pool) {
  tryCatch({
    result <- dbGetQuery(pool, "
      SELECT COUNT(*)::int AS total_samples,
             COUNT(DISTINCT species)::int AS total_species,
             COUNT(DISTINCT created_by)::int AS total_contributors
      FROM soil_samples
    ")

    total <- result$total_samples[1]
    list(
      total_samples = total,
      total_species = result$total_species[1],
      total_contributors = result$total_contributors[1],
      meets_site_threshold = total >= MIN_TOTAL_SAMPLES_FOR_SITE_STATS
    )
  }, error = function(e) {
    message("Error getting site stats: ", e$message)
    list(total_samples = 0L, total_species = 0L, total_contributors = 0L,
         meets_site_threshold = FALSE)
  })
}

# ---------------------------
# Nearby Samples Query
# ---------------------------

#' Get soil samples near a given location
#' Uses bounding box pre-filter in SQL, then Haversine post-filter in R.
#' @param lat Latitude (degrees)
#' @param lon Longitude (degrees)
#' @param radius_miles Search radius in miles (default 10, max 50)
#' @param pool Database connection pool
#' @param exclude_user_id Optional user ID to exclude own samples
#' @return Data frame of matching samples with distance_miles column
db_get_nearby_samples <- function(lat, lon, radius_miles = DEFAULT_NEIGHBOR_RADIUS_MILES, pool, exclude_user_id = NULL) {
  if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) return(data.frame())
  radius_miles <- min(radius_miles, MAX_NEIGHBOR_RADIUS_MILES)

  # Bounding box approximation: 1 degree latitude ~ 69 miles
  # 1 degree longitude varies by latitude, using conservative estimate
  lat_delta <- radius_miles / 69.0
  lon_delta <- radius_miles / (69.0 * cos(lat * pi / 180))

  lat_min <- lat - lat_delta
  lat_max <- lat + lat_delta
  lon_min <- lon - lon_delta
  lon_max <- lon + lon_delta

  tryCatch({
    # Bounding box pre-filter in SQL
    if (!is.null(exclude_user_id) && nzchar(exclude_user_id)) {
      query <- paste("
        SELECT", SOIL_SAMPLE_SELECT, "
        FROM soil_samples
        WHERE location_lat IS NOT NULL AND location_long IS NOT NULL
          AND location_lat BETWEEN $1 AND $2
          AND location_long BETWEEN $3 AND $4
          AND (created_by IS NULL OR created_by != $5)
      ")
      result <- dbGetQuery(pool, query, params = list(lat_min, lat_max, lon_min, lon_max, exclude_user_id))
    } else {
      query <- paste("
        SELECT", SOIL_SAMPLE_SELECT, "
        FROM soil_samples
        WHERE location_lat IS NOT NULL AND location_long IS NOT NULL
          AND location_lat BETWEEN $1 AND $2
          AND location_long BETWEEN $3 AND $4
      ")
      result <- dbGetQuery(pool, query, params = list(lat_min, lat_max, lon_min, lon_max))
    }

    if (nrow(result) == 0) return(data.frame())

    # Haversine post-filter in R
    result$distance_miles <- mapply(
      function(r_lat, r_lon) haversine_miles(lat, lon, r_lat, r_lon),
      result$location_lat, result$location_long
    )

    # Filter by actual distance and sort
    result <- result[result$distance_miles <= radius_miles, ]
    result <- result[order(result$distance_miles), ]
    result
  }, error = function(e) {
    message("Error getting nearby samples: ", e$message)
    data.frame()
  })
}
