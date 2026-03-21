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

    # Wildlife reference tables (proprietary data loaded via ETL)
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS ref_wildlife_plants (
        id SERIAL PRIMARY KEY,
        taxon_id INTEGER REFERENCES ref_taxon(id) ON DELETE CASCADE,
        species_code VARCHAR(50) UNIQUE NOT NULL,
        genus VARCHAR(100),
        life_form VARCHAR(50),
        lepidoptera_species_count INTEGER DEFAULT 0,
        specialist_bee_species_count INTEGER DEFAULT 0,
        songbird_value VARCHAR(50),
        bird_food TEXT,
        hummingbird_plant BOOLEAN DEFAULT FALSE,
        is_keystone_genus BOOLEAN DEFAULT FALSE,
        updated_at TIMESTAMPTZ DEFAULT now()
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_wildlife_plants_taxon ON ref_wildlife_plants(taxon_id)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_wildlife_plants_genus ON ref_wildlife_plants(genus)")

    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS ref_wildlife_species (
        id SERIAL PRIMARY KEY,
        wildlife_id VARCHAR(50) UNIQUE NOT NULL,
        scientific_name TEXT NOT NULL,
        common_name TEXT,
        wildlife_type VARCHAR(30) NOT NULL,
        family VARCHAR(100),
        specialist_generalist VARCHAR(30),
        functional_group VARCHAR(50),
        updated_at TIMESTAMPTZ DEFAULT now()
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_wildlife_species_type ON ref_wildlife_species(wildlife_type)")
    dbExecute(pool, "ALTER TABLE ref_wildlife_species ADD COLUMN IF NOT EXISTS midatlantic_present BOOLEAN")

    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS ref_wildlife_interactions (
        id SERIAL PRIMARY KEY,
        plant_species_code VARCHAR(50) NOT NULL,
        wildlife_id VARCHAR(50) NOT NULL,
        interaction_type VARCHAR(50) NOT NULL,
        evidence_level VARCHAR(30),
        source TEXT,
        updated_at TIMESTAMPTZ DEFAULT now(),
        UNIQUE(plant_species_code, wildlife_id, interaction_type)
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_wildlife_int_plant ON ref_wildlife_interactions(plant_species_code)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_wildlife_int_wildlife ON ref_wildlife_interactions(wildlife_id)")

    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS ref_specialist_bees_by_genus (
        id SERIAL PRIMARY KEY,
        host_genus VARCHAR(100) UNIQUE NOT NULL,
        specialist_bee_count INTEGER DEFAULT 0,
        specialist_bee_species TEXT,
        updated_at TIMESTAMPTZ DEFAULT now()
      )
    ")

    # Wildlife state-level presence (GBIF + eBird occurrence data)
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS ref_wildlife_state_presence (
        id SERIAL PRIMARY KEY,
        wildlife_id VARCHAR(50) NOT NULL,
        state_code VARCHAR(2) NOT NULL,
        source VARCHAR(50) NOT NULL,
        observation_count INTEGER,
        updated_at TIMESTAMPTZ DEFAULT now(),
        UNIQUE(wildlife_id, state_code)
      )
    ")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_wsp_state_wildlife ON ref_wildlife_state_presence(state_code, wildlife_id)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_wsp_wildlife ON ref_wildlife_state_presence(wildlife_id)")

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

# ---------------------------
# Wildlife Dashboard Queries
# ---------------------------

#' Get distinct species from a user's soil samples (their "garden")
#' @param user_id Firebase UID
#' @param pool Database connection pool
#' @return Character vector of species names
db_get_user_garden_species <- function(user_id, pool) {
  if (is.null(user_id) || !nzchar(user_id)) return(character())
  tryCatch({
    result <- dbGetQuery(pool, "
      SELECT DISTINCT species FROM soil_samples
      WHERE created_by = $1 AND species IS NOT NULL AND species != ''
      ORDER BY species
    ", params = list(user_id))
    result$species
  }, error = function(e) {
    message("Error fetching garden species: ", e$message)
    character()
  })
}

#' Get wildlife coverage for a set of garden species
#' Joins species → ref_taxon (genus match) → ref_wildlife_plants → ref_wildlife_interactions → ref_wildlife_species
#' @param species_list Character vector of species names from soil_samples
#' @param pool Database connection pool
#' @return Data frame with columns: garden_species, plant_species_code, wildlife_id, wildlife_type,
#'         wildlife_family, wildlife_common_name, wildlife_scientific_name, interaction_type,
#'         specialist_generalist, functional_group
db_get_wildlife_coverage <- function(species_list, pool) {
  if (length(species_list) == 0) return(data.frame())
  tryCatch({
    # Stage garden species in temp table for join
    con <- poolCheckout(pool)
    on.exit(poolReturn(con), add = TRUE)

    dbExecute(con, "DROP TABLE IF EXISTS tmp_garden_species")
    dbExecute(con, "CREATE TEMP TABLE tmp_garden_species (species TEXT)")
    if (length(species_list) > 0) {
      dbWriteTable(con, "tmp_garden_species",
                   data.frame(species = species_list, stringsAsFactors = FALSE),
                   append = TRUE, temporary = TRUE, row.names = FALSE)
    }

    dbGetQuery(con, "
      SELECT DISTINCT
        g.species AS garden_species,
        wp.species_code AS plant_species_code,
        ws.wildlife_id,
        ws.wildlife_type,
        COALESCE(ws.family, 'Unknown') AS wildlife_family,
        ws.common_name AS wildlife_common_name,
        ws.scientific_name AS wildlife_scientific_name,
        wi.interaction_type,
        ws.specialist_generalist,
        ws.functional_group
      FROM tmp_garden_species g
      JOIN ref_taxon t ON lower(split_part(t.scientific_name, ' ', 1))
                        = lower(split_part(g.species, ' ', 1))
      JOIN ref_wildlife_plants wp ON wp.taxon_id = t.id
      JOIN ref_wildlife_interactions wi ON wi.plant_species_code = wp.species_code
      JOIN ref_wildlife_species ws ON ws.wildlife_id = wi.wildlife_id
    ")
  }, error = function(e) {
    message("Error fetching wildlife coverage: ", e$message)
    data.frame()
  })
}

#' Aggregate wildlife coverage into per-family/group summary stats
#' Pure R function — no DB hit.
#' @param coverage_df Data frame from db_get_wildlife_coverage
#' @return List keyed by wildlife_type, each containing a data frame with
#'         family, species_covered, total_species, coverage_pct
db_get_wildlife_summary <- function(coverage_df, all_species_df = NULL) {
  if (is.null(coverage_df) || nrow(coverage_df) == 0) return(list())

  types <- unique(coverage_df$wildlife_type)
  result <- list()

  for (wtype in types) {
    type_df <- coverage_df[coverage_df$wildlife_type == wtype, ]
    covered_by_family <- tapply(type_df$wildlife_id, type_df$wildlife_family, function(x) length(unique(x)))

    # Get total species per family from all_species_df if available
    if (!is.null(all_species_df) && nrow(all_species_df) > 0) {
      all_type <- all_species_df[all_species_df$wildlife_type == wtype, ]
      total_by_family <- tapply(all_type$wildlife_id, all_type$family, function(x) length(unique(x)))
    } else {
      total_by_family <- covered_by_family
    }

    families <- union(names(covered_by_family), names(total_by_family))
    families <- families[!is.na(families) & nzchar(families)]

    summary_df <- data.frame(
      family = families,
      species_covered = as.integer(covered_by_family[families]),
      total_species = as.integer(total_by_family[families]),
      stringsAsFactors = FALSE
    )
    summary_df$species_covered[is.na(summary_df$species_covered)] <- 0L
    summary_df$total_species[is.na(summary_df$total_species)] <- 0L
    summary_df$coverage_pct <- ifelse(summary_df$total_species > 0,
                                       round(100 * summary_df$species_covered / summary_df$total_species, 1),
                                       0)
    summary_df <- summary_df[order(-summary_df$species_covered), ]
    result[[wtype]] <- summary_df
  }

  result
}

#' Get all wildlife species (for total counts per family)
#' @param pool Database connection pool
#' @return Data frame with wildlife_id, wildlife_type, family
db_get_all_wildlife_species <- function(pool) {
  tryCatch({
    dbGetQuery(pool, "
      SELECT wildlife_id, wildlife_type, COALESCE(family, 'Unknown') AS family
      FROM ref_wildlife_species
    ")
  }, error = function(e) {
    message("Error fetching all wildlife species: ", e$message)
    data.frame()
  })
}

#' Get gap recommendations — plants NOT in user's garden that would add the most wildlife
#' @param covered_codes Character vector of plant species_codes already in garden
#' @param user_state Two-letter state code for native filtering
#' @param pool Database connection pool
#' @param max_results Max recommendations to return
#' @return Data frame with species info and wildlife impact counts
db_get_wildlife_gap_recs <- function(covered_codes, user_state, pool,
                                     life_form_filter = NULL, max_results = 10) {
  tryCatch({
    con <- poolCheckout(pool)
    on.exit(poolReturn(con), add = TRUE)

    # Validate state_code format (2 uppercase letters only)
    if (!is.null(user_state) && nzchar(user_state)) {
      if (!grepl("^[A-Z]{2}$", user_state)) {
        message("Invalid state_code format, skipping state filter")
        user_state <- NULL
      }
    }

    # Stage covered codes
    dbExecute(con, "DROP TABLE IF EXISTS tmp_covered_codes")
    dbExecute(con, "CREATE TEMP TABLE tmp_covered_codes (species_code VARCHAR(50))")
    if (length(covered_codes) > 0) {
      dbWriteTable(con, "tmp_covered_codes",
                   data.frame(species_code = covered_codes, stringsAsFactors = FALSE),
                   append = TRUE, temporary = TRUE, row.names = FALSE)
    }

    # Build state filter using parameterized query
    state_join <- ""
    query_params <- list()
    param_idx <- 0L

    if (!is.null(user_state) && nzchar(user_state)) {
      param_idx <- param_idx + 1L
      state_join <- sprintf("
        JOIN ref_state_distribution sd ON sd.taxon_id = wp.taxon_id
          AND sd.state_code = $%d AND sd.native_status = 'Native'
      ", param_idx)
      query_params <- c(query_params, list(user_state))
    }

    # Build life_form filter
    # Values come from a hardcoded switch statement, so they are safe,
    # but we use parameterized placeholders for defense-in-depth
    life_form_clause <- ""
    if (!is.null(life_form_filter) && nzchar(life_form_filter) && life_form_filter != "All") {
      # Map filter categories to actual life_form values
      lf_values <- switch(life_form_filter,
        "Tree" = c("Tree"),
        "Shrub" = c("Shrub", "Shrub/small tree"),
        "Perennial" = c("Perennial", "Perennial (ephemeral)", "Groundcover"),
        "Grass/Sedge" = c("Grass", "Sedge"),
        "Vine" = c("Vine"),
        "Fern" = c("Fern"),
        "Annual" = c("Annual"),
        NULL
      )
      if (!is.null(lf_values)) {
        lf_placeholders <- vapply(seq_along(lf_values), function(i) {
          param_idx <<- param_idx + 1L
          query_params[[param_idx]] <<- lf_values[i]
          sprintf("$%d", param_idx)
        }, character(1))
        life_form_clause <- sprintf("AND wp.life_form IN (%s)",
                                    paste(lf_placeholders, collapse = ", "))
      }
    }

    # max_results as parameterized value
    param_idx <- param_idx + 1L
    query_params <- c(query_params, list(as.integer(max_results)))
    limit_placeholder <- sprintf("$%d", param_idx)

    # Aggregate at genus level — show one representative per genus
    query <- sprintf("
      WITH gap_plants AS (
        SELECT wp.species_code, wp.taxon_id, wp.genus, wp.is_keystone_genus,
               wp.lepidoptera_species_count, wp.specialist_bee_species_count,
               wp.life_form,
               t.scientific_name
        FROM ref_wildlife_plants wp
        JOIN ref_taxon t ON t.id = wp.taxon_id
        %s
        WHERE NOT EXISTS (
          SELECT 1 FROM tmp_covered_codes cc WHERE cc.species_code = wp.species_code
        )
        %s
      ),
      genus_impact AS (
        SELECT gp.genus,
               MAX(gp.is_keystone_genus::int)::boolean AS is_keystone_genus,
               MAX(gp.life_form) AS life_form,
               COUNT(DISTINCT wi.wildlife_id) AS new_wildlife_count,
               COUNT(DISTINCT CASE WHEN ws.wildlife_type IN ('Moth', 'Butterfly', 'Skipper')
                     THEN ws.wildlife_id END) AS lep_count,
               COUNT(DISTINCT CASE WHEN ws.wildlife_type = 'Bee'
                     THEN ws.wildlife_id END) AS bee_count,
               COUNT(DISTINCT CASE WHEN ws.wildlife_type = 'Bird'
                     THEN ws.wildlife_id END) AS bird_count
        FROM gap_plants gp
        JOIN ref_wildlife_interactions wi ON wi.plant_species_code = gp.species_code
        JOIN ref_wildlife_species ws ON ws.wildlife_id = wi.wildlife_id
        GROUP BY gp.genus
      )
      SELECT * FROM genus_impact
      ORDER BY is_keystone_genus DESC, new_wildlife_count DESC
      LIMIT %s
    ", state_join, life_form_clause, limit_placeholder)

    dbGetQuery(con, query, params = query_params)
  }, error = function(e) {
    message("Error fetching wildlife gap recommendations: ", e$message)
    data.frame()
  })
}

#' Get native species for a genus in a given state (from BONAP)
#' Deduplicates at genus+epithet level (ignores varieties/subspecies)
#' @param genus Character, e.g., "Quercus"
#' @param state_code Two-letter state code, e.g., "MN"
#' @param pool Database connection pool
#' @return Data frame with species_name and common_name columns
db_get_native_species_for_genus <- function(genus, state_code, pool) {
  if (is.null(genus) || is.null(state_code) || !nzchar(genus) || !nzchar(state_code)) {
    return(data.frame(species_name = character(), common_name = character(), stringsAsFactors = FALSE))
  }
  tryCatch({
    dbGetQuery(pool, "
      SELECT DISTINCT ON (split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
        split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2) AS species_name,
        u.common_name
      FROM ref_state_distribution sd
      JOIN ref_taxon t ON t.id = sd.taxon_id
      LEFT JOIN ref_usda_traits u ON u.taxon_id = t.id
      WHERE sd.state_code = $1
        AND sd.native_status = 'Native'
        AND lower(split_part(t.scientific_name, ' ', 1)) = lower($2)
        AND split_part(t.scientific_name, ' ', 2) NOT LIKE '%[%'
        AND split_part(t.scientific_name, ' ', 2) != ''
      ORDER BY split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2),
        CASE WHEN t.scientific_name !~ ' (var|ssp|subsp|f)\\.? ' THEN 0 ELSE 1 END,
        t.scientific_name
    ", params = list(state_code, genus))
  }, error = function(e) {
    message("Error fetching native species for genus: ", e$message)
    data.frame(species_name = character(), common_name = character(), stringsAsFactors = FALSE)
  })
}
