# R/db.R - Database connection and helper functions

library(DBI)
library(RPostgres)
library(pool)

# Note: %||% operator defined in R/helpers.R (canonical definition)

# ---------------------------
# Column Whitelist
# ---------------------------
SOIL_SAMPLE_COLUMNS <- c(
  "species", "cultivar", "ph", "organic_matter",
  "nitrate_ppm", "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
  "calcium_ppm", "magnesium_ppm", "sulfur_ppm", "iron_ppm", "manganese_ppm",
  "zinc_ppm", "boron_ppm", "copper_ppm", "soluble_salts_ppm",
  "cec_meq", "texture_sand", "texture_silt", "texture_clay", "texture_class",
  "location_lat", "location_long", "date", "ecoregion_l4", "ecoregion_l4_code",
  "ecoregion_l3", "ecoregion_l3_code", "ecoregion_l2", "ecoregion_l2_code",
  "notes", "created_by", "outcome", "sun_exposure", "site_hydrology"
)

SOIL_SAMPLE_SELECT <- paste("id,", paste(SOIL_SAMPLE_COLUMNS, collapse = ", "), ", flagged, flag_reason, created_at")

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

# Bump this whenever the DDL block in db_migrate() changes. The fast-path
# check below short-circuits when the DB already records this version,
# turning the 66-statement migration into a single SELECT round-trip.
SCHEMA_VERSION <- 3L

db_migrate <- function() {
  # Fast path: one round-trip. If schema_version table reports the current
  # version, skip the full DDL block entirely.
  current_version <- tryCatch({
    res <- dbGetQuery(pool, "SELECT version FROM schema_version LIMIT 1")
    if (nrow(res) > 0) as.integer(res$version[1]) else 0L
  }, error = function(e) NA_integer_)

  if (!is.na(current_version) && current_version >= SCHEMA_VERSION) {
    return(TRUE)
  }

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

    # organic_matter_class dropped 2026-05-11 — descriptors aren't comparable
    # across lab methods. Storage is numeric % only; users convert descriptors
    # via their lab's own chart before entry.
    dbExecute(pool, "ALTER TABLE soil_samples DROP COLUMN IF EXISTS organic_matter_class")

    # Level III ecoregion columns (added 2025-01)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l3 VARCHAR(255)")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l3_code VARCHAR(50)")

    # Level II ecoregion columns (added 2025-01)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l2 VARCHAR(255)")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS ecoregion_l2_code VARCHAR(50)")

    # Indices
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_species ON soil_samples(species)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_date ON soil_samples(date)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_created_by ON soil_samples(created_by)")
    dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_location ON soil_samples(location_lat, location_long) WHERE location_lat IS NOT NULL")

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

    # Data quality: flagged samples (admin moderation)
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS flagged BOOLEAN DEFAULT FALSE")
    dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS flag_reason TEXT")

    # Admin kill-switch: ban a user from new submissions / exports / extractions.
    # Existing samples are not auto-removed — admin can bulk-flag separately.
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS disabled_users (
        user_id TEXT PRIMARY KEY,
        disabled_at TIMESTAMPTZ DEFAULT now(),
        disabled_by TEXT,
        reason TEXT
      )
    ")

    # Record current schema version so future startups can short-circuit.
    # Single-row table enforced by id=1 CHECK; upsert keeps it idempotent.
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS schema_version (
        id INTEGER PRIMARY KEY DEFAULT 1,
        version INTEGER NOT NULL,
        updated_at TIMESTAMPTZ DEFAULT now(),
        CONSTRAINT schema_version_single_row CHECK (id = 1)
      )
    ")
    dbExecute(pool,
      "INSERT INTO schema_version (id, version) VALUES (1, $1)
       ON CONFLICT (id) DO UPDATE SET version = EXCLUDED.version, updated_at = now()",
      params = list(SCHEMA_VERSION))

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

#' Fetch all samples. Defaults to filtering out flagged rows so callers don't
#' accidentally leak moderated data to public surfaces (exports, charts,
#' similar-species matching). Admin callers pass include_flagged = TRUE.
db_get_all_samples <- function(limit = NULL, include_flagged = FALSE) {
  tryCatch({
    where_clause <- if (include_flagged) "" else " WHERE flagged IS NULL OR flagged = FALSE"
    sql <- paste0("SELECT ", SOIL_SAMPLE_SELECT, " FROM soil_samples",
                  where_clause, " ORDER BY created_at DESC")
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
    sql <- paste("SELECT", SOIL_SAMPLE_SELECT, "FROM soil_samples WHERE species = $1 AND (flagged IS NULL OR flagged = FALSE) ORDER BY created_at DESC")
    if (!is.null(limit) && is.numeric(limit) && limit > 0) {
      sql <- paste(sql, "LIMIT", as.integer(limit))
    }
    dbGetQuery(pool, sql, params = list(species))
  }, error = function(e) {
    message("Error fetching species data: ", e$message)
    data.frame()
  })
}

db_get_unique_species <- function(include_flagged = FALSE) {
  tryCatch({
    sql <- if (include_flagged) {
      "SELECT DISTINCT species FROM soil_samples ORDER BY species"
    } else {
      "SELECT DISTINCT species FROM soil_samples WHERE flagged IS NULL OR flagged = FALSE ORDER BY species"
    }
    res <- dbGetQuery(pool, sql)
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
  if (db_is_user_disabled(user_id)) return(FALSE)
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
        id, date, created_at, ph, organic_matter,
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
      SELECT ph, organic_matter,
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
        AND (flagged IS NULL OR flagged = FALSE)
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
      WHERE (flagged IS NULL OR flagged = FALSE)
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
    # Bounding box pre-filter in SQL (excludes flagged samples)
    if (!is.null(exclude_user_id) && nzchar(exclude_user_id)) {
      query <- paste("
        SELECT", SOIL_SAMPLE_SELECT, "
        FROM soil_samples
        WHERE location_lat IS NOT NULL AND location_long IS NOT NULL
          AND (flagged IS NULL OR flagged = FALSE)
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
          AND (flagged IS NULL OR flagged = FALSE)
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

#' Batch-classify a list of garden species for the My Garden non-native panel.
#'
#' Replaces the per-species N+1 loop in `nonnative_summary` (which fired
#' 3-5 round-trips per species via get_native_status_for_user/get_invasive_status).
#' This issues a single round-trip that returns the raw fields needed to
#' compute native_class + invasive flags downstream in R.
#'
#' @param species_list Character vector of "Genus species" names.
#' @param state_code Two-letter US state code (user's home state). If NULL/blank
#'   the function returns an empty frame — matches old behavior, since
#'   nonnative_summary collapses to empty without a home state.
#' @param pool Database connection pool.
#' @return Data frame, one row per input species, with:
#'   species, taxon_id, state_native_status, state_native_source,
#'   na_native_status_str, is_federal_invasive (logical),
#'   federal_designation, user_state_invasive_designation,
#'   invasive_state_codes (list-column of character vectors).
db_get_garden_classification_batch <- function(species_list, state_code, pool) {
  empty <- data.frame(
    species = character(0),
    taxon_id = integer(0),
    state_native_status = character(0),
    state_native_source = character(0),
    na_native_status_str = character(0),
    is_federal_invasive = logical(0),
    federal_designation = character(0),
    user_state_invasive_designation = character(0),
    invasive_state_codes = I(list()),
    stringsAsFactors = FALSE
  )
  if (length(species_list) == 0) return(empty)
  if (is.null(state_code) || !nzchar(state_code)) return(empty)
  state_code <- toupper(state_code)
  if (!grepl("^[A-Z]{2}$", state_code)) return(empty)

  tryCatch({
    con <- poolCheckout(pool)
    on.exit(poolReturn(con), add = TRUE)

    dbExecute(con, "DROP TABLE IF EXISTS tmp_garden_class")
    dbExecute(con, "CREATE TEMP TABLE tmp_garden_class (species TEXT)")
    dbWriteTable(con, "tmp_garden_class",
                 data.frame(species = species_list, stringsAsFactors = FALSE),
                 append = TRUE, temporary = TRUE, row.names = FALSE)

    # Resolve each garden species to a taxon_id using the same 3-step
    # fallback chain as resolve_taxon_id():
    #   (1) ref_taxon.usda_symbol exact match,
    #   (2) genus+species match on ref_taxon.scientific_name,
    #   (3) genus+species match via ref_synonym.
    # The synonym subselect references ref_synonym; if the table is missing
    # the whole query fails and the tryCatch returns the empty frame,
    # which preserves the old behavior of "unmatched → not classified".
    query <- "
      WITH garden_taxon AS (
        SELECT
          g.species,
          COALESCE(
            (SELECT id FROM ref_taxon
               WHERE usda_symbol = upper(g.species) LIMIT 1),
            (SELECT id FROM ref_taxon
               WHERE lower(split_part(scientific_name, ' ', 1) || ' ' ||
                           split_part(scientific_name, ' ', 2)) = lower(g.species)
               LIMIT 1),
            (SELECT t.id FROM ref_synonym s
               JOIN ref_taxon t ON t.id = s.taxon_id
               WHERE lower(split_part(s.synonym_name, ' ', 1) || ' ' ||
                           split_part(s.synonym_name, ' ', 2)) = lower(g.species)
               LIMIT 1)
          ) AS taxon_id
        FROM tmp_garden_class g
      )
      SELECT
        gt.species,
        gt.taxon_id,
        (SELECT native_status FROM ref_state_distribution
           WHERE taxon_id = gt.taxon_id AND state_code = $1
           LIMIT 1) AS state_native_status,
        (SELECT source FROM ref_state_distribution
           WHERE taxon_id = gt.taxon_id AND state_code = $1
           LIMIT 1) AS state_native_source,
        (SELECT native_status FROM ref_usda_traits
           WHERE taxon_id = gt.taxon_id AND native_status IS NOT NULL
           LIMIT 1) AS na_native_status_str,
        EXISTS (
          SELECT 1 FROM ref_noxious_invasive
          WHERE taxon_id = gt.taxon_id
            AND (state_code IS NULL OR state_code = 'US')
        ) AS is_federal_invasive,
        (SELECT designation FROM ref_noxious_invasive
           WHERE taxon_id = gt.taxon_id
             AND (state_code IS NULL OR state_code = 'US')
           LIMIT 1) AS federal_designation,
        (SELECT designation FROM ref_noxious_invasive
           WHERE taxon_id = gt.taxon_id AND state_code = $1
           LIMIT 1) AS user_state_invasive_designation,
        ARRAY(
          SELECT DISTINCT state_code FROM ref_noxious_invasive
          WHERE taxon_id = gt.taxon_id
            AND state_code IS NOT NULL
            AND state_code <> 'US'
        ) AS invasive_state_codes
      FROM garden_taxon gt
    "

    res <- dbGetQuery(con, query, params = list(state_code))

    # RPostgres returns Postgres text[] either as a list-column of character
    # vectors (modern versions) or as a character column holding the array
    # literal "{AA,BB}" (older versions). Normalize to a list of plain
    # character vectors either way.
    res$invasive_state_codes <- if (is.list(res$invasive_state_codes)) {
      lapply(res$invasive_state_codes, function(x) {
        if (is.null(x)) return(character(0))
        x <- x[!is.na(x) & nzchar(x)]
        as.character(x)
      })
    } else {
      lapply(res$invasive_state_codes, function(x) {
        if (is.na(x) || !nzchar(x) || x == "{}") return(character(0))
        inner <- sub("^\\{", "", sub("\\}$", "", x))
        if (!nzchar(inner)) return(character(0))
        parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
        gsub("^\"|\"$", "", parts)
      })
    }
    res
  }, error = function(e) {
    message("db_get_garden_classification_batch error: ", e$message)
    empty
  })
}

#' Get wildlife coverage for a set of garden species
#' Joins species → ref_taxon (genus match) → ref_wildlife_plants → ref_wildlife_interactions → ref_wildlife_species
#' @param species_list Character vector of species names from soil_samples
#' @param pool Database connection pool
#' @return Data frame with columns: garden_species, plant_species_code, wildlife_id, wildlife_type,
#'         wildlife_family, wildlife_common_name, wildlife_scientific_name, interaction_type,
#'         specialist_generalist, functional_group
db_get_wildlife_coverage <- function(species_list, pool, state_code = NULL) {
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

    # Three-class matching:
    #   - Invasive in scope (federal noxious or state-listed): EXCLUDED entirely from
    #     wildlife coverage. Per product rule, invasives never count as wildlife support.
    #   - Native or unknown: GENUS-level join (covers cases where the wildlife table
    #     only documents one congener but the data is genus-applicable).
    #   - Introduced (non-invasive): strict SPECIES-level match — credited only for
    #     interactions documented for that exact species (e.g. parsley → Black Swallowtail).
    nativity_scope <- ""  # appended to ref_state_distribution lookups; empty = anywhere in US
    invasive_state_clause <- "(ni.state_code IS NULL OR ni.state_code = 'US')"
    params <- list()
    next_param <- 1L
    has_state <- !is.null(state_code) && nzchar(state_code) && grepl("^[A-Z]{2}$", state_code)
    if (has_state) {
      nativity_scope <- sprintf("AND sd.state_code = $%d", next_param)
      invasive_state_clause <- sprintf("(ni.state_code IS NULL OR ni.state_code = 'US' OR ni.state_code = $%d)", next_param)
      params <- c(params, list(state_code))
      next_param <- next_param + 1L
    }

    state_join <- ""
    if (has_state) {
      state_join <- sprintf("JOIN ref_wildlife_state_presence wsp ON wsp.wildlife_id = cov.wildlife_id AND wsp.state_code = $%d", next_param)
      params <- c(params, list(state_code))
    }

    query_sql <- sprintf("
      WITH garden_taxon AS (
        -- ref_taxon.scientific_name carries authority (e.g. 'Acer platanoides L.'),
        -- so match on the genus+species prefix only — same pattern as the BONAP ETL.
        SELECT g.species, t.id AS taxon_id
        FROM tmp_garden_species g
        LEFT JOIN ref_taxon t ON
          lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
          = lower(g.species)
      ),
      garden_classified AS (
        SELECT
          gt.species,
          CASE
            WHEN EXISTS (
              SELECT 1 FROM ref_noxious_invasive ni
              WHERE ni.taxon_id = gt.taxon_id
                AND %s
            ) THEN 'invasive'
            WHEN EXISTS (
              SELECT 1 FROM ref_state_distribution sd
              WHERE sd.taxon_id = gt.taxon_id
                AND sd.native_status = 'Native'
                %s
            ) THEN 'native'
            WHEN NOT EXISTS (
              SELECT 1 FROM ref_state_distribution sd
              WHERE sd.taxon_id = gt.taxon_id
                %s
            ) THEN 'unknown'
            ELSE 'introduced'
          END AS native_class
        FROM garden_taxon gt
      ),
      native_cov AS (
        -- genus-level inheritance for species native or undocumented in scope
        SELECT
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
        FROM garden_classified g
        JOIN ref_taxon t ON lower(split_part(t.scientific_name, ' ', 1))
                          = lower(split_part(g.species, ' ', 1))
        JOIN ref_wildlife_plants wp ON wp.taxon_id = t.id
        JOIN ref_wildlife_interactions wi ON wi.plant_species_code = wp.species_code
        JOIN ref_wildlife_species ws ON ws.wildlife_id = wi.wildlife_id
        WHERE g.native_class IN ('native', 'unknown')
      ),
      intro_cov AS (
        -- species-level match only for known introduced species
        SELECT
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
        FROM garden_classified g
        JOIN ref_taxon t ON
          lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
          = lower(g.species)
        JOIN ref_wildlife_plants wp ON wp.taxon_id = t.id
        JOIN ref_wildlife_interactions wi ON wi.plant_species_code = wp.species_code
        JOIN ref_wildlife_species ws ON ws.wildlife_id = wi.wildlife_id
        WHERE g.native_class = 'introduced'
      )
      SELECT DISTINCT
        cov.garden_species,
        cov.plant_species_code,
        cov.wildlife_id,
        cov.wildlife_type,
        cov.wildlife_family,
        cov.wildlife_common_name,
        cov.wildlife_scientific_name,
        cov.interaction_type,
        cov.specialist_generalist,
        cov.functional_group
      FROM (
        SELECT * FROM native_cov
        UNION ALL
        SELECT * FROM intro_cov
      ) cov
      %s
    ", invasive_state_clause, nativity_scope, nativity_scope, state_join)
    if (length(params) > 0) {
      dbGetQuery(con, query_sql, params = params)
    } else {
      dbGetQuery(con, query_sql)
    }
  }, error = function(e) {
    message("Error fetching wildlife coverage: ", e$message)
    data.frame()
  })
}

#' Get species-level documented wildlife counts for a set of garden species.
#' Used by the Introduced Plants panel to show what (if any) wildlife each
#' non-native plant supports without inheriting genus-level associations.
#' @param species_list Character vector of "Genus species" names.
#' @param pool Database pool.
#' @param state_code Two-letter state code (optional). When provided, wildlife is
#'   restricted to species confirmed in that state via ref_wildlife_state_presence.
#' @return Data frame: species, lep_count, bee_count, bird_count, total_count.
#'   Always one row per input species; counts are 0 when no species-level evidence exists.
db_get_species_level_wildlife_counts <- function(species_list, pool, state_code = NULL) {
  empty <- data.frame(
    species = character(0), lep_count = integer(0),
    bee_count = integer(0), bird_count = integer(0), total_count = integer(0),
    stringsAsFactors = FALSE
  )
  if (length(species_list) == 0) return(empty)
  tryCatch({
    con <- poolCheckout(pool)
    on.exit(poolReturn(con), add = TRUE)

    dbExecute(con, "DROP TABLE IF EXISTS tmp_intro_species")
    dbExecute(con, "CREATE TEMP TABLE tmp_intro_species (species TEXT)")
    dbWriteTable(con, "tmp_intro_species",
                 data.frame(species = species_list, stringsAsFactors = FALSE),
                 append = TRUE, temporary = TRUE, row.names = FALSE)

    has_state <- !is.null(state_code) && nzchar(state_code) && grepl("^[A-Z]{2}$", state_code)
    state_join <- ""
    params <- list()
    if (has_state) {
      state_join <- "JOIN ref_wildlife_state_presence wsp ON wsp.wildlife_id = ws.wildlife_id AND wsp.state_code = $1"
      params <- list(state_code)
    }

    sql <- sprintf("
      WITH per_species AS (
        SELECT
          g.species,
          ws.wildlife_id,
          ws.wildlife_type
        FROM tmp_intro_species g
        JOIN ref_taxon t ON
          lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
          = lower(g.species)
        JOIN ref_wildlife_plants wp ON wp.taxon_id = t.id
        JOIN ref_wildlife_interactions wi ON wi.plant_species_code = wp.species_code
        JOIN ref_wildlife_species ws ON ws.wildlife_id = wi.wildlife_id
        %s
      )
      SELECT
        g.species,
        COALESCE(COUNT(DISTINCT CASE WHEN ps.wildlife_type IN ('Moth','Butterfly','Skipper')
                                     THEN ps.wildlife_id END), 0)::int AS lep_count,
        COALESCE(COUNT(DISTINCT CASE WHEN ps.wildlife_type = 'Bee'
                                     THEN ps.wildlife_id END), 0)::int AS bee_count,
        COALESCE(COUNT(DISTINCT CASE WHEN ps.wildlife_type = 'Bird'
                                     THEN ps.wildlife_id END), 0)::int AS bird_count,
        COALESCE(COUNT(DISTINCT ps.wildlife_id), 0)::int AS total_count
      FROM tmp_intro_species g
      LEFT JOIN per_species ps ON ps.species = g.species
      GROUP BY g.species
    ", state_join)

    if (length(params) > 0) {
      dbGetQuery(con, sql, params = params)
    } else {
      dbGetQuery(con, sql)
    }
  }, error = function(e) {
    message("Error fetching species-level wildlife counts: ", e$message)
    empty
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
#' @param state_code Optional 2-letter state code to filter to species confirmed in that state
#' @return Data frame with wildlife_id, wildlife_type, family
db_get_all_wildlife_species <- function(pool, state_code = NULL) {
  tryCatch({
    if (!is.null(state_code) && nzchar(state_code) && grepl("^[A-Z]{2}$", state_code)) {
      dbGetQuery(pool, "
        SELECT DISTINCT ws.wildlife_id, ws.wildlife_type, COALESCE(ws.family, 'Unknown') AS family
        FROM ref_wildlife_species ws
        JOIN ref_wildlife_state_presence wsp ON wsp.wildlife_id = ws.wildlife_id
          AND wsp.state_code = $1
      ", params = list(state_code))
    } else {
      dbGetQuery(pool, "
        SELECT wildlife_id, wildlife_type, COALESCE(family, 'Unknown') AS family
        FROM ref_wildlife_species
      ")
    }
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

    # When state is set, filter plants to native-in-state AND wildlife to confirmed-in-state
    wildlife_state_join <- ""
    if (!is.null(user_state) && nzchar(user_state)) {
      param_idx <- param_idx + 1L
      state_join <- sprintf("
        JOIN ref_state_distribution sd ON sd.taxon_id = wp.taxon_id
          AND sd.state_code = $%d AND sd.native_status = 'Native'
      ", param_idx)
      query_params <- c(query_params, list(user_state))

      param_idx <- param_idx + 1L
      wildlife_state_join <- sprintf("
        JOIN ref_wildlife_state_presence wsp ON wsp.wildlife_id = ws.wildlife_id
          AND wsp.state_code = $%d
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
        %s
        GROUP BY gp.genus
      )
      SELECT * FROM genus_impact
      ORDER BY new_wildlife_count DESC
      LIMIT %s
    ", state_join, life_form_clause, wildlife_state_join, limit_placeholder)

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

# ---------------------------
# User Disable / Ban
# ---------------------------

#' Check whether a user is banned.
#'
#' Failure mode handling:
#'  - "relation does not exist" (table not yet migrated) → return FALSE.
#'    If the ban infrastructure isn't deployed, nobody can be banned by
#'    definition. Treating this as "banned" was a deploy-time foot-gun that
#'    locked out every user on first launch because the shiny_app role
#'    couldn't CREATE TABLE.
#'  - Any other DB error → fail CLOSED (treat as banned) so a Postgres
#'    hiccup doesn't let an active spam attack through unchecked.
db_is_user_disabled <- function(user_id, pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(FALSE)
  p <- if (is.null(pool)) get("pool", envir = .GlobalEnv, inherits = TRUE) else pool
  tryCatch({
    res <- dbGetQuery(p,
      "SELECT 1 FROM disabled_users WHERE user_id = $1 LIMIT 1",
      params = list(user_id))
    nrow(res) > 0
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("disabled_users.*does not exist|relation .* does not exist",
              msg, ignore.case = TRUE)) {
      # Table not deployed — assume nobody is banned.
      return(FALSE)
    }
    message("db_is_user_disabled error: ", msg)
    TRUE
  })
}

db_disable_user <- function(user_id, reason, admin_uid, pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(FALSE)
  p <- if (is.null(pool)) get("pool", envir = .GlobalEnv, inherits = TRUE) else pool
  tryCatch({
    dbExecute(p,
      "INSERT INTO disabled_users (user_id, disabled_by, reason) VALUES ($1, $2, $3)
       ON CONFLICT (user_id) DO UPDATE SET
         disabled_by = EXCLUDED.disabled_by,
         reason = EXCLUDED.reason,
         disabled_at = now()",
      params = list(user_id, admin_uid %||% NA, reason %||% NA))
    TRUE
  }, error = function(e) { message("db_disable_user error: ", e$message); FALSE })
}

db_enable_user <- function(user_id, pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(FALSE)
  p <- if (is.null(pool)) get("pool", envir = .GlobalEnv, inherits = TRUE) else pool
  tryCatch({
    dbExecute(p, "DELETE FROM disabled_users WHERE user_id = $1", params = list(user_id))
    TRUE
  }, error = function(e) { message("db_enable_user error: ", e$message); FALSE })
}

db_list_disabled_users <- function(pool = NULL) {
  p <- if (is.null(pool)) get("pool", envir = .GlobalEnv, inherits = TRUE) else pool
  tryCatch({
    dbGetQuery(p,
      "SELECT user_id, disabled_at, disabled_by, reason
       FROM disabled_users ORDER BY disabled_at DESC")
  }, error = function(e) { data.frame() })
}

#' Flag every soil_samples row from a given user in one shot. Used by the admin
#' bulk-moderation flow when reacting to spam — much faster than per-row flagging.
db_flag_samples_by_user <- function(user_id, reason, pool = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) return(0L)
  p <- if (is.null(pool)) get("pool", envir = .GlobalEnv, inherits = TRUE) else pool
  tryCatch({
    n <- dbExecute(p,
      "UPDATE soil_samples
       SET flagged = TRUE,
           flag_reason = COALESCE(flag_reason, $2)
       WHERE created_by = $1 AND (flagged IS NULL OR flagged = FALSE)",
      params = list(user_id, reason %||% "Bulk flagged: user disabled"))
    as.integer(n)
  }, error = function(e) {
    message("db_flag_samples_by_user error: ", e$message); 0L
  })
}

# ---------------------------
# Submission Rate Limiting
# ---------------------------

#' Check if a user is under the daily submission limit AND not banned.
#' @param user_id Firebase UID
#' @param pool Database connection pool
#' @param max_per_day Maximum submissions per 24 hours (default 20)
#' @return TRUE if allowed to submit, FALSE if blocked
db_check_submission_rate <- function(user_id, pool, max_per_day = 20) {
  if (is.null(user_id) || !nzchar(user_id)) return(FALSE)
  if (db_is_user_disabled(user_id, pool)) return(FALSE)
  tryCatch({
    result <- dbGetQuery(pool,
      "SELECT COUNT(*)::int AS n FROM soil_samples
       WHERE created_by = $1 AND created_at > now() - interval '24 hours'",
      params = list(user_id))
    result$n[1] < max_per_day
  }, error = function(e) {
    message("Error checking submission rate: ", e$message)
    # Fail CLOSED on DB error — pre-launch we'd rather inconvenience a legit
    # user during a DB hiccup than let a spam burst through unmetered.
    FALSE
  })
}

# ---------------------------
# Duplicate Detection
# ---------------------------

#' Check if user already submitted the same species within 24 hours
#' @param species Species name
#' @param user_id Firebase UID
#' @param pool Database connection pool
#' @return Integer count of recent duplicates
db_check_duplicate <- function(species, user_id, pool) {
  if (is.null(species) || is.null(user_id) || !nzchar(species) || !nzchar(user_id)) return(0L)
  tryCatch({
    result <- dbGetQuery(pool,
      "SELECT COUNT(*)::int AS n FROM soil_samples
       WHERE species = $1 AND created_by = $2 AND created_at > now() - interval '24 hours'",
      params = list(species, user_id))
    as.integer(result$n[1])
  }, error = function(e) {
    message("Error checking duplicate: ", e$message)
    0L
  })
}

# ---------------------------
# Export Rate Limiting
# ---------------------------

#' Check if a user is under the daily export limit
#' @param user_id Firebase UID
#' @param pool Database connection pool
#' @param max_per_day Maximum exports per 24 hours (default 10)
#' @return TRUE if under limit, FALSE if over
db_check_export_rate <- function(user_id, pool, max_per_day = 10) {
  if (is.null(user_id) || !nzchar(user_id)) return(TRUE)
  if (db_is_user_disabled(user_id, pool)) return(FALSE)
  tryCatch({
    result <- dbGetQuery(pool,
      "SELECT COUNT(*)::int AS n FROM audit_log
       WHERE user_id = $1 AND action = 'export' AND created_at > now() - interval '24 hours'",
      params = list(user_id))
    result$n[1] < max_per_day
  }, error = function(e) {
    message("Error checking export rate: ", e$message)
    # Fail CLOSED on DB error — see db_check_submission_rate for rationale.
    FALSE
  })
}

# ---------------------------
# Admin Flagging System
# ---------------------------

#' Flag a sample for review
#' @param sample_id ID of the soil_samples record
#' @param reason Text reason for flagging
#' @param pool Database connection pool
#' @return TRUE on success, FALSE on failure
db_flag_sample <- function(sample_id, reason, pool) {
  tryCatch({
    dbExecute(pool,
      "UPDATE soil_samples SET flagged = TRUE, flag_reason = $1 WHERE id = $2",
      params = list(reason, sample_id))
    TRUE
  }, error = function(e) {
    message("Error flagging sample: ", e$message)
    FALSE
  })
}

#' Unflag a sample (clear flag)
#' @param sample_id ID of the soil_samples record
#' @param pool Database connection pool
#' @return TRUE on success, FALSE on failure
db_unflag_sample <- function(sample_id, pool) {
  tryCatch({
    dbExecute(pool,
      "UPDATE soil_samples SET flagged = FALSE, flag_reason = NULL WHERE id = $1",
      params = list(sample_id))
    TRUE
  }, error = function(e) {
    message("Error unflagging sample: ", e$message)
    FALSE
  })
}
