# R/db.R - Database connection and helper functions

library(DBI)
library(RPostgres)
library(pool)

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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
    sql <- "SELECT * FROM soil_samples ORDER BY created_at DESC"
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
    sql <- "SELECT * FROM soil_samples WHERE species = $1 ORDER BY created_at DESC"
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
    dbGetQuery(pool, "SELECT * FROM soil_samples WHERE id = $1", params = list(id))
  }, error = function(e) {
    message("Error fetching sample by id: ", e$message)
    data.frame()
  })
}

db_get_user_samples <- function(user_id) {
  if (is.null(user_id) || !nzchar(trimws(user_id))) return(data.frame())
  tryCatch({
    dbGetQuery(pool, "SELECT * FROM soil_samples WHERE created_by = $1 ORDER BY created_at DESC",
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
    result$count[1]
  }, error = function(e) {
    message("Error getting extraction count: ", e$message)
    0
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
  max(0, daily_limit - count)
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
