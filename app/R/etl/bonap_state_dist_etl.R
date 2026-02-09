# R/etl/bonap_state_dist_etl.R — Load BONAP state-level nativity data
# Source: BONAP Taxonomic Data Center (bonap.net/tdc)
# Input: data/bonap_state_nativity_compiled.csv (from scripts/scrape_bonap_nativity.R)
#
# Usage:
#   source("app/R/etl/bonap_state_dist_etl.R")
#   bonap_state_dist_etl_run()
#   bonap_state_dist_etl_run("data/bonap_state_nativity_compiled.csv")

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(readr)
  library(dplyr)
  library(stringr)
})

# ---- Configuration -----------------------------------------------------------

bonap_default_path <- "data/bonap_state_nativity_compiled.csv"

US_STATE_CODES <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# ---- Validation --------------------------------------------------------------

validate_bonap_data <- function(df) {
  issues <- character()

  # Check state codes
  bad_states <- setdiff(unique(df$state_code), US_STATE_CODES)
  if (length(bad_states) > 0) {
    issues <- c(issues, sprintf("Invalid state codes: %s", paste(bad_states, collapse = ", ")))
  }

  # Check native_status values
  valid_statuses <- c("Native", "Introduced", "Both")
  bad_statuses <- setdiff(unique(df$native_status), valid_statuses)
  if (length(bad_statuses) > 0) {
    issues <- c(issues, sprintf("Invalid native_status values: %s", paste(bad_statuses, collapse = ", ")))
  }

  # Check all states have data
  states_with_data <- unique(df$state_code)
  missing_states <- setdiff(US_STATE_CODES, states_with_data)
  if (length(missing_states) > 0) {
    issues <- c(issues, sprintf("Missing states: %s", paste(missing_states, collapse = ", ")))
  }

  # Check species counts per state
  state_counts <- df %>% count(state_code)
  low_states <- state_counts %>% filter(n < 1000)
  if (nrow(low_states) > 0) {
    for (i in seq_len(nrow(low_states))) {
      issues <- c(issues, sprintf("Low species count for %s: %d",
                                  low_states$state_code[i], low_states$n[i]))
    }
  }

  if (length(issues) > 0) {
    message("Validation warnings:")
    for (issue in issues) message("  - ", issue)
  } else {
    message("Validation passed: all checks OK")
  }

  invisible(length(issues) == 0)
}

# ---- Main ETL ----------------------------------------------------------------

bonap_state_dist_etl_run <- function(csv_path = bonap_default_path) {
  message("== Edaphic Flora | BONAP State Distribution ETL ==")
  message("Reading: ", csv_path)

  if (!file.exists(csv_path)) {
    stop("BONAP compiled CSV not found: ", csv_path,
         "\nRun scripts/scrape_bonap_nativity.R first.")
  }

  df <- read_csv(csv_path, col_types = cols(.default = col_character()))
  message(sprintf("Input records: %d (%d unique species, %d states)",
                  nrow(df), n_distinct(df$scientific_name), n_distinct(df$state_code)))

  # Validate
  validate_bonap_data(df)

  if (nrow(df) == 0) {
    message("No records found. Nothing to load.")
    return(invisible(list(records_in = 0, matched = 0, loaded = 0, unmatched = 0)))
  }

  # Connect to database
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("POSTGRES_HOST"),
    port = as.integer(Sys.getenv("POSTGRES_PORT")),
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
    password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
    sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Ensure table exists
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_state_distribution (
      id SERIAL PRIMARY KEY,
      taxon_id INTEGER REFERENCES ref_taxon(id) ON DELETE CASCADE,
      state_code VARCHAR(2) NOT NULL,
      native_status VARCHAR(20) NOT NULL,
      source VARCHAR(50) DEFAULT 'USDA',
      updated_at TIMESTAMPTZ DEFAULT now(),
      UNIQUE(taxon_id, state_code)
    );
  ")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_state_dist_taxon ON ref_state_distribution(taxon_id);")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_state_dist_state ON ref_state_distribution(state_code);")

  # Stage data in temp table
  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_bonap_state_dist;")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tmp_bonap_state_dist (
      scientific_name TEXT,
      state_code VARCHAR(2),
      native_status VARCHAR(20)
    );
  ")
  DBI::dbWriteTable(con, "tmp_bonap_state_dist", df, append = TRUE, temporary = TRUE, row.names = FALSE)

  # Match to ref_taxon using multiple strategies:
  # 1. Exact match on genus+species (first two words of scientific_name)
  # 2. Match via case-insensitive comparison
  message("Matching species to ref_taxon...")

  # UPSERT matched records with source='BONAP'
  n_upserted <- DBI::dbExecute(con, "
    WITH matched AS (
      SELECT DISTINCT ON (t.id, d.state_code)
        t.id AS taxon_id,
        d.state_code,
        d.native_status
      FROM tmp_bonap_state_dist d
      JOIN ref_taxon t ON lower(
        split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2)
      ) = lower(d.scientific_name)
    )
    INSERT INTO ref_state_distribution (taxon_id, state_code, native_status, source, updated_at)
    SELECT taxon_id, state_code, native_status, 'BONAP', now()
    FROM matched
    ON CONFLICT (taxon_id, state_code) DO UPDATE SET
      native_status = EXCLUDED.native_status,
      source = 'BONAP',
      updated_at = now();
  ")

  # Get stats
  total_loaded <- DBI::dbGetQuery(con, "
    SELECT COUNT(*)::int AS n FROM ref_state_distribution;
  ")$n[1]

  bonap_loaded <- DBI::dbGetQuery(con, "
    SELECT COUNT(*)::int AS n FROM ref_state_distribution WHERE source = 'BONAP';
  ")$n[1]

  matched_species <- DBI::dbGetQuery(con, "
    SELECT COUNT(DISTINCT d.scientific_name)::int AS n
    FROM tmp_bonap_state_dist d
    JOIN ref_taxon t ON lower(
      split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2)
    ) = lower(d.scientific_name);
  ")$n[1]

  total_species <- DBI::dbGetQuery(con, "
    SELECT COUNT(DISTINCT scientific_name)::int AS n FROM tmp_bonap_state_dist;
  ")$n[1]

  unmatched_count <- total_species - matched_species

  # Sample unmatched species
  unmatched_sample <- DBI::dbGetQuery(con, "
    SELECT DISTINCT d.scientific_name
    FROM tmp_bonap_state_dist d
    WHERE NOT EXISTS (
      SELECT 1 FROM ref_taxon t
      WHERE lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
            = lower(d.scientific_name)
    )
    LIMIT 20;
  ")

  # Save unmatched to file for review
  if (unmatched_count > 0) {
    all_unmatched <- DBI::dbGetQuery(con, "
      SELECT DISTINCT d.scientific_name
      FROM tmp_bonap_state_dist d
      WHERE NOT EXISTS (
        SELECT 1 FROM ref_taxon t
        WHERE lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
              = lower(d.scientific_name)
      )
      ORDER BY d.scientific_name;
    ")
    write_csv(all_unmatched, "data/bonap_unmatched.csv")
    message(sprintf("Unmatched species saved to: data/bonap_unmatched.csv"))
  }

  # Summary
  message(sprintf("\n=== BONAP ETL Complete ==="))
  message(sprintf("Input records: %d", nrow(df)))
  message(sprintf("Species matched: %d / %d (%.1f%%)",
                  matched_species, total_species,
                  100 * matched_species / max(total_species, 1)))
  message(sprintf("Species unmatched: %d", unmatched_count))
  message(sprintf("Records upserted: %d", n_upserted))
  message(sprintf("Total BONAP records in DB: %d", bonap_loaded))
  message(sprintf("Total records in ref_state_distribution: %d", total_loaded))

  if (nrow(unmatched_sample) > 0) {
    message("\nSample unmatched species:")
    for (sp in unmatched_sample$scientific_name) {
      message("  ", sp)
    }
  }

  # Source distribution
  source_dist <- DBI::dbGetQuery(con, "
    SELECT source, COUNT(*)::int AS n FROM ref_state_distribution GROUP BY source ORDER BY n DESC;
  ")
  message("\nRecords by source:")
  for (j in seq_len(nrow(source_dist))) {
    message(sprintf("  %s: %d", source_dist$source[j], source_dist$n[j]))
  }

  invisible(list(
    records_in = nrow(df),
    matched = matched_species,
    loaded = bonap_loaded,
    unmatched = unmatched_count
  ))
}

# ---- CLI Entrypoint ----------------------------------------------------------
if (!interactive() && identical(commandArgs(trailingOnly = TRUE)[1], "run")) {
  bonap_state_dist_etl_run()
}
