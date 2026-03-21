# R/etl/wildlife_state_presence_etl.R — Load GBIF + eBird wildlife state presence data
# Source: GBIF occurrence downloads (Lep + Bees) and eBird species lists (Birds)
# Input: data/gbif_wildlife_state_presence.csv, data/ebird_wildlife_state_presence.csv
#
# Usage:
#   source("app/R/etl/wildlife_state_presence_etl.R")
#   wildlife_state_presence_etl_run()

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(readr)
  library(dplyr)
  library(stringr)
})

# ---- Configuration -----------------------------------------------------------

gbif_default_path <- "data/gbif_wildlife_state_presence.csv"
ebird_default_path <- "data/ebird_wildlife_state_presence.csv"

US_STATE_CODES <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# ---- Validation --------------------------------------------------------------

validate_presence_data <- function(df, source_name) {
  issues <- character()

  if (!"scientific_name" %in% names(df)) {
    issues <- c(issues, "Missing column: scientific_name")
  }
  if (!"state_code" %in% names(df)) {
    issues <- c(issues, "Missing column: state_code")
  }

  if (length(issues) == 0) {
    bad_states <- setdiff(unique(df$state_code), US_STATE_CODES)
    if (length(bad_states) > 0) {
      issues <- c(issues, sprintf("Invalid state codes: %s", paste(head(bad_states, 10), collapse = ", ")))
    }
  }

  if (length(issues) > 0) {
    message(sprintf("Validation warnings (%s):", source_name))
    for (issue in issues) message("  - ", issue)
  } else {
    message(sprintf("Validation passed (%s): all checks OK", source_name))
  }

  invisible(length(issues) == 0)
}

# ---- Main ETL ----------------------------------------------------------------

wildlife_state_presence_etl_run <- function(gbif_path = gbif_default_path,
                                             ebird_path = ebird_default_path) {
  message("== Edaphic Flora | Wildlife State Presence ETL ==")

  # --- Read inputs ---
  combined <- data.frame()

  if (file.exists(gbif_path)) {
    gbif_df <- read_csv(gbif_path, col_types = cols(.default = col_character(),
                                                     observation_count = col_integer()))
    message(sprintf("GBIF input: %d records (%d species, %d states)",
                    nrow(gbif_df), n_distinct(gbif_df$scientific_name), n_distinct(gbif_df$state_code)))
    validate_presence_data(gbif_df, "GBIF")
    # Ensure source column
    if (!"source" %in% names(gbif_df)) gbif_df$source <- "GBIF"
    combined <- rbind(combined, gbif_df)
  } else {
    message("GBIF file not found: ", gbif_path, " — skipping")
  }

  if (file.exists(ebird_path)) {
    ebird_df <- read_csv(ebird_path, col_types = cols(.default = col_character(),
                                                       observation_count = col_integer()))
    message(sprintf("eBird input: %d records (%d species, %d states)",
                    nrow(ebird_df), n_distinct(ebird_df$scientific_name), n_distinct(ebird_df$state_code)))
    validate_presence_data(ebird_df, "eBird")
    if (!"source" %in% names(ebird_df)) ebird_df$source <- "eBird"
    combined <- rbind(combined, ebird_df)
  } else {
    message("eBird file not found: ", ebird_path, " — skipping")
  }

  if (nrow(combined) == 0) {
    message("No input data found. Nothing to load.")
    return(invisible(list(records_in = 0, matched = 0, loaded = 0, unmatched = 0)))
  }

  # Normalize observation_count
  if (!"observation_count" %in% names(combined)) combined$observation_count <- NA_integer_
  combined$observation_count <- as.integer(combined$observation_count)

  message(sprintf("\nCombined input: %d records (%d unique species)",
                  nrow(combined), n_distinct(combined$scientific_name)))

  # --- Connect to database ---
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
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_wsp_state_wildlife ON ref_wildlife_state_presence(state_code, wildlife_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_wsp_wildlife ON ref_wildlife_state_presence(wildlife_id)")

  # Stage combined data
  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_wildlife_presence")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tmp_wildlife_presence (
      scientific_name TEXT,
      state_code VARCHAR(2),
      source VARCHAR(50),
      observation_count INTEGER
    )
  ")
  staging <- data.frame(
    scientific_name = combined$scientific_name,
    state_code = combined$state_code,
    source = combined$source,
    observation_count = combined$observation_count,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "tmp_wildlife_presence", staging,
                    append = TRUE, temporary = TRUE, row.names = FALSE)

  # Match to ref_wildlife_species on scientific name (genus + epithet)
  message("Matching to ref_wildlife_species...")

  n_upserted <- DBI::dbExecute(con, "
    WITH matched AS (
      SELECT DISTINCT ON (ws.wildlife_id, tp.state_code)
        ws.wildlife_id,
        tp.state_code,
        tp.source,
        tp.observation_count
      FROM tmp_wildlife_presence tp
      JOIN ref_wildlife_species ws ON lower(
        split_part(ws.scientific_name, ' ', 1) || ' ' || split_part(ws.scientific_name, ' ', 2)
      ) = lower(
        split_part(tp.scientific_name, ' ', 1) || ' ' || split_part(tp.scientific_name, ' ', 2)
      )
    )
    INSERT INTO ref_wildlife_state_presence (wildlife_id, state_code, source, observation_count, updated_at)
    SELECT wildlife_id, state_code, source, observation_count, now()
    FROM matched
    ON CONFLICT (wildlife_id, state_code) DO UPDATE SET
      source = EXCLUDED.source,
      observation_count = COALESCE(EXCLUDED.observation_count, ref_wildlife_state_presence.observation_count),
      updated_at = now()
  ")

  # Match stats per wildlife_type
  match_by_type <- DBI::dbGetQuery(con, "
    SELECT ws.wildlife_type,
      COUNT(DISTINCT tp.scientific_name)::int AS input_species,
      COUNT(DISTINCT ws.wildlife_id)::int AS matched_species
    FROM tmp_wildlife_presence tp
    LEFT JOIN ref_wildlife_species ws ON lower(
      split_part(ws.scientific_name, ' ', 1) || ' ' || split_part(ws.scientific_name, ' ', 2)
    ) = lower(
      split_part(tp.scientific_name, ' ', 1) || ' ' || split_part(tp.scientific_name, ' ', 2)
    )
    GROUP BY ws.wildlife_type
    ORDER BY input_species DESC
  ")

  total_matched <- DBI::dbGetQuery(con, "
    SELECT COUNT(DISTINCT scientific_name)::int AS n
    FROM tmp_wildlife_presence tp
    WHERE EXISTS (
      SELECT 1 FROM ref_wildlife_species ws
      WHERE lower(split_part(ws.scientific_name, ' ', 1) || ' ' || split_part(ws.scientific_name, ' ', 2))
          = lower(split_part(tp.scientific_name, ' ', 1) || ' ' || split_part(tp.scientific_name, ' ', 2))
    )
  ")$n[1]

  total_input <- n_distinct(combined$scientific_name)
  unmatched_count <- total_input - total_matched

  # Save unmatched for review
  if (unmatched_count > 0) {
    unmatched <- DBI::dbGetQuery(con, "
      SELECT DISTINCT tp.scientific_name, tp.source
      FROM tmp_wildlife_presence tp
      WHERE NOT EXISTS (
        SELECT 1 FROM ref_wildlife_species ws
        WHERE lower(split_part(ws.scientific_name, ' ', 1) || ' ' || split_part(ws.scientific_name, ' ', 2))
            = lower(split_part(tp.scientific_name, ' ', 1) || ' ' || split_part(tp.scientific_name, ' ', 2))
      )
      ORDER BY tp.scientific_name
    ")
    write_csv(unmatched, "data/wildlife_presence_unmatched.csv")
    message(sprintf("Unmatched species saved to: data/wildlife_presence_unmatched.csv"))
  }

  # Summary
  total_loaded <- DBI::dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM ref_wildlife_state_presence")$n[1]

  message(sprintf("\n=== Wildlife State Presence ETL Complete ==="))
  message(sprintf("Input records: %d", nrow(combined)))
  message(sprintf("Species matched: %d / %d (%.1f%%)",
                  total_matched, total_input,
                  100 * total_matched / max(total_input, 1)))
  message(sprintf("Species unmatched: %d", unmatched_count))
  message(sprintf("Records upserted: %d", n_upserted))
  message(sprintf("Total records in ref_wildlife_state_presence: %d", total_loaded))

  message("\nMatch rate by wildlife type:")
  for (i in seq_len(nrow(match_by_type))) {
    message(sprintf("  %s: %d matched",
                    match_by_type$wildlife_type[i] %||% "(no match)",
                    match_by_type$matched_species[i]))
  }

  # State coverage
  state_counts <- DBI::dbGetQuery(con, "
    SELECT state_code, COUNT(DISTINCT wildlife_id)::int AS n
    FROM ref_wildlife_state_presence
    GROUP BY state_code
    ORDER BY n DESC
  ")
  message(sprintf("\nStates with data: %d", nrow(state_counts)))
  if (nrow(state_counts) > 0) {
    message(sprintf("  Top: %s (%d species), Bottom: %s (%d species)",
                    state_counts$state_code[1], state_counts$n[1],
                    state_counts$state_code[nrow(state_counts)],
                    state_counts$n[nrow(state_counts)]))
  }

  invisible(list(
    records_in = nrow(combined),
    matched = total_matched,
    loaded = total_loaded,
    unmatched = unmatched_count
  ))
}

# ---- CLI Entrypoint ----------------------------------------------------------
if (!interactive() && identical(commandArgs(trailingOnly = TRUE)[1], "run")) {
  wildlife_state_presence_etl_run()
}
