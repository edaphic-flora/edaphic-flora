# R/etl/usda_state_dist_etl.R — Load USDA state-level native status data
# Source: USDA PLANTS Database Complete State Distribution
# Download from: https://plants.usda.gov/home/downloads

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# ---- Configuration -----------------------------------------------------------
# Path to USDA state distribution CSV file
# Format: Symbol, Scientific Name, State, Native Status (N/I/N-I)
usda_state_dist_default <- Sys.getenv("USDA_STATE_DIST_LOCAL", "data/raw/usda/usda_state_dist.csv")

# US state codes for validation
US_STATE_CODES <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# ---- Helpers ----------------------------------------------------------------
normalize_names <- function(v) tolower(gsub("[^a-z0-9]+", "_", trimws(v)))

# Return the ORIGINAL header name (not the normalized token)
pick_orig <- function(cands, nms_norm, names_orig) {
  cn <- normalize_names(cands)
  hit <- which(nms_norm %in% cn)
  if (length(hit)) return(names_orig[hit[1]])
  g <- grep(paste(cn, collapse = "|"), nms_norm, fixed = TRUE)
  if (length(g)) return(names_orig[g[1]])
  NA_character_
}

# Parse native status code to standardized value
parse_native_status <- function(code) {
  code <- trimws(toupper(as.character(code)))
  case_when(
    code %in% c("N", "NATIVE", "YES", "Y") ~ "Native",
    code %in% c("I", "INTRODUCED", "NO") ~ "Introduced",
    code %in% c("N-I", "NI", "N/I", "BOTH", "NATIVE AND INTRODUCED") ~ "Both",
    code %in% c("W", "WAIF", "CULTIVATED") ~ "Introduced",  # Treat waifs as introduced
    TRUE ~ NA_character_
  )
}

# ---- File Reading -----------------------------------------------------------
read_state_dist_table <- function(path) {
  stopifnot(is.character(path), length(path) == 1)
  if (!nzchar(path) || !file.exists(path)) stop("State distribution file not found: ", path)

  # Detect delimiter
  first <- readLines(path, n = 1, warn = FALSE)
  delims <- c("," = ",", "\t" = "\t", ";" = ";", "|" = "|")
  best <- names(which.max(sapply(delims, function(d) length(strsplit(first, d, fixed = TRUE)[[1]]))))

  x <- readr::read_delim(path, delim = delims[[best]],
                         col_types = readr::cols(.default = readr::col_character()))

  # Clean headers
  names_orig <- trimws(names(x))
  names_orig <- sub("^\uFEFF", "", names_orig, useBytes = TRUE)
  nms_norm <- normalize_names(names_orig)

  n <- nrow(x)
  col_or_na <- function(nm) if (!is.na(nm)) x[[nm]] else rep(NA_character_, n)

  # Expected columns: Symbol, Scientific Name, State, Native Status
  # Or wide format with state columns
  cols <- list(
    symbol = pick_orig(c("Symbol", "PLANTS Symbol", "Plant Symbol", "Accepted Symbol"), nms_norm, names_orig),
    scientific = pick_orig(c("Scientific Name", "Scientific Name with Author", "Sci Name"), nms_norm, names_orig),
    state = pick_orig(c("State", "State Code", "ST"), nms_norm, names_orig),
    native_status = pick_orig(c("Native Status", "Native/Introduced", "Nativity", "Status", "L48 Native Status"), nms_norm, names_orig)
  )

  # Check if this is a wide format (one column per state)
  state_cols <- names_orig[toupper(names_orig) %in% US_STATE_CODES]

  if (length(state_cols) > 10) {
    # Wide format - pivot to long
    message("Detected wide format with state columns: ", length(state_cols), " states")

    df <- x %>%
      select(
        usda_symbol = all_of(ifelse(!is.na(cols$symbol), cols$symbol, cols$scientific)),
        all_of(state_cols)
      ) %>%
      pivot_longer(
        cols = all_of(state_cols),
        names_to = "state_code",
        values_to = "native_status_raw"
      ) %>%
      filter(!is.na(native_status_raw) & nzchar(trimws(native_status_raw))) %>%
      mutate(
        state_code = toupper(state_code),
        native_status = parse_native_status(native_status_raw)
      ) %>%
      filter(!is.na(native_status)) %>%
      select(usda_symbol, state_code, native_status)

  } else if (!is.na(cols$state) && !is.na(cols$native_status)) {
    # Long format
    message("Detected long format with State and Native Status columns")

    df <- tibble::tibble(
      usda_symbol = col_or_na(cols$symbol),
      state_code = toupper(col_or_na(cols$state)),
      native_status_raw = col_or_na(cols$native_status)
    ) %>%
      filter(!is.na(usda_symbol) & nzchar(usda_symbol)) %>%
      filter(state_code %in% US_STATE_CODES) %>%
      mutate(native_status = parse_native_status(native_status_raw)) %>%
      filter(!is.na(native_status)) %>%
      select(usda_symbol, state_code, native_status)

  } else {
    stop("Could not determine file format. Expected either:\n",
         "  - Wide format: Symbol + state code columns (AL, AK, etc.)\n",
         "  - Long format: Symbol, State, Native Status columns")
  }

  # Deduplicate (keep first occurrence per symbol+state)
  df <- df %>%
    distinct(usda_symbol, state_code, .keep_all = TRUE)

  message("Parsed ", nrow(df), " state distribution records for ", n_distinct(df$usda_symbol), " taxa")
  df
}

# ---- Database Functions -----------------------------------------------------
ensure_tables <- function(con) {
  # Table is created by db_migrate(), but ensure it exists
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
}

# ---- Main ETL ---------------------------------------------------------------
usda_state_dist_etl_run <- function(dist_path = usda_state_dist_default) {
  message("== Edaphic Flora | USDA State Distribution ETL ==")
  message("Reading: ", dist_path)

  df <- read_state_dist_table(dist_path)
  message("Records to process: ", nrow(df))

  if (nrow(df) == 0) {
    message("No records found. Nothing to load.")
    return(invisible(list(records_in = 0, records_loaded = 0, unmatched = 0)))
  }

  # Connect with admin role for DDL/UPSERT
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

  ensure_tables(con)

  # Stage data in temp table
  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_state_dist;")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tmp_state_dist (
      usda_symbol TEXT,
      state_code VARCHAR(2),
      native_status VARCHAR(20)
    ) ON COMMIT DROP;
  ")
  DBI::dbWriteTable(con, "tmp_state_dist", df, append = TRUE, temporary = TRUE, row.names = FALSE)

  # Match to ref_taxon by usda_symbol and upsert
  result <- DBI::dbExecute(con, "
    WITH matched AS (
      SELECT
        t.id AS taxon_id,
        d.state_code,
        d.native_status
      FROM tmp_state_dist d
      JOIN ref_taxon t ON t.usda_symbol = d.usda_symbol
    )
    INSERT INTO ref_state_distribution (taxon_id, state_code, native_status, source, updated_at)
    SELECT taxon_id, state_code, native_status, 'USDA', now()
    FROM matched
    ON CONFLICT (taxon_id, state_code) DO UPDATE SET
      native_status = EXCLUDED.native_status,
      source = EXCLUDED.source,
      updated_at = now();
  ")

  # Get stats
  records_loaded <- DBI::dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM ref_state_distribution;")$n[1]
  unmatched_count <- DBI::dbGetQuery(con, "
    SELECT COUNT(DISTINCT usda_symbol)::int AS n
    FROM tmp_state_dist d
    WHERE NOT EXISTS (SELECT 1 FROM ref_taxon t WHERE t.usda_symbol = d.usda_symbol);
  ")$n[1]

  # Sample unmatched
  sample_unmatched <- DBI::dbGetQuery(con, "
    SELECT DISTINCT usda_symbol
    FROM tmp_state_dist d
    WHERE NOT EXISTS (SELECT 1 FROM ref_taxon t WHERE t.usda_symbol = d.usda_symbol)
    LIMIT 10;
  ")

  message(sprintf("Total records in ref_state_distribution: %d", records_loaded))
  message(sprintf("Taxa not found in ref_taxon: %d", unmatched_count))
  if (nrow(sample_unmatched) > 0) {
    message("Sample unmatched symbols:")
    for (sym in sample_unmatched$usda_symbol) {
      message("  ", sym)
    }
  }

  # Stats by state
  state_stats <- DBI::dbGetQuery(con, "
    SELECT state_code, native_status, COUNT(*)::int AS n
    FROM ref_state_distribution
    GROUP BY state_code, native_status
    ORDER BY state_code, native_status;
  ")

  message("\nDistribution by state and status:")
  top_states <- state_stats %>%
    group_by(state_code) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    arrange(desc(total)) %>%
    head(10)
  print(top_states)

  invisible(list(
    records_in = nrow(df),
    records_loaded = records_loaded,
    unmatched = unmatched_count
  ))
}

# ---- CLI Entrypoint ---------------------------------------------------------
if (!interactive() && identical(commandArgs(trailingOnly = TRUE)[1], "run")) {
  usda_state_dist_etl_run()
}
