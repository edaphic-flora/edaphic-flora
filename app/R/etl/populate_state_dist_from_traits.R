# Populate ref_state_distribution from existing ref_usda_traits.native_status
# This parses the regional native status (L48, AK, HI) and expands to state level
#
# The native_status field format is like:
#   "AK, CAN, L48, N, Native" = Native in AK, CAN, L48
#   "L48, I, Introduced" = Introduced in L48
#   "AK, CAN, L48, N, I, Native, Introduced" = Native and Introduced (mixed)

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# US State codes by region
L48_STATES <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
  "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
  "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
  "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# Parse native_status string to extract regional status
# Returns a named list: L48, AK, HI with values "Native", "Introduced", "Both", or NA
parse_native_status <- function(status_str) {
  if (is.na(status_str) || !nzchar(status_str)) {
    return(list(L48 = NA, AK = NA, HI = NA))
  }

  # Split by comma and trim
  parts <- str_trim(str_split(status_str, ",")[[1]])

  # Check for regions present
  has_l48 <- "L48" %in% parts
  has_ak <- "AK" %in% parts
  has_hi <- "HI" %in% parts

  # Check for status indicators
  has_native <- any(c("N", "Native") %in% parts)
  has_introduced <- any(c("I", "Introduced", "W") %in% parts)  # W = Waif (introduced)
  has_both_explicit <- "Both" %in% parts || "NI" %in% parts || "N-I" %in% parts

  # Determine status based on combinations
  determine_status <- function(region_present) {
    if (!region_present) return(NA_character_)
    if (has_both_explicit || (has_native && has_introduced)) return("Both")
    if (has_native) return("Native")
    if (has_introduced) return("Introduced")
    NA_character_
  }

  list(
    L48 = determine_status(has_l48),
    AK = determine_status(has_ak),
    HI = determine_status(has_hi)
  )
}

# Main function
populate_state_distribution <- function() {
  message("== Populate ref_state_distribution from ref_usda_traits ==")

  # Connect to database (prefer admin user for DDL operations)
  admin_user <- Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER"))
  admin_pass <- Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD"))

  con <- dbConnect(
    Postgres(),
    host = Sys.getenv("POSTGRES_HOST"),
    port = as.integer(Sys.getenv("POSTGRES_PORT")),
    dbname = Sys.getenv("POSTGRES_DB"),
    user = admin_user,
    password = admin_pass,
    sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
  )
  on.exit(dbDisconnect(con), add = TRUE)
  message(sprintf("Connected as user: %s", admin_user))

  # Ensure table exists (may fail if user lacks DDL permissions - that's OK if table already exists)
  table_created <- tryCatch({
    dbExecute(con, "
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
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_state_dist_taxon ON ref_state_distribution(taxon_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_state_dist_state ON ref_state_distribution(state_code);")
    message("Table created/verified successfully")
    TRUE
  }, error = function(e) {
    message("Note: Could not create table (may already exist or need admin permissions)")
    message("  Error: ", e$message)
    FALSE
  })

  # Check if table exists
  tables <- dbListTables(con)
  if (!"ref_state_distribution" %in% tables) {
    stop("Table ref_state_distribution does not exist. Please run the app first to create it via db_migrate(), or set POSTGRES_ADMIN_USER/PASSWORD.")
  }

  # Get all taxon IDs with native_status
  message("Fetching native status data from ref_usda_traits...")
  traits <- dbGetQuery(con, "
    SELECT taxon_id, native_status
    FROM ref_usda_traits
    WHERE native_status IS NOT NULL AND native_status != ''
  ")
  message(sprintf("Found %d taxa with native_status data", nrow(traits)))

  if (nrow(traits) == 0) {
    message("No data to process.")
    return(invisible(0))
  }

  # Parse each status and expand to states
  message("Parsing native status and expanding to states...")
  all_records <- list()

  for (i in seq_len(nrow(traits))) {
    taxon_id <- traits$taxon_id[i]
    status <- parse_native_status(traits$native_status[i])

    # L48 states
    if (!is.na(status$L48)) {
      for (state in L48_STATES) {
        all_records[[length(all_records) + 1]] <- data.frame(
          taxon_id = taxon_id,
          state_code = state,
          native_status = status$L48,
          stringsAsFactors = FALSE
        )
      }
    }

    # Alaska
    if (!is.na(status$AK)) {
      all_records[[length(all_records) + 1]] <- data.frame(
        taxon_id = taxon_id,
        state_code = "AK",
        native_status = status$AK,
        stringsAsFactors = FALSE
      )
    }

    # Hawaii
    if (!is.na(status$HI)) {
      all_records[[length(all_records) + 1]] <- data.frame(
        taxon_id = taxon_id,
        state_code = "HI",
        native_status = status$HI,
        stringsAsFactors = FALSE
      )
    }

    if (i %% 1000 == 0) {
      message(sprintf("  Processed %d/%d taxa", i, nrow(traits)))
    }
  }

  if (length(all_records) == 0) {
    message("No valid records to insert.")
    return(invisible(0))
  }

  # Combine all records
  df <- bind_rows(all_records)
  message(sprintf("Generated %d state distribution records", nrow(df)))

  # Insert directly in batches using parameterized queries
  message("Loading to database...")

  batch_size <- 500
  n_batches <- ceiling(nrow(df) / batch_size)
  total_inserted <- 0

  for (b in seq_len(n_batches)) {
    start_idx <- (b - 1) * batch_size + 1
    end_idx <- min(b * batch_size, nrow(df))
    batch <- df[start_idx:end_idx, ]

    # Build VALUES clause for batch insert
    values_list <- paste(
      sprintf("(%d, '%s', '%s', 'USDA-traits', now())",
              batch$taxon_id, batch$state_code, batch$native_status),
      collapse = ", "
    )

    sql <- sprintf("
      INSERT INTO ref_state_distribution (taxon_id, state_code, native_status, source, updated_at)
      VALUES %s
      ON CONFLICT (taxon_id, state_code) DO UPDATE SET
        native_status = EXCLUDED.native_status,
        source = EXCLUDED.source,
        updated_at = now();
    ", values_list)

    tryCatch({
      dbExecute(con, sql)
      total_inserted <- total_inserted + nrow(batch)
    }, error = function(e) {
      message(sprintf("  Error in batch %d: %s", b, e$message))
    })

    if (b %% 10 == 0 || b == n_batches) {
      message(sprintf("  Inserted batch %d/%d (%d total records)", b, n_batches, total_inserted))
    }
  }

  # Get final count
  final_count <- dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM ref_state_distribution;")$n[1]
  taxa_count <- dbGetQuery(con, "SELECT COUNT(DISTINCT taxon_id)::int AS n FROM ref_state_distribution;")$n[1]

  message(sprintf("\nDone! ref_state_distribution now has %d records for %d taxa", final_count, taxa_count))

  # Show sample
  sample <- dbGetQuery(con, "
    SELECT t.scientific_name, d.state_code, d.native_status
    FROM ref_state_distribution d
    JOIN ref_taxon t ON t.id = d.taxon_id
    ORDER BY RANDOM()
    LIMIT 10;
  ")
  message("\nSample records:")
  print(sample)

  invisible(final_count)
}

# Run if called directly
if (!interactive() || identical(commandArgs(trailingOnly = TRUE)[1], "run")) {
  populate_state_distribution()
}
