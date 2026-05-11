# Fetch missing common names from USDA API
# Run: Rscript scripts/fetch_common_names.R
# Or with limit: Rscript -e "source('scripts/fetch_common_names.R'); fetch_common_names(limit=500)"

library(DBI)
library(RPostgres)
library(httr2)
library(jsonlite)

get_db_connection <- function() {
  dbConnect(Postgres(),
    host = Sys.getenv("POSTGRES_HOST"),
    port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD"),
    sslmode = "require")
}

#' Fetch common name from USDA API
fetch_common_name <- function(symbol) {
  tryCatch({
    resp <- request(sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantProfile?symbol=%s",
                            URLencode(symbol))) |>
      req_user_agent("EdaphicFlora/1.0 (research)") |>
      req_timeout(15) |>
      req_perform()

    if (resp_status(resp) >= 400) return(NULL)

    data <- resp_body_json(resp)
    if (!is.null(data$CommonName) && nzchar(data$CommonName)) {
      return(data$CommonName)
    }
    NULL
  }, error = function(e) NULL)
}

#' Fetch common names for records missing them
#' @param limit Max records to process (default 1000)
#' @param delay Seconds between API calls (default 0.3)
fetch_common_names <- function(limit = 1000, delay = 0.3) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)

  cat("=== Fetching Missing Common Names ===\n\n")

  # Get records missing common names
  missing <- dbGetQuery(con, "
    SELECT DISTINCT usda_symbol
    FROM ref_usda_traits
    WHERE usda_symbol IS NOT NULL
      AND usda_symbol != ''
      AND (common_name IS NULL OR common_name = '')
    LIMIT $1
  ", params = list(limit))

  if (nrow(missing) == 0) {
    cat("No records missing common names!\n")
    return(invisible(NULL))
  }

  cat(sprintf("Processing %d symbols...\n", nrow(missing)))
  cat(sprintf("Estimated time: %.1f minutes\n\n", nrow(missing) * delay / 60))

  updated <- 0
  not_found <- 0
  errors <- 0

  for (i in seq_len(nrow(missing))) {
    symbol <- missing$usda_symbol[i]

    if (i %% 50 == 0 || i == nrow(missing)) {
      cat(sprintf("[%d/%d] Updated: %d, Not found: %d, Errors: %d\n",
                  i, nrow(missing), updated, not_found, errors))
    }

    common_name <- fetch_common_name(symbol)

    if (!is.null(common_name)) {
      n <- dbExecute(con, "
        UPDATE ref_usda_traits
        SET common_name = $1
        WHERE usda_symbol = $2
      ", params = list(common_name, symbol))
      updated <- updated + n
    } else {
      not_found <- not_found + 1
    }

    Sys.sleep(delay)
  }

  cat("\n=== Complete ===\n")
  cat(sprintf("Updated: %d records\n", updated))
  cat(sprintf("Not found: %d symbols\n", not_found))

  # Show updated coverage
  coverage <- dbGetQuery(con, "
    SELECT
      COUNT(*) as total,
      COUNT(*) FILTER (WHERE common_name IS NOT NULL AND common_name != '') as has_name
    FROM ref_usda_traits
  ")
  cat(sprintf("\nCoverage: %.1f%% (%s/%s)\n",
              100 * coverage$has_name / coverage$total,
              format(coverage$has_name, big.mark = ","),
              format(coverage$total, big.mark = ",")))

  invisible(list(updated = updated, not_found = not_found))
}

# Run if called directly
if (!interactive()) {
  fetch_common_names(limit = 1000)
}

cat("Common name fetcher loaded.\n")
cat("Usage: fetch_common_names(limit = 1000, delay = 0.3)\n")
