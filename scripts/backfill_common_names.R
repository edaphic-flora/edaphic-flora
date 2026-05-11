# =============================================================================
# Backfill common names from cached USDA profile JSON files
# =============================================================================
# One-time script: reads cached profile_*.json files, extracts CommonName,
# and UPDATEs ref_usda_traits.common_name for matching USDA symbols.
#
# Usage:
#   cd "C:/Users/toddt/OneDrive/Desktop/edaphic flora"
#   Rscript scripts/backfill_common_names.R
# =============================================================================

library(DBI)
library(RPostgres)
library(jsonlite)

if (file.exists(".Renviron")) readRenviron(".Renviron")

con <- dbConnect(
  Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
)
on.exit(dbDisconnect(con), add = TRUE)

cache_dirs <- c("data/cache/usda_char", "data/cache_usda/usda_char")
files <- unlist(lapply(cache_dirs, function(d) {
  if (dir.exists(d)) list.files(d, pattern = "^profile_.*\\.json$", full.names = TRUE)
}))

cat(sprintf("Found %d cached profile files\n", length(files)))

# Check current state
before <- dbGetQuery(con, "
  SELECT
    count(*) as total,
    count(common_name) FILTER (WHERE common_name IS NOT NULL AND common_name != '') as with_cn
  FROM ref_usda_traits
")
cat(sprintf("Before: %d/%d have common names (%.1f%%)\n",
            as.integer(before$with_cn), as.integer(before$total),
            100 * before$with_cn / before$total))

# Batch: collect symbol -> common_name pairs
updates <- data.frame(symbol = character(), common_name = character(), stringsAsFactors = FALSE)

for (f in files) {
  tryCatch({
    raw <- readLines(f, warn = FALSE)
    if (length(raw) == 0) next
    data <- fromJSON(paste(raw, collapse = ""), simplifyVector = TRUE)

    symbol <- data$Symbol
    cn <- data$CommonName

    if (!is.null(symbol) && !is.null(cn) && nzchar(cn) && !is.na(cn)) {
      updates <- rbind(updates, data.frame(symbol = symbol, common_name = cn, stringsAsFactors = FALSE))
    }
  }, error = function(e) {
    # Skip malformed files silently
  })
}

cat(sprintf("Extracted %d common names from cache\n", nrow(updates)))

if (nrow(updates) == 0) {
  cat("Nothing to update.\n")
  q("no")
}

# Batch update in chunks of 500
chunk_size <- 500
updated_total <- 0

for (i in seq(1, nrow(updates), by = chunk_size)) {
  chunk <- updates[i:min(i + chunk_size - 1, nrow(updates)), ]

  # Create temp table for batch update
  dbExecute(con, "CREATE TEMP TABLE tmp_cn (symbol TEXT, common_name TEXT) ON COMMIT DROP")
  dbWriteTable(con, "tmp_cn", chunk, append = TRUE, row.names = FALSE)

  n <- dbExecute(con, "
    UPDATE ref_usda_traits r
    SET common_name = t.common_name
    FROM tmp_cn t
    WHERE r.usda_symbol = t.symbol
      AND (r.common_name IS NULL OR r.common_name = '')
  ")
  updated_total <- updated_total + n

  dbExecute(con, "DROP TABLE IF EXISTS tmp_cn")

  if (i %% 2000 == 1) {
    cat(sprintf("  ...processed %d/%d\n", min(i + chunk_size - 1, nrow(updates)), nrow(updates)))
  }
}

cat(sprintf("\nUpdated %d rows\n", updated_total))

# Check final state
after <- dbGetQuery(con, "
  SELECT
    count(*) as total,
    count(common_name) FILTER (WHERE common_name IS NOT NULL AND common_name != '') as with_cn
  FROM ref_usda_traits
")
cat(sprintf("After: %d/%d have common names (%.1f%%)\n",
            as.integer(after$with_cn), as.integer(after$total),
            100 * after$with_cn / after$total))
cat("Done!\n")
