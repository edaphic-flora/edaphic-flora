# Add common_name column to ref_usda_traits and populate from cache
library(DBI)
library(RPostgres)
library(jsonlite)

con <- dbConnect(
  Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode = "require"
)

cat("=== Adding common_name column ===\n\n")

# Add column if it doesn't exist
tryCatch({
  dbExecute(con, "ALTER TABLE ref_usda_traits ADD COLUMN IF NOT EXISTS common_name TEXT")
  cat("Column added (or already exists)\n")
}, error = function(e) {
  cat("Error adding column:", e$message, "\n")
})

# Populate from cache files
cache_dirs <- c(
  "data/cache_usda/usda_char",
  "app/data/cache/usda_char"
)

# Find the cache directory
cache_dir <- NULL
for (dir in cache_dirs) {
  if (dir.exists(dir)) {
    cache_dir <- dir
    break
  }
}

if (is.null(cache_dir)) {
  cat("No cache directory found. Cannot populate common names.\n")
  dbDisconnect(con)
  stop("Cache directory not found")
}

cat("Using cache directory:", cache_dir, "\n\n")

# Get all profile files
profile_files <- list.files(cache_dir, pattern = "^profile_.*\\.json$", full.names = TRUE)
cat("Found", length(profile_files), "profile files\n\n")

# Process files in batches
updated <- 0
errors <- 0

for (i in seq_along(profile_files)) {
  if (i %% 500 == 0) {
    cat(sprintf("[%d/%d] Updated: %d, Errors: %d\n", i, length(profile_files), updated, errors))
  }

  tryCatch({
    json <- fromJSON(profile_files[i], simplifyVector = TRUE)

    symbol <- json$Symbol
    common_name <- json$CommonName

    if (!is.null(symbol) && !is.null(common_name) && nzchar(common_name)) {
      n <- dbExecute(con, "
        UPDATE ref_usda_traits
        SET common_name = $1
        WHERE usda_symbol = $2 AND (common_name IS NULL OR common_name = '')
      ", params = list(common_name, symbol))

      if (n > 0) updated <- updated + n
    }
  }, error = function(e) {
    errors <<- errors + 1
  })
}

cat("\n=== Complete ===\n")
cat(sprintf("Updated: %d records\n", updated))
cat(sprintf("Errors: %d files\n", errors))

# Show sample
cat("\n=== Sample common names ===\n")
sample <- dbGetQuery(con, "
  SELECT usda_symbol, common_name
  FROM ref_usda_traits
  WHERE common_name IS NOT NULL
  LIMIT 10
")
print(sample)

dbDisconnect(con)
cat("\nDone!\n")
