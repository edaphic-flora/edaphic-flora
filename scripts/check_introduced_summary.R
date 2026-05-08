# Smoke-test the introduced summary helper end-to-end without the Shiny app.
suppressPackageStartupMessages({
  library(DBI); library(pool); library(RPostgres)
})
source("app/R/db.R")
source("app/R/helpers.R")

pool <- dbPool(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)
on.exit(poolClose(pool), add = TRUE)

probes <- c("Acer platanoides", "Petroselinum crispum", "Acer rubrum",
            "Foeniculum vulgare", "Ailanthus altissima")

cat("=== db_get_species_level_wildlife_counts (CT) ===\n")
print(db_get_species_level_wildlife_counts(probes, pool, state_code = "CT"))

cat("\n=== db_get_species_level_wildlife_counts (no state) ===\n")
print(db_get_species_level_wildlife_counts(probes, pool, state_code = NULL))

cat("\nExpected: total_count = 0 for every probe (no species-level data exists yet),\n")
cat("but the helper should return one row per input species without crashing.\n")
