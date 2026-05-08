# Verify the Norway maple fix in db_get_wildlife_coverage().
# Run from RStudio with the Edaphic Flora .Renviron loaded so DB env vars are available.

suppressPackageStartupMessages({
  library(DBI)
  library(pool)
  library(RPostgres)
})

source("app/R/db.R")

pool <- dbPool(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)
on.exit(poolClose(pool), add = TRUE)

# Inspect what BONAP says about each Acer species in CT
# Note: ref_taxon.scientific_name carries authority strings, so match on the genus+species prefix.
cat("=== Acer state-distribution rows (CT) ===\n")
acer <- dbGetQuery(pool, "
  SELECT t.scientific_name, sd.state_code, sd.native_status, sd.source
  FROM ref_taxon t
  JOIN ref_state_distribution sd ON sd.taxon_id = t.id
  WHERE lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
        IN ('acer platanoides', 'acer rubrum', 'acer saccharum')
    AND sd.state_code = 'CT'
  ORDER BY t.scientific_name
")
print(acer)

cat("\n=== Sanity: does the prefix lookup find Acer platanoides taxon_id? ===\n")
ap <- dbGetQuery(pool, "
  SELECT id, scientific_name
  FROM ref_taxon
  WHERE lower(split_part(scientific_name, ' ', 1) || ' ' || split_part(scientific_name, ' ', 2))
        = 'acer platanoides'
")
print(ap)

cat("\n=== Coverage WITHOUT state filter ===\n")
test_set <- c("Acer platanoides", "Acer rubrum")
cov_no_state <- db_get_wildlife_coverage(test_set, pool, state_code = NULL)
cat("Total interaction rows:", nrow(cov_no_state), "\n")
cat("By garden_species:\n")
print(table(cov_no_state$garden_species))

cat("\n=== Coverage WITH state_code = 'CT' ===\n")
cov_ct <- db_get_wildlife_coverage(test_set, pool, state_code = "CT")
cat("Total interaction rows:", nrow(cov_ct), "\n")
cat("By garden_species:\n")
print(table(cov_ct$garden_species))

cat("\nExpected: Acer platanoides should produce ZERO rows in both queries.\n")
cat("         Acer rubrum should produce many rows.\n")
