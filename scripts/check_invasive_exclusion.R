suppressPackageStartupMessages({
  library(DBI); library(pool); library(RPostgres)
})
source("app/R/db.R"); source("app/R/helpers.R")

pool <- dbPool(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)
on.exit(poolClose(pool), add = TRUE)

probes <- c("Acer platanoides", "Ailanthus altissima",
            "Petroselinum crispum", "Foeniculum vulgare", "Allium hollandicum")

cat("=== ref_noxious_invasive listings ===\n")
for (sp in probes) {
  hit <- dbGetQuery(pool, "
    SELECT ni.state_code, ni.designation, ni.source
    FROM ref_noxious_invasive ni
    JOIN ref_taxon t ON t.id = ni.taxon_id
    WHERE lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
          = lower($1)
    ORDER BY ni.state_code NULLS FIRST
  ", params = list(sp))
  cat("\n--", sp, "--\n")
  if (nrow(hit) > 0) print(hit) else cat("  (not on any invasive list)\n")
}

cat("\n=== Coverage classification (CT) ===\n")
cov <- db_get_wildlife_coverage(probes, pool, state_code = "CT")
cat("Total rows:", nrow(cov), "\n")
if (nrow(cov) > 0) print(table(cov$garden_species))

cat("\nExpectation: probes that are invasive in CT or federal-listed should be excluded ('invasive').\n")
cat("Probes that are merely introduced should also be 0 here (no species-level wildlife data).\n")
