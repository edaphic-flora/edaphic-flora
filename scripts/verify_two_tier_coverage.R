# Probe the two-tier wildlife coverage logic for non-native species.
# Native species have already been validated via verify_norway_maple_fix.R.

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

probes <- c(
  "Petroselinum crispum",   # parsley — non-native, supports Black Swallowtail (Papilio polyxenes)
  "Foeniculum vulgare",     # fennel — non-native, supports Black Swallowtail
  "Ailanthus altissima",    # tree of heaven — invasive, no native NA wildlife credit expected
  "Acer platanoides"        # Norway maple — invasive, regression check
)

cat("=== Species-level rows in ref_wildlife_plants ===\n")
for (sp in probes) {
  hit <- dbGetQuery(pool, "
    SELECT wp.species_code, t.scientific_name, wp.life_form
    FROM ref_wildlife_plants wp
    JOIN ref_taxon t ON t.id = wp.taxon_id
    WHERE lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
          = lower($1)
  ", params = list(sp))
  cat("  ", sp, " -> ", if (nrow(hit) > 0) sprintf("%d species-level row(s)", nrow(hit)) else "no species-level row", "\n", sep = "")
  if (nrow(hit) > 0) print(hit)
}

cat("\n=== BONAP nativity classification (CT scope) for each probe ===\n")
for (sp in probes) {
  cls <- dbGetQuery(pool, "
    WITH gt AS (
      SELECT t.id AS taxon_id
      FROM ref_taxon t
      WHERE lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
            = lower($1)
    )
    SELECT
      (SELECT COUNT(*) FROM ref_state_distribution sd
       JOIN gt ON sd.taxon_id = gt.taxon_id
       WHERE sd.native_status='Native' AND sd.state_code='CT')::int AS ct_native,
      (SELECT COUNT(*) FROM ref_state_distribution sd
       JOIN gt ON sd.taxon_id = gt.taxon_id
       WHERE sd.native_status<>'Native' AND sd.state_code='CT')::int AS ct_intro,
      (SELECT COUNT(*) FROM ref_state_distribution sd
       JOIN gt ON sd.taxon_id = gt.taxon_id
       WHERE sd.state_code='CT')::int AS ct_total
  ", params = list(sp))
  cat("  ", sp, ": CT native=", cls$ct_native, ", CT introduced=", cls$ct_intro,
      ", CT total=", cls$ct_total, "\n", sep = "")
}

cat("\n=== db_get_wildlife_coverage with state_code = 'CT' ===\n")
cov_ct <- db_get_wildlife_coverage(probes, pool, state_code = "CT")
cat("Total rows:", nrow(cov_ct), "\n")
if (nrow(cov_ct) > 0) {
  cat("Rows by garden_species:\n")
  print(table(cov_ct$garden_species))
} else {
  cat("(no rows)\n")
}

cat("\n=== db_get_wildlife_coverage with no state filter ===\n")
cov <- db_get_wildlife_coverage(probes, pool, state_code = NULL)
cat("Total rows:", nrow(cov), "\n")
if (nrow(cov) > 0) {
  cat("Rows by garden_species:\n")
  print(table(cov$garden_species))
} else {
  cat("(no rows)\n")
}

cat("\nInterpretation:\n")
cat("  - For each probe: rows > 0 only when ref_wildlife_plants has a species-level entry.\n")
cat("  - If a probe has no species-level row, expect 0 rows (no genus inheritance).\n")
cat("  - Acer platanoides is a regression check — should still produce 0 rows.\n")
