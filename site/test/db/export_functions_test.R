# Exercises site_api.export_my_samples (site/sql/export.sql) as a throwaway
# role with exactly site_writer's rights, inside one transaction that is
# ALWAYS rolled back. export.sql itself is applied inside that transaction, so
# this runs before the file has been applied anywhere and nothing is kept.
#
#   Rscript site/test/db/export_functions_test.R
#
# Needs contribute.sql already applied. Uses the owner login from .Renviron
# in the current directory (the repo root). Prints pass/fail lines only.

readRenviron(".Renviron")
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)][1])))
sql <- paste(readLines(file.path(here, "..", "..", "sql", "export.sql"), warn = FALSE), collapse = "\n")
sql <- sub("(?m)^BEGIN;\\s*$", "", sql, perl = TRUE)
sql <- sub("(?m)^COMMIT;\\s*$", "", sql, perl = TRUE)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"), port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
  dbname = Sys.getenv("POSTGRES_DB"), user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"), sslmode = "require"
)
q <- function(sql, ...) {
  p <- list(...)
  if (length(p)) DBI::dbGetQuery(con, sql, params = p) else DBI::dbGetQuery(con, sql)
}
x <- function(sql) invisible(DBI::dbExecute(con, sql))
fails <- 0
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS" else { fails <<- fails + 1; "FAIL" }, label, "\n")
}
export <- function(uid) jsonlite::fromJSON(q("SELECT site_api.export_my_samples($1)::text AS r", uid)$r, simplifyVector = FALSE)
submit <- function(uid, species) {
  q("SELECT site_api.submit_samples($1, $2::jsonb, $3::jsonb, $4::jsonb, $5::date, $6, $7)::text AS r",
    uid, '{"ph": 6.4, "organic_matter": 5.1, "calcium_ppm": 1200, "texture_class": "Loam"}',
    as.character(jsonlite::toJSON(lapply(species, function(s) list(species = s, outcome = "Thriving")), auto_unbox = TRUE)),
    '{"lat": 41.312345, "lon": -72.923456}', "2026-06-01", "test notes, with a comma", FALSE)
}

DBI::dbBegin(con)
tryCatch({
  DBI::dbExecute(con, sql, immediate = TRUE)   # several statements: simple query protocol
  sp <- q("SELECT taxon_name FROM ref_species_accepted WHERE taxon_name ~ '^[A-Z][a-z]+ [a-z]+$' ORDER BY taxon_name LIMIT 3")$taxon_name

  x("CREATE ROLE tmp_site_writer_test NOLOGIN")
  x("GRANT USAGE ON SCHEMA site_api TO tmp_site_writer_test")
  x("GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA site_api TO tmp_site_writer_test")
  x("GRANT tmp_site_writer_test TO CURRENT_USER")
  x("SET ROLE tmp_site_writer_test")

  x("SAVEPOINT s1")
  denied <- tryCatch({ q("SELECT count(*) FROM audit_log"); FALSE }, error = function(e) grepl("permission denied", conditionMessage(e)))
  x("ROLLBACK TO SAVEPOINT s1")
  check("writer cannot read audit_log", denied)

  check("unknown account gets nothing", export("nobody-at-all")$status == "unknown_user")

  q("SELECT * FROM site_api.resolve_user('test-fb-exp-1', 'nobody1@example.invalid')")
  q("SELECT * FROM site_api.resolve_user('test-fb-exp-2', 'nobody2@example.invalid')")
  e0 <- export("test-fb-exp-1")
  check("no samples yet: ok with no rows", e0$status == "ok" && length(e0$rows) == 0 && e0$remaining == 9)

  check("submit for person 1", jsonlite::fromJSON(submit("test-fb-exp-1", sp[1:2])$r)$status == "ok")
  check("submit for person 2", jsonlite::fromJSON(submit("test-fb-exp-2", sp[3])$r)$status == "ok")

  e1 <- export("test-fb-exp-1")
  r1 <- e1$rows[[1]]
  check("only the person's own rows", e1$status == "ok" && length(e1$rows) == 2 &&
          setequal(vapply(e1$rows, `[[`, "", "species"), sp[1:2]))
  check("their own location, notes and values come back", r1$location_lat == 41.312345 && r1$location_long == -72.923456 &&
          r1$notes == "test notes, with a comma" && r1$ph == 6.4 && r1$calcium_ppm == 1200 && r1$date == "2026-06-01")
  check("never created_by", !("created_by" %in% names(r1)))
  check("flagged is false, created_at is text", identical(r1$flagged, FALSE) && grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", r1$created_at))
  check("columns match the CSV builder (netlify/lib/csv.mjs)", setequal(names(r1), c(
    "id", "date", "species", "cultivar", "outcome", "sun_exposure", "site_hydrology",
    "ph", "organic_matter", "cec_meq", "soluble_salts_ppm", "nitrate_ppm", "ammonium_ppm",
    "phosphorus_ppm", "potassium_ppm", "calcium_ppm", "magnesium_ppm", "sulfur_ppm",
    "iron_ppm", "manganese_ppm", "zinc_ppm", "copper_ppm", "boron_ppm",
    "texture_class", "texture_sand", "texture_silt", "texture_clay",
    "location_lat", "location_long", "ecoregion_l4", "ecoregion_l4_code",
    "inat_url", "notes", "created_at", "flagged")))

  for (i in 3:10) export("test-fb-exp-1")
  check("11th export in a day is refused", export("test-fb-exp-1")$status == "rate_limited")
  check("the limit is per person", export("test-fb-exp-2")$status == "ok")

  x("RESET ROLE")
  logged <- q("SELECT count(*)::int AS n, max(details) AS d FROM audit_log WHERE user_id = 'test-fb-exp-1' AND action = 'export'")
  check("each export is in audit_log as 'export'", logged$n == 10 && grepl("exported own data \\(\\d+ rows\\) \\(edaphicflora.org\\)", logged$d))
  x("INSERT INTO disabled_users (user_id, reason) VALUES ('test-fb-exp-2', 'test')")
  x("SET ROLE tmp_site_writer_test")
  check("disabled accounts cannot export", export("test-fb-exp-2")$status == "disabled")
  x("RESET ROLE")
}, error = function(e) {
  fails <<- fails + 1
  cat("ERROR", conditionMessage(e), "\n")
}, finally = {
  DBI::dbRollback(con)
  DBI::dbDisconnect(con)
})

cat(if (fails == 0) "All export checks passed (rolled back).\n" else sprintf("%d check(s) failed (rolled back).\n", fails))
quit(status = if (fails == 0) 0 else 1)
