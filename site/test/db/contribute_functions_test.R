# Exercises the site_api database functions (site/sql/contribute.sql) as a
# throwaway role with exactly site_writer's rights, inside one transaction that
# is ALWAYS rolled back: nothing is kept, on any branch.
#
#   Rscript site/test/db/contribute_functions_test.R
#
# Uses the owner login from .Renviron. Prints pass/fail lines only (no emails).

readRenviron(".Renviron")
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
status <- function(res) jsonlite::fromJSON(res$r)$status
submit <- function(uid, plants, soil = '{"ph": 6.4, "organic_matter": 5.1, "texture_class": "Loam", "texture_sand": 40, "texture_silt": 42, "texture_clay": 18}',
                   loc = '{"zip": "06511"}', confirm = FALSE, date = "2026-06-01") {
  q("SELECT site_api.submit_samples($1, $2::jsonb, $3::jsonb, $4::jsonb, $5::date, $6, $7)::text AS r",
    uid, soil, as.character(jsonlite::toJSON(lapply(plants, function(s) list(species = s, outcome = "Thriving")), auto_unbox = TRUE)),
    loc, date, "test", confirm)
}

DBI::dbBegin(con)
tryCatch({
  sp <- q("SELECT taxon_name FROM ref_species_accepted WHERE taxon_name ~ '^[A-Z][a-z]+ [a-z]+$' ORDER BY taxon_name LIMIT 25")$taxon_name
  legacy <- q("SELECT email_lower, polished_uid FROM legacy_users LIMIT 1")

  x("CREATE ROLE tmp_site_writer_test NOLOGIN")
  x("GRANT USAGE ON SCHEMA site_api TO tmp_site_writer_test")
  x("GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA site_api TO tmp_site_writer_test")
  x("GRANT tmp_site_writer_test TO CURRENT_USER")
  x("SET ROLE tmp_site_writer_test")

  x("SAVEPOINT s1")
  denied <- tryCatch({ q("SELECT count(*) FROM soil_samples"); FALSE }, error = function(e) grepl("permission denied", conditionMessage(e)))
  x("ROLLBACK TO SAVEPOINT s1")
  check("writer cannot read soil_samples", denied)
  x("SAVEPOINT s2")
  denied <- tryCatch({ q("SELECT count(*) FROM site_users"); FALSE }, error = function(e) grepl("permission denied", conditionMessage(e)))
  x("ROLLBACK TO SAVEPOINT s2")
  check("writer cannot read site_users (emails)", denied)

  u1 <- q("SELECT * FROM site_api.resolve_user('test-fb-uid-1', 'nobody@example.invalid')")
  check("new account uses its Firebase uid, not disabled, 20 left", u1$user_id == "test-fb-uid-1" && !u1$disabled && u1$remaining_today == 20)
  u1b <- q("SELECT * FROM site_api.resolve_user('test-fb-uid-1', 'changed@example.invalid')")
  check("link never changes after first sign-in", u1b$user_id == "test-fb-uid-1")
  if (nrow(legacy)) {
    u2 <- q("SELECT * FROM site_api.resolve_user('test-fb-uid-2', $1)", toupper(legacy$email_lower))
    check("existing contributor links to their Polished id by email (any case)", u2$user_id == legacy$polished_uid)
  }

  check("unknown user cannot submit", status(submit("not-a-site-user", sp[1])) == "unknown_user")
  r <- jsonlite::fromJSON(submit("test-fb-uid-1", sp[1])$r)
  check("valid submit saved one row", r$status == "ok" && length(r$ids) == 1)
  check("duplicate within 24 h asks first", status(submit("test-fb-uid-1", sp[1])) == "duplicate")
  check("duplicate saved when confirmed", status(submit("test-fb-uid-1", sp[1], confirm = TRUE)) == "ok")
  check("invalid species refused", status(submit("test-fb-uid-1", "Notaplant fakeus")) == "invalid_species")
  check("same plant twice in one submit refused", status(submit("test-fb-uid-1", c(sp[2], sp[2]))) == "invalid")
  check("pH out of range refused", status(submit("test-fb-uid-1", sp[3], soil = '{"ph": 15}')) == "invalid")
  check("pH and OM both missing refused", status(submit("test-fb-uid-1", sp[3], soil = '{"calcium_ppm": 900}')) == "invalid")
  check("unknown soil field refused", status(submit("test-fb-uid-1", sp[3], soil = '{"ph": 6, "lead_ppm": 3}')) == "invalid")
  check("string where a number belongs refused", status(submit("test-fb-uid-1", sp[3], soil = '{"ph": "6"}')) == "invalid")
  check("unknown zip refused", status(submit("test-fb-uid-1", sp[3], loc = '{"zip": "00000"}')) == "invalid")
  check("missing coordinates refused", status(submit("test-fb-uid-1", sp[3], loc = '{"lat": 41.3}')) == "invalid")
  check("future date refused", status(submit("test-fb-uid-1", sp[3], date = "2099-01-01")) == "invalid")

  r <- jsonlite::fromJSON(submit("test-fb-uid-1", sp[4:22])$r)
  check("cap counts the rows being added (2 + 19 > 20)", r$status == "rate_limited" && r$remaining == 18)
  check("up to the cap is fine (2 + 18 = 20)", status(submit("test-fb-uid-1", sp[4:21])) == "ok")
  check("21st sample refused", status(submit("test-fb-uid-1", sp[25], confirm = TRUE)) == "rate_limited")
  check("resolve_user reports 0 left", q("SELECT remaining_today FROM site_api.resolve_user('test-fb-uid-1', '')")$remaining_today == 0)

  mine <- q("SELECT * FROM site_api.my_samples('test-fb-uid-1')")
  check("my_samples lists own rows without location or created_by",
        nrow(mine) == 20 && !any(c("location_lat", "created_by", "notes") %in% names(mine)))
  check("my_samples shows nothing for an unknown id", nrow(q("SELECT * FROM site_api.my_samples('not-a-site-user')")) == 0)
  other <- q("SELECT id FROM site_api.my_samples('test-fb-uid-1') LIMIT 1")$id
  check("cannot delete someone else's sample", !q("SELECT site_api.delete_my_sample('test-fb-uid-2', $1) AS ok", other)$ok)
  check("can delete own sample", q("SELECT site_api.delete_my_sample('test-fb-uid-1', $1) AS ok", other)$ok)
  check("deleting it twice finds nothing", !q("SELECT site_api.delete_my_sample('test-fb-uid-1', $1) AS ok", other)$ok)

  x("RESET ROLE")
  row <- q("SELECT s.created_by, s.location_lat, s.location_long, s.ecoregion_l4, s.texture_class, s.outcome, s.date::text AS d
              FROM soil_samples s WHERE s.created_by = 'test-fb-uid-1' ORDER BY s.id LIMIT 1")
  check("stored row: zip centroid, ecoregion, texture, outcome, date",
        row$created_by == "test-fb-uid-1" && abs(row$location_lat - 41.3) < 0.2 && !is.na(row$ecoregion_l4) &&
          row$texture_class == "Loam" && row$outcome == "Thriving" && row$d == "2026-06-01")
  aud <- q("SELECT count(*) FILTER (WHERE action = 'create')::int AS c, count(*) FILTER (WHERE action = 'delete')::int AS d,
                   count(*) FILTER (WHERE action = 'site_link')::int AS l
              FROM audit_log WHERE user_id = 'test-fb-uid-1'")
  check("audit log: 20 creates, 1 delete, 1 link", aud$c == 20 && aud$d == 1 && aud$l == 1)
  x("INSERT INTO disabled_users (user_id, reason) VALUES ('test-fb-uid-1', 'test')")
  x("SET ROLE tmp_site_writer_test")
  check("disabled account is reported", q("SELECT disabled FROM site_api.resolve_user('test-fb-uid-1', '')")$disabled)
  check("disabled account cannot submit", status(submit("test-fb-uid-1", sp[24], confirm = TRUE)) == "disabled")
  x("RESET ROLE")
  check("site_reader cannot call site_api", !q("SELECT has_function_privilege('site_reader', 'site_api.my_samples(text)', 'EXECUTE') AS ok")$ok)

  # ---- home location
  x("SET ROLE tmp_site_writer_test")
  r <- jsonlite::fromJSON(q("SELECT site_api.save_prefs('test-fb-uid-1', '06511', NULL)::text AS r")$r)
  check("save a home zip: state and city come from the zip", r$status == "ok" && r$home_state == "CT" && r$home_city == "New Haven")
  p <- q("SELECT * FROM site_api.get_prefs('test-fb-uid-1')")
  check("read it back", nrow(p) == 1 && p$home_zipcode == "06511" && p$home_state == "CT")
  r <- jsonlite::fromJSON(q("SELECT site_api.save_prefs('test-fb-uid-1', '', 'vt')::text AS r")$r)
  check("state only (any case) replaces the zip", r$status == "ok" && r$home_state == "VT" && is.null(r$home_zipcode))
  check("unknown zip refused", jsonlite::fromJSON(q("SELECT site_api.save_prefs('test-fb-uid-1', '00000', NULL)::text AS r")$r)$status == "invalid")
  check("bad state refused", jsonlite::fromJSON(q("SELECT site_api.save_prefs('test-fb-uid-1', NULL, 'ZZ')::text AS r")$r)$status == "invalid")
  check("clearing works", jsonlite::fromJSON(q("SELECT site_api.save_prefs('test-fb-uid-1', '', '')::text AS r")$r)$status == "ok" &&
          is.na(q("SELECT home_state FROM site_api.get_prefs('test-fb-uid-1')")$home_state))
  check("unknown user cannot save", jsonlite::fromJSON(q("SELECT site_api.save_prefs('not-a-site-user', '06511', NULL)::text AS r")$r)$status == "unknown_user")
  check("get_prefs shows nothing for an unknown id", nrow(q("SELECT * FROM site_api.get_prefs('not-a-site-user')")) == 0)

  # ---- extraction limits (test-fb-uid-1 is disabled above, so use account 2's link)
  uid2 <- q("SELECT user_id FROM site_api.resolve_user('test-fb-uid-3', 'nobody3@example.invalid')")$user_id
  claim <- function() jsonlite::fromJSON(q("SELECT site_api.claim_extraction($1, 'report.pdf')::text AS r", uid2)$r)
  c1 <- claim()
  check("first extraction claimed, 2 left", c1$status == "ok" && c1$remaining == 2)
  x(sprintf("SELECT site_api.release_extraction('%s', %d)", uid2, c1$id))
  check("a failed call is released and doesn't count", q("SELECT site_api.extractions_left($1) AS n", uid2)$n == 3)
  for (i in 1:3) { ci <- claim(); x(sprintf("SELECT site_api.finish_extraction('%s', %d, 1200)", uid2, ci$id)) }
  check("4th extraction in a day refused", claim()$status == "user_limit")
  check("finished extraction can't be released", {
    x(sprintf("SELECT site_api.release_extraction('%s', %d)", uid2, ci$id)); q("SELECT site_api.extractions_left($1) AS n", uid2)$n == 0 })
  check("disabled account cannot extract", jsonlite::fromJSON(q("SELECT site_api.claim_extraction('test-fb-uid-1', 'x.pdf')::text AS r")$r)$status == "disabled")
  x("RESET ROLE")
}, error = function(e) {
  fails <<- fails + 1
  cat("ERROR", conditionMessage(e), "\n")
}, finally = {
  DBI::dbRollback(con)
  DBI::dbDisconnect(con)
  cat("rolled back;", if (fails == 0) "all checks passed\n" else paste(fails, "failed\n"))
})
