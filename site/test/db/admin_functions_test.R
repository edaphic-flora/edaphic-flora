# Exercises the admin_api database functions (site/sql/admin.sql) as a
# throwaway role with exactly site_admin's rights, inside one transaction that
# is ALWAYS rolled back: nothing is kept, on any branch. admin.sql itself is
# loaded inside that transaction, so this tests the file as it is in the repo
# even on a branch where it has never been run. contribute.sql must already
# have been run there (site_users, legacy_users, site_api).
#
#   Rscript site/test/db/admin_functions_test.R
#
# Run from the repo root (owner login from .Renviron). Prints pass/fail lines
# only (no emails or real user ids).

readRenviron(".Renviron")

# admin.sql, found next to this script, without its own BEGIN/COMMIT so it runs
# inside the test transaction
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
sql_path <- if (is.na(script)) "site/sql/admin.sql" else
  file.path(dirname(normalizePath(script)), "..", "..", "sql", "admin.sql")
sql_lines <- readLines(sql_path, warn = FALSE)
tx <- grepl("^\\s*(BEGIN|COMMIT|ROLLBACK|END)\\s*;\\s*$", sql_lines, ignore.case = TRUE)
if (sum(tx) != 2 || !grepl("^BEGIN;", sql_lines[tx][1]) || !grepl("^COMMIT;", sql_lines[tx][2])) {
  stop("admin.sql should have exactly one BEGIN; and one COMMIT; line")
}
admin_sql <- paste(sql_lines[!tx], collapse = "\n")

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
# TRUE when the statement fails with an error matching `pattern` (the
# transaction carries on: the failure is rolled back to a savepoint)
raises <- function(pattern, sql, ...) {
  x("SAVEPOINT t")
  ok <- tryCatch({ q(sql, ...); FALSE }, error = function(e) grepl(pattern, conditionMessage(e)))
  suppressWarnings(x("ROLLBACK TO SAVEPOINT t"))  # "closing open result set" after the error
  ok
}

ADMIN <- "admin-test@example.invalid"
FB <- "test-admin-fb"
ACTOR <- "test-admin-actor"
TARGET <- "test-target-user"
OTHER <- "test-other-user"
placeholders <- function(n) paste0("$", seq_len(n), collapse = ", ")
# admin_api function returning JSONB -> parsed list
jcall <- function(fn, ..., uid = FB, email = ADMIN) {
  args <- list(uid, email, ...)
  jsonlite::fromJSON(do.call(q, c(list(sprintf("SELECT admin_api.%s(%s)::text AS r", fn, placeholders(length(args)))), args))$r)
}
# admin_api function returning a table -> data frame
tcall <- function(fn, ..., uid = FB, email = ADMIN) {
  args <- list(uid, email, ...)
  do.call(q, c(list(sprintf("SELECT * FROM admin_api.%s(%s)", fn, placeholders(length(args)))), args))
}
samples <- function(user = NA, search = NA, flagged_only = FALSE, before = NA, limit = 50) {
  tcall("samples", flagged_only, user, search, before, limit)
}
sample_state <- function(id) q("SELECT flagged, flag_reason FROM soil_samples WHERE id = $1", id)

DBI::dbBegin(con)
tryCatch({
  DBI::dbExecute(con, admin_sql, immediate = TRUE)
  check("admin.sql loads inside the transaction", TRUE)

  # ---- fixtures (as the owner)
  x(sprintf("INSERT INTO site_users (firebase_uid, user_id, email) VALUES ('%s', '%s', '%s')", FB, ACTOR, ADMIN))
  x(sprintf("INSERT INTO site_users (firebase_uid, user_id, email) VALUES ('test-target-fb', '%s', 'target@example.invalid')", TARGET))
  x(sprintf("INSERT INTO site_admins (email_lower, note) VALUES ('%s', 'test')", ADMIN))
  tids <- q(sprintf("INSERT INTO soil_samples (species, cultivar, created_by, ph, notes, date) VALUES
      ('Testus adminii', NULL, '%1$s', 6.1, 'buy cheap pills at spam.invalid', '2026-06-01'),
      ('Testus adminii', 'Blue', '%1$s', 6.2, NULL, '2026-06-01'),
      ('Testus secundus', NULL, '%1$s', 6.3, 'fine', '2026-06-01')
      RETURNING id", TARGET))$id
  pre <- q(sprintf("INSERT INTO soil_samples (species, created_by, ph, flagged, flag_reason, date)
      VALUES ('Testus adminii', '%s', 6.4, TRUE, 'already flagged', '2026-06-01') RETURNING id", TARGET))$id
  oids <- q(sprintf("INSERT INTO soil_samples (species, created_by, ph, date) VALUES
      ('Testus adminii', '%1$s', 7.0, '2026-06-01'), ('Testus tertius', '%1$s', 7.1, '2026-06-01') RETURNING id", OTHER))$id
  t1 <- tids[1]

  # ---- a role with exactly the rights admin.sql gives site_admin
  x("CREATE ROLE tmp_site_admin_test NOLOGIN")
  x("GRANT USAGE ON SCHEMA admin_api TO tmp_site_admin_test")
  x("GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA admin_api TO tmp_site_admin_test")
  x("REVOKE EXECUTE ON FUNCTION admin_api._actor(TEXT, TEXT), admin_api._known_user(TEXT) FROM tmp_site_admin_test")
  x("GRANT tmp_site_admin_test TO CURRENT_USER")
  x("SET ROLE tmp_site_admin_test")

  for (t in c("soil_samples", "site_admins", "site_users", "audit_log", "disabled_users")) {
    check(paste("admin login cannot read", t), raises("permission denied", sprintf("SELECT count(*) FROM %s", t)))
  }
  check("admin login cannot call the helpers", raises("permission denied", "SELECT admin_api._actor($1, $2)", FB, ADMIN))

  # ---- every function refuses someone who is not in site_admins
  nobody <- "nobody@example.invalid"
  refused <- function(sql, ...) raises("not a site admin", sql, ...)
  check("whoami refuses a non-admin", refused("SELECT admin_api.whoami($1, $2)", FB, nobody))
  check("whoami refuses a missing email", refused("SELECT admin_api.whoami($1, $2)", FB, NA))
  check("whoami refuses an empty account id", refused("SELECT admin_api.whoami($1, $2)", "", ADMIN))
  check("samples refuses a non-admin", refused("SELECT * FROM admin_api.samples($1, $2, FALSE, NULL, NULL, NULL, 10)", FB, nobody))
  check("contributors refuses a non-admin", refused("SELECT * FROM admin_api.contributors($1, $2, 10)", FB, nobody))
  check("disabled_accounts refuses a non-admin", refused("SELECT * FROM admin_api.disabled_accounts($1, $2)", FB, nobody))
  check("audit_entries refuses a non-admin", refused("SELECT * FROM admin_api.audit_entries($1, $2, NULL, NULL, NULL, 10)", FB, nobody))
  check("flag_sample refuses a non-admin", refused("SELECT admin_api.flag_sample($1, $2, $3, 'x')", FB, nobody, t1))
  check("unflag_sample refuses a non-admin", refused("SELECT admin_api.unflag_sample($1, $2, $3)", FB, nobody, pre))
  check("flag_user_samples refuses a non-admin", refused("SELECT admin_api.flag_user_samples($1, $2, $3, 'x')", FB, nobody, TARGET))
  check("disable_user refuses a non-admin", refused("SELECT admin_api.disable_user($1, $2, $3, 'x', TRUE)", FB, nobody, TARGET))
  check("enable_user refuses a non-admin", refused("SELECT admin_api.enable_user($1, $2, $3)", FB, nobody, TARGET))
  check("a near-miss email is refused", refused("SELECT admin_api.whoami($1, $2)", FB, paste0("x", ADMIN)))

  # ---- who is acting
  me <- jcall("whoami", email = paste0("  ", toupper(ADMIN), " "))
  check("admin email matches in any case, trimmed; actor is their contributor id", me$actor == ACTOR)
  check("whoami counts include the fixtures", me$samples >= 6 && me$flagged >= 1 && me$samples_24h >= 6 && me$contributors >= 2)
  check("whoami lists audit actions", is.character(me$actions) || length(me$actions) == 0)
  check("an admin who never used /account/ acts as their Firebase uid", jcall("whoami", uid = "fresh-fb-uid")$actor == "fresh-fb-uid")

  # ---- browsing samples
  s <- samples(user = TARGET)
  check("samples for one contributor, newest first", nrow(s) == 4 && identical(s$id, sort(c(tids, pre), decreasing = TRUE)))
  check("samples include notes and flag state but no location or email",
        "notes" %in% names(s) && "flag_reason" %in% names(s) &&
          !any(c("location_lat", "location_long", "email") %in% names(s)))
  check("search finds text in notes", identical(samples(search = "cheap PILLS", user = TARGET)$id, t1))
  check("search by #id", identical(samples(search = paste0("#", t1))$id, t1))
  check("search by cultivar", identical(samples(search = "blue", user = TARGET)$id, tids[2]))
  check("% in a search is literal", nrow(samples(search = "%", user = TARGET)) == 0)
  check("flagged only", identical(samples(user = TARGET, flagged_only = TRUE)$id, pre))
  p1 <- samples(user = TARGET, limit = 2)
  p2 <- samples(user = TARGET, limit = 2, before = min(p1$id))
  check("paging with before_id", nrow(p1) == 2 && nrow(p2) == 2 && max(p2$id) < min(p1$id))
  check("limit is capped at 200", nrow(samples(limit = 100000)) <= 200)

  # ---- flag / unflag one sample
  r <- jcall("flag_sample", t1, "   ")
  check("flag a sample (blank reason gets a default)", r$status == "ok" && r$changed &&
          { x("RESET ROLE"); st <- sample_state(t1); x("SET ROLE tmp_site_admin_test"); st$flagged && st$flag_reason == "Flagged by admin" })
  check("flagging again with the same reason changes nothing", !jcall("flag_sample", t1, "")$changed)
  check("a new reason replaces the old one", jcall("flag_sample", t1, "Spam link")$changed &&
          samples(search = paste0("#", t1))$flag_reason == "Spam link")
  check("flag an unknown sample", jcall("flag_sample", -1L, "x")$status == "not_found")
  check("an over-long reason is refused", jcall("flag_sample", t1, strrep("a", 501))$status == "invalid")
  r <- jcall("unflag_sample", t1)
  check("unflag clears the flag and the reason", r$status == "ok" && r$changed &&
          { s1 <- samples(search = paste0("#", t1)); !s1$flagged && is.na(s1$flag_reason) })
  check("unflagging again changes nothing", !jcall("unflag_sample", t1)$changed)
  check("unflag an unknown sample", jcall("unflag_sample", -1L)$status == "not_found")

  # ---- bulk flag
  r <- jcall("flag_user_samples", TARGET, "Spam account")
  check("bulk flag flags only the unflagged ones", r$status == "ok" && r$flagged == 3)
  s <- samples(user = TARGET)
  check("bulk flag keeps an existing reason", all(s$flagged) && s$flag_reason[s$id == pre] == "already flagged" &&
          s$flag_reason[s$id == t1] == "Spam account")
  check("bulk flag again flags nothing", jcall("flag_user_samples", TARGET, "x")$flagged == 0)
  check("bulk flag an unknown user", jcall("flag_user_samples", "no-such-user", "x")$status == "unknown_user")

  # ---- disable / re-enable
  r <- jcall("disable_user", TARGET, "spam", FALSE)
  check("disable a contributor", r$status == "ok" && r$flagged == 0)
  check("their samples show as disabled", all(samples(user = TARGET)$user_disabled))
  r <- jcall("disable_user", TARGET, "spam, second look", FALSE)
  d <- tcall("disabled_accounts")
  d <- d[d$user_id == TARGET, ]
  check("disabling again updates the reason, one row, records who did it",
        r$status == "ok" && nrow(d) == 1 && d$reason == "spam, second look" && d$disabled_by == ACTOR &&
          d$n_samples == 4 && d$n_flagged == 4)
  check("cannot disable yourself (contributor id)", jcall("disable_user", ACTOR, "x", FALSE)$status == "self")
  check("cannot disable yourself (Firebase uid)", jcall("disable_user", FB, "x", FALSE)$status == "self")
  check("disable an unknown user", jcall("disable_user", "no-such-user", "x", FALSE)$status == "unknown_user")
  check("disable with no user", jcall("disable_user", NA, "x", FALSE)$status == "unknown_user")
  r <- jcall("disable_user", OTHER, "bot", TRUE)
  so <- samples(user = OTHER)
  check("disable and flag their samples in one step", r$status == "ok" && r$flagged == 2 &&
          all(so$flagged) && all(so$flag_reason == "User disabled: bot"))

  cn <- tcall("contributors", 2000L)
  ct <- cn[cn$user_id == TARGET, ]
  check("contributors list counts and status", nrow(ct) == 1 && ct$n_samples == 4 && ct$n_flagged == 4 &&
          ct$disabled && ct$disable_reason == "spam, second look" && !("email" %in% names(cn)))

  x("RESET ROLE")
  check("a disabled account is disabled for contributing too",
        q("SELECT disabled FROM site_api.resolve_user('test-target-fb', '')")$disabled)
  x("SET ROLE tmp_site_admin_test")

  check("re-enable", jcall("enable_user", TARGET)$status == "ok" && !(TARGET %in% tcall("disabled_accounts")$user_id))
  check("re-enabling again: not disabled", jcall("enable_user", TARGET)$status == "not_disabled")
  check("re-enabling keeps their flags", all(samples(user = TARGET)$flagged))
  x("RESET ROLE")
  check("a re-enabled account can contribute again",
        !q("SELECT disabled FROM site_api.resolve_user('test-target-fb', '')")$disabled)
  x("SET ROLE tmp_site_admin_test")

  # ---- audit log
  a <- tcall("audit_entries", NA, TARGET, NA, 100L)
  check("audit: every change about a user is findable by their id",
        sum(a$action == "disable_user") == 2 && sum(a$action == "enable_user") == 1 && sum(a$action == "bulk_flag") == 1)
  check("audit: newest first, actor recorded", all(diff(a$id) < 0) && all(a$user_id == ACTOR))
  f <- tcall("audit_entries", "flag", as.character(t1), NA, 100L)
  u <- tcall("audit_entries", "unflag", as.character(t1), NA, 100L)
  f <- f[f$record_id %in% t1, ]
  u <- u[u$record_id %in% t1, ]
  check("audit: one row per real change (2 flags, 1 unflag; repeats not logged)",
        nrow(f) == 2 && nrow(u) == 1 && all(f$table_name == "soil_samples") && all(f$user_id == ACTOR))
  check("audit: reason is in the details", any(grepl("Spam link", f$details)))
  ao <- tcall("audit_entries", "bulk_flag", OTHER, NA, 100L)
  check("audit: disable with flagging logs the bulk flag too", nrow(ao) == 1 && grepl("2 samples", ao$details))
  a1 <- tcall("audit_entries", NA, NA, NA, 1L)
  a2 <- tcall("audit_entries", NA, NA, a1$id, 1L)
  check("audit: paging with before_id", nrow(a1) == 1 && nrow(a2) == 1 && a2$id < a1$id)
  x("RESET ROLE")

  # ---- the other logins get nothing here
  for (role in c("site_writer", "site_reader")) {
    if (nrow(q("SELECT 1 FROM pg_roles WHERE rolname = $1", role))) {
      check(paste(role, "has no access to admin_api or site_admins"),
            !q("SELECT has_schema_privilege($1, 'admin_api', 'USAGE') AS ok", role)$ok &&
              !q("SELECT has_function_privilege($1, 'admin_api.flag_sample(text,text,integer,text)', 'EXECUTE') AS ok", role)$ok &&
              !q("SELECT has_table_privilege($1, 'site_admins', 'SELECT') AS ok", role)$ok)
    }
  }
  if (nrow(q("SELECT 1 FROM pg_roles WHERE rolname = 'site_admin'"))) {
    check("site_admin can call the functions but not the helpers",
          q("SELECT has_function_privilege('site_admin', 'admin_api.flag_sample(text,text,integer,text)', 'EXECUTE') AS ok")$ok &&
            !q("SELECT has_function_privilege('site_admin', 'admin_api._actor(text,text)', 'EXECUTE') AS ok")$ok)
  }
}, error = function(e) {
  fails <<- fails + 1
  cat("ERROR", conditionMessage(e), "\n")
}, finally = {
  DBI::dbRollback(con)
  DBI::dbDisconnect(con)
  cat("rolled back;", if (fails == 0) "all checks passed\n" else paste(fails, "failed\n"))
})
