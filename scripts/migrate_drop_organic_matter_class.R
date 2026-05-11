# Move storage from categorical organic_matter_class to numeric organic_matter,
# then drop organic_matter_class. Rationale: lab methods (Morgan / Mehlich-3 /
# Bray / Olsen) define descriptor bins differently, so a class string is not
# comparable across labs. ppm/% is.
#
# Current prod state (audited 2026-05-11):
#   - 85 rows class='High'        + organic_matter=13  (Winter, just backfilled)
#   - 87 rows class='Medium High' + organic_matter=NULL (created_by 3882df3d...)
# Bugbee 2026 CAES Morgan: Medium High = 9-10% → midpoint 9.5%.
#
# Dry-run by default. APPLY=yes to commit.

suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

DRY_RUN <- !identical(tolower(Sys.getenv("APPLY")), "yes")

con <- dbConnect(Postgres(),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname   = Sys.getenv("POSTGRES_DB"),
  user     = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", "require")
)
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

cat("DB host:", Sys.getenv("POSTGRES_HOST"), "\n")
cat("Mode:   ", if (DRY_RUN) "DRY RUN (no writes)" else "APPLY", "\n\n")

# ---- Pre-flight ------------------------------------------------------------
pre <- dbGetQuery(con, "
  SELECT organic_matter_class, COUNT(*)::int AS n,
         COUNT(organic_matter)::int AS n_om_numeric_set,
         (COUNT(*)::int - COUNT(organic_matter)::int) AS n_need_backfill
  FROM soil_samples
  WHERE organic_matter_class IS NOT NULL
  GROUP BY 1 ORDER BY 1")
cat("=== Pre-flight: rows by class ===\n"); print(pre); cat("\n")

# Class -> midpoint % map (Bugbee 2026, used only as a one-time migration
# anchor since we don't have lab provenance per row).
class_to_pct <- list(
  "Very Low"    = 0.5,
  "Low"         = 1.5,
  "Medium Low"  = 3.5,
  "Medium"      = 6.0,
  "Medium High" = 9.5,
  "High"        = 13.0,
  "Very High"   = 17.5
)

unknown <- setdiff(pre$organic_matter_class, names(class_to_pct))
if (length(unknown) > 0) {
  stop("Unknown class values in prod: ", paste(unknown, collapse = ", "))
}

# ---- Plan ------------------------------------------------------------------
plan <- pre[pre$n_need_backfill > 0, , drop = FALSE]
if (nrow(plan) == 0) {
  cat("No rows need backfill. Will only drop the column.\n\n")
} else {
  cat("=== Backfill plan ===\n")
  plan$pct <- vapply(plan$organic_matter_class,
                     function(k) class_to_pct[[k]], numeric(1))
  print(plan); cat("\n")
}

if (DRY_RUN) {
  cat("Would:\n")
  if (nrow(plan) > 0) {
    for (i in seq_len(nrow(plan))) {
      cat(sprintf("  UPDATE %d rows: class='%s' -> organic_matter=%g\n",
                  plan$n_need_backfill[i],
                  plan$organic_matter_class[i],
                  plan$pct[i]))
    }
  }
  cat("  ALTER TABLE soil_samples DROP COLUMN organic_matter_class\n")
  cat("\n*** DRY RUN — set env APPLY=yes to actually run. ***\n")
  quit(status = 0)
}

# ---- Apply in a transaction -------------------------------------------------
dbBegin(con)
ok <- tryCatch({
  if (nrow(plan) > 0) {
    cat("Backfilling:\n")
    for (i in seq_len(nrow(plan))) {
      cls <- plan$organic_matter_class[i]
      pct <- class_to_pct[[cls]]
      n <- dbExecute(con, "
        UPDATE soil_samples
        SET organic_matter = $1
        WHERE organic_matter_class = $2
          AND organic_matter IS NULL", params = list(pct, cls))
      cat(sprintf("  class='%s' -> organic_matter=%g  rows: %d  (expected %d)\n",
                  cls, pct, n, plan$n_need_backfill[i]))
      if (n != plan$n_need_backfill[i]) {
        stop("row count mismatch for class ", cls)
      }
    }
  }

  cat("\nDropping organic_matter_class column...\n")
  dbExecute(con, "ALTER TABLE soil_samples DROP COLUMN organic_matter_class")
  TRUE
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n"); FALSE
})

if (ok) {
  dbCommit(con)
  cat("Committed.\n\n")
} else {
  dbRollback(con)
  cat("Rolled back.\n"); quit(status = 1)
}

# ---- Verify ----------------------------------------------------------------
cat("=== Post-migration verify ===\n")
cols <- dbGetQuery(con, "
  SELECT column_name FROM information_schema.columns
  WHERE table_name = 'soil_samples' AND column_name = 'organic_matter_class'")
cat("organic_matter_class column still present:", nrow(cols) > 0, "\n")

print(dbGetQuery(con, "
  SELECT COUNT(*)::int                          AS total,
         COUNT(organic_matter)::int             AS with_om,
         COUNT(*) FILTER (WHERE organic_matter IS NULL)::int AS without_om
  FROM soil_samples"))
