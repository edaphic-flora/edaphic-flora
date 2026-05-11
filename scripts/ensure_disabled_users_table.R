# Hotfix: ensure the disabled_users table exists on prod. The app-side
# db_migrate uses the least-privilege shiny_app role, which may lack
# CREATE TABLE; this script uses admin creds to guarantee the table exists.

suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

con <- dbConnect(Postgres(),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname   = Sys.getenv("POSTGRES_DB"),
  user     = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", "require")
)
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

cat("Connected to:", Sys.getenv("POSTGRES_HOST"), "\n")

cat("Checking for disabled_users table...\n")
exists_before <- dbGetQuery(con, "
  SELECT COUNT(*)::int AS n FROM information_schema.tables
  WHERE table_name = 'disabled_users'")$n[1]
cat("  Existed before:", exists_before > 0, "\n")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS disabled_users (
    user_id TEXT PRIMARY KEY,
    disabled_at TIMESTAMPTZ DEFAULT now(),
    disabled_by TEXT,
    reason TEXT
  )
")

# Grant the shiny_app role read access so db_is_user_disabled can query it
shiny_role <- Sys.getenv("SHINY_APP_ROLE", "shiny_app")
tryCatch({
  dbExecute(con, sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON disabled_users TO %s", shiny_role))
  cat(sprintf("  Granted SELECT/INSERT/UPDATE/DELETE on disabled_users to '%s'\n", shiny_role))
}, error = function(e) {
  cat("  GRANT skipped:", conditionMessage(e), "\n")
})

exists_after <- dbGetQuery(con, "
  SELECT COUNT(*)::int AS n FROM information_schema.tables
  WHERE table_name = 'disabled_users'")$n[1]
cat("  Exists now:    ", exists_after > 0, "\n")

cat("\nCurrent disabled_users rows:\n")
print(dbGetQuery(con, "SELECT * FROM disabled_users ORDER BY disabled_at DESC LIMIT 10"))

# Also create schema_version table so the app's fast-path migration check works
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS schema_version (
    id INTEGER PRIMARY KEY DEFAULT 1,
    version INTEGER NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT now(),
    CONSTRAINT schema_version_single_row CHECK (id = 1)
  )
")
tryCatch({
  dbExecute(con, sprintf("GRANT SELECT, INSERT, UPDATE ON schema_version TO %s", shiny_role))
  cat(sprintf("  Granted SELECT/INSERT/UPDATE on schema_version to '%s'\n", shiny_role))
}, error = function(e) {
  cat("  GRANT on schema_version skipped:", conditionMessage(e), "\n")
})

dbExecute(con, "
  INSERT INTO schema_version (id, version) VALUES (1, 3)
  ON CONFLICT (id) DO UPDATE SET version = GREATEST(schema_version.version, 3),
                                 updated_at = now()")
v <- dbGetQuery(con, "SELECT version FROM schema_version LIMIT 1")$version[1]
cat(sprintf("\nschema_version = %d\n", v))
