# Read-only probe: find Eric's Firebase UID in prod and inspect schema before any write.
suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(dplyr)
})

# Load prod credentials
readRenviron("app/.Renviron")

con <- dbConnect(
  Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_ADMIN_USER"),  # admin role for migrations/probes
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD"),
  sslmode = Sys.getenv("POSTGRES_SSLMODE")
)
on.exit(dbDisconnect(con), add = TRUE)

cat("=== Tables in DB ===\n")
print(dbGetQuery(con, "SELECT tablename FROM pg_tables WHERE schemaname='public' ORDER BY tablename"))

cat("\n=== user_preferences columns ===\n")
print(dbGetQuery(con, "SELECT column_name, data_type FROM information_schema.columns WHERE table_name='user_preferences' ORDER BY ordinal_position"))

cat("\n=== Search user_preferences for any 'winter' / 'eric22' hint ===\n")
rows <- dbGetQuery(con, "SELECT * FROM user_preferences WHERE user_id::text ILIKE '%winter%' OR user_id::text ILIKE '%eric%' LIMIT 10")
print(rows)

cat("\n=== Look for polished/firebase user tables ===\n")
print(dbGetQuery(con, "SELECT tablename FROM pg_tables WHERE schemaname='public' AND (tablename ILIKE '%user%' OR tablename ILIKE '%polish%') ORDER BY tablename"))

# soil_samples row count + recent created_by values
cat("\n=== soil_samples row count + 5 most-recent distinct created_by ===\n")
print(dbGetQuery(con, "SELECT COUNT(*) AS n FROM soil_samples"))
print(dbGetQuery(con, "
  SELECT created_by, COUNT(*) AS n, MAX(created_at) AS last_at
  FROM soil_samples
  WHERE created_by IS NOT NULL
  GROUP BY created_by
  ORDER BY MAX(created_at) DESC
  LIMIT 10"))
