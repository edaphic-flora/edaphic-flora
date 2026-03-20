if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")
suppressMessages(library(DBI))
suppressMessages(library(RPostgres))
con <- dbConnect(Postgres(), host=Sys.getenv("POSTGRES_HOST"), port=as.integer(Sys.getenv("POSTGRES_PORT")), dbname=Sys.getenv("POSTGRES_DB"), user=Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")), password=Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")), sslmode="require")

cat("=== Before cleanup ===\n")
print(dbGetQuery(con, "SELECT wildlife_type, COUNT(*)::int AS n FROM ref_wildlife_species GROUP BY wildlife_type ORDER BY n DESC"))

# Delete species not updated in the latest ETL run (updated_at older than 5 min ago)
n_stale <- dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM ref_wildlife_species WHERE updated_at < now() - interval '5 minutes'")$n[1]
cat(sprintf("\nStale records (not updated in latest ETL): %d\n", n_stale))

if (n_stale > 0) {
  # Delete stale interactions first (FK constraint)
  n_int <- dbExecute(con, "
    DELETE FROM ref_wildlife_interactions
    WHERE wildlife_id IN (
      SELECT wildlife_id FROM ref_wildlife_species WHERE updated_at < now() - interval '5 minutes'
    )
  ")
  cat(sprintf("Deleted %d stale interactions\n", n_int))

  n_del <- dbExecute(con, "DELETE FROM ref_wildlife_species WHERE updated_at < now() - interval '5 minutes'")
  cat(sprintf("Deleted %d stale species\n", n_del))
}

cat("\n=== After cleanup ===\n")
print(dbGetQuery(con, "SELECT wildlife_type, COUNT(*)::int AS n FROM ref_wildlife_species GROUP BY wildlife_type ORDER BY n DESC"))
cat(sprintf("Total interactions: %d\n", dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM ref_wildlife_interactions")$n[1]))

dbDisconnect(con)
