suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")
con <- dbConnect(Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode = Sys.getenv("POSTGRES_SSLMODE", "require"))
on.exit(dbDisconnect(con), add = TRUE)

UID <- "3882df3d-cf87-433a-9238-258b3a6b8742"

cat("=== Total rows for Todd ===\n")
print(dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM soil_samples WHERE created_by = $1", params = list(UID)))

cat("\n=== Just-inserted batch (510..544) ===\n")
print(dbGetQuery(con, "
  SELECT split_part(notes, '(', 1) AS zone, COUNT(*)::int AS n,
         MIN(id) AS min_id, MAX(id) AS max_id,
         AVG(ph)::numeric(4,2) AS avg_ph
  FROM soil_samples
  WHERE id BETWEEN 510 AND 544 AND created_by = $1
  GROUP BY 1 ORDER BY 1", params = list(UID)))

cat("\n=== Outcome / sun breakdown for new rows ===\n")
print(dbGetQuery(con, "
  SELECT outcome, sun_exposure, COUNT(*)::int AS n
  FROM soil_samples WHERE id BETWEEN 510 AND 544 AND created_by = $1
  GROUP BY 1,2 ORDER BY 1,2", params = list(UID)))

cat("\n=== Nutrient ppm sanity (per zone) ===\n")
print(dbGetQuery(con, "
  SELECT MIN(id) AS first_id, ph, nitrate_ppm, ammonium_ppm, phosphorus_ppm,
         potassium_ppm, calcium_ppm, magnesium_ppm, COUNT(*)::int AS n
  FROM soil_samples WHERE id BETWEEN 510 AND 544 AND created_by = $1
  GROUP BY ph, nitrate_ppm, ammonium_ppm, phosphorus_ppm, potassium_ppm, calcium_ppm, magnesium_ppm
  ORDER BY MIN(id)", params = list(UID)))
