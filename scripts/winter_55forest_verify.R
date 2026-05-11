suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
if (file.exists(".Renviron"))     readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")
con <- dbConnect(Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
)
on.exit(dbDisconnect(con), add = TRUE)

UID <- "7b9df450-79de-48e9-9960-c9627422b6c2"

cat("=== Total rows for Eric ===\n")
print(dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM soil_samples WHERE created_by = $1", params = list(UID)))

cat("\n=== Rows by zone ===\n")
print(dbGetQuery(con, "
  SELECT split_part(notes, '.', 1) AS zone, COUNT(*)::int AS n,
         MIN(id) AS min_id, MAX(id) AS max_id
  FROM soil_samples WHERE created_by = $1
  GROUP BY 1 ORDER BY 1", params = list(UID)))

cat("\n=== ph / texture / om sanity ===\n")
print(dbGetQuery(con, "
  SELECT ph, texture_class, organic_matter, COUNT(*)::int AS n
  FROM soil_samples WHERE created_by = $1
  GROUP BY 1,2,3 ORDER BY 1", params = list(UID)))

cat("\n=== Sun exposure / outcome / hydrology ===\n")
print(dbGetQuery(con, "
  SELECT sun_exposure, outcome, site_hydrology, COUNT(*)::int AS n
  FROM soil_samples WHERE created_by = $1
  GROUP BY 1,2,3 ORDER BY 1", params = list(UID)))

cat("\n=== Sample row ===\n")
print(dbGetQuery(con, "SELECT id, species, cultivar, ph, sun_exposure, outcome, notes
                       FROM soil_samples WHERE created_by = $1 LIMIT 3", params = list(UID)))
