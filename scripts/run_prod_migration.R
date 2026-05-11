# scripts/run_prod_migration.R — Run pending migrations on prod database

if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

library(DBI)
library(RPostgres)

con <- dbConnect(Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
)

# Add flagged columns
cat("Adding flagged columns...\n")
dbExecute(con, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS flagged BOOLEAN DEFAULT FALSE")
dbExecute(con, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS flag_reason TEXT")

# Add wildlife state presence table
cat("Adding ref_wildlife_state_presence table...\n")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS ref_wildlife_state_presence (
    id SERIAL PRIMARY KEY,
    wildlife_id VARCHAR(50) NOT NULL,
    state_code VARCHAR(2) NOT NULL,
    source VARCHAR(50) NOT NULL,
    observation_count INTEGER,
    updated_at TIMESTAMPTZ DEFAULT now(),
    UNIQUE(wildlife_id, state_code)
  )
")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_wsp_state_wildlife ON ref_wildlife_state_presence(state_code, wildlife_id)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_wsp_wildlife ON ref_wildlife_state_presence(wildlife_id)")

# Add missing indexes on soil_samples
cat("Adding performance indexes...\n")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_samples_created_by ON soil_samples(created_by)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_samples_location ON soil_samples(location_lat, location_long) WHERE location_lat IS NOT NULL")

cat("Done. All migrations applied.\n")
dbDisconnect(con)
