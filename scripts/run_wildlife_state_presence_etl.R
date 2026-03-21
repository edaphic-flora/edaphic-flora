# scripts/run_wildlife_state_presence_etl.R — Run wildlife state presence ETL
#
# Usage:
#   Rscript scripts/run_wildlife_state_presence_etl.R
#
# Requires:
#   - data/gbif_wildlife_state_presence.csv (from scripts/fetch_gbif_wildlife_presence.R)
#   - data/ebird_wildlife_state_presence.csv (from scripts/fetch_ebird_wildlife_presence.R)
#   - .Renviron with POSTGRES_* variables

# Load env vars
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

source("app/R/etl/wildlife_state_presence_etl.R")

wildlife_state_presence_etl_run()
