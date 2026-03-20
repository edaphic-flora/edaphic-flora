# scripts/run_wildlife_etl.R — Runner script for wildlife ETL
# Usage: Rscript scripts/run_wildlife_etl.R

if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")
source("app/R/etl/wildlife_etl.R")
wildlife_etl_run()
