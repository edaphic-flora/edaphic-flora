# Convert large CSV files to RDS for faster loading and lower memory usage
# Run from app/ directory: Rscript scripts/convert_to_rds.R

message("Converting CSV files to RDS format...")

# Species database (21MB CSV -> ~8MB RDS)
if (file.exists("species_accepted.csv")) {
  message("Converting species_accepted.csv...")
  species <- read.csv("species_accepted.csv", stringsAsFactors = FALSE)
  saveRDS(species, "species_accepted.rds", compress = "xz")
  message(sprintf("  CSV: %.1f MB -> RDS: %.1f MB",
                  file.size("species_accepted.csv") / 1e6,
                  file.size("species_accepted.rds") / 1e6))
}

# Zipcode lookup (if CSV exists but RDS doesn't)
if (file.exists("data/zipcode_lookup.csv") && !file.exists("data/zipcode_lookup.rds")) {
  message("Converting zipcode_lookup.csv...")
  zipcodes <- read.csv("data/zipcode_lookup.csv", stringsAsFactors = FALSE,
                       colClasses = c(zipcode = "character"))
  saveRDS(zipcodes, "data/zipcode_lookup.rds", compress = "xz")
  message(sprintf("  CSV: %.1f MB -> RDS: %.1f MB",
                  file.size("data/zipcode_lookup.csv") / 1e6,
                  file.size("data/zipcode_lookup.rds") / 1e6))
}

# Ecoregion grid (if CSV exists but RDS doesn't)
if (file.exists("data/ecoregion_grid.csv") && !file.exists("data/ecoregion_grid.rds")) {
  message("Converting ecoregion_grid.csv...")
  grid <- read.csv("data/ecoregion_grid.csv", stringsAsFactors = FALSE)
  saveRDS(grid, "data/ecoregion_grid.rds", compress = "xz")
  message(sprintf("  CSV: %.1f MB -> RDS: %.1f MB",
                  file.size("data/ecoregion_grid.csv") / 1e6,
                  file.size("data/ecoregion_grid.rds") / 1e6))
}

message("\nDone! RDS files load faster and use less memory.")
message("You can now delete the original CSV files if desired.")
