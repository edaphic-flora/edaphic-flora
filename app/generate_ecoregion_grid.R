# Generate a pre-computed ecoregion lookup grid for production use
# This creates a small CSV that maps lat/long grid cells to ecoregions
# Much lighter than loading the full shapefile (~1-2 MB vs ~95 MB)

library(sf)
library(ecoregions)
library(dplyr)

message("Loading ecoregions data...")
data(ContinentalUsEcoregion4)
eco_sf <- st_as_sf(ContinentalUsEcoregion4) %>%
  st_make_valid() %>%
  st_transform(4326)

# Grid resolution in degrees (~10 km at US latitudes)
GRID_RESOLUTION <- 0.1

# Continental US bounds (approximate)
lat_min <- 24
lat_max <- 50
lon_min <- -125
lon_max <- -66

message("Generating grid points...")
lats <- seq(lat_min, lat_max, by = GRID_RESOLUTION)
lons <- seq(lon_min, lon_max, by = GRID_RESOLUTION)

grid <- expand.grid(lat = lats, lon = lons)
message(sprintf("Grid has %d points", nrow(grid)))

# Convert to sf points
message("Converting to spatial points...")
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Find which ecoregion each point falls in
message("Finding ecoregions for each grid point (this may take a few minutes)...")
intersections <- st_intersects(grid_sf, eco_sf)

# Extract ecoregion info
message("Extracting ecoregion data...")
grid$ecoregion_name <- NA_character_
grid$ecoregion_code <- NA_character_

for (i in seq_len(nrow(grid))) {
  if (length(intersections[[i]]) > 0) {
    idx <- intersections[[i]][1]
    grid$ecoregion_name[i] <- as.character(eco_sf$us_l4name[idx])
    grid$ecoregion_code[i] <- as.character(eco_sf$us_l4code[idx])
  }

  if (i %% 10000 == 0) {
    message(sprintf("  Processed %d/%d points (%.1f%%)", i, nrow(grid), 100 * i / nrow(grid)))
  }
}

# Remove points outside ecoregions (ocean, etc.)
grid_valid <- grid %>% filter(!is.na(ecoregion_name))
message(sprintf("Grid has %d valid points (%.1f%% of total)",
                nrow(grid_valid), 100 * nrow(grid_valid) / nrow(grid)))

# Save as CSV
output_path <- "data/ecoregion_grid.csv"
if (!dir.exists("data")) dir.create("data")
write.csv(grid_valid, output_path, row.names = FALSE)

file_size <- file.size(output_path)
message(sprintf("Saved to %s (%.2f MB)", output_path, file_size / 1024 / 1024))

# Also save as compressed RDS for faster loading
rds_path <- "data/ecoregion_grid.rds"
saveRDS(grid_valid, rds_path, compress = "xz")
rds_size <- file.size(rds_path)
message(sprintf("Also saved to %s (%.2f MB)", rds_path, rds_size / 1024 / 1024))

message("\nDone! The ecoregion grid is ready for deployment.")
message("Update R/data.R to use load_ecoregion_grid() in production.")
