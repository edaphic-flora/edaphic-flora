# Generate a pre-computed state lookup grid for production use
# This creates a small RDS that maps lat/long grid cells to US states
# Used for native status lookups without loading full state boundaries

library(sf)
library(tigris)
library(dplyr)

# Use tigris caching to avoid re-downloading
options(tigris_use_cache = TRUE)

message("Loading US state boundaries from tigris...")
states_sf <- states(cb = TRUE, resolution = "20m") %>%
  st_transform(4326)

# Filter to continental US + Alaska + Hawaii (exclude territories)
continental_codes <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
  "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
  "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
  "WV", "WI", "WY", "AK", "HI", "DC"
)

states_sf <- states_sf %>%
  filter(STUSPS %in% continental_codes)

message(sprintf("Loaded %d states/territories", nrow(states_sf)))

# Grid resolution in degrees (~10 km at US latitudes)
GRID_RESOLUTION <- 0.1

# Continental US bounds (approximate, extended for Alaska)
lat_min <- 18  # Hawaii southern extent
lat_max <- 72  # Alaska northern extent
lon_min <- -180  # Alaska western extent (crosses antimeridian)
lon_max <- -66

message("Generating grid points...")
lats <- seq(lat_min, lat_max, by = GRID_RESOLUTION)
lons <- seq(lon_min, lon_max, by = GRID_RESOLUTION)

grid <- expand.grid(lat = lats, lon = lons)
message(sprintf("Grid has %d points", nrow(grid)))

# Convert to sf points
message("Converting to spatial points...")
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Find which state each point falls in
message("Finding states for each grid point (this may take a few minutes)...")
intersections <- st_intersects(grid_sf, states_sf)

# Extract state info
message("Extracting state data...")
grid$state_code <- NA_character_
grid$state_name <- NA_character_

for (i in seq_len(nrow(grid))) {
  if (length(intersections[[i]]) > 0) {
    idx <- intersections[[i]][1]
    grid$state_code[i] <- as.character(states_sf$STUSPS[idx])
    grid$state_name[i] <- as.character(states_sf$NAME[idx])
  }

  if (i %% 10000 == 0) {
    message(sprintf("  Processed %d/%d points (%.1f%%)", i, nrow(grid), 100 * i / nrow(grid)))
  }
}

# Remove points outside US states (ocean, Canada, Mexico)
grid_valid <- grid %>% filter(!is.na(state_code))
message(sprintf("Grid has %d valid points (%.1f%% of total)",
                nrow(grid_valid), 100 * nrow(grid_valid) / nrow(grid)))

# Check state coverage
state_counts <- grid_valid %>%
  group_by(state_code, state_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
message(sprintf("Grid covers %d states:", nrow(state_counts)))
print(head(state_counts, 10))

# Save as compressed RDS for production
if (!dir.exists("data")) dir.create("data")

rds_path <- "data/state_grid.rds"
saveRDS(grid_valid, rds_path, compress = "xz")
rds_size <- file.size(rds_path)
message(sprintf("Saved to %s (%.2f MB)", rds_path, rds_size / 1024 / 1024))

message("\nDone! The state grid is ready for deployment.")
message("Update R/data.R to use load_state_grid() in production.")
