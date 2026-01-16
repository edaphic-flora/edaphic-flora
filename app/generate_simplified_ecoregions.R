# Generate a simplified ecoregions shapefile for production use
# Run this script locally to create data/ecoregions_simplified.rds
# The simplified file is much smaller and can be loaded on shinyapps.io free tier

library(sf)
library(ecoregions)
library(dplyr)

message("Loading full ecoregions data...")
data(ContinentalUsEcoregion4)
eco_sf <- st_as_sf(ContinentalUsEcoregion4) %>%
  st_make_valid() %>%
  st_transform(4326)

message("Original data size: ", format(object.size(eco_sf), units = "MB"))

# Simplify geometries - tolerance in degrees (~2km at US latitudes)
# More aggressive simplification to reduce memory footprint
message("Simplifying geometries...")
eco_simplified <- eco_sf %>%
  st_simplify(dTolerance = 0.02, preserveTopology = TRUE) %>%
  # Keep only the columns we need
  select(us_l4name, us_l4code)

message("Simplified data size: ", format(object.size(eco_simplified), units = "MB"))

# Save as RDS (compressed)
output_path <- "data/ecoregions_simplified.rds"
if (!dir.exists("data")) dir.create("data")
message("Saving to ", output_path, "...")
saveRDS(eco_simplified, output_path, compress = "xz")

file_size <- file.size(output_path)
message("File size: ", round(file_size / 1024 / 1024, 2), " MB")
message("Done! The simplified ecoregions file is ready for deployment.")
