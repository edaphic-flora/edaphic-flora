# R/data.R - Reference data loading

library(sf)
library(ecoregions)
library(dplyr)

# ---------------------------
# Species Database
# ---------------------------

load_species_db <- function() {
  read.csv("species_accepted.csv", stringsAsFactors = FALSE)
}

# ---------------------------
# Ecoregions
# ---------------------------

load_ecoregions <- function() {
  data(ContinentalUsEcoregion4)
  st_as_sf(ContinentalUsEcoregion4) %>%
    st_make_valid() %>%
    st_transform(4326)
}

# ---------------------------
# Soil Texture Classes
# ---------------------------

soil_texture_classes <- data.frame(
  Texture = c("Heavy Clay", "Silty Clay", "Clay", "Silty Clay Loam", "Clay Loam",
              "Silt", "Silty Loam", "Sandy Clay", "Loam", "Sandy Clay Loam",
              "Sandy Loam", "Loamy Sand", "Sand"),
  Clay_Min = c(60, 40, 40, 27, 27, 0, 0, 35, 7, 20, 0, 0, 0),
  Clay_Max = c(100, 60, 60, 40, 40, 12, 27, 55, 27, 35, 20, 15, 10),
  Silt_Min = c(0, 40, 40, 40, 15, 88, 74, 0, 28, 20, 0, 0, 0),
  Silt_Max = c(40, 60, 60, 60, 52, 100, 88, 20, 50, 45, 50, 30, 14),
  Sand_Min = c(0, 0, 0, 0, 20, 0, 0, 45, 23, 45, 50, 70, 86),
  Sand_Max = c(45, 20, 45, 20, 45, 20, 50, 65, 52, 80, 70, 86, 100)
)

# ---------------------------
# CSV Template Structure
# ---------------------------

soil_data_template <- data.frame(
  species = character(), cultivar = character(),
  ph = numeric(), organic_matter = numeric(),
  nitrate_ppm = numeric(), ammonium_ppm = numeric(),
  phosphorus_ppm = numeric(), potassium_ppm = numeric(),
  calcium_ppm = numeric(), magnesium_ppm = numeric(),
  soluble_salts_ppm = numeric(),
  texture_sand = numeric(), texture_silt = numeric(), texture_clay = numeric(),
  texture_class = character(),
  location_lat = numeric(), location_long = numeric(),
  date = as.Date(character()),
  ecoregion_l4 = character(), ecoregion_l4_code = character(),
  photo_url = character(), notes = character(),
  stringsAsFactors = FALSE
)
