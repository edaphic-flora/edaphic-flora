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

# Load full shapefile (dev only - uses ~95MB memory)
load_ecoregions <- function() {
  data(ContinentalUsEcoregion4, package = "ecoregions")
  st_as_sf(ContinentalUsEcoregion4) %>%
    st_make_valid() %>%
    st_transform(4326)
}

# Load pre-computed grid lookup (prod - uses ~2MB memory)
load_ecoregion_grid <- function() {
  grid_path <- "data/ecoregion_grid.rds"
  if (file.exists(grid_path)) {
    message("Loading ecoregion grid lookup...")
    return(readRDS(grid_path))
  }
  warning("Ecoregion grid not found: ", grid_path)
  NULL
}

# Grid-based ecoregion lookup (for production)
# Rounds coordinates to nearest grid cell and returns matching ecoregion
get_ecoregion_from_grid <- function(lat, lon, grid, resolution = 0.1) {
  if (is.null(grid) || is.na(lat) || is.na(lon)) {
    return(list(name = NA_character_, code = NA_character_))
  }

  # Round to nearest grid cell
  grid_lat <- round(lat / resolution) * resolution
  grid_lon <- round(lon / resolution) * resolution

  # Find matching row
  match_idx <- which(abs(grid$lat - grid_lat) < 0.001 &
                     abs(grid$lon - grid_lon) < 0.001)

  if (length(match_idx) > 0) {
    list(
      name = grid$ecoregion_name[match_idx[1]],
      code = grid$ecoregion_code[match_idx[1]]
    )
  } else {
    list(name = NA_character_, code = NA_character_)
  }
}

# ---------------------------
# State Grid (for native status lookups)
# ---------------------------

# Load pre-computed state grid lookup
load_state_grid <- function() {
  grid_path <- "data/state_grid.rds"
  if (file.exists(grid_path)) {
    message("Loading state grid lookup...")
    return(readRDS(grid_path))
  }
  warning("State grid not found: ", grid_path)
  NULL
}

# Grid-based state lookup (for production)
# Rounds coordinates to nearest grid cell and returns matching state
get_state_from_grid <- function(lat, lon, grid, resolution = 0.1) {
  if (is.null(grid) || is.na(lat) || is.na(lon)) {
    return(NA_character_)
  }

  # Round to nearest grid cell
  grid_lat <- round(lat / resolution) * resolution
  grid_lon <- round(lon / resolution) * resolution

  # Find matching row
  match_idx <- which(abs(grid$lat - grid_lat) < 0.001 &
                     abs(grid$lon - grid_lon) < 0.001)

  if (length(match_idx) > 0) {
    grid$state_code[match_idx[1]]
  } else {
    NA_character_
  }
}

# Get unique states from a set of coordinates
get_states_from_coords <- function(lats, lons, grid, resolution = 0.1) {
  if (is.null(grid) || length(lats) == 0) {
    return(character(0))
  }

  states <- mapply(
    function(lat, lon) get_state_from_grid(lat, lon, grid, resolution),
    lats, lons,
    USE.NAMES = FALSE
  )

  unique(states[!is.na(states)])
}

# ---------------------------
# Zipcode Lookup
# ---------------------------

load_zipcode_db <- function() {
  rds_path <- "data/zipcode_lookup.rds"
  csv_path <- "data/zipcode_lookup.csv"

  if (file.exists(rds_path)) {
    message("Loading zipcode lookup...")
    return(readRDS(rds_path))
  } else if (file.exists(csv_path)) {
    message("Loading zipcode lookup from CSV...")
    return(read.csv(csv_path, stringsAsFactors = FALSE, colClasses = c(zipcode = "character")))
  }

  warning("Zipcode lookup not found")
  NULL
}

lookup_zipcode <- function(zipcode, zipcode_db) {
  if (is.null(zipcode_db) || is.null(zipcode) || !nzchar(zipcode)) {
    return(NULL)
  }

  # Clean zipcode (remove spaces, take first 5 digits)
  zip_clean <- gsub("[^0-9]", "", zipcode)
  zip_clean <- substr(zip_clean, 1, 5)

  if (nchar(zip_clean) != 5) {
    return(NULL)
  }

  match_idx <- which(zipcode_db$zipcode == zip_clean)
  if (length(match_idx) > 0) {
    row <- zipcode_db[match_idx[1], ]
    list(
      city = row$city,
      state = row$state,
      latitude = row$latitude,
      longitude = row$longitude
    )
  } else {
    NULL
  }
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
