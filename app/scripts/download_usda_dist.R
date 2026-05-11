# Download USDA PLANTS state distribution data
# This script fetches native/introduced status by state for US plants

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Create output directory
output_dir <- "data/raw/usda"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# USDA PLANTS API endpoint for state distribution
# The API allows querying plants by state and getting native status

# Function to query USDA PLANTS API for a state
get_state_plants <- function(state_code) {
  message(sprintf("Fetching plants for state: %s", state_code))

  # USDA PLANTS API endpoint
  url <- sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantProfile/GetPlantsForState/%s", state_code)

  tryCatch({
    resp <- GET(url, timeout(60))
    if (status_code(resp) == 200) {
      content(resp, "parsed", simplifyVector = TRUE)
    } else {
      message(sprintf("  Failed with status: %d", status_code(resp)))
      NULL
    }
  }, error = function(e) {
    message(sprintf("  Error: %s", e$message))
    NULL
  })
}

# Alternative: Try the characteristics API
get_plant_characteristics <- function(symbol) {
  url <- sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantCharacteristics/%s", symbol)

  tryCatch({
    resp <- GET(url, timeout(30))
    if (status_code(resp) == 200) {
      content(resp, "parsed", simplifyVector = TRUE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

# Test the API
message("Testing USDA PLANTS API...")
test <- get_state_plants("PA")

if (is.null(test)) {
  message("\nDirect API access not available. Trying alternative approach...")

  # Try characteristics API for a known plant
  message("Testing characteristics API for ACRU (Acer rubrum)...")
  char_test <- get_plant_characteristics("ACRU")

  if (!is.null(char_test)) {
    message("Characteristics API works!")
    print(names(char_test))
  } else {
    message("Characteristics API also unavailable.")
  }
} else {
  message(sprintf("Got %d plants for PA", length(test)))

  # If API works, fetch all states
  US_STATES <- c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
    "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
    "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
    "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
  )

  all_data <- list()
  for (state in US_STATES) {
    plants <- get_state_plants(state)
    if (!is.null(plants) && length(plants) > 0) {
      all_data[[state]] <- plants
    }
    Sys.sleep(0.5)  # Rate limiting
  }

  # Combine and save
  if (length(all_data) > 0) {
    combined <- bind_rows(all_data, .id = "state_code")
    output_file <- file.path(output_dir, "usda_state_dist.csv")
    write_csv(combined, output_file)
    message(sprintf("Saved %d records to %s", nrow(combined), output_file))
  }
}

message("\nDone!")
