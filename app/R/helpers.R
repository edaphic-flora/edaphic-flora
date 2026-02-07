# R/helpers.R - Helper functions for soil analysis

library(sf)

# ---------------------------
# Null-coalescing operator (canonical definition)
# ---------------------------
# Robust version: handles NULL, empty, all-NA, data frames, lists
`%||%` <- function(a, b) {
 if (is.null(a)) return(b)
 if (is.data.frame(a)) a <- as.list(a[1, , drop = TRUE])
 if (is.list(a)) a <- unlist(a, use.names = FALSE)
 if (!length(a) || all(is.na(a))) return(b)
 if (is.character(a)) {
   a <- a[!is.na(a) & nzchar(a)]
   if (!length(a)) return(b)
   return(paste(unique(a), collapse = ", "))
 }
 if (is.numeric(a)) return(a[which(!is.na(a))[1]])
 a[1]
}

# ---------------------------
# Constants
# ---------------------------

# Tolerance for texture classification (allows for small rounding errors)
TEXTURE_TOLERANCE <- 0.1

# Stats gating thresholds
MIN_SAMPLES_FOR_PUBLIC_STATS <- 5L
MIN_CONTRIBUTORS_FOR_PUBLIC_STATS <- 2L
MIN_TOTAL_SAMPLES_FOR_SITE_STATS <- 20L

# Neighbor soil query radius defaults
DEFAULT_NEIGHBOR_RADIUS_MILES <- 10
MAX_NEIGHBOR_RADIUS_MILES <- 50

# Experience mode field definitions
# Casual mode shows these fields only; everything else is hidden
CASUAL_FIELDS <- c("ph", "organic_matter", "organic_matter_class",
                    "nitrate", "phosphorus", "potassium", "texture_class")
# Enthusiast mode shows all fields (no filtering)
ENTHUSIAST_FIELDS <- "all"

# ---------------------------
# Safe aggregation helpers (guard against all-NA vectors)
# ---------------------------
safe_min <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else min(x) }
safe_max <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else max(x) }

# ---------------------------
# Ecoregion Lookup
# ---------------------------

get_ecoregion <- function(lat, long, eco_sf) {
  if (is.null(lat) || is.null(long) || is.na(lat) || is.na(long)) {
    return(list(
      l4_name = NA_character_, l4_code = NA_character_,
      l3_name = NA_character_, l3_code = NA_character_,
      l2_name = NA_character_, l2_code = NA_character_,
      # Legacy fields for backwards compatibility
      name = NA_character_, code = NA_character_
    ))
  }
  tryCatch({
    pt <- st_as_sf(data.frame(long = long, lat = lat), coords = c("long", "lat"), crs = 4326)
    inters <- st_intersects(eco_sf, pt, sparse = FALSE)
    if (any(inters)) {
      idx <- which(inters)[1]
      # Extract L4 data
      l4_name <- if ("us_l4name" %in% names(eco_sf)) as.character(eco_sf$us_l4name[idx]) else NA_character_
      l4_code <- if ("us_l4code" %in% names(eco_sf)) as.character(eco_sf$us_l4code[idx]) else NA_character_
      # Extract L3 data
      l3_name <- if ("us_l3name" %in% names(eco_sf)) as.character(eco_sf$us_l3name[idx]) else NA_character_
      l3_code <- if ("us_l3code" %in% names(eco_sf)) as.character(eco_sf$us_l3code[idx]) else NA_character_
      # Extract L2 data (North American level)
      l2_name <- if ("na_l2name" %in% names(eco_sf)) as.character(eco_sf$na_l2name[idx]) else NA_character_
      l2_code <- if ("na_l2code" %in% names(eco_sf)) as.character(eco_sf$na_l2code[idx]) else NA_character_

      list(
        l4_name = l4_name, l4_code = l4_code,
        l3_name = l3_name, l3_code = l3_code,
        l2_name = l2_name, l2_code = l2_code,
        # Legacy fields
        name = l4_name, code = l4_code
      )
    } else {
      list(
        l4_name = NA_character_, l4_code = NA_character_,
        l3_name = NA_character_, l3_code = NA_character_,
        l2_name = NA_character_, l2_code = NA_character_,
        name = NA_character_, code = NA_character_
      )
    }
  }, error = function(e) {
    list(
      l4_name = NA_character_, l4_code = NA_character_,
      l3_name = NA_character_, l3_code = NA_character_,
      l2_name = NA_character_, l2_code = NA_character_,
      name = NA_character_, code = NA_character_
    )
  })
}

# ---------------------------
# Soil Texture Functions
# ---------------------------

get_texture_percentages <- function(texture_class, soil_texture_classes) {
  cd <- soil_texture_classes[soil_texture_classes$Texture == texture_class, ]
  if (!nrow(cd)) return(NULL)
  sand_mid <- mean(c(cd$Sand_Min, cd$Sand_Max))
  silt_mid <- mean(c(cd$Silt_Min, cd$Silt_Max))
  clay_mid <- mean(c(cd$Clay_Min, cd$Clay_Max))
  total <- sand_mid + silt_mid + clay_mid
  list(
    sand = sand_mid * (100 / total),
    silt = silt_mid * (100 / total),
    clay = clay_mid * (100 / total)
  )
}

classify_texture <- function(sand, silt, clay, soil_texture_classes) {
  if (abs((sand + silt + clay) - 100) > TEXTURE_TOLERANCE) {
    return("Error: Percentages must sum to 100%")
  }
  for (i in seq_len(nrow(soil_texture_classes))) {
    if (clay >= (soil_texture_classes$Clay_Min[i] - TEXTURE_TOLERANCE) &&
        clay <= (soil_texture_classes$Clay_Max[i] + TEXTURE_TOLERANCE) &&
        silt >= (soil_texture_classes$Silt_Min[i] - TEXTURE_TOLERANCE) &&
        silt <= (soil_texture_classes$Silt_Max[i] + TEXTURE_TOLERANCE) &&
        sand >= (soil_texture_classes$Sand_Min[i] - TEXTURE_TOLERANCE) &&
        sand <= (soil_texture_classes$Sand_Max[i] + TEXTURE_TOLERANCE)) {
      return(as.character(soil_texture_classes$Texture[i]))
    }
  }
  "Unclassified"
}

# ---------------------------
# Species Profile Calculations
# ---------------------------

#' Calculate soil profile statistics for a species
#' @param dat Data frame with soil sample data
#' @return List with profile statistics (ph, om, texture, nutrients, success metrics)
calc_species_profile <- function(dat) {
  if (nrow(dat) == 0) return(NULL)

  profile <- list(
    n_samples = nrow(dat),
    ph_mean = mean(dat$ph, na.rm = TRUE),
    ph_min = safe_min(dat$ph),
    ph_max = safe_max(dat$ph),
    om_mean = mean(dat$organic_matter, na.rm = TRUE),
    om_min = safe_min(dat$organic_matter),
    om_max = safe_max(dat$organic_matter)
  )

  # Texture - most common class
  if (sum(!is.na(dat$texture_class)) > 0) {
    profile$texture_class <- names(sort(table(dat$texture_class), decreasing = TRUE))[1]
  }

  # Nutrients - means
  if (sum(!is.na(dat$nitrate_ppm)) > 0) profile$nitrate_mean <- mean(dat$nitrate_ppm, na.rm = TRUE)
  if (sum(!is.na(dat$phosphorus_ppm)) > 0) profile$phosphorus_mean <- mean(dat$phosphorus_ppm, na.rm = TRUE)
  if (sum(!is.na(dat$potassium_ppm)) > 0) profile$potassium_mean <- mean(dat$potassium_ppm, na.rm = TRUE)
  if (sum(!is.na(dat$calcium_ppm)) > 0) profile$calcium_mean <- mean(dat$calcium_ppm, na.rm = TRUE)
  if (sum(!is.na(dat$magnesium_ppm)) > 0) profile$magnesium_mean <- mean(dat$magnesium_ppm, na.rm = TRUE)

  # Success rate
  if ("outcome" %in% names(dat) && sum(!is.na(dat$outcome)) > 0) {
    outcomes <- dat$outcome[!is.na(dat$outcome)]
    profile$success_rate <- sum(outcomes %in% c("Thriving", "Established")) / length(outcomes) * 100
  }

  # Best conditions
  if (sum(!is.na(dat$sun_exposure)) > 0) {
    profile$best_sun <- names(sort(table(dat$sun_exposure), decreasing = TRUE))[1]
  }
  if (sum(!is.na(dat$site_hydrology)) > 0) {
    profile$best_hydrology <- names(sort(table(dat$site_hydrology), decreasing = TRUE))[1]
  }

  profile
}

#' Calculate similarity score between two species profiles (0-100)
#' @param profile1 First species profile (from calc_species_profile)
#' @param profile2 Second species profile
#' @return Numeric similarity score 0-100
calc_similarity <- function(profile1, profile2) {
  if (is.null(profile1) || is.null(profile2)) return(0)

  scores <- c()
  weights <- c()

  # pH similarity (weight: 30) - check range overlap
  if (!is.na(profile1$ph_mean) && !is.na(profile2$ph_mean)) {
    ph_diff <- abs(profile1$ph_mean - profile2$ph_mean)
    ph_score <- max(0, 100 - ph_diff * 25)  # -25 points per pH unit difference
    scores <- c(scores, ph_score)
    weights <- c(weights, 30)
  }

  # OM similarity (weight: 20)
  if (!is.na(profile1$om_mean) && !is.na(profile2$om_mean)) {
    om_diff <- abs(profile1$om_mean - profile2$om_mean)
    om_score <- max(0, 100 - om_diff * 10)  # -10 points per % difference
    scores <- c(scores, om_score)
    weights <- c(weights, 20)
  }

  # Texture similarity (weight: 15)
  if (!is.null(profile1$texture_class) && !is.null(profile2$texture_class)) {
    texture_score <- if (profile1$texture_class == profile2$texture_class) 100 else 50
    scores <- c(scores, texture_score)
    weights <- c(weights, 15)
  }

  # Nutrient similarities (weight: 5 each, total 25)
  nutrient_params <- c("nitrate_mean", "phosphorus_mean", "potassium_mean", "calcium_mean", "magnesium_mean")
  for (param in nutrient_params) {
    if (!is.null(profile1[[param]]) && !is.null(profile2[[param]])) {
      # Normalize by using ratio (within 2x = similar)
      ratio <- profile1[[param]] / max(profile2[[param]], 0.1)
      if (ratio > 1) ratio <- 1 / ratio
      nutrient_score <- ratio * 100
      scores <- c(scores, nutrient_score)
      weights <- c(weights, 5)
    }
  }

  # Calculate weighted average
  if (length(scores) == 0) return(0)
  sum(scores * weights) / sum(weights)
}

# ---------------------------
# Batch Plant Upload
# ---------------------------

#' Parse plant list CSV for batch upload
#' @param file_path Path to uploaded CSV file
#' @param species_db Species database for validation
#' @param max_rows Maximum rows to allow (default 100)
#' @return List with $valid (data frame), $invalid (character vector), $error (string or NULL)
parse_plant_list <- function(file_path, species_db, max_rows = 100) {
  # Read file based on extension
  ext <- tolower(tools::file_ext(file_path))

  df <- tryCatch({
    if (ext == "xlsx" || ext == "xls") {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        return(list(valid = NULL, invalid = NULL, error = "Excel support requires the readxl package"))
      }
      readxl::read_excel(file_path)
    } else {
      read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    }
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(df)) {
    return(list(valid = NULL, invalid = NULL, error = "Could not read file. Please check the format."))
  }

  # Normalize column names (lowercase, trim whitespace)
  names(df) <- tolower(trimws(names(df)))

  # Require species column
  if (!"species" %in% names(df)) {
    return(list(valid = NULL, invalid = NULL, error = "Missing required 'species' column"))
  }

  # Clean species names
 df$species <- trimws(as.character(df$species))
  df <- df[!is.na(df$species) & df$species != "", ]

  if (nrow(df) == 0) {
    return(list(valid = NULL, invalid = NULL, error = "No valid species found in file"))
  }

  # Check row limit
  if (nrow(df) > max_rows) {
    return(list(valid = NULL, invalid = NULL,
                error = sprintf("Too many rows (%d). Maximum allowed is %d.", nrow(df), max_rows)))
  }

  # Validate species against WCVP database
  valid_species_mask <- df$species %in% species_db$taxon_name

  # Standardize column names to expected format
  col_mapping <- list(
    cultivar = "cultivar",
    outcome = "outcome",
    sun_exposure = c("sun_exposure", "sun", "light"),
    site_hydrology = c("site_hydrology", "hydrology", "moisture"),
    inat_url = c("inat_url", "inaturalist", "inat"),
    notes = "notes"
  )

  # Apply column mapping
  for (target_col in names(col_mapping)) {
    possible_names <- col_mapping[[target_col]]
    found_col <- intersect(names(df), possible_names)[1]
    if (!is.na(found_col) && found_col != target_col) {
      df[[target_col]] <- df[[found_col]]
    }
  }

  # Ensure all expected columns exist
  expected_cols <- c("species", "cultivar", "outcome", "sun_exposure", "site_hydrology", "inat_url", "notes")
  for (col in expected_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }

  # Clean and validate enum values
  valid_outcomes <- c("Thriving", "Established", "Struggling", "Failed/Died")
  valid_sun <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade")
  valid_hydro <- c("Dry", "Mesic", "Wet")

  # Normalize outcome values (handle case variations)
  if ("outcome" %in% names(df)) {
    df$outcome <- trimws(as.character(df$outcome))
    # Try to match case-insensitively
    outcome_lower <- tolower(df$outcome)
    for (valid_val in valid_outcomes) {
      df$outcome[outcome_lower == tolower(valid_val)] <- valid_val
    }
    # Set invalid values to NA
    df$outcome[!df$outcome %in% valid_outcomes] <- NA_character_
  }

  # Normalize sun_exposure values
  if ("sun_exposure" %in% names(df)) {
    df$sun_exposure <- trimws(as.character(df$sun_exposure))
    sun_lower <- tolower(df$sun_exposure)
    for (valid_val in valid_sun) {
      df$sun_exposure[sun_lower == tolower(valid_val)] <- valid_val
    }
    df$sun_exposure[!df$sun_exposure %in% valid_sun] <- NA_character_
  }

  # Normalize site_hydrology values
  if ("site_hydrology" %in% names(df)) {
    df$site_hydrology <- trimws(as.character(df$site_hydrology))
    hydro_lower <- tolower(df$site_hydrology)
    for (valid_val in valid_hydro) {
      df$site_hydrology[hydro_lower == tolower(valid_val)] <- valid_val
    }
    df$site_hydrology[!df$site_hydrology %in% valid_hydro] <- NA_character_
  }

  # Clean other text fields
  if ("cultivar" %in% names(df)) {
    df$cultivar <- trimws(as.character(df$cultivar))
    df$cultivar[df$cultivar == ""] <- NA_character_
  }
  if ("inat_url" %in% names(df)) {
    df$inat_url <- trimws(as.character(df$inat_url))
    df$inat_url[df$inat_url == ""] <- NA_character_
  }
  if ("notes" %in% names(df)) {
    df$notes <- trimws(as.character(df$notes))
    df$notes[df$notes == ""] <- NA_character_
  }

  # Select only expected columns
  df <- df[, expected_cols, drop = FALSE]

  list(
    valid = df[valid_species_mask, , drop = FALSE],
    invalid = df$species[!valid_species_mask],
    error = NULL
  )
}

# ---------------------------
# Geospatial Utilities
# ---------------------------

#' Calculate Haversine distance between two points in miles
#' @param lat1 Latitude of point 1 (degrees)
#' @param lon1 Longitude of point 1 (degrees)
#' @param lat2 Latitude of point 2 (degrees)
#' @param lon2 Longitude of point 2 (degrees)
#' @return Distance in miles
haversine_miles <- function(lat1, lon1, lat2, lon2) {
  R <- 3958.8  # Earth's radius in miles
  to_rad <- pi / 180

  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  lat1_r <- lat1 * to_rad
  lat2_r <- lat2 * to_rad

  a <- sin(dlat / 2)^2 + cos(lat1_r) * cos(lat2_r) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

#' Calculate aggregate neighbor soil profile from nearby samples
#' @param nearby_df Data frame of nearby soil samples (from db_get_nearby_samples)
#' @return List with has_data, n_samples, n_contributors, soil_profile, successful_plants, status
calc_neighbor_profile <- function(nearby_df) {
  if (is.null(nearby_df) || nrow(nearby_df) == 0) {
    return(list(has_data = FALSE, n_samples = 0L, n_contributors = 0L,
                soil_profile = NULL, successful_plants = NULL, status = "no_data"))
  }

  n_samples <- nrow(nearby_df)
  n_contributors <- length(unique(nearby_df$created_by[!is.na(nearby_df$created_by)]))

  # Check gating thresholds
  if (n_samples < MIN_SAMPLES_FOR_PUBLIC_STATS || n_contributors < MIN_CONTRIBUTORS_FOR_PUBLIC_STATS) {
    return(list(has_data = TRUE, n_samples = n_samples, n_contributors = n_contributors,
                soil_profile = NULL, successful_plants = NULL, status = "early_access"))
  }

  # Build soil profile using medians for robustness
  safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)

  soil_profile <- list(
    ph = safe_median(nearby_df$ph),
    om = safe_median(nearby_df$organic_matter),
    nitrate = safe_median(nearby_df$nitrate_ppm),
    phosphorus = safe_median(nearby_df$phosphorus_ppm),
    potassium = safe_median(nearby_df$potassium_ppm),
    calcium = safe_median(nearby_df$calcium_ppm),
    magnesium = safe_median(nearby_df$magnesium_ppm)
  )

  # Mode texture class
  texture_vals <- nearby_df$texture_class[!is.na(nearby_df$texture_class) & nzchar(nearby_df$texture_class)]
  if (length(texture_vals) > 0) {
    soil_profile$texture <- names(sort(table(texture_vals), decreasing = TRUE))[1]
  }

  # Successful plants with outcome counts
  if ("outcome" %in% names(nearby_df) && "species" %in% names(nearby_df)) {
    sp_outcomes <- nearby_df[!is.na(nearby_df$species) & !is.na(nearby_df$outcome), c("species", "outcome")]
    if (nrow(sp_outcomes) > 0) {
      successful <- sp_outcomes[sp_outcomes$outcome %in% c("Thriving", "Established"), ]
      if (nrow(successful) > 0) {
        sp_counts <- sort(table(successful$species), decreasing = TRUE)
        successful_plants <- data.frame(
          species = names(sp_counts),
          success_count = as.integer(sp_counts),
          stringsAsFactors = FALSE
        )
      } else {
        successful_plants <- data.frame(species = character(), success_count = integer(), stringsAsFactors = FALSE)
      }
    } else {
      successful_plants <- data.frame(species = character(), success_count = integer(), stringsAsFactors = FALSE)
    }
  } else {
    successful_plants <- data.frame(species = character(), success_count = integer(), stringsAsFactors = FALSE)
  }

  list(
    has_data = TRUE,
    n_samples = n_samples,
    n_contributors = n_contributors,
    soil_profile = soil_profile,
    successful_plants = successful_plants,
    status = "ready"
  )
}
