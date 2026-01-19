# R/helpers.R - Helper functions for soil analysis

library(sf)

# ---------------------------
# Constants
# ---------------------------

# Tolerance for texture classification (allows for small rounding errors)
TEXTURE_TOLERANCE <- 0.1

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
    ph_min = min(dat$ph, na.rm = TRUE),
    ph_max = max(dat$ph, na.rm = TRUE),
    om_mean = mean(dat$organic_matter, na.rm = TRUE),
    om_min = min(dat$organic_matter, na.rm = TRUE),
    om_max = max(dat$organic_matter, na.rm = TRUE)
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
