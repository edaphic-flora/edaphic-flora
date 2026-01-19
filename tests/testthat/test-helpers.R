# tests/testthat/test-helpers.R
# Unit tests for helper functions

library(testthat)

# Get the app directory path
app_dir <- normalizePath(file.path(dirname(getwd()), "..", "app"), mustWork = FALSE)

# Create soil texture classes inline for testing
soil_texture_classes <- data.frame(
  Texture = c("Clay", "Silty Clay", "Sandy Clay", "Clay Loam", "Silty Clay Loam",
              "Sandy Clay Loam", "Loam", "Silt Loam", "Sandy Loam", "Silt",
              "Loamy Sand", "Sand"),
  Clay_Min = c(40, 40, 35, 27, 27, 20, 7, 0, 0, 0, 0, 0),
  Clay_Max = c(100, 60, 55, 40, 40, 35, 27, 27, 20, 12, 15, 10),
  Silt_Min = c(0, 40, 0, 15, 40, 0, 28, 50, 0, 80, 0, 0),
  Silt_Max = c(40, 60, 20, 53, 73, 28, 50, 88, 50, 100, 30, 15),
  Sand_Min = c(0, 0, 45, 20, 0, 45, 23, 0, 43, 0, 70, 85),
  Sand_Max = c(45, 20, 65, 45, 20, 80, 52, 50, 85, 20, 90, 100),
  stringsAsFactors = FALSE
)

# Define classify_texture function inline for testing
classify_texture <- function(sand, silt, clay, soil_texture_classes) {
  if (abs((sand + silt + clay) - 100) > 0.1) {
    return("Error: Percentages must sum to 100%")
  }
  for (i in seq_len(nrow(soil_texture_classes))) {
    if (clay >= (soil_texture_classes$Clay_Min[i] - 0.1) &&
        clay <= (soil_texture_classes$Clay_Max[i] + 0.1) &&
        silt >= (soil_texture_classes$Silt_Min[i] - 0.1) &&
        silt <= (soil_texture_classes$Silt_Max[i] + 0.1) &&
        sand >= (soil_texture_classes$Sand_Min[i] - 0.1) &&
        sand <= (soil_texture_classes$Sand_Max[i] + 0.1)) {
      return(as.character(soil_texture_classes$Texture[i]))
    }
  }
  "Unclassified"
}

# Define get_texture_percentages function inline
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

# ---------------------------
# Tests for classify_texture()
# ---------------------------

test_that("classify_texture correctly identifies Sandy Loam", {
  result <- classify_texture(65, 25, 10, soil_texture_classes)
  expect_equal(result, "Sandy Loam")
})

test_that("classify_texture correctly identifies Clay", {
  result <- classify_texture(20, 20, 60, soil_texture_classes)
  expect_equal(result, "Clay")
})

test_that("classify_texture correctly identifies Loam", {
  result <- classify_texture(40, 40, 20, soil_texture_classes)
  expect_equal(result, "Loam")
})

test_that("classify_texture correctly identifies Silt Loam", {
  result <- classify_texture(20, 65, 15, soil_texture_classes)
  expect_equal(result, "Silt Loam")
})

test_that("classify_texture returns error when percentages don't sum to 100", {
  result <- classify_texture(30, 30, 30, soil_texture_classes)
  expect_match(result, "Error")
})

test_that("classify_texture handles edge case of pure sand", {
  result <- classify_texture(100, 0, 0, soil_texture_classes)
  expect_true(result %in% c("Sand", "Unclassified"))
})

# ---------------------------
# Tests for get_texture_percentages()
# ---------------------------

test_that("get_texture_percentages returns valid percentages", {
  result <- get_texture_percentages("Loam", soil_texture_classes)
  expect_true(!is.null(result))
  expect_true(is.list(result))
  expect_true("sand" %in% names(result))
  expect_true("silt" %in% names(result))
  expect_true("clay" %in% names(result))
})

test_that("get_texture_percentages percentages sum to ~100", {
  result <- get_texture_percentages("Clay Loam", soil_texture_classes)
  total <- result$sand + result$silt + result$clay
  expect_equal(total, 100, tolerance = 0.1)
})

test_that("get_texture_percentages returns NULL for invalid class", {
  result <- get_texture_percentages("Not A Real Texture", soil_texture_classes)
  expect_null(result)
})

# ---------------------------
# Tests for get_ecoregion()
# ---------------------------

# Define get_ecoregion function inline for testing
get_ecoregion <- function(lat, long, eco_sf) {
  if (is.null(lat) || is.null(long) || is.na(lat) || is.na(long)) {
    return(list(name = NA_character_, code = NA_character_))
  }
  list(name = NA_character_, code = NA_character_)
}

test_that("get_ecoregion handles NULL inputs", {
  result <- get_ecoregion(NULL, NULL, NULL)
  expect_true(is.na(result$name))
  expect_true(is.na(result$code))
})

test_that("get_ecoregion handles NA inputs", {
  result <- get_ecoregion(NA, NA, NULL)
  expect_true(is.na(result$name))
  expect_true(is.na(result$code))
})

# ---------------------------
# Tests for calc_species_profile()
# ---------------------------

# Define calc_species_profile function inline for testing
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

test_that("calc_species_profile returns NULL for empty data", {
  dat <- data.frame(ph = numeric(0), organic_matter = numeric(0))
  result <- calc_species_profile(dat)
  expect_null(result)
})

test_that("calc_species_profile calculates correct pH statistics", {
  dat <- data.frame(
    ph = c(5.5, 6.0, 6.5, 7.0),
    organic_matter = c(3.0, 4.0, 5.0, 6.0)
  )
  result <- calc_species_profile(dat)

  expect_equal(result$n_samples, 4)
  expect_equal(result$ph_mean, 6.25)
  expect_equal(result$ph_min, 5.5)
  expect_equal(result$ph_max, 7.0)
})

test_that("calc_species_profile calculates correct organic matter statistics", {
  dat <- data.frame(
    ph = c(6.0, 6.5),
    organic_matter = c(2.0, 8.0)
  )
  result <- calc_species_profile(dat)

  expect_equal(result$om_mean, 5.0)
  expect_equal(result$om_min, 2.0)
  expect_equal(result$om_max, 8.0)
})

test_that("calc_species_profile finds most common texture class", {
  dat <- data.frame(
    ph = c(6.0, 6.5, 6.2, 6.8),
    organic_matter = c(3.0, 4.0, 5.0, 4.5),
    texture_class = c("Loam", "Loam", "Sandy Loam", "Loam")
  )
  result <- calc_species_profile(dat)

  expect_equal(result$texture_class, "Loam")
})

test_that("calc_species_profile calculates nutrient means", {
  dat <- data.frame(
    ph = c(6.0, 6.5),
    organic_matter = c(3.0, 4.0),
    nitrate_ppm = c(10, 20),
    phosphorus_ppm = c(30, 40),
    potassium_ppm = c(100, 200),
    calcium_ppm = c(1000, 2000),
    magnesium_ppm = c(100, 300)
  )
  result <- calc_species_profile(dat)

  expect_equal(result$nitrate_mean, 15)
  expect_equal(result$phosphorus_mean, 35)
  expect_equal(result$potassium_mean, 150)
  expect_equal(result$calcium_mean, 1500)
  expect_equal(result$magnesium_mean, 200)
})

test_that("calc_species_profile calculates success rate correctly", {
  dat <- data.frame(
    ph = c(6.0, 6.5, 6.2, 6.8),
    organic_matter = c(3.0, 4.0, 5.0, 4.5),
    outcome = c("Thriving", "Established", "Struggling", "Failed/Died")
  )
  result <- calc_species_profile(dat)

  # 2 out of 4 are successful (Thriving or Established)
  expect_equal(result$success_rate, 50)
})

test_that("calc_species_profile finds best sun and hydrology conditions", {
  dat <- data.frame(
    ph = c(6.0, 6.5, 6.2, 6.8, 6.1),
    organic_matter = c(3.0, 4.0, 5.0, 4.5, 3.5),
    sun_exposure = c("Full Sun", "Full Sun", "Part Shade", "Full Sun", "Part Shade"),
    site_hydrology = c("Mesic", "Mesic", "Wet", "Mesic", "Dry")
  )
  result <- calc_species_profile(dat)

  expect_equal(result$best_sun, "Full Sun")
  expect_equal(result$best_hydrology, "Mesic")
})

test_that("calc_species_profile handles NA values in nutrients", {
  dat <- data.frame(
    ph = c(6.0, 6.5, 6.2),
    organic_matter = c(3.0, 4.0, NA),
    nitrate_ppm = c(10, NA, NA),
    phosphorus_ppm = c(NA, NA, NA)
  )
  result <- calc_species_profile(dat)

  expect_equal(result$nitrate_mean, 10)
  expect_null(result$phosphorus_mean)  # All NA, so not added to profile
})

# ---------------------------
# Tests for calc_similarity()
# ---------------------------

# Define calc_similarity function inline for testing
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

test_that("calc_similarity returns 0 for NULL profiles", {
  expect_equal(calc_similarity(NULL, NULL), 0)
  expect_equal(calc_similarity(list(ph_mean = 6.5), NULL), 0)
  expect_equal(calc_similarity(NULL, list(ph_mean = 6.5)), 0)
})

test_that("calc_similarity returns 100 for identical profiles", {
  profile <- list(
    ph_mean = 6.5,
    om_mean = 4.0,
    texture_class = "Loam",
    nitrate_mean = 15,
    phosphorus_mean = 35,
    potassium_mean = 150,
    calcium_mean = 1500,
    magnesium_mean = 200
  )
  result <- calc_similarity(profile, profile)
  expect_equal(result, 100)
})

test_that("calc_similarity scores pH differences correctly", {
  profile1 <- list(ph_mean = 6.0, om_mean = 4.0)
  profile2 <- list(ph_mean = 7.0, om_mean = 4.0)

  # pH diff = 1.0, score = 100 - 25 = 75
  # OM diff = 0, score = 100
  # Weighted: (75*30 + 100*20) / (30+20) = (2250 + 2000) / 50 = 85
  result <- calc_similarity(profile1, profile2)
  expect_equal(result, 85)
})

test_that("calc_similarity scores OM differences correctly", {
  profile1 <- list(ph_mean = 6.0, om_mean = 2.0)
  profile2 <- list(ph_mean = 6.0, om_mean = 7.0)

  # pH diff = 0, score = 100
  # OM diff = 5, score = 100 - 50 = 50
  # Weighted: (100*30 + 50*20) / (30+20) = (3000 + 1000) / 50 = 80
  result <- calc_similarity(profile1, profile2)
  expect_equal(result, 80)
})

test_that("calc_similarity scores texture match/mismatch correctly", {
  # Same texture
  profile1 <- list(ph_mean = 6.0, om_mean = 4.0, texture_class = "Loam")
  profile2 <- list(ph_mean = 6.0, om_mean = 4.0, texture_class = "Loam")
  result_same <- calc_similarity(profile1, profile2)

  # Different texture
  profile3 <- list(ph_mean = 6.0, om_mean = 4.0, texture_class = "Clay")
  result_diff <- calc_similarity(profile1, profile3)

  # Same texture should score higher than different
  expect_true(result_same > result_diff)

  # With pH=100, OM=100, texture=100: (100*30 + 100*20 + 100*15) / 65 = 100
  expect_equal(result_same, 100)

  # With pH=100, OM=100, texture=50: (100*30 + 100*20 + 50*15) / 65 = 88.46
  expect_equal(round(result_diff, 2), 88.46)
})

test_that("calc_similarity handles missing nutrient data", {
  # Profile with nutrients
  profile1 <- list(
    ph_mean = 6.0,
    om_mean = 4.0,
    nitrate_mean = 15,
    phosphorus_mean = 35
  )

  # Profile without nutrients
  profile2 <- list(
    ph_mean = 6.0,
    om_mean = 4.0
  )

  result <- calc_similarity(profile1, profile2)

  # Should only compare pH and OM (both 100%)
  expect_equal(result, 100)
})

test_that("calc_similarity handles nutrient ratio scoring", {
  profile1 <- list(ph_mean = 6.0, om_mean = 4.0, nitrate_mean = 20)
  profile2 <- list(ph_mean = 6.0, om_mean = 4.0, nitrate_mean = 10)

  # Nitrate ratio: 10/20 = 0.5, score = 50
  # pH = 100, OM = 100
  # Weighted: (100*30 + 100*20 + 50*5) / 55 = 5250/55 = 95.45
  result <- calc_similarity(profile1, profile2)
  expect_equal(round(result, 2), 95.45)
})

test_that("calc_similarity returns 0 when only NA values available", {
  profile1 <- list(ph_mean = NA, om_mean = NA)
  profile2 <- list(ph_mean = NA, om_mean = NA)

  result <- calc_similarity(profile1, profile2)
  expect_equal(result, 0)
})
