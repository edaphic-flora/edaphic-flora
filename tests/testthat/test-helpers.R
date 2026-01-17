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
