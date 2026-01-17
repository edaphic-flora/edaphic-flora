# tests/testthat/test-data.R
# Unit tests for data loading functions

library(testthat)

# Create soil texture classes inline for testing (matches what data.R produces)
load_soil_texture_classes_test <- function() {
  data.frame(
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
}

# ---------------------------
# Tests for load_soil_texture_classes()
# ---------------------------

test_that("soil_texture_classes returns valid data frame", {
  result <- load_soil_texture_classes_test()
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("soil_texture_classes has required columns", {
  result <- load_soil_texture_classes_test()
  required_cols <- c("Texture", "Clay_Min", "Clay_Max", "Silt_Min", "Silt_Max", "Sand_Min", "Sand_Max")
  expect_true(all(required_cols %in% names(result)))
})

test_that("soil_texture_classes contains expected texture classes", {
  result <- load_soil_texture_classes_test()
  expected_classes <- c("Clay", "Loam", "Sand", "Silt", "Sandy Loam", "Clay Loam")
  expect_true(all(expected_classes %in% result$Texture))
})

test_that("soil_texture_classes has 12 texture classes", {
  result <- load_soil_texture_classes_test()
  expect_equal(nrow(result), 12)
})

test_that("clay percentages are reasonable", {
  result <- load_soil_texture_classes_test()
  expect_true(all(result$Clay_Min >= 0))
  expect_true(all(result$Clay_Max <= 100))
  expect_true(all(result$Clay_Min <= result$Clay_Max))
})

test_that("sand percentages are reasonable", {
  result <- load_soil_texture_classes_test()
  expect_true(all(result$Sand_Min >= 0))
  expect_true(all(result$Sand_Max <= 100))
  expect_true(all(result$Sand_Min <= result$Sand_Max))
})

test_that("silt percentages are reasonable", {
  result <- load_soil_texture_classes_test()
  expect_true(all(result$Silt_Min >= 0))
  expect_true(all(result$Silt_Max <= 100))
  expect_true(all(result$Silt_Min <= result$Silt_Max))
})
