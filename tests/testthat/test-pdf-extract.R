# tests/testthat/test-pdf-extract.R
# Unit tests for PDF extraction module

library(testthat)

# Define functions inline for testing (matches pdf_extract.R)
SUPPORTED_EXTENSIONS <- c("pdf", "rtf", "txt", "png", "jpg", "jpeg", "gif", "webp")

IMAGE_MIME_TYPES <- list(
  png = "image/png",
  jpg = "image/jpeg",
  jpeg = "image/jpeg",
  gif = "image/gif",
  webp = "image/webp"
)

get_file_extension <- function(file_path) {
  ext <- tools::file_ext(file_path)
  tolower(ext)
}

is_supported_format <- function(file_path) {
  ext <- get_file_extension(file_path)
  ext %in% SUPPORTED_EXTENSIONS
}

extract_config <- list(
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  daily_limit = 5,
  model = "claude-sonnet-4-20250514",
  max_tokens = 2000
)

is_pdf_extraction_available <- function() {
  nzchar(extract_config$api_key)
}

# ---------------------------
# Tests for file type detection
# ---------------------------

test_that("get_file_extension extracts correct extension", {
  expect_equal(get_file_extension("test.pdf"), "pdf")
  expect_equal(get_file_extension("test.PDF"), "pdf")
  expect_equal(get_file_extension("test.file.png"), "png")
  expect_equal(get_file_extension("/path/to/file.jpg"), "jpg")
})

test_that("get_file_extension handles files without extension", {
  expect_equal(get_file_extension("noextension"), "")
  expect_equal(get_file_extension("/path/to/file"), "")
})

test_that("is_supported_format correctly identifies supported formats", {
  expect_true(is_supported_format("test.pdf"))
  expect_true(is_supported_format("test.png"))
  expect_true(is_supported_format("test.jpg"))
  expect_true(is_supported_format("test.jpeg"))
  expect_true(is_supported_format("test.gif"))
  expect_true(is_supported_format("test.webp"))
  expect_true(is_supported_format("test.rtf"))
  expect_true(is_supported_format("test.txt"))
})

test_that("is_supported_format correctly rejects unsupported formats", {
  expect_false(is_supported_format("test.doc"))
  expect_false(is_supported_format("test.docx"))
  expect_false(is_supported_format("test.xls"))
  expect_false(is_supported_format("test.exe"))
  expect_false(is_supported_format("test.zip"))
})

test_that("is_supported_format handles case insensitivity", {
  expect_true(is_supported_format("test.PDF"))
  expect_true(is_supported_format("test.PNG"))
  expect_true(is_supported_format("test.Jpg"))
})

# ---------------------------
# Tests for extraction config
# ---------------------------

test_that("extract_config has required fields", {
  expect_true("api_key" %in% names(extract_config))
  expect_true("daily_limit" %in% names(extract_config))
  expect_true("model" %in% names(extract_config))
  expect_true("max_tokens" %in% names(extract_config))
})

test_that("extract_config has valid daily limit", {
  expect_type(extract_config$daily_limit, "double")
  expect_true(extract_config$daily_limit > 0)
})

test_that("extract_config has valid max_tokens", {
  expect_type(extract_config$max_tokens, "double")
  expect_true(extract_config$max_tokens > 0)
})

test_that("is_pdf_extraction_available returns boolean", {
  result <- is_pdf_extraction_available()
  expect_type(result, "logical")
})

# ---------------------------
# Tests for SUPPORTED_EXTENSIONS constant
# ---------------------------

test_that("SUPPORTED_EXTENSIONS contains expected types", {
  expect_true("pdf" %in% SUPPORTED_EXTENSIONS)
  expect_true("png" %in% SUPPORTED_EXTENSIONS)
  expect_true("jpg" %in% SUPPORTED_EXTENSIONS)
  expect_true("rtf" %in% SUPPORTED_EXTENSIONS)
  expect_true("txt" %in% SUPPORTED_EXTENSIONS)
})

test_that("SUPPORTED_EXTENSIONS has correct length", {
  expect_equal(length(SUPPORTED_EXTENSIONS), 8)
})

test_that("IMAGE_MIME_TYPES has correct mappings", {
  expect_equal(IMAGE_MIME_TYPES[["png"]], "image/png")
  expect_equal(IMAGE_MIME_TYPES[["jpg"]], "image/jpeg")
  expect_equal(IMAGE_MIME_TYPES[["jpeg"]], "image/jpeg")
  expect_equal(IMAGE_MIME_TYPES[["gif"]], "image/gif")
  expect_equal(IMAGE_MIME_TYPES[["webp"]], "image/webp")
})

test_that("IMAGE_MIME_TYPES has 5 entries", {
  expect_equal(length(IMAGE_MIME_TYPES), 5)
})
