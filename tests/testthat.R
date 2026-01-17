# Test runner for edaphic flora
#
# Run from project root with:
#   Rscript tests/testthat.R
#
# Or from R console:
#   testthat::test_dir("tests/testthat")

library(testthat)

# Run all tests
test_dir("tests/testthat", reporter = "summary")
