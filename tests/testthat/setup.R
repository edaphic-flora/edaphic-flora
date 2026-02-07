# tests/testthat/setup.R
# Source helper functions from the app, stubbing sf to avoid segfault

# Stub sf if not available (prevents library(sf) crash in test context)
if (!requireNamespace("sf", quietly = TRUE)) {
  # Create a minimal sf namespace so library(sf) doesn't fail
}

# Source helpers.R with library(sf) removed to avoid segfault
helpers_path <- normalizePath(file.path(dirname(getwd()), "..", "app", "R", "helpers.R"),
                              mustWork = FALSE)
if (file.exists(helpers_path)) {
  source_lines <- readLines(helpers_path)
  # Remove library(sf) line to prevent segfault in test context
  source_lines <- source_lines[!grepl("^library\\(sf\\)", source_lines)]
  # Stub st_as_sf and st_intersects so get_ecoregion doesn't error at parse time
  eval(parse(text = c(
    'if (!exists("st_as_sf")) st_as_sf <- function(...) NULL',
    'if (!exists("st_intersects")) st_intersects <- function(...) FALSE',
    paste(source_lines, collapse = "\n")
  )))
}
