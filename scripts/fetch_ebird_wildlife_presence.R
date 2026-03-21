# scripts/fetch_ebird_wildlife_presence.R — Fetch eBird species lists per US state
# Produces: data/ebird_wildlife_state_presence.csv
#
# Strategy:
#   1. For each of 50 states + DC, call eBird API /v2/product/spplist/US-{state}
#   2. Convert eBird species codes to scientific names via eBird taxonomy
#   3. Match to ref_wildlife_species birds
#   4. Output one row per (species, state) pair
#
# Requirements:
#   - EBIRD_API_KEY in .Renviron
#   - install.packages(c("httr", "jsonlite", "readr", "dplyr"))
#
# Usage:
#   Rscript scripts/fetch_ebird_wildlife_presence.R

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(stringr)
})

# Load env vars
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

# ---- Configuration -----------------------------------------------------------

OUTPUT_FILE <- "data/ebird_wildlife_state_presence.csv"
CACHE_DIR <- "data/cache_ebird"
TAXONOMY_CACHE <- file.path(CACHE_DIR, "ebird_taxonomy.csv")

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

EBIRD_API_KEY <- Sys.getenv("EBIRD_API_KEY")
if (!nzchar(EBIRD_API_KEY)) {
  stop("Missing EBIRD_API_KEY in .Renviron. Get one at https://ebird.org/api/keygen")
}

EBIRD_BASE_URL <- "https://api.ebird.org/v2"

US_STATES <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# ---- Step 1: Get eBird taxonomy (species code -> scientific name) -----------

get_ebird_taxonomy <- function() {
  if (file.exists(TAXONOMY_CACHE)) {
    message("Using cached eBird taxonomy")
    return(read_csv(TAXONOMY_CACHE, show_col_types = FALSE))
  }

  message("Downloading eBird taxonomy...")
  resp <- GET(
    paste0(EBIRD_BASE_URL, "/ref/taxonomy/ebird"),
    add_headers("X-eBirdApiToken" = EBIRD_API_KEY),
    query = list(fmt = "csv")
  )
  stop_for_status(resp, "fetch eBird taxonomy")

  tax_text <- content(resp, as = "text", encoding = "UTF-8")
  tax_df <- read_csv(tax_text, show_col_types = FALSE)

  # Keep only species (not subspecies/groups)
  tax_df <- tax_df %>%
    filter(CATEGORY == "species") %>%
    select(speciesCode = SPECIES_CODE, sciName = SCI_NAME, comName = PRIMARY_COM_NAME)

  write_csv(tax_df, TAXONOMY_CACHE)
  message(sprintf("  Cached %d species to %s", nrow(tax_df), TAXONOMY_CACHE))
  tax_df
}

# ---- Step 2: Fetch species list per state -----------------------------------

fetch_state_species <- function(state_code, taxonomy) {
  cache_file <- file.path(CACHE_DIR, paste0("spplist_", state_code, ".json"))

  if (file.exists(cache_file)) {
    species_codes <- fromJSON(readLines(cache_file, warn = FALSE))
  } else {
    region_code <- paste0("US-", state_code)
    url <- paste0(EBIRD_BASE_URL, "/product/spplist/", region_code)

    resp <- GET(url, add_headers("X-eBirdApiToken" = EBIRD_API_KEY))

    if (status_code(resp) != 200) {
      message(sprintf("  WARNING: Failed for %s (HTTP %d)", state_code, status_code(resp)))
      return(data.frame())
    }

    species_codes <- content(resp, as = "parsed", simplifyVector = TRUE)
    writeLines(toJSON(species_codes), cache_file)
  }

  if (length(species_codes) == 0) return(data.frame())

  # Match codes to scientific names via taxonomy
  matched <- taxonomy %>%
    filter(speciesCode %in% species_codes)

  data.frame(
    scientific_name = matched$sciName,
    state_code = state_code,
    source = "eBird",
    observation_count = NA_integer_,
    stringsAsFactors = FALSE
  )
}

# ---- Main -------------------------------------------------------------------

message("== eBird Wildlife State Presence Fetcher ==")

taxonomy <- get_ebird_taxonomy()
message(sprintf("Taxonomy: %d species", nrow(taxonomy)))

# Fetch all states
all_results <- list()
for (i in seq_along(US_STATES)) {
  state <- US_STATES[i]
  message(sprintf("[%d/%d] Fetching %s...", i, length(US_STATES), state))

  result <- fetch_state_species(state, taxonomy)
  if (nrow(result) > 0) {
    all_results[[state]] <- result
    message(sprintf("  %d species", nrow(result)))
  }

  # Rate limit: 2 seconds between requests
  if (i < length(US_STATES)) Sys.sleep(2)
}

combined <- bind_rows(all_results)
message(sprintf("\nCombined output: %d records (%d species, %d states)",
                nrow(combined), n_distinct(combined$scientific_name), n_distinct(combined$state_code)))

# Write output
dir.create(dirname(OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)
write_csv(combined, OUTPUT_FILE)
message(sprintf("Written to: %s", OUTPUT_FILE))

# Summary
state_summary <- combined %>% count(state_code) %>% arrange(desc(n))
message("\nTop 10 states by species count:")
for (i in seq_len(min(10, nrow(state_summary)))) {
  message(sprintf("  %s: %d species", state_summary$state_code[i], state_summary$n[i]))
}

message(sprintf("\nBottom 5 states:"))
for (i in seq(max(1, nrow(state_summary) - 4), nrow(state_summary))) {
  message(sprintf("  %s: %d species", state_summary$state_code[i], state_summary$n[i]))
}

message("\n== eBird fetch complete ==")
