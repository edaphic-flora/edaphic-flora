# scrape_invasive_atlas.R
# Scrapes state-level invasive species data from invasiveplantatlas.org
#
# Usage:
#   source("scripts/scrape_invasive_atlas.R")
#   scrape_invasive_atlas()  # Full scrape (~1548 species)
#   scrape_invasive_atlas(limit = 10)  # Test with 10 species
#
# Output:
#   - data/invasive_atlas_raw.csv - Raw scraped data
#   - data/invasive_atlas_for_import.csv - Cleaned data ready for DB import

library(httr)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

# ---------------------------
# Configuration
# ---------------------------

BASE_URL <- "https://www.invasiveplantatlas.org"
ALL_SPECIES_URL <- paste0(BASE_URL, "/distribution.cfm")
CACHE_DIR <- "data/cache_invasive_atlas"
OUTPUT_DIR <- "data"
DELAY_SECONDS <- 1.5  # Be respectful - wait between requests

# Browser-like headers
HEADERS <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  `Accept-Language` = "en-US,en;q=0.5",
  `Connection` = "keep-alive"
)

# State name patterns to match in source listings
# Maps pattern -> state code
STATE_PATTERNS <- c(
  "Alabama" = "AL",
  "Alaska" = "AK",
  "Arizona" = "AZ",
  "Arkansas" = "AR",
  "California" = "CA",
  "Colorado" = "CO",
  "Connecticut" = "CT",
  "Delaware" = "DE",
  "Florida" = "FL",
  "Georgia" = "GA",
  "Hawaii" = "HI",
  "Idaho" = "ID",
  "Illinois" = "IL",
  "Indiana" = "IN",
  "Iowa" = "IA",
  "Kansas" = "KS",
  "Kentucky" = "KY",
  "Louisiana" = "LA",
  "Maine" = "ME",
  "Maryland" = "MD",
  "Massachusetts" = "MA",
  "Michigan" = "MI",
  "Minnesota" = "MN",
  "Mississippi" = "MS",
  "Missouri" = "MO",
  "Montana" = "MT",
  "Nebraska" = "NE",
  "Nevada" = "NV",
  "New Hampshire" = "NH",
  "New Jersey" = "NJ",
  "New Mexico" = "NM",
  "New York" = "NY",
  "North Carolina" = "NC",
  "North Dakota" = "ND",
  "Ohio" = "OH",
  "Oklahoma" = "OK",
  "Oregon" = "OR",
  "Pennsylvania" = "PA",
  "Rhode Island" = "RI",
  "South Carolina" = "SC",
  "South Dakota" = "SD",
  "Tennessee" = "TN",
  "Texas" = "TX",
  "Utah" = "UT",
  "Vermont" = "VT",
  "Virginia" = "VA",
  "Washington" = "WA",
  "West Virginia" = "WV",
  "Wisconsin" = "WI",
  "Wyoming" = "WY",
  "District of Columbia" = "DC",
  "Puerto Rico" = "PR",
  "Virgin Islands" = "VI",
  # Regional patterns that map to multiple states
  "Mid-Atlantic" = "REGIONAL_MID_ATLANTIC",
  "Pacific Northwest" = "REGIONAL_PNW",
  "Southeast" = "REGIONAL_SE",
  "New England" = "REGIONAL_NE"
)

# Regional expansions
REGIONAL_EXPANSIONS <- list(
  "REGIONAL_MID_ATLANTIC" = c("DE", "MD", "NJ", "NY", "PA", "VA", "WV", "DC"),
  "REGIONAL_PNW" = c("OR", "WA", "ID"),
  "REGIONAL_SE" = c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", "WV"),
  "REGIONAL_NE" = c("CT", "ME", "MA", "NH", "RI", "VT")
)

# ---------------------------
# Helper Functions
# ---------------------------

#' Fetch a URL with browser headers and retry logic
fetch_url <- function(url, max_retries = 3) {
  for (attempt in 1:max_retries) {
    tryCatch({
      response <- GET(url, HEADERS, timeout(30))

      if (status_code(response) == 200) {
        return(response)
      } else if (status_code(response) == 429) {
        message(sprintf("  Rate limited, waiting 30s..."))
        Sys.sleep(30)
      } else {
        message(sprintf("  HTTP %d (attempt %d)", status_code(response), attempt))
        Sys.sleep(DELAY_SECONDS * attempt)
      }
    }, error = function(e) {
      message(sprintf("  Error (attempt %d): %s", attempt, e$message))
      Sys.sleep(DELAY_SECONDS * attempt)
    })
  }
  NULL
}

#' Get cached HTML or fetch from web
get_page <- function(url, cache_file = NULL) {
  if (!is.null(cache_file) && file.exists(cache_file)) {
    return(read_html(cache_file))
  }

  response <- fetch_url(url)
  if (is.null(response)) return(NULL)

  html <- content(response, as = "text", encoding = "UTF-8")

  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(html, cache_file, useBytes = TRUE)
  }

  read_html(html)
}

#' Extract state codes from source listings
extract_states_from_sources <- function(source_texts) {
  states_found <- character(0)

  for (source_text in source_texts) {
    for (pattern in names(STATE_PATTERNS)) {
      if (grepl(pattern, source_text, ignore.case = TRUE)) {
        code <- STATE_PATTERNS[pattern]

        # Handle regional patterns
        if (startsWith(code, "REGIONAL_")) {
          states_found <- c(states_found, REGIONAL_EXPANSIONS[[code]])
        } else {
          states_found <- c(states_found, code)
        }
      }
    }
  }

  unique(states_found)
}

# ---------------------------
# Scraping Functions
# ---------------------------

#' Get list of all species from the distribution page
get_species_list <- function() {
  message("Fetching species list from distribution.cfm...")

  cache_file <- file.path(CACHE_DIR, "distribution.html")
  page <- get_page(ALL_SPECIES_URL, cache_file)

  if (is.null(page)) {
    stop("Failed to fetch species list")
  }

  # Parse the species table
  # Links are like: subject.cfm?sub=2425
  species_rows <- page %>%
    html_nodes("table tr") %>%
    lapply(function(row) {
      links <- row %>% html_nodes("a[href*='subject.cfm']")
      if (length(links) == 0) return(NULL)

      # Get common name and scientific name from first two links
      common_link <- links[1]
      sci_link <- if (length(links) >= 2) links[2] else links[1]

      data.frame(
        common_name = html_text(common_link, trim = TRUE),
        scientific_name = html_text(sci_link, trim = TRUE) %>%
          str_remove_all("\\s+[A-Z][a-z]*\\..*$") %>%  # Remove author
          str_trim(),
        url = html_attr(common_link, "href"),
        stringsAsFactors = FALSE
      )
    })

  species_df <- bind_rows(species_rows) %>%
    filter(!is.na(url), nzchar(scientific_name)) %>%
    mutate(
      url = paste0(BASE_URL, "/", url),
      species_id = str_extract(url, "sub=([0-9]+)", group = 1)
    ) %>%
    distinct(species_id, .keep_all = TRUE)

  message(sprintf("Found %d species", nrow(species_df)))
  species_df
}

#' Parse a species page for state distribution from listing sources
parse_species_page <- function(url, scientific_name, common_name = NA) {
  species_id <- str_extract(url, "sub=([0-9]+)", group = 1)
  cache_file <- file.path(CACHE_DIR, "species", paste0(species_id, ".html"))

  page <- get_page(url, cache_file)

  if (is.null(page)) {
    return(data.frame())
  }

  # Find the "Invasive Listing Sources" section ONLY
  # These are links like list.html?id=XX (NOT park.html which lists parks where found)
  # We specifically want state councils/agencies that list the species as invasive
  source_links <- page %>%
    html_nodes("a[href*='list.html']") %>%
    html_text(trim = TRUE)

  # Extract states from source listings only - this is the authoritative data
  # Don't use fallback methods that might pick up park names or other mentions
  states_found <- extract_states_from_sources(source_links)

  if (length(states_found) == 0) {
    return(data.frame(
      scientific_name = scientific_name,
      common_name = common_name,
      state_code = NA_character_,
      species_id = species_id,
      source_url = url,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    scientific_name = scientific_name,
    common_name = common_name,
    state_code = states_found,
    species_id = species_id,
    source_url = url,
    stringsAsFactors = FALSE
  )
}

# ---------------------------
# Main Function
# ---------------------------

#' Scrape the Invasive Plant Atlas
#' @param limit Max species to scrape (NULL for all)
#' @param resume Skip already-cached species
scrape_invasive_atlas <- function(limit = NULL, resume = TRUE) {
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(CACHE_DIR, "species"), showWarnings = FALSE)
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  # Get species list
  species_list <- get_species_list()

  if (!is.null(limit)) {
    species_list <- head(species_list, limit)
  }

  results <- list()
  n_total <- nrow(species_list)
  n_success <- 0
  n_cached <- 0

  message(sprintf("\nScraping %d species...\n", n_total))

  for (i in seq_len(n_total)) {
    sp <- species_list[i, ]

    # Check cache
    cache_file <- file.path(CACHE_DIR, "species", paste0(sp$species_id, ".html"))
    is_cached <- file.exists(cache_file)

    if (is_cached) {
      n_cached <- n_cached + 1
      if (i %% 100 == 0 || i == n_total) {
        message(sprintf("[%d/%d] %s (cached)", i, n_total, sp$scientific_name))
      }
    } else {
      message(sprintf("[%d/%d] %s", i, n_total, sp$scientific_name))
      Sys.sleep(DELAY_SECONDS)
    }

    tryCatch({
      result <- parse_species_page(sp$url, sp$scientific_name, sp$common_name)
      if (nrow(result) > 0) {
        results[[length(results) + 1]] <- result
        n_states <- sum(!is.na(result$state_code))
        if (!is_cached && n_states > 0) {
          message(sprintf("  -> %d states", n_states))
        }
        n_success <- n_success + 1
      }
    }, error = function(e) {
      message(sprintf("  Error: %s", e$message))
    })

    # Save progress every 100 species
    if (i %% 100 == 0 && length(results) > 0) {
      interim_df <- bind_rows(results)
      write.csv(interim_df, file.path(OUTPUT_DIR, "invasive_atlas_interim.csv"), row.names = FALSE)
      message(sprintf("--- Saved interim: %d records ---", nrow(interim_df)))
    }
  }

  if (length(results) == 0) {
    message("No data scraped!")
    return(data.frame())
  }

  final_df <- bind_rows(results)

  # Save raw data
  write.csv(final_df, file.path(OUTPUT_DIR, "invasive_atlas_raw.csv"), row.names = FALSE)

  # Create import-ready version
  import_df <- final_df %>%
    filter(!is.na(state_code)) %>%
    mutate(
      designation = "Invasive",
      source = "Invasive Plant Atlas of the United States",
      # Clean scientific name (remove italics markup if present)
      scientific_name = str_remove_all(scientific_name, "<[^>]+>") %>% str_trim()
    ) %>%
    select(scientific_name, common_name, state_code, designation, source, source_url) %>%
    distinct()

  write.csv(import_df, file.path(OUTPUT_DIR, "invasive_atlas_for_import.csv"), row.names = FALSE)

  # Summary
  message(sprintf("\n=== Complete ==="))
  message(sprintf("Species processed: %d (%d from cache)", n_total, n_cached))
  message(sprintf("Species with state data: %d", length(unique(import_df$scientific_name))))
  message(sprintf("Total state records: %d", nrow(import_df)))
  message(sprintf("\nFiles saved:"))
  message(sprintf("  - %s", file.path(OUTPUT_DIR, "invasive_atlas_raw.csv")))
  message(sprintf("  - %s", file.path(OUTPUT_DIR, "invasive_atlas_for_import.csv")))

  invisible(final_df)
}

#' Load scraped data into database
#' @param csv_path Path to import CSV
#' @param pool Database connection pool
load_invasive_to_db <- function(csv_path = "data/invasive_atlas_for_import.csv", pool = NULL) {
  if (!file.exists(csv_path)) {
    stop("Import file not found. Run scrape_invasive_atlas() first.")
  }

  # Load db.R if pool not provided
  if (is.null(pool)) {
    if (file.exists("app/R/db.R")) {
      source("app/R/db.R")
    }
    pool <- get("pool", envir = globalenv())
  }

  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  message(sprintf("Loading %d records...", nrow(data)))

  n_matched <- 0
  n_inserted <- 0
  n_unmatched <- 0
  unmatched_species <- character()

  # Get unique species to reduce lookups
  unique_species <- unique(data$scientific_name)
  taxon_cache <- list()

  message("Resolving taxon IDs...")
  for (sp in unique_species) {
    # Extract genus + species (first two words)
    gs <- paste(strsplit(sp, "\\s+")[[1]][1:2], collapse = " ")

    taxon <- tryCatch({
      DBI::dbGetQuery(pool, "
        SELECT id FROM ref_taxon
        WHERE lower(split_part(scientific_name, ' ', 1) || ' ' || split_part(scientific_name, ' ', 2)) = lower($1)
        LIMIT 1
      ", params = list(gs))
    }, error = function(e) data.frame())

    if (nrow(taxon) > 0) {
      taxon_cache[[sp]] <- taxon$id[1]
      n_matched <- n_matched + 1
    } else {
      unmatched_species <- c(unmatched_species, sp)
      n_unmatched <- n_unmatched + 1
    }
  }

  message(sprintf("Matched %d/%d species to ref_taxon", n_matched, length(unique_species)))

  # Insert records
  message("Inserting records...")
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    taxon_id <- taxon_cache[[row$scientific_name]]

    if (is.null(taxon_id)) next

    tryCatch({
      DBI::dbExecute(pool, "
        INSERT INTO ref_noxious_invasive (taxon_id, state_code, designation, source, source_url)
        VALUES ($1, $2, $3, $4, $5)
        ON CONFLICT (taxon_id, state_code, designation) DO NOTHING
      ", params = list(taxon_id, row$state_code, row$designation, row$source, row$source_url))
      n_inserted <- n_inserted + 1
    }, error = function(e) NULL)

    if (i %% 500 == 0) message(sprintf("  %d/%d", i, nrow(data)))
  }

  message(sprintf("\n=== Load Complete ==="))
  message(sprintf("Records inserted: %d", n_inserted))
  message(sprintf("Unmatched species: %d", n_unmatched))

  if (length(unmatched_species) > 0 && length(unmatched_species) <= 20) {
    message("Unmatched: ", paste(head(unmatched_species, 20), collapse = ", "))
  }

  invisible(n_inserted)
}

# ---------------------------
# Quick Test
# ---------------------------
message("
Invasive Plant Atlas Scraper loaded.

Usage:
  # Test with a few species
  scrape_invasive_atlas(limit = 10)

  # Full scrape (~1548 species, ~40 min with caching)
  scrape_invasive_atlas()

  # Load into database
  load_invasive_to_db()
")
