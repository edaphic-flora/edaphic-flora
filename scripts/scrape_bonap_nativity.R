# scrape_bonap_nativity.R
# Scrapes state-level nativity data from BONAP's Taxonomic Data Center (TDC)
#
# Usage:
#   source("scripts/scrape_bonap_nativity.R")
#   scrape_bonap_nativity()           # Full scrape (50 states × 3 categories)
#   scrape_bonap_nativity(limit = 2)  # Test with 2 states
#
# Output:
#   - data/cache_bonap/{state}_{nativity}.tsv  — Cached raw responses
#   - data/bonap_state_nativity_compiled.csv    — Compiled output for ETL

library(httr)
library(dplyr)
library(stringr)
library(readr)

# ---------------------------
# Configuration
# ---------------------------

BASE_URL <- "https://bonap.net"
TDC_URL <- paste0(BASE_URL, "/tdc")
DOWNLOAD_URL <- paste0(BASE_URL, "/TDC/Query/FullTaxonList")
CACHE_DIR <- "data/cache_bonap"
OUTPUT_DIR <- "data"
DELAY_BETWEEN_REQUESTS <- 3   # seconds between requests
DELAY_BETWEEN_STATES <- 5     # seconds between states

# Browser-like headers
HEADERS <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  `Accept-Language` = "en-US,en;q=0.5",
  `Referer` = "https://bonap.net/tdc"
)

# BONAP Biological Attribute IDs for nativity
NATIVITY_ATTRS <- c(
  native = "33",
  exotic = "34",
  adventive = "35"
)

# US state codes (50 states + DC)
STATE_CODES <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# ---------------------------
# Helper Functions
# ---------------------------

#' Initialize session by visiting TDC page and return a handle with cookies
get_session_handle <- function() {
  message("Initializing session...")
  h <- handle(BASE_URL)
  response <- GET(TDC_URL, HEADERS, handle = h, timeout(30))
  if (status_code(response) != 200) {
    stop("Failed to initialize session: HTTP ", status_code(response))
  }
  h
}

#' Fetch species list for a state + nativity combination
#' @param state_code Two-letter state code
#' @param nativity_key One of "native", "exotic", "adventive"
#' @param session_handle httr handle with session cookies
#' @return Character vector of species names, or NULL on failure
fetch_species_list <- function(state_code, nativity_key, session_handle) {
  cache_file <- file.path(CACHE_DIR, paste0(state_code, "_", nativity_key, ".tsv"))

  # Check cache
  if (file.exists(cache_file)) {
    lines <- readLines(cache_file, warn = FALSE)
    return(lines)
  }

  attr_id <- NATIVITY_ATTRS[[nativity_key]]
  # Use synonymMode=4 to exclude infraspecific taxa (subspecies/varieties)
  # This gives cleaner species-level matches to our ref_taxon table
  params <- list(
    state = state_code,
    selectedAttributes = paste0(";", attr_id, ";"),
    synonymMode = "4",
    authorMode = "1",
    commonNameMode = "1",
    taxonPerPage = "5000",
    selectedSpeciesPage = "0"
  )

  for (attempt in 1:3) {
    tryCatch({
      response <- GET(
        DOWNLOAD_URL,
        query = params,
        HEADERS,
        handle = session_handle,
        timeout(60)
      )

      if (status_code(response) == 200) {
        content_text <- content(response, as = "text", encoding = "UTF-8")
        lines <- strsplit(content_text, "\r?\n")[[1]]

        # Cache the result
        dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
        writeLines(lines, cache_file)

        return(lines)
      } else if (status_code(response) == 429) {
        message("  Rate limited, waiting 30s...")
        Sys.sleep(30)
      } else {
        message(sprintf("  HTTP %d (attempt %d)", status_code(response), attempt))
        Sys.sleep(DELAY_BETWEEN_REQUESTS * attempt)
      }
    }, error = function(e) {
      message(sprintf("  Error (attempt %d): %s", attempt, e$message))
      Sys.sleep(DELAY_BETWEEN_REQUESTS * attempt)
    })
  }

  message(sprintf("  FAILED: %s %s after 3 attempts", state_code, nativity_key))
  NULL
}

#' Parse TSV lines from BONAP FullTaxonList response
#' @param lines Character vector of TSV lines
#' @return Data frame with family, genus, scientific_name columns
parse_species_lines <- function(lines) {
  if (is.null(lines) || length(lines) == 0) {
    return(data.frame(family = character(), genus = character(),
                      scientific_name = character(), stringsAsFactors = FALSE))
  }

  # First line is copyright notice, skip it
  # Also skip empty lines
  data_lines <- lines[!grepl("^(©|$)", lines) & nzchar(trimws(lines))]

  if (length(data_lines) == 0) {
    return(data.frame(family = character(), genus = character(),
                      scientific_name = character(), stringsAsFactors = FALSE))
  }

  # Parse tab-separated: Family, Genus, Species
  parts <- strsplit(data_lines, "\t")
  df <- data.frame(
    family = sapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_),
    genus = sapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_),
    scientific_name = sapply(parts, function(p) if (length(p) >= 3) p[3] else NA_character_),
    stringsAsFactors = FALSE
  )

  df <- df[!is.na(df$scientific_name) & nzchar(trimws(df$scientific_name)), ]
  # Extract genus + species (first two words) for matching
  df$genus_species <- str_extract(df$scientific_name, "^\\S+\\s+\\S+")
  df
}

# ---------------------------
# Main Function
# ---------------------------

#' Scrape BONAP TDC for state-level nativity data
#' @param limit Max number of states to process (NULL for all 51)
#' @param resume Skip states that already have all 3 cached files
scrape_bonap_nativity <- function(limit = NULL, resume = TRUE) {
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  states <- STATE_CODES
  if (!is.null(limit)) {
    states <- head(states, limit)
  }

  # Get session handle with cookies
  session_handle <- get_session_handle()

  all_results <- list()
  n_total <- length(states)
  n_processed <- 0
  n_cached <- 0

  message(sprintf("\nScraping %d states × 3 nativity categories = %d requests max\n",
                  n_total, n_total * 3))

  for (i in seq_along(states)) {
    state <- states[i]

    # Check if all 3 files are cached for resume
    if (resume) {
      cached_files <- file.path(CACHE_DIR, paste0(state, "_", names(NATIVITY_ATTRS), ".tsv"))
      if (all(file.exists(cached_files))) {
        n_cached <- n_cached + 1
      }
    }

    message(sprintf("[%d/%d] %s", i, n_total, state))

    # Fetch each nativity category
    native_lines <- NULL
    exotic_lines <- NULL
    adventive_lines <- NULL

    for (nativity in names(NATIVITY_ATTRS)) {
      cache_file <- file.path(CACHE_DIR, paste0(state, "_", nativity, ".tsv"))
      is_cached <- file.exists(cache_file)

      if (!is_cached) {
        Sys.sleep(DELAY_BETWEEN_REQUESTS)
      }

      lines <- fetch_species_list(state, nativity, session_handle)

      if (nativity == "native") native_lines <- lines
      else if (nativity == "exotic") exotic_lines <- lines
      else if (nativity == "adventive") adventive_lines <- lines

      n_sp <- length(parse_species_lines(lines)$scientific_name)
      if (is_cached) {
        message(sprintf("  %s: %d species (cached)", nativity, n_sp))
      } else {
        message(sprintf("  %s: %d species", nativity, n_sp))
      }
    }

    # Combine for this state
    native_df <- parse_species_lines(native_lines)
    exotic_df <- parse_species_lines(exotic_lines)
    adventive_df <- parse_species_lines(adventive_lines)

    # Build state records with nuanced nativity logic:
    #
    # BONAP categories:
    #   Native    = native to this state
    #   Exotic    = not native to North America at all
    #   Adventive = native to NA but found outside original county range in this state
    #
    # Key insight: A species on BOTH the Native and Adventive lists for the
    # same state is fundamentally native — it just expanded beyond its original
    # counties (e.g., Quercus alba planted in new counties within its native state).
    # Marking these as "Both" would mislead users into thinking it's not native.
    #
    # Classification rules:
    #   Native only               → "Native"
    #   Native + Adventive only   → "Native" (still native to the state)
    #   Native + Exotic           → "Both"   (rare: native AND foreign-exotic)
    #   Exotic only               → "Introduced"
    #   Adventive only            → "Introduced" (native to NA but not this state)
    #   Exotic + Adventive        → "Introduced"
    native_species <- unique(native_df$genus_species)
    exotic_species <- unique(exotic_df$genus_species)
    adventive_species <- unique(adventive_df$genus_species)

    # Species native AND exotic (very rare) → Both
    native_and_exotic <- intersect(native_species, exotic_species)
    # Species native but NOT exotic → Native (adventive overlap doesn't change this)
    pure_native <- setdiff(native_species, native_and_exotic)
    # Species not native → Introduced (whether exotic, adventive, or both)
    non_native_species <- unique(c(exotic_species, adventive_species))
    pure_introduced <- setdiff(non_native_species, native_species)

    state_records <- bind_rows(
      if (length(pure_native) > 0) data.frame(
        genus_species = pure_native, state_code = state,
        native_status = "Native", stringsAsFactors = FALSE),
      if (length(pure_introduced) > 0) data.frame(
        genus_species = pure_introduced, state_code = state,
        native_status = "Introduced", stringsAsFactors = FALSE),
      if (length(native_and_exotic) > 0) data.frame(
        genus_species = native_and_exotic, state_code = state,
        native_status = "Both", stringsAsFactors = FALSE)
    )

    if (nrow(state_records) > 0) {
      all_results[[length(all_results) + 1]] <- state_records
    }

    n_processed <- n_processed + 1
    message(sprintf("  -> %d Native, %d Introduced, %d Both",
                    length(pure_native), length(pure_introduced), length(native_and_exotic)))

    # Save checkpoint every 5 states
    if (i %% 5 == 0 && length(all_results) > 0) {
      interim_df <- bind_rows(all_results)
      write_csv(interim_df, file.path(OUTPUT_DIR, "bonap_state_nativity_interim.csv"))
      message(sprintf("--- Checkpoint: %d records for %d states ---", nrow(interim_df), i))
    }

    # Rate limit between states
    if (i < n_total) {
      Sys.sleep(DELAY_BETWEEN_STATES)
    }
  }

  if (length(all_results) == 0) {
    message("No data scraped!")
    return(data.frame())
  }

  # Compile final output
  compiled_df <- bind_rows(all_results) %>%
    filter(!is.na(genus_species) & nzchar(genus_species)) %>%
    rename(scientific_name = genus_species) %>%
    distinct(scientific_name, state_code, .keep_all = TRUE)

  output_path <- file.path(OUTPUT_DIR, "bonap_state_nativity_compiled.csv")
  write_csv(compiled_df, output_path)

  # Clean up interim file
  interim_path <- file.path(OUTPUT_DIR, "bonap_state_nativity_interim.csv")
  if (file.exists(interim_path)) file.remove(interim_path)

  # Summary
  message(sprintf("\n=== Complete ==="))
  message(sprintf("States processed: %d (%d from cache)", n_processed, n_cached))
  message(sprintf("Total records: %d", nrow(compiled_df)))
  message(sprintf("Unique species: %d", n_distinct(compiled_df$scientific_name)))
  message(sprintf("Records by status:"))
  status_counts <- compiled_df %>% count(native_status) %>% arrange(desc(n))
  for (j in seq_len(nrow(status_counts))) {
    message(sprintf("  %s: %d", status_counts$native_status[j], status_counts$n[j]))
  }
  message(sprintf("\nState coverage:"))
  state_counts <- compiled_df %>% count(state_code) %>% arrange(state_code)
  low_states <- state_counts %>% filter(n < 1000)
  if (nrow(low_states) > 0) {
    message(sprintf("  WARNING: %d states have <1000 species:", nrow(low_states)))
    for (j in seq_len(nrow(low_states))) {
      message(sprintf("    %s: %d", low_states$state_code[j], low_states$n[j]))
    }
  } else {
    message(sprintf("  All states have 1000+ species"))
  }
  message(sprintf("\nOutput: %s", output_path))

  invisible(compiled_df)
}

# ---------------------------
# Quick Test
# ---------------------------
message("
BONAP State Nativity Scraper loaded.

Usage:
  # Test with 2 states
  scrape_bonap_nativity(limit = 2)

  # Full scrape (50 states + DC, ~10-15 min)
  scrape_bonap_nativity()
")
