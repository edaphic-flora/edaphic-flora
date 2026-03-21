# scripts/fetch_gbif_wildlife_presence.R — Download GBIF occurrence data for Lepidoptera + Bees
# Produces: data/gbif_wildlife_state_presence.csv
#
# Strategy:
#   1. Use rgbif to request a GBIF download (Lepidoptera + Apoidea, US, year>=2000)
#   2. Download and process the CSV to extract (scientific_name, state_code) pairs
#   3. Deduplicate and output one row per (species, state) with observation counts
#
# Requirements:
#   - GBIF account credentials in .Renviron: GBIF_USER, GBIF_PWD, GBIF_EMAIL
#   - install.packages(c("rgbif", "readr", "dplyr", "stringr"))
#
# Note: GBIF downloads are async and may take 10-60 minutes to prepare.
# The script will poll until ready. Cache raw downloads in data/cache_gbif/.
#
# Usage:
#   Rscript scripts/fetch_gbif_wildlife_presence.R

suppressPackageStartupMessages({
  library(rgbif)
  library(readr)
  library(dplyr)
  library(stringr)
})

# Load env vars
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

# ---- Configuration -----------------------------------------------------------

OUTPUT_FILE <- "data/gbif_wildlife_state_presence.csv"
CACHE_DIR <- "data/cache_gbif"
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# US state name -> 2-letter code mapping
STATE_NAME_TO_CODE <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
  "District of Columbia" = "DC", "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI",
  "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA",
  "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
  "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN",
  "Mississippi" = "MS", "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE",
  "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM",
  "New York" = "NY", "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH",
  "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI",
  "South Carolina" = "SC", "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX",
  "Utah" = "UT", "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA",
  "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY"
)

# ---- Helper: normalize scientific name to genus + epithet -------------------

normalize_species <- function(name) {
  # Extract first two words (genus + specific epithet)
  parts <- str_split(str_trim(name), "\\s+")
  sapply(parts, function(p) {
    if (length(p) >= 2) paste(p[1], p[2]) else p[1]
  })
}

# ---- Step 1: Request GBIF downloads -----------------------------------------

message("== GBIF Wildlife State Presence Fetcher ==")
message("Checking GBIF credentials...")

gbif_user <- Sys.getenv("GBIF_USER")
gbif_pwd <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

if (!nzchar(gbif_user) || !nzchar(gbif_pwd) || !nzchar(gbif_email)) {
  stop("Missing GBIF credentials. Set GBIF_USER, GBIF_PWD, GBIF_EMAIL in .Renviron")
}

# Check for cached download key
lep_key_file <- file.path(CACHE_DIR, "gbif_lep_download_key.txt")

# --- Lepidoptera download ---
if (file.exists(lep_key_file)) {
  lep_key <- readLines(lep_key_file, n = 1)
  message(sprintf("Using cached Lepidoptera download key: %s", lep_key))
} else {
  message("Requesting Lepidoptera occurrence download from GBIF...")
  # Lepidoptera order: taxonKey = 797 (GBIF backbone)
  lep_download <- occ_download(
    pred("taxonKey", 797),          # Lepidoptera
    pred("country", "US"),
    pred("hasCoordinate", TRUE),
    pred_gte("year", 2000),
    pred("hasGeospatialIssue", FALSE),
    format = "SIMPLE_CSV",
    user = gbif_user, pwd = gbif_pwd, email = gbif_email
  )
  lep_key <- lep_download[1]
  writeLines(lep_key, lep_key_file)
  message(sprintf("Lepidoptera download key: %s", lep_key))
}

# --- Bee downloads (all 6 families) ---
# taxonKey 4334 was only Apidae — need all bee families for full coverage
BEE_FAMILIES <- list(
  apidae       = 4334,   # Bumble bees, carpenter bees, honey bees, digger bees
  andrenidae   = 7901,   # Mining bees (Andrena, Perdita, etc.)
  halictidae   = 7908,   # Sweat bees (Lasioglossum, etc.)
  megachilidae = 7911,   # Leafcutter & mason bees (Megachile, Osmia)
  colletidae   = 7905,   # Plasterer bees (Colletes, Hylaeus)
  melittidae   = 4345    # Oil-collecting bees (Macropis, Melitta)
)

bee_keys <- list()
# Request downloads in batches to respect GBIF's 3 simultaneous download limit.
# Already-cached keys don't count against the limit.
pending_families <- character()
for (fam_name in names(BEE_FAMILIES)) {
  key_file <- file.path(CACHE_DIR, paste0("gbif_", fam_name, "_download_key.txt"))
  if (file.exists(key_file)) {
    bee_keys[[fam_name]] <- readLines(key_file, n = 1)
    message(sprintf("Using cached %s download key: %s", fam_name, bee_keys[[fam_name]]))
  } else {
    pending_families <- c(pending_families, fam_name)
  }
}

# Request pending families, waiting for slots to open if needed
for (fam_name in pending_families) {
  key_file <- file.path(CACHE_DIR, paste0("gbif_", fam_name, "_download_key.txt"))
  success <- FALSE
  while (!success) {
    tryCatch({
      message(sprintf("Requesting %s occurrence download from GBIF (taxonKey=%d)...",
                      fam_name, BEE_FAMILIES[[fam_name]]))
      dl <- occ_download(
        pred("taxonKey", BEE_FAMILIES[[fam_name]]),
        pred("country", "US"),
        pred("hasCoordinate", TRUE),
        pred_gte("year", 2000),
        pred("hasGeospatialIssue", FALSE),
        format = "SIMPLE_CSV",
        user = gbif_user, pwd = gbif_pwd, email = gbif_email
      )
      bee_keys[[fam_name]] <- dl[1]
      writeLines(bee_keys[[fam_name]], key_file)
      message(sprintf("  %s download key: %s", fam_name, bee_keys[[fam_name]]))
      success <- TRUE
    }, error = function(e) {
      if (grepl("too many simultaneous downloads", e$message, ignore.case = TRUE)) {
        message("  Download limit reached — waiting 60s for a slot to open...")
        Sys.sleep(60)
      } else {
        stop(e)
      }
    })
  }
}

# ---- Step 2: Wait for downloads and retrieve --------------------------------

wait_and_download <- function(key, label, cache_dir) {
  csv_cache <- file.path(cache_dir, paste0(label, "_occurrences.csv"))

  if (file.exists(csv_cache)) {
    message(sprintf("Using cached %s CSV: %s", label, csv_cache))
    return(csv_cache)
  }

  message(sprintf("Waiting for %s download (%s)...", label, key))
  # Poll until ready (checks every 30 seconds)
  status <- occ_download_meta(key)
  while (status$status != "SUCCEEDED") {
    if (status$status == "FAILED" || status$status == "CANCELLED") {
      stop(sprintf("%s download failed with status: %s", label, status$status))
    }
    message(sprintf("  Status: %s — waiting 30s...", status$status))
    Sys.sleep(30)
    status <- occ_download_meta(key)
  }
  message(sprintf("  %s download ready! DOI: %s", label, status$doi))

  # Download the ZIP
  zip_path <- file.path(cache_dir, paste0(label, ".zip"))
  occ_download_get(key, path = cache_dir, overwrite = TRUE)

  # The downloaded file is named {key}.zip
  downloaded_zip <- file.path(cache_dir, paste0(key, ".zip"))
  if (file.exists(downloaded_zip)) {
    file.rename(downloaded_zip, zip_path)
  }

  # Extract CSV from ZIP
  csv_files <- unzip(zip_path, list = TRUE)$Name
  occurrence_csv <- csv_files[grepl("occurrence", csv_files, ignore.case = TRUE)]
  if (length(occurrence_csv) == 0) occurrence_csv <- csv_files[1]

  unzip(zip_path, files = occurrence_csv, exdir = cache_dir, overwrite = TRUE)
  extracted <- file.path(cache_dir, occurrence_csv)

  # Rename to standard name
  file.rename(extracted, csv_cache)
  message(sprintf("  Extracted to: %s", csv_cache))
  csv_cache
}

lep_csv <- wait_and_download(lep_key, "lepidoptera", CACHE_DIR)

# Download all bee family CSVs
bee_csvs <- list()
for (fam_name in names(bee_keys)) {
  bee_csvs[[fam_name]] <- wait_and_download(bee_keys[[fam_name]], fam_name, CACHE_DIR)
}

# ---- Step 3: Process occurrences -> state presence --------------------------

process_gbif_csv <- function(csv_path, wildlife_type_label) {
  message(sprintf("\nProcessing %s: %s", wildlife_type_label, csv_path))

  # Read only needed columns (file may be very large)
  # GBIF SIMPLE_CSV is tab-delimited with columns: species, stateProvince, etc.
  df <- read_tsv(csv_path, col_types = cols_only(
    species = col_character(),
    stateProvince = col_character()
  ), locale = locale(encoding = "UTF-8"))

  message(sprintf("  Raw records: %d", nrow(df)))

  # Filter to records with species and state
  df <- df[!is.na(df$species) & nzchar(df$species) & !is.na(df$stateProvince), ]

  # Normalize species names to genus+epithet
  df$scientific_name <- normalize_species(df$species)

  # Convert state names to codes
  df$state_code <- STATE_NAME_TO_CODE[df$stateProvince]
  df <- df[!is.na(df$state_code), ]

  message(sprintf("  After filtering: %d records", nrow(df)))

  # Aggregate to unique (species, state) with counts
  result <- df %>%
    group_by(scientific_name, state_code) %>%
    summarise(observation_count = n(), .groups = "drop") %>%
    mutate(source = "GBIF")

  message(sprintf("  Unique species-state pairs: %d (%d species, %d states)",
                  nrow(result), n_distinct(result$scientific_name), n_distinct(result$state_code)))
  result
}

lep_presence <- process_gbif_csv(lep_csv, "Lepidoptera")

# Process all bee family CSVs
bee_presence_list <- list()
for (fam_name in names(bee_csvs)) {
  bee_presence_list[[fam_name]] <- process_gbif_csv(bee_csvs[[fam_name]], fam_name)
}
bee_presence <- bind_rows(bee_presence_list)
message(sprintf("\nAll bee families combined: %d species-state pairs (%d species)",
                nrow(bee_presence), n_distinct(bee_presence$scientific_name)))

# ---- Step 4: Combine and output ---------------------------------------------

combined <- bind_rows(lep_presence, bee_presence) %>%
  # Dedupe across families (some species may appear in multiple downloads)
  group_by(scientific_name, state_code) %>%
  summarise(
    observation_count = sum(observation_count),
    source = first(source),
    .groups = "drop"
  )

message(sprintf("\nCombined output: %d records (%d species, %d states)",
                nrow(combined), n_distinct(combined$scientific_name), n_distinct(combined$state_code)))

dir.create(dirname(OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)
write_csv(combined, OUTPUT_FILE)
message(sprintf("Written to: %s", OUTPUT_FILE))

# Quick summary
state_summary <- combined %>% count(state_code) %>% arrange(desc(n))
message("\nTop 10 states by species count:")
for (i in seq_len(min(10, nrow(state_summary)))) {
  message(sprintf("  %s: %d species", state_summary$state_code[i], state_summary$n[i]))
}

message("\n== GBIF fetch complete ==")
