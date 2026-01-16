# R/etl/load_cached_usda.R
# Load cached USDA JSON data into database tables
# Run this script once to seed the reference data from cached API responses

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})
# DBI/RPostgres loaded conditionally below

# ============================================================================
# Configuration
# ============================================================================

CACHE_DIR <- "data/cache/usda_char"
OUTPUT_DIR <- "data/processed"

# ============================================================================
# Helpers
# ============================================================================

`%||%` <- function(a, b) {
 if (is.null(a)) return(b)
 if (is.data.frame(a)) a <- as.list(a[1, , drop = TRUE])
 if (is.list(a)) a <- unlist(a, use.names = FALSE)
 if (!length(a) || all(is.na(a))) return(b)
 if (is.character(a)) {
   a <- a[!is.na(a) & nzchar(a)]
   if (!length(a)) return(b)
   return(paste(unique(a), collapse = ", "))
 }
 if (is.numeric(a)) return(a[which(!is.na(a))[1]])
 a[1]
}

safe_numeric <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- as.character(x)
  nums <- suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))
  if (all(is.na(nums))) return(NA_real_)
  nums[!is.na(nums)][1]
}

safe_char <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  paste(unique(x), collapse = ", ")
}

# Strip HTML tags from scientific names
strip_html <- function(x) {
  if (is.null(x)) return(NA_character_)
  gsub("<[^>]+>", "", as.character(x))
}

# Convert inches to mm
in_to_mm <- function(x) {
  if (is.na(x)) return(NA_integer_)
  as.integer(round(x * 25.4))
}

# Convert Fahrenheit to Celsius
f_to_c <- function(x) {
  if (is.na(x)) return(NA_integer_)
  as.integer(round((x - 32) * 5 / 9))
}

# ============================================================================
# Parse Profile JSON
# ============================================================================

parse_profile_json <- function(filepath) {
  tryCatch({
    json <- fromJSON(filepath, simplifyVector = TRUE)

    # Handle both single object and array responses
    if (is.data.frame(json)) json <- as.list(json[1, , drop = TRUE])

    tibble(
      usda_id = json$Id %||% NA_integer_,
      usda_symbol = json$Symbol %||% NA_character_,
      scientific_name = strip_html(json$ScientificName %||% NA_character_),
      common_name = json$CommonName %||% NA_character_,
      usda_group = json$Group %||% NA_character_,
      duration = safe_char(json$Durations),
      growth_habit = safe_char(json$GrowthHabits),
      native_status = {
        ns <- json$NativeStatuses
        if (is.null(ns)) NA_character_
        else if (is.data.frame(ns) && "Status" %in% names(ns)) {
          paste(unique(ns$Status), collapse = ", ")
        } else safe_char(ns)
      },
      has_characteristics = isTRUE(json$HasCharacteristics),
      has_wetland_data = isTRUE(json$HasWetlandData)
    )
  }, error = function(e) {
    message("Error parsing profile: ", filepath, " - ", e$message)
    NULL
  })
}

# ============================================================================
# Parse Characteristics JSON
# ============================================================================

parse_characteristics_json <- function(filepath) {
  tryCatch({
    json <- fromJSON(filepath, simplifyVector = TRUE)

    # USDA format: array of {PlantCharacteristicName, PlantCharacteristicValue, ...}
    if (!is.data.frame(json) ||
        !all(c("PlantCharacteristicName", "PlantCharacteristicValue") %in% names(json))) {
      return(NULL)
    }

    # Convert to named list for easy lookup
    chars <- setNames(
      as.character(json$PlantCharacteristicValue),
      as.character(json$PlantCharacteristicName)
    )

    # Extract the plant ID from filename
    fname <- basename(filepath)
    usda_id <- as.integer(gsub("char_|\\.json", "", fname))

    tibble(
      usda_id = usda_id,
      # Soil/pH
      ph_min = safe_numeric(chars["pH, Minimum"]),
      ph_max = safe_numeric(chars["pH, Maximum"]),
      soil_texture_coarse = isTRUE(chars["Adapted to Coarse Textured Soils"] == "Yes"),
      soil_texture_medium = isTRUE(chars["Adapted to Medium Textured Soils"] == "Yes"),
      soil_texture_fine = isTRUE(chars["Adapted to Fine Textured Soils"] == "Yes"),
      # Climate (convert to metric for DB)
      precip_min_in = safe_numeric(chars["Precipitation, Minimum"]),
      precip_max_in = safe_numeric(chars["Precipitation, Maximum"]),
      temp_min_f = safe_numeric(chars["Temperature, Minimum (°F)"]),
      # Tolerances
      shade_tolerance = chars["Shade Tolerance"] %||% NA_character_,
      drought_tolerance = chars["Drought Tolerance"] %||% NA_character_,
      salinity_tolerance = chars["Salinity Tolerance"] %||% NA_character_
    )
  }, error = function(e) {
    message("Error parsing characteristics: ", filepath, " - ", e$message)
    NULL
  })
}

# ============================================================================
# Main: Load All Cached Data
# ============================================================================

load_all_cached_usda <- function(cache_dir = CACHE_DIR, verbose = TRUE) {
  if (!dir.exists(cache_dir)) {
    stop("Cache directory not found: ", cache_dir)
  }

  # Find all profile and characteristics files
  profile_files <- list.files(cache_dir, pattern = "^profile_.*\\.json$", full.names = TRUE)
  char_files <- list.files(cache_dir, pattern = "^char_.*\\.json$", full.names = TRUE)

  if (verbose) {
    message(sprintf("Found %d profile files and %d characteristics files",
                    length(profile_files), length(char_files)))
  }

  # Parse profiles
  if (verbose) message("Parsing profile files...")
  profiles <- map(profile_files, parse_profile_json, .progress = verbose) %>%
    compact() %>%
    bind_rows()

  if (verbose) message(sprintf("  Parsed %d profiles", nrow(profiles)))

  # Parse characteristics
  if (verbose) message("Parsing characteristics files...")
  characteristics <- map(char_files, parse_characteristics_json, .progress = verbose) %>%
    compact() %>%
    bind_rows()

  if (verbose) message(sprintf("  Parsed %d characteristics records", nrow(characteristics)))

  # Join profiles with characteristics
  if (verbose) message("Joining data...")
  combined <- profiles %>%
    left_join(characteristics, by = "usda_id") %>%
    filter(!is.na(usda_symbol), usda_symbol != "") %>%
    distinct(usda_symbol, .keep_all = TRUE)

  if (verbose) {
    message(sprintf("Combined dataset: %d species", nrow(combined)))
    message(sprintf("  With pH data: %d", sum(!is.na(combined$ph_min) & !is.na(combined$ph_max))))
    message(sprintf("  With tolerances: %d", sum(!is.na(combined$drought_tolerance))))
  }

  combined
}

# ============================================================================
# Export to CSV (for manual import or backup)
# ============================================================================

export_to_csv <- function(data, output_dir = OUTPUT_DIR) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # ref_taxon format
  taxon_df <- data %>%
    transmute(
      usda_symbol,
      scientific_name,
      common_name,
      family = NA_character_,
      genus = str_extract(scientific_name, "^[A-Z][a-z]+"),
      usda_group
    ) %>%
    filter(!is.na(scientific_name))

  # ref_usda_characteristics format (matching existing DB schema)
  traits_df <- data %>%
    transmute(
      usda_symbol,
      ph_min,
      ph_max,
      salinity_tolerance,
      shade_tolerance,
      drought_tolerance,
      soil_texture_coarse,
      soil_texture_medium,
      soil_texture_fine,
      precipitation_min_mm = in_to_mm(precip_min_in),
      precipitation_max_mm = in_to_mm(precip_max_in),
      min_temp_c = f_to_c(temp_min_f)
    )

  taxon_path <- file.path(output_dir, "ref_taxon.csv")
  traits_path <- file.path(output_dir, "ref_usda_characteristics.csv")

  write.csv(taxon_df, taxon_path, row.names = FALSE, na = "")
  write.csv(traits_df, traits_path, row.names = FALSE, na = "")

  message(sprintf("Exported %d taxa to %s", nrow(taxon_df), taxon_path))
  message(sprintf("Exported %d traits records to %s", nrow(traits_df), traits_path))

  invisible(list(taxon = taxon_path, traits = traits_path))
}

# ============================================================================
# Load into Database (matches existing schema from for_claude_EdaphicFlora)
# ============================================================================

load_into_database <- function(data, pool = NULL) {
  if (is.null(pool)) {
    library(DBI)
    library(RPostgres)
    pool <- dbConnect(
      Postgres(),
      host = Sys.getenv("POSTGRES_HOST"),
      port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
      dbname = Sys.getenv("POSTGRES_DB"),
      user = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
      password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
      sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
    )
    on.exit(dbDisconnect(pool), add = TRUE)
  }

  message("Loading data into database...")

  # Start transaction
  dbBegin(pool)
  tryCatch({

    # 1. Insert into ref_taxon (existing schema: scientific_name, usda_symbol, family, author, notes)
    message("  Inserting taxa...")
    taxon_count <- 0
    for (i in seq_len(nrow(data))) {
      row <- data[i, ]
      if (is.na(row$scientific_name) || row$scientific_name == "") next

      # Use ON CONFLICT on the unique index (lower(scientific_name))
      res <- dbExecute(pool, "
        INSERT INTO ref_taxon (usda_symbol, scientific_name)
        VALUES ($1, $2)
        ON CONFLICT (lower(scientific_name)) DO UPDATE SET
          usda_symbol = COALESCE(EXCLUDED.usda_symbol, ref_taxon.usda_symbol)
      ", params = list(row$usda_symbol, row$scientific_name))
      taxon_count <- taxon_count + 1

      if (taxon_count %% 500 == 0) message(sprintf("    %d taxa...", taxon_count))
    }
    message(sprintf("  Inserted/updated %d taxa", taxon_count))

    # 2. Insert into ref_usda_characteristics (existing schema)
    message("  Inserting characteristics...")
    char_count <- 0
    for (i in seq_len(nrow(data))) {
      row <- data[i, ]

      # Skip if no characteristics data
      if (is.na(row$ph_min) && is.na(row$ph_max) &&
          is.na(row$drought_tolerance) && is.na(row$shade_tolerance)) next

      # Get taxon_id by scientific_name
      taxon <- dbGetQuery(pool,
        "SELECT id FROM ref_taxon WHERE lower(scientific_name) = lower($1) LIMIT 1",
        params = list(row$scientific_name))

      if (nrow(taxon) == 0) next
      taxon_id <- taxon$id[1]

      # Convert units
      precip_min_mm <- if (!is.na(row$precip_min_in)) as.integer(round(row$precip_min_in * 25.4)) else NA_integer_
      precip_max_mm <- if (!is.na(row$precip_max_in)) as.integer(round(row$precip_max_in * 25.4)) else NA_integer_
      min_temp_c <- if (!is.na(row$temp_min_f)) as.integer(round((row$temp_min_f - 32) * 5 / 9)) else NA_integer_

      dbExecute(pool, "
        INSERT INTO ref_usda_characteristics (
          taxon_id, ph_min, ph_max,
          salinity_tolerance, shade_tolerance, drought_tolerance,
          soil_texture_coarse, soil_texture_medium, soil_texture_fine,
          precipitation_min_mm, precipitation_max_mm, min_temp_c
        ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)
        ON CONFLICT (taxon_id) DO UPDATE SET
          ph_min = COALESCE(EXCLUDED.ph_min, ref_usda_characteristics.ph_min),
          ph_max = COALESCE(EXCLUDED.ph_max, ref_usda_characteristics.ph_max),
          salinity_tolerance = COALESCE(EXCLUDED.salinity_tolerance, ref_usda_characteristics.salinity_tolerance),
          shade_tolerance = COALESCE(EXCLUDED.shade_tolerance, ref_usda_characteristics.shade_tolerance),
          drought_tolerance = COALESCE(EXCLUDED.drought_tolerance, ref_usda_characteristics.drought_tolerance),
          soil_texture_coarse = COALESCE(EXCLUDED.soil_texture_coarse, ref_usda_characteristics.soil_texture_coarse),
          soil_texture_medium = COALESCE(EXCLUDED.soil_texture_medium, ref_usda_characteristics.soil_texture_medium),
          soil_texture_fine = COALESCE(EXCLUDED.soil_texture_fine, ref_usda_characteristics.soil_texture_fine),
          precipitation_min_mm = COALESCE(EXCLUDED.precipitation_min_mm, ref_usda_characteristics.precipitation_min_mm),
          precipitation_max_mm = COALESCE(EXCLUDED.precipitation_max_mm, ref_usda_characteristics.precipitation_max_mm),
          min_temp_c = COALESCE(EXCLUDED.min_temp_c, ref_usda_characteristics.min_temp_c)
      ", params = list(
        taxon_id, row$ph_min, row$ph_max,
        row$salinity_tolerance, row$shade_tolerance, row$drought_tolerance,
        row$soil_texture_coarse, row$soil_texture_medium, row$soil_texture_fine,
        precip_min_mm, precip_max_mm, min_temp_c
      ))
      char_count <- char_count + 1

      if (char_count %% 100 == 0) message(sprintf("    %d characteristics...", char_count))
    }
    message(sprintf("  Inserted/updated %d characteristic records", char_count))

    dbCommit(pool)
    message("Database load complete!")

    invisible(list(taxa = taxon_count, characteristics = char_count))

  }, error = function(e) {
    dbRollback(pool)
    stop("Database load failed: ", e$message)
  })
}

# ============================================================================
# Main Entry Point
# ============================================================================

#' Run the full ETL process
#' @param to_database If TRUE, load into database. If FALSE, export to CSV.
#' @param cache_dir Directory containing cached JSON files
#' @param pool Optional database connection pool
run_usda_cache_etl <- function(to_database = FALSE, cache_dir = CACHE_DIR, pool = NULL) {
  message("=" %>% rep(60) %>% paste(collapse = ""))
  message("Edaphic Flora | USDA Cache Loader")
  message("=" %>% rep(60) %>% paste(collapse = ""))

  # Load and parse all cached data
  data <- load_all_cached_usda(cache_dir)

  if (to_database) {
    load_into_database(data, pool)
  } else {
    export_to_csv(data)
  }

  invisible(data)
}

# Run if executed directly
if (sys.nframe() == 0) {
  # Default: export to CSV (safer for testing)
  run_usda_cache_etl(to_database = FALSE)
}
