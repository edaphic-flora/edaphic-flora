# R/usda.R - USDA reference data integration
# Queries ref_taxon and ref_usda_characteristics tables

# ---------------------------
# In-Memory Cache
# ---------------------------

# Cache environments for USDA lookups (reduce database queries)
.usda_cache <- new.env(parent = emptyenv())
.usda_cache$taxon <- new.env(parent = emptyenv())
.usda_cache$characteristics <- new.env(parent = emptyenv())
.usda_cache$nwpl <- new.env(parent = emptyenv())
.usda_cache$native_status <- new.env(parent = emptyenv())
.usda_cache$hits <- 0L
.usda_cache$misses <- 0L

#' Clear all USDA caches
clear_usda_cache <- function() {
  .usda_cache$taxon <- new.env(parent = emptyenv())
  .usda_cache$characteristics <- new.env(parent = emptyenv())
  .usda_cache$nwpl <- new.env(parent = emptyenv())
  .usda_cache$native_status <- new.env(parent = emptyenv())
  .usda_cache$invasive <- new.env(parent = emptyenv())
  .usda_cache$hits <- 0L
  .usda_cache$misses <- 0L
  invisible(TRUE)
}

#' Get cache stats
usda_cache_stats <- function() {
  list(
    taxon_entries = length(ls(.usda_cache$taxon)),
    characteristics_entries = length(ls(.usda_cache$characteristics)),
    nwpl_entries = length(ls(.usda_cache$nwpl)),
    native_status_entries = length(ls(.usda_cache$native_status)),
    invasive_entries = length(ls(.usda_cache$invasive)),
    hits = .usda_cache$hits,
    misses = .usda_cache$misses,
    hit_rate = if (.usda_cache$hits + .usda_cache$misses > 0)
      round(.usda_cache$hits / (.usda_cache$hits + .usda_cache$misses) * 100, 1)
    else 0
  )
}

# ---------------------------
# Helper Functions
# ---------------------------

# Note: %||% operator defined in R/helpers.R (canonical definition)

#' Extract canonical genus-species name from full taxon name
canonical_gs <- function(name) {
 toks <- strsplit(trimws(name %||% ""), "\\s+")[[1]]
 if (length(toks) >= 2) paste(toks[1], toks[2]) else trimws(name %||% "")
}

# ---------------------------
# USDA Taxon Resolution
# ---------------------------

#' Resolve a species name to ref_taxon record (with caching)
#' Uses synonym-aware lookup: tries direct match on scientific_name or usda_symbol
#' @param gs_name Species name (genus species format or USDA symbol)
#' @param pool Database connection pool
#' @return Data frame with id, scientific_name, usda_symbol or NULL
resolve_taxon_id <- function(gs_name, pool) {
 gs <- canonical_gs(gs_name)
 if (!nzchar(gs)) return(NULL)

 # Check cache first
 cache_key <- tolower(gs)
 if (exists(cache_key, envir = .usda_cache$taxon, inherits = FALSE)) {
   .usda_cache$hits <- .usda_cache$hits + 1L
   return(get(cache_key, envir = .usda_cache$taxon))
 }
 .usda_cache$misses <- .usda_cache$misses + 1L

 result <- tryCatch({
   # 1) Try exact match on usda_symbol
   q1 <- DBI::dbGetQuery(pool, "
     SELECT id AS taxon_id, scientific_name, usda_symbol
     FROM ref_taxon
     WHERE usda_symbol = $1
     LIMIT 1;", params = list(toupper(gs_name)))

   if (nrow(q1)) return(q1[1, , drop = FALSE])

   # 2) Try match on genus+species portion of scientific_name
   q2 <- DBI::dbGetQuery(pool, "
     SELECT id AS taxon_id, scientific_name, usda_symbol
     FROM ref_taxon
     WHERE lower(split_part(scientific_name, ' ', 1) || ' ' || split_part(scientific_name, ' ', 2)) = lower($1)
     LIMIT 1;", params = list(gs))

   if (nrow(q2)) return(q2[1, , drop = FALSE])

   # 3) Try via synonyms table if it exists
   q3 <- DBI::dbGetQuery(pool, "
     SELECT t.id AS taxon_id, t.scientific_name, t.usda_symbol
     FROM ref_synonym s
     JOIN ref_taxon t ON t.id = s.taxon_id
     WHERE lower(split_part(s.synonym_name, ' ', 1) || ' ' || split_part(s.synonym_name, ' ', 2)) = lower($1)
     LIMIT 1;", params = list(gs))

   if (nrow(q3)) return(q3[1, , drop = FALSE])

   NULL
 }, error = function(e) {
   message("resolve_taxon_id error: ", e$message)
   NULL
 })

 # Cache the result (even NULL results to avoid repeated lookups)
 assign(cache_key, result, envir = .usda_cache$taxon)
 result
}

# ---------------------------
# USDA Characteristics Lookup
# ---------------------------

#' Get USDA characteristics for a species name (with caching)
#' @param gs_name Species name
#' @param pool Database connection pool
#' @return Data frame with characteristics or NULL
get_usda_characteristics_for_name <- function(gs_name, pool) {
 gs <- canonical_gs(gs_name)
 if (!nzchar(gs)) return(NULL)

 # Check cache first
 cache_key <- tolower(gs)
 if (exists(cache_key, envir = .usda_cache$characteristics, inherits = FALSE)) {
   .usda_cache$hits <- .usda_cache$hits + 1L
   return(get(cache_key, envir = .usda_cache$characteristics))
 }
 .usda_cache$misses <- .usda_cache$misses + 1L

 row <- resolve_taxon_id(gs_name, pool)
 if (is.null(row)) {
   assign(cache_key, NULL, envir = .usda_cache$characteristics)
   return(NULL)
 }

 result <- tryCatch({
   # Try ref_usda_traits first (new table from fetch_usda_data.R)
   result <- DBI::dbGetQuery(pool, "
     SELECT
       r.taxon_id,
       t.usda_symbol,
       t.scientific_name,
       r.soil_ph_min,
       r.soil_ph_max,
       r.salinity_tolerance,
       r.shade_tolerance,
       r.drought_tolerance,
       r.moisture_use,
       r.bloom_period,
       r.duration,
       r.growth_habit,
       r.native_status,
       r.precip_min_in,
       r.precip_max_in,
       r.temp_min_f
     FROM ref_usda_traits r
     JOIN ref_taxon t ON t.id = r.taxon_id
     WHERE r.taxon_id = $1
     LIMIT 1",
     params = list(row$taxon_id))

   if (nrow(result) > 0) return(result)

   NULL
 }, error = function(e) {
   message("get_usda_characteristics_for_name error: ", e$message)
   NULL
 })

 # Cache the result
 assign(cache_key, result, envir = .usda_cache$characteristics)
 result
}

#' Alias for backwards compatibility
get_usda_traits_for_name <- function(gs_name, pool) {
 get_usda_characteristics_for_name(gs_name, pool)
}

# ---------------------------
# NWPL Wetland Indicator Lookup
# ---------------------------

#' Get NWPL national wetland indicator for a species (with caching)
#' @param gs_name Species name
#' @param pool Database connection pool
#' @return Character string with indicator or NA
get_nwpl_national_for_name <- function(gs_name, pool) {
 gs <- canonical_gs(gs_name)
 if (!nzchar(gs)) return(NA_character_)

 # Check cache first
 cache_key <- tolower(gs)
 if (exists(cache_key, envir = .usda_cache$nwpl, inherits = FALSE)) {
   .usda_cache$hits <- .usda_cache$hits + 1L
   return(get(cache_key, envir = .usda_cache$nwpl))
 }
 .usda_cache$misses <- .usda_cache$misses + 1L

 row <- resolve_taxon_id(gs_name, pool)
 if (is.null(row)) {
   assign(cache_key, NA_character_, envir = .usda_cache$nwpl)
   return(NA_character_)
 }

 result <- tryCatch({
   # Try national indicator first
   res <- DBI::dbGetQuery(pool, "
     SELECT indicator
     FROM ref_wetland_indicator
     WHERE taxon_id = $1 AND region = 'National'
     LIMIT 1;", params = list(row$taxon_id))

   if (nrow(res) && !is.na(res$indicator[1])) {
     return(res$indicator[1])
   }

   # Fallback: get most common indicator across regions
   res2 <- DBI::dbGetQuery(pool, "
     SELECT indicator, COUNT(*) as cnt
     FROM ref_wetland_indicator
     WHERE taxon_id = $1
     GROUP BY indicator
     ORDER BY cnt DESC
     LIMIT 1;", params = list(row$taxon_id))

   if (nrow(res2) && !is.na(res2$indicator[1])) {
     return(res2$indicator[1])
   }

   NA_character_
 }, error = function(e) NA_character_)

 # Cache the result
 assign(cache_key, result, envir = .usda_cache$nwpl)
 result
}

#' Lookup NWPL indicator by WCVP name (alternate)
#' @param gs_name Species name
#' @param pool Database connection pool
#' @return Character string with indicator or NA
lookup_nwpl_national_by_wcvp <- function(gs_name, pool) {
 get_nwpl_national_for_name(gs_name, pool)
}

# ---------------------------
# Reference Data Summary
# ---------------------------

#' Check if USDA reference data exists for a species
#' @param gs_name Species name
#' @param pool Database connection pool
#' @return Logical
has_usda_reference <- function(gs_name, pool) {
 tr <- get_usda_characteristics_for_name(gs_name, pool)
 !is.null(tr) && nrow(tr) > 0
}

#' Get a summary of available reference data for a species
#' @param gs_name Species name
#' @param pool Database connection pool
#' @return List with has_traits, has_nwpl, traits, nwpl_indicator
get_reference_summary <- function(gs_name, pool) {
 traits <- get_usda_characteristics_for_name(gs_name, pool)
 nwpl <- get_nwpl_national_for_name(gs_name, pool)

 list(
   has_traits = !is.null(traits) && nrow(traits) > 0,
   has_nwpl = !is.na(nwpl) && nzchar(nwpl),
   traits = traits,
   nwpl_indicator = nwpl
 )
}

# ---------------------------
# Formatted Display Helpers
# ---------------------------

#' Format soil texture as readable string
format_soil_texture <- function(coarse, medium, fine) {
 textures <- c()
 if (isTRUE(coarse)) textures <- c(textures, "Coarse")
 if (isTRUE(medium)) textures <- c(textures, "Medium")
 if (isTRUE(fine)) textures <- c(textures, "Fine")
 if (length(textures) == 0) return(NA_character_)
 paste(textures, collapse = ", ")
}

#' Convert mm to inches for display
mm_to_in <- function(mm) {
 if (is.na(mm)) return(NA_real_)
 round(mm / 25.4, 1)
}

#' Convert Celsius to Fahrenheit for display
c_to_f <- function(c) {
 if (is.na(c)) return(NA_integer_)
 as.integer(round(c * 9 / 5 + 32))
}

# ---------------------------
# Native Status Lookup (North America)
# ---------------------------

# Region code descriptions for tooltips
.region_names <- c(
  "L48" = "Lower 48 US States",
  "AK" = "Alaska",
  "HI" = "Hawaii",
  "CAN" = "Canada",
  "PR" = "Puerto Rico",
  "VI" = "US Virgin Islands",
  "GL" = "Greenland",
  "SPM" = "St. Pierre & Miquelon",
  "PB" = "Pacific Basin"
)

#' Parse native_status string from ref_usda_traits for North America badge
#' @param native_status_str The native_status string (e.g., "L48, N, Native" or "CAN, L48, I, Introduced")
#' @return List with: status ("native", "introduced", "both", "unknown"),
#'         native_regions, introduced_regions, tooltip_text
parse_native_status_na <- function(native_status_str) {
  # Default unknown result
  unknown <- list(
    status = "unknown",
    native_regions = character(0),
    introduced_regions = character(0),
    tooltip = "Native status data not available for this species."
  )


  if (is.null(native_status_str) || is.na(native_status_str) || !nzchar(native_status_str)) {
    return(unknown)
  }

  # Check for native and introduced markers

  # Format can be: "L48, N, Native" or "L48, I, Introduced" or "L48, N, I, Native, Introduced"
  # Look for ", N," or ", N$" (N as standalone code) AND "Native" anywhere
  has_native <- (grepl(", N,", native_status_str) || grepl(", N$", native_status_str)) &&
                grepl("Native", native_status_str, fixed = TRUE)
  has_introduced <- (grepl(", I,", native_status_str) || grepl(", I$", native_status_str)) &&
                    grepl("Introduced", native_status_str, fixed = TRUE)

  # Extract region codes (uppercase 2-3 letter codes at start or after comma)
  # Known regions: L48, AK, HI, CAN, PR, VI, GL, SPM, PB
  parts <- strsplit(native_status_str, ",\\s*")[[1]]
  region_codes <- parts[parts %in% names(.region_names)]

  # Determine which regions are native vs introduced
  # The format appears to be: regions, then N/I, then Native/Introduced
  # If there's only one status type, all regions have that status
  native_regions <- character(0)
  introduced_regions <- character(0)

  if (has_native && !has_introduced) {
    native_regions <- region_codes
  } else if (has_introduced && !has_native) {
    introduced_regions <- region_codes
  } else if (has_native && has_introduced) {
    # Species is both native and introduced in different regions
    # For simplicity, mark all regions as "mixed" context
    native_regions <- region_codes
    introduced_regions <- region_codes
  }

  # Build tooltip text
  tooltip_parts <- c()

  if (length(native_regions) > 0 && !has_introduced) {
    region_labels <- sapply(native_regions, function(r) .region_names[r] %||% r)
    if (length(region_labels) == length(names(.region_names)[names(.region_names) %in% c("L48", "AK", "HI", "CAN", "PR", "VI")])) {
      tooltip_parts <- c(tooltip_parts, "Native throughout North America")
    } else {
      tooltip_parts <- c(tooltip_parts, paste0("Native to: ", paste(region_labels, collapse = ", ")))
    }
  }

  if (length(introduced_regions) > 0 && !has_native) {
    region_labels <- sapply(introduced_regions, function(r) .region_names[r] %||% r)
    tooltip_parts <- c(tooltip_parts, paste0("Introduced to: ", paste(region_labels, collapse = ", ")))
  }

  if (has_native && has_introduced) {
    tooltip_parts <- c(tooltip_parts, "Native to some North American regions, introduced to others")
  }

  # Determine overall status
  status <- if (has_native && has_introduced) {
    "both"
  } else if (has_native) {
    "native"
  } else if (has_introduced) {
    "introduced"
  } else {
    "unknown"
  }

  list(
    status = status,
    native_regions = native_regions,
    introduced_regions = introduced_regions,
    tooltip = paste(tooltip_parts, collapse = ". ")
  )
}

#' Get native status for North America from ref_usda_traits (with caching)
#' @param gs_name Species name (genus species format)
#' @param pool Database connection pool
#' @return List with status, native_regions, introduced_regions, tooltip
get_native_status_na <- function(gs_name, pool) {
  gs <- canonical_gs(gs_name)
  if (!nzchar(gs)) {
    return(parse_native_status_na(NULL))
  }

  # Check cache first
  cache_key <- paste0("na_", tolower(gs))
  if (exists(cache_key, envir = .usda_cache$native_status, inherits = FALSE)) {
    .usda_cache$hits <- .usda_cache$hits + 1L
    return(get(cache_key, envir = .usda_cache$native_status))
  }
  .usda_cache$misses <- .usda_cache$misses + 1L

  result <- tryCatch({
    # Get native_status from ref_usda_traits
    traits <- get_usda_characteristics_for_name(gs_name, pool)

    native_status_str <- NULL
    if (!is.null(traits) && nrow(traits) > 0 && !is.na(traits$native_status[1])) {
      native_status_str <- traits$native_status[1]
    }

    # If no native_status, try to find another record with the same USDA symbol
    if (is.null(native_status_str) && !is.null(traits) && nrow(traits) > 0 && !is.na(traits$usda_symbol[1])) {
      symbol_lookup <- DBI::dbGetQuery(pool, "
        SELECT native_status
        FROM ref_usda_traits
        WHERE usda_symbol = $1 AND native_status IS NOT NULL
        LIMIT 1",
        params = list(traits$usda_symbol[1]))
      if (nrow(symbol_lookup) > 0) {
        native_status_str <- symbol_lookup$native_status[1]
      }
    }

    parse_native_status_na(native_status_str)
  }, error = function(e) {
    message("get_native_status_na error: ", e$message)
    parse_native_status_na(NULL)
  })

  # Cache the result
  assign(cache_key, result, envir = .usda_cache$native_status)
  result
}

#' Get common name for a species from USDA data
#' @param gs_name Species name (genus species format)
#' @param pool Database connection pool
#' @return Character string with common name or NULL
get_usda_common_name <- function(gs_name, pool) {
  gs <- canonical_gs(gs_name)
  if (!nzchar(gs)) return(NULL)

  # Check cache
  cache_key <- paste0("cn_", tolower(gs))
  if (exists(cache_key, envir = .usda_cache$taxon, inherits = FALSE)) {
    cached <- get(cache_key, envir = .usda_cache$taxon)
    if (!is.null(cached)) return(cached)
  }

  result <- tryCatch({
    # Try to get common_name from ref_usda_traits if column exists
    res <- DBI::dbGetQuery(pool, "
      SELECT r.common_name
      FROM ref_taxon t
      JOIN ref_usda_traits r ON r.taxon_id = t.id
      WHERE lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2)) = lower($1)
        AND r.common_name IS NOT NULL
      LIMIT 1",
      params = list(gs))
    if (nrow(res) > 0 && !is.na(res$common_name[1])) {
      return(res$common_name[1])
    }
    NULL
  }, error = function(e) {
    # Column might not exist yet
    NULL
  })

  # Cache the result
  assign(cache_key, result, envir = .usda_cache$taxon)
  result
}

# ---------------------------
# State-Level Native Status Lookup (Legacy)
# ---------------------------

#' Get native status for a species in a specific state (with caching)
#' @param gs_name Species name (genus species format)
#' @param state_code Two-letter US state abbreviation
#' @param pool Database connection pool
#' @return Character: "Native", "Introduced", "Both", or NA if unknown
get_native_status_for_state <- function(gs_name, state_code, pool) {
  gs <- canonical_gs(gs_name)
  if (!nzchar(gs) || is.na(state_code) || !nzchar(state_code)) return(NA_character_)

  # Check cache first
  cache_key <- paste0(tolower(gs), "_", toupper(state_code))
  if (exists(cache_key, envir = .usda_cache$native_status, inherits = FALSE)) {
    .usda_cache$hits <- .usda_cache$hits + 1L
    return(get(cache_key, envir = .usda_cache$native_status))
  }
  .usda_cache$misses <- .usda_cache$misses + 1L

  result <- tryCatch({
    # First resolve taxon_id
    taxon <- resolve_taxon_id(gs_name, pool)
    if (is.null(taxon)) return(NA_character_)

    # Query native status for this state
    res <- DBI::dbGetQuery(pool, "
      SELECT native_status, source
      FROM ref_state_distribution
      WHERE taxon_id = $1 AND state_code = $2
      LIMIT 1;",
      params = list(taxon$taxon_id, toupper(state_code)))

    if (nrow(res) > 0 && !is.na(res$native_status[1])) {
      result <- res$native_status[1]
      attr(result, "source") <- res$source[1]
      result
    } else {
      NA_character_
    }
  }, error = function(e) {
    message("get_native_status_for_state error: ", e$message)
    NA_character_
  })

  # Cache the result
  assign(cache_key, result, envir = .usda_cache$native_status)
  result
}

#' Get native status summary for a species across multiple states
#' @param gs_name Species name (genus species format)
#' @param state_codes Vector of two-letter US state abbreviations
#' @param pool Database connection pool
#' @return List with: native_states, introduced_states, both_states, unknown_states, summary
get_native_status_summary <- function(gs_name, state_codes, pool) {
  if (length(state_codes) == 0) {
    return(list(
      native_states = character(0),
      introduced_states = character(0),
      both_states = character(0),
      unknown_states = character(0),
      summary = "unknown"
    ))
  }

  # Look up status for each state
  statuses <- sapply(state_codes, function(st) {
    get_native_status_for_state(gs_name, st, pool)
  }, USE.NAMES = TRUE)

  native_states <- names(statuses[statuses == "Native" & !is.na(statuses)])
  introduced_states <- names(statuses[statuses == "Introduced" & !is.na(statuses)])
  both_states <- names(statuses[statuses == "Both" & !is.na(statuses)])
  unknown_states <- names(statuses[is.na(statuses)])

  # Determine overall summary
  has_native <- length(native_states) > 0 || length(both_states) > 0
  has_introduced <- length(introduced_states) > 0 || length(both_states) > 0
  all_unknown <- length(unknown_states) == length(state_codes)

  summary <- if (all_unknown) {
    "unknown"
  } else if (has_native && has_introduced) {
    "mixed"
  } else if (has_native) {
    "native"
  } else if (has_introduced) {
    "introduced"
  } else {
    "unknown"
  }

  list(
    native_states = native_states,
    introduced_states = introduced_states,
    both_states = both_states,
    unknown_states = unknown_states,
    summary = summary
  )
}

# ---------------------------
# Noxious/Invasive Status Lookup
# ---------------------------

# Cache for invasive status lookups
.usda_cache$invasive <- new.env(parent = emptyenv())

#' Get invasive/noxious status for a species
#' Checks user's state first, then returns info about other states
#' @param gs_name Species name (genus species format)
#' @param user_state Two-letter US state code (user's home state, or NULL)
#' @param pool Database connection pool
#' @return List with:
#'   - in_user_state: TRUE if invasive/noxious in user's state, FALSE if not, NA if unknown
#'   - user_state_designation: "Federal Noxious", "State Noxious", "Invasive", or NULL
#'   - in_other_states: TRUE if invasive in any other US state
#'   - states_listed: character vector of state codes where species is listed
#'   - designations: character vector of unique designations
#'   - is_federal: TRUE if on federal noxious weed list
get_invasive_status <- function(gs_name, user_state = NULL, pool) {
  gs <- canonical_gs(gs_name)
  if (!nzchar(gs)) {
    return(list(
      in_user_state = NA,
      user_state_designation = NULL,
      in_other_states = FALSE,
      states_listed = character(0),
      designations = character(0),
      is_federal = FALSE
    ))
  }

  # Check cache first
  cache_key <- paste0(tolower(gs), "_inv")
  cached_data <- NULL
  if (exists(cache_key, envir = .usda_cache$invasive, inherits = FALSE)) {
    .usda_cache$hits <- .usda_cache$hits + 1L
    cached_data <- get(cache_key, envir = .usda_cache$invasive)
  } else {
    .usda_cache$misses <- .usda_cache$misses + 1L
  }

  # Fetch from database if not cached
  if (is.null(cached_data)) {
    cached_data <- tryCatch({
      # Resolve taxon_id first
      taxon <- resolve_taxon_id(gs_name, pool)
      if (is.null(taxon)) {
        list(records = data.frame(), taxon_found = FALSE)
      } else {
        # Query all invasive/noxious records for this taxon
        records <- DBI::dbGetQuery(pool, "
          SELECT state_code, designation, source
          FROM ref_noxious_invasive
          WHERE taxon_id = $1
        ", params = list(taxon$taxon_id))

        list(records = records, taxon_found = TRUE)
      }
    }, error = function(e) {
      message("get_invasive_status error: ", e$message)
      list(records = data.frame(), taxon_found = FALSE)
    })

    # Cache the result
    assign(cache_key, cached_data, envir = .usda_cache$invasive)
  }

  # Process cached data
  records <- cached_data$records

  if (nrow(records) == 0) {
    return(list(
      in_user_state = if (cached_data$taxon_found) FALSE else NA,
      user_state_designation = NULL,
      in_other_states = FALSE,
      states_listed = character(0),
      designations = character(0),
      is_federal = FALSE
    ))
  }

  # Check for federal noxious listing (state_code is NULL or "US")
  federal_records <- records[is.na(records$state_code) | records$state_code == "US", ]
  is_federal <- nrow(federal_records) > 0

  # Get state-specific records
  state_records <- records[!is.na(records$state_code) & records$state_code != "US", ]
  states_listed <- unique(state_records$state_code)
  designations <- unique(c(federal_records$designation, state_records$designation))

  # Check user's state
  in_user_state <- NA
  user_state_designation <- NULL

  if (!is.null(user_state) && nzchar(user_state)) {
    user_state <- toupper(user_state)

    # Federal noxious applies to all states
    if (is_federal) {
      in_user_state <- TRUE
      user_state_designation <- federal_records$designation[1]
    } else if (user_state %in% states_listed) {
      in_user_state <- TRUE
      user_record <- state_records[state_records$state_code == user_state, ]
      user_state_designation <- user_record$designation[1]
    } else {
      in_user_state <- FALSE
    }
  }

  # Check if in other states (excluding user's state)
  other_states <- states_listed[states_listed != toupper(user_state %||% "")]
  in_other_states <- length(other_states) > 0 || is_federal

  list(
    in_user_state = in_user_state,
    user_state_designation = user_state_designation,
    in_other_states = in_other_states,
    states_listed = states_listed,
    designations = designations,
    is_federal = is_federal
  )
}

#' Get native status for user's home state (uses user preferences)
#' @param gs_name Species name (genus species format)
#' @param user_prefs List from db_get_user_prefs() with home_state
#' @param pool Database connection pool
#' @return List with: status ("native", "introduced", "unknown"), state_code, tooltip
get_native_status_for_user <- function(gs_name, user_prefs, pool) {
  # Default result when no preferences set
  no_prefs_result <- list(
    status = "no_prefs",
    state_code = NULL,
    state_name = NULL,
    tooltip = "Set your home location in Preferences to see state-specific native status."
  )

  if (is.null(user_prefs) || is.null(user_prefs$home_state) || !nzchar(user_prefs$home_state)) {
    return(no_prefs_result)
  }

  state_code <- toupper(user_prefs$home_state)

  # Get state name for display
  state_names <- c(
    AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California",
    CO = "Colorado", CT = "Connecticut", DE = "Delaware", FL = "Florida", GA = "Georgia",
    HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa",
    KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
    MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi", MO = "Missouri",
    MT = "Montana", NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", NJ = "New Jersey",
    NM = "New Mexico", NY = "New York", NC = "North Carolina", ND = "North Dakota", OH = "Ohio",
    OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina",
    SD = "South Dakota", TN = "Tennessee", TX = "Texas", UT = "Utah", VT = "Vermont",
    VA = "Virginia", WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming",
    DC = "District of Columbia", PR = "Puerto Rico", VI = "Virgin Islands"
  )
  state_name <- state_names[state_code] %||% state_code

  # Check state-specific native status in ref_state_distribution
  status <- get_native_status_for_state(gs_name, state_code, pool)

  if (is.na(status)) {
    # Fall back to North America-level native status
    na_status <- get_native_status_na(gs_name, pool)

    if (na_status$status == "native") {
      return(list(
        status = "native_na",
        state_code = state_code,
        state_name = state_name,
        tooltip = paste0("Native to other US states, not recorded in ", state_name, " (USDA)")
      ))
    } else if (na_status$status == "introduced") {
      return(list(
        status = "introduced_na",
        state_code = state_code,
        state_name = state_name,
        tooltip = paste0("Introduced to North America, not native to ", state_name, " (USDA)")
      ))
    } else if (na_status$status == "both") {
      return(list(
        status = "native_na",
        state_code = state_code,
        state_name = state_name,
        tooltip = paste0("Native to other US states, not recorded in ", state_name, " (USDA)")
      ))
    } else {
      return(list(
        status = "unknown",
        state_code = state_code,
        state_name = state_name,
        tooltip = paste0("Native status not available for ", state_name)
      ))
    }
  }

  # We have state-specific data — include source attribution if available
  source_tag <- attr(status, "source")
  source_suffix <- if (!is.null(source_tag) && nzchar(source_tag)) paste0(" (", source_tag, ")") else ""

  if (status == "Native") {
    list(
      status = "native",
      state_code = state_code,
      state_name = state_name,
      tooltip = paste0("Native to ", state_name, source_suffix)
    )
  } else if (status == "Introduced") {
    list(
      status = "introduced",
      state_code = state_code,
      state_name = state_name,
      tooltip = paste0("Introduced in ", state_name, " (not native)", source_suffix)
    )
  } else if (status == "Both") {
    list(
      status = "both",
      state_code = state_code,
      state_name = state_name,
      tooltip = paste0("Native to parts of ", state_name, ", introduced in others", source_suffix)
    )
  } else {
    list(
      status = "unknown",
      state_code = state_code,
      state_name = state_name,
      tooltip = paste0("Native status data not available for ", state_name)
    )
  }
}
