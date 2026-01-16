# R/usda.R - USDA reference data integration
# Queries ref_taxon and ref_usda_characteristics tables

# ---------------------------
# Helper Functions
# ---------------------------

#' Null-coalescing operator
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

#' Extract canonical genus-species name from full taxon name
canonical_gs <- function(name) {
 toks <- strsplit(trimws(name %||% ""), "\\s+")[[1]]
 if (length(toks) >= 2) paste(toks[1], toks[2]) else trimws(name %||% "")
}

# ---------------------------
# USDA Taxon Resolution
# ---------------------------

#' Resolve a species name to ref_taxon record
#' Uses synonym-aware lookup: tries direct match on scientific_name or usda_symbol
#' @param gs_name Species name (genus species format or USDA symbol)
#' @param pool Database connection pool
#' @return Data frame with id, scientific_name, usda_symbol or NULL
resolve_taxon_id <- function(gs_name, pool) {
 gs <- canonical_gs(gs_name)
 if (!nzchar(gs)) return(NULL)

 tryCatch({
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
}

# ---------------------------
# USDA Characteristics Lookup
# ---------------------------

#' Get USDA characteristics for a species name
#' @param gs_name Species name
#' @param pool Database connection pool
#' @return Data frame with characteristics or NULL
get_usda_characteristics_for_name <- function(gs_name, pool) {
 row <- resolve_taxon_id(gs_name, pool)
 if (is.null(row)) return(NULL)

 tryCatch({
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

   # Fallback to ref_usda_characteristics (legacy table)
   result2 <- DBI::dbGetQuery(pool, "
     SELECT
       c.taxon_id,
       t.usda_symbol,
       t.scientific_name,
       c.ph_min AS soil_ph_min,
       c.ph_max AS soil_ph_max,
       c.salinity_tolerance,
       c.shade_tolerance,
       c.drought_tolerance,
       c.precipitation_min_mm,
       c.precipitation_max_mm,
       c.min_temp_c
     FROM ref_usda_characteristics c
     JOIN ref_taxon t ON t.id = c.taxon_id
     WHERE c.taxon_id = $1
     LIMIT 1",
     params = list(row$taxon_id))

   if (nrow(result2) == 0) return(NULL)
   result2
 }, error = function(e) {
   message("get_usda_characteristics_for_name error: ", e$message)
   NULL
 })
}

#' Alias for backwards compatibility
get_usda_traits_for_name <- function(gs_name, pool) {
 get_usda_characteristics_for_name(gs_name, pool)
}

# ---------------------------
# NWPL Wetland Indicator Lookup
# ---------------------------

#' Get NWPL national wetland indicator for a species
#' @param gs_name Species name
#' @param pool Database connection pool
#' @return Character string with indicator or NA
get_nwpl_national_for_name <- function(gs_name, pool) {
 row <- resolve_taxon_id(gs_name, pool)
 if (is.null(row)) return(NA_character_)

 tryCatch({
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
