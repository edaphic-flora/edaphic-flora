# insert_usda_test_data.R
# Inserts test data for species that have USDA pH reference data
# to test the USDA overlay features in the Analysis tab
#
# Run from project root:
#   Rscript scripts/insert_usda_test_data.R

library(DBI)
library(RPostgres)

# Load environment variables
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Connect to database
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode = Sys.getenv("POSTGRES_SSLMODE", unset = "require")
)

cat("Finding species with USDA pH reference data...\n\n")

# Find species with pH data in ref_usda_traits
species_with_ph <- dbGetQuery(con, "
  SELECT
    t.scientific_name,
    t.usda_symbol,
    r.soil_ph_min,
    r.soil_ph_max,
    r.shade_tolerance,
    r.drought_tolerance,
    r.moisture_use,
    r.native_status
  FROM ref_usda_traits r
  JOIN ref_taxon t ON t.id = r.taxon_id
  WHERE r.soil_ph_min IS NOT NULL
    AND r.soil_ph_max IS NOT NULL
  ORDER BY t.scientific_name
  LIMIT 50
")

if (nrow(species_with_ph) == 0) {
  cat("No species found with USDA pH data!\n")
  cat("You may need to run the USDA ETL scripts first.\n")
  dbDisconnect(con)
  quit(save = "no")
}

cat(sprintf("Found %d species with pH data. Sample:\n", nrow(species_with_ph)))
print(head(species_with_ph, 10))

# Helper to insert a sample
insert_sample <- function(data) {
  data <- lapply(data, function(x) if (is.null(x)) NA else x)
  fields <- names(data)
  placeholders <- paste0("$", seq_along(fields))
  sql <- sprintf(
    "INSERT INTO soil_samples (%s) VALUES (%s)",
    paste(fields, collapse = ", "),
    paste(placeholders, collapse = ", ")
  )
  dbExecute(con, sql, params = as.list(unname(data)))
}

# Helper to extract simple binomial (genus + species) from full name
simple_binomial <- function(full_name) {
  parts <- strsplit(trimws(full_name), "\\s+")[[1]]
  if (length(parts) >= 2) paste(parts[1], parts[2]) else full_name
}

# Select 3 diverse species for testing
# Pick species with different pH ranges if available
test_species <- list()

# Look for a species that prefers acidic soil (ph_max < 6.5)
acidic_sp <- species_with_ph[species_with_ph$soil_ph_max <= 6.5, ]
if (nrow(acidic_sp) > 0) {
  test_species$acidic <- acidic_sp[1, ]
  test_species$acidic$simple_name <- simple_binomial(test_species$acidic$scientific_name)
  cat(sprintf("\nSelected acidic-loving species: %s (pH %.1f-%.1f)\n",
              test_species$acidic$simple_name,
              test_species$acidic$soil_ph_min,
              test_species$acidic$soil_ph_max))
}

# Look for a species that prefers neutral soil (ph range spans 6.5-7.5)
neutral_sp <- species_with_ph[species_with_ph$soil_ph_min <= 6.5 & species_with_ph$soil_ph_max >= 7.5, ]
if (nrow(neutral_sp) > 0) {
  test_species$neutral <- neutral_sp[1, ]
  test_species$neutral$simple_name <- simple_binomial(test_species$neutral$scientific_name)
  cat(sprintf("Selected neutral-tolerant species: %s (pH %.1f-%.1f)\n",
              test_species$neutral$simple_name,
              test_species$neutral$soil_ph_min,
              test_species$neutral$soil_ph_max))
}

# Look for a species that prefers alkaline soil (ph_min > 6.5)
alkaline_sp <- species_with_ph[species_with_ph$soil_ph_min >= 6.5, ]
if (nrow(alkaline_sp) > 0) {
  test_species$alkaline <- alkaline_sp[1, ]
  test_species$alkaline$simple_name <- simple_binomial(test_species$alkaline$scientific_name)
  cat(sprintf("Selected alkaline-loving species: %s (pH %.1f-%.1f)\n",
              test_species$alkaline$simple_name,
              test_species$alkaline$soil_ph_min,
              test_species$alkaline$soil_ph_max))
}

# If we didn't find specific ranges, just pick the first 3
if (length(test_species) < 3 && nrow(species_with_ph) >= 3) {
  for (i in 1:min(3, nrow(species_with_ph))) {
    if (length(test_species) < 3) {
      nm <- paste0("species_", i)
      if (!any(sapply(test_species, function(x) x$scientific_name == species_with_ph$scientific_name[i]))) {
        test_species[[nm]] <- species_with_ph[i, ]
        test_species[[nm]]$simple_name <- simple_binomial(species_with_ph$scientific_name[i])
        cat(sprintf("Selected species %d: %s (pH %.1f-%.1f)\n",
                    i, test_species[[nm]]$simple_name,
                    species_with_ph$soil_ph_min[i],
                    species_with_ph$soil_ph_max[i]))
      }
    }
  }
}

cat("\n========================================\n")
cat("Inserting test data for USDA species...\n")
cat("========================================\n\n")

# Generate test samples for each species
generate_samples <- function(sp_info) {
  # Use simple binomial (genus + species) for soil_samples.species column
  species_name <- sp_info$simple_name
  ph_min <- sp_info$soil_ph_min
  ph_max <- sp_info$soil_ph_max
  ph_mid <- (ph_min + ph_max) / 2

  samples <- list(
    # THRIVING - Within USDA pH range
    list(species = species_name, cultivar = NULL,
         ph = round(ph_mid, 1),
         organic_matter = 4.0,
         texture_sand = 40, texture_silt = 40, texture_clay = 20, texture_class = "Loam",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2024-06-15",
         sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Thriving",
         nitrate_ppm = 40, phosphorus_ppm = 30, potassium_ppm = 175, calcium_ppm = 1200,
         ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_usda",
         notes = sprintf("pH %.1f within USDA range (%.1f-%.1f)", ph_mid, ph_min, ph_max)),

    list(species = species_name, cultivar = NULL,
         ph = round(ph_min + 0.2, 1),
         organic_matter = 3.5,
         texture_sand = 45, texture_silt = 35, texture_clay = 20, texture_class = "Loam",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2024-07-10",
         sun_exposure = "Part Sun", site_hydrology = "Mesic", outcome = "Thriving",
         nitrate_ppm = 35, phosphorus_ppm = 28, potassium_ppm = 165, calcium_ppm = 1100,
         ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_usda",
         notes = sprintf("Near low end of USDA pH range")),

    list(species = species_name, cultivar = NULL,
         ph = round(ph_max - 0.2, 1),
         organic_matter = 4.5,
         texture_sand = 35, texture_silt = 45, texture_clay = 20, texture_class = "Loam",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2024-05-20",
         sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Thriving",
         nitrate_ppm = 45, phosphorus_ppm = 32, potassium_ppm = 180, calcium_ppm = 1300,
         ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_usda",
         notes = sprintf("Near high end of USDA pH range")),

    # ESTABLISHED - At edges of USDA range
    list(species = species_name, cultivar = NULL,
         ph = round(ph_min - 0.3, 1),
         organic_matter = 3.8,
         texture_sand = 50, texture_silt = 35, texture_clay = 15, texture_class = "Sandy Loam",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2024-06-25",
         sun_exposure = "Part Sun", site_hydrology = "Dry", outcome = "Established",
         nitrate_ppm = 30, phosphorus_ppm = 25, potassium_ppm = 150, calcium_ppm = 1000,
         ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_usda",
         notes = sprintf("pH slightly below USDA min (%.1f)", ph_min)),

    list(species = species_name, cultivar = NULL,
         ph = round(ph_max + 0.3, 1),
         organic_matter = 3.2,
         texture_sand = 40, texture_silt = 40, texture_clay = 20, texture_class = "Loam",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2024-07-20",
         sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Established",
         nitrate_ppm = 38, phosphorus_ppm = 27, potassium_ppm = 160, calcium_ppm = 1400,
         ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_usda",
         notes = sprintf("pH slightly above USDA max (%.1f)", ph_max)),

    # STRUGGLING - Well outside USDA range
    list(species = species_name, cultivar = NULL,
         ph = round(ph_min - 1.0, 1),
         organic_matter = 5.5,
         texture_sand = 25, texture_silt = 45, texture_clay = 30, texture_class = "Clay Loam",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2024-08-05",
         sun_exposure = "Part Shade", site_hydrology = "Wet", outcome = "Struggling",
         nitrate_ppm = 50, phosphorus_ppm = 35, potassium_ppm = 190, calcium_ppm = 800,
         ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_usda",
         notes = sprintf("pH well below USDA range - showing stress")),

    list(species = species_name, cultivar = NULL,
         ph = round(ph_max + 1.0, 1),
         organic_matter = 2.5,
         texture_sand = 30, texture_silt = 35, texture_clay = 35, texture_class = "Clay Loam",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2024-07-30",
         sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Struggling",
         nitrate_ppm = 25, phosphorus_ppm = 18, potassium_ppm = 130, calcium_ppm = 2000,
         ecoregion_l4 = "Driftless Area", created_by = "test_usda",
         notes = sprintf("pH well above USDA range - chlorosis observed")),

    # FAILED - Far outside USDA range
    list(species = species_name, cultivar = NULL,
         ph = round(max(3.5, ph_min - 1.5), 1),
         organic_matter = 6.0,
         texture_sand = 20, texture_silt = 40, texture_clay = 40, texture_class = "Clay",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2023-05-10",
         sun_exposure = "Part Shade", site_hydrology = "Wet", outcome = "Failed/Died",
         nitrate_ppm = 55, phosphorus_ppm = 40, potassium_ppm = 200, calcium_ppm = 600,
         ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_usda",
         notes = sprintf("pH far outside USDA range - did not survive")),

    list(species = species_name, cultivar = NULL,
         ph = round(min(9.0, ph_max + 1.5), 1),
         organic_matter = 1.8,
         texture_sand = 25, texture_silt = 30, texture_clay = 45, texture_class = "Clay",
         location_lat = 44.95 + runif(1, -0.2, 0.2),
         location_long = -93.10 + runif(1, -0.2, 0.2),
         date = "2023-06-15",
         sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Failed/Died",
         nitrate_ppm = 20, phosphorus_ppm = 12, potassium_ppm = 100, calcium_ppm = 2500,
         ecoregion_l4 = "Driftless Area", created_by = "test_usda",
         notes = sprintf("Extremely alkaline - severe nutrient deficiency, died"))
  )

  # Ensure pH values are within valid range (3.5 - 9.0)
  for (i in seq_along(samples)) {
    samples[[i]]$ph <- max(3.5, min(9.0, samples[[i]]$ph))
  }

  samples
}

# Insert samples for each test species
total_inserted <- 0
for (sp_key in names(test_species)) {
  sp_info <- test_species[[sp_key]]
  cat(sprintf("\n--- %s ---\n", sp_info$scientific_name))
  cat(sprintf("    USDA pH range: %.1f - %.1f\n", sp_info$soil_ph_min, sp_info$soil_ph_max))

  samples <- generate_samples(sp_info)

  for (i in seq_along(samples)) {
    tryCatch({
      insert_sample(samples[[i]])
      cat(sprintf("  [%d] %s (pH %.1f) - %s\n",
                  i, samples[[i]]$outcome, samples[[i]]$ph, samples[[i]]$notes))
      total_inserted <- total_inserted + 1
    }, error = function(e) {
      cat(sprintf("  [%d] ERROR: %s\n", i, e$message))
    })
  }
}

# Summary
cat("\n========================================\n")
cat("Test data insertion complete!\n")
cat("========================================\n")
cat(sprintf("Total samples inserted: %d\n", total_inserted))
cat(sprintf("Species with test data: %d\n", length(test_species)))

cat("\nSpecies summary:\n")
for (sp_key in names(test_species)) {
  sp <- test_species[[sp_key]]
  cat(sprintf("  - %s (USDA pH: %.1f-%.1f)\n",
              sp$simple_name, sp$soil_ph_min, sp$soil_ph_max))
}

cat("\nTo test USDA overlays:\n")
cat("1. Open the app and go to Analysis tab\n")
cat("2. Select one of the species above\n")
cat("3. Check the pH Distribution tab for USDA range overlay\n")
cat("4. Check the USDA Traits tab for reference data\n")

dbDisconnect(con)
cat("\nDone!\n")
