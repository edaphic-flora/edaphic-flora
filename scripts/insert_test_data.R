# insert_test_data.R
# Inserts realistic test data for Acer rubrum and Agastache foeniculum
# to test the plant performance analysis features
#
# Run from project root:
#   Rscript scripts/insert_test_data.R

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

# Helper to insert a sample
insert_sample <- function(data) {
  # Convert NULL to NA for database compatibility
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

cat("Inserting test data for Acer rubrum and Agastache foeniculum...\n\n")

# =============================================================================
# ACER RUBRUM (Red Maple)
# - Adaptable native tree, prefers acidic to slightly acidic soils (pH 4.5-6.5)
# - Tolerates wet conditions well (natural swamp maple)
# - Struggles in alkaline soils and drought
# =============================================================================

acer_samples <- list(
  # THRIVING - Ideal conditions: acidic, moist, part sun
  list(species = "Acer rubrum", cultivar = "October Glory", ph = 5.2, organic_matter = 4.8,
       texture_sand = 35, texture_silt = 45, texture_clay = 20, texture_class = "Loam",
       location_lat = 44.9778, location_long = -93.2650, date = "2024-06-15",
       sun_exposure = "Part Sun", site_hydrology = "Mesic", outcome = "Thriving",
       nitrate_ppm = 45, phosphorus_ppm = 32, potassium_ppm = 180, calcium_ppm = 1200,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Excellent fall color, vigorous growth"),

  list(species = "Acer rubrum", cultivar = NULL, ph = 4.8, organic_matter = 6.2,
       texture_sand = 25, texture_silt = 50, texture_clay = 25, texture_class = "Silt Loam",
       location_lat = 45.1234, location_long = -93.4567, date = "2024-07-20",
       sun_exposure = "Full Sun", site_hydrology = "Wet", outcome = "Thriving",
       nitrate_ppm = 38, phosphorus_ppm = 28, potassium_ppm = 165, calcium_ppm = 980,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Growing near wetland edge - loves it"),

  list(species = "Acer rubrum", cultivar = "Red Sunset", ph = 5.5, organic_matter = 3.9,
       texture_sand = 40, texture_silt = 40, texture_clay = 20, texture_class = "Loam",
       location_lat = 44.8901, location_long = -93.1234, date = "2024-05-10",
       sun_exposure = "Part Shade", site_hydrology = "Mesic", outcome = "Thriving",
       nitrate_ppm = 52, phosphorus_ppm = 35, potassium_ppm = 195, calcium_ppm = 1450,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Understory position, doing great"),

  list(species = "Acer rubrum", cultivar = NULL, ph = 5.0, organic_matter = 5.5,
       texture_sand = 30, texture_silt = 45, texture_clay = 25, texture_class = "Loam",
       location_lat = 45.0567, location_long = -93.3456, date = "2024-08-05",
       sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Thriving",
       nitrate_ppm = 41, phosphorus_ppm = 30, potassium_ppm = 175, calcium_ppm = 1100,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Planted 3 years ago, excellent establishment"),

  # ESTABLISHED - Good but not optimal conditions
  list(species = "Acer rubrum", cultivar = "Autumn Flame", ph = 6.2, organic_matter = 3.2,
       texture_sand = 45, texture_silt = 35, texture_clay = 20, texture_class = "Loam",
       location_lat = 44.7890, location_long = -93.0123, date = "2024-06-25",
       sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Established",
       nitrate_ppm = 35, phosphorus_ppm = 25, potassium_ppm = 150, calcium_ppm = 1600,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Slightly higher pH than ideal but adapted"),

  list(species = "Acer rubrum", cultivar = NULL, ph = 5.8, organic_matter = 2.8,
       texture_sand = 55, texture_silt = 30, texture_clay = 15, texture_class = "Sandy Loam",
       location_lat = 45.2345, location_long = -93.5678, date = "2024-07-10",
       sun_exposure = "Part Sun", site_hydrology = "Dry", outcome = "Established",
       nitrate_ppm = 28, phosphorus_ppm = 22, potassium_ppm = 140, calcium_ppm = 1350,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Sandy site, needs supplemental water in drought"),

  list(species = "Acer rubrum", cultivar = "Brandywine", ph = 6.0, organic_matter = 4.1,
       texture_sand = 35, texture_silt = 40, texture_clay = 25, texture_class = "Loam",
       location_lat = 44.9012, location_long = -93.2345, date = "2024-05-28",
       sun_exposure = "Full Shade", site_hydrology = "Mesic", outcome = "Established",
       nitrate_ppm = 48, phosphorus_ppm = 31, potassium_ppm = 170, calcium_ppm = 1500,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Heavy shade - growing slowly but healthy"),

  # STRUGGLING - Suboptimal conditions
  list(species = "Acer rubrum", cultivar = "October Glory", ph = 7.2, organic_matter = 2.5,
       texture_sand = 20, texture_silt = 35, texture_clay = 45, texture_class = "Clay",
       location_lat = 44.6789, location_long = -92.8901, date = "2024-06-01",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Struggling",
       nitrate_ppm = 25, phosphorus_ppm = 18, potassium_ppm = 120, calcium_ppm = 2200,
       ecoregion_l4 = "Driftless Area", created_by = "test_user",
       notes = "Alkaline clay - showing chlorosis, iron deficiency likely"),

  list(species = "Acer rubrum", cultivar = NULL, ph = 7.5, organic_matter = 1.8,
       texture_sand = 25, texture_silt = 40, texture_clay = 35, texture_class = "Clay Loam",
       location_lat = 44.5678, location_long = -92.7890, date = "2024-07-15",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Struggling",
       nitrate_ppm = 22, phosphorus_ppm = 15, potassium_ppm = 110, calcium_ppm = 2500,
       ecoregion_l4 = "Driftless Area", created_by = "test_user",
       notes = "Very alkaline - yellowing leaves, stunted growth"),

  list(species = "Acer rubrum", cultivar = "Red Sunset", ph = 6.8, organic_matter = 2.2,
       texture_sand = 60, texture_silt = 25, texture_clay = 15, texture_class = "Sandy Loam",
       location_lat = 45.3456, location_long = -93.6789, date = "2024-08-20",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Struggling",
       nitrate_ppm = 18, phosphorus_ppm = 12, potassium_ppm = 95, calcium_ppm = 1800,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Droughty sand - leaf scorch in summer, needs irrigation"),

  # FAILED/DIED - Poor conditions
  list(species = "Acer rubrum", cultivar = "Autumn Flame", ph = 7.8, organic_matter = 1.5,
       texture_sand = 15, texture_silt = 30, texture_clay = 55, texture_class = "Clay",
       location_lat = 44.4567, location_long = -92.6789, date = "2023-05-15",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Failed/Died",
       nitrate_ppm = 20, phosphorus_ppm = 10, potassium_ppm = 85, calcium_ppm = 2800,
       ecoregion_l4 = "Driftless Area", created_by = "test_user",
       notes = "Heavy alkaline clay - severe chlorosis then death after 2 years"),

  list(species = "Acer rubrum", cultivar = NULL, ph = 7.6, organic_matter = 2.0,
       texture_sand = 20, texture_silt = 35, texture_clay = 45, texture_class = "Clay",
       location_lat = 44.3456, location_long = -92.5678, date = "2023-06-20",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Failed/Died",
       nitrate_ppm = 15, phosphorus_ppm = 8, potassium_ppm = 75, calcium_ppm = 2600,
       ecoregion_l4 = "Driftless Area", created_by = "test_user",
       notes = "Wrong site - compacted alkaline clay, no drainage, died first summer")
)

# =============================================================================
# AGASTACHE FOENICULUM (Anise Hyssop)
# - Prairie native, prefers neutral to slightly alkaline (pH 6.0-7.5)
# - REQUIRES well-drained soil - does NOT tolerate wet feet
# - Needs full sun for best performance
# =============================================================================

agastache_samples <- list(
  # THRIVING - Ideal: well-drained, full sun, neutral pH
  list(species = "Agastache foeniculum", cultivar = "Blue Fortune", ph = 6.8, organic_matter = 3.5,
       texture_sand = 50, texture_silt = 35, texture_clay = 15, texture_class = "Sandy Loam",
       location_lat = 44.9500, location_long = -93.1000, date = "2024-06-10",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Thriving",
       nitrate_ppm = 30, phosphorus_ppm = 25, potassium_ppm = 145, calcium_ppm = 1400,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Perfect prairie conditions - covered in pollinators"),

  list(species = "Agastache foeniculum", cultivar = NULL, ph = 7.0, organic_matter = 2.8,
       texture_sand = 55, texture_silt = 30, texture_clay = 15, texture_class = "Sandy Loam",
       location_lat = 44.8500, location_long = -93.0500, date = "2024-07-05",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Thriving",
       nitrate_ppm = 28, phosphorus_ppm = 22, potassium_ppm = 135, calcium_ppm = 1550,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Xeric garden - self-seeding beautifully"),

  list(species = "Agastache foeniculum", cultivar = "Golden Jubilee", ph = 6.5, organic_matter = 3.2,
       texture_sand = 45, texture_silt = 40, texture_clay = 15, texture_class = "Loam",
       location_lat = 45.0200, location_long = -93.2200, date = "2024-05-25",
       sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Thriving",
       nitrate_ppm = 35, phosphorus_ppm = 28, potassium_ppm = 155, calcium_ppm = 1300,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Well-drained loam, excellent vigor"),

  list(species = "Agastache foeniculum", cultivar = NULL, ph = 7.2, organic_matter = 2.5,
       texture_sand = 60, texture_silt = 28, texture_clay = 12, texture_class = "Sandy Loam",
       location_lat = 44.7800, location_long = -92.9500, date = "2024-08-01",
       sun_exposure = "Full Sun", site_hydrology = "Dry", outcome = "Thriving",
       nitrate_ppm = 25, phosphorus_ppm = 20, potassium_ppm = 130, calcium_ppm = 1650,
       ecoregion_l4 = "Driftless Area", created_by = "test_user",
       notes = "Limestone-derived soil, loves it"),

  # ESTABLISHED - Decent but not ideal conditions
  list(species = "Agastache foeniculum", cultivar = "Blue Fortune", ph = 6.2, organic_matter = 4.5,
       texture_sand = 40, texture_silt = 40, texture_clay = 20, texture_class = "Loam",
       location_lat = 45.1100, location_long = -93.3300, date = "2024-06-20",
       sun_exposure = "Part Sun", site_hydrology = "Mesic", outcome = "Established",
       nitrate_ppm = 40, phosphorus_ppm = 30, potassium_ppm = 160, calcium_ppm = 1250,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Afternoon shade - less floriferous but healthy"),

  list(species = "Agastache foeniculum", cultivar = NULL, ph = 5.8, organic_matter = 3.8,
       texture_sand = 45, texture_silt = 38, texture_clay = 17, texture_class = "Loam",
       location_lat = 44.9200, location_long = -93.1500, date = "2024-07-12",
       sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Established",
       nitrate_ppm = 38, phosphorus_ppm = 26, potassium_ppm = 150, calcium_ppm = 1100,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Slightly acid for this species but doing okay"),

  list(species = "Agastache foeniculum", cultivar = "Black Adder", ph = 6.5, organic_matter = 3.0,
       texture_sand = 50, texture_silt = 35, texture_clay = 15, texture_class = "Sandy Loam",
       location_lat = 45.0800, location_long = -93.2800, date = "2024-05-18",
       sun_exposure = "Part Sun", site_hydrology = "Dry", outcome = "Established",
       nitrate_ppm = 32, phosphorus_ppm = 24, potassium_ppm = 142, calcium_ppm = 1380,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Morning sun only - shorter stems but blooming well"),

  # STRUGGLING - Suboptimal conditions (too wet, too shady, or heavy soil)
  list(species = "Agastache foeniculum", cultivar = "Blue Fortune", ph = 6.0, organic_matter = 5.5,
       texture_sand = 30, texture_silt = 45, texture_clay = 25, texture_class = "Loam",
       location_lat = 45.1800, location_long = -93.4000, date = "2024-06-30",
       sun_exposure = "Part Shade", site_hydrology = "Mesic", outcome = "Struggling",
       nitrate_ppm = 48, phosphorus_ppm = 35, potassium_ppm = 175, calcium_ppm = 1200,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Too much shade, leggy growth, few flowers"),

  list(species = "Agastache foeniculum", cultivar = NULL, ph = 5.5, organic_matter = 6.0,
       texture_sand = 25, texture_silt = 50, texture_clay = 25, texture_class = "Silt Loam",
       location_lat = 45.2500, location_long = -93.4500, date = "2024-07-25",
       sun_exposure = "Part Sun", site_hydrology = "Wet", outcome = "Struggling",
       nitrate_ppm = 45, phosphorus_ppm = 32, potassium_ppm = 165, calcium_ppm = 950,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Low spot collects water - yellowing, possible root issues"),

  list(species = "Agastache foeniculum", cultivar = "Golden Jubilee", ph = 5.2, organic_matter = 7.0,
       texture_sand = 20, texture_silt = 45, texture_clay = 35, texture_class = "Clay Loam",
       location_lat = 45.3200, location_long = -93.5000, date = "2024-08-10",
       sun_exposure = "Full Sun", site_hydrology = "Mesic", outcome = "Struggling",
       nitrate_ppm = 55, phosphorus_ppm = 38, potassium_ppm = 185, calcium_ppm = 850,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Heavy clay, acid soil - poor drainage causing crown rot symptoms"),

  # FAILED/DIED - Very poor conditions (wet feet, heavy shade, waterlogged)
  list(species = "Agastache foeniculum", cultivar = NULL, ph = 5.0, organic_matter = 8.5,
       texture_sand = 15, texture_silt = 45, texture_clay = 40, texture_class = "Clay",
       location_lat = 45.4000, location_long = -93.5500, date = "2023-06-15",
       sun_exposure = "Part Shade", site_hydrology = "Wet", outcome = "Failed/Died",
       nitrate_ppm = 60, phosphorus_ppm = 42, potassium_ppm = 195, calcium_ppm = 780,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Wrong plant for site - waterlogged clay, rotted over winter"),

  list(species = "Agastache foeniculum", cultivar = "Blue Fortune", ph = 5.5, organic_matter = 7.5,
       texture_sand = 20, texture_silt = 40, texture_clay = 40, texture_class = "Clay",
       location_lat = 45.3800, location_long = -93.5200, date = "2023-07-20",
       sun_exposure = "Full Shade", site_hydrology = "Wet", outcome = "Failed/Died",
       nitrate_ppm = 52, phosphorus_ppm = 40, potassium_ppm = 190, calcium_ppm = 820,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Woodland garden attempt - complete failure, needs sun and drainage"),

  list(species = "Agastache foeniculum", cultivar = NULL, ph = 5.8, organic_matter = 6.5,
       texture_sand = 22, texture_silt = 43, texture_clay = 35, texture_class = "Clay Loam",
       location_lat = 45.4500, location_long = -93.6000, date = "2023-05-25",
       sun_exposure = "Part Sun", site_hydrology = "Wet", outcome = "Failed/Died",
       nitrate_ppm = 58, phosphorus_ppm = 45, potassium_ppm = 200, calcium_ppm = 900,
       ecoregion_l4 = "North Central Hardwood Forests", created_by = "test_user",
       notes = "Rain garden edge - standing water killed it within months")
)

# Insert all samples
cat("Inserting Acer rubrum samples...\n")
for (i in seq_along(acer_samples)) {
  insert_sample(acer_samples[[i]])
  cat(sprintf("  [%d/%d] %s - %s\n", i, length(acer_samples),
              acer_samples[[i]]$outcome, acer_samples[[i]]$notes))
}

cat("\nInserting Agastache foeniculum samples...\n")
for (i in seq_along(agastache_samples)) {
  insert_sample(agastache_samples[[i]])
  cat(sprintf("  [%d/%d] %s - %s\n", i, length(agastache_samples),
              agastache_samples[[i]]$outcome, agastache_samples[[i]]$notes))
}

# Summary
cat("\n========================================\n")
cat("Test data insertion complete!\n")
cat("========================================\n")
cat(sprintf("Acer rubrum: %d samples\n", length(acer_samples)))
cat(sprintf("  Thriving: %d, Established: %d, Struggling: %d, Failed: %d\n",
            sum(sapply(acer_samples, function(x) x$outcome == "Thriving")),
            sum(sapply(acer_samples, function(x) x$outcome == "Established")),
            sum(sapply(acer_samples, function(x) x$outcome == "Struggling")),
            sum(sapply(acer_samples, function(x) x$outcome == "Failed/Died"))))

cat(sprintf("\nAgastache foeniculum: %d samples\n", length(agastache_samples)))
cat(sprintf("  Thriving: %d, Established: %d, Struggling: %d, Failed: %d\n",
            sum(sapply(agastache_samples, function(x) x$outcome == "Thriving")),
            sum(sapply(agastache_samples, function(x) x$outcome == "Established")),
            sum(sapply(agastache_samples, function(x) x$outcome == "Struggling")),
            sum(sapply(agastache_samples, function(x) x$outcome == "Failed/Died"))))

cat("\nKey patterns to observe:\n")
cat("- Acer rubrum: Thrives in acidic (pH <6.5), moist soils; fails in alkaline clay\n")
cat("- Agastache foeniculum: Thrives in well-drained, full sun; fails in wet/heavy soil\n")

dbDisconnect(con)
cat("\nDone!\n")
