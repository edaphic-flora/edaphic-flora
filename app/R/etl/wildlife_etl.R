# R/etl/wildlife_etl.R — Load wildlife interaction data into ref tables
# Source: Proprietary Excel files from edaphic consulting database
#
# Source directory layout:
#   plant_species.xlsx
#   pollinator_species.xlsx
#   plant_pollinator_interactions.xlsx
#   bird_species.xlsx
#   plant_bird_interactions.xlsx
#   reference/specialist_bees_by_genus.csv
#   reference/lep_species_master.csv (optional, for Mid-Atlantic filtering)
#
# Usage:
#   source("app/R/etl/wildlife_etl.R")
#   wildlife_etl_run()   # uses default consulting directory
#   wildlife_etl_run("data/wildlife_source/")  # or custom path

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(readxl)
  library(readr)
  library(dplyr)
  library(stringr)
})

# ---- Configuration -----------------------------------------------------------

wildlife_default_dir <- "C:/Users/toddt/OneDrive/Desktop/edaphic consulting/02_Analysis/Databases/"

# ---- Main ETL ----------------------------------------------------------------

wildlife_etl_run <- function(data_dir = wildlife_default_dir) {
  message("== Edaphic Flora | Wildlife Data ETL ==")
  message("Reading from: ", data_dir)

  if (!dir.exists(data_dir)) {
    stop("Wildlife data directory not found: ", data_dir)
  }

  # --- Read source files ---
  message("\n--- Reading source files ---")

  # Plants (required)
  plants_file <- file.path(data_dir, "plant_species.xlsx")
  if (!file.exists(plants_file)) stop("Missing: ", plants_file)
  plants_df <- read_excel(plants_file)
  message(sprintf("  plant_species.xlsx: %d rows, %d cols", nrow(plants_df), ncol(plants_df)))

  # Pollinators (required)
  poll_sp_file <- file.path(data_dir, "pollinator_species.xlsx")
  if (!file.exists(poll_sp_file)) stop("Missing: ", poll_sp_file)
  poll_sp_df <- read_excel(poll_sp_file)
  message(sprintf("  pollinator_species.xlsx: %d rows", nrow(poll_sp_df)))

  # Pollinator interactions (required)
  poll_int_file <- file.path(data_dir, "plant_pollinator_interactions.xlsx")
  if (!file.exists(poll_int_file)) stop("Missing: ", poll_int_file)
  poll_int_df <- read_excel(poll_int_file)
  message(sprintf("  plant_pollinator_interactions.xlsx: %d rows", nrow(poll_int_df)))

  # Birds (optional)
  bird_sp_file <- file.path(data_dir, "bird_species.xlsx")
  bird_sp_df <- if (file.exists(bird_sp_file)) {
    df <- read_excel(bird_sp_file)
    message(sprintf("  bird_species.xlsx: %d rows", nrow(df)))
    df
  } else {
    message("  bird_species.xlsx: not found, skipping")
    data.frame()
  }

  # Bird interactions (optional)
  bird_int_file <- file.path(data_dir, "plant_bird_interactions.xlsx")
  bird_int_df <- if (file.exists(bird_int_file)) {
    df <- read_excel(bird_int_file)
    message(sprintf("  plant_bird_interactions.xlsx: %d rows", nrow(df)))
    df
  } else {
    message("  plant_bird_interactions.xlsx: not found, skipping")
    data.frame()
  }

  # Specialist bees (optional, in reference/ subdirectory)
  spec_bees_file <- file.path(data_dir, "reference", "specialist_bees_by_genus.csv")
  spec_bees_df <- if (file.exists(spec_bees_file)) {
    df <- read_csv(spec_bees_file, show_col_types = FALSE)
    message(sprintf("  specialist_bees_by_genus.csv: %d rows", nrow(df)))
    df
  } else {
    message("  specialist_bees_by_genus.csv: not found, skipping")
    data.frame()
  }

  # Lep species master (optional, for Mid-Atlantic geographic tagging)
  lep_master_file <- file.path(data_dir, "reference", "lep_species_master.csv")
  lep_master_df <- if (file.exists(lep_master_file)) {
    df <- read_csv(lep_master_file, show_col_types = FALSE)
    message(sprintf("  lep_species_master.csv: %d rows (%d midatlantic_present)",
                    nrow(df), sum(df$midatlantic_present, na.rm = TRUE)))
    df
  } else {
    message("  lep_species_master.csv: not found, skipping Mid-Atlantic tagging")
    data.frame()
  }

  # --- Connect to database ---
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("POSTGRES_HOST"),
    port = as.integer(Sys.getenv("POSTGRES_PORT")),
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
    password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
    sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Ensure tables exist
  message("\n--- Ensuring tables exist ---")
  for (ddl in c(
    "CREATE TABLE IF NOT EXISTS ref_wildlife_plants (
      id SERIAL PRIMARY KEY,
      taxon_id INTEGER REFERENCES ref_taxon(id) ON DELETE CASCADE,
      species_code VARCHAR(50) UNIQUE NOT NULL,
      genus VARCHAR(100),
      life_form VARCHAR(50),
      lepidoptera_species_count INTEGER DEFAULT 0,
      specialist_bee_species_count INTEGER DEFAULT 0,
      songbird_value VARCHAR(50),
      bird_food TEXT,
      hummingbird_plant BOOLEAN DEFAULT FALSE,
      is_keystone_genus BOOLEAN DEFAULT FALSE,
      updated_at TIMESTAMPTZ DEFAULT now()
    )",
    "CREATE TABLE IF NOT EXISTS ref_wildlife_species (
      id SERIAL PRIMARY KEY,
      wildlife_id VARCHAR(50) UNIQUE NOT NULL,
      scientific_name TEXT NOT NULL,
      common_name TEXT,
      wildlife_type VARCHAR(30) NOT NULL,
      family VARCHAR(100),
      specialist_generalist VARCHAR(30),
      functional_group VARCHAR(50),
      updated_at TIMESTAMPTZ DEFAULT now()
    )",
    "CREATE TABLE IF NOT EXISTS ref_wildlife_interactions (
      id SERIAL PRIMARY KEY,
      plant_species_code VARCHAR(50) NOT NULL,
      wildlife_id VARCHAR(50) NOT NULL,
      interaction_type VARCHAR(50) NOT NULL,
      evidence_level VARCHAR(30),
      source TEXT,
      updated_at TIMESTAMPTZ DEFAULT now(),
      UNIQUE(plant_species_code, wildlife_id, interaction_type)
    )",
    "CREATE TABLE IF NOT EXISTS ref_specialist_bees_by_genus (
      id SERIAL PRIMARY KEY,
      host_genus VARCHAR(100) UNIQUE NOT NULL,
      specialist_bee_count INTEGER DEFAULT 0,
      specialist_bee_species TEXT,
      updated_at TIMESTAMPTZ DEFAULT now()
    )"
  )) {
    DBI::dbExecute(con, ddl)
  }

  # =========================================================================
  # 1. ref_wildlife_plants (from plant_species.xlsx)
  # =========================================================================
  # Source cols: Species_code, Genus, Scientific_name, Life_form,
  #   Lepidoptera_species_count, Specialist_bee_species_count,
  #   Songbird_value, Bird_food, Hummingbird_plant, Is_keystone_genus
  message("\n--- Loading ref_wildlife_plants ---")

  staging_plants <- data.frame(
    species_code = plants_df$Species_code,
    genus = plants_df$Genus,
    scientific_name = plants_df$Scientific_name,
    life_form = plants_df$Life_form,
    lepidoptera_species_count = as.integer(plants_df$Lepidoptera_species_count),
    specialist_bee_species_count = as.integer(plants_df$Specialist_bee_species_count),
    songbird_value = if ("Songbird_value" %in% names(plants_df)) as.character(plants_df$Songbird_value) else NA_character_,
    bird_food = if ("Bird_food" %in% names(plants_df)) as.character(plants_df$Bird_food) else NA_character_,
    hummingbird_plant = if ("Hummingbird_plant" %in% names(plants_df)) as.logical(plants_df$Hummingbird_plant) else FALSE,
    is_keystone_genus = if ("Is_keystone_genus" %in% names(plants_df)) as.logical(plants_df$Is_keystone_genus) else FALSE,
    stringsAsFactors = FALSE
  )
  # Replace NAs in boolean/integer columns
  staging_plants$lepidoptera_species_count[is.na(staging_plants$lepidoptera_species_count)] <- 0L
  staging_plants$specialist_bee_species_count[is.na(staging_plants$specialist_bee_species_count)] <- 0L
  staging_plants$hummingbird_plant[is.na(staging_plants$hummingbird_plant)] <- FALSE
  staging_plants$is_keystone_genus[is.na(staging_plants$is_keystone_genus)] <- FALSE

  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_wildlife_plants")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tmp_wildlife_plants (
      species_code VARCHAR(50),
      genus VARCHAR(100),
      scientific_name TEXT,
      life_form VARCHAR(50),
      lepidoptera_species_count INTEGER,
      specialist_bee_species_count INTEGER,
      songbird_value VARCHAR(50),
      bird_food TEXT,
      hummingbird_plant BOOLEAN,
      is_keystone_genus BOOLEAN
    )
  ")
  DBI::dbWriteTable(con, "tmp_wildlife_plants", staging_plants,
                    append = TRUE, temporary = TRUE, row.names = FALSE)

  # Match to ref_taxon on genus+species epithet and UPSERT
  n_plants <- DBI::dbExecute(con, "
    INSERT INTO ref_wildlife_plants (taxon_id, species_code, genus, life_form,
      lepidoptera_species_count, specialist_bee_species_count,
      songbird_value, bird_food, hummingbird_plant, is_keystone_genus, updated_at)
    SELECT DISTINCT ON (tmp.species_code)
      t.id AS taxon_id,
      tmp.species_code, tmp.genus, tmp.life_form,
      tmp.lepidoptera_species_count, tmp.specialist_bee_species_count,
      tmp.songbird_value, tmp.bird_food, tmp.hummingbird_plant, tmp.is_keystone_genus,
      now()
    FROM tmp_wildlife_plants tmp
    LEFT JOIN ref_taxon t
      ON lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2))
       = lower(split_part(tmp.scientific_name, ' ', 1) || ' ' || split_part(tmp.scientific_name, ' ', 2))
    ON CONFLICT (species_code) DO UPDATE SET
      taxon_id = EXCLUDED.taxon_id,
      genus = EXCLUDED.genus,
      life_form = EXCLUDED.life_form,
      lepidoptera_species_count = EXCLUDED.lepidoptera_species_count,
      specialist_bee_species_count = EXCLUDED.specialist_bee_species_count,
      songbird_value = EXCLUDED.songbird_value,
      bird_food = EXCLUDED.bird_food,
      hummingbird_plant = EXCLUDED.hummingbird_plant,
      is_keystone_genus = EXCLUDED.is_keystone_genus,
      updated_at = now()
  ")
  message(sprintf("  Upserted %d plant records", n_plants))

  match_stats <- DBI::dbGetQuery(con, "
    SELECT COUNT(*)::int AS total, COUNT(taxon_id)::int AS matched
    FROM ref_wildlife_plants
  ")
  message(sprintf("  Taxon match rate: %d/%d (%.1f%%)",
                  match_stats$matched[1], match_stats$total[1],
                  100 * match_stats$matched[1] / max(match_stats$total[1], 1)))

  # =========================================================================
  # 2. ref_wildlife_species (pollinator_species.xlsx + bird_species.xlsx)
  # =========================================================================
  # Pollinator cols: Pollinator_ID, Scientific_name, Common_name, Pollinator_type,
  #   Family, Specialist_generalist, Functional_group
  # Bird cols: Bird_ID, Scientific_name, Common_name, Family, Functional_group
  message("\n--- Loading ref_wildlife_species ---")

  # Pollinators → unified schema
  species_staging <- data.frame(
    wildlife_id = poll_sp_df$Pollinator_ID,
    scientific_name = poll_sp_df$Scientific_name,
    common_name = poll_sp_df$Common_name,
    wildlife_type = poll_sp_df$Pollinator_type,
    family = poll_sp_df$Family,
    specialist_generalist = if ("Specialist_generalist" %in% names(poll_sp_df)) poll_sp_df$Specialist_generalist else NA_character_,
    functional_group = if ("Functional_group" %in% names(poll_sp_df)) poll_sp_df$Functional_group else NA_character_,
    stringsAsFactors = FALSE
  )

  # Birds → append
  if (nrow(bird_sp_df) > 0) {
    bird_staging <- data.frame(
      wildlife_id = bird_sp_df$Bird_ID,
      scientific_name = bird_sp_df$Scientific_name,
      common_name = bird_sp_df$Common_name,
      wildlife_type = "Bird",
      family = bird_sp_df$Family,
      specialist_generalist = NA_character_,
      functional_group = if ("Functional_group" %in% names(bird_sp_df)) bird_sp_df$Functional_group else NA_character_,
      stringsAsFactors = FALSE
    )
    species_staging <- rbind(species_staging, bird_staging)
  }

  message(sprintf("  Staging %d total wildlife species (%d pollinators + %d birds)",
                  nrow(species_staging), nrow(poll_sp_df), nrow(bird_sp_df)))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_wildlife_species")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tmp_wildlife_species (
      wildlife_id VARCHAR(50),
      scientific_name TEXT,
      common_name TEXT,
      wildlife_type VARCHAR(30),
      family VARCHAR(100),
      specialist_generalist VARCHAR(30),
      functional_group VARCHAR(50)
    )
  ")
  DBI::dbWriteTable(con, "tmp_wildlife_species", species_staging,
                    append = TRUE, temporary = TRUE, row.names = FALSE)

  n_species <- DBI::dbExecute(con, "
    INSERT INTO ref_wildlife_species (wildlife_id, scientific_name, common_name,
      wildlife_type, family, specialist_generalist, functional_group, updated_at)
    SELECT wildlife_id, scientific_name, common_name,
           wildlife_type, family, specialist_generalist, functional_group, now()
    FROM tmp_wildlife_species
    ON CONFLICT (wildlife_id) DO UPDATE SET
      scientific_name = EXCLUDED.scientific_name,
      common_name = EXCLUDED.common_name,
      wildlife_type = EXCLUDED.wildlife_type,
      family = EXCLUDED.family,
      specialist_generalist = EXCLUDED.specialist_generalist,
      functional_group = EXCLUDED.functional_group,
      updated_at = now()
  ")
  message(sprintf("  Upserted %d wildlife species", n_species))

  # Type breakdown
  type_counts <- DBI::dbGetQuery(con, "
    SELECT wildlife_type, COUNT(*)::int AS n
    FROM ref_wildlife_species GROUP BY wildlife_type ORDER BY n DESC
  ")
  for (i in seq_len(nrow(type_counts))) {
    message(sprintf("    %s: %d", type_counts$wildlife_type[i], type_counts$n[i]))
  }

  # --- Backfill families from lep_species_master.csv ---
  if (nrow(lep_master_df) > 0) {
    message("\n  Backfilling Lepidoptera families from lep_species_master.csv...")
    lep_fam <- data.frame(
      scientific_name = lep_master_df$scientific_name,
      family = lep_master_df$family,
      stringsAsFactors = FALSE
    )
    lep_fam <- lep_fam[!is.na(lep_fam$family) & nzchar(lep_fam$family), ]

    DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_lep_families")
    DBI::dbExecute(con, "CREATE TEMP TABLE tmp_lep_families (scientific_name TEXT, family VARCHAR(100))")
    DBI::dbWriteTable(con, "tmp_lep_families", lep_fam,
                      append = TRUE, temporary = TRUE, row.names = FALSE)

    n_fam_updated <- DBI::dbExecute(con, "
      UPDATE ref_wildlife_species ws
      SET family = lf.family
      FROM tmp_lep_families lf
      WHERE lower(ws.scientific_name) = lower(lf.scientific_name)
        AND ws.wildlife_type IN ('Moth', 'Butterfly', 'Skipper')
        AND (ws.family IS NULL OR ws.family = '')
    ")
    message(sprintf("    Updated %d Lep species with family data", n_fam_updated))
  }

  # --- Derive bee families from genus ---
  message("  Deriving bee families from genus...")
  bee_family_map <- list(
    Andrena = "Andrenidae", Calliopsis = "Andrenidae", Panurginus = "Andrenidae",
    Perdita = "Andrenidae", Pseudopanurgus = "Andrenidae",
    Agapostemon = "Halictidae", Augochlora = "Halictidae", Augochlorella = "Halictidae",
    Augochloropsis = "Halictidae", Dufourea = "Halictidae", Halictus = "Halictidae",
    Lasioglossum = "Halictidae", Sphecodes = "Halictidae",
    Anthidium = "Megachilidae", Chelostoma = "Megachilidae", Coelioxys = "Megachilidae",
    Heriades = "Megachilidae", Hoplitis = "Megachilidae", Megachile = "Megachilidae",
    Osmia = "Megachilidae", Stelis = "Megachilidae",
    Colletes = "Colletidae", Hylaeus = "Colletidae",
    Anthophora = "Apidae", Apis = "Apidae", Bombus = "Apidae", Ceratina = "Apidae",
    Eucera = "Apidae", Habropoda = "Apidae", Melissodes = "Apidae",
    Nomada = "Apidae", Peponapis = "Apidae", Ptilothrix = "Apidae",
    Svastra = "Apidae", Tetraloniella = "Apidae", Triepeolus = "Apidae",
    Xylocopa = "Apidae",
    Macropis = "Melittidae", Melitta = "Melittidae"
  )
  for (genus in names(bee_family_map)) {
    DBI::dbExecute(con, sprintf("
      UPDATE ref_wildlife_species
      SET family = '%s'
      WHERE wildlife_type = 'Bee'
        AND (family IS NULL OR family = '')
        AND lower(split_part(scientific_name, ' ', 1)) = lower('%s')
    ", bee_family_map[[genus]], genus))
  }
  n_bee_fam <- DBI::dbGetQuery(con, "
    SELECT COUNT(*)::int AS n FROM ref_wildlife_species
    WHERE wildlife_type = 'Bee' AND family IS NOT NULL AND family != ''
  ")$n[1]
  message(sprintf("    Bees with family: %d / %d", n_bee_fam,
                  sum(type_counts$n[type_counts$wildlife_type == "Bee"])))

  # Family coverage check
  fam_stats <- DBI::dbGetQuery(con, "
    SELECT wildlife_type,
      COUNT(*)::int AS total,
      COUNT(CASE WHEN family IS NOT NULL AND family != '' THEN 1 END)::int AS has_family
    FROM ref_wildlife_species GROUP BY wildlife_type ORDER BY total DESC
  ")
  message("\n  Family coverage after backfill:")
  for (i in seq_len(nrow(fam_stats))) {
    message(sprintf("    %s: %d/%d (%.0f%%)", fam_stats$wildlife_type[i],
                    fam_stats$has_family[i], fam_stats$total[i],
                    100 * fam_stats$has_family[i] / max(fam_stats$total[i], 1)))
  }

  # =========================================================================
  # 3. ref_wildlife_interactions (pollinator + bird interactions merged)
  # =========================================================================
  # Pollinator cols: Plant_species_code, Pollinator_ID, Interaction_type, Evidence_level, Source
  # Bird cols: Plant_species_code, Bird_ID, Interaction_type, Evidence_level, Source
  message("\n--- Loading ref_wildlife_interactions ---")

  int_staging <- data.frame(
    plant_species_code = poll_int_df$Plant_species_code,
    wildlife_id = poll_int_df$Pollinator_ID,
    interaction_type = poll_int_df$Interaction_type,
    evidence_level = if ("Evidence_level" %in% names(poll_int_df)) poll_int_df$Evidence_level else NA_character_,
    source = if ("Source" %in% names(poll_int_df)) poll_int_df$Source else NA_character_,
    stringsAsFactors = FALSE
  )

  if (nrow(bird_int_df) > 0) {
    bird_int_staging <- data.frame(
      plant_species_code = bird_int_df$Plant_species_code,
      wildlife_id = bird_int_df$Bird_ID,
      interaction_type = bird_int_df$Interaction_type,
      evidence_level = if ("Evidence_level" %in% names(bird_int_df)) bird_int_df$Evidence_level else NA_character_,
      source = if ("Source" %in% names(bird_int_df)) bird_int_df$Source else NA_character_,
      stringsAsFactors = FALSE
    )
    int_staging <- rbind(int_staging, bird_int_staging)
  }

  message(sprintf("  Staging %d interactions (%d pollinator + %d bird)",
                  nrow(int_staging), nrow(poll_int_df), nrow(bird_int_df)))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_wildlife_interactions")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tmp_wildlife_interactions (
      plant_species_code VARCHAR(50),
      wildlife_id VARCHAR(50),
      interaction_type VARCHAR(50),
      evidence_level VARCHAR(30),
      source TEXT
    )
  ")
  DBI::dbWriteTable(con, "tmp_wildlife_interactions", int_staging,
                    append = TRUE, temporary = TRUE, row.names = FALSE)

  n_interactions <- DBI::dbExecute(con, "
    INSERT INTO ref_wildlife_interactions (plant_species_code, wildlife_id,
      interaction_type, evidence_level, source, updated_at)
    SELECT plant_species_code, wildlife_id, interaction_type,
           evidence_level, source, now()
    FROM tmp_wildlife_interactions
    ON CONFLICT (plant_species_code, wildlife_id, interaction_type) DO UPDATE SET
      evidence_level = EXCLUDED.evidence_level,
      source = EXCLUDED.source,
      updated_at = now()
  ")
  message(sprintf("  Upserted %d interactions", n_interactions))

  # =========================================================================
  # 4. ref_specialist_bees_by_genus
  # =========================================================================
  # Cols: Host_genus, Specialist_bee_count, Specialist_bee_species, Notes
  if (nrow(spec_bees_df) > 0) {
    message("\n--- Loading ref_specialist_bees_by_genus ---")

    bees_staging <- data.frame(
      host_genus = spec_bees_df$Host_genus,
      specialist_bee_count = as.integer(spec_bees_df$Specialist_bee_count),
      specialist_bee_species = spec_bees_df$Specialist_bee_species,
      stringsAsFactors = FALSE
    )
    bees_staging$specialist_bee_count[is.na(bees_staging$specialist_bee_count)] <- 0L

    DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_spec_bees")
    DBI::dbExecute(con, "
      CREATE TEMP TABLE tmp_spec_bees (
        host_genus VARCHAR(100),
        specialist_bee_count INTEGER,
        specialist_bee_species TEXT
      )
    ")
    DBI::dbWriteTable(con, "tmp_spec_bees", bees_staging,
                      append = TRUE, temporary = TRUE, row.names = FALSE)

    n_bees <- DBI::dbExecute(con, "
      INSERT INTO ref_specialist_bees_by_genus (host_genus, specialist_bee_count,
        specialist_bee_species, updated_at)
      SELECT host_genus, specialist_bee_count, specialist_bee_species, now()
      FROM tmp_spec_bees
      WHERE host_genus IS NOT NULL AND host_genus != ''
      ON CONFLICT (host_genus) DO UPDATE SET
        specialist_bee_count = EXCLUDED.specialist_bee_count,
        specialist_bee_species = EXCLUDED.specialist_bee_species,
        updated_at = now()
    ")
    message(sprintf("  Upserted %d specialist bee genus records", n_bees))
  }

  # =========================================================================
  # 5. Mid-Atlantic geographic tagging (from lep_species_master.csv)
  # =========================================================================
  # Adds midatlantic_present flag to ref_wildlife_species for Lep dedup
  if (nrow(lep_master_df) > 0) {
    message("\n--- Tagging Mid-Atlantic Lepidoptera ---")

    # Add column if not exists
    tryCatch({
      DBI::dbExecute(con, "ALTER TABLE ref_wildlife_species ADD COLUMN IF NOT EXISTS midatlantic_present BOOLEAN")
    }, error = function(e) message("  Note: ", e$message))

    # Stage lep master
    lep_staging <- data.frame(
      scientific_name = lep_master_df$scientific_name,
      midatlantic_present = as.logical(lep_master_df$midatlantic_present),
      stringsAsFactors = FALSE
    )

    DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_lep_master")
    DBI::dbExecute(con, "
      CREATE TEMP TABLE tmp_lep_master (
        scientific_name TEXT,
        midatlantic_present BOOLEAN
      )
    ")
    DBI::dbWriteTable(con, "tmp_lep_master", lep_staging,
                      append = TRUE, temporary = TRUE, row.names = FALSE)

    n_tagged <- DBI::dbExecute(con, "
      UPDATE ref_wildlife_species ws
      SET midatlantic_present = lm.midatlantic_present
      FROM tmp_lep_master lm
      WHERE lower(ws.scientific_name) = lower(lm.scientific_name)
        AND ws.wildlife_type IN ('Moth', 'Butterfly')
    ")
    message(sprintf("  Tagged %d Lep species with midatlantic_present flag", n_tagged))

    midatl_counts <- DBI::dbGetQuery(con, "
      SELECT midatlantic_present, COUNT(*)::int AS n
      FROM ref_wildlife_species
      WHERE wildlife_type IN ('Moth', 'Butterfly') AND midatlantic_present IS NOT NULL
      GROUP BY midatlantic_present
    ")
    if (nrow(midatl_counts) > 0) {
      for (i in seq_len(nrow(midatl_counts))) {
        message(sprintf("    midatlantic_present=%s: %d",
                        midatl_counts$midatlantic_present[i], midatl_counts$n[i]))
      }
    }
  }

  # =========================================================================
  # Summary
  # =========================================================================
  message("\n=== Wildlife ETL Complete ===")

  counts <- DBI::dbGetQuery(con, "
    SELECT
      (SELECT COUNT(*)::int FROM ref_wildlife_plants) AS plants,
      (SELECT COUNT(*)::int FROM ref_wildlife_species) AS species,
      (SELECT COUNT(*)::int FROM ref_wildlife_interactions) AS interactions,
      (SELECT COUNT(*)::int FROM ref_specialist_bees_by_genus) AS bee_genera,
      (SELECT COUNT(*)::int FROM ref_wildlife_plants WHERE taxon_id IS NOT NULL) AS plants_matched
  ")

  message(sprintf("Plants: %d (%d matched to ref_taxon, %.1f%%)",
                  counts$plants[1], counts$plants_matched[1],
                  100 * counts$plants_matched[1] / max(counts$plants[1], 1)))
  message(sprintf("Wildlife species: %d", counts$species[1]))
  message(sprintf("Interactions: %d", counts$interactions[1]))
  message(sprintf("Specialist bee genera: %d", counts$bee_genera[1]))

  invisible(list(
    plants = counts$plants[1],
    species = counts$species[1],
    interactions = counts$interactions[1],
    bee_genera = counts$bee_genera[1],
    match_rate = counts$plants_matched[1] / max(counts$plants[1], 1)
  ))
}

# ---- CLI Entrypoint ----------------------------------------------------------
if (!interactive() && identical(commandArgs(trailingOnly = TRUE)[1], "run")) {
  wildlife_etl_run()
}
