# scripts/remove_nonnative_wildlife.R — Remove introduced/invasive wildlife from DB
# These species should not appear in a "support native wildlife" dashboard

if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

con <- dbConnect(Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_ADMIN_USER", Sys.getenv("POSTGRES_USER")),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD", Sys.getenv("POSTGRES_PASSWORD")),
  sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
)

# Species to remove (all confirmed non-native to North America)
REMOVE_SPECIES <- c(
  # Birds
  "Sturnus vulgaris",            # European Starling

  # Bees
  "Apis mellifera",              # European Honeybee

  # Lepidoptera — invasive/introduced
  "Lymantria dispar",            # Spongy Moth
  "Ostrinia nubilalis",          # European Corn Borer
  "Plodia interpunctella",       # Indian Meal Moth
  "Operophtera brumata",         # Winter Moth
  "Thymelicus lineola",          # European Skipper
  "Archips fuscocupreanus",      # Asian Leafroller
  "Coleophora limosipennella",   # European Elm-Casebearer
  "Ypsolopha dentella",          # European Honeysuckle Leaf Roller
  "Epinotia nanana",             # European Spruce Needle Miner
  "Grapholita molesta",          # Oriental Fruit Moth
  "Cnidocampa flavescens",       # Oriental Moth
  "Rhyacionia buoliana",         # European Pine Shoot Moth
  "Archips rosana"               # Rose Tortrix (European Leafroller)
)

# NOT removing: Hesperia sassacus (Indian Skipper) — native to eastern US despite name

cat("=== Removing Non-Native Wildlife Species ===\n\n")

total_interactions <- 0
total_species <- 0
total_presence <- 0

for (sp in REMOVE_SPECIES) {
  # Find wildlife_id(s)
  rows <- dbGetQuery(con, sprintf("
    SELECT wildlife_id, scientific_name, common_name, wildlife_type
    FROM ref_wildlife_species
    WHERE lower(split_part(scientific_name, ' ', 1) || ' ' || split_part(scientific_name, ' ', 2))
        = lower('%s')
  ", gsub("'", "''", sp)))

  if (nrow(rows) == 0) next

  for (i in seq_len(nrow(rows))) {
    wid <- rows$wildlife_id[i]

    # Remove interactions
    n_int <- dbExecute(con, sprintf(
      "DELETE FROM ref_wildlife_interactions WHERE wildlife_id = '%s'",
      gsub("'", "''", wid)))

    # Remove state presence records (table may not exist yet)
    n_pres <- tryCatch(
      dbExecute(con, sprintf(
        "DELETE FROM ref_wildlife_state_presence WHERE wildlife_id = '%s'",
        gsub("'", "''", wid))),
      error = function(e) 0L
    )

    # Remove the species
    n_sp <- dbExecute(con, sprintf(
      "DELETE FROM ref_wildlife_species WHERE wildlife_id = '%s'",
      gsub("'", "''", wid)))

    cat(sprintf("  Removed: %s (%s) — %d interactions, %d presence records\n",
        rows$scientific_name[i], rows$common_name[i], n_int, n_pres))

    total_interactions <- total_interactions + n_int
    total_species <- total_species + n_sp
    total_presence <- total_presence + n_pres
  }
}

cat(sprintf("\n=== Done: removed %d species, %d interactions, %d presence records ===\n",
    total_species, total_interactions, total_presence))

dbDisconnect(con)
