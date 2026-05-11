# scripts/find_nonnative_wildlife.R — Identify non-native wildlife species in the DB

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
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
)

# Known introduced/invasive wildlife species in North America
# These should NOT be in a "support native wildlife" dashboard
KNOWN_INTRODUCED <- c(
  # Birds
  "Sturnus vulgaris",       # European Starling
  "Passer domesticus",      # House Sparrow
  "Columba livia",          # Rock Pigeon (feral)
  "Haemorhous mexicanus",   # House Finch (native to west, introduced to east — borderline)

  # Bees
  "Apis mellifera",         # European Honeybee

  # Lepidoptera
  "Pieris rapae",           # Cabbage White (introduced from Europe)
  "Lymantria dispar",       # Spongy Moth (invasive)
  "Cydalima perspectalis",  # Box Tree Moth (invasive)
  "Plutella xylostella",    # Diamondback Moth (cosmopolitan pest)
  "Ostrinia nubilalis",     # European Corn Borer
  "Tineola bisselliella",   # Webbing Clothes Moth
  "Tinea pellionella",      # Casemaking Clothes Moth
  "Galleria mellonella",    # Greater Wax Moth
  "Plodia interpunctella",  # Indian Meal Moth
  "Ephestia kuehniella",    # Mediterranean Flour Moth
  "Operophtera brumata"     # Winter Moth (invasive in NE US)
)

cat("=== Non-Native Wildlife Species Check ===\n\n")

# Check which of these are in our DB
for (sp in KNOWN_INTRODUCED) {
  result <- dbGetQuery(con, sprintf("
    SELECT wildlife_id, scientific_name, common_name, wildlife_type, family
    FROM ref_wildlife_species
    WHERE lower(split_part(scientific_name, ' ', 1) || ' ' || split_part(scientific_name, ' ', 2))
        = lower('%s')
  ", gsub("'", "''", sp)))

  if (nrow(result) > 0) {
    for (i in seq_len(nrow(result))) {
      # Count interactions
      n_int <- dbGetQuery(con, sprintf(
        "SELECT COUNT(*)::int AS n FROM ref_wildlife_interactions WHERE wildlife_id = '%s'",
        result$wildlife_id[i]))$n[1]
      cat(sprintf("  FOUND: %s (%s) — %s, %s, %d interactions\n",
          result$scientific_name[i], result$common_name[i],
          result$wildlife_type[i], result$family[i], n_int))
    }
  }
}

# Also scan for any species with obviously non-native common names
cat("\n--- Species with 'European', 'Asian', 'Japanese', 'Chinese' in common name ---\n")
suspect <- dbGetQuery(con, "
  SELECT wildlife_id, scientific_name, common_name, wildlife_type
  FROM ref_wildlife_species
  WHERE common_name ~* '(european|asian|japanese|chinese|oriental|african|indian|australian)'
  ORDER BY wildlife_type, common_name
")
if (nrow(suspect) > 0) {
  for (i in seq_len(nrow(suspect))) {
    cat(sprintf("  %s — %s (%s)\n",
        suspect$common_name[i], suspect$scientific_name[i], suspect$wildlife_type[i]))
  }
} else {
  cat("  None found\n")
}

dbDisconnect(con)
cat("\n=== Done ===\n")
