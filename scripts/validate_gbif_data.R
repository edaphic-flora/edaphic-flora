# scripts/validate_gbif_data.R — Validate GBIF wildlife state presence data
# Usage: Rscript scripts/validate_gbif_data.R

if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists("app/.Renviron")) readRenviron("app/.Renviron")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(DBI)
  library(RPostgres)
})

df <- read_csv("data/gbif_wildlife_state_presence.csv", show_col_types = FALSE)

# ============================================================
# 1. Overall stats
# ============================================================
cat("=== GBIF Wildlife State Presence Validation ===\n")
cat(sprintf("Total records: %d\n", nrow(df)))
cat(sprintf("Unique species: %d\n", n_distinct(df$scientific_name)))
cat(sprintf("Unique states: %d\n", n_distinct(df$state_code)))
cat(sprintf("Source values: %s\n", paste(unique(df$source), collapse = ", ")))

# ============================================================
# 2. Per-state counts
# ============================================================
cat("\n--- Records per state ---\n")
state_counts <- df %>% count(state_code) %>% arrange(desc(n))
for (i in seq_len(nrow(state_counts))) {
  cat(sprintf("  %s: %d\n", state_counts$state_code[i], state_counts$n[i]))
}

# ============================================================
# 3. Observation count stats
# ============================================================
cat("\n--- Observation count stats ---\n")
cat(sprintf("  Min: %d\n", min(df$observation_count, na.rm = TRUE)))
cat(sprintf("  Median: %d\n", median(df$observation_count, na.rm = TRUE)))
cat(sprintf("  Mean: %.0f\n", mean(df$observation_count, na.rm = TRUE)))
cat(sprintf("  Max: %d\n", max(df$observation_count, na.rm = TRUE)))
cat(sprintf("  NAs: %d\n", sum(is.na(df$observation_count))))

# ============================================================
# 4. Name quality
# ============================================================
cat("\n--- Species name quality ---\n")
word_count <- str_count(df$scientific_name, "\\S+")
cat(sprintf("  1-word names (genus only): %d records (%d species)\n",
    sum(word_count == 1), n_distinct(df$scientific_name[word_count == 1])))
cat(sprintf("  2-word names (expected): %d records (%d species)\n",
    sum(word_count == 2), n_distinct(df$scientific_name[word_count == 2])))
cat(sprintf("  3+ word names: %d records (%d species)\n",
    sum(word_count >= 3), n_distinct(df$scientific_name[word_count >= 3])))

one_word <- unique(df$scientific_name[word_count == 1])
if (length(one_word) > 0) {
  cat("\n  Sample 1-word names:\n")
  for (n in head(sort(one_word), 20)) cat("    ", n, "\n")
}

bad_pattern <- df %>%
  filter(!grepl("^[A-Z][a-z]+ [a-z]+$", scientific_name)) %>%
  distinct(scientific_name)
cat(sprintf("\n  Names NOT matching 'Genus species' pattern: %d / %d\n",
    nrow(bad_pattern), n_distinct(df$scientific_name)))
if (nrow(bad_pattern) > 0) {
  cat("  Sample non-standard:\n")
  for (n in head(sort(bad_pattern$scientific_name), 15)) cat("    ", n, "\n")
}

# ============================================================
# 5. Match rate against ref_wildlife_species
# ============================================================
cat("\n--- Match rate against ref_wildlife_species ---\n")
con <- tryCatch({
  dbConnect(Postgres(),
    host = Sys.getenv("POSTGRES_HOST"),
    port = as.integer(Sys.getenv("POSTGRES_PORT")),
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD"),
    sslmode = Sys.getenv("POSTGRES_SSLMODE", "require")
  )
}, error = function(e) {
  cat("  Could not connect to DB: ", e$message, "\n")
  NULL
})

if (!is.null(con)) {
  ws <- dbGetQuery(con, "
    SELECT wildlife_id, scientific_name, wildlife_type,
      lower(split_part(scientific_name, ' ', 1) || ' ' || split_part(scientific_name, ' ', 2)) AS match_key
    FROM ref_wildlife_species
  ")
  cat(sprintf("  DB wildlife species: %d\n", nrow(ws)))

  gbif_species <- unique(tolower(df$scientific_name))
  db_keys <- unique(ws$match_key)

  matched <- intersect(gbif_species, db_keys)
  cat(sprintf("  GBIF unique species: %d\n", length(gbif_species)))
  cat(sprintf("  Matched to ref_wildlife_species: %d (%.1f%%)\n",
      length(matched), 100 * length(matched) / length(gbif_species)))

  # Match rate per wildlife_type
  cat("\n  Per wildlife_type:\n")
  for (wt in sort(unique(ws$wildlife_type))) {
    type_keys <- unique(ws$match_key[ws$wildlife_type == wt])
    type_matched <- intersect(gbif_species, type_keys)
    type_coverage <- 100 * length(type_matched) / max(length(type_keys), 1)
    cat(sprintf("    %s: %d/%d matched (%.1f%% coverage of DB)\n",
        wt, length(type_matched), length(type_keys), type_coverage))
  }

  # DB Lep+Bee species NOT in GBIF
  db_lep_bee <- ws %>% filter(wildlife_type %in% c("Moth", "Butterfly", "Skipper", "Bee"))
  missing_from_gbif <- setdiff(unique(db_lep_bee$match_key), gbif_species)
  cat(sprintf("\n  DB Lep+Bee species NOT in GBIF: %d / %d (%.1f%%)\n",
      length(missing_from_gbif), n_distinct(db_lep_bee$match_key),
      100 * length(missing_from_gbif) / max(n_distinct(db_lep_bee$match_key), 1)))

  if (length(missing_from_gbif) > 0) {
    cat("  Sample missing:\n")
    for (s in head(sort(missing_from_gbif), 20)) {
      wtype <- ws$wildlife_type[ws$match_key == s][1]
      cat(sprintf("    %s (%s)\n", s, wtype))
    }
  }

  # Genus-level match (fallback matching)
  gbif_genera <- unique(str_extract(gbif_species, "^\\S+"))
  db_genera <- unique(str_extract(db_keys, "^\\S+"))
  genus_matched <- intersect(gbif_genera, db_genera)
  cat(sprintf("\n  Genus-level overlap: %d/%d DB genera found in GBIF (%.1f%%)\n",
      length(genus_matched), length(db_genera),
      100 * length(genus_matched) / max(length(db_genera), 1)))

  dbDisconnect(con)
}

cat("\n=== Validation complete ===\n")
