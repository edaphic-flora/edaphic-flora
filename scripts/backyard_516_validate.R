suppressPackageStartupMessages({
  library(readr); library(readxl); library(dplyr); library(stringr)
})

ref <- read_csv("app/species_accepted.csv", show_col_types = FALSE)
ref_names <- unique(ref$taxon_name)

# Front garage low (sample 516)
plants <- tibble::tribble(
  ~input,
  "Echinacea purpurea",
  "Clethra alnifolia",         # cv 'Ruby Spice'
  "Allium cernuum",
  "Penstemon digitalis",
  "Pycnanthemum tenuifolium",
  "Coreopsis rosea",
  "Fragaria virginica",        # likely Fragaria virginiana (orthographic)
  "Liatris spicata",
  "Schizachyrium scoparium",
  "Glechoma hederacea"
)

fix_candidates <- function(name) {
  cands <- name
  if (name == "Fragaria virginica") cands <- c(cands, "Fragaria virginiana")
  unique(cands)
}

cat("=== WCVP validation ===\n")
results <- lapply(plants$input, function(s) {
  cands <- fix_candidates(s)
  matched <- cands[cands %in% ref_names]
  list(input = s,
       status = if (length(matched)) "OK" else "MISSING",
       match  = if (length(matched)) matched[1] else NA_character_)
})
results_df <- bind_rows(lapply(results, as_tibble))
print(results_df, n = Inf)

cat("\n=== Plant Order DB cross-ref ===\n")
order_db <- read_excel("C:/Users/toddt/OneDrive/Documents/Plant Order Forms/Plant_Order_Database.xlsx",
                       sheet = "Plant Orders")
cat("Order DB cols:", paste(names(order_db), collapse = " | "), "\n\n")

# Loose match: any row where Scientific Name contains the genus + epithet
for (sp in plants$input) {
  hits <- order_db |>
    filter(!is.na(`Scientific Name`)) |>
    filter(str_detect(tolower(`Scientific Name`),
                      tolower(str_replace(sp, " ", " ?"))))
  if (nrow(hits)) {
    cat(sprintf("\n>> %s — %d order(s):\n", sp, nrow(hits)))
    print(hits |> select(`Scientific Name`, `Variety/Cultivar`,
                         Supplier, `Order Date`, Quantity) )
  } else {
    cat(sprintf("\n>> %s — NO order on file\n", sp))
  }
}
