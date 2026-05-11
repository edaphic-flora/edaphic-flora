suppressPackageStartupMessages({
  library(readr); library(readxl); library(dplyr); library(stringr)
})

ref <- read_csv("app/species_accepted.csv", show_col_types = FALSE)
ref_names <- unique(ref$taxon_name)
order_db <- read_excel("C:/Users/toddt/OneDrive/Documents/Plant Order Forms/Plant_Order_Database.xlsx",
                       sheet = "Plant Orders")

plants <- tibble::tribble(
  ~input,                                ~setting,         ~outcome,
  "Hibiscus moscheutos",                 "rain garden",    "Thriving",
  "Vernonia noveboracensis",             "rain garden",    "Thriving",
  "Eupatorium (need species)",           "rain garden",    "Thriving",
  "Cephalanthus occidentalis",           "rain garden",    "Thriving",   # buttonbush
  "Gentiana andrewsii",                  "rain garden",    "Thriving",
  "Asclepias incarnata",                 "rain garden",    "Thriving",
  "Carex muskimigiensis",                "rain garden",    "Established",  # cv 'Little Midge'; check spelling
  "Chelone glabra",                      "rain garden",    "Struggling",
  "Solidago speciosa",                   "outside RG",     "Thriving",
  "Symphytrichium novae-angliae",        "outside RG",     "Thriving"      # spelling needs check
)

fix_candidates <- function(name) {
  cands <- name
  if (name == "Carex muskimigiensis")        cands <- c(cands, "Carex muskingumensis")
  if (name == "Symphytrichium novae-angliae") cands <- c(cands, "Symphyotrichum novae-angliae")
  if (name == "Eupatorium (need species)")   cands <- character(0)
  unique(cands)
}

cat("=== WCVP validation ===\n")
results <- lapply(plants$input, function(s) {
  cands <- fix_candidates(s)
  matched <- cands[cands %in% ref_names]
  list(input = s,
       status = if (length(matched)) "OK" else (if (length(cands)==0) "MANUAL" else "MISSING"),
       match  = if (length(matched)) matched[1] else NA_character_)
})
print(bind_rows(lapply(results, as_tibble)), n = Inf)

cat("\n=== Eupatorium options in your order DB ===\n")
print(order_db |> filter(str_detect(`Scientific Name`, "^Eupatorium")) |>
        select(`Scientific Name`, `Variety/Cultivar`, Supplier, `Order Date`, Quantity))

cat("\n=== Plant Order DB cross-ref (other species) ===\n")
to_check <- c("Hibiscus moscheutos", "Vernonia noveboracensis",
              "Cephalanthus occidentalis", "Gentiana andrewsii",
              "Asclepias incarnata", "Carex muskingumensis",
              "Chelone glabra", "Solidago speciosa", "Symphyotrichum novae-angliae")
for (sp in to_check) {
  pat <- str_replace(sp, " ", " ?")
  hits <- order_db |> filter(!is.na(`Scientific Name`)) |>
          filter(str_detect(tolower(`Scientific Name`), tolower(pat)))
  if (nrow(hits)) {
    cat(sprintf("\n>> %s — %d order(s):\n", sp, nrow(hits)))
    print(hits |> select(`Scientific Name`, `Variety/Cultivar`,
                         Supplier, `Order Date`, Quantity))
  } else {
    cat(sprintf(">> %s — NO order on file\n", sp))
  }
}
