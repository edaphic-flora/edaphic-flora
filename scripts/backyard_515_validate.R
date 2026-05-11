suppressPackageStartupMessages({
  library(readr); library(readxl); library(dplyr); library(stringr)
})

ref <- read_csv("app/species_accepted.csv", show_col_types = FALSE)
ref_names <- unique(ref$taxon_name)
order_db <- read_excel("C:/Users/toddt/OneDrive/Documents/Plant Order Forms/Plant_Order_Database.xlsx",
                       sheet = "Plant Orders")

plants <- tibble::tribble(
  ~input,                                       ~cultivar,                ~sun,        ~outcome,
  "Aronia melanocarpa",                         NA,                       "Full Sun",  "Thriving",
  "Aronia arbutifolia",                         NA,                       "Full Sun",  "Thriving",
  "Amelanchier laevis",                         NA,                       "Full Sun",  "Thriving",
  "Solidago rugosa",                            "Fireworks",              "Full Sun",  "Thriving",
  "Chionothus virginicus",                      NA,                       "Full Sun",  "Thriving",   # typo: Chionanthus
  "Monarda fistulosa",                          "Claire Grace",           "Full Sun",  "Thriving",
  "Rhus Typhina",                               "Tiger Eyes",             "Full Sun",  "Thriving",   # case
  "Antennaria neglecta",                        NA,                       "Full Sun",  "Thriving",
  "Symphyotrichium oblongifolium",              "Raydon's Favorite",      "Full Sun",  "Thriving",   # typo: Symphyotrichum
  "Ratibita pinnata",                           NA,                       "Full Sun",  "Thriving",   # typo: Ratibida
  "Veronicastrum (need epithet)",               NA,                       "Full Sun",  "Thriving",
  "Amsonia tabernaemontana",                    NA,                       "Full Sun",  "Thriving",
  "Cercis canadensis",                          NA,                       "Full Sun",  "Failed/Died",
  "Rudbeckia deamii",                           NA,                       "Part Sun",  "Thriving",   # may be R. fulgida var. deamii
  "Echinacea pallida",                          NA,                       "Part Sun",  "Established"
)

fix_candidates <- function(name) {
  cands <- name
  if (name == "Chionothus virginicus")          cands <- c(cands, "Chionanthus virginicus")
  if (name == "Rhus Typhina")                   cands <- c(cands, "Rhus typhina")
  if (name == "Symphyotrichium oblongifolium")  cands <- c(cands, "Symphyotrichum oblongifolium")
  if (name == "Ratibita pinnata")               cands <- c(cands, "Ratibida pinnata")
  if (name == "Rudbeckia deamii")               cands <- c(cands, "Rudbeckia fulgida")
  if (name == "Veronicastrum (need epithet)")   cands <- character(0)
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

cat("\n=== Veronicastrum options in your order DB ===\n")
print(order_db |> filter(str_detect(`Scientific Name`, "^Veronicastrum")) |>
        select(`Scientific Name`, `Variety/Cultivar`, Supplier, `Order Date`, Quantity))

cat("\n=== Plant Order DB cross-ref (other species) ===\n")
to_check <- c("Aronia melanocarpa","Aronia arbutifolia","Amelanchier laevis",
              "Solidago rugosa","Chionanthus virginicus","Monarda fistulosa",
              "Rhus typhina","Antennaria neglecta","Symphyotrichum oblongifolium",
              "Ratibida pinnata","Amsonia tabernaemontana","Cercis canadensis",
              "Rudbeckia fulgida","Rudbeckia deamii","Echinacea pallida")
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
