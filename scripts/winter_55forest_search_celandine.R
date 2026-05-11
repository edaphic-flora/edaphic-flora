suppressPackageStartupMessages({ library(readr); library(dplyr) })
ref <- read_csv("app/species_accepted.csv", show_col_types = FALSE)
print(ref |> filter(grepl("Ficaria|ficaria", taxon_name)) |> select(taxon_name))
