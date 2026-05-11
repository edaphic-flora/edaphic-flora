suppressPackageStartupMessages({
  library(readxl); library(openxlsx); library(readr); library(dplyr)
})

# Verify Eutrochium maculatum is in WCVP
ref <- read_csv("app/species_accepted.csv", show_col_types = FALSE)
stopifnot("Eutrochium maculatum" %in% ref$taxon_name)
cat("Eutrochium maculatum: WCVP OK\n")

f <- "C:/Users/toddt/OneDrive/Documents/Plant Order Forms/Plant_Order_Database.xlsx"
existing <- read_excel(f, sheet = "Plant Orders")
cat("Before:", nrow(existing), "\n")

new_rows <- tibble::tribble(
  ~Supplier,                     ~`Order #`, ~`Order Date`, ~`Common Name`,        ~`Scientific Name`,            ~`Variety/Cultivar`, ~`Form/Type`, ~Quantity, ~`Unit Price`, ~Subtotal, ~`Source Image`,
  "Earth Tones Native Plants",   NA,         "2023-08-25",  "Spotted Joe Pye Weed", "Eutrochium maculatum",       NA,                  "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-08-25",  "Bottle Gentian",       "Gentiana andrewsii",         NA,                  "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-08-25",  "New England Aster",    "Symphyotrichum novae-angliae",NA,                 "plug",       NA_real_,  NA_real_,      NA_real_,  NA
)
stopifnot(identical(names(new_rows), names(existing)))

combined <- bind_rows(existing, new_rows)
cat("After:", nrow(combined), "  (added", nrow(new_rows), ")\n")

wb <- openxlsx::loadWorkbook(f)
openxlsx::removeWorksheet(wb, "Plant Orders")
openxlsx::addWorksheet(wb, "Plant Orders")
openxlsx::writeData(wb, "Plant Orders", combined)
openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
cat("Saved.\n")
