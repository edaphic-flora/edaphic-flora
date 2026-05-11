# Append Earth Tones order (sample 516 plants) to the user's Plant Order Database.
suppressPackageStartupMessages({
  library(readxl); library(openxlsx); library(dplyr)
})

f <- "C:/Users/toddt/OneDrive/Documents/Plant Order Forms/Plant_Order_Database.xlsx"
existing <- read_excel(f, sheet = "Plant Orders")
cat("Existing rows:", nrow(existing), "\n")

new_rows <- tibble::tribble(
  ~Supplier,                     ~`Order #`, ~`Order Date`, ~`Common Name`,             ~`Scientific Name`,        ~`Variety/Cultivar`, ~`Form/Type`, ~Quantity, ~`Unit Price`, ~Subtotal, ~`Source Image`,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Purple Coneflower",        "Echinacea purpurea",      NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Summersweet",              "Clethra alnifolia",       "Ruby Spice",         "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Nodding Onion",            "Allium cernuum",          NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Foxglove Beardtongue",     "Penstemon digitalis",     NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Narrowleaf Mountain Mint", "Pycnanthemum tenuifolium",NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Pink Tickseed",            "Coreopsis rosea",         NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Virginia Strawberry",      "Fragaria virginiana",     NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Dense Blazing Star",       "Liatris spicata",         NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Little Bluestem",          "Schizachyrium scoparium", NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA,
  "Earth Tones Native Plants",   NA,         "2023-07-11",  "Ground Ivy",               "Glechoma hederacea",      NA,                   "plug",       NA_real_,  NA_real_,      NA_real_,  NA
)

stopifnot(identical(names(new_rows), names(existing)))

combined <- bind_rows(existing, new_rows)
cat("New total:", nrow(combined), "  (added", nrow(new_rows), ")\n")

# Write back, preserving the workbook
wb <- openxlsx::loadWorkbook(f)
openxlsx::writeData(wb, "Plant Orders", combined)
openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
cat("Saved.\n")
