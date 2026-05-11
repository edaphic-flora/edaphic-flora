suppressPackageStartupMessages({
  library(readxl); library(openxlsx); library(dplyr)
})
f <- "C:/Users/toddt/OneDrive/Documents/Plant Order Forms/Plant_Order_Database.xlsx"
existing <- read_excel(f, sheet = "Plant Orders")
cat("Before:", nrow(existing), "\n")

new_rows <- tibble::tribble(
  ~Supplier,                   ~`Order #`, ~`Order Date`, ~`Common Name`,                        ~`Scientific Name`,             ~`Variety/Cultivar`, ~`Form/Type`, ~Quantity, ~`Unit Price`, ~Subtotal, ~`Source Image`,
  "Native Plant Trust",        NA,         NA,            "Black Chokeberry",                    "Aronia melanocarpa",           NA,                  "plant",      NA_real_,  NA_real_,      NA_real_,  NA,
  "Native Plant Trust",        NA,         NA,            "Red Chokeberry",                      "Aronia arbutifolia",           NA,                  "plant",      NA_real_,  NA_real_,      NA_real_,  NA,
  "Unknown (Order Invoice)",   NA,         NA,            "Wild Bergamot",                       "Monarda fistulosa",            "Claire Grace",      "plant",      NA_real_,  NA_real_,      NA_real_,  NA,
  "Unknown (Order Invoice)",   NA,         NA,            "Staghorn Sumac",                      "Rhus typhina",                 "Tiger Eyes",        "plant",      NA_real_,  NA_real_,      NA_real_,  NA,
  "Unknown (Order Invoice)",   NA,         NA,            "Aromatic Aster",                      "Symphyotrichum oblongifolium", "Raydon's Favorite", "plant",      NA_real_,  NA_real_,      NA_real_,  NA,
  "Unknown (Order Invoice)",   NA,         NA,            "Eastern Bluestar",                    "Amsonia tabernaemontana",      NA,                  "plant",      NA_real_,  NA_real_,      NA_real_,  NA
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
