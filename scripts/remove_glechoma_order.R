suppressPackageStartupMessages({
  library(readxl); library(openxlsx); library(dplyr)
})
f <- "C:/Users/toddt/OneDrive/Documents/Plant Order Forms/Plant_Order_Database.xlsx"
existing <- read_excel(f, sheet = "Plant Orders")
cat("Before:", nrow(existing), "\n")

# Drop the Glechoma row I just added (Earth Tones, 2023-07-11)
mask <- existing$`Scientific Name` == "Glechoma hederacea" &
        existing$Supplier == "Earth Tones Native Plants" &
        existing$`Order Date` == "2023-07-11"
cat("Rows to drop:", sum(mask, na.rm = TRUE), "\n")
trimmed <- existing[!mask | is.na(mask), ]
cat("After:", nrow(trimmed), "\n")

wb <- openxlsx::loadWorkbook(f)
openxlsx::removeWorksheet(wb, "Plant Orders")
openxlsx::addWorksheet(wb, "Plant Orders")
openxlsx::writeData(wb, "Plant Orders", trimmed)
openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
cat("Saved.\n")
