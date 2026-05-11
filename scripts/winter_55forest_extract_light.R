suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr)
})

f <- "C:/Users/toddt/OneDrive/Desktop/edaphic consulting/01_Clients/Winter_Eric_Wyoming_2026/Winter_Garden_Inventory.xlsx"
df <- read_excel(f, sheet = "Garden Inventory", col_names = FALSE)

# Column 1 = scientific name OR zone header; columns 9 (per ...9 indexing) = Light
# In a no-col-names read, col index 1 and 9
sci   <- df[[1]]
light <- df[[9]]
common <- df[[2]]

# A "zone header" row is one where ...2 is NA but ...1 has text.
is_zone <- !is.na(sci) & is.na(common) & !is.na(sci) & sci != ""
# Skip the first 4 header-ish rows (title, blank, "Scientific Name", first zone)

zone <- character(length(sci))
current <- NA_character_
for (i in seq_along(sci)) {
  if (!is.na(sci[i]) && is.na(common[i])) {
    # zone header
    current <- sci[i]
  }
  zone[i] <- current
}

plants <- tibble(
  zone = zone,
  raw_name = sci,
  common = common,
  light = light
) |>
  filter(!is.na(common))   # drop zone headers + blanks

target_zones <- c("Front Island Garden", "Living-Room Garden", "Shade Garden (Rear)")
out <- plants |> filter(zone %in% target_zones) |>
  select(zone, raw_name, light)
cat("=== Per-zone plant counts ===\n")
print(out |> count(zone))
cat("\n=== Distinct Light values across these zones ===\n")
print(out |> distinct(light) |> arrange(light))
cat("\n=== Full listing ===\n")
options(width = 200)
print(out, n = Inf, width = 200)
write.csv(out, "data/winter_55forest_inventory_light.csv", row.names = FALSE)
cat("\nWrote: data/winter_55forest_inventory_light.csv\n")
