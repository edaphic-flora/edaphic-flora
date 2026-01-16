# Generate zipcode lookup database
# Uses the free zipcodeR package to create a lightweight CSV

if (!requireNamespace("zipcodeR", quietly = TRUE)) {
  message("Installing zipcodeR package...")
  install.packages("zipcodeR", repos = "https://cloud.r-project.org")
}

library(zipcodeR)
library(dplyr)

message("Loading zipcode data...")
data("zip_code_db")

# Select only the columns we need
zipcode_lookup <- zip_code_db %>%
  select(zipcode, major_city, state, lat, lng) %>%
  rename(city = major_city, latitude = lat, longitude = lng) %>%
  filter(!is.na(city), !is.na(state)) %>%
  distinct(zipcode, .keep_all = TRUE)

message(sprintf("Loaded %d zipcodes", nrow(zipcode_lookup)))

# Save as CSV
output_path <- "data/zipcode_lookup.csv"
if (!dir.exists("data")) dir.create("data")
write.csv(zipcode_lookup, output_path, row.names = FALSE)

file_size <- file.size(output_path)
message(sprintf("Saved to %s (%.2f MB)", output_path, file_size / 1024 / 1024))

# Also save as compressed RDS for faster loading
rds_path <- "data/zipcode_lookup.rds"
saveRDS(zipcode_lookup, rds_path, compress = "xz")
rds_size <- file.size(rds_path)
message(sprintf("Also saved to %s (%.2f MB)", rds_path, rds_size / 1024 / 1024))

message("\nDone! Zipcode lookup database is ready.")
