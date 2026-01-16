# generate_species_db.R
# Run this script ONCE to create a filtered species database from wcvp_names.csv
# This reduces the 295MB file to a much smaller file containing only accepted species

library(dplyr)

message("Reading wcvp_names.csv (this may take a moment)...")

species_db <- read.csv("wcvp_names.csv", sep="|", quote="",
                       col.names=c("plant_name_id","ipni_id","taxon_rank","taxon_status",
                                   "family","genus_hybrid","genus","species_hybrid","species",
                                   "infraspecific_rank","infraspecies","parenthetical_author",
                                   "primary_author","publication_author","place_of_publication",
                                   "volume_and_page","first_published","nomenclatural_remarks",
                                   "geographic_area","lifeform_description","climate_description",
                                   "taxon_name","taxon_authors","accepted_plant_name_id",
                                   "basionym_plant_name_id","replaced_synonym_author",
                                   "homotypic_synonym","parent_plant_name_id","powo_id",
                                   "hybrid_formula","reviewed"),
                       na.strings=c("", "NA"), stringsAsFactors=FALSE)

message(sprintf("Loaded %d rows", nrow(species_db)))

# Filter to accepted species only and select needed columns
filtered <- species_db %>%
  filter(taxon_rank == "Species", taxon_status == "Accepted") %>%
  select(taxon_name, family, genus, species) %>%
  distinct() %>%
  arrange(taxon_name)

message(sprintf("Filtered to %d accepted species", nrow(filtered)))

# Save to new file
write.csv(filtered, "species_accepted.csv", row.names = FALSE)

message("Saved to species_accepted.csv")
message(sprintf("Original file: %.1f MB", file.size("wcvp_names.csv") / 1024^2))
message(sprintf("New file: %.1f MB", file.size("species_accepted.csv") / 1024^2))
