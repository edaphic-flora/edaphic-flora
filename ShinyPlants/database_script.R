library(expowo)
library(dplyr)

create_species_database <- function() {
  # Get all plant families
  all_families <- lookup_powo()
  
  # Initialize empty dataframe for species
  species_list <- data.frame(
    species_name = character(),
    family = character(),
    author = character(),
    powo_id = character(),
    stringsAsFactors = FALSE
  )
  
  # Process each family
  for(i in seq_along(all_families$family)) {
    tryCatch({
      family_code <- all_families$powoid[i]
      species_result <- powoSpecies(powoid = family_code)
      
      if(!is.null(species_result)) {
        new_species <- data.frame(
          species_name = species_result$scientific_name,
          family = all_families$family[i],
          author = species_result$author,
          powo_id = species_result$powoid,
          stringsAsFactors = FALSE
        )
        species_list <- rbind(species_list, new_species)
      }
      
      # Add a small delay to avoid overwhelming the API
      Sys.sleep(0.5)
      
      # Print progress
      if(i %% 10 == 0) {
        print(paste("Processed", i, "families out of", nrow(all_families)))
      }
      
    }, error = function(e) {
      print(paste("Error processing family:", all_families$family[i]))
      print(e$message)
    })
  }
  
  # Clean up the data
  species_list <- species_list %>%
    filter(!is.na(species_name)) %>%
    distinct(species_name, .keep_all = TRUE) %>%
    arrange(species_name)
  
  # Save to CSV
  write.csv(species_list, "www/species_database.csv", row.names = FALSE)
  
  return(species_list)
}

# Run this once to create the database
species_db <- create_species_database()