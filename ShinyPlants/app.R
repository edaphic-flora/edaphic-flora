library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(ggtern)
library(tidygeocoder)
library(sf)
library(ecoregions)
library(sp)
library(maps)
library(mapdata)
library(DBI)
library(RPostgres)
library(pool)

#Force reading of .Renviron file
readRenviron("C:/Users/toddt/OneDrive/Desktop/ShinyPlants/.Renviron")

# Database connection pool
pool <- dbPool(
  drv = Postgres(),
  dbname = Sys.getenv("POSTGRES_DB"),
  host = Sys.getenv("POSTGRES_HOST"),
  port = Sys.getenv("POSTGRES_PORT"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  minSize = 1,
  maxSize = 25
)

# Ensure pool is closed when app stops
onStop(function() {
  poolClose(pool)
})

# Initialize database tables
tryCatch({
  # Create soil_samples table
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS soil_samples (
      id SERIAL PRIMARY KEY,
      species VARCHAR(255),
      cultivar VARCHAR(255),
      ph NUMERIC(4,2),
      organic_matter NUMERIC(5,2),
      nitrate_ppm NUMERIC,
      ammonium_ppm NUMERIC,
      phosphorus_ppm NUMERIC,
      potassium_ppm NUMERIC,
      calcium_ppm NUMERIC,
      magnesium_ppm NUMERIC,
      soluble_salts_ppm NUMERIC,
      texture_sand NUMERIC(5,2),
      texture_silt NUMERIC(5,2),
      texture_clay NUMERIC(5,2),
      texture_class VARCHAR(50),
      location_lat NUMERIC(10,6),
      location_long NUMERIC(10,6),
      date DATE,
      ecoregion_l4 VARCHAR(255),
      ecoregion_l4_code VARCHAR(50),
      notes TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")
  
  # Create sample_files table
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS sample_files (
      id SERIAL PRIMARY KEY,
      sample_id INTEGER REFERENCES soil_samples(id),
      filename VARCHAR(255),
      file_type VARCHAR(50),
      file_size INTEGER,
      storage_path VARCHAR(255),
      upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")
  
  # Create indices
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_species ON soil_samples(species)")
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_date ON soil_samples(date)")
  
}, error = function(e) {
  message("Error creating tables: ", e$message)
})

# Database helper functions
db_get_all_samples <- function() {
  tryCatch({
    dbGetQuery(pool, "SELECT * FROM soil_samples ORDER BY created_at DESC")
  }, error = function(e) {
    message("Error fetching samples: ", e$message)
    data.frame()
  })
}

soil_data <- data.frame(
  species = character(),
  cultivar = character(),
  ph = numeric(),
  organic_matter = numeric(),
  nitrate_ppm = numeric(),
  ammonium_ppm = numeric(),
  phosphorus_ppm = numeric(),
  potassium_ppm = numeric(),
  calcium_ppm = numeric(),
  magnesium_ppm = numeric(),
  soluble_salts_ppm = numeric(),
  texture_sand = numeric(),
  texture_silt = numeric(),
  texture_clay = numeric(),
  texture_class = character(),
  location_lat = numeric(),
  location_long = numeric(),
  date = as.Date(character()),
  ecoregion_l4 = character(),
  ecoregion_l4_code = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

db_get_species_data <- function(species) {
  if (is.null(species) || nchar(trimws(species)) == 0) {
    return(data.frame())
  }
  
  tryCatch({
    query <- "SELECT * FROM soil_samples WHERE species = $1 ORDER BY created_at DESC"
    result <- dbGetQuery(pool, query, params = list(species))
    
    if (nrow(result) == 0) {
      message("No data found for species: ", species)
    }
    return(result)
  }, error = function(e) {
    message("Error fetching species data: ", e$message)
    return(data.frame())
  })
}

db_get_unique_species <- function() {
  tryCatch({
    result <- dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")
    result$species
  }, error = function(e) {
    message("Error fetching unique species: ", e$message)
    character()
  })
}

db_add_file <- function(sample_id, filename, file_type, file_size, storage_path) {
  tryCatch({
    dbExecute(pool, "
      INSERT INTO sample_files (sample_id, filename, file_type, file_size, storage_path)
      VALUES (?, ?, ?, ?, ?)",
              params = list(sample_id, filename, file_type, file_size, storage_path)
    )
  }, error = function(e) {
    message("Error adding file: ", e$message)
    return(FALSE)
  })
}

db_get_files <- function(sample_id = NULL) {
  tryCatch({
    if (is.null(sample_id)) {
      dbGetQuery(pool, "SELECT * FROM sample_files ORDER BY upload_date DESC")
    } else {
      dbGetQuery(pool, 
                 "SELECT * FROM sample_files WHERE sample_id = ? ORDER BY upload_date DESC",
                 params = list(sample_id))
    }
  }, error = function(e) {
    message("Error fetching files: ", e$message)
    data.frame()
  })
}

db_add_sample <- function(sample_data) {
  tryCatch({
    # Convert date to proper format
    if ("date" %in% names(sample_data)) {
      sample_data$date <- as.character(as.Date(sample_data$date))
    }
    
    # Create vectors of field names and values
    fields <- names(sample_data)
    values <- as.list(unname(sample_data))  # Unname the values
    
    # Create the SQL query using $n instead of ?
    placeholders <- paste0("$", seq_along(fields))
    query <- sprintf(
      "INSERT INTO soil_samples (%s) VALUES (%s) RETURNING id",
      paste(fields, collapse = ", "),
      paste(placeholders, collapse = ", ")
    )
    
    # Debug output
    message("Query: ", query)
    message("Number of fields: ", length(fields))
    message("Number of values: ", length(values))
    
    # Execute the query
    result <- dbGetQuery(pool, query, params = values)
    
    return(result$id[1])
  }, error = function(e) {
    message("Error details in db_add_sample: ", e$message)
    message("Data being inserted: ", paste(names(sample_data), collapse=", "))
    message("Values being inserted: ", paste(unlist(sample_data), collapse=", "))
    return(NULL)
  })
}

# Load and prepare species data
species_db <- read.csv("wcvp_names.csv", sep="|", quote="", 
                       col.names=c("plant_name_id", "ipni_id", "taxon_rank", "taxon_status",
                                   "family", "genus_hybrid", "genus", "species_hybrid", "species",
                                   "infraspecific_rank", "infraspecies", "parenthetical_author",
                                   "primary_author", "publication_author", "place_of_publication",
                                   "volume_and_page", "first_published", "nomenclatural_remarks",
                                   "geographic_area", "lifeform_description", "climate_description",
                                   "taxon_name", "taxon_authors", "accepted_plant_name_id",
                                   "basionym_plant_name_id", "replaced_synonym_author",
                                   "homotypic_synonym", "parent_plant_name_id", "powo_id",
                                   "hybrid_formula", "reviewed"),
                       na.strings=c("", "NA"), stringsAsFactors=FALSE) %>%
  filter(taxon_rank == "Species", 
         taxon_status == "Accepted") %>%
  select(taxon_name, family, genus, species) %>%
  distinct()

# Load ecoregions data
data(ContinentalUsEcoregion4)
eco_sf <- st_as_sf(ContinentalUsEcoregion4) %>%
  st_make_valid() %>%  # Fix invalid geometries
  st_transform(4326)   # Ensure WGS84 projection

# Define soil texture classes
soil_texture_classes <- data.frame(
  Texture = c(
    "Heavy Clay", "Silty Clay", "Clay", "Silty Clay Loam", "Clay Loam",
    "Silt", "Silty Loam", "Sandy Clay", "Loam", "Sandy Clay Loam",
    "Sandy Loam", "Loamy Sand", "Sand"
  ),
  Clay_Min = c(60, 40, 40, 27, 27, 0, 0, 35, 7, 20, 0, 0, 0),
  Clay_Max = c(100, 60, 60, 40, 40, 12, 27, 55, 27, 35, 20, 15, 10),
  Silt_Min = c(0, 40, 40, 40, 15, 88, 74, 0, 28, 20, 0, 0, 0),
  Silt_Max = c(40, 60, 60, 60, 52, 100, 88, 20, 50, 45, 50, 30, 14),
  Sand_Min = c(0, 0, 0, 0, 20, 0, 0, 45, 23, 45, 50, 70, 86),
  Sand_Max = c(45, 20, 45, 20, 45, 20, 50, 65, 52, 80, 70, 86, 100)
)

# Create directory for file uploads if it doesn't exist
if (!dir.exists("www/uploads")) {
  dir.create("www/uploads", recursive = TRUE)
}

ui <- page_fluid(
  theme = bs_theme(bootswatch = "minty"),
  
  card(
    card_header("Soil Testing Database for Plant Species"),
    "Record and analyze soil conditions for different plant species across locations"
  ),
  
  layout_columns(
    # Input card
    card(
      card_header("Add New Soil Data"),
      
      # Species selection
      div(
        class = "form-group",
        tags$label("Plant Species"),
        selectizeInput("species", NULL, 
                       choices = NULL,
                       options = list(
                         maxItems = 1,
                         maxOptions = 100,
                         placeholder = 'Type to search species...'
                       )
        ),
        helpText("Start typing to search for species from the database.")
      ),
      
      # Cultivar input
      textInput("cultivar", "Cultivar (optional)", ""),
      
      # Basic soil parameters and cultivar and note sections
      numericInput("ph", "Soil pH", value = 7.0, min = 0, max = 14, step = 0.1),
      numericInput("organic_matter", "Organic Matter (%)", value = 2, min = 0, max = 100),
      
      # Nutrient levels
      h4("Nutrient Levels (ppm)"),
      numericInput("nitrate", "Nitrate", value = 0, min = 0),
      numericInput("ammonium", "Ammonium", value = 0, min = 0),
      numericInput("phosphorus", "Phosphorus", value = 0, min = 0),
      numericInput("potassium", "Potassium", value = 0, min = 0),
      numericInput("calcium", "Calcium", value = 0, min = 0),
      numericInput("magnesium", "Magnesium", value = 0, min = 0),
      numericInput("soluble_salts", "Soluble Salts", value = 0, min = 0),
      
      # Soil texture input options
      h4("Soil Texture"),
      radioButtons("texture_input_type", "Input Method:",
                   choices = c("Percentages" = "pct", "Classification" = "class")),
      
      # Conditional texture inputs
      conditionalPanel(
        condition = "input.texture_input_type == 'pct'",
        numericInput("sand", "Sand (%)", value = 33, min = 0, max = 100),
        numericInput("silt", "Silt (%)", value = 33, min = 0, max = 100),
        numericInput("clay", "Clay (%)", value = 34, min = 0, max = 100),
        textOutput("texture_class_result")
      ),
      
      conditionalPanel(
        condition = "input.texture_input_type == 'class'",
        selectInput("texture_class", "Soil Texture Class",
                    choices = soil_texture_classes$Texture),
        helpText("Note: When using texture class, approximate percentages will be used for plotting.")
      ),
      
      # Location
      h4("Location"),
      textInput("street", "Street Address (optional)", ""),
      textInput("city", "City/Town", ""),
      selectInput("state", "State", 
                  choices = state.name,
                  selected = "New York"),
      actionButton("geocode", "Get Coordinates", class = "btn-info"),
      div(
        style = "margin-top: 10px;",
        textOutput("geocode_result")
      ),
      div(
        style = "margin-top: 10px;",
        textOutput("ecoregion_result")
      ),
      numericInput("latitude", "Latitude", value = 0, min = -90, max = 90),
      numericInput("longitude", "Longitude", value = 0, min = -180, max = 180),
      dateInput("date", "Sample Date", value = Sys.Date()),
      textAreaInput("notes", "Notes", "", height = "100px"),
      actionButton("submit", "Submit Data", class = "btn-primary")
    ),
    
    
    card(
      card_header("Data Management"),
      # CSV Import
      fileInput("csv_import", "Import CSV Data",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      downloadButton("download_template", "Download CSV Template"),
      downloadButton("export_data", "Export All Data"),
      
      # Sample Photos and PDFs
      fileInput("photo_upload", "Upload Plant Photo",
                accept = c('image/png', 'image/jpeg', 'image/jpg'),
                multiple = TRUE),
      fileInput("pdf_upload", "Upload Soil Report (PDF)",
                accept = c('application/pdf'),
                multiple = TRUE),
      DTOutput("uploaded_files_table")
    ),
    
    # Analysis card
    card(
      card_header("Analysis"),
      selectInput("analysis_species", "Select Species to Analyze", choices = NULL),
      tabsetPanel(
        tabPanel("Summary Statistics",
                 tableOutput("summary_stats")
        ),
        tabPanel("pH-OM Relationship",
                 plotOutput("ph_om_plot")
        ),
        tabPanel("Parameter Correlations",
                 plotOutput("heatmap_plot")
        ),
        tabPanel("pH Distribution",
                 plotOutput("ph_plot")
        ),
        tabPanel("Soil Texture",
                 plotOutput("texture_plot")
        ),
        tabPanel("Nutrient Analysis",
                 plotOutput("nutrient_plot")
        ),
        tabPanel("Geographic Distribution",
                 plotOutput("map_plot")
        ),
        tabPanel("Raw Data",
                 DTOutput("raw_data")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_changed <- reactiveVal(0)
  # CSV Template Download
  output$download_template <- downloadHandler(
    filename = function() {
      "soil_data_template.csv"
    },
    content = function(file) {
      # Use soil_data structure for template
      template <- soil_data[0, ]
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Export Data
  output$export_data <- downloadHandler(
    filename = function() {
      paste0("soil_data_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- db_get_all_samples()  # Use database function instead
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # CSV import handler
  observeEvent(input$csv_import, {
    req(input$csv_import)
    
    tryCatch({
      imported_data <- read.csv(input$csv_import$datapath)
      
      # Validate required columns
      required_cols <- c("species", "ph", "organic_matter", "nitrate_ppm",
                         "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
                         "calcium_ppm", "magnesium_ppm", "soluble_salts_ppm",
                         "texture_sand", "texture_silt", "texture_clay",
                         "texture_class", "location_lat", "location_long",
                         "date", "ecoregion_l4", "ecoregion_l4_code")
      
      missing_cols <- setdiff(required_cols, names(imported_data))
      
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return()
      }
      
      # Validate species against database
      invalid_species <- setdiff(imported_data$species, species_db$taxon_name)
      if (length(invalid_species) > 0) {
        showNotification(
          paste("Invalid species found:", paste(invalid_species, collapse = ", "),
                "\nPlease ensure all species match the database."),
          type = "error"
        )
        return()
      }
      
      # Add each row to database
      for(i in 1:nrow(imported_data)) {
        db_add_sample(imported_data[i,])
      }
      
      showNotification("CSV data imported successfully!", type = "message")
      data_changed(data_changed() + 1)
    }, error = function(e) {
      showNotification(paste("Error importing CSV:", e$message), type = "error")
    })
  })
  
  # File Upload Handler
  observeEvent(input$photo_upload, {
    req(input$photo_upload)
    
    # Create directory if it doesn't exist
    dir.create(file.path("www", "uploads"), showWarnings = FALSE, recursive = TRUE)
    
    for (i in 1:nrow(input$photo_upload)) {
      file <- input$photo_upload[i,]
      ext <- tools::file_ext(file$name)
      new_filename <- paste0(
        "photo_", 
        format(Sys.time(), "%Y%m%d_%H%M%S"), 
        "_", 
        i, 
        ".", 
        ext
      )
      
      # Create full path
      storage_path <- file.path("uploads", new_filename)
      full_path <- file.path("www", storage_path)
      
      # Copy file to uploads directory
      file.copy(file$datapath, full_path, overwrite = TRUE)
      
      # Add file record to database with error handling
      tryCatch({
        db_add_file(
          sample_id = NULL,  # Will be updated when linked to a sample
          filename = file$name,
          file_type = "photo",
          file_size = file$size,
          storage_path = storage_path  # Store relative path
        )
        showNotification(sprintf("Uploaded: %s", file$name), type = "message")
      }, error = function(e) {
        showNotification(sprintf("Error uploading %s: %s", file$name, e$message), type = "error")
      })
    }
  })
  
  observeEvent(input$pdf_upload, {
    req(input$pdf_upload)
    
    # Create directory if it doesn't exist
    dir.create(file.path("www", "uploads"), showWarnings = FALSE, recursive = TRUE)
    
    for (i in 1:nrow(input$pdf_upload)) {
      file <- input$pdf_upload[i,]
      new_filename <- paste0(
        "pdf_", 
        format(Sys.time(), "%Y%m%d_%H%M%S"), 
        "_", 
        i, 
        ".pdf"
      )
      
      # Create full path
      storage_path <- file.path("uploads", new_filename)
      full_path <- file.path("www", storage_path)
      
      # Copy file to uploads directory
      file.copy(file$datapath, full_path, overwrite = TRUE)
      
      # Add file record to database with error handling
      tryCatch({
        db_add_file(
          sample_id = NULL,  # Will be updated when linked to a sample
          filename = file$name,
          file_type = "pdf",
          file_size = file$size,
          storage_path = storage_path  # Store relative path
        )
        showNotification(sprintf("Uploaded: %s", file$name), type = "message")
      }, error = function(e) {
        showNotification(sprintf("Error uploading %s: %s", file$name, e$message), type = "error")
      })
    }
  })
  
  observeEvent(input$pdf_upload, {
    req(input$pdf_upload)
    for (i in 1:nrow(input$pdf_upload)) {
      file <- input$pdf_upload[i,]
      new_filename <- paste0(
        "pdf_", 
        format(Sys.time(), "%Y%m%d_%H%M%S"), 
        "_", 
        i, 
        ".pdf"
      )
      
      # Create full path
      storage_path <- file.path("uploads", new_filename)
      full_path <- file.path("www", storage_path)
      
      # Copy file to uploads directory
      file.copy(file$datapath, full_path)
      
      # Add file record to database
      db_add_file(
        sample_id = NULL,  # Will be updated when linked to a sample
        filename = file$name,
        file_type = "pdf",
        file_size = file$size,
        storage_path = storage_path  # Store relative path
      )
    }
    showNotification("PDFs uploaded successfully!", type = "message")
  })
  
  # Display uploaded files
  output$uploaded_files_table <- renderDT({
    files_data <- db_get_files()
    if (nrow(files_data) > 0) {
      # Create display data frame
      display_data <- data.frame(
        Filename = files_data$filename,
        Type = files_data$file_type,
        Size = paste0(round(files_data$file_size / 1024, 1), " KB"),
        Uploaded = files_data$upload_date,
        Actions = sprintf(
          '<a href="%s" target="_blank" class="btn btn-sm btn-info">View</a>',
          files_data$storage_path
        ),
        stringsAsFactors = FALSE
      )
      
      datatable(
        display_data,
        escape = FALSE,
        options = list(
          pageLength = 5,
          dom = 'rtip',
          columnDefs = list(
            list(targets = 4, className = 'dt-center')
          )
        ),
        selection = 'none'
      )
    }
  })
  
  # Helper function to get texture percentages from class
  get_texture_percentages <- function(texture_class) {
    class_data <- soil_texture_classes[soil_texture_classes$Texture == texture_class, ]
    if(nrow(class_data) == 0) return(NULL)
    
    # Calculate midpoints of ranges
    sand_mid <- mean(c(class_data$Sand_Min, class_data$Sand_Max))
    silt_mid <- mean(c(class_data$Silt_Min, class_data$Silt_Max))
    clay_mid <- mean(c(class_data$Clay_Min, class_data$Clay_Max))
    
    # Normalize to ensure sum is 100
    total <- sand_mid + silt_mid + clay_mid
    list(
      sand = sand_mid * (100/total),
      silt = silt_mid * (100/total),
      clay = clay_mid * (100/total)
    )
  }
  
  # Get ecoregion based on coordinates
  get_ecoregion <- function(lat, long) {
    # Input validation
    if(is.null(lat) || is.null(long) || is.na(lat) || is.na(long)) {
      message("Invalid coordinates: NULL or NA values")
      return(list(name = NA_character_, code = NA_character_))
    }
    
    message(sprintf("Checking ecoregion for coordinates: %f, %f", lat, long))
    
    tryCatch({
      # Create point geometry
      point_sf <- st_as_sf(data.frame(long = long, lat = lat), 
                           coords = c("long", "lat"), 
                           crs = 4326)
      
      message("Created point geometry successfully")
      
      # Check if eco_sf exists and is valid
      if(!exists("eco_sf") || is.null(eco_sf)) {
        message("eco_sf object not found or is NULL")
        return(list(name = NA_character_, code = NA_character_))
      }
      
      message("Checking intersection with ecoregions")
      
      # Check for intersection
      intersects <- st_intersects(eco_sf, point_sf, sparse = FALSE)
      message(sprintf("Found %d intersecting ecoregions", sum(intersects)))
      
      if(any(intersects)) {
        match_idx <- which(intersects)[1]
        message(sprintf("Using ecoregion at index %d", match_idx))
        
        # Use the correct lowercase column names
        name <- as.character(eco_sf$us_l4name[match_idx])
        code <- as.character(eco_sf$us_l4code[match_idx])
        
        return(list(
          name = if(is.null(name)) NA_character_ else name,
          code = if(is.null(code)) NA_character_ else code
        ))
      } else {
        message("No intersection found")
        return(list(name = NA_character_, code = NA_character_))
      }
    }, error = function(e) {
      message("Error in get_ecoregion: ", e$message)
      return(list(name = NA_character_, code = NA_character_))
    })
  }
  
  # Determine texture class from percentages
  texture_class <- reactive({
    req(input$texture_input_type == "pct")
    req(input$sand, input$silt, input$clay)
    
    # First check if percentages sum to 100
    if(abs((input$sand + input$silt + input$clay) - 100) > 0.1) {
      return("Error: Percentages must sum to 100%")
    }
    
    # Check each texture class with some tolerance
    for(i in 1:nrow(soil_texture_classes)) {
      if(input$clay >= (soil_texture_classes$Clay_Min[i] - 0.1) && 
         input$clay <= (soil_texture_classes$Clay_Max[i] + 0.1) &&
         input$silt >= (soil_texture_classes$Silt_Min[i] - 0.1) && 
         input$silt <= (soil_texture_classes$Silt_Max[i] + 0.1) &&
         input$sand >= (soil_texture_classes$Sand_Min[i] - 0.1) && 
         input$sand <= (soil_texture_classes$Sand_Max[i] + 0.1)) {
        return(as.character(soil_texture_classes$Texture[i]))
      }
    }
    return("Unclassified")
  })
  
  output$texture_class_result <- renderText({
    paste("Calculated Texture Class:", texture_class())
  })
  
  # Geocoding observer
  observeEvent(input$geocode, {
    req(input$city, input$state)  # Require at least city and state
    
    # Construct address string
    address <- paste(
      if(!is.null(input$street) && input$street != "") paste(input$street, ",", "") else "",
      input$city, ",",
      input$state
    )
    
    # Ensure geocode_result exists even if geocoding fails
    output$geocode_result <- renderText("")
    output$ecoregion_result <- renderText("")
    
    # Geocode the address
    result <- tryCatch({
      geo(address = address, method = "osm")
    }, error = function(e) {
      output$geocode_result <- renderText({
        "Error in geocoding. Please check the address or enter coordinates manually."
      })
      return(NULL)
    })
    
    # Proceed only if we have valid results
    if(!is.null(result) && length(result) > 0 && !is.na(result$lat[1]) && !is.na(result$long[1])) {
      # Update the lat/long inputs
      updateNumericInput(session, "latitude", value = result$lat[1])
      updateNumericInput(session, "longitude", value = result$long[1])
      
      # Update geocoding result
      output$geocode_result <- renderText({
        paste("Successfully found coordinates:", 
              round(result$lat[1], 4), ",", 
              round(result$long[1], 4))
      })
      
      # Get ecoregion with additional error handling
      tryCatch({
        eco <- get_ecoregion(result$lat[1], result$long[1])
        
        # Update ecoregion result only if we have valid data
        if(!is.null(eco) && !is.null(eco$name) && !is.na(eco$name)) {
          output$ecoregion_result <- renderText({
            ecocode <- if(!is.null(eco$code) && !is.na(eco$code)) {
              paste0(" (", eco$code, ")")
            } else {
              ""
            }
            paste("Ecoregion:", eco$name, ecocode)
          })
        } else {
          output$ecoregion_result <- renderText({
            "Could not determine ecoregion for these coordinates"
          })
        }
      }, error = function(e) {
        output$ecoregion_result <- renderText({
          "Error determining ecoregion"
        })
      })
    } else {
      # Show error message for failed geocoding
      output$geocode_result <- renderText({
        "Could not find coordinates for this address. Please check the address or enter coordinates manually."
      })
    }
  })
  
  # Add new data
  observeEvent(input$submit, {
    # Validation
    if (input$texture_input_type == "pct") {
      texture_sum <- input$sand + input$silt + input$clay
      if (abs(texture_sum - 100) > 0.1) {
        showNotification("Soil texture percentages must sum to 100%", type = "error")
        return()
      }
    }
    
    # Get ecoregion data using the input coordinates directly
    eco <- get_ecoregion(input$latitude, input$longitude)
    
    # Get texture percentages based on input type
    if(input$texture_input_type == "class") {
      texture_pcts <- get_texture_percentages(input$texture_class)
    }
    
    new_data <- data.frame(
      species = input$species,
      cultivar = input$cultivar,
      ph = input$ph,
      organic_matter = input$organic_matter,
      nitrate_ppm = input$nitrate,
      ammonium_ppm = input$ammonium,
      phosphorus_ppm = input$phosphorus,
      potassium_ppm = input$potassium,
      calcium_ppm = input$calcium,
      magnesium_ppm = input$magnesium,
      soluble_salts_ppm = input$soluble_salts,
      texture_class = if(input$texture_input_type == "pct") texture_class() else input$texture_class,
      texture_sand = if(input$texture_input_type == "pct") input$sand else texture_pcts$sand,
      texture_silt = if(input$texture_input_type == "pct") input$silt else texture_pcts$silt,
      texture_clay = if(input$texture_input_type == "pct") input$clay else texture_pcts$clay,
      ecoregion_l4 = eco$name,
      ecoregion_l4_code = eco$code,
      location_lat = input$latitude,
      location_long = input$longitude,
      notes = input$notes,
      date = input$date
    )
    
    # Add to database
    sample_id <- db_add_sample(new_data)
    
    if (!is.null(sample_id)) {
      showNotification("Data added successfully!", type = "message")
      data_changed(data_changed() + 1)  # Increment the counter to trigger update
    } else {
      showNotification("Error adding data", type = "error")
    }
  })
  
  # Initialize the species inputs
  observe({
    # Get all species names
    species_choices <- species_db %>%
      pull(taxon_name) %>%
      sort()
    
    # Update the main species input
    updateSelectizeInput(session, "species",
                         choices = species_choices,
                         selected = "",
                         server = TRUE,
                         options = list(
                           maxItems = 1,
                           maxOptions = 100,
                           placeholder = 'Type to search species...'
                         )
    )
  })
  
  # Update analysis species choices
  observe({
    data_changed()
    
    species_list <- tryCatch({
      result <- dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")
      if(nrow(result) > 0) result$species else character(0)
    }, error = function(e) {
      message("Error fetching species list: ", e$message)
      character(0)
    })
    
    updateSelectInput(session, "analysis_species", 
                      choices = c("Select a species" = "", species_list),
                      selected = "")
  })
  
  # Summary statistics
  output$summary_stats <- renderTable({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    
    if(nrow(species_data) == 0) {
      return(data.frame(Measure = "No data available", Value = ""))
    }
    
    data.frame(
      Measure = c("Number of Samples", "Average pH", "pH Range", 
                  "Average Organic Matter (%)",
                  "Average Nitrate (ppm)",
                  "Average Phosphorus (ppm)",
                  "Average Potassium (ppm)"),
      Value = c(
        nrow(species_data),
        round(mean(species_data$ph, na.rm = TRUE), 2),
        paste(round(range(species_data$ph, na.rm = TRUE), 2), collapse = " - "),
        round(mean(species_data$organic_matter, na.rm = TRUE), 2),
        round(mean(species_data$nitrate_ppm, na.rm = TRUE), 2),
        round(mean(species_data$phosphorus_ppm, na.rm = TRUE), 2),
        round(mean(species_data$potassium_ppm, na.rm = TRUE), 2)
      )
    )
  })
  
  # pH distribution plot
  output$ph_plot <- renderPlot({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    
    if(nrow(species_data) == 0) {
      return(NULL)
    }
    
    ggplot(species_data, aes(x = ph)) +
      geom_histogram(bins = 15, fill = "skyblue", color = "black") +
      labs(title = paste("pH Distribution for", input$analysis_species),
           x = "pH", y = "Count") +
      theme_minimal()
  })
  
  # Soil texture plot
  output$texture_plot <- renderPlot({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    
    # Remove any rows where texture data is NA
    species_data <- species_data[!is.na(species_data$texture_sand) & 
                                   !is.na(species_data$texture_silt) & 
                                   !is.na(species_data$texture_clay), ]
    
    if(nrow(species_data) == 0) {
      return(NULL)
    }
    
    p <- ggtern(species_data, aes(x = texture_sand, y = texture_silt, z = texture_clay)) +
      geom_point(alpha = 0.6) +
      theme_bw() +
      theme_showarrows() +
      labs(
        title = paste("Soil Texture Distribution for", input$analysis_species)
      ) +
      xlab("Sand (%)") +
      ylab("Silt (%)") +
      zlab("Clay (%)") +
      theme(
        tern.panel.background = element_rect(fill = "aliceblue"),
        tern.panel.grid.major = element_line(color = "white"),
        tern.panel.grid.minor = element_line(color = "white"),
        axis.title = element_text(size = 12, face = "bold")
      ) +
      scale_T_continuous(breaks = seq(0, 100, by = 10)) +
      scale_L_continuous(breaks = seq(0, 100, by = 10)) +
      scale_R_continuous(breaks = seq(0, 100, by = 10))
    
    print(p)
  }, height = 600, width = 600)
  
  # Nutrient analysis plot
  output$nutrient_plot <- renderPlot({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    
    if(nrow(species_data) == 0) {
      return(NULL)
    }
    
    nutrient_data <- species_data %>%
      select(nitrate_ppm, phosphorus_ppm, potassium_ppm, 
             calcium_ppm, magnesium_ppm) %>%
      pivot_longer(everything(), 
                   names_to = "nutrient", 
                   values_to = "value",
                   names_pattern = "(.+)_ppm")
    
    ggplot(nutrient_data, aes(x = nutrient, y = value)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("Nutrient Distribution for", input$analysis_species),
           x = "Nutrient", y = "Concentration (ppm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # pH vs Organic Matter relationship plot
  output$ph_om_plot <- renderPlot({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    
    if(nrow(species_data) == 0) {
      return(NULL)
    }
    
    ggplot(species_data, aes(x = ph, y = organic_matter)) +
      geom_point(aes(color = texture_class), size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      labs(title = paste("pH vs Organic Matter for", input$analysis_species),
           x = "pH",
           y = "Organic Matter (%)",
           color = "Texture Class") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      )
  })
  
  # Parameter correlation heatmap
  output$heatmap_plot <- renderPlot({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    
    if(nrow(species_data) == 0) {
      return(NULL)
    }
    
    # Select numeric columns for correlation
    numeric_cols <- c("ph", "organic_matter", "nitrate_ppm", "phosphorus_ppm", 
                      "potassium_ppm", "calcium_ppm", "magnesium_ppm", 
                      "soluble_salts_ppm")
    
    # Calculate correlation matrix
    cor_matrix <- cor(species_data[, numeric_cols], use = "complete.obs")
    cor_data <- as.data.frame(as.table(cor_matrix))
    
    # Create shorter names for better display
    name_mapping <- c(
      "ph" = "pH",
      "organic_matter" = "Organic",
      "nitrate_ppm" = "Nitrate",
      "phosphorus_ppm" = "P",
      "potassium_ppm" = "K",
      "calcium_ppm" = "Ca",
      "magnesium_ppm" = "Mg",
      "soluble_salts_ppm" = "Salts"
    )
    
    cor_data$Var1 <- name_mapping[as.character(cor_data$Var1)]
    cor_data$Var2 <- name_mapping[as.character(cor_data$Var2)]
    
    ggplot(cor_data, aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "blue",
        high = "red",
        mid = "white",
        midpoint = 0,
        limit = c(-1,1),
        name = "Correlation"
      ) +
      geom_text(aes(label = sprintf("%.2f", Freq)), size = 3) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(title = paste("Parameter Correlations for", input$analysis_species))
  })
  
  # Geographic distribution plot
  output$map_plot <- renderPlot({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    
    if(nrow(species_data) == 0) {
      return(NULL)
    }
    
    # Get US map data
    us_map <- map_data("usa")
    states_map <- map_data("state")
    
    ggplot() +
      # Add US base map layers
      geom_polygon(data = states_map, 
                   aes(x = long, y = lat, group = group),
                   fill = "white", 
                   color = "gray70", 
                   size = 0.2) +
      # Add sample points
      geom_point(data = species_data,
                 aes(x = location_long, y = location_lat),
                 color = "red",
                 size = 3,
                 alpha = 0.6) +
      # Customize the map
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      labs(title = paste("Geographic Distribution for", input$analysis_species),
           x = "Longitude",
           y = "Latitude") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      ) +
      # Set reasonable limits for continental US
      xlim(-125, -65) +
      ylim(25, 50)
  }, height = 500)
  
  # Raw data table
  output$raw_data <- renderDT({
    req(input$analysis_species)
    species_data <- db_get_species_data(input$analysis_species)
    datatable(species_data)
  })
}

# Create Shiny app
shinyApp(ui, server)