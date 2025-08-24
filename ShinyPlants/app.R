# ---------------------------
# Edaphic Flora - Shiny App
# ---------------------------

# --- Dev: keep the origin stable for Firebase redirect/popup flows
if (interactive()) {
  options(shiny.launch.browser = FALSE)
  options(shiny.host = "127.0.0.1")
  options(shiny.port = 7420)        # pick a free port; keep it fixed during dev
}

# --- Load env vars (works locally & on shinyapps.io / Connect)
if (file.exists(".Renviron")) readRenviron(".Renviron")

# --- Packages
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
library(aws.s3)
library(uuid)
library(mime)
library(polished)

# ---------------------------
# Configuration & helpers
# ---------------------------

# Downloads of media from S3 (UI table) are OFF by default for now
DOWNLOADS_ENABLED <- FALSE

# Basic upload guards (tune as you like)
MAX_FILE_SIZE_MB <- as.numeric(Sys.getenv("MAX_FILE_SIZE_MB", unset = "20"))
MAX_FILES_PER_USER_PER_DAY <- as.integer(Sys.getenv("MAX_FILES_PER_USER_PER_DAY", unset = "200"))

# S3 helpers
s3_bucket <- Sys.getenv("S3_BUCKET")

s3_put <- function(local_path, object_key, content_type = NULL) {
  put_object(
    file   = local_path,
    object = object_key,
    bucket = s3_bucket,
    headers = c(
      if (!is.null(content_type)) c(`Content-Type` = content_type),
      # server-side encryption on by default
      `x-amz-server-side-encryption` = "AES256"
    )
  )
}

s3_delete <- function(object_key) {
  # best-effort delete; returns TRUE/FALSE
  tryCatch({
    delete_object(object = object_key, bucket = s3_bucket)
    TRUE
  }, error = function(e) FALSE)
}

s3_presign_get <- function(object_key, expires = 3600) {
  get_signed_url(object = object_key, bucket = s3_bucket, expires = expires)
}

# Firebase / Polished
firebase_cfg <- list(
  apiKey     = Sys.getenv("FIREBASE_API_KEY"),
  authDomain = Sys.getenv("FIREBASE_AUTH_DOMAIN"),   # e.g. REDACTED_FIREBASE_DOMAIN
  projectId  = Sys.getenv("FIREBASE_PROJECT_ID")
)

polished::polished_config(
  app_name  = Sys.getenv("POLISHED_APP_NAME"),
  api_key   = Sys.getenv("POLISHED_API_KEY"),
  firebase_config   = firebase_cfg,
  sign_in_providers = c("google", "email"),
  is_invite_required = FALSE,
  is_email_verification_required = FALSE
)

# ---------------------------
# Database (Neon/Postgres) via pool
# ---------------------------

pool <- dbPool(
  drv      = Postgres(),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname   = Sys.getenv("POSTGRES_DB"),
  user     = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", unset = "require"),
  minSize  = 1,
  maxSize  = 25
)

onStop(function() poolClose(pool))

# Bootstrap / migrate schema (idempotent)
tryCatch({
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
    )
  ")
  
  # Add new columns if missing
  dbExecute(pool, "ALTER TABLE soil_samples ADD COLUMN IF NOT EXISTS created_by TEXT")
  
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS sample_files (
      id SERIAL PRIMARY KEY,
      sample_id INTEGER REFERENCES soil_samples(id),
      filename VARCHAR(255),
      file_type VARCHAR(50),
      file_size INTEGER,
      storage_path VARCHAR(255), -- kept for backwards compatibility; we store S3 key here
      upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Extended file metadata (added safely if missing)
  dbExecute(pool, "ALTER TABLE sample_files ADD COLUMN IF NOT EXISTS mime_type TEXT")
  dbExecute(pool, "ALTER TABLE sample_files ADD COLUMN IF NOT EXISTS bucket TEXT")
  dbExecute(pool, "ALTER TABLE sample_files ADD COLUMN IF NOT EXISTS object_key TEXT")
  dbExecute(pool, "ALTER TABLE sample_files ADD COLUMN IF NOT EXISTS session_token TEXT")
  dbExecute(pool, "ALTER TABLE sample_files ADD COLUMN IF NOT EXISTS uploader_user_id TEXT")
  dbExecute(pool, "ALTER TABLE sample_files ADD COLUMN IF NOT EXISTS version_id TEXT")
  dbExecute(pool, "ALTER TABLE sample_files ADD COLUMN IF NOT EXISTS visibility TEXT DEFAULT 'private'")
  
  # Indices
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_species ON soil_samples(species)")
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_date ON soil_samples(date)")
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_files_sample_id ON sample_files(sample_id)")
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_files_uploader ON sample_files(uploader_user_id)")
}, error = function(e) {
  message("DB bootstrap error: ", e$message)
})

# ---------------------------
# DB helpers
# ---------------------------

db_get_all_samples <- function() {
  tryCatch({
    dbGetQuery(pool, "SELECT * FROM soil_samples ORDER BY created_at DESC")
  }, error = function(e) {
    message("Error fetching samples: ", e$message)
    data.frame()
  })
}

db_get_species_data <- function(species) {
  if (is.null(species) || !nzchar(trimws(species))) return(data.frame())
  tryCatch({
    dbGetQuery(pool, "SELECT * FROM soil_samples WHERE species = $1 ORDER BY created_at DESC",
               params = list(species))
  }, error = function(e) {
    message("Error fetching species data: ", e$message)
    data.frame()
  })
}

db_get_unique_species <- function() {
  tryCatch({
    res <- dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")
    res$species
  }, error = function(e) {
    message("Error fetching unique species: ", e$message)
    character()
  })
}

db_files_uploaded_today <- function(user_uid) {
  tryCatch({
    val <- dbGetQuery(pool,
                      "SELECT COUNT(*)::int AS n FROM sample_files 
                       WHERE uploader_user_id = $1 AND upload_date::date = CURRENT_DATE",
                      params = list(user_uid))
    as.integer(val$n[1])
  }, error = function(e) 0L)
}

db_add_sample <- function(sample_data) {
  tryCatch({
    if ("date" %in% names(sample_data)) {
      sample_data$date <- as.character(as.Date(sample_data$date))
    }
    fields <- names(sample_data)
    values <- as.list(unname(sample_data))
    placeholders <- paste0("$", seq_along(fields))
    sql <- sprintf("INSERT INTO soil_samples (%s) VALUES (%s) RETURNING id",
                   paste(fields, collapse = ", "), paste(placeholders, collapse = ", "))
    ret <- dbGetQuery(pool, sql, params = values)
    ret$id[1]
  }, error = function(e) {
    message("db_add_sample error: ", e$message)
    NULL
  })
}

db_add_file <- function(sample_id, filename, file_type, file_size, storage_path,
                        mime_type = NULL, bucket = s3_bucket, session_token = NULL,
                        uploader_user_id = NULL, version_id = NULL, visibility = "private",
                        object_key = storage_path) {
  # NOTE: use NA_* to represent SQL NULLs (NOT NULL)
  sample_id <- if (is.null(sample_id)) NA_integer_ else as.integer(sample_id)
  tryCatch({
    dbExecute(pool, "
      INSERT INTO sample_files
      (sample_id, filename, file_type, file_size, storage_path, mime_type,
       bucket, object_key, session_token, uploader_user_id, version_id, visibility)
      VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)
    ",
              params = list(sample_id, filename, file_type, as.integer(file_size), storage_path, mime_type,
                            bucket, object_key, session_token, uploader_user_id, version_id, visibility))
    TRUE
  }, error = function(e) {
    message("db_add_file error: ", e$message)
    FALSE
  })
}

db_get_files <- function(sample_id = NULL, user_uid = NULL, only_mine = FALSE) {
  tryCatch({
    if (!is.null(sample_id)) {
      dbGetQuery(pool,
                 "SELECT * FROM sample_files WHERE sample_id = $1 ORDER BY upload_date DESC",
                 params = list(as.integer(sample_id)))
    } else if (only_mine && !is.null(user_uid)) {
      dbGetQuery(pool,
                 "SELECT * FROM sample_files WHERE uploader_user_id = $1 ORDER BY upload_date DESC",
                 params = list(user_uid))
    } else {
      dbGetQuery(pool, "SELECT * FROM sample_files ORDER BY upload_date DESC")
    }
  }, error = function(e) {
    message("db_get_files error: ", e$message)
    data.frame()
  })
}

db_delete_file <- function(file_id, user_uid, is_admin = FALSE) {
  # Authorize & return the row to allow S3 delete
  tryCatch({
    row <- dbGetQuery(pool,
                      "SELECT * FROM sample_files WHERE id = $1",
                      params = list(as.integer(file_id))
    )
    if (nrow(row) == 0) return(list(ok = FALSE, reason = "not_found"))
    if (!is_admin && !identical(row$uploader_user_id[1], user_uid)) {
      return(list(ok = FALSE, reason = "forbidden"))
    }
    # delete DB row
    dbExecute(pool, "DELETE FROM sample_files WHERE id = $1", params = list(as.integer(file_id)))
    list(ok = TRUE, object_key = row$object_key[1], bucket = row$bucket[1])
  }, error = function(e) {
    list(ok = FALSE, reason = e$message)
  })
}

# ---------------------------
# Reference data
# ---------------------------

# Species (accepted species only)
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
                       na.strings=c("", "NA"), stringsAsFactors=FALSE) %>%
  filter(taxon_rank == "Species", taxon_status == "Accepted") %>%
  select(taxon_name, family, genus, species) %>%
  distinct()

# Ecoregions
data(ContinentalUsEcoregion4)
eco_sf <- st_as_sf(ContinentalUsEcoregion4) %>%
  st_make_valid() %>% st_transform(4326)

# Soil texture classes
soil_texture_classes <- data.frame(
  Texture = c("Heavy Clay","Silty Clay","Clay","Silty Clay Loam","Clay Loam",
              "Silt","Silty Loam","Sandy Clay","Loam","Sandy Clay Loam",
              "Sandy Loam","Loamy Sand","Sand"),
  Clay_Min = c(60,40,40,27,27,0,0,35,7,20,0,0,0),
  Clay_Max = c(100,60,60,40,40,12,27,55,27,35,20,15,10),
  Silt_Min = c(0,40,40,40,15,88,74,0,28,20,0,0,0),
  Silt_Max = c(40,60,60,60,52,100,88,20,50,45,50,30,14),
  Sand_Min = c(0,0,0,0,20,0,0,45,23,45,50,70,86),
  Sand_Max = c(45,20,45,20,45,20,50,65,52,80,70,86,100)
)

# CSV template structure
soil_data <- data.frame(
  species = character(), cultivar = character(),
  ph = numeric(), organic_matter = numeric(),
  nitrate_ppm = numeric(), ammonium_ppm = numeric(),
  phosphorus_ppm = numeric(), potassium_ppm = numeric(),
  calcium_ppm = numeric(), magnesium_ppm = numeric(),
  soluble_salts_ppm = numeric(),
  texture_sand = numeric(), texture_silt = numeric(), texture_clay = numeric(),
  texture_class = character(),
  location_lat = numeric(), location_long = numeric(),
  date = as.Date(character()),
  ecoregion_l4 = character(), ecoregion_l4_code = character(),
  notes = character(), stringsAsFactors = FALSE
)

# ---------------------------
# UI
# ---------------------------

base_ui <- page_fluid(
  theme = bs_theme(bootswatch = "minty"),
  
  card(
    card_header("Edaphic Flora — Soil Testing Database for Plant Species"),
    "Record and analyze soil conditions for different plant species across locations."
  ),
  
  layout_columns(
    
    # --- Input card
    card(
      card_header("Add New Soil Data"),
      
      # Species
      div(class="form-group",
          tags$label("Plant Species"),
          selectizeInput("species", NULL, choices = NULL,
                         options = list(maxItems = 1, maxOptions = 100, placeholder = 'Type to search species...')
          ),
          helpText("Start typing to search for species from the database.")
      ),
      
      textInput("cultivar", "Cultivar (optional)", ""),
      
      numericInput("ph", "Soil pH", value = 7.0, min = 0, max = 14, step = 0.1),
      numericInput("organic_matter", "Organic Matter (%)", value = 2, min = 0, max = 100),
      
      h4("Nutrient Levels (ppm)"),
      numericInput("nitrate",    "Nitrate",    value = 0, min = 0),
      numericInput("ammonium",   "Ammonium",   value = 0, min = 0),
      numericInput("phosphorus", "Phosphorus", value = 0, min = 0),
      numericInput("potassium",  "Potassium",  value = 0, min = 0),
      numericInput("calcium",    "Calcium",    value = 0, min = 0),
      numericInput("magnesium",  "Magnesium",  value = 0, min = 0),
      numericInput("soluble_salts", "Soluble Salts", value = 0, min = 0),
      
      h4("Soil Texture"),
      radioButtons("texture_input_type", "Input Method:",
                   choices = c("Percentages" = "pct", "Classification" = "class")),
      conditionalPanel(
        condition = "input.texture_input_type == 'pct'",
        numericInput("sand", "Sand (%)", value = 33, min = 0, max = 100),
        numericInput("silt", "Silt (%)", value = 33, min = 0, max = 100),
        numericInput("clay", "Clay (%)", value = 34, min = 0, max = 100),
        textOutput("texture_class_result")
      ),
      conditionalPanel(
        condition = "input.texture_input_type == 'class'",
        selectInput("texture_class", "Soil Texture Class", choices = soil_texture_classes$Texture),
        helpText("Note: When using texture class, approximate percentages will be used for plotting.")
      ),
      
      h4("Location"),
      textInput("street", "Street Address (optional)", ""),
      textInput("city", "City/Town", ""),
      selectInput("state", "State", choices = state.name, selected = "New York"),
      actionButton("geocode", "Get Coordinates", class = "btn-info"),
      div(style="margin-top:10px;", textOutput("geocode_result")),
      div(style="margin-top:10px;", textOutput("ecoregion_result")),
      numericInput("latitude",  "Latitude",  value = 0, min = -90, max = 90),
      numericInput("longitude", "Longitude", value = 0, min = -180, max = 180),
      dateInput("date", "Sample Date", value = Sys.Date()),
      textAreaInput("notes", "Notes", "", height = "100px"),
      actionButton("submit", "Submit Data", class = "btn-primary")
    ),
    
    # --- Data management
    card(
      card_header("Data Management"),
      fileInput("csv_import", "Import CSV Data",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      downloadButton("download_template", "Download CSV Template"),
      downloadButton("export_data", "Export All Data"),
      
      h5("Upload Photos & PDFs"),
      fileInput("photo_upload", "Upload Plant Photo",
                accept = c('image/png','image/jpeg','image/jpg'), multiple = TRUE),
      fileInput("pdf_upload", "Upload Soil Report (PDF)",
                accept = c('application/pdf'), multiple = TRUE),
      
      hr(),
      DTOutput("uploaded_files_table")
    ),
    
    # --- Analysis
    card(
      card_header("Analysis"),
      selectInput("analysis_species", "Select Species to Analyze", choices = NULL),
      tabsetPanel(
        tabPanel("Summary Statistics", tableOutput("summary_stats")),
        tabPanel("pH-OM Relationship", plotOutput("ph_om_plot")),
        tabPanel("Parameter Correlations", plotOutput("heatmap_plot")),
        tabPanel("pH Distribution", plotOutput("ph_plot")),
        tabPanel("Soil Texture", plotOutput("texture_plot")),
        tabPanel("Nutrient Analysis", plotOutput("nutrient_plot")),
        tabPanel("Geographic Distribution", plotOutput("map_plot")),
        tabPanel("Raw Data", DTOutput("raw_data"))
      )
    )
  )
)

# Wrap UI with Polished
ui <- polished::secure_ui(base_ui)

# ---------------------------
# Server
# ---------------------------

server_inner <- function(input, output, session) {
  
  # current signed-in user (list with uid, email, etc.)
  current_user <- reactive({
    f <- session$userData$user
    if (is.null(f)) return(NULL)
    f()
  })
  
  is_admin <- reactive({
    u <- current_user()
    if (is.null(u) || is.null(u$is_admin)) FALSE else isTRUE(u$is_admin)
  })
  
  data_changed <- reactiveVal(0)
  
  # CSV Template download
  output$download_template <- downloadHandler(
    filename = function() "soil_data_template.csv",
    content  = function(file) write.csv(soil_data[0,], file, row.names = FALSE)
  )
  
  # Export all samples
  output$export_data <- downloadHandler(
    filename = function() paste0("soil_data_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) write.csv(db_get_all_samples(), file, row.names = FALSE)
  )
  
  # CSV Import
  observeEvent(input$csv_import, {
    req(input$csv_import)
    u <- current_user(); if (is.null(u)) { showNotification("Please sign in.", type="error"); return() }
    
    tryCatch({
      imported <- read.csv(input$csv_import$datapath, stringsAsFactors = FALSE)
      
      required_cols <- c("species","ph","organic_matter","nitrate_ppm",
                         "ammonium_ppm","phosphorus_ppm","potassium_ppm",
                         "calcium_ppm","magnesium_ppm","soluble_salts_ppm",
                         "texture_sand","texture_silt","texture_clay",
                         "texture_class","location_lat","location_long",
                         "date","ecoregion_l4","ecoregion_l4_code")
      
      miss <- setdiff(required_cols, names(imported))
      if (length(miss) > 0) {
        showNotification(paste("Missing required columns:", paste(miss, collapse=", ")), type="error")
        return()
      }
      
      invalid <- setdiff(imported$species, species_db$taxon_name)
      if (length(invalid) > 0) {
        showNotification(
          paste("Invalid species found:", paste(head(invalid, 10), collapse=", "),
                if (length(invalid) > 10) "..."), type="error")
        return()
      }
      
      for (i in seq_len(nrow(imported))) {
        row <- imported[i,]
        row$created_by <- u$user_uid
        db_add_sample(row)
      }
      
      showNotification("CSV data imported successfully!", type="message")
      data_changed(data_changed()+1)
    }, error = function(e) {
      showNotification(paste("Error importing CSV:", e$message), type="error")
    })
  })
  
  # --- Geocoding
  get_ecoregion <- function(lat, long) {
    if (is.null(lat) || is.null(long) || is.na(lat) || is.na(long)) {
      return(list(name = NA_character_, code = NA_character_))
    }
    tryCatch({
      pt <- st_as_sf(data.frame(long=long, lat=lat), coords=c("long","lat"), crs=4326)
      inters <- st_intersects(eco_sf, pt, sparse = FALSE)
      if (any(inters)) {
        idx <- which(inters)[1]
        list(name = as.character(eco_sf$us_l4name[idx]),
             code = as.character(eco_sf$us_l4code[idx]))
      } else list(name = NA_character_, code = NA_character_)
    }, error = function(e) list(name = NA_character_, code = NA_character_))
  }
  
  # Soil texture helper
  get_texture_percentages <- function(texture_class) {
    cd <- soil_texture_classes[soil_texture_classes$Texture == texture_class,]
    if (!nrow(cd)) return(NULL)
    sand_mid <- mean(c(cd$Sand_Min, cd$Sand_Max))
    silt_mid <- mean(c(cd$Silt_Min, cd$Silt_Max))
    clay_mid <- mean(c(cd$Clay_Min, cd$Clay_Max))
    total <- sand_mid + silt_mid + clay_mid
    list(sand = sand_mid*(100/total), silt = silt_mid*(100/total), clay = clay_mid*(100/total))
  }
  
  # Texture class (from %)
  texture_class <- reactive({
    req(input$texture_input_type == "pct", input$sand, input$silt, input$clay)
    if (abs((input$sand + input$silt + input$clay) - 100) > 0.1) {
      return("Error: Percentages must sum to 100%")
    }
    for (i in seq_len(nrow(soil_texture_classes))) {
      if (input$clay >= (soil_texture_classes$Clay_Min[i]-0.1) &&
          input$clay <= (soil_texture_classes$Clay_Max[i]+0.1) &&
          input$silt >= (soil_texture_classes$Silt_Min[i]-0.1) &&
          input$silt <= (soil_texture_classes$Silt_Max[i]+0.1) &&
          input$sand >= (soil_texture_classes$Sand_Min[i]-0.1) &&
          input$sand <= (soil_texture_classes$Sand_Max[i]+0.1)) {
        return(as.character(soil_texture_classes$Texture[i]))
      }
    }
    "Unclassified"
  })
  output$texture_class_result <- renderText({ paste("Calculated Texture Class:", texture_class()) })
  
  # Geocode
  observeEvent(input$geocode, {
    req(input$city, input$state)
    address <- paste(
      if (!is.null(input$street) && nzchar(input$street)) paste0(input$street, ", ") else "",
      input$city, ", ", input$state
    )
    output$geocode_result <- renderText("")
    output$ecoregion_result <- renderText("")
    res <- tryCatch({ geo(address = address, method = "osm") }, error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0 && !is.na(res$lat[1]) && !is.na(res$long[1])) {
      updateNumericInput(session, "latitude", value = res$lat[1])
      updateNumericInput(session, "longitude", value = res$long[1])
      output$geocode_result <- renderText({
        paste("Successfully found coordinates:", round(res$lat[1], 4), ",", round(res$long[1], 4))
      })
      eco <- get_ecoregion(res$lat[1], res$long[1])
      if (!is.na(eco$name)) {
        output$ecoregion_result <- renderText({
          paste0("Ecoregion: ", eco$name, if (!is.na(eco$code)) paste0(" (", eco$code, ")"))
        })
      } else {
        output$ecoregion_result <- renderText("Could not determine ecoregion for these coordinates")
      }
    } else {
      output$geocode_result <- renderText(
        "Could not find coordinates for this address. Please check the address or enter coordinates manually."
      )
    }
  })
  
  # --- Add new sample
  observeEvent(input$submit, {
    u <- current_user(); if (is.null(u)) { showNotification("Please sign in.", type="error"); return() }
    
    if (input$texture_input_type == "pct") {
      if (abs((input$sand + input$silt + input$clay) - 100) > 0.1) {
        showNotification("Soil texture percentages must sum to 100%", type="error")
        return()
      }
    }
    
    eco <- get_ecoregion(input$latitude, input$longitude)
    if (input$texture_input_type == "class") {
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
      texture_class = if (input$texture_input_type == "pct") texture_class() else input$texture_class,
      texture_sand = if (input$texture_input_type == "pct") input$sand else texture_pcts$sand,
      texture_silt = if (input$texture_input_type == "pct") input$silt else texture_pcts$silt,
      texture_clay = if (input$texture_input_type == "pct") input$clay else texture_pcts$clay,
      ecoregion_l4 = eco$name,
      ecoregion_l4_code = eco$code,
      location_lat = input$latitude,
      location_long = input$longitude,
      notes = input$notes,
      date = input$date,
      created_by = u$user_uid,
      stringsAsFactors = FALSE
    )
    
    sample_id <- db_add_sample(new_data)
    
    if (!is.null(sample_id)) {
      # link any session-staged files to this sample
      dbExecute(pool, "
        UPDATE sample_files
           SET sample_id = $1
         WHERE session_token = $2
           AND sample_id IS NULL
      ", params = list(as.integer(sample_id), session$token))
      showNotification("Data added successfully!", type="message")
      data_changed(data_changed()+1)
    } else {
      showNotification("Error adding data", type="error")
    }
  })
  
  # --- Upload handlers (S3)
  observeEvent(input$photo_upload, {
    req(input$photo_upload)
    u <- current_user(); if (is.null(u)) { showNotification("Please sign in.", type="error"); return() }
    
    # guard: daily quota
    already <- db_files_uploaded_today(u$user_uid)
    if (already >= MAX_FILES_PER_USER_PER_DAY) {
      showNotification("Daily upload limit reached.", type="error"); return()
    }
    
    for (i in seq_len(nrow(input$photo_upload))) {
      file <- input$photo_upload[i,]
      if (file$size > MAX_FILE_SIZE_MB*1024^2) {
        showNotification(sprintf("'%s' exceeds %d MB limit", file$name, MAX_FILE_SIZE_MB), type="error")
        next
      }
      ext <- tools::file_ext(file$name)
      key <- sprintf("sessions/%s/photos/%s.%s", session$token, UUIDgenerate(), ext)
      ctype <- mime::guess_type(file$name, empty = "application/octet-stream")
      ok <- tryCatch(s3_put(file$datapath, key, ctype), error = function(e) FALSE)
      if (!isTRUE(ok)) { showNotification(sprintf("S3 upload failed: %s", file$name), type="error"); next }
      
      db_add_file(
        sample_id = NA_integer_,
        filename = file$name,
        file_type = "photo",
        file_size = as.integer(file$size),
        storage_path = key,
        mime_type = ctype,
        bucket = s3_bucket,
        session_token = session$token,
        uploader_user_id = u$user_uid,
        version_id = NULL,
        visibility = "private",
        object_key = key
      )
    }
    showNotification("Photo(s) uploaded.", type="message")
    data_changed(data_changed()+1)
  })
  
  observeEvent(input$pdf_upload, {
    req(input$pdf_upload)
    u <- current_user(); if (is.null(u)) { showNotification("Please sign in.", type="error"); return() }
    
    already <- db_files_uploaded_today(u$user_uid)
    if (already >= MAX_FILES_PER_USER_PER_DAY) {
      showNotification("Daily upload limit reached.", type="error"); return()
    }
    
    for (i in seq_len(nrow(input$pdf_upload))) {
      file <- input$pdf_upload[i,]
      if (file$size > MAX_FILE_SIZE_MB*1024^2) {
        showNotification(sprintf("'%s' exceeds %d MB limit", file$name, MAX_FILE_SIZE_MB), type="error")
        next
      }
      key <- sprintf("sessions/%s/pdfs/%s.pdf", session$token, UUIDgenerate())
      ctype <- "application/pdf"
      ok <- tryCatch(s3_put(file$datapath, key, ctype), error = function(e) FALSE)
      if (!isTRUE(ok)) { showNotification(sprintf("S3 upload failed: %s", file$name), type="error"); next }
      
      db_add_file(
        sample_id = NA_integer_,
        filename = file$name,
        file_type = "pdf",
        file_size = as.integer(file$size),
        storage_path = key,
        mime_type = ctype,
        bucket = s3_bucket,
        session_token = session$token,
        uploader_user_id = u$user_uid,
        version_id = NULL,
        visibility = "private",
        object_key = key
      )
    }
    showNotification("PDF(s) uploaded.", type="message")
    data_changed(data_changed()+1)
  })
  
  # --- Files table (with optional presigned View + Delete)
  output$uploaded_files_table <- renderDT({
    data_changed()
    u <- current_user()
    if (is.null(u)) return(NULL)
    
    files <- db_get_files(user_uid = u$user_uid, only_mine = !is_admin())
    if (!nrow(files)) return(NULL)
    
    actions <- if (DOWNLOADS_ENABLED) {
      urls <- vapply(files$object_key, function(k) {
        tryCatch(s3_presign_get(k, 3600), error = function(e) NA_character_)
      }, character(1))
      ifelse(is.na(urls), "(unavailable)",
             sprintf('<a href="%s" target="_blank" class="btn btn-sm btn-info">View</a>', urls))
    } else {
      rep('<span title="Downloads disabled" class="badge bg-secondary">Locked</span>', nrow(files))
    }
    
    del_btns <- sprintf(
      '<button class="btn btn-sm btn-danger delete-file" data-id="%s">Delete</button>',
      files$id
    )
    
    display <- data.frame(
      ID       = files$id,
      Filename = files$filename,
      Type     = files$file_type,
      Size_KB  = round(files$file_size/1024, 1),
      Uploaded = format(as.POSIXct(files$upload_date), "%Y-%m-%d %H:%M"),
      Actions  = actions,
      Delete   = del_btns,
      stringsAsFactors = FALSE
    )
    
    datatable(
      display,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 5,
        dom = 'rtip',
        columnDefs = list(
          list(targets = c(5,6), className = 'dt-center')
        )
      ),
      callback = JS(
        "table.on('click', 'button.delete-file', function() {",
        "  var id = $(this).data('id');",
        "  Shiny.setInputValue('delete_file_id', id, {priority: 'event'});",
        "});"
      )
    )
  })
  
  observeEvent(input$delete_file_id, {
    req(input$delete_file_id)
    u <- current_user(); if (is.null(u)) return()
    res <- db_delete_file(input$delete_file_id, user_uid = u$user_uid, is_admin = is_admin())
    if (!res$ok) {
      showNotification(paste("Delete failed:", res$reason), type="error")
    } else {
      # best-effort S3 delete
      if (!is.null(res$object_key)) s3_delete(res$object_key)
      showNotification("File deleted.", type="message")
      data_changed(data_changed()+1)
    }
  })
  
  # --- Populate species inputs
  observe({
    updateSelectizeInput(session, "species",
                         choices = sort(species_db$taxon_name),
                         selected = "",
                         server = TRUE,
                         options = list(maxItems=1, maxOptions=100, placeholder='Type to search species...')
    )
  })
  
  # --- Refresh analysis species choices on change
  observe({
    data_changed()
    sp <- tryCatch({
      res <- dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")
      if (nrow(res) > 0) res$species else character(0)
    }, error = function(e) character(0))
    
    updateSelectInput(session, "analysis_species",
                      choices = c("Select a species" = "", sp), selected = "")
  })
  
  # --- Analysis outputs
  
  output$summary_stats <- renderTable({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    if (!nrow(dat)) return(data.frame(Measure="No data available", Value=""))
    
    data.frame(
      Measure = c("Number of Samples","Average pH","pH Range",
                  "Average Organic Matter (%)","Average Nitrate (ppm)",
                  "Average Phosphorus (ppm)","Average Potassium (ppm)"),
      Value = c(
        nrow(dat),
        round(mean(dat$ph, na.rm=TRUE), 2),
        paste(round(range(dat$ph, na.rm=TRUE), 2), collapse=" - "),
        round(mean(dat$organic_matter, na.rm=TRUE), 2),
        round(mean(dat$nitrate_ppm, na.rm=TRUE), 2),
        round(mean(dat$phosphorus_ppm, na.rm=TRUE), 2),
        round(mean(dat$potassium_ppm,  na.rm=TRUE), 2)
      )
    )
  })
  
  output$ph_plot <- renderPlot({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    if (!nrow(dat)) return(NULL)
    ggplot(dat, aes(x = ph)) +
      geom_histogram(bins = 15, fill = "skyblue", color = "black") +
      labs(title = paste("pH Distribution for", input$analysis_species),
           x = "pH", y = "Count") +
      theme_minimal()
  })
  
  output$texture_plot <- renderPlot({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
    if (!nrow(dat)) return(NULL)
    p <- ggtern(dat, aes(x = texture_sand, y = texture_silt, z = texture_clay)) +
      geom_point(alpha = 0.6) + theme_bw() + theme_showarrows() +
      labs(title = paste("Soil Texture Distribution for", input$analysis_species)) +
      xlab("Sand (%)") + ylab("Silt (%)") + zlab("Clay (%)") +
      theme(tern.panel.background = element_rect(fill = "aliceblue"),
            tern.panel.grid.major = element_line(color = "white"),
            tern.panel.grid.minor = element_line(color = "white"),
            axis.title = element_text(size = 12, face = "bold")) +
      scale_T_continuous(breaks = seq(0, 100, by = 10)) +
      scale_L_continuous(breaks = seq(0, 100, by = 10)) +
      scale_R_continuous(breaks = seq(0, 100, by = 10))
    print(p)
  }, height = 600, width = 600)
  
  output$nutrient_plot <- renderPlot({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    if (!nrow(dat)) return(NULL)
    nutrient_data <- dat %>%
      select(nitrate_ppm, phosphorus_ppm, potassium_ppm, calcium_ppm, magnesium_ppm) %>%
      pivot_longer(everything(), names_to = "nutrient", values_to = "value",
                   names_pattern = "(.+)_ppm")
    ggplot(nutrient_data, aes(x = nutrient, y = value)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("Nutrient Distribution for", input$analysis_species),
           x = "Nutrient", y = "Concentration (ppm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$ph_om_plot <- renderPlot({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    if (!nrow(dat)) return(NULL)
    ggplot(dat, aes(x = ph, y = organic_matter)) +
      geom_point(aes(color = texture_class), size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      labs(title = paste("pH vs Organic Matter for", input$analysis_species),
           x = "pH", y = "Organic Matter (%)", color = "Texture Class") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.text = element_text(size = 8))
  })
  
  output$heatmap_plot <- renderPlot({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    if (!nrow(dat)) return(NULL)
    numeric_cols <- c("ph","organic_matter","nitrate_ppm","phosphorus_ppm",
                      "potassium_ppm","calcium_ppm","magnesium_ppm","soluble_salts_ppm")
    cor_matrix <- cor(dat[, numeric_cols], use = "complete.obs")
    cor_data <- as.data.frame(as.table(cor_matrix))
    name_mapping <- c("ph"="pH","organic_matter"="Organic","nitrate_ppm"="Nitrate",
                      "phosphorus_ppm"="P","potassium_ppm"="K",
                      "calcium_ppm"="Ca","magnesium_ppm"="Mg","soluble_salts_ppm"="Salts")
    cor_data$Var1 <- name_mapping[as.character(cor_data$Var1)]
    cor_data$Var2 <- name_mapping[as.character(cor_data$Var2)]
    ggplot(cor_data, aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), name="Correlation") +
      geom_text(aes(label = sprintf("%.2f", Freq)), size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = paste("Parameter Correlations for", input$analysis_species))
  })
  
  output$map_plot <- renderPlot({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    if (!nrow(dat)) return(NULL)
    states_map <- map_data("state")
    ggplot() +
      geom_polygon(data = states_map, aes(x = long, y = lat, group = group),
                   fill = "white", color = "gray70", size = 0.2) +
      geom_point(data = dat, aes(x = location_long, y = location_lat),
                 color = "red", size = 3, alpha = 0.6) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      labs(title = paste("Geographic Distribution for", input$analysis_species),
           x = "Longitude", y = "Latitude") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_blank()) +
      xlim(-125, -65) + ylim(25, 50)
  }, height = 500)
  
  output$raw_data <- renderDT({
    req(input$analysis_species)
    dat <- db_get_species_data(input$analysis_species)
    datatable(dat)
  })
}

# Wrap server with Polished
server <- polished::secure_server(server_inner)

# Launch app
shinyApp(ui, server)
