# app.R — Edaphic Flora

# --- Libraries ---------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(ggtern)
library(tidygeocoder)
library(sf)
suppressWarnings({
  # 'ecoregions' may not be installed everywhere; we handle gracefully below
  ok_ecoreg <- requireNamespace("ecoregions", quietly = TRUE)
})
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

# --- Env & options -----------------------------------------------------------
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Auth / Polished + Firebase
polished::global_sessions_config(
  app_name = Sys.getenv("POLISHED_APP_NAME"),
  api_key  = Sys.getenv("POLISHED_API_KEY"),
  # Enable email link & Google
  sign_in_providers = c("email_link", "google"),
  firebase_config = list(
    apiKey       = Sys.getenv("FIREBASE_API_KEY"),
    authDomain   = Sys.getenv("FIREBASE_AUTH_DOMAIN"),
    projectId    = Sys.getenv("FIREBASE_PROJECT_ID")
  )
)

# open in external browser (helps avoid pop-up/cookie issues locally)
options(shiny.launch.browser = TRUE)

# Feature toggles / guards
DOWNLOADS_ENABLED        <- FALSE
MAX_FILE_SIZE_MB         <- as.numeric(Sys.getenv("MAX_FILE_SIZE_MB", "20"))
MAX_FILES_PER_USER_DAY   <- as.integer(Sys.getenv("MAX_FILES_PER_USER_PER_DAY", "200"))

# --- S3 Helpers --------------------------------------------------------------
s3_bucket <- Sys.getenv("S3_BUCKET")

s3_put <- function(local_path, object_key, content_type = NULL) {
  stopifnot(nzchar(s3_bucket))
  put_object(
    file   = local_path,
    object = object_key,
    bucket = s3_bucket,
    headers = c(
      if (!is.null(content_type)) c(`Content-Type` = content_type),
      `x-amz-server-side-encryption` = "AES256"
    )
  )
}

s3_presign_get <- function(object_key, expires = 3600) {
  get_signed_url(object = object_key, bucket = s3_bucket, expires = expires)
}

s3_get_to_temp <- function(object_key) {
  stopifnot(nzchar(s3_bucket))
  tf_ext <- tools::file_ext(object_key)
  tf <- tempfile(fileext = if (nzchar(tf_ext)) paste0(".", tf_ext) else "")
  ok <- tryCatch(
    save_object(object = object_key, bucket = s3_bucket, file = tf),
    error = function(e) e
  )
  if (!isTRUE(ok)) stop("Failed to download '", object_key, "' from S3: ", conditionMessage(ok))
  tf
}

# --- WCVP loader (local or S3; RDS preferred) --------------------------------
read_wcvp_from_path <- function(path) {
  # RDS fast path
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    df <- readRDS(path)
    
  } else {
    # CSV/CSV.GZ path
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rt") else path
    df <- read.csv(
      con, sep="|", quote="",
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
      na.strings=c("","NA"), stringsAsFactors=FALSE
    )
  }
  
  # If already slimmed to 4 columns, respect it; else trim full export
  needed <- c("taxon_name","family","genus","species")
  if (all(needed %in% names(df)) && ncol(df) <= 6L) {
    out <- df[, needed, drop = FALSE]
  } else if ("taxon_rank" %in% names(df)) {
    out <- df |>
      dplyr::filter(taxon_rank == "Species", taxon_status == "Accepted") |>
      dplyr::select(taxon_name, family, genus, species) |>
      dplyr::distinct()
  } else {
    stop("Unrecognized WCVP format. Expecting RDS (4 columns) or full CSV.")
  }
  out
}

load_species_db <- function() {
  # 1) Local candidates
  local_candidates <- c(
    Sys.getenv("WCVP_LOCAL_RDS", unset = "data/wcvp_species_min.rds"),
    Sys.getenv("WCVP_LOCAL_GZ",  unset = "data/wcvp_names.csv.gz"),
    Sys.getenv("WCVP_LOCAL_CSV", unset = "data/wcvp_names.csv")
  )
  for (p in local_candidates) {
    if (nzchar(p) && file.exists(p)) {
      message("Loading WCVP from local file: ", p)
      return(read_wcvp_from_path(p))
    }
  }
  
  # 2) S3 keys
  key_rds <- Sys.getenv("WCVP_RDS_S3_KEY")
  key_csv <- Sys.getenv("WCVP_S3_KEY")
  if (nzchar(key_rds)) {
    message("Downloading WCVP RDS from S3: ", key_rds)
    return(read_wcvp_from_path(s3_get_to_temp(key_rds)))
  }
  if (nzchar(key_csv)) {
    message("Downloading WCVP CSV from S3: ", key_csv)
    return(read_wcvp_from_path(s3_get_to_temp(key_csv)))
  }
  
  stop("Could not load WCVP species db. Set WCVP_RDS_S3_KEY (preferred) or WCVP_S3_KEY, ",
       "or place a local file under data/.")
}

species_db <- load_species_db()

# --- Ecoregions (graceful) ---------------------------------------------------
eco_sf <- NULL
if (ok_ecoreg) {
  data("ContinentalUsEcoregion4", package = "ecoregions", envir = environment())
  eco_sf <- st_as_sf(ContinentalUsEcoregion4) |>
    st_make_valid() |>
    st_transform(4326)
} else {
  warning("Package 'ecoregions' not installed; ecoregion lookup disabled.")
}

# --- Database pool (Neon / Postgres) -----------------------------------------
pool <- dbPool(
  drv = Postgres(),
  dbname   = Sys.getenv("POSTGRES_DB"),
  host     = Sys.getenv("POSTGRES_HOST"),
  port     = Sys.getenv("POSTGRES_PORT"),
  user     = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode  = Sys.getenv("POSTGRES_SSLMODE", unset = "require"),
  minSize  = 1,
  maxSize  = 25
)

onStop(function() poolClose(pool))

# Bootstrap DDL (idempotent)
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
      created_by VARCHAR(255),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )");
  
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS sample_files (
      id SERIAL PRIMARY KEY,
      sample_id INTEGER REFERENCES soil_samples(id),
      filename VARCHAR(255),
      file_type VARCHAR(50),
      file_size INTEGER,
      storage_path VARCHAR(512),
      mime_type VARCHAR(120),
      bucket VARCHAR(120),
      object_key VARCHAR(512),
      session_token VARCHAR(128),
      uploader_user_id VARCHAR(255),
      version_id VARCHAR(255),
      visibility VARCHAR(20) DEFAULT 'private',
      upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )");
  
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_species ON soil_samples(species)");
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_samples_date    ON soil_samples(date)");
}, error = function(e) {
  message("DB bootstrap error: ", e$message)
})

# --- DB helpers --------------------------------------------------------------
db_get_all_samples <- function() {
  tryCatch(
    dbGetQuery(pool, "SELECT * FROM soil_samples ORDER BY created_at DESC"),
    error = function(e) { message("Error fetching samples: ", e$message); data.frame() }
  )
}

db_get_species_data <- function(species) {
  if (is.null(species) || !nzchar(trimws(species))) return(data.frame())
  tryCatch(
    dbGetQuery(pool, "SELECT * FROM soil_samples WHERE species = $1 ORDER BY created_at DESC",
               params = list(species)),
    error = function(e) { message("Error fetching species data: ", e$message); data.frame() }
  )
}

db_get_unique_species <- function() {
  tryCatch(
    dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")$species,
    error = function(e) { message("Error fetching unique species: ", e$message); character() }
  )
}

db_get_files <- function(sample_id = NULL, session_token = NULL) {
  tryCatch({
    if (!is.null(sample_id)) {
      dbGetQuery(pool,
                 "SELECT * FROM sample_files WHERE sample_id = $1 ORDER BY upload_date DESC",
                 params = list(sample_id))
    } else if (!is.null(session_token)) {
      dbGetQuery(pool,
                 "SELECT * FROM sample_files WHERE session_token = $1 ORDER BY upload_date DESC",
                 params = list(session_token))
    } else {
      dbGetQuery(pool, "SELECT * FROM sample_files ORDER BY upload_date DESC LIMIT 200")
    }
  }, error = function(e) {
    message("Error fetching files: ", e$message); data.frame()
  })
}

db_add_file <- function(sample_id, filename, file_type, file_size, storage_path,
                        mime_type = NULL, bucket = Sys.getenv("S3_BUCKET"),
                        session_token = NULL, uploader_user_id = NULL, version_id = NULL,
                        visibility = "private") {
  tryCatch({
    dbExecute(pool, "
      INSERT INTO sample_files
      (sample_id, filename, file_type, file_size, storage_path, mime_type,
       bucket, object_key, session_token, uploader_user_id, version_id, visibility)
      VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)",
              params = list(sample_id, filename, file_type, file_size, storage_path,
                            mime_type, bucket, storage_path, session_token, uploader_user_id,
                            version_id, visibility)
    )
  }, error = function(e) {
    message('Error adding file: ', e$message); return(FALSE)
  })
}

db_add_sample <- function(sample_data) {
  tryCatch({
    if ("date" %in% names(sample_data)) {
      sample_data$date <- as.character(as.Date(sample_data$date))
    }
    fields <- names(sample_data)
    values <- as.list(unname(sample_data))
    placeholders <- paste0("$", seq_along(fields))
    query <- sprintf(
      "INSERT INTO soil_samples (%s) VALUES (%s) RETURNING id",
      paste(fields, collapse = ", "),
      paste(placeholders, collapse = ", ")
    )
    result <- dbGetQuery(pool, query, params = values)
    result$id[1]
  }, error = function(e) {
    message("Error in db_add_sample: ", e$message); NULL
  })
}

# --- Static ref data ---------------------------------------------------------
soil_texture_classes <- data.frame(
  Texture = c(
    "Heavy Clay","Silty Clay","Clay","Silty Clay Loam","Clay Loam",
    "Silt","Silty Loam","Sandy Clay","Loam","Sandy Clay Loam",
    "Sandy Loam","Loamy Sand","Sand"
  ),
  Clay_Min = c(60,40,40,27,27,0,0,35,7,20,0,0,0),
  Clay_Max = c(100,60,60,40,40,12,27,55,27,35,20,15,10),
  Silt_Min = c(0,40,40,40,15,88,74,0,28,20,0,0,0),
  Silt_Max = c(40,60,60,60,52,100,88,20,50,45,50,30,14),
  Sand_Min = c(0,0,0,0,20,0,0,45,23,45,50,70,86),
  Sand_Max = c(45,20,45,20,45,20,50,65,52,80,70,86,100)
)

# --- UI ----------------------------------------------------------------------
base_ui <- page_fluid(
  theme = bs_theme(bootswatch = "minty"),
  card(
    card_header("Edaphic Flora"),
    "Record and analyze soil conditions for different plant species across locations"
  ),
  layout_columns(
    # Input card ---------------------------------------------------------------
    card(
      card_header("Add New Soil Data"),
      div(
        class = "form-group",
        tags$label("Plant Species"),
        selectizeInput("species", NULL,
                       choices = NULL,
                       options = list(maxItems = 1, maxOptions = 100,
                                      placeholder = 'Type to search species...')),
        helpText("Start typing to search for species from the database.")
      ),
      textInput("cultivar", "Cultivar (optional)", ""),
      numericInput("ph", "Soil pH", value = 7.0, min = 0, max = 14, step = 0.1),
      numericInput("organic_matter", "Organic Matter (%)", value = 2, min = 0, max = 100),
      
      h4("Nutrient Levels (ppm)"),
      numericInput("nitrate", "Nitrate", value = 0, min = 0),
      numericInput("ammonium", "Ammonium", value = 0, min = 0),
      numericInput("phosphorus", "Phosphorus", value = 0, min = 0),
      numericInput("potassium", "Potassium", value = 0, min = 0),
      numericInput("calcium", "Calcium", value = 0, min = 0),
      numericInput("magnesium", "Magnesium", value = 0, min = 0),
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
        selectInput("texture_class", "Soil Texture Class",
                    choices = soil_texture_classes$Texture),
        helpText("When using texture class, approximate percentages are used for plotting.")
      ),
      
      h4("Location"),
      textInput("street", "Street Address (optional)", ""),
      textInput("city",   "City/Town", ""),
      selectInput("state","State", choices = state.name, selected = "New York"),
      actionButton("geocode", "Get Coordinates", class = "btn-info"),
      div(style = "margin-top: 10px;", textOutput("geocode_result")),
      div(style = "margin-top: 10px;", textOutput("ecoregion_result")),
      numericInput("latitude",  "Latitude",  value = 0, min = -90,  max = 90),
      numericInput("longitude", "Longitude", value = 0, min = -180, max = 180),
      dateInput("date", "Sample Date", value = Sys.Date()),
      textAreaInput("notes", "Notes", "", height = "100px"),
      actionButton("submit", "Submit Data", class = "btn-primary")
    ),
    
    # Data management ----------------------------------------------------------
    card(
      card_header("Data Management"),
      fileInput("csv_import", "Import CSV Data",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      downloadButton("download_template", "Download CSV Template"),
      downloadButton("export_data", "Export All Data"),
      
      fileInput("photo_upload", "Upload Plant Photo",
                accept = c('image/png','image/jpeg','image/jpg'), multiple = TRUE),
      fileInput("pdf_upload", "Upload Soil Report (PDF)",
                accept = c('application/pdf'), multiple = TRUE),
      DTOutput("uploaded_files_table")
    ),
    
    # Analysis -----------------------------------------------------------------
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

ui <- polished::secure_ui(base_ui)

# --- Server ------------------------------------------------------------------
server_core <- function(input, output, session) {
  data_changed <- reactiveVal(0)
  
  # Current user (from polished)
  current_user <- reactive({
    u <- session$userData$user()
    if (is.null(u)) return(NULL)
    # returns list with email, user_uid, is_admin, etc.
    u
  })
  
  # Template download
  empty_template <- data.frame(
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
  
  output$download_template <- downloadHandler(
    filename = function() "soil_data_template.csv",
    content = function(file) write.csv(empty_template[0,], file, row.names = FALSE)
  )
  
  output$export_data <- downloadHandler(
    filename = function() paste0("soil_data_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) write.csv(db_get_all_samples(), file, row.names = FALSE)
  )
  
  # CSV import
  observeEvent(input$csv_import, {
    req(input$csv_import)
    tryCatch({
      imported <- read.csv(input$csv_import$datapath)
      required_cols <- c("species","ph","organic_matter","nitrate_ppm",
                         "ammonium_ppm","phosphorus_ppm","potassium_ppm",
                         "calcium_ppm","magnesium_ppm","soluble_salts_ppm",
                         "texture_sand","texture_silt","texture_clay",
                         "texture_class","location_lat","location_long",
                         "date","ecoregion_l4","ecoregion_l4_code")
      missing_cols <- setdiff(required_cols, names(imported))
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing required columns:", paste(missing_cols, collapse = ", ")), type = "error")
        return()
      }
      invalid_species <- setdiff(imported$species, species_db$taxon_name)
      if (length(invalid_species) > 0) {
        showNotification(
          paste("Invalid species found:", paste(invalid_species, collapse = ", "),
                "\nPlease ensure all species match the database."),
          type = "error"
        )
        return()
      }
      for (i in seq_len(nrow(imported))) db_add_sample(imported[i,])
      showNotification("CSV data imported successfully!", type = "message")
      data_changed(data_changed() + 1)
    }, error = function(e) {
      showNotification(paste("Error importing CSV:", e$message), type = "error")
    })
  })
  
  # ---- Uploads → S3 ---------------------------------------------------------
  check_files_quota <- function(n) {
    if (n > MAX_FILES_PER_USER_DAY) {
      showNotification("Upload limit reached for today.", type = "error")
      return(FALSE)
    }
    TRUE
  }
  
  observeEvent(input$photo_upload, {
    req(input$photo_upload)
    # crude per-event size guard
    too_big <- input$photo_upload$size > (MAX_FILE_SIZE_MB * 1024^2)
    if (any(too_big)) {
      showNotification("One or more files exceed size limit.", type = "error")
      return()
    }
    
    usr <- current_user()
    uid <- if (!is.null(usr)) usr$user_uid else NA_character_
    
    for (i in seq_len(nrow(input$photo_upload))) {
      file <- input$photo_upload[i,]
      ext  <- tools::file_ext(file$name)
      key  <- sprintf("sessions/%s/photos/%s.%s",
                      session$token, UUIDgenerate(), ext %||% "jpg")
      ctype <- mime::guess_type(file$name) %||% "image/jpeg"
      
      # Upload to S3
      ok <- tryCatch(s3_put(file$datapath, key, content_type = ctype), error = function(e) e)
      if (!isTRUE(ok)) {
        showNotification(sprintf("Error uploading %s", file$name), type = "error")
      } else {
        db_add_file(
          sample_id = NULL,
          filename  = file$name,
          file_type = "photo",
          file_size = file$size,
          storage_path = key,
          mime_type    = ctype,
          session_token = session$token,
          uploader_user_id = uid,
          visibility = "private"
        )
        showNotification(sprintf("Uploaded: %s", file$name), type = "message")
      }
    }
    data_changed(data_changed() + 1)
  })
  
  observeEvent(input$pdf_upload, {
    req(input$pdf_upload)
    too_big <- input$pdf_upload$size > (MAX_FILE_SIZE_MB * 1024^2)
    if (any(too_big)) {
      showNotification("One or more PDFs exceed size limit.", type = "error")
      return()
    }
    
    usr <- current_user()
    uid <- if (!is.null(usr)) usr$user_uid else NA_character_
    
    for (i in seq_len(nrow(input$pdf_upload))) {
      file <- input$pdf_upload[i,]
      key  <- sprintf("sessions/%s/pdfs/%s.pdf", session$token, UUIDgenerate())
      ok <- tryCatch(s3_put(file$datapath, key, content_type = "application/pdf"), error = function(e) e)
      if (!isTRUE(ok)) {
        showNotification(sprintf("Error uploading %s", file$name), type = "error")
      } else {
        db_add_file(
          sample_id = NULL,
          filename  = file$name,
          file_type = "pdf",
          file_size = file$size,
          storage_path = key,
          mime_type    = "application/pdf",
          session_token = session$token,
          uploader_user_id = uid,
          visibility = "private"
        )
        showNotification(sprintf("Uploaded: %s", file$name), type = "message")
      }
    }
    data_changed(data_changed() + 1)
  })
  
  # Table of uploaded files (session-scoped)
  output$uploaded_files_table <- renderDT({
    data_changed()
    files_data <- db_get_files(session_token = session$token)
    if (nrow(files_data) == 0) return(NULL)
    
    Actions <- if (DOWNLOADS_ENABLED) {
      urls <- vapply(files_data$storage_path, function(k) {
        tryCatch(s3_presign_get(k, 3600), error = function(e) NA_character_)
      }, character(1))
      ifelse(is.na(urls), "(unavailable)",
             sprintf('<a href="%s" target="_blank" class="btn btn-sm btn-info">View</a>', urls))
    } else {
      rep('<span title="Downloads disabled" class="badge bg-secondary">Locked</span>', nrow(files_data))
    }
    
    display_data <- data.frame(
      Filename = files_data$filename,
      Type     = files_data$file_type,
      Size     = paste0(round(files_data$file_size / 1024, 1), " KB"),
      Uploaded = files_data$upload_date,
      Actions  = Actions,
      stringsAsFactors = FALSE
    )
    
    datatable(display_data, escape = FALSE,
              options = list(pageLength = 5, dom = 'rtip',
                             columnDefs = list(list(targets = 4, className = 'dt-center'))),
              selection = 'none')
  })
  
  # Helper: texture midpoints
  get_texture_percentages <- function(texture_class) {
    class_data <- soil_texture_classes[soil_texture_classes$Texture == texture_class, ]
    if (nrow(class_data) == 0) return(NULL)
    sand_mid <- mean(c(class_data$Sand_Min, class_data$Sand_Max))
    silt_mid <- mean(c(class_data$Silt_Min, class_data$Silt_Max))
    clay_mid <- mean(c(class_data$Clay_Min, class_data$Clay_Max))
    total <- sand_mid + silt_mid + clay_mid
    list(
      sand = sand_mid * (100/total),
      silt = silt_mid * (100/total),
      clay = clay_mid * (100/total)
    )
  }
  
  # Ecoregion lookup
  get_ecoregion <- function(lat, long) {
    if (is.null(lat) || is.null(long) || is.na(lat) || is.na(long) || is.null(eco_sf)) {
      return(list(name = NA_character_, code = NA_character_))
    }
    point_sf <- st_as_sf(data.frame(long = long, lat = lat), coords = c("long","lat"), crs = 4326)
    intersects <- st_intersects(eco_sf, point_sf, sparse = FALSE)
    if (any(intersects)) {
      idx <- which(intersects)[1]
      list(name = as.character(eco_sf$us_l4name[idx]),
           code = as.character(eco_sf$us_l4code[idx]))
    } else {
      list(name = NA_character_, code = NA_character_)
    }
  }
  
  # Reactive texture class
  texture_class <- reactive({
    req(input$texture_input_type == "pct", input$sand, input$silt, input$clay)
    if (abs((input$sand + input$silt + input$clay) - 100) > 0.1) return("Error: Percentages must sum to 100%")
    for (i in seq_len(nrow(soil_texture_classes))) {
      if (input$clay >= (soil_texture_classes$Clay_Min[i] - 0.1) &&
          input$clay <= (soil_texture_classes$Clay_Max[i] + 0.1) &&
          input$silt >= (soil_texture_classes$Silt_Min[i] - 0.1) &&
          input$silt <= (soil_texture_classes$Silt_Max[i] + 0.1) &&
          input$sand >= (soil_texture_classes$Sand_Min[i] - 0.1) &&
          input$sand <= (soil_texture_classes$Sand_Max[i] + 0.1)) {
        return(as.character(soil_texture_classes$Texture[i]))
      }
    }
    "Unclassified"
  })
  output$texture_class_result <- renderText({ paste("Calculated Texture Class:", texture_class()) })
  
  # Geocoding
  observeEvent(input$geocode, {
    req(input$city, input$state)
    address <- paste(
      if (!is.null(input$street) && input$street != "") paste(input$street, ",") else "",
      input$city, ",", input$state
    )
    output$geocode_result  <- renderText("")
    output$ecoregion_result <- renderText("")
    result <- tryCatch(geo(address = address, method = "osm"), error = function(e) NULL)
    if (!is.null(result) && nrow(result) > 0 && !is.na(result$lat[1]) && !is.na(result$long[1])) {
      updateNumericInput(session, "latitude",  value = result$lat[1])
      updateNumericInput(session, "longitude", value = result$long[1])
      output$geocode_result <- renderText({
        paste("Successfully found coordinates:", round(result$lat[1], 4), ",", round(result$long[1], 4))
      })
      eco <- get_ecoregion(result$lat[1], result$long[1])
      if (!is.na(eco$name)) {
        output$ecoregion_result <- renderText({
          ecocode <- if (!is.na(eco$code)) paste0(" (", eco$code, ")") else ""
          paste("Ecoregion:", eco$name, ecocode)
        })
      } else {
        output$ecoregion_result <- renderText("Could not determine ecoregion for these coordinates")
      }
    } else {
      output$geocode_result <- renderText("Could not find coordinates; check the address or enter coordinates manually.")
    }
  })
  
  # Submit sample
  observeEvent(input$submit, {
    if (input$texture_input_type == "pct") {
      if (abs((input$sand + input$silt + input$clay) - 100) > 0.1) {
        showNotification("Soil texture percentages must sum to 100%", type = "error"); return()
      }
    }
    
    eco <- get_ecoregion(input$latitude, input$longitude)
    if (input$texture_input_type == "class") {
      texture_pcts <- get_texture_percentages(input$texture_class)
    }
    
    usr <- current_user()
    created_by <- if (!is.null(usr)) usr$email else NA_character_
    
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
      created_by = created_by,
      stringsAsFactors = FALSE
    )
    
    sample_id <- db_add_sample(new_data)
    if (!is.null(sample_id)) {
      dbExecute(pool, "
        UPDATE sample_files
           SET sample_id = $1
         WHERE session_token = $2
           AND sample_id IS NULL",
                params = list(sample_id, session$token)
      )
      showNotification("Data added successfully!", type = "message")
      data_changed(data_changed() + 1)
    } else {
      showNotification("Error adding data", type = "error")
    }
  })
  
  # Populate species choices
  observe({
    species_choices <- species_db |>
      pull(taxon_name) |>
      sort()
    updateSelectizeInput(session, "species",
                         choices = species_choices,
                         selected = "",
                         server = TRUE,
                         options = list(maxItems = 1, maxOptions = 100,
                                        placeholder = 'Type to search species...'))
  })
  
  # Update analysis species choices
  observe({
    data_changed()
    species_list <- tryCatch({
      res <- dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")
      if (nrow(res) > 0) res$species else character(0)
    }, error = function(e) { message("Error fetching species list: ", e$message); character(0) })
    updateSelectInput(session, "analysis_species",
                      choices = c("Select a species" = "", species_list),
                      selected = "")
  })
  
  # Summary stats
  output$summary_stats <- renderTable({
    req(nzchar(input$analysis_species))
    species_data <- db_get_species_data(input$analysis_species)
    if (nrow(species_data) == 0) return(data.frame(Measure = "No data available", Value = ""))
    data.frame(
      Measure = c("Number of Samples", "Average pH", "pH Range",
                  "Average Organic Matter (%)", "Average Nitrate (ppm)",
                  "Average Phosphorus (ppm)", "Average Potassium (ppm)"),
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
  
  # Plots
  output$ph_plot <- renderPlot({
    req(nzchar(input$analysis_species))
    species_data <- db_get_species_data(input$analysis_species)
    if (nrow(species_data) == 0) return(NULL)
    ggplot(species_data, aes(x = ph)) +
      geom_histogram(bins = 15, fill = "skyblue", color = "black") +
      labs(title = paste("pH Distribution for", input$analysis_species),
           x = "pH", y = "Count") +
      theme_minimal()
  })
  
  output$texture_plot <- renderPlot({
    req(nzchar(input$analysis_species))
    species_data <- db_get_species_data(input$analysis_species)
    species_data <- species_data[!is.na(species_data$texture_sand) &
                                   !is.na(species_data$texture_silt) &
                                   !is.na(species_data$texture_clay), ]
    if (nrow(species_data) == 0) return(NULL)
    ggtern(species_data, aes(x = texture_sand, y = texture_silt, z = texture_clay)) +
      geom_point(alpha = 0.6) +
      theme_bw() + theme_showarrows() +
      labs(title = paste("Soil Texture Distribution for", input$analysis_species)) +
      xlab("Sand (%)") + ylab("Silt (%)") + zlab("Clay (%)") +
      theme(
        tern.panel.background = element_rect(fill = "aliceblue"),
        tern.panel.grid.major = element_line(color = "white"),
        tern.panel.grid.minor = element_line(color = "white"),
        axis.title = element_text(size = 12, face = "bold")
      ) +
      scale_T_continuous(breaks = seq(0, 100, by = 10)) +
      scale_L_continuous(breaks = seq(0, 100, by = 10)) +
      scale_R_continuous(breaks = seq(0, 100, by = 10))
  }, height = 600, width = 600)
  
  output$nutrient_plot <- renderPlot({
    req(nzchar(input$analysis_species))
    species_data <- db_get_species_data(input$analysis_species)
    if (nrow(species_data) == 0) return(NULL)
    nutrient_data <- species_data |>
      select(nitrate_ppm, phosphorus_ppm, potassium_ppm, calcium_ppm, magnesium_ppm) |>
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
    req(nzchar(input$analysis_species))
    species_data <- db_get_species_data(input$analysis_species)
    if (nrow(species_data) == 0) return(NULL)
    ggplot(species_data, aes(x = ph, y = organic_matter)) +
      geom_point(aes(color = texture_class), size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      labs(title = paste("pH vs Organic Matter for", input$analysis_species),
           x = "pH", y = "Organic Matter (%)",
           color = "Texture Class") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.text = element_text(size = 8))
  })
  
  output$heatmap_plot <- renderPlot({
    req(nzchar(input$analysis_species))
    species_data <- db_get_species_data(input$analysis_species)
    if (nrow(species_data) == 0) return(NULL)
    numeric_cols <- c("ph","organic_matter","nitrate_ppm","phosphorus_ppm",
                      "potassium_ppm","calcium_ppm","magnesium_ppm","soluble_salts_ppm")
    cor_matrix <- cor(species_data[, numeric_cols], use = "complete.obs")
    cor_data <- as.data.frame(as.table(cor_matrix))
    name_mapping <- c("ph"="pH","organic_matter"="Organic","nitrate_ppm"="Nitrate",
                      "phosphorus_ppm"="P","potassium_ppm"="K","calcium_ppm"="Ca",
                      "magnesium_ppm"="Mg","soluble_salts_ppm"="Salts")
    cor_data$Var1 <- name_mapping[as.character(cor_data$Var1)]
    cor_data$Var2 <- name_mapping[as.character(cor_data$Var2)]
    ggplot(cor_data, aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1,1), name = "Correlation") +
      geom_text(aes(label = sprintf("%.2f", Freq)), size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = paste("Parameter Correlations for", input$analysis_species))
  })
  
  output$map_plot <- renderPlot({
    req(nzchar(input$analysis_species))
    species_data <- db_get_species_data(input$analysis_species)
    if (nrow(species_data) == 0) return(NULL)
    states_map <- map_data("state")
    ggplot() +
      geom_polygon(data = states_map, aes(x = long, y = lat, group = group),
                   fill = "white", color = "gray70", size = 0.2) +
      geom_point(data = species_data,
                 aes(x = location_long, y = location_lat),
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
    req(nzchar(input$analysis_species))
    datatable(db_get_species_data(input$analysis_species))
  })
}

server <- polished::secure_server(server_core)

# --- Run ---------------------------------------------------------------------
shinyApp(ui, server)
