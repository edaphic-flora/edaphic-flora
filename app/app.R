# ---------------------------
# Edaphic Flora - Shiny App
# ---------------------------

# --- Dev settings
if (interactive()) {
 options(shiny.launch.browser = TRUE)
 options(shiny.host = "127.0.0.1")
 options(shiny.port = 7420)
}

# --- Load environment
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
library(maps)
library(mapdata)
library(polished)
library(plotly)
library(leaflet)
library(shinycssloaders)

# --- Load modules
source("R/db.R")
source("R/data.R")
source("R/helpers.R")
source("R/theme.R")
source("R/usda.R")
source("R/pdf_extract.R")
source("R/mod_help.R")
source("R/mod_welcome.R")
source("R/mod_admin.R")
source("R/mod_data_management.R")
source("R/mod_find_plants.R")
source("R/mod_data_entry.R")
source("R/mod_analysis.R")

# --- Initialize
db_migrate()
onStop(function() poolClose(pool))

species_db <- load_species_db()
zipcode_db <- tryCatch(load_zipcode_db(), error = function(e) NULL)

# Ecoregions: use full shapefile in dev, lightweight grid lookup in prod
is_prod <- Sys.getenv("ENV", "dev") != "dev"
eco_sf <- NULL
eco_grid <- NULL

if (is_prod) {
  # Production: use pre-computed grid (~2MB vs ~95MB)
  eco_grid <- tryCatch({
    load_ecoregion_grid()
  }, error = function(e) {
    message("Warning: Could not load ecoregion grid: ", e$message)
    NULL
  })
} else {
  # Development: use full shapefile for accuracy
  eco_sf <- tryCatch({
    load_ecoregions()
  }, error = function(e) {
    message("Warning: Could not load ecoregions data: ", e$message)
    NULL
  })
}

# Unified ecoregion lookup function
lookup_ecoregion <- function(lat, lon) {
  if (!is.null(eco_sf)) {
    get_ecoregion(lat, lon, eco_sf)
  } else if (!is.null(eco_grid)) {
    get_ecoregion_from_grid(lat, lon, eco_grid)
  } else {
    list(name = NA_character_, code = NA_character_)
  }
}

# State grid for native status lookups (dev only - data not granular enough yet)
state_grid <- NULL
if (!is_prod) {
  state_grid <- tryCatch({
    load_state_grid()
  }, error = function(e) {
    message("Warning: Could not load state grid: ", e$message)
    NULL
  })
}

# --- Auth config
firebase_cfg <- list(
 apiKey     = Sys.getenv("FIREBASE_API_KEY"),
 authDomain = Sys.getenv("FIREBASE_AUTH_DOMAIN"),
 projectId  = Sys.getenv("FIREBASE_PROJECT_ID")
)

polished::polished_config(
 app_name  = Sys.getenv("POLISHED_APP_NAME"),
 api_key   = Sys.getenv("POLISHED_API_KEY"),
 firebase_config   = firebase_cfg,
 sign_in_providers = c("google", "email"),
 is_invite_required = TRUE,
 is_email_verification_required = FALSE
)

# --- Environment detection
app_env <- Sys.getenv("ENV", "prod")  # Default to prod for safety
is_dev <- (app_env == "dev")

# --- Admin configuration
# Set ADMIN_EMAILS env var as comma-separated list, e.g., "admin@example.com,owner@example.com"
admin_emails <- strsplit(Sys.getenv("ADMIN_EMAILS", ""), ",")[[1]]
admin_emails <- trimws(admin_emails[nzchar(admin_emails)])

is_admin_user <- function(user_email) {
  if (is.null(user_email) || !nzchar(user_email)) return(FALSE)
  tolower(user_email) %in% tolower(admin_emails)
}

# ---------------------------
# UI
# ---------------------------

base_ui <- page_navbar(
 id = "main_nav",
 title = span(
   class = "brand-name",
   HTML('<svg width="36" height="28" viewBox="170 0 250 280" style="vertical-align: middle; margin-right: 8px;">
     <path d="M233.399 149.972L237.399 149.972L237.399 270.114L233.399 270.114L233.399 149.972Z" fill="#7A9A86"/>
     <path d="M280.485 171.514C280.485 183.114 270.542 208.8 237.399 206.314C237.399 184.772 251.485 171.514 280.485 171.514Z" fill="#7A9A86"/>
     <path d="M280.485 217.086C280.485 228.686 270.542 254.372 237.399 251.886C237.399 230.343 251.485 217.086 280.485 217.086Z" fill="#7A9A86"/>
     <path d="M234.085 252.054C234.085 240.454 224.142 214.768 190.999 217.254C190.999 238.797 205.085 252.054 234.085 252.054Z" fill="#7A9A86"/>
     <path d="M234.085 206.483C234.085 194.883 224.142 169.197 190.999 171.683C190.999 193.226 205.085 206.483 234.085 206.483Z" fill="#7A9A86"/>
     <path d="M293.742 91.9716C293.742 124.004 267.775 149.972 235.742 149.972C203.71 149.972 177.742 124.004 177.742 91.9716H293.742Z" fill="#7A9A86"/>
     <path d="M198.457 87.8286C198.457 81.8797 203.094 77.0572 208.814 77.0572C214.534 77.0572 219.171 81.8797 219.171 87.8286L198.457 87.8286Z" fill="#7A9A86"/>
     <path d="M224.97 87.8286C224.97 81.8797 229.607 77.0572 235.327 77.0572C241.048 77.0572 245.685 81.8797 245.685 87.8286L224.97 87.8286Z" fill="#7A9A86"/>
     <path d="M251.485 87.8286C251.485 81.8797 256.122 77.0572 261.842 77.0572C267.562 77.0572 272.199 81.8797 272.199 87.8286L251.485 87.8286Z" fill="#7A9A86"/>
     <path d="M349.742 70.4288L353.742 70.4288L353.742 270.114L349.742 270.114L349.742 70.4288Z" fill="#7A9A86"/>
     <path d="M396.485 171.514C396.485 183.114 386.542 208.8 353.399 206.314C353.399 184.772 367.485 171.514 396.485 171.514Z" fill="#7A9A86"/>
     <path d="M396.485 125.943C396.485 137.543 386.542 163.229 353.399 160.743C353.399 139.2 367.485 125.943 396.485 125.943Z" fill="#7A9A86"/>
     <path d="M396.485 80.3716C396.485 91.9716 386.542 117.657 353.399 115.172C353.399 93.6287 367.485 80.3716 396.485 80.3716Z" fill="#7A9A86"/>
     <path d="M396.485 217.086C396.485 228.686 386.542 254.372 353.399 251.886C353.399 230.343 367.485 217.086 396.485 217.086Z" fill="#7A9A86"/>
     <path d="M350.085 252.054C350.085 240.454 340.142 214.768 306.999 217.254C306.999 238.797 321.085 252.054 350.085 252.054Z" fill="#7A9A86"/>
     <path d="M350.085 206.483C350.085 194.883 340.142 169.197 306.999 171.683C306.999 193.226 321.085 206.483 350.085 206.483Z" fill="#7A9A86"/>
     <path d="M350.085 160.911C350.085 149.311 340.142 123.626 306.999 126.111C306.999 147.654 321.085 160.911 350.085 160.911Z" fill="#7A9A86"/>
     <path d="M350.085 115.34C350.085 103.74 340.142 78.0541 306.999 80.5398C306.999 102.083 321.085 115.34 350.085 115.34Z" fill="#7A9A86"/>
     <path d="M409.742 14.9144C409.742 46.9469 383.775 72.9144 351.742 72.9144C319.71 72.9144 293.742 46.9469 293.742 14.9144H409.742Z" fill="#7A9A86"/>
     <path d="M314.457 10.7715C314.457 4.82259 319.094 5.6113e-05 324.814 5.6613e-05C330.534 5.71131e-05 335.171 4.82259 335.171 10.7715L314.457 10.7715Z" fill="#7A9A86"/>
     <path d="M340.97 10.7715C340.97 4.82259 345.607 5.6113e-05 351.327 5.6613e-05C357.048 5.71131e-05 361.685 4.82259 361.685 10.7715L340.97 10.7715Z" fill="#7A9A86"/>
     <path d="M367.485 10.7715C367.485 4.82259 372.122 5.6113e-05 377.842 5.6613e-05C383.562 5.71131e-05 388.199 4.82259 388.199 10.7715L367.485 10.7715Z" fill="#7A9A86"/>
   </svg>'),
   span(class = "brand-name-edaphic", "edaphic"),
   " ",
   span(class = "brand-name-flora", "flora")
 ),
 theme = edaphic_bs_theme(),
 fillable = TRUE,

 header = tagList(
   edaphic_css(),
   tags$head(
     tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
     tags$link(rel = "stylesheet",
               href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
     tags$link(rel = "stylesheet",
               href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@400;500;600;700&display=swap")
   )
 ),

 # ========== WELCOME TAB ==========
 welcomeUI("welcome"),

 # ========== DATA ENTRY TAB ==========
 dataEntryUI("data_entry"),

 # ========== ANALYSIS TAB ==========
 analysisUI("analysis"),

# ========== FIND PLANTS TAB ==========
findPlantsUI("find_plants"),

 # ========== DATA MANAGEMENT TAB ==========
 dataManagementUI("data_mgmt"),

 # ========== ADMIN TAB ==========
 adminUI("admin"),

 # ========== HELP MENU ==========
 helpUI("help"),

 # ========== NAV SPACER & USER INFO ==========
 nav_spacer(),
 # Dev environment indicator
 if (is_dev) nav_item(
   tags$span(class = "badge bg-warning text-dark me-2", "DEV")
 ),
 nav_item(
   tags$span(class = "navbar-text me-3", textOutput("user_display", inline = TRUE))
 )
)

# Edit Modal
edit_modal_ui <- modalDialog(
  title = span(icon("edit"), "Edit Entry"),
  size = "l",
  easyClose = FALSE,
  footer = tagList(
    modalButton("Cancel"),
    actionButton("save_edit", "Save Changes", class = "btn-primary")
  ),

  # Hidden field for entry ID (using div with display:none)
  div(style = "display: none;", numericInput("edit_id", "ID", value = NA)),

  layout_columns(
    col_widths = c(6, 6),

    # Left column
    div(
      selectizeInput("edit_species", "Species", choices = NULL),
      textInput("edit_cultivar", "Cultivar", ""),
      selectInput("edit_outcome", "Outcome",
                  choices = c("Select..." = "", "Thriving", "Established", "Struggling", "Failed/Died")),
      selectInput("edit_sun_exposure", "Sun Exposure",
                  choices = c("Select..." = "", "Full Sun", "Part Sun", "Part Shade", "Full Shade")),
      selectInput("edit_site_hydrology", "Site Hydrology",
                  choices = c("Select..." = "", "Dry", "Mesic", "Wet")),
      numericInput("edit_ph", "pH", value = NA, min = 0, max = 14, step = 0.1),
      numericInput("edit_organic_matter", "Organic Matter (%)", value = NA, min = 0, max = 100, step = 0.1),
      selectInput("edit_organic_matter_class", "OM Class (Qualitative)",
                  choices = c("Select..." = "", "Very Low", "Low", "Medium Low",
                              "Medium", "Medium High", "High", "Very High"))
    ),

    # Right column
    div(
      numericInput("edit_nitrate", "Nitrate (ppm)", value = NA, min = 0),
      numericInput("edit_phosphorus", "Phosphorus (ppm)", value = NA, min = 0),
      numericInput("edit_potassium", "Potassium (ppm)", value = NA, min = 0),
      numericInput("edit_calcium", "Calcium (ppm)", value = NA, min = 0),
      numericInput("edit_magnesium", "Magnesium (ppm)", value = NA, min = 0),
      dateInput("edit_date", "Sample Date"),
      textAreaInput("edit_notes", "Notes", "", height = "80px")
    )
  )
)

# Delete Confirmation Modal
delete_modal_ui <- modalDialog(
  title = span(icon("exclamation-triangle"), "Confirm Delete"),
  size = "s",
  easyClose = FALSE,
  footer = tagList(
    modalButton("Cancel"),
    actionButton("confirm_delete", "Delete", class = "btn-danger")
  ),
  div(style = "display: none;", numericInput("delete_id", "ID", value = NA)),
  p("Are you sure you want to delete this entry?"),
  p(class = "text-muted", "This action cannot be undone.")
)

# Custom branded sign-in page
custom_sign_in_ui <- tagList(
  tags$head(
    tags$style(HTML("
      .sign-in-container {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #f5f5dc 0%, #e8e4d4 100%);
        padding: 20px;
      }
      .sign-in-logo {
        max-width: 200px;
        margin-bottom: 20px;
      }
      .sign-in-welcome {
        color: #2c3e50;
        font-size: 1.5rem;
        margin-bottom: 10px;
        font-weight: 600;
      }
      .sign-in-tagline {
        color: #666;
        font-size: 1rem;
        margin-bottom: 30px;
      }
      .sign-in-box {
        background: white;
        padding: 30px;
        border-radius: 8px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        max-width: 400px;
        width: 100%;
      }
      .sign-in-box h1 {
        color: #27ae60 !important;
        font-size: 1.75rem;
      }
      .sign-in-footer {
        margin-top: 30px;
        color: #888;
        font-size: 0.85rem;
      }
      .sign-in-footer a {
        color: #27ae60;
        text-decoration: none;
      }
      .sign-in-footer a:hover {
        text-decoration: underline;
      }
    "))
  ),
  div(class = "sign-in-container",
    img(src = "logo.svg", class = "sign-in-logo", alt = "edaphic flora"),
    h2(class = "sign-in-welcome", "Welcome to edaphic flora"),
    p(class = "sign-in-tagline", "Record and analyze soil conditions for plant species"),
    div(class = "sign-in-box",
      polished::sign_in_ui_default(
        color = "#27ae60",
        company_name = "edaphic flora",
        logo_top = NULL,
        logo_bottom = NULL
      )
    ),
    div(class = "sign-in-footer",
      a(href = "privacy.html", target = "_blank", "Privacy Policy"),
      span(" | "),
      a(href = "terms.html", target = "_blank", "Terms of Service")
    )
  )
)

ui <- polished::secure_ui(
  base_ui,
  sign_in_page_ui = custom_sign_in_ui
)

# ---------------------------
# Server
# ---------------------------

server_inner <- function(input, output, session) {

 # --- User & State ---
 current_user <- reactive({
   f <- session$userData$user
   if (is.null(f)) return(NULL)
   f()
 })

 output$user_display <- renderText({
   u <- current_user()
   if (is.null(u)) return("")
   u$email
 })

 # --- Admin check ---
 is_admin <- reactive({
   u <- current_user()
   if (is.null(u)) return(FALSE)
   is_admin_user(u$email)
 })

 data_changed <- reactiveVal(0)

 # --- Module servers ---
 helpServer("help")
 welcomeServer("welcome", pool, data_changed)
 adminServer("admin", pool, is_admin, current_user, data_changed)
 dataManagementServer("data_mgmt", pool, current_user, data_changed, soil_data_template)

 # --- Find Plants module ---
 pdf_extract_limit <- as.integer(Sys.getenv("PDF_EXTRACT_DAILY_LIMIT", "5"))
 find_plants_faq <- findPlantsServer("find_plants", pool, current_user, is_admin, data_changed,
                                      pdf_extract_limit)

 # Handle FAQ link from Find Plants module
 observeEvent(find_plants_faq(), {
   nav_select("main_nav", "FAQ")
 }, ignoreInit = TRUE)

 # --- Data Entry module ---
 dataEntryServer("data_entry", pool, species_db, zipcode_db, soil_texture_classes,
                 current_user, is_admin, data_changed, lookup_ecoregion, pdf_extract_limit)

 # --- Analysis module ---
analysisServer("analysis", pool, data_changed, state_grid, is_prod,
               edaphic_colors, theme_edaphic, scale_color_edaphic, scale_fill_edaphic)

# Help links from welcome page - navigate to Field Guide
observeEvent(input$help_link_soil, {
   nav_select("main_nav", "Field Guide")
 })
 observeEvent(input$help_link_nutrients, {
   nav_select("main_nav", "Field Guide")
 })
 observeEvent(input$help_link_performance, {
   nav_select("main_nav", "Field Guide")
 })

 # ---------------------------
 # Edit/Delete Handlers
 # ---------------------------

 # Handle edit button click
 observeEvent(input$edit_entry, {
   entry_id <- input$edit_entry
   u <- current_user()
   admin_user <- is_admin()
   if (is.null(u)) {
     showNotification("Please sign in to edit entries.", type = "error")
     return()
   }

   # Fetch the entry
   entry <- db_get_sample_by_id(entry_id)
   if (nrow(entry) == 0) {
     showNotification("Entry not found.", type = "error")
     return()
   }

   # Verify ownership (unless admin)
   is_owner <- !is.na(entry$created_by[1]) && entry$created_by[1] == u$user_uid
   if (!is_owner && !admin_user) {
     showNotification("You can only edit your own entries.", type = "error")
     return()
   }

   # Populate modal fields
   updateNumericInput(session, "edit_id", value = entry$id[1])
   # For server-side selectize, must provide choices with selected value
   updateSelectizeInput(session, "edit_species",
                        choices = sort(species_db$taxon_name),
                        selected = entry$species[1],
                        server = TRUE)
   updateTextInput(session, "edit_cultivar", value = entry$cultivar[1] %||% "")
   updateSelectInput(session, "edit_outcome", selected = entry$outcome[1] %||% "")
   updateSelectInput(session, "edit_sun_exposure", selected = entry$sun_exposure[1] %||% "")
   updateSelectInput(session, "edit_site_hydrology", selected = entry$site_hydrology[1] %||% "")
   updateNumericInput(session, "edit_ph", value = entry$ph[1])
   updateNumericInput(session, "edit_organic_matter", value = entry$organic_matter[1])
   updateSelectInput(session, "edit_organic_matter_class", selected = entry$organic_matter_class[1] %||% "")
   updateNumericInput(session, "edit_nitrate", value = entry$nitrate_ppm[1])
   updateNumericInput(session, "edit_phosphorus", value = entry$phosphorus_ppm[1])
   updateNumericInput(session, "edit_potassium", value = entry$potassium_ppm[1])
   updateNumericInput(session, "edit_calcium", value = entry$calcium_ppm[1])
   updateNumericInput(session, "edit_magnesium", value = entry$magnesium_ppm[1])
   updateDateInput(session, "edit_date", value = entry$date[1])
   updateTextAreaInput(session, "edit_notes", value = entry$notes[1] %||% "")

   showModal(edit_modal_ui)
 })

 # Handle save edit
 observeEvent(input$save_edit, {
   u <- current_user()
   if (is.null(u)) {
     showNotification("Please sign in.", type = "error")
     return()
   }

   entry_id <- input$edit_id
   if (is.null(entry_id) || is.na(entry_id)) {
     showNotification("Invalid entry.", type = "error")
     return()
   }

   # Build update data
   update_data <- list(
     species = input$edit_species,
     cultivar = if (nzchar(input$edit_cultivar)) input$edit_cultivar else NA,
     outcome = if (nzchar(input$edit_outcome)) input$edit_outcome else NA,
     sun_exposure = if (nzchar(input$edit_sun_exposure)) input$edit_sun_exposure else NA,
     site_hydrology = if (nzchar(input$edit_site_hydrology)) input$edit_site_hydrology else NA,
     ph = input$edit_ph,
     organic_matter = input$edit_organic_matter,
     organic_matter_class = if (nzchar(input$edit_organic_matter_class)) input$edit_organic_matter_class else NA,
     nitrate_ppm = input$edit_nitrate,
     phosphorus_ppm = input$edit_phosphorus,
     potassium_ppm = input$edit_potassium,
     calcium_ppm = input$edit_calcium,
     magnesium_ppm = input$edit_magnesium,
     date = input$edit_date,
     notes = if (nzchar(input$edit_notes)) input$edit_notes else NA
   )

   # Attempt update (pass admin flag)
   success <- db_update_sample(entry_id, update_data, u$user_uid, is_admin = is_admin())

   if (success) {
     db_audit_log("update", "soil_samples", entry_id, u$user_uid, "entry updated")
     removeModal()
     showNotification("Entry updated successfully!", type = "message")
     data_changed(data_changed() + 1)
   } else {
     showNotification("Failed to update entry. You may not have permission.", type = "error")
   }
 })

 # Handle delete button click
 observeEvent(input$delete_entry, {
   entry_id <- input$delete_entry
   u <- current_user()
   admin_user <- is_admin()
   if (is.null(u)) {
     showNotification("Please sign in to delete entries.", type = "error")
     return()
   }

   # Verify ownership before showing modal (unless admin)
   entry <- db_get_sample_by_id(entry_id)
   if (nrow(entry) == 0) {
     showNotification("Entry not found.", type = "error")
     return()
   }

   is_owner <- !is.na(entry$created_by[1]) && entry$created_by[1] == u$user_uid
   if (!is_owner && !admin_user) {
     showNotification("You can only delete your own entries.", type = "error")
     return()
   }

   updateNumericInput(session, "delete_id", value = entry_id)
   showModal(delete_modal_ui)
 })

 # Handle confirm delete
 observeEvent(input$confirm_delete, {
   u <- current_user()
   if (is.null(u)) {
     showNotification("Please sign in.", type = "error")
     return()
   }

   entry_id <- input$delete_id
   if (is.null(entry_id) || is.na(entry_id)) {
     showNotification("Invalid entry.", type = "error")
     return()
   }

   success <- db_delete_sample(entry_id, u$user_uid, is_admin = is_admin())

   if (success) {
     db_audit_log("delete", "soil_samples", entry_id, u$user_uid, "entry deleted")
     removeModal()
     showNotification("Entry deleted.", type = "message")
     data_changed(data_changed() + 1)
   } else {
     showNotification("Failed to delete entry. You may not have permission.", type = "error")
   }
 })

}

server <- polished::secure_server(server_inner)
shinyApp(ui, server)
