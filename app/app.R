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
 nav_panel(
   title = "Analysis",
   icon = icon("chart-line"),
   layout_sidebar(
     sidebar = sidebar(
       title = "Select Species",
       width = 300,
       selectizeInput("analysis_species", "Species", choices = NULL, width = "100%",
                      options = list(
                        create = TRUE, persist = FALSE, maxOptions = 50,
                        openOnFocus = FALSE, closeAfterSelect = TRUE, selectOnTab = TRUE,
                        placeholder = 'Type a species name...'
                      )),
       hr(),
       uiOutput("species_summary"),
       uiOutput("reference_badges"),
       uiOutput("native_status_badge"),
       hr(),

       # --- Filter Controls ---
       tags$details(
         open = NA,  # Start collapsed
         tags$summary(
           style = "cursor: pointer; font-weight: 500; color: #7A9A86;",
           icon("filter"), " Filter Data"
         ),
         div(
           class = "mt-2",
           selectizeInput("filter_outcome", "Outcome",
                          choices = c("All" = "", "Thriving" = "Thriving",
                                      "Established" = "Established",
                                      "Struggling" = "Struggling",
                                      "Failed/Died" = "Failed/Died"),
                          selected = "",
                          options = list(placeholder = "All", plugins = list("clear_button"))),
           selectizeInput("filter_sun", "Sun Exposure",
                          choices = c("All" = "", "Full Sun" = "Full Sun",
                                      "Part Sun" = "Part Sun",
                                      "Part Shade" = "Part Shade",
                                      "Full Shade" = "Full Shade"),
                          selected = "",
                          options = list(placeholder = "All", plugins = list("clear_button"))),
           selectizeInput("filter_hydrology", "Site Hydrology",
                          choices = c("All" = "", "Dry/Xeric" = "Dry",
                                      "Mesic" = "Mesic",
                                      "Wet/Hydric" = "Wet"),
                          selected = "",
                          options = list(placeholder = "All", plugins = list("clear_button"))),
           uiOutput("filter_cultivar_ui"),
           div(class = "d-flex justify-content-between align-items-center mt-2",
               uiOutput("filter_status"),
               actionLink("clear_all_filters", "Clear All", icon = icon("times"),
                          class = "text-muted small")
           )
         )
       ),
       uiOutput("reference_msg"),
       # Download button for current species data
       uiOutput("download_species_ui")
     ),

     navset_card_tab(
       id = "analysis_tabs",
       full_screen = TRUE,

       nav_panel(
         title = "Summary",
         icon = icon("table"),
         uiOutput("summary_ui")
       ),
       nav_panel(
         title = "pH",
         icon = icon("chart-bar"),
         uiOutput("ph_plot_ui")
       ),
       nav_panel(
         title = "pH vs OM",
         icon = icon("circle-nodes"),
         uiOutput("ph_om_plot_ui")
       ),
       nav_panel(
         title = "Nutrients",
         icon = icon("leaf"),
         uiOutput("nutrient_plot_ui")
       ),
       nav_panel(
         title = "Correlations",
         icon = icon("project-diagram"),
         uiOutput("heatmap_ui")
       ),
       nav_panel(
         title = "Soil Texture",
         icon = icon("mountain"),
         uiOutput("texture_plot_ui")
       ),
       nav_panel(
         title = "Map",
         icon = icon("map"),
         uiOutput("map_ui")
       ),
       nav_panel(
         title = "Performance",
         icon = icon("seedling"),
         uiOutput("performance_ui")
       ),
       nav_panel(
         title = "Similar Species",
         icon = icon("shuffle"),
         uiOutput("similar_species_ui")
       ),
       nav_panel(
         title = "Raw Data",
         icon = icon("database"),
         card_body(DTOutput("raw_data"))
       ),
       nav_panel(
         title = "USDA Traits",
         icon = icon("book"),
         uiOutput("traits_ui")
       )
     )
   )
 ),

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

 # --- Analysis species dropdown ---
 observe({
   data_changed()
   sp <- tryCatch({
     res <- dbGetQuery(pool, "SELECT DISTINCT species FROM soil_samples ORDER BY species")
     if (nrow(res) > 0) res$species else character(0)
   }, error = function(e) character(0))

   current <- isolate(input$analysis_species) %||% ""

   updateSelectizeInput(session, "analysis_species",
                        choices = sp,
                        selected = if (nzchar(current)) current else NULL,
                        server = TRUE,
                        options = list(
                          create = TRUE, persist = FALSE, maxOptions = 50,
                          openOnFocus = FALSE, closeAfterSelect = TRUE, selectOnTab = TRUE,
                          placeholder = 'Type a species name...'
                        ))
 })

 # --- Reference badges (USDA traits + NWPL) ---
 output$reference_badges <- renderUI({
   sp <- input$analysis_species %||% ""
   if (!nzchar(sp)) return(NULL)

   ref <- get_reference_summary(sp, pool)

   chip <- function(lbl, val, href = NULL) {
     # Handle NULL, NA, empty, or vector inputs safely
     if (is.null(val)) return(NULL)
     if (length(val) == 0) return(NULL)
     val <- val[1]  # Take first element if vector
     if (is.na(val) || !nzchar(as.character(val))) return(NULL)
     badge <- tags$span(class = "badge bg-light text-dark me-1 mb-1",
                        style = "font-size: 0.75rem;",
                        sprintf("%s: %s", lbl, val))
     # Wrap in link if href provided - add external link icon to indicate clickable
     if (!is.null(href) && nzchar(href)) {
       tags$a(href = href, target = "_blank",
              style = "text-decoration: none;",
              title = paste("View on", lbl, "website"),
              tags$span(class = "badge bg-light text-dark me-1 mb-1",
                        style = "font-size: 0.75rem; cursor: pointer; border: 1px solid #27ae60;",
                        sprintf("%s: %s ", lbl, val),
                        icon("external-link-alt", class = "fa-xs", style = "color: #27ae60;")))
     } else {
       badge
     }
   }

   # Helper to safely extract single value from data frame column
   safe_val <- function(x) {
     if (is.null(x) || length(x) == 0) return(NA)
     x[1]
   }

   # Build USDA PLANTS URL from symbol
   usda_url <- function(symbol) {
     if (is.na(symbol) || !nzchar(symbol)) return(NULL)
     sprintf("https://plants.usda.gov/plant-profile/%s", symbol)
   }

   # Show reference section if USDA data exists
   if (!ref$has_traits) return(NULL)

   # Build reference badges
   tr <- ref$traits[1, , drop = FALSE]
   symbol <- safe_val(tr$usda_symbol)
   ph_min <- safe_val(tr$soil_ph_min)
   ph_max <- safe_val(tr$soil_ph_max)
   precip_min <- safe_val(if ("precip_min_in" %in% names(tr)) tr$precip_min_in else tr$precipitation_min_mm)
   precip_max <- safe_val(if ("precip_max_in" %in% names(tr)) tr$precip_max_in else tr$precipitation_max_mm)
   precip_unit <- if ("precip_min_in" %in% names(tr)) "in" else "mm"

   div(
     class = "reference-section mt-2 mb-2 p-2 rounded",
     style = "background-color: #f8f9fa; border: 1px solid #e9ecef;",
     # Header with toggle
     div(class = "d-flex justify-content-between align-items-center mb-2",
         tags$span(class = "small fw-bold text-muted", icon("book-open"), " USDA Reference"),
         tags$label(class = "d-flex align-items-center mb-0", style = "cursor: pointer;",
           tags$input(type = "checkbox", id = "show_usda_ref", checked = "checked",
                      class = "form-check-input me-1", style = "margin-top: 0;"),
           tags$span(class = "small text-muted", "Overlay")
         )
     ),
     # Badges row
     div(class = "d-flex flex-wrap gap-1",
         chip("USDA", symbol, href = usda_url(symbol)),
         chip("pH", if (!is.na(ph_min) && !is.na(ph_max)) sprintf("%.1fâ€“%.1f", ph_min, ph_max) else NA),
         chip("Shade", safe_val(tr$shade_tolerance)),
         chip("Drought", safe_val(tr$drought_tolerance))
     )
   )
 })

 # --- Native status badge (dev only - data not granular enough yet) ---
 output$native_status_badge <- renderUI({
   # Only show in dev mode - regional data (L48/AK/HI) not state-specific yet
   if (is_prod || is.null(state_grid)) return(NULL)

   sp <- input$analysis_species %||% ""
   if (!nzchar(sp)) return(NULL)

   # Get sample locations for this species
   dat <- db_get_species_data(sp)
   if (nrow(dat) == 0) return(NULL)

   # Extract states from sample coordinates
   valid_coords <- dat %>%
     filter(!is.na(location_lat) & !is.na(location_long))

   if (nrow(valid_coords) == 0) return(NULL)

   # Look up states from coordinates
   user_states <- get_states_from_coords(
     valid_coords$location_lat,
     valid_coords$location_long,
     state_grid
   )

   if (length(user_states) == 0) {
     # No state data available (outside US or grid not loaded)
     return(NULL)
   }

   # Get native status summary
   status <- get_native_status_summary(sp, user_states, pool)

   # Build badge based on summary
   badge_class <- switch(status$summary,
     "native" = "bg-success",
     "introduced" = "bg-warning text-dark",
     "mixed" = "bg-info text-dark",
     "bg-secondary"  # unknown
   )

   badge_icon <- switch(status$summary,
     "native" = icon("check-circle"),
     "introduced" = icon("exclamation-triangle"),
     "mixed" = icon("info-circle"),
     icon("question-circle")
   )

   badge_text <- switch(status$summary,
     "native" = "Native to your area",
     "introduced" = "Introduced in your area",
     "mixed" = "Mixed native status",
     "Native status unknown"
   )

   # Build detail text for mixed status
   detail_text <- NULL
   if (status$summary == "mixed" && (length(status$native_states) > 0 || length(status$introduced_states) > 0)) {
     parts <- c()
     if (length(status$native_states) > 0) {
       parts <- c(parts, paste0("Native: ", paste(status$native_states, collapse = ", ")))
     }
     if (length(status$introduced_states) > 0) {
       parts <- c(parts, paste0("Introduced: ", paste(status$introduced_states, collapse = ", ")))
     }
     detail_text <- paste(parts, collapse = " | ")
   }

   div(
     class = "native-status-section mt-2 p-2 rounded",
     style = switch(status$summary,
       "native" = "background-color: #f0fdf4; border: 1px solid #86efac;",
       "introduced" = "background-color: #fefce8; border: 1px solid #fde047;",
       "mixed" = "background-color: #ecfeff; border: 1px solid #67e8f9;",
       "background-color: #f3f4f6; border: 1px solid #d1d5db;"
     ),
     div(
       class = "d-flex align-items-center gap-2",
       tags$span(
         class = paste("badge", badge_class),
         badge_icon, " ", badge_text
       )
     ),
     if (!is.null(detail_text)) {
       div(class = "small text-muted mt-1", detail_text)
     }
   )
 })

 # --- Reference message (when no sample data) ---
 output$reference_msg <- renderUI({
   sp <- input$analysis_species %||% ""
   if (!nzchar(sp)) return(NULL)

   dat <- db_get_species_data(sp)
   n <- nrow(dat)

   if (n > 0) return(NULL)

   ref <- get_reference_summary(sp, pool)
   if (!ref$has_traits && !ref$has_nwpl) {
     div(class = "text-muted small mt-2",
         icon("info-circle"), " No samples or reference data for this species.")
   } else {
     div(class = "text-info small mt-2",
         icon("info-circle"), " No samples yet. Showing reference data only.")
   }
 })

 # --- Species summary sidebar ---
 output$species_summary <- renderUI({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) == 0) return(NULL)

   n_samples <- nrow(dat)

   # Calculate success rate
   success_rate <- NA
   if ("outcome" %in% names(dat) && sum(!is.na(dat$outcome)) > 0) {
     outcomes <- dat$outcome[!is.na(dat$outcome)]
     n_success <- sum(outcomes %in% c("Thriving", "Established"))
     success_rate <- round(n_success / length(outcomes) * 100)
   }

   # Count ecoregions
   n_ecoregions <- length(unique(dat$ecoregion_l4[!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4)]))

   # pH range
   ph_range <- if (sum(!is.na(dat$ph)) > 0) {
     sprintf("%.1f â€“ %.1f", min(dat$ph, na.rm = TRUE), max(dat$ph, na.rm = TRUE))
   } else "â€”"

   # Build compact preview
   div(class = "small",
     # Sample count and success rate in one line
     div(class = "d-flex justify-content-between align-items-center mb-2 pb-2 border-bottom",
       span(class = "fw-bold", style = "color: #7A9A86;", paste(n_samples, "samples")),
       if (!is.na(success_rate)) {
         span(class = if (success_rate >= 70) "text-success" else if (success_rate >= 50) "text-warning" else "text-danger",
              paste0(success_rate, "% success"))
       }
     ),
     # Quick stats
     div(class = "text-muted",
       div(class = "d-flex justify-content-between", span("pH range"), span(ph_range)),
       div(class = "d-flex justify-content-between", span("Ecoregions"), span(n_ecoregions)),
       if (sum(!is.na(dat$organic_matter)) > 0) {
         div(class = "d-flex justify-content-between",
             span("Avg OM"),
             span(paste0(round(mean(dat$organic_matter, na.rm = TRUE), 1), "%")))
       }
     )
   )
 })

 # ---------------------------
 # Filtered Analysis Data
 # ---------------------------

 # Cultivar filter UI - dynamically populated based on selected species
 output$filter_cultivar_ui <- renderUI({
   sp <- input$analysis_species %||% ""
   if (!nzchar(sp)) return(NULL)

   dat <- db_get_species_data(sp)
   cultivars <- unique(dat$cultivar[!is.na(dat$cultivar) & nzchar(dat$cultivar)])

   if (length(cultivars) == 0) return(NULL)

   # Sort cultivars alphabetically
   cultivars <- sort(cultivars)
   choices <- c("All" = "", setNames(cultivars, cultivars))

   selectizeInput("filter_cultivar", "Cultivar",
                  choices = choices,
                  selected = "",
                  options = list(placeholder = "All", plugins = list("clear_button")))
 })

 # Clear all filters button
 observeEvent(input$clear_all_filters, {
   updateSelectizeInput(session, "filter_outcome", selected = "")
   updateSelectizeInput(session, "filter_sun", selected = "")
   updateSelectizeInput(session, "filter_hydrology", selected = "")
   updateSelectizeInput(session, "filter_cultivar", selected = "")
 })

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

 # Reactive for filtered species data based on sidebar filters
 filtered_species_data <- reactive({
   sp <- input$analysis_species %||% ""
   if (!nzchar(sp)) return(data.frame())

   dat <- db_get_species_data(sp)
   if (nrow(dat) == 0) return(dat)

   # Apply outcome filter
   if (nzchar(input$filter_outcome %||% "")) {
     dat <- dat[!is.na(dat$outcome) & dat$outcome == input$filter_outcome, , drop = FALSE]
   }

   # Apply sun exposure filter
   if (nzchar(input$filter_sun %||% "")) {
     dat <- dat[!is.na(dat$sun_exposure) & dat$sun_exposure == input$filter_sun, , drop = FALSE]
   }

   # Apply hydrology filter
   if (nzchar(input$filter_hydrology %||% "")) {
     dat <- dat[!is.na(dat$site_hydrology) & dat$site_hydrology == input$filter_hydrology, , drop = FALSE]
   }

   # Apply cultivar filter
   if (nzchar(input$filter_cultivar %||% "")) {
     dat <- dat[!is.na(dat$cultivar) & dat$cultivar == input$filter_cultivar, , drop = FALSE]
   }

   dat
 })

 # Filter status display
 output$filter_status <- renderUI({
   sp <- input$analysis_species %||% ""
   if (!nzchar(sp)) return(NULL)

   total <- nrow(db_get_species_data(sp))
   filtered <- nrow(filtered_species_data())

   if (total == filtered) {
     span(class = "small text-muted", icon("check"), sprintf(" Showing all %d", total))
   } else {
     span(class = "small text-info", icon("filter"),
          sprintf(" Showing %d of %d", filtered, total))
   }
 })

 # Download button UI (shows when species is selected)
 output$download_species_ui <- renderUI({
   sp <- input$analysis_species %||% ""
   if (!nzchar(sp)) return(NULL)

   dat <- filtered_species_data()
   if (nrow(dat) == 0) return(NULL)

   div(class = "mt-3 pt-3 border-top",
     downloadButton("download_species_csv", "Download CSV",
                    class = "btn-sm btn-outline-secondary w-100",
                    icon = icon("download"))
   )
 })

 # Download handler for species data
 output$download_species_csv <- downloadHandler(
   filename = function() {
     sp <- input$analysis_species %||% "species"
     # Clean species name for filename
     clean_name <- gsub("[^a-zA-Z0-9]", "_", sp)
     paste0("edaphic_flora_", clean_name, "_", Sys.Date(), ".csv")
   },
   content = function(file) {
     dat <- filtered_species_data()
     # Select useful columns for export
     export_cols <- c("species", "cultivar", "outcome", "sun_exposure", "site_hydrology",
                      "ph", "organic_matter", "texture_class", "texture_sand", "texture_silt", "texture_clay",
                      "nitrate_ppm", "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
                      "calcium_ppm", "magnesium_ppm", "cec", "soluble_salts_ppm",
                      "location_lat", "location_long", "ecoregion_l4", "date", "notes")
     export_cols <- export_cols[export_cols %in% names(dat)]
     write.csv(dat[, export_cols, drop = FALSE], file, row.names = FALSE, na = "")
   }
 )

 # ---------------------------
 # Analysis Outputs
 # ---------------------------

 # Helper for empty states
 empty_state <- function(icon_name, title, message) {
   div(class = "empty-state",
       tags$i(class = paste("fa", paste0("fa-", icon_name))),
       h5(title),
       p(class = "text-muted", message))
 }

 # ---------------------------
 # Similar Species Matching
 # ---------------------------


 # Get all species profiles (cached for performance)
 get_all_species_profiles <- reactive({
   data_changed()

   # Get all species with 10+ samples
   species_counts <- tryCatch({
     dbGetQuery(pool, "
       SELECT species, COUNT(*) as n
       FROM soil_samples
       GROUP BY species
       HAVING COUNT(*) >= 10
       ORDER BY COUNT(*) DESC
     ")
   }, error = function(e) data.frame())

   if (nrow(species_counts) == 0) return(list())

   # Calculate profile for each species
   profiles <- list()
   for (sp in species_counts$species) {
     dat <- db_get_species_data(sp)
     profiles[[sp]] <- calc_species_profile(dat)
     profiles[[sp]]$species <- sp
   }

   profiles
 })

 # --- Similar Species UI ---
 output$similar_species_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("shuffle", "No Species Selected", "Choose a species to find similar ones"))
   }

   current_sp <- input$analysis_species
   current_dat <- filtered_species_data()

   if (nrow(current_dat) < 3) {
     return(empty_state("database", "Insufficient Data",
                        "Need at least 3 samples to find similar species"))
   }

   current_profile <- calc_species_profile(current_dat)
   all_profiles <- get_all_species_profiles()

   # Remove current species from comparison
   all_profiles <- all_profiles[names(all_profiles) != current_sp]

   if (length(all_profiles) == 0) {
     return(empty_state("seedling", "No Comparison Data",
                        "No other species have enough samples (10+) for comparison"))
   }

   # Calculate similarity scores
   similarities <- sapply(all_profiles, function(p) calc_similarity(current_profile, p))
   similarities <- sort(similarities, decreasing = TRUE)

   # Take top 10
   top_matches <- head(similarities, 10)

   # Build UI
   tagList(
     div(class = "mb-3",
       tags$small(class = "text-muted",
         "Species with similar soil conditions to ", tags$strong(current_sp),
         ". Based on pH, organic matter, texture, and nutrients from user-submitted data."
       )
     ),

     # Current species profile summary
     card(
       class = "mb-3",
       card_header(class = "py-2", tags$small(icon("crosshairs"), " Current Profile: ", tags$strong(current_sp))),
       card_body(
         class = "py-2",
         div(class = "d-flex flex-wrap gap-3 small",
           if (!is.na(current_profile$ph_mean)) span(class = "text-muted", "pH: ", tags$strong(sprintf("%.1f", current_profile$ph_mean))),
           if (!is.na(current_profile$om_mean)) span(class = "text-muted", "OM: ", tags$strong(sprintf("%.1f%%", current_profile$om_mean))),
           if (!is.null(current_profile$texture_class)) span(class = "text-muted", "Texture: ", tags$strong(current_profile$texture_class)),
           span(class = "text-muted", "Samples: ", tags$strong(current_profile$n_samples))
         )
       )
     ),

     # Similar species list
     div(class = "similar-species-list",
       lapply(names(top_matches), function(sp) {
         score <- round(top_matches[sp])
         profile <- all_profiles[[sp]]

         # Color based on match score
         score_color <- if (score >= 80) "#27ae60" else if (score >= 60) "#7A9A86" else if (score >= 40) "#f39c12" else "#95a5a6"

         card(
           class = "mb-2",
           card_body(
             class = "py-2 px-3",
             div(class = "d-flex justify-content-between align-items-center",
               div(
                 tags$strong(sp),
                 div(class = "small text-muted",
                   paste(profile$n_samples, "samples"),
                   if (!is.null(profile$success_rate)) paste0(" Â· ", round(profile$success_rate), "% success"),
                   if (!is.null(profile$best_sun)) paste0(" Â· ", profile$best_sun),
                   if (!is.null(profile$best_hydrology)) paste0(" Â· ", profile$best_hydrology)
                 )
               ),
               div(class = "text-end",
                 tags$span(class = "badge fs-6", style = paste0("background-color:", score_color),
                           paste0(score, "% match")),
                 div(class = "small text-muted mt-1",
                   if (!is.na(profile$ph_mean)) sprintf("pH %.1f", profile$ph_mean) else "",
                   if (!is.na(profile$om_mean)) sprintf(" Â· %.1f%% OM", profile$om_mean) else ""
                 )
               )
             )
           )
         )
       })
     ),

     if (length(top_matches) > 0) {
       tagList(
         div(class = "mt-3 small text-muted",
           icon("info-circle"), " Match scores based on similarity in pH (30%), organic matter (20%), ",
           "texture (15%), and nutrient levels (35%). Only species with 10+ samples shown."
         ),
        div(class = "mt-2 p-2 bg-light rounded small text-muted",
          icon("exclamation-triangle"), tags$strong(" Important: "),
          "Soil chemistry is just one factor in plant success. These matches do not account for ",
          "climate/hardiness zones, drainage, microclimate, or pest pressure. ",
          tags$strong("Prioritize native plants"), " and verify species are not invasive in your area before planting. ",
          tags$a(href = "https://www.invasivespeciesinfo.gov/", target = "_blank", "Check invasive species lists.")
        )
       )
     }
   )
 })

 # --- Summary ---
 output$summary_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("search", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   if (nrow(dat) == 0) {
     return(empty_state("database", "No Data", paste("No samples for", input$analysis_species, "(with current filters)")))
   }

   # Calculate metrics for cards
   n_samples <- nrow(dat)
   n_cultivars <- length(unique(dat$cultivar[!is.na(dat$cultivar) & nzchar(dat$cultivar)]))

   # Outcome breakdown
   outcome_counts <- if ("outcome" %in% names(dat)) table(dat$outcome[!is.na(dat$outcome)]) else NULL
   has_outcomes <- !is.null(outcome_counts) && length(outcome_counts) > 0

   # Success rate
   if (has_outcomes) {
     n_success <- sum(outcome_counts[names(outcome_counts) %in% c("Thriving", "Established")], na.rm = TRUE)
     n_total_outcome <- sum(outcome_counts)
     success_rate <- if (n_total_outcome > 0) round(n_success / n_total_outcome * 100) else NA
   } else {
     success_rate <- NA
   }

   tagList(
     # Key metrics row
     layout_column_wrap(
       width = 1/4,
       card(
         class = "text-center",
         card_body(
           tags$h2(class = "mb-0", n_samples),
           tags$small(class = "text-muted", "Samples")
         )
       ),
       card(
         class = "text-center",
         card_body(
           tags$h2(class = "mb-0", if (!is.na(success_rate)) paste0(success_rate, "%") else "â€”"),
           tags$small(class = "text-muted", "Success Rate")
         )
       ),
       card(
         class = "text-center",
         card_body(
           tags$h2(class = "mb-0", if (n_cultivars > 0) n_cultivars else "â€”"),
           tags$small(class = "text-muted", "Cultivars")
         )
       ),
       card(
         class = "text-center",
         card_body(
           tags$h2(class = "mb-0", length(unique(dat$ecoregion_l4[!is.na(dat$ecoregion_l4)]))),
           tags$small(class = "text-muted", "Ecoregions")
         )
       )
     ),

     # Details row
     layout_column_wrap(
       width = 1/2,
       class = "mt-3",
       # Soil Chemistry Card
       card(
         card_header(icon("flask"), " Soil Chemistry"),
         card_body(tableOutput("summary_soil_stats"))
       ),
       # Performance & Conditions Card
       card(
         card_header(icon("seedling"), " Performance & Conditions"),
         card_body(uiOutput("summary_performance"))
       )
     )
   )
 })

 output$summary_soil_stats <- renderTable({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   if (nrow(dat) == 0) return(NULL)

   # Only show stats for parameters that have data
   stats <- list()

   if (sum(!is.na(dat$ph)) > 0) {
     stats[["pH"]] <- sprintf("%.1f (%.1f â€“ %.1f)",
                               mean(dat$ph, na.rm = TRUE),
                               min(dat$ph, na.rm = TRUE),
                               max(dat$ph, na.rm = TRUE))
   }
   if (sum(!is.na(dat$organic_matter)) > 0) {
     stats[["Organic Matter"]] <- sprintf("%.1f%% (%.1f â€“ %.1f)",
                                           mean(dat$organic_matter, na.rm = TRUE),
                                           min(dat$organic_matter, na.rm = TRUE),
                                           max(dat$organic_matter, na.rm = TRUE))
   }
   if (sum(!is.na(dat$texture_class)) > 0) {
     top_textures <- names(sort(table(dat$texture_class), decreasing = TRUE))[1:min(2, length(unique(dat$texture_class)))]
     stats[["Texture"]] <- paste(top_textures, collapse = ", ")
   }
   if (sum(!is.na(dat$nitrate_ppm)) > 0) {
     stats[["Nitrate (ppm)"]] <- sprintf("%.0f avg", mean(dat$nitrate_ppm, na.rm = TRUE))
   }
   if (sum(!is.na(dat$phosphorus_ppm)) > 0) {
     stats[["Phosphorus (ppm)"]] <- sprintf("%.0f avg", mean(dat$phosphorus_ppm, na.rm = TRUE))
   }
   if (sum(!is.na(dat$potassium_ppm)) > 0) {
     stats[["Potassium (ppm)"]] <- sprintf("%.0f avg", mean(dat$potassium_ppm, na.rm = TRUE))
   }

   if (length(stats) == 0) return(data.frame(Parameter = "No data", Value = "â€”"))

   data.frame(Parameter = names(stats), Value = unlist(stats), stringsAsFactors = FALSE)
 }, striped = TRUE, hover = TRUE, width = "100%", colnames = FALSE)

 output$summary_performance <- renderUI({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   if (nrow(dat) == 0) return(NULL)

   # Outcome breakdown
   outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
   outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                       "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")

   outcome_html <- if (sum(!is.na(dat$outcome)) > 0) {
     oc <- table(factor(dat$outcome, levels = outcome_order))
     oc <- oc[oc > 0]
     div(
       tags$strong("Outcomes:"),
       tags$div(class = "d-flex flex-wrap gap-2 mt-1",
         lapply(names(oc), function(o) {
           span(class = "badge", style = paste0("background-color:", outcome_colors[o]),
                paste(o, "(", oc[o], ")"))
         })
       )
     )
   } else {
     tags$p(class = "text-muted small", "No outcome data recorded")
   }

   # Sun exposure breakdown
   sun_order <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade")
   sun_html <- if (sum(!is.na(dat$sun_exposure)) > 0) {
     sc <- table(factor(dat$sun_exposure, levels = sun_order))
     sc <- sc[sc > 0]
     div(class = "mt-3",
       tags$strong("Sun Exposure:"),
       tags$div(class = "mt-1",
         paste(sapply(names(sc), function(s) paste0(s, " (", sc[s], ")")), collapse = " Â· ")
       )
     )
   } else NULL

   # Hydrology breakdown
   hydro_order <- c("Dry", "Mesic", "Wet")
   hydro_html <- if (sum(!is.na(dat$site_hydrology)) > 0) {
     hc <- table(factor(dat$site_hydrology, levels = hydro_order))
     hc <- hc[hc > 0]
     div(class = "mt-3",
       tags$strong("Site Hydrology:"),
       tags$div(class = "mt-1",
         paste(sapply(names(hc), function(h) paste0(h, " (", hc[h], ")")), collapse = " Â· ")
       )
     )
   } else NULL

   # Cultivar breakdown
   cultivar_html <- if (sum(!is.na(dat$cultivar) & nzchar(dat$cultivar)) > 0) {
     cc <- sort(table(dat$cultivar[!is.na(dat$cultivar) & nzchar(dat$cultivar)]), decreasing = TRUE)
     div(class = "mt-3",
       tags$strong("Cultivars:"),
       tags$div(class = "mt-1",
         paste(sapply(names(cc), function(c) paste0("'", c, "' (", cc[c], ")")), collapse = " Â· ")
       )
     )
   } else NULL

   # Ecoregion breakdown
   ecoregion_html <- if (sum(!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4)) > 0) {
     ec <- sort(table(dat$ecoregion_l4[!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4)]), decreasing = TRUE)
     # Show top 3 if many ecoregions
     if (length(ec) > 3) {
       shown <- head(ec, 3)
       others <- sum(tail(ec, -3))
       label <- paste(c(sapply(names(shown), function(e) paste0(e, " (", shown[e], ")")),
                        paste0("+ ", length(ec) - 3, " more")), collapse = " Â· ")
     } else {
       label <- paste(sapply(names(ec), function(e) paste0(e, " (", ec[e], ")")), collapse = " Â· ")
     }
     div(class = "mt-3",
       tags$strong(icon("map-location-dot"), " Ecoregions:"),
       tags$div(class = "mt-1 small", label)
     )
   } else NULL

   tagList(outcome_html, sun_html, hydro_html, cultivar_html, ecoregion_html)
 })

 # --- pH Distribution (Plotly) ---
 output$ph_plot_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("chart-bar", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   # Allow reference-only mode if USDA data exists
   if (nrow(dat) == 0) {
     tr <- get_usda_traits_for_name(input$analysis_species, pool)
     if (is.null(tr) || nrow(tr) == 0 || is.na(tr$soil_ph_min) || is.na(tr$soil_ph_max)) {
       return(empty_state("database", "No Data", "No samples (with current filters) or USDA pH reference available"))
     }
   }
   tagList(
     plotlyOutput("ph_plot", height = "500px"),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "This histogram shows the distribution of pH values from user-submitted samples. ",
            "The green shaded region (if visible) indicates the USDA reference pH range for this species. ",
            "Soil pH affects nutrient availability; most plants prefer slightly acidic to neutral conditions (6.0-7.0).")
   )
 })

 output$ph_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   tr <- if (isTRUE(input$show_usda_ref)) get_usda_traits_for_name(input$analysis_species, pool) else NULL
   has_usda_ph <- !is.null(tr) && nrow(tr) > 0 && !is.na(tr$soil_ph_min) && !is.na(tr$soil_ph_max)

   # Reference-only mode: no samples but USDA data exists
   if (nrow(dat) == 0) {
     if (!has_usda_ph) return(NULL)
     p <- ggplot() +
       annotate("rect", xmin = tr$soil_ph_min[1], xmax = tr$soil_ph_max[1],
                ymin = -Inf, ymax = Inf, alpha = 0.15, fill = edaphic_colors$accent) +
       annotate("text", x = (tr$soil_ph_min[1] + tr$soil_ph_max[1]) / 2, y = 0.5,
                label = sprintf("USDA pH: %.1f - %.1f", tr$soil_ph_min[1], tr$soil_ph_max[1]),
                color = edaphic_colors$dark, size = 4) +
       xlim(0, 14) + ylim(0, 1) +
       labs(title = paste("pH Reference Range -", input$analysis_species),
            subtitle = "No sample data - showing USDA reference range",
            x = "Soil pH", y = "") +
       theme_edaphic() +
       theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
     return(ggplotly(p) %>% config(displayModeBar = TRUE, displaylogo = FALSE))
   }

   # Normal mode with samples
   p <- ggplot(dat, aes(x = ph)) +
     geom_histogram(bins = 15, fill = edaphic_colors$primary, color = "white", alpha = 0.85) +
     geom_vline(xintercept = mean(dat$ph, na.rm = TRUE),
                color = edaphic_colors$danger, linetype = "dashed", linewidth = 1)

   # Add USDA overlay if available
   if (has_usda_ph) {
     p <- p + annotate("rect", xmin = tr$soil_ph_min[1], xmax = tr$soil_ph_max[1],
                       ymin = -Inf, ymax = Inf, alpha = 0.12, fill = edaphic_colors$accent)
   }

   p <- p + labs(title = paste("pH Distribution -", input$analysis_species),
                 subtitle = paste("n =", nrow(dat), "| Mean =", round(mean(dat$ph, na.rm = TRUE), 2),
                                  if (has_usda_ph) paste("| USDA:", tr$soil_ph_min[1], "-", tr$soil_ph_max[1]) else ""),
                 x = "Soil pH", y = "Count") +
     theme_edaphic()

   ggplotly(p, tooltip = c("x", "y")) %>%
     config(displayModeBar = TRUE, displaylogo = FALSE)
 })

 # --- pH vs OM (Plotly) ---
 output$ph_om_plot_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("circle-nodes", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   # Allow reference-only mode if USDA data exists
   if (nrow(dat) == 0) {
     tr <- get_usda_traits_for_name(input$analysis_species, pool)
     if (is.null(tr) || nrow(tr) == 0 || is.na(tr$soil_ph_min) || is.na(tr$soil_ph_max)) {
       return(empty_state("database", "No Data", "No samples (with current filters) or USDA pH reference available"))
     }
   }
   tagList(
     plotlyOutput("ph_om_plot", height = "500px"),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "This scatter plot shows the relationship between soil pH and organic matter content. ",
            "Points are colored by plant outcome (if available). ",
            "The green shaded region shows the USDA reference pH range if available.")
   )
 })

 output$ph_om_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   tr <- if (isTRUE(input$show_usda_ref)) get_usda_traits_for_name(input$analysis_species, pool) else NULL
   has_usda_ph <- !is.null(tr) && nrow(tr) > 0 && !is.na(tr$soil_ph_min) && !is.na(tr$soil_ph_max)

   # Reference-only mode
   if (nrow(dat) == 0) {
     if (!has_usda_ph) return(NULL)
     p <- ggplot() +
       annotate("rect", xmin = tr$soil_ph_min[1], xmax = tr$soil_ph_max[1],
                ymin = -Inf, ymax = Inf, alpha = 0.15, fill = edaphic_colors$accent) +
       annotate("text", x = (tr$soil_ph_min[1] + tr$soil_ph_max[1]) / 2, y = 50,
                label = sprintf("USDA pH: %.1f - %.1f", tr$soil_ph_min[1], tr$soil_ph_max[1]),
                color = edaphic_colors$dark, size = 4) +
       xlim(0, 14) + ylim(0, 100) +
       labs(title = paste("pH vs Organic Matter -", input$analysis_species),
            subtitle = "No sample data - showing USDA pH reference range",
            x = "Soil pH", y = "Organic Matter (%)") +
       theme_edaphic()
     return(ggplotly(p) %>% config(displayModeBar = TRUE, displaylogo = FALSE))
   }

   cor_val <- cor(dat$ph, dat$organic_matter, use = "complete.obs")

   # Determine if we have outcome data for coloring
   has_outcome <- sum(!is.na(dat$outcome)) > 0

   p <- ggplot(dat, aes(x = ph, y = organic_matter))

   # Add USDA overlay if available (before points so it's behind)
   if (has_usda_ph) {
     p <- p + annotate("rect", xmin = tr$soil_ph_min[1], xmax = tr$soil_ph_max[1],
                       ymin = -Inf, ymax = Inf, alpha = 0.12, fill = edaphic_colors$accent)
   }

   p <- p +
     geom_smooth(method = "lm", se = TRUE, color = edaphic_colors$dark,
                 fill = edaphic_colors$light, alpha = 0.3, linetype = "dashed")

   # Color by outcome if available, otherwise by texture
   if (has_outcome) {
     outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
     outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                         "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
     dat$outcome <- factor(dat$outcome, levels = outcome_order)
     dat <- dat[order(dat$outcome), ]
     p <- p +
       geom_point(aes(color = outcome,
                      text = paste0("pH: ", ph, "\nOM: ", organic_matter, "%\nOutcome: ", outcome)),
                  size = 3, alpha = 0.8) +
       scale_color_manual(values = outcome_colors, limits = outcome_order, breaks = outcome_order, drop = FALSE, na.value = "#95a5a6") +
       labs(color = "Outcome")
   } else {
     p <- p +
       geom_point(aes(color = texture_class,
                      text = paste0("pH: ", ph, "\nOM: ", organic_matter, "%\nTexture: ", texture_class)),
                  size = 3, alpha = 0.7) +
       scale_color_edaphic() +
       labs(color = "Texture")
   }

   p <- p +
     labs(title = paste("pH vs Organic Matter -", input$analysis_species),
          subtitle = paste("r =", round(cor_val, 3),
                           if (has_usda_ph) paste("| USDA pH:", tr$soil_ph_min[1], "-", tr$soil_ph_max[1]) else ""),
          x = "Soil pH", y = "Organic Matter (%)") +
     theme_edaphic()

  plt <- ggplotly(p, tooltip = "text") %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)

  # Reorder legend traces to match desired order: Thriving, Established, Struggling, Failed/Died
  if (has_outcome && length(plt$x$data) > 0) {
    trace_names <- sapply(plt$x$data, function(t) if (!is.null(t$name)) t$name else "")
    # Find indices for each outcome in the desired order
    desired_order <- c()
    for (outcome_name in outcome_order) {
      idx <- which(trace_names == outcome_name)
      if (length(idx) > 0) desired_order <- c(desired_order, idx)
    }
    # Add any remaining traces (like the smoothed line) at the beginning
    other_indices <- setdiff(seq_along(plt$x$data), desired_order)
    new_order <- c(other_indices, desired_order)
    if (length(new_order) == length(plt$x$data)) {
      plt$x$data <- plt$x$data[new_order]
    }
  }

  plt
 })

 # --- Nutrients (Plotly) ---
 output$nutrient_plot_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("leaf", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   if (nrow(dat) == 0) {
     return(empty_state("database", "No Data", "No samples match current filters"))
   }
   tagList(
     plotlyOutput("nutrient_plot", height = "500px"),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "Box plots showing the distribution of nutrient concentrations (ppm) across all samples. ",
            "N-P-K (nitrogen, phosphorus, potassium) are primary macronutrients. Ca, Mg, and S are secondary macronutrients. ",
            "Fe, Mn, Zn, Cu, and B are micronutrients needed in smaller quantities. Only nutrients with data are shown.")
   )
 })

 output$nutrient_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   if (nrow(dat) == 0) return(NULL)

   nutrient_cols <- c("nitrate_ppm", "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
                      "calcium_ppm", "magnesium_ppm", "sulfur_ppm",
                      "iron_ppm", "manganese_ppm", "zinc_ppm", "copper_ppm", "boron_ppm")
   # Only include columns that exist in the data
   nutrient_cols <- intersect(nutrient_cols, names(dat))

   nutrient_data <- dat %>%
     select(all_of(nutrient_cols)) %>%
     pivot_longer(everything(), names_to = "nutrient", values_to = "value") %>%
     filter(!is.na(value)) %>%  # Exclude NA values from analysis
     mutate(nutrient = case_when(
       nutrient == "nitrate_ppm" ~ "Nitrate (N)",
       nutrient == "ammonium_ppm" ~ "Ammonium (N)",
       nutrient == "phosphorus_ppm" ~ "Phosphorus (P)",
       nutrient == "potassium_ppm" ~ "Potassium (K)",
       nutrient == "calcium_ppm" ~ "Calcium (Ca)",
       nutrient == "magnesium_ppm" ~ "Magnesium (Mg)",
       nutrient == "sulfur_ppm" ~ "Sulfur (S)",
       nutrient == "iron_ppm" ~ "Iron (Fe)",
       nutrient == "manganese_ppm" ~ "Manganese (Mn)",
       nutrient == "zinc_ppm" ~ "Zinc (Zn)",
       nutrient == "copper_ppm" ~ "Copper (Cu)",
       nutrient == "boron_ppm" ~ "Boron (B)",
       TRUE ~ nutrient
     ))

   if (nrow(nutrient_data) == 0) {
     return(plotly_empty() %>% layout(title = "No nutrient data available"))
   }

   p <- ggplot(nutrient_data, aes(x = reorder(nutrient, value, FUN = function(x) median(x, na.rm = TRUE)),
                                   y = value, fill = nutrient)) +
     geom_boxplot(alpha = 0.8) +
     coord_flip() +
     scale_fill_edaphic() +
     labs(title = paste("Nutrient Levels -", input$analysis_species),
          x = NULL, y = "Concentration (ppm)") +
     theme_edaphic() +
     theme(legend.position = "none")

   ggplotly(p, tooltip = c("y")) %>%
     config(displayModeBar = TRUE, displaylogo = FALSE)
 })

 # --- Correlations (Plotly) ---
 output$heatmap_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("project-diagram", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   if (nrow(dat) < 3) {
     return(empty_state("database", "Insufficient Data", "Need at least 3 samples for correlations"))
   }
   tagList(
     plotlyOutput("heatmap_plot", height = "550px"),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "Correlation matrix showing relationships between soil parameters. Blue indicates positive correlation (when one increases, so does the other); ",
            "red indicates negative correlation. Values range from -1 (perfect negative) to +1 (perfect positive). ",
            "Strong correlations (|r| > 0.7) suggest linked soil processes.")
   )
 })

 output$heatmap_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   if (nrow(dat) < 3) return(NULL)

   numeric_cols <- c("ph", "organic_matter", "cec_meq", "soluble_salts_ppm",
                     "nitrate_ppm", "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
                     "calcium_ppm", "magnesium_ppm", "sulfur_ppm",
                     "iron_ppm", "manganese_ppm", "zinc_ppm", "copper_ppm", "boron_ppm")
   nice_labels <- c(ph = "pH", organic_matter = "Organic Matter", cec_meq = "CEC",
                    soluble_salts_ppm = "Soluble Salts",
                    nitrate_ppm = "Nitrate", ammonium_ppm = "Ammonium",
                    phosphorus_ppm = "Phosphorus", potassium_ppm = "Potassium",
                    calcium_ppm = "Calcium", magnesium_ppm = "Magnesium", sulfur_ppm = "Sulfur",
                    iron_ppm = "Iron", manganese_ppm = "Manganese", zinc_ppm = "Zinc",
                    copper_ppm = "Copper", boron_ppm = "Boron")

   # Only include columns that exist and have non-NA data
   numeric_cols <- intersect(numeric_cols, names(dat))
   has_data <- sapply(numeric_cols, function(col) sum(!is.na(dat[[col]])) >= 3)
   numeric_cols <- numeric_cols[has_data]

   if (length(numeric_cols) < 2) {
     return(plotly_empty() %>% layout(title = "Insufficient data for correlations"))
   }

   cor_matrix <- cor(dat[, numeric_cols, drop = FALSE], use = "pairwise.complete.obs")
   rownames(cor_matrix) <- nice_labels[rownames(cor_matrix)]
   colnames(cor_matrix) <- nice_labels[colnames(cor_matrix)]

   plot_ly(z = cor_matrix, x = colnames(cor_matrix), y = rownames(cor_matrix),
           type = "heatmap",
           colorscale = list(list(0, "#3498db"), list(0.5, "#ffffff"), list(1, "#e74c3c")),
           zmin = -1, zmax = 1,
           text = round(cor_matrix, 2), texttemplate = "%{text}",
           hovertemplate = "%{y} vs %{x}<br>r = %{z:.3f}<extra></extra>") %>%
     layout(title = list(text = paste("Parameter Correlations -", input$analysis_species)),
            xaxis = list(tickangle = 45),
            margin = list(l = 100, b = 100)) %>%
     config(displayModeBar = TRUE, displaylogo = FALSE)
 })

 # --- Texture (ggtern - static) ---
 output$texture_plot_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("mountain", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
   if (nrow(dat) == 0) {
     return(empty_state("database", "No Texture Data", "No samples with texture data matching filters"))
   }
   tagList(
     plotOutput("texture_plot", height = "550px"),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "Ternary diagram showing soil texture composition (% sand, silt, and clay). ",
            "Points are colored by USDA texture class. Soil texture affects drainage, water retention, and root penetration. ",
            "Sandy soils drain quickly; clay soils retain water and nutrients but may become compacted.")
   )
 })

 output$texture_plot <- renderPlot({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
   if (nrow(dat) == 0) return(NULL)

   # Check if we have outcome data to color by
   has_outcome <- "outcome" %in% names(dat) && sum(!is.na(dat$outcome)) >= 2

   p <- ggtern(dat, aes(x = texture_sand, y = texture_silt, z = texture_clay)) +
     theme_bw() + theme_showarrows() +
     xlab("Sand (%)") + ylab("Silt (%)") + zlab("Clay (%)")

   # Color by outcome if available, otherwise by texture class
   if (has_outcome) {
     outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
     outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                         "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
     dat$outcome <- factor(dat$outcome, levels = outcome_order)
     p <- p +
       geom_point(aes(color = outcome), size = 4, alpha = 0.8) +
       scale_color_manual(values = outcome_colors, breaks = outcome_order, na.value = "#95a5a6") +
       labs(title = paste("Soil Texture -", input$analysis_species),
            subtitle = paste(nrow(dat), "samples (colored by outcome)"),
            color = "Outcome")
   } else {
     p <- p +
       geom_point(aes(color = texture_class), size = 4, alpha = 0.7) +
       scale_color_edaphic() +
       labs(title = paste("Soil Texture -", input$analysis_species),
            subtitle = paste(nrow(dat), "samples"),
            color = "Texture")
   }

   p <- p +
     theme(
       tern.panel.background = element_rect(fill = "#f8f9fa"),
       tern.panel.grid.major = element_line(color = "#dee2e6"),
       plot.title = element_text(size = 14, face = "bold", color = edaphic_colors$dark),
       plot.subtitle = element_text(color = edaphic_colors$muted),
       legend.position = "right"
     ) +
     scale_T_continuous(breaks = seq(0, 100, by = 20)) +
     scale_L_continuous(breaks = seq(0, 100, by = 20)) +
     scale_R_continuous(breaks = seq(0, 100, by = 20))

   print(p)
 })

 # --- Map (Leaflet) ---
 output$map_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("map", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$location_lat) & !is.na(dat$location_long), ]
   if (nrow(dat) == 0) {
     return(empty_state("map-marker-alt", "No Location Data", "No samples with coordinates matching filters"))
   }
   tagList(
     leafletOutput("map_plot", height = "550px"),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "Geographic distribution of sample locations. Click markers to view soil data and ecoregion information. ",
            "Clustering samples by geography helps identify regional soil patterns and growing conditions.")
   )
 })

 output$map_plot <- renderLeaflet({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$location_lat) & !is.na(dat$location_long), ]
   if (nrow(dat) == 0) return(NULL)

   # Color markers by outcome if available
   outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                       "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
   has_outcome <- sum(!is.na(dat$outcome)) > 0

   dat$popup <- paste0(
     "<strong>", dat$species, "</strong><br>",
     "<b>pH:</b> ", dat$ph, "<br>",
     "<b>OM:</b> ", dat$organic_matter, "%<br>",
     "<b>Texture:</b> ", dat$texture_class, "<br>",
     ifelse(!is.na(dat$outcome), paste0("<b>Outcome:</b> ", dat$outcome, "<br>"), ""),
     ifelse(!is.na(dat$sun_exposure), paste0("<b>Sun:</b> ", dat$sun_exposure, "<br>"), ""),
     ifelse(!is.na(dat$site_hydrology), paste0("<b>Hydrology:</b> ", dat$site_hydrology, "<br>"), ""),
     "<b>Date:</b> ", dat$date,
     ifelse(!is.na(dat$ecoregion_l4), paste0("<br><b>Ecoregion:</b> ", dat$ecoregion_l4), "")
   )

   # Assign colors based on outcome
   if (has_outcome) {
     dat$marker_color <- sapply(dat$outcome, function(o) {
       if (is.na(o)) edaphic_colors$muted else outcome_colors[o]
     })
   } else {
     dat$marker_color <- edaphic_colors$accent
   }

   leaflet(dat) %>%
     addProviderTiles(providers$CartoDB.Positron) %>%
     addCircleMarkers(
       lng = ~location_long, lat = ~location_lat,
       radius = 8, color = edaphic_colors$primary,
       fillColor = ~marker_color, fillOpacity = 0.7,
       stroke = TRUE, weight = 2, popup = ~popup
     ) %>%
     setView(lng = mean(dat$location_long), lat = mean(dat$location_lat), zoom = 5)
 })

 # --- Plant Performance ---
 output$performance_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("seedling", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- filtered_species_data()
   if (nrow(dat) == 0) {
     return(empty_state("seedling", "No Data", "No samples match current filters"))
   }

   # Check if we have any outcome data
   has_outcome <- sum(!is.na(dat$outcome)) > 0
   has_sun <- sum(!is.na(dat$sun_exposure)) > 0
   has_hydro <- sum(!is.na(dat$site_hydrology)) > 0

   if (!has_outcome && !has_sun && !has_hydro) {
     return(div(
       class = "p-4 text-center",
       icon("info-circle", class = "fa-2x text-muted mb-3"),
       tags$h5("No Performance Data Yet"),
       tags$p(class = "text-muted",
              "This tab shows plant outcome, sun exposure, and hydrology data. ",
              "Add this information when entering new samples to see performance analysis.")
     ))
   }

   tagList(
     layout_column_wrap(
       width = 1/2,
       card(
         card_header("Outcome Distribution"),
         card_body(plotlyOutput("performance_outcome_plot", height = "300px"))
       ),
       card(
         card_header(icon("lightbulb"), " Key Insights"),
         card_body(uiOutput("performance_insights"))
       )
     ),
     layout_column_wrap(
       width = 1/2,
       card(
         card_header("Sun Exposure"),
         card_body(plotlyOutput("performance_sun_plot", height = "250px"))
       ),
       card(
         card_header("Site Hydrology"),
         card_body(plotlyOutput("performance_hydro_plot", height = "250px"))
       )
     ),
     # Success Factors section - selectable soil parameters by outcome
     if (has_outcome) {
       tagList(
         tags$h5(class = "mt-4 mb-3", icon("search"), " Success Factors"),
         tags$p(class = "text-muted small mb-3",
                "Compare soil conditions between thriving and struggling/failed plants to identify optimal ranges."),
         layout_column_wrap(
           width = 1/2,
           card(
             card_header(
               class = "d-flex justify-content-between align-items-center",
               span(icon("chart-bar"), " Parameter by Outcome"),
               selectInput("success_factor_param", NULL,
                           choices = c("pH" = "ph",
                                       "Organic Matter (%)" = "organic_matter",
                                       "Clay Content (%)" = "texture_clay",
                                       "Sand Content (%)" = "texture_sand",
                                       "Nitrate (ppm)" = "nitrate_ppm",
                                       "Phosphorus (ppm)" = "phosphorus_ppm",
                                       "Potassium (ppm)" = "potassium_ppm",
                                       "Calcium (ppm)" = "calcium_ppm"),
                           selected = "ph",
                           width = "180px")
             ),
             card_body(plotlyOutput("success_factor_plot", height = "320px"))
           ),
           # Condition matrix heatmap
           if (has_sun && has_hydro) {
             card(
               card_header(icon("th"), " Success Matrix: Sun Ã— Hydrology"),
               card_body(
                 plotlyOutput("success_matrix_plot", height = "320px"),
                 tags$p(class = "text-muted small mt-2",
                        "Success rate (% Thriving + Established) by condition. Darker green = higher success.")
               )
             )
           } else {
             card(
               card_header(icon("info-circle"), " More Data Needed"),
               card_body(
                 class = "text-center text-muted",
                 tags$p("Record sun exposure and hydrology data to see the Success Matrix visualization.")
               )
             )
           }
         )
       )
     }
   )
 })

 # Outcome distribution bar chart
 output$performance_outcome_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$outcome), ]
   if (nrow(dat) == 0) return(NULL)

   outcome_counts <- as.data.frame(table(dat$outcome))
   names(outcome_counts) <- c("Outcome", "Count")

   # Order outcomes logically
   outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
   outcome_counts$Outcome <- factor(outcome_counts$Outcome, levels = outcome_order)
   outcome_counts <- outcome_counts[order(outcome_counts$Outcome), ]

   # Colors: green for thriving, yellow for established, orange for struggling, red for failed
   outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                       "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")

   p <- ggplot(outcome_counts, aes(x = Outcome, y = Count, fill = Outcome)) +
     geom_bar(stat = "identity") +
     scale_fill_manual(values = outcome_colors, breaks = outcome_order) +
     labs(x = "", y = "Number of Plants") +
     theme_minimal() +
     theme(legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1))

   ggplotly(p, tooltip = c("x", "y")) %>%
     layout(margin = list(b = 80))
 })

 # Key insights based on outcome data
 output$performance_insights <- renderUI({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$outcome), ]
   if (nrow(dat) < 3) {
     return(div(class = "text-muted", "Need more outcome data to generate insights."))
   }

   insights <- list()
   outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                       "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")

   # Calculate success
   dat$success <- dat$outcome %in% c("Thriving", "Established")
   overall_success <- round(mean(dat$success) * 100)

   # Best sun exposure
   if (sum(!is.na(dat$sun_exposure)) >= 3) {
     sun_rates <- aggregate(success ~ sun_exposure, data = dat[!is.na(dat$sun_exposure), ],
                            FUN = function(x) c(rate = mean(x) * 100, n = length(x)))
     sun_rates <- do.call(data.frame, sun_rates)
     names(sun_rates) <- c("sun_exposure", "rate", "n")
     sun_rates <- sun_rates[sun_rates$n >= 2, ]  # Need at least 2 samples
     if (nrow(sun_rates) > 0) {
       best_sun <- sun_rates[which.max(sun_rates$rate), ]
       if (best_sun$rate >= 50) {
         insights$sun <- div(class = "mb-2",
           icon("sun", class = "text-warning me-2"),
           tags$strong("Best sun: "), best_sun$sun_exposure,
           tags$span(class = "text-success ms-1", sprintf("(%d%% success)", round(best_sun$rate)))
         )
       }
     }
   }

   # Best hydrology
   if (sum(!is.na(dat$site_hydrology)) >= 3) {
     hydro_rates <- aggregate(success ~ site_hydrology, data = dat[!is.na(dat$site_hydrology), ],
                              FUN = function(x) c(rate = mean(x) * 100, n = length(x)))
     hydro_rates <- do.call(data.frame, hydro_rates)
     names(hydro_rates) <- c("site_hydrology", "rate", "n")
     hydro_rates <- hydro_rates[hydro_rates$n >= 2, ]
     if (nrow(hydro_rates) > 0) {
       best_hydro <- hydro_rates[which.max(hydro_rates$rate), ]
       if (best_hydro$rate >= 50) {
         insights$hydro <- div(class = "mb-2",
           icon("droplet", class = "text-info me-2"),
           tags$strong("Best hydrology: "), best_hydro$site_hydrology,
           tags$span(class = "text-success ms-1", sprintf("(%d%% success)", round(best_hydro$rate)))
         )
       }
     }
   }

   # pH comparison between success and failure
   if (sum(!is.na(dat$ph)) >= 4) {
     success_ph <- dat$ph[dat$success & !is.na(dat$ph)]
     fail_ph <- dat$ph[!dat$success & !is.na(dat$ph)]
     if (length(success_ph) >= 2 && length(fail_ph) >= 2) {
       success_ph_range <- sprintf("%.1fâ€“%.1f", min(success_ph), max(success_ph))
       fail_ph_range <- sprintf("%.1fâ€“%.1f", min(fail_ph), max(fail_ph))
       insights$ph <- div(class = "mb-2",
         icon("flask", class = "text-secondary me-2"),
         tags$strong("pH patterns: "),
         tags$span(class = "text-success", sprintf("Success: %s", success_ph_range)),
         " vs ",
         tags$span(class = "text-danger", sprintf("Fail: %s", fail_ph_range))
       )
     }
   }

   # Worst conditions (if clear pattern)
   if (sum(!is.na(dat$sun_exposure)) >= 3 || sum(!is.na(dat$site_hydrology)) >= 3) {
     avoid_items <- c()
     if (exists("sun_rates") && nrow(sun_rates) > 0) {
       worst_sun <- sun_rates[which.min(sun_rates$rate), ]
       if (worst_sun$rate < 40 && worst_sun$n >= 2) {
         avoid_items <- c(avoid_items, worst_sun$sun_exposure)
       }
     }
     if (exists("hydro_rates") && nrow(hydro_rates) > 0) {
       worst_hydro <- hydro_rates[which.min(hydro_rates$rate), ]
       if (worst_hydro$rate < 40 && worst_hydro$n >= 2) {
         avoid_items <- c(avoid_items, worst_hydro$site_hydrology)
       }
     }
     if (length(avoid_items) > 0) {
       insights$avoid <- div(class = "mb-2",
         icon("triangle-exclamation", class = "text-warning me-2"),
         tags$strong("Avoid: "),
         tags$span(class = "text-danger", paste(avoid_items, collapse = ", "))
       )
     }
   }

   # Build output
   if (length(insights) == 0) {
     return(div(
       class = "text-center py-3",
       div(class = "display-6 mb-2", paste0(overall_success, "%")),
       div(class = "text-muted small", "Overall success rate"),
       tags$hr(),
       div(class = "text-muted small", "Add more samples with varied conditions to see specific insights.")
     ))
   }

   tagList(
     div(class = "d-flex align-items-center mb-3",
         div(class = "display-6 me-3", style = paste0("color: ", if(overall_success >= 60) "#27ae60" else if(overall_success >= 40) "#f39c12" else "#e74c3c"),
             paste0(overall_success, "%")),
         div(tags$small(class = "text-muted", "overall success"))
     ),
     tags$hr(class = "my-2"),
     do.call(tagList, insights)
   )
 })

 # Sun exposure distribution
 output$performance_sun_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$sun_exposure), ]
   if (nrow(dat) == 0) return(NULL)

   sun_counts <- as.data.frame(table(dat$sun_exposure))
   names(sun_counts) <- c("Sun", "Count")

   # Order logically
   sun_order <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade")
   sun_counts$Sun <- factor(sun_counts$Sun, levels = sun_order)

   p <- ggplot(sun_counts, aes(x = Sun, y = Count, fill = Sun)) +
     geom_bar(stat = "identity") +
     scale_fill_manual(values = c("Full Sun" = "#f1c40f", "Part Sun" = "#f5d76e",
                                  "Part Shade" = "#95a5a6", "Full Shade" = "#7f8c8d")) +
     labs(x = "", y = "Count") +
     theme_minimal() +
     theme(legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1))

   ggplotly(p, tooltip = c("x", "y")) %>%
     layout(margin = list(b = 60))
 })

 # Hydrology distribution
 output$performance_hydro_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$site_hydrology), ]
   if (nrow(dat) == 0) return(NULL)

   hydro_counts <- as.data.frame(table(dat$site_hydrology))
   names(hydro_counts) <- c("Hydrology", "Count")

   # Order logically
   hydro_order <- c("Dry", "Mesic", "Wet")
   hydro_counts$Hydrology <- factor(hydro_counts$Hydrology, levels = hydro_order)

   p <- ggplot(hydro_counts, aes(x = Hydrology, y = Count, fill = Hydrology)) +
     geom_bar(stat = "identity") +
     scale_fill_manual(values = c("Dry" = "#e67e22", "Mesic" = "#27ae60", "Wet" = "#3498db")) +
     labs(x = "", y = "Count") +
     theme_minimal() +
     theme(legend.position = "none")

   ggplotly(p, tooltip = c("x", "y"))
 })

 # --- Success Factors: Parameter distributions by outcome ---

 # Helper to create outcome boxplot
 create_outcome_boxplot <- function(dat, param, param_label, unit = "") {
   dat <- dat[!is.na(dat$outcome) & !is.na(dat[[param]]), ]
   if (nrow(dat) < 3) return(NULL)

   outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
   dat$outcome <- factor(dat$outcome, levels = outcome_order)
   outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                       "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")

   # Rename param column to 'value' to avoid scoping issues
   dat$value <- dat[[param]]

   p <- ggplot(dat, aes(x = outcome, y = value, fill = outcome)) +
     geom_boxplot(alpha = 0.8, outlier.shape = 21) +
     geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
     scale_fill_manual(values = outcome_colors, breaks = outcome_order) +
     labs(x = "", y = paste0(param_label, if (nzchar(unit)) paste0(" (", unit, ")") else "")) +
     theme_minimal() +
     theme(legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1))

   ggplotly(p, tooltip = c("y")) %>%
     layout(margin = list(b = 80))
 }

 # Dynamic Success Factor plot based on dropdown selection
 output$success_factor_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   req(input$success_factor_param)
   dat <- filtered_species_data()

   # Parameter labels and units
   param_info <- list(
     ph = list(label = "pH", unit = ""),
     organic_matter = list(label = "Organic Matter", unit = "%"),
     texture_clay = list(label = "Clay Content", unit = "%"),
     texture_sand = list(label = "Sand Content", unit = "%"),
     nitrate_ppm = list(label = "Nitrate", unit = "ppm"),
     phosphorus_ppm = list(label = "Phosphorus", unit = "ppm"),
     potassium_ppm = list(label = "Potassium", unit = "ppm"),
     calcium_ppm = list(label = "Calcium", unit = "ppm")
   )

   param <- input$success_factor_param
   info <- param_info[[param]]
   if (is.null(info)) info <- list(label = param, unit = "")

   create_outcome_boxplot(dat, param, info$label, info$unit)
 })

 # Success Matrix: Sun Ã— Hydrology heatmap
 output$success_matrix_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()
   dat <- dat[!is.na(dat$outcome) & !is.na(dat$sun_exposure) & !is.na(dat$site_hydrology), ]
   if (nrow(dat) < 3) return(NULL)

   # Calculate success rate for each sun Ã— hydrology combination
   dat$success <- dat$outcome %in% c("Thriving", "Established")

   # Create all possible combinations
   sun_levels <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade")
   hydro_levels <- c("Dry", "Mesic", "Wet")

   matrix_data <- expand.grid(sun_exposure = sun_levels, site_hydrology = hydro_levels,
                              stringsAsFactors = FALSE)

   # Calculate success rate for each combination
   matrix_data$success_rate <- sapply(1:nrow(matrix_data), function(i) {
     subset_dat <- dat[dat$sun_exposure == matrix_data$sun_exposure[i] &
                         dat$site_hydrology == matrix_data$site_hydrology[i], ]
     if (nrow(subset_dat) == 0) return(NA)
     round(mean(subset_dat$success) * 100, 0)
   })

   matrix_data$n <- sapply(1:nrow(matrix_data), function(i) {
     nrow(dat[dat$sun_exposure == matrix_data$sun_exposure[i] &
                dat$site_hydrology == matrix_data$site_hydrology[i], ])
   })

   matrix_data$label <- ifelse(is.na(matrix_data$success_rate), "",
                               paste0(matrix_data$success_rate, "%\n(n=", matrix_data$n, ")"))

   # Order factors
   matrix_data$sun_exposure <- factor(matrix_data$sun_exposure, levels = sun_levels)
   matrix_data$site_hydrology <- factor(matrix_data$site_hydrology, levels = hydro_levels)

   p <- ggplot(matrix_data, aes(x = site_hydrology, y = sun_exposure, fill = success_rate)) +
     geom_tile(color = "white", size = 1) +
     geom_text(aes(label = label), color = "white", fontface = "bold", size = 3.5) +
     scale_fill_gradient(low = "#e74c3c", high = "#27ae60", na.value = "#cccccc",
                         limits = c(0, 100), name = "Success %") +
     labs(x = "Site Hydrology", y = "Sun Exposure") +
     theme_minimal() +
     theme(panel.grid = element_blank(),
           axis.text = element_text(size = 10))

   ggplotly(p, tooltip = c("fill")) %>%
     config(displayModeBar = FALSE)
 })

 # --- Raw Data ---
 output$raw_data <- renderDT({
   req(input$analysis_species, input$analysis_species != "")
   dat <- filtered_species_data()  # Use filtered data
   if (nrow(dat) == 0) return(NULL)
   datatable(dat, options = list(scrollX = TRUE, pageLength = 15))
 })

 # --- USDA Traits ---
 output$traits_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("book", "No Species Selected", "Choose a species from the sidebar"))
   }
   tr <- get_usda_traits_for_name(input$analysis_species, pool)
   if (is.null(tr) || nrow(tr) == 0) {
     return(empty_state("book", "No USDA Data", paste("No USDA traits found for", input$analysis_species)))
   }
   tagList(
     card_body(
       tags$h5(class = "mb-3", icon("leaf"), " USDA Plant Characteristics"),
       tableOutput("traits_table")
     ),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "Reference data from the USDA PLANTS Database. These values represent typical growing conditions and tolerances ",
            "observed for this species, serving as a baseline to compare against your actual soil samples.")
   )
 })

 output$traits_table <- renderTable({
   req(input$analysis_species, input$analysis_species != "")
   tr <- get_usda_traits_for_name(input$analysis_species, pool)
   if (is.null(tr) || nrow(tr) == 0) return(NULL)

   # Build display data manually with unit conversions
   result <- data.frame(
     Trait = character(),
     Value = character(),
     stringsAsFactors = FALSE
   )

   add_row <- function(label, value) {
     if (!is.null(value) && !is.na(value) && nzchar(as.character(value)) && as.character(value) != "NA") {
       result <<- rbind(result, data.frame(Trait = label, Value = as.character(value), stringsAsFactors = FALSE))
     }
   }

   add_row("USDA Symbol", tr$usda_symbol[1])
   add_row("Scientific Name", tr$scientific_name[1])

   # Basic info (from new table)
   if ("duration" %in% names(tr)) add_row("Duration", tr$duration[1])
   if ("growth_habit" %in% names(tr)) add_row("Growth Habit", tr$growth_habit[1])
   if ("native_status" %in% names(tr)) add_row("Native Status", tr$native_status[1])

   # pH range
   ph_min <- tr$soil_ph_min[1]
   ph_max <- tr$soil_ph_max[1]
   if (!is.null(ph_min) && !is.na(ph_min) && !is.null(ph_max) && !is.na(ph_max)) {
     add_row("Soil pH Range", sprintf("%.1f - %.1f", ph_min, ph_max))
   }

   # Tolerances
   add_row("Shade Tolerance", tr$shade_tolerance[1])
   add_row("Drought Tolerance", tr$drought_tolerance[1])
   add_row("Salinity Tolerance", tr$salinity_tolerance[1])
   if ("moisture_use" %in% names(tr)) add_row("Moisture Use", tr$moisture_use[1])
   if ("bloom_period" %in% names(tr)) add_row("Bloom Period", tr$bloom_period[1])

   # Soil texture (legacy table only)
   if (all(c("soil_texture_coarse", "soil_texture_medium", "soil_texture_fine") %in% names(tr))) {
     texture <- format_soil_texture(tr$soil_texture_coarse[1], tr$soil_texture_medium[1], tr$soil_texture_fine[1])
     add_row("Adapted Soil Textures", texture)
   }

   # Precipitation - check for both formats
   if ("precip_min_in" %in% names(tr) && !is.na(tr$precip_min_in[1]) && !is.na(tr$precip_max_in[1])) {
     add_row("Precipitation Range", sprintf("%.0f - %.0f in", tr$precip_min_in[1], tr$precip_max_in[1]))
   } else if ("precipitation_min_mm" %in% names(tr) && !is.na(tr$precipitation_min_mm[1])) {
     precip_min_in <- round(tr$precipitation_min_mm[1] / 25.4, 1)
     precip_max_in <- round(tr$precipitation_max_mm[1] / 25.4, 1)
     add_row("Precipitation Range", sprintf("%.1f - %.1f in", precip_min_in, precip_max_in))
   }

   # Temperature - check for both formats
   if ("temp_min_f" %in% names(tr) && !is.na(tr$temp_min_f[1])) {
     add_row("Minimum Temperature", sprintf("%.0fÂ°F", tr$temp_min_f[1]))
   } else if ("min_temp_c" %in% names(tr) && !is.na(tr$min_temp_c[1])) {
     temp_f <- as.integer(round(tr$min_temp_c[1] * 9 / 5 + 32))
     add_row("Minimum Temperature", sprintf("%dÂ°F", temp_f))
   }

   result
 }, striped = TRUE, hover = TRUE, width = "100%")

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
