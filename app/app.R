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

# --- Initialize
db_migrate()
onStop(function() poolClose(pool))

species_db <- load_species_db()
eco_sf <- load_ecoregions()

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
 is_invite_required = FALSE,
 is_email_verification_required = FALSE
)

# ---------------------------
# UI
# ---------------------------

base_ui <- page_navbar(
 title = span(
   HTML('<svg width="24" height="20" viewBox="0 0 345 279" style="vertical-align: middle; margin-right: 6px;"><path d="M345 279C345 186.448 265.385 -18.4897 0 1.34294C0 173.226 112.788 279 345 279Z" fill="currentColor"/></svg>'),
   "Edaphic Flora"
 ),
 theme = edaphic_bs_theme(),
 fillable = TRUE,

 header = tagList(
   edaphic_css(),
   tags$head(
     tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
     tags$link(rel = "stylesheet",
               href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
   )
 ),

 # ========== WELCOME TAB ==========
 nav_panel(
   title = "Welcome",
   icon = icon("home"),
   layout_columns(
     col_widths = c(8, 4),

     # Main welcome content
     card(
       card_header(
         class = "bg-transparent border-0 pt-4",
         div(class = "text-center",
             HTML('<svg width="60" height="48" viewBox="0 0 345 279" style="margin-bottom: 1rem;"><path d="M345 279C345 186.448 265.385 -18.4897 0 1.34294C0 173.226 112.788 279 345 279Z" fill="#7A9A86"/></svg>'),
             h2("Welcome to Edaphic Flora"),
             p(class = "lead text-muted", "A community database for soil conditions and plant species")
         )
       ),
       card_body(
         div(class = "px-md-5",
             h4("What is Edaphic Flora?"),
             p("Edaphic Flora helps gardeners, horticulturists, and researchers understand the relationship ",
               "between soil conditions and plant success. By collecting real-world soil data from locations ",
               "where specific plants grow, we build a reference database that can guide planting decisions."),

             h4(class = "mt-4", "How It Works"),
             tags$ol(
               tags$li(tags$strong("Submit soil data"), " - Enter soil test results along with the species growing in that soil"),
               tags$li(tags$strong("Include location"), " - Geocode your sample location for ecoregion analysis"),
               tags$li(tags$strong("Analyze patterns"), " - View pH distributions, nutrient levels, and texture profiles"),
               tags$li(tags$strong("Compare to references"), " - See how your data compares to USDA plant characteristics")
             ),

             h4(class = "mt-4", "Getting Started"),
             p("Head to the ", tags$strong("Data Entry"), " tab to submit your first soil sample. ",
               "You can enter data for multiple species that share the same soil conditions."),
             p("Use the ", tags$strong("Analysis"), " tab to explore existing data by species."),

             div(class = "mt-4 p-3 bg-light rounded",
                 tags$small(class = "text-muted",
                            icon("info-circle"), " ",
                            tags$strong("Data Usage: "),
                            "All submitted data is shared under CC BY-NC 4.0. ",
                            "By contributing, you help build a free resource for the gardening and research community."))
         )
       )
     ),

     # Quick stats sidebar
     card(
       card_header(icon("chart-simple"), "Database Stats"),
       card_body(
         uiOutput("welcome_stats")
       )
     )
   )
 ),

 # ========== DATA ENTRY TAB ==========
 nav_panel(
   title = "Data Entry",
   icon = icon("plus-circle"),
   layout_sidebar(
     sidebar = sidebar(
       title = "Add Soil Sample",
       width = 380,
       bg = "#f8f9fa",

       # Species (always visible - most important)
       selectizeInput("species", "Plant Species", choices = NULL, multiple = TRUE,
                      options = list(maxItems = 20, maxOptions = 100,
                                     placeholder = "Type to search species...")),
       uiOutput("species_count_indicator"),

       # Per-species metadata (dynamic based on selected species)
       uiOutput("per_species_fields"),

       helpText(class = "text-muted small",
                "Tip: Select multiple species if they share the same soil sample."),

       accordion(
         id = "form_sections",
         open = "soil_props",

         # Soil Properties
         accordion_panel(
           title = "Soil Properties",
           value = "soil_props",
           icon = icon("flask"),
           numericInput("ph", "Soil pH", value = 7.0, min = 0, max = 14, step = 0.1),
           numericInput("organic_matter", "Organic Matter (%)", value = 2, min = 0, max = 100, step = 0.1)
         ),

         # Nutrients
         accordion_panel(
           title = "Nutrients (ppm)",
           value = "nutrients",
           icon = icon("leaf"),
           layout_column_wrap(
             width = 1/2,
             numericInput("nitrate", "Nitrate", value = 0, min = 0),
             numericInput("ammonium", "Ammonium", value = 0, min = 0),
             numericInput("phosphorus", "Phosphorus", value = 0, min = 0),
             numericInput("potassium", "Potassium", value = 0, min = 0),
             numericInput("calcium", "Calcium", value = 0, min = 0),
             numericInput("magnesium", "Magnesium", value = 0, min = 0)
           ),
           numericInput("soluble_salts", "Soluble Salts", value = 0, min = 0)
         ),

         # Texture
         accordion_panel(
           title = "Soil Texture",
           value = "texture",
           icon = icon("mountain"),
           radioButtons("texture_input_type", "Input Method:",
                        choices = c("Percentages" = "pct", "Classification" = "class"),
                        inline = TRUE),
           conditionalPanel(
             condition = "input.texture_input_type == 'pct'",
             layout_column_wrap(
               width = 1/3,
               numericInput("sand", "Sand %", value = 33, min = 0, max = 100),
               numericInput("silt", "Silt %", value = 33, min = 0, max = 100),
               numericInput("clay", "Clay %", value = 34, min = 0, max = 100)
             ),
             uiOutput("texture_validation")
           ),
           conditionalPanel(
             condition = "input.texture_input_type == 'class'",
             selectInput("texture_class", "Texture Class", choices = soil_texture_classes$Texture)
           )
         ),

         # Location
         accordion_panel(
           title = "Location",
           value = "location",
           icon = icon("map-marker-alt"),
           textInput("street", "Street Address (optional)", ""),
           textInput("city", "City/Town", ""),
           selectInput("state", "State", choices = state.name, selected = "New York"),
           actionButton("geocode", "Get Coordinates", class = "btn-info btn-sm w-100",
                        icon = icon("search-location")),
           div(class = "mt-2 small", uiOutput("geocode_status")),
           hr(),
           layout_column_wrap(
             width = 1/2,
             numericInput("latitude", "Latitude", value = 0, min = -90, max = 90, step = 0.0001),
             numericInput("longitude", "Longitude", value = 0, min = -180, max = 180, step = 0.0001)
           )
         ),

         # Additional
         accordion_panel(
           title = "Additional Info",
           value = "additional",
           icon = icon("info-circle"),
           dateInput("date", "Sample Date", value = Sys.Date()),
           textAreaInput("notes", "Notes", "", height = "80px",
                         placeholder = "General notes about this soil sample...")
         )
       ),

       hr(),
       actionButton("submit", "Submit Sample", class = "btn-primary btn-lg w-100",
                    icon = icon("paper-plane"))
     ),

     # Main content - recent entries
     card(
       card_header(
         class = "d-flex justify-content-between align-items-center",
         span(icon("clock"), "Recent Entries"),
         span(class = "badge bg-secondary", textOutput("entry_count", inline = TRUE))
       ),
       card_body(
         DTOutput("recent_entries")
       )
     )
   )
 ),

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
       hr(),
       checkboxInput("show_usda_ref", "Show USDA reference overlays", TRUE),
       uiOutput("reference_msg")
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
         title = "pH Distribution",
         icon = icon("chart-bar"),
         uiOutput("ph_plot_ui")
       ),
       nav_panel(
         title = "pH vs Organic Matter",
         icon = icon("chart-scatter"),
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

 # ========== DATA MANAGEMENT TAB ==========
 nav_panel(
   title = "Data",
   icon = icon("database"),
   layout_columns(
     col_widths = c(12),

     card(
       card_header(icon("file-export"), "Export Data"),
       card_body(
         p("Download all soil sample data as a CSV file."),
         downloadButton("export_data", "Export All Data", class = "btn-primary")
       )
     ),

     card(
       card_header(
         class = "d-flex justify-content-between align-items-center",
         span(icon("file-import"), "Advanced: Bulk CSV Import"),
         tags$small(class = "text-muted", "For importing historical data")
       ),
       card_body(
         accordion(
           id = "import_accordion",
           open = FALSE,
           accordion_panel(
             title = "Show Import Options",
             value = "import_panel",
             icon = icon("upload"),
             p(class = "text-muted mb-3",
               "Use CSV import for bulk historical data. For new entries, ",
               "use the Data Entry tab which supports multiple species per soil sample."),
             fileInput("csv_import", "Upload CSV File",
                       accept = c("text/csv", ".csv"),
                       buttonLabel = "Browse...",
                       placeholder = "No file selected"),
             downloadButton("download_template", "Download Template", class = "btn-outline-secondary btn-sm")
           )
         )
       )
     )
   )
 ),

 # ========== HELP MENU ==========
 nav_menu(
   title = "Help",
   icon = icon("circle-question"),
   align = "right",

   # Help Page
   nav_panel(
     title = "Field Guide",
     icon = icon("book-open"),
     layout_columns(
       col_widths = 12,
       card(
         card_header(icon("book-open"), "Field Guide"),
         card_body(
           class = "px-md-5",

           h4("Soil Properties"),
           tags$dl(class = "row",
             tags$dt(class = "col-sm-3", "Soil pH"),
             tags$dd(class = "col-sm-9", "Measures acidity/alkalinity on a scale of 0-14. Most plants prefer 6.0-7.0. ",
                     "Values below 7 are acidic; above 7 are alkaline."),
             tags$dt(class = "col-sm-3", "Organic Matter (%)"),
             tags$dd(class = "col-sm-9", "Decomposed plant/animal material. Higher values (3-5%) indicate fertile soil ",
                     "with good water retention and microbial activity.")
           ),

           h4(class = "mt-4", "Nutrients (ppm)"),
           tags$dl(class = "row",
             tags$dt(class = "col-sm-3", "Nitrate (NO₃)"),
             tags$dd(class = "col-sm-9", "Primary nitrogen source for plants. Optimal: 25-50 ppm for most crops."),
             tags$dt(class = "col-sm-3", "Phosphorus (P)"),
             tags$dd(class = "col-sm-9", "Essential for root development and flowering. Optimal: 25-50 ppm."),
             tags$dt(class = "col-sm-3", "Potassium (K)"),
             tags$dd(class = "col-sm-9", "Supports overall plant health and disease resistance. Optimal: 150-250 ppm."),
             tags$dt(class = "col-sm-3", "Calcium (Ca)"),
             tags$dd(class = "col-sm-9", "Structural component of cell walls. Optimal: 1000-2000 ppm."),
             tags$dt(class = "col-sm-3", "Magnesium (Mg)"),
             tags$dd(class = "col-sm-9", "Central atom in chlorophyll. Optimal: 50-120 ppm."),
             tags$dt(class = "col-sm-3", "Soluble Salts"),
             tags$dd(class = "col-sm-9", "Total dissolved salts. High values (>1000 ppm) can stress plants.")
           ),

           h4(class = "mt-4", "Soil Texture"),
           p("Texture is determined by the proportions of sand, silt, and clay particles:"),
           tags$ul(
             tags$li(tags$strong("Sand"), " (0.05-2mm): Large particles, drains quickly, low nutrient retention"),
             tags$li(tags$strong("Silt"), " (0.002-0.05mm): Medium particles, good moisture retention"),
             tags$li(tags$strong("Clay"), " (<0.002mm): Tiny particles, holds water and nutrients but may drain poorly")
           ),
           p("The texture triangle in the Analysis tab shows your samples plotted by their sand/silt/clay percentages."),

           h4(class = "mt-4", "Analysis Charts Explained"),
           tags$dl(class = "row",
             tags$dt(class = "col-sm-3", "pH Distribution"),
             tags$dd(class = "col-sm-9", "Histogram showing the range of pH values for a species. ",
                     "The green shaded area shows the USDA reference pH range when available."),
             tags$dt(class = "col-sm-3", "pH vs Organic Matter"),
             tags$dd(class = "col-sm-9", "Scatter plot exploring the relationship between soil acidity and organic content. ",
                     "Points are colored by texture class."),
             tags$dt(class = "col-sm-3", "Correlations"),
             tags$dd(class = "col-sm-9", "Heatmap showing statistical correlations between soil parameters. ",
                     "Red = positive correlation, Blue = negative correlation.")
           )
         )
       )
     )
   ),

   # FAQ Page
   nav_panel(
     title = "FAQ",
     icon = icon("circle-question"),
     layout_columns(
       col_widths = 12,
       card(
         card_header(icon("circle-question"), "Frequently Asked Questions"),
         card_body(
           class = "px-md-5",

           h5("Where should my soil test come from?"),
           p(class = "text-muted mb-4",
             "Use results from a professional soil testing lab (university extension services, commercial labs) ",
             "or a reliable home soil test kit. For best results, collect samples from the root zone of the plants you're recording."),

           h5("Can I submit data for plants that died or failed?"),
           p(class = "text-muted mb-4",
             "Yes! Data from unsuccessful plantings is valuable—it helps identify soil conditions that certain species ",
             "struggle with. Use the Outcome dropdown (Thriving, Established, Struggling, or Failed/Died) for each species."),

           h5("What if I don't have all the soil test values?"),
           p(class = "text-muted mb-4",
             "Submit what you have. pH and organic matter are the most important. Leave other nutrient fields at 0 ",
             "or their defaults if you don't have that data."),

           h5("How are ecoregions determined?"),
           p(class = "text-muted mb-4",
             "When you enter coordinates, the app automatically identifies the EPA Level IV ecoregion. ",
             "This provides ecological context—plants in the same ecoregion often face similar conditions."),

           h5("What is USDA reference data?"),
           p(class = "text-muted mb-4",
             "The USDA PLANTS database provides characteristics for many species including preferred pH range, ",
             "drought tolerance, and shade tolerance. When available, this data is shown alongside your samples for comparison."),

           h5("Can I edit or delete my submissions?"),
           p(class = "text-muted mb-4",
             "Currently, direct editing isn't available in the app. Contact the administrator if you need to correct data."),

           h5("How is my data used?"),
           p(class = "text-muted mb-4",
             "All submitted data is shared under a CC BY-NC 4.0 license. It's freely available for non-commercial use ",
             "by researchers, gardeners, and educators. Your email is stored for attribution but not publicly displayed."),

           h5("What species can I enter?"),
           p(class = "text-muted",
             "The species search uses the World Checklist of Vascular Plants (WCVP), which includes over 360,000 accepted ",
             "plant species. If you can't find a species, try the scientific name without the author citation.")
         )
       )
     )
   )
 ),

 # ========== NAV SPACER & USER INFO ==========
 nav_spacer(),
 nav_item(
   tags$span(class = "navbar-text me-3", textOutput("user_display", inline = TRUE))
 )
)

ui <- polished::secure_ui(base_ui)

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

 data_changed <- reactiveVal(0)

 # --- Species dropdown population ---
 observe({
   updateSelectizeInput(session, "species",
                        choices = sort(species_db$taxon_name),
                        selected = character(0),
                        server = TRUE,
                        options = list(maxItems = 20, maxOptions = 100,
                                       placeholder = "Type to search species..."))
 })

 # --- Species count indicator ---
 output$species_count_indicator <- renderUI({
   n <- length(input$species)
   if (n == 0) return(NULL)
   div(class = "alert alert-info py-2 mb-2",
       icon("seedling"),
       sprintf(" %d species selected - %d record%s will be created",
               n, n, if (n == 1) "" else "s"))
 })

 # --- Per-species metadata fields ---
 output$per_species_fields <- renderUI({
   species_list <- input$species
   if (is.null(species_list) || length(species_list) == 0) return(NULL)

   # Create a collapsible card for per-species details
   card(
     class = "mb-3",
     card_header(
       class = "py-2",
       icon("list"), " Species Details",
       tags$small(class = "text-muted ms-2", "(per species)")
     ),
     card_body(
       class = "p-2",
       lapply(seq_along(species_list), function(i) {
         sp <- species_list[i]
         sp_id <- gsub("[^a-zA-Z0-9]", "_", sp)  # Safe ID

         div(
           class = "border rounded p-2 mb-2 bg-light",
           tags$strong(class = "d-block mb-2 text-truncate", title = sp,
                       sprintf("%d. %s", i, sp)),
           layout_column_wrap(
             width = 1/2,
             textInput(paste0("cultivar_", sp_id), "Cultivar",
                       placeholder = "e.g., 'Forest Pansy'"),
             selectInput(paste0("outcome_", sp_id), "Outcome",
                         choices = c("Select..." = "", "Thriving" = "Thriving",
                                     "Established" = "Established",
                                     "Struggling" = "Struggling",
                                     "Failed/Died" = "Failed/Died"),
                         selected = "")
           ),
           layout_column_wrap(
             width = 1/2,
             selectInput(paste0("sun_", sp_id), "Sun Exposure",
                         choices = c("Select..." = "", "Full Sun" = "Full Sun",
                                     "Part Sun" = "Part Sun",
                                     "Part Shade" = "Part Shade",
                                     "Full Shade" = "Full Shade"),
                         selected = ""),
             selectInput(paste0("hydrology_", sp_id), "Site Hydrology",
                         choices = c("Select..." = "", "Dry/Xeric" = "Dry",
                                     "Mesic" = "Mesic",
                                     "Wet/Hydric" = "Wet"),
                         selected = "")
           ),
           textInput(paste0("inat_", sp_id), "iNaturalist URL",
                     placeholder = "https://www.inaturalist.org/observations/...")
         )
       })
     )
   )
 })

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
   if (!ref$has_traits && !ref$has_nwpl) return(NULL)

   chip <- function(lbl, val) {
     if (is.null(val) || is.na(val) || !nzchar(as.character(val))) return(NULL)
     tags$span(class = "badge bg-light text-dark me-1 mb-1",
               style = "font-size: 0.75rem;",
               sprintf("%s: %s", lbl, val))
   }

   tagList(
     tags$h6(class = "text-muted mt-2 mb-2", icon("book-open"), " Reference Data"),
     div(class = "d-flex flex-wrap",
         chip("NWPL", ref$nwpl_indicator),
         if (ref$has_traits) {
           tr <- ref$traits[1, ]
           list(
             chip("USDA", tr$usda_symbol),
             chip("pH", if (!is.na(tr$soil_ph_min) && !is.na(tr$soil_ph_max))
               sprintf("%.1f-%.1f", tr$soil_ph_min, tr$soil_ph_max) else NA),
             chip("Shade", tr$shade_tolerance),
             chip("Drought", tr$drought_tolerance),
             chip("Salinity", tr$salinity_tolerance),
             chip("Precip", if (!is.na(tr$precipitation_min_mm) && !is.na(tr$precipitation_max_mm))
               sprintf("%d-%d mm", tr$precipitation_min_mm, tr$precipitation_max_mm) else NA)
           )
         }
     )
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

 # --- Entry count ---
 output$entry_count <- renderText({
   data_changed()
   n <- tryCatch({
     res <- dbGetQuery(pool, "SELECT COUNT(*) as n FROM soil_samples")
     res$n[1]
   }, error = function(e) 0)
   paste(n, "total")
 })

 # --- Welcome page stats ---
 output$welcome_stats <- renderUI({
   data_changed()
   stats <- tryCatch({
     list(
       samples = dbGetQuery(pool, "SELECT COUNT(*) as n FROM soil_samples")$n[1],
       species = dbGetQuery(pool, "SELECT COUNT(DISTINCT species) as n FROM soil_samples")$n[1],
       users = dbGetQuery(pool, "SELECT COUNT(DISTINCT created_by) as n FROM soil_samples")$n[1],
       ecoregions = dbGetQuery(pool, "SELECT COUNT(DISTINCT ecoregion_l4) as n FROM soil_samples WHERE ecoregion_l4 IS NOT NULL")$n[1]
     )
   }, error = function(e) list(samples = 0, species = 0, users = 0, ecoregions = 0))

   stat_box <- function(value, label, icon_name) {
     div(class = "text-center py-3 border-bottom",
         div(class = "fs-2 fw-bold", style = "color: #7A9A86;", value),
         div(class = "text-muted small", icon(icon_name), " ", label))
   }

   tagList(
     stat_box(stats$samples, "Soil Samples", "flask"),
     stat_box(stats$species, "Species", "seedling"),
     stat_box(stats$users, "Contributors", "users"),
     stat_box(stats$ecoregions, "Ecoregions", "map")
   )
 })

 # --- Recent entries table ---
 output$recent_entries <- renderDT({
   data_changed()
   dat <- db_get_all_samples()
   if (nrow(dat) == 0) return(NULL)

   display <- dat %>%
     select(id, species, outcome, ph, organic_matter, texture_class, date) %>%
     mutate(date = as.character(date),
            outcome = ifelse(is.na(outcome), "", outcome)) %>%
     head(20)

   datatable(display,
             options = list(pageLength = 10, dom = 'tip', scrollX = TRUE),
             rownames = FALSE,
             colnames = c("ID", "Species", "Outcome", "pH", "OM %", "Texture", "Date"))
 })

 # --- Texture validation ---
 output$texture_validation <- renderUI({
   req(input$texture_input_type == "pct")
   total <- input$sand + input$silt + input$clay
   texture <- classify_texture(input$sand, input$silt, input$clay, soil_texture_classes)

   if (abs(total - 100) > 0.1) {
     div(class = "alert alert-danger py-2 mt-2",
         icon("exclamation-triangle"),
         sprintf(" Total: %.1f%% (must equal 100%%)", total))
   } else {
     div(class = "alert alert-success py-2 mt-2",
         icon("check-circle"),
         sprintf(" %s (Total: 100%%)", texture))
   }
 })

 # --- Geocoding ---
 observeEvent(input$geocode, {
   req(input$city, input$state)

   output$geocode_status <- renderUI({
     div(class = "text-info", icon("spinner", class = "fa-spin"), " Looking up address...")
   })

   address <- paste(
     if (nzchar(input$street)) paste0(input$street, ", ") else "",
     input$city, ", ", input$state
   )

   res <- tryCatch({ geo(address = address, method = "osm") }, error = function(e) NULL)

   if (!is.null(res) && nrow(res) > 0 && !is.na(res$lat[1])) {
     updateNumericInput(session, "latitude", value = round(res$lat[1], 6))
     updateNumericInput(session, "longitude", value = round(res$long[1], 6))

     eco <- get_ecoregion(res$lat[1], res$long[1], eco_sf)

     output$geocode_status <- renderUI({
       tagList(
         div(class = "text-success", icon("check-circle"),
             sprintf(" Found: %.4f, %.4f", res$lat[1], res$long[1])),
         if (!is.na(eco$name)) {
           div(class = "text-muted mt-1", icon("map"), " ", eco$name)
         }
       )
     })
   } else {
     output$geocode_status <- renderUI({
       div(class = "text-danger", icon("times-circle"), " Address not found")
     })
   }
 })

 # --- Submit sample ---
 observeEvent(input$submit, {
   u <- current_user()
   if (is.null(u)) {
     showNotification("Please sign in to submit data.", type = "error")
     return()
   }

   # Validation
   species_list <- input$species
   if (is.null(species_list) || length(species_list) == 0) {
     showNotification("Please select at least one species.", type = "error")
     return()
   }

   if (input$texture_input_type == "pct") {
     if (abs((input$sand + input$silt + input$clay) - 100) > 0.1) {
       showNotification("Soil texture percentages must sum to 100%", type = "error")
       return()
     }
   }

   # Calculate shared values once
   eco <- get_ecoregion(input$latitude, input$longitude, eco_sf)
   texture_pcts <- if (input$texture_input_type == "class") {
     get_texture_percentages(input$texture_class, soil_texture_classes)
   } else NULL

   texture_class_val <- if (input$texture_input_type == "pct") {
     classify_texture(input$sand, input$silt, input$clay, soil_texture_classes)
   } else input$texture_class

   # Helper to safely get per-species input values
   get_sp_input <- function(prefix, sp) {
     sp_id <- gsub("[^a-zA-Z0-9]", "_", sp)
     val <- input[[paste0(prefix, "_", sp_id)]]
     if (is.null(val) || !nzchar(trimws(as.character(val)))) NA_character_ else trimws(val)
   }

   # Create one record per species with per-species metadata
   success_count <- 0
   for (sp in species_list) {
     new_data <- data.frame(
       species = sp,
       cultivar = get_sp_input("cultivar", sp),
       outcome = get_sp_input("outcome", sp),
       sun_exposure = get_sp_input("sun", sp),
       site_hydrology = get_sp_input("hydrology", sp),
       inat_url = get_sp_input("inat", sp),
       ph = input$ph,
       organic_matter = input$organic_matter,
       nitrate_ppm = input$nitrate,
       ammonium_ppm = input$ammonium,
       phosphorus_ppm = input$phosphorus,
       potassium_ppm = input$potassium,
       calcium_ppm = input$calcium,
       magnesium_ppm = input$magnesium,
       soluble_salts_ppm = input$soluble_salts,
       texture_class = texture_class_val,
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
     if (!is.null(sample_id)) success_count <- success_count + 1
   }

   if (success_count > 0) {
     showNotification(
       span(icon("check-circle"),
            sprintf(" %d sample%s added successfully!",
                    success_count, if (success_count == 1) "" else "s")),
       type = "message", duration = 3
     )
     data_changed(data_changed() + 1)

     # Reset form
     updateSelectizeInput(session, "species", selected = character(0))
   } else {
     showNotification("Error adding samples. Please try again.", type = "error")
   }
 })

 # --- Downloads ---
 output$download_template <- downloadHandler(
   filename = function() "soil_data_template.csv",
   content  = function(file) write.csv(soil_data_template[0, ], file, row.names = FALSE)
 )

 output$export_data <- downloadHandler(
   filename = function() paste0("soil_data_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
   content  = function(file) write.csv(db_get_all_samples(), file, row.names = FALSE)
 )

 # --- CSV Import ---
 observeEvent(input$csv_import, {
   req(input$csv_import)
   u <- current_user()
   if (is.null(u)) { showNotification("Please sign in.", type = "error"); return() }

   tryCatch({
     imported <- read.csv(input$csv_import$datapath, stringsAsFactors = FALSE)

     required_cols <- c("species", "ph", "organic_matter", "texture_sand",
                        "texture_silt", "texture_clay", "location_lat", "location_long")
     miss <- setdiff(required_cols, names(imported))

     if (length(miss) > 0) {
       showNotification(paste("Missing columns:", paste(miss, collapse = ", ")), type = "error")
       return()
     }

     count <- 0
     for (i in seq_len(nrow(imported))) {
       row <- imported[i, ]
       row$created_by <- u$user_uid
       if (db_add_sample(row)) count <- count + 1
     }

     showNotification(sprintf("Imported %d samples successfully!", count), type = "message")
     data_changed(data_changed() + 1)
   }, error = function(e) {
     showNotification(paste("Import error:", e$message), type = "error")
   })
 })

 # --- Species summary sidebar ---
 output$species_summary <- renderUI({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) == 0) return(NULL)

   tagList(
     div(class = "small text-muted mb-2", paste(nrow(dat), "samples")),
     tags$table(class = "table table-sm",
       tags$tr(tags$td("Avg pH"), tags$td(class = "text-end", round(mean(dat$ph, na.rm = TRUE), 2))),
       tags$tr(tags$td("Avg OM"), tags$td(class = "text-end", paste0(round(mean(dat$organic_matter, na.rm = TRUE), 1), "%"))),
       tags$tr(tags$td("Locations"), tags$td(class = "text-end", sum(!is.na(dat$location_lat))))
     )
   )
 })

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

 # --- Summary ---
 output$summary_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("search", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) == 0) {
     return(empty_state("database", "No Data", paste("No samples for", input$analysis_species)))
   }
   tableOutput("summary_stats")
 })

 output$summary_stats <- renderTable({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) == 0) return(NULL)

   data.frame(
     Measure = c("Number of Samples", "Average pH", "pH Range",
                 "Average Organic Matter (%)", "Average Nitrate (ppm)",
                 "Average Phosphorus (ppm)", "Average Potassium (ppm)"),
     Value = c(
       nrow(dat),
       round(mean(dat$ph, na.rm = TRUE), 2),
       paste(round(range(dat$ph, na.rm = TRUE), 2), collapse = " - "),
       round(mean(dat$organic_matter, na.rm = TRUE), 2),
       round(mean(dat$nitrate_ppm, na.rm = TRUE), 2),
       round(mean(dat$phosphorus_ppm, na.rm = TRUE), 2),
       round(mean(dat$potassium_ppm, na.rm = TRUE), 2)
     )
   )
 }, striped = TRUE, hover = TRUE, width = "100%")

 # --- pH Distribution (Plotly) ---
 output$ph_plot_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("chart-bar", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- db_get_species_data(input$analysis_species)
   # Allow reference-only mode if USDA data exists
   if (nrow(dat) == 0) {
     tr <- get_usda_traits_for_name(input$analysis_species, pool)
     if (is.null(tr) || nrow(tr) == 0 || is.na(tr$soil_ph_min) || is.na(tr$soil_ph_max)) {
       return(empty_state("database", "No Data", "No samples or USDA pH reference available"))
     }
   }
   plotlyOutput("ph_plot", height = "500px")
 })

 output$ph_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
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
     return(empty_state("chart-scatter", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- db_get_species_data(input$analysis_species)
   # Allow reference-only mode if USDA data exists
   if (nrow(dat) == 0) {
     tr <- get_usda_traits_for_name(input$analysis_species, pool)
     if (is.null(tr) || nrow(tr) == 0 || is.na(tr$soil_ph_min) || is.na(tr$soil_ph_max)) {
       return(empty_state("database", "No Data", "No samples or USDA pH reference available"))
     }
   }
   plotlyOutput("ph_om_plot", height = "500px")
 })

 output$ph_om_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
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

   p <- ggplot(dat, aes(x = ph, y = organic_matter))

   # Add USDA overlay if available (before points so it's behind)
   if (has_usda_ph) {
     p <- p + annotate("rect", xmin = tr$soil_ph_min[1], xmax = tr$soil_ph_max[1],
                       ymin = -Inf, ymax = Inf, alpha = 0.12, fill = edaphic_colors$accent)
   }

   p <- p +
     geom_smooth(method = "lm", se = TRUE, color = edaphic_colors$dark,
                 fill = edaphic_colors$light, alpha = 0.3, linetype = "dashed") +
     geom_point(aes(color = texture_class,
                    text = paste0("pH: ", ph, "\nOM: ", organic_matter, "%\nTexture: ", texture_class)),
                size = 3, alpha = 0.7) +
     scale_color_edaphic() +
     labs(title = paste("pH vs Organic Matter -", input$analysis_species),
          subtitle = paste("r =", round(cor_val, 3),
                           if (has_usda_ph) paste("| USDA pH:", tr$soil_ph_min[1], "-", tr$soil_ph_max[1]) else ""),
          x = "Soil pH", y = "Organic Matter (%)", color = "Texture") +
     theme_edaphic()

   ggplotly(p, tooltip = "text") %>%
     config(displayModeBar = TRUE, displaylogo = FALSE)
 })

 # --- Nutrients (Plotly) ---
 output$nutrient_plot_ui <- renderUI({
   if (is.null(input$analysis_species) || input$analysis_species == "") {
     return(empty_state("leaf", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) == 0) {
     return(empty_state("database", "No Data", "No samples available"))
   }
   plotlyOutput("nutrient_plot", height = "500px")
 })

 output$nutrient_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) == 0) return(NULL)

   nutrient_data <- dat %>%
     select(nitrate_ppm, phosphorus_ppm, potassium_ppm, calcium_ppm, magnesium_ppm) %>%
     pivot_longer(everything(), names_to = "nutrient", values_to = "value") %>%
     mutate(nutrient = case_when(
       nutrient == "nitrate_ppm" ~ "Nitrate",
       nutrient == "phosphorus_ppm" ~ "Phosphorus",
       nutrient == "potassium_ppm" ~ "Potassium",
       nutrient == "calcium_ppm" ~ "Calcium",
       nutrient == "magnesium_ppm" ~ "Magnesium"
     ))

   p <- ggplot(nutrient_data, aes(x = reorder(nutrient, value, FUN = median),
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
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) < 3) {
     return(empty_state("database", "Insufficient Data", "Need at least 3 samples for correlations"))
   }
   plotlyOutput("heatmap_plot", height = "550px")
 })

 output$heatmap_plot <- renderPlotly({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
   if (nrow(dat) < 3) return(NULL)

   numeric_cols <- c("ph", "organic_matter", "nitrate_ppm", "phosphorus_ppm",
                     "potassium_ppm", "calcium_ppm", "magnesium_ppm", "soluble_salts_ppm")
   nice_labels <- c(ph = "pH", organic_matter = "Organic Matter", nitrate_ppm = "Nitrate",
                    phosphorus_ppm = "Phosphorus", potassium_ppm = "Potassium",
                    calcium_ppm = "Calcium", magnesium_ppm = "Magnesium",
                    soluble_salts_ppm = "Soluble Salts")

   cor_matrix <- cor(dat[, numeric_cols], use = "complete.obs")
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
   dat <- db_get_species_data(input$analysis_species)
   dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
   if (nrow(dat) == 0) {
     return(empty_state("database", "No Texture Data", "No samples with texture data"))
   }
   plotOutput("texture_plot", height = "550px")
 })

 output$texture_plot <- renderPlot({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
   dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
   if (nrow(dat) == 0) return(NULL)

   p <- ggtern(dat, aes(x = texture_sand, y = texture_silt, z = texture_clay)) +
     geom_point(aes(color = texture_class), size = 4, alpha = 0.7) +
     theme_bw() + theme_showarrows() +
     labs(title = paste("Soil Texture -", input$analysis_species),
          subtitle = paste(nrow(dat), "samples")) +
     xlab("Sand (%)") + ylab("Silt (%)") + zlab("Clay (%)") +
     scale_color_edaphic() +
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
   dat <- db_get_species_data(input$analysis_species)
   dat <- dat[!is.na(dat$location_lat) & !is.na(dat$location_long), ]
   if (nrow(dat) == 0) {
     return(empty_state("map-marker-alt", "No Location Data", "No samples with coordinates"))
   }
   leafletOutput("map_plot", height = "550px")
 })

 output$map_plot <- renderLeaflet({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
   dat <- dat[!is.na(dat$location_lat) & !is.na(dat$location_long), ]
   if (nrow(dat) == 0) return(NULL)

   dat$popup <- paste0(
     "<strong>", dat$species, "</strong><br>",
     "<b>pH:</b> ", dat$ph, "<br>",
     "<b>OM:</b> ", dat$organic_matter, "%<br>",
     "<b>Texture:</b> ", dat$texture_class, "<br>",
     "<b>Date:</b> ", dat$date,
     ifelse(!is.na(dat$ecoregion_l4), paste0("<br><b>Ecoregion:</b> ", dat$ecoregion_l4), "")
   )

   leaflet(dat) %>%
     addProviderTiles(providers$CartoDB.Positron) %>%
     addCircleMarkers(
       lng = ~location_long, lat = ~location_lat,
       radius = 8, color = edaphic_colors$primary,
       fillColor = edaphic_colors$accent, fillOpacity = 0.7,
       stroke = TRUE, weight = 2, popup = ~popup
     ) %>%
     setView(lng = mean(dat$location_long), lat = mean(dat$location_lat), zoom = 5)
 })

 # --- Raw Data ---
 output$raw_data <- renderDT({
   req(input$analysis_species, input$analysis_species != "")
   dat <- db_get_species_data(input$analysis_species)
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
   card_body(
     tags$h5(class = "mb-3", icon("leaf"), " USDA Plant Characteristics"),
     tableOutput("traits_table")
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

   # pH range
   if (!is.na(tr$soil_ph_min[1]) && !is.na(tr$soil_ph_max[1])) {
     add_row("Soil pH Range", sprintf("%.1f - %.1f", tr$soil_ph_min[1], tr$soil_ph_max[1]))
   }

   # Tolerances
   add_row("Shade Tolerance", tr$shade_tolerance[1])
   add_row("Drought Tolerance", tr$drought_tolerance[1])
   add_row("Salinity Tolerance", tr$salinity_tolerance[1])

   # Soil texture
   texture <- format_soil_texture(tr$soil_texture_coarse[1], tr$soil_texture_medium[1], tr$soil_texture_fine[1])
   add_row("Adapted Soil Textures", texture)

   # Climate (convert from metric)
   if (!is.na(tr$precipitation_min_mm[1]) && !is.na(tr$precipitation_max_mm[1])) {
     precip_min_in <- round(tr$precipitation_min_mm[1] / 25.4, 1)
     precip_max_in <- round(tr$precipitation_max_mm[1] / 25.4, 1)
     add_row("Precipitation Range", sprintf("%.1f - %.1f in (%d - %d mm)",
             precip_min_in, precip_max_in, tr$precipitation_min_mm[1], tr$precipitation_max_mm[1]))
   }

   if (!is.na(tr$min_temp_c[1])) {
     temp_f <- as.integer(round(tr$min_temp_c[1] * 9 / 5 + 32))
     add_row("Minimum Temperature", sprintf("%d°F (%d°C)", temp_f, tr$min_temp_c[1]))
   }

   result
 }, striped = TRUE, hover = TRUE, width = "100%")
}

server <- polished::secure_server(server_inner)
shinyApp(ui, server)
