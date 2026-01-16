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
             h1(class = "welcome-brand mb-3",
                span(class = "brand-name-edaphic", "edaphic"),
                " ",
                span(class = "brand-name-flora", "flora")),
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
           numericInput("ph", "Soil pH", value = NA, min = 0, max = 14, step = 0.1),
           numericInput("organic_matter", "Organic Matter (%)", value = NA, min = 0, max = 100, step = 0.1),
           numericInput("cec", "Cation Exchange Capacity (meq/100g)", value = NA, min = 0, step = 0.1),
           numericInput("soluble_salts", "Soluble Salts (ppm)", value = NA, min = 0)
         ),

         # Macronutrients
         accordion_panel(
           title = "Macronutrients (ppm)",
           value = "macronutrients",
           icon = icon("leaf"),
           layout_column_wrap(
             width = 1/2,
             numericInput("nitrate", "Nitrate (N)", value = NA, min = 0),
             numericInput("ammonium", "Ammonium (N)", value = NA, min = 0),
             numericInput("phosphorus", "Phosphorus (P)", value = NA, min = 0),
             numericInput("potassium", "Potassium (K)", value = NA, min = 0),
             numericInput("calcium", "Calcium (Ca)", value = NA, min = 0),
             numericInput("magnesium", "Magnesium (Mg)", value = NA, min = 0),
             numericInput("sulfur", "Sulfur (S)", value = NA, min = 0)
           )
         ),

         # Micronutrients
         accordion_panel(
           title = "Micronutrients (ppm)",
           value = "micronutrients",
           icon = icon("seedling"),
           layout_column_wrap(
             width = 1/2,
             numericInput("iron", "Iron (Fe)", value = NA, min = 0),
             numericInput("manganese", "Manganese (Mn)", value = NA, min = 0),
             numericInput("zinc", "Zinc (Zn)", value = NA, min = 0),
             numericInput("copper", "Copper (Cu)", value = NA, min = 0),
             numericInput("boron", "Boron (B)", value = NA, min = 0, step = 0.1)
           )
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
           ),

           h4(class = "mt-4", "External Resources"),
           tags$ul(
             tags$li(tags$a(href = "https://plants.usda.gov/", target = "_blank", "USDA PLANTS Database"),
                     " - Comprehensive plant information including characteristics and distributions"),
             tags$li(tags$a(href = "https://soilhealthlab.cals.cornell.edu/", target = "_blank", "Cornell Soil Health Lab"),
                     " - Comprehensive soil health assessment and testing"),
             tags$li(tags$a(href = "https://ohioline.osu.edu/factsheet/agf-0514", target = "_blank", "Interpreting Soil Test Reports (Ohio State)"),
                     " - Guide to understanding lab results"),
             tags$li(tags$a(href = "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/", target = "_blank", "USDA Web Soil Survey"),
                     " - Detailed soil information by location"),
             tags$li(tags$a(href = "https://powo.science.kew.org/", target = "_blank", "Plants of the World Online (Kew)"),
                     " - Authoritative plant taxonomy and nomenclature"),
             tags$li(tags$a(href = "https://www.inaturalist.org/", target = "_blank", "iNaturalist"),
                     " - Citizen science platform for species observations")
           ),

           div(class = "mt-4 p-3 bg-light rounded",
               tags$small(class = "text-muted",
                          icon("book"), " ",
                          tags$strong("References: "),
                          "Nutrient guidelines adapted from university extension publications. ",
                          "Species data from WCVP (Royal Botanic Gardens, Kew) and USDA PLANTS Database."))
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
             "Use results from a professional soil testing lab (",
             tags$a(href = "https://www.nifa.usda.gov/grants/programs/cooperative-extension-system", target = "_blank", "university extension services"),
             ", commercial labs) or a reliable home soil test kit. For best results, collect samples from the root zone of the plants you're recording. ",
             "See the ", tags$a(href = "https://soilhealthlab.cals.cornell.edu/", target = "_blank", "Cornell Soil Health Lab"),
             " for detailed sampling guidance."),

           h5("Can I submit data for plants that died or failed?"),
           p(class = "text-muted mb-4",
             "Yes! Data from unsuccessful plantings is valuable—it helps identify soil conditions that certain species ",
             "struggle with. Use the Outcome dropdown (Thriving, Established, Struggling, or Failed/Died) for each species."),

           h5("What if I don't have all the soil test values?"),
           p(class = "text-muted mb-4",
             "Submit what you have! Leave fields blank if you don't have that data—they'll be excluded from analysis ",
             "rather than treated as zeros. pH and organic matter are the most valuable parameters to include."),

           h5("How are ecoregions determined?"),
           p(class = "text-muted mb-4",
             "When you enter coordinates, the app automatically identifies the ",
             tags$a(href = "https://www.epa.gov/eco-research/ecoregions", target = "_blank", "EPA Level IV ecoregion"),
             ". This provides ecological context—plants in the same ecoregion often face similar soil, climate, and vegetation conditions."),

           h5("What is USDA reference data?"),
           p(class = "text-muted mb-4",
             "The ", tags$a(href = "https://plants.usda.gov/", target = "_blank", "USDA PLANTS Database"),
             " provides characteristics for many species including preferred pH range, drought tolerance, and shade tolerance. ",
             "When available, this reference data is shown alongside your samples for comparison."),

           h5("Can I edit or delete my submissions?"),
           p(class = "text-muted mb-4",
             "Currently, direct editing isn't available in the app. Contact the administrator if you need to correct data."),

           h5("How is my data used?"),
           p(class = "text-muted mb-4",
             "All submitted data is shared under a CC BY-NC 4.0 license. It's freely available for non-commercial use ",
             "by researchers, gardeners, and educators. Your email is stored for attribution but not publicly displayed."),

           h5("What species can I enter?"),
           p(class = "text-muted",
             "The species search uses the ",
             tags$a(href = "https://powo.science.kew.org/", target = "_blank", "World Checklist of Vascular Plants (WCVP)"),
             ", which includes over 360,000 accepted plant species. If you can't find a species, try the scientific name without the author citation.")
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
       cec_meq = input$cec,
       soluble_salts_ppm = input$soluble_salts,
       nitrate_ppm = input$nitrate,
       ammonium_ppm = input$ammonium,
       phosphorus_ppm = input$phosphorus,
       potassium_ppm = input$potassium,
       calcium_ppm = input$calcium,
       magnesium_ppm = input$magnesium,
       sulfur_ppm = input$sulfur,
       iron_ppm = input$iron,
       manganese_ppm = input$manganese,
       zinc_ppm = input$zinc,
       copper_ppm = input$copper,
       boron_ppm = input$boron,
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
     return(empty_state("circle-nodes", "No Species Selected", "Choose a species from the sidebar"))
   }
   dat <- db_get_species_data(input$analysis_species)
   # Allow reference-only mode if USDA data exists
   if (nrow(dat) == 0) {
     tr <- get_usda_traits_for_name(input$analysis_species, pool)
     if (is.null(tr) || nrow(tr) == 0 || is.na(tr$soil_ph_min) || is.na(tr$soil_ph_max)) {
       return(empty_state("database", "No Data", "No samples or USDA pH reference available"))
     }
   }
   tagList(
     plotlyOutput("ph_om_plot", height = "500px"),
     tags$p(class = "text-muted small mt-2 px-3",
            icon("info-circle"), " ",
            "This scatter plot shows the relationship between soil pH and organic matter content. ",
            "Points represent individual samples. Higher organic matter generally improves soil structure and water retention. ",
            "The green shaded region shows the USDA reference pH range if available.")
   )
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
   dat <- db_get_species_data(input$analysis_species)
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
   dat <- db_get_species_data(input$analysis_species)
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
   dat <- db_get_species_data(input$analysis_species)
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
   dat <- db_get_species_data(input$analysis_species)
   dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
   if (nrow(dat) == 0) {
     return(empty_state("database", "No Texture Data", "No samples with texture data"))
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
