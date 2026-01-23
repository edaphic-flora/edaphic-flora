# R/mod_welcome.R - Welcome page module
# Landing page with stats and sample locations map

# ---------------------------
# UI
# ---------------------------

welcomeUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Welcome",
    icon = icon("home"),
    layout_columns(
      col_widths = c(8, 4),

      # Main welcome content
      card(
        card_header(
          class = "bg-transparent border-0 pt-2 welcome-hero",
          # Alpha feedback banner - compact
          div(class = "alert alert-warning mb-3 mx-3", style = "font-size: 0.9rem;",
              div(class = "d-flex align-items-start",
                  icon("flask", class = "me-2 mt-1", style = "font-size: 1.25rem; color: #7A9A86;"),
                  div(
                    tags$strong("Welcome to the Alpha!", class = "d-block"),
                    "You're among the first to test edaphic flora. Please report bugs or share feedback.",
                    tags$small(class = "d-block text-muted", icon("desktop"), " Desktop optimized."),
                    div(class = "mt-1",
                        tags$a(href = "mailto:edaphicflora@gmail.com?subject=Edaphic%20Flora%20Alpha%20Feedback",
                               class = "btn btn-sm",
                               style = "background-color: #D39B35; color: #373D3C; font-weight: 600; padding: 0.2rem 0.5rem;",
                               icon("envelope"), " Send Feedback"))
                  )
              )
          ),
          div(class = "text-center",
              h1(class = "welcome-brand mb-1",
                 span(class = "brand-name-edaphic", "edaphic"),
                 " ",
                 span(class = "brand-name-flora", "flora")),
              p(class = "text-muted mb-0", style = "font-size: 0.95rem;",
                "A community database for soil conditions and plant species")
          )
        ),
        card_body(
          div(class = "px-md-4",
              h4("What is edaphic flora?"),
              p("edaphic flora helps gardeners, horticulturists, and researchers understand the relationship ",
                "between soil conditions and plant success. By collecting real-world soil data from locations ",
                "where specific plants grow, we build a reference database that can guide planting decisions."),

              h4(class = "mt-3", "How It Works"),
              # Visual step cards - compact
              div(class = "row g-2 mb-3 stagger-reveal",
                # Step 1
                div(class = "col-md-6 col-lg-3",
                    div(class = "text-center p-2 h-100",
                        style = "background: linear-gradient(135deg, rgba(122,154,134,0.1) 0%, rgba(122,154,134,0.05) 100%); border-radius: 10px;",
                        icon("flask", class = "fa-lg mb-1", style = "color: #7A9A86;"),
                        div(style = "font-family: 'Montserrat', sans-serif; font-size: 0.9rem; font-weight: 600;", "1. Submit Data"),
                        tags$small(class = "text-muted", style = "font-size: 0.8rem;", "Enter soil test results")
                    )
                ),
                # Step 2
                div(class = "col-md-6 col-lg-3",
                    div(class = "text-center p-2 h-100",
                        style = "background: linear-gradient(135deg, rgba(122,154,134,0.1) 0%, rgba(122,154,134,0.05) 100%); border-radius: 10px;",
                        icon("map-marker-alt", class = "fa-lg mb-1", style = "color: #7A9A86;"),
                        div(style = "font-family: 'Montserrat', sans-serif; font-size: 0.9rem; font-weight: 600;", "2. Add Location"),
                        tags$small(class = "text-muted", style = "font-size: 0.8rem;", "Geocode for ecoregions")
                    )
                ),
                # Step 3
                div(class = "col-md-6 col-lg-3",
                    div(class = "text-center p-2 h-100",
                        style = "background: linear-gradient(135deg, rgba(122,154,134,0.1) 0%, rgba(122,154,134,0.05) 100%); border-radius: 10px;",
                        icon("chart-line", class = "fa-lg mb-1", style = "color: #7A9A86;"),
                        div(style = "font-family: 'Montserrat', sans-serif; font-size: 0.9rem; font-weight: 600;", "3. Analyze"),
                        tags$small(class = "text-muted", style = "font-size: 0.8rem;", "View patterns & profiles")
                    )
                ),
                # Step 4
                div(class = "col-md-6 col-lg-3",
                    div(class = "text-center p-2 h-100",
                        style = "background: linear-gradient(135deg, rgba(122,154,134,0.1) 0%, rgba(122,154,134,0.05) 100%); border-radius: 10px;",
                        icon("seedling", class = "fa-lg mb-1", style = "color: #7A9A86;"),
                        div(style = "font-family: 'Montserrat', sans-serif; font-size: 0.9rem; font-weight: 600;", "4. Discover"),
                        tags$small(class = "text-muted", style = "font-size: 0.8rem;", "Find plants for your soil")
                    )
                )
              ),

              h4(class = "mt-2", "Getting Started"),
              p(class = "mb-1", "Head to the ", tags$strong("Data Entry"), " tab to submit your first soil sample. ",
                "Use the ", tags$strong("Analysis"), " tab to explore existing data."),

              # Help links - NOT namespaced so main app observers can handle navigation
              div(class = "mt-2 p-2 border rounded",
                  tags$span(class = "text-muted d-block mb-1", style = "font-size: 0.9rem;", icon("question-circle"), " Need help understanding the fields?"),
                  tags$ul(class = "mb-0", style = "font-size: 0.85rem;",
                    tags$li(actionLink("help_link_soil", "Soil Properties", class = "text-decoration-none"),
                            " \u2014 pH, organic matter, texture"),
                    tags$li(actionLink("help_link_nutrients", "Nutrient Guide", class = "text-decoration-none"),
                            " \u2014 Macro and micronutrient ranges"),
                    tags$li(actionLink("help_link_performance", "Plant Performance", class = "text-decoration-none"),
                            " \u2014 Outcomes and conditions")
                  )
              ),

              div(class = "mt-2 p-2 bg-light rounded", style = "font-size: 0.9rem;",
                  tags$span(class = "text-muted",
                             icon("info-circle"), " ",
                             tags$strong("Data Usage: "),
                             "All submitted data is shared under CC BY-NC 4.0.")),

              div(class = "mt-2 text-center", style = "font-size: 0.85rem;",
                  tags$span(class = "text-muted",
                             "Feedback? Email ",
                             tags$a(href = "mailto:edaphicflora@gmail.com",
                                    "edaphicflora@gmail.com")))
          )
        )
      ),

      # Quick stats sidebar
      card(
        card_header(icon("chart-simple"), "Database Stats"),
        card_body(
          # Alpha test data notice
          div(class = "alert alert-info py-2 px-2 mb-3", style = "font-size: 0.75rem;",
              icon("info-circle"), " ",
              tags$strong("Alpha Note:"), " Current data is simulated for testing. ",
              "Real community data will replace this as users contribute."),
          uiOutput(ns("stats")),
          div(class = "text-muted text-center mb-1", style = "font-size: 0.7rem;",
              icon("map-location-dot"), " Sample Locations"),
          leafletOutput(ns("map"), height = "280px")
        )
      )
    )
  )
}

# ---------------------------
# Server
# ---------------------------

welcomeServer <- function(id, pool, data_changed) {
  moduleServer(id, function(input, output, session) {

    # Database stats
    output$stats <- renderUI({
      data_changed()
      stats <- tryCatch({
        samples_q <- dbGetQuery(pool, "SELECT COUNT(*)::int as n FROM soil_samples")
        species_q <- dbGetQuery(pool, "SELECT COUNT(DISTINCT species)::int as n FROM soil_samples")
        users_q <- dbGetQuery(pool, "SELECT COUNT(DISTINCT created_by)::int as n FROM soil_samples")
        eco_q <- dbGetQuery(pool, "SELECT COUNT(DISTINCT ecoregion_l4)::int as n FROM soil_samples WHERE ecoregion_l4 IS NOT NULL")
        list(
          samples = if (nrow(samples_q) > 0) samples_q$n[1] else 0,
          species = if (nrow(species_q) > 0) species_q$n[1] else 0,
          users = if (nrow(users_q) > 0) users_q$n[1] else 0,
          ecoregions = if (nrow(eco_q) > 0) eco_q$n[1] else 0
        )
      }, error = function(e) {
        message("Welcome stats error: ", e$message)
        list(samples = 0, species = 0, users = 0, ecoregions = 0)
      })

      stat_box <- function(value, label, icon_name) {
        div(class = "stat-card",
            div(class = "stat-number", value),
            div(class = "stat-label", icon(icon_name), " ", label))
      }

      # 2x2 grid layout for stats with staggered reveal
      div(class = "row g-0 border-bottom pb-2 mb-2 stagger-reveal",
        div(class = "col-6 border-end border-bottom", stat_box(stats$samples, "Samples", "flask")),
        div(class = "col-6 border-bottom", stat_box(stats$species, "Species", "seedling")),
        div(class = "col-6 border-end", stat_box(stats$users, "Contributors", "users")),
        div(class = "col-6", stat_box(stats$ecoregions, "Ecoregions", "map"))
      )
    })

    # Mini map with sample locations
    output$map <- renderLeaflet({
      # Get all sample locations
      locs <- tryCatch({
        dbGetQuery(pool, "
          SELECT location_lat, location_long, ecoregion_l4
          FROM soil_samples
          WHERE location_lat IS NOT NULL AND location_long IS NOT NULL
        ")
      }, error = function(e) data.frame())

      # Create base map centered on US
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.8, zoom = 3)

      # Add markers if we have data
      if (nrow(locs) > 0) {
        map <- map %>%
          addCircleMarkers(
            data = locs,
            lng = ~location_long,
            lat = ~location_lat,
            radius = 4,
            color = "#7A9A86",
            fillColor = "#7A9A86",
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 1,
            popup = ~ifelse(is.na(ecoregion_l4), "Sample location", ecoregion_l4)
          )
      }

      map
    })
  })
}
