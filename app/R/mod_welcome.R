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
          class = "bg-transparent border-0 pt-4",
          # Alpha feedback banner
          div(class = "alert alert-warning mb-4 mx-3",
              style = "border-left: 4px solid #7A9A86;",
              div(class = "d-flex align-items-start",
                  icon("flask", class = "me-3 mt-1", style = "font-size: 1.5rem; color: #7A9A86;"),
                  div(
                    tags$strong("Welcome to the Alpha!", class = "d-block mb-1"),
                    "You're among the first to test edaphic flora. Your feedback is invaluable! ",
                    "Please report bugs, suggest improvements, or share your experience:",
                    div(class = "mt-2",
                        tags$a(href = "https://github.com/todd-testerman/edaphic-flora/issues",
                               target = "_blank", class = "btn btn-sm btn-outline-secondary me-2",
                               icon("github"), " Open an Issue"),
                        tags$a(href = "mailto:edaphicflora@gmail.com?subject=Alpha%20Feedback",
                               class = "btn btn-sm btn-outline-secondary",
                               icon("envelope"), " Email Feedback"))
                  )
              )
          ),
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
              h4("What is edaphic flora?"),
              p("edaphic flora helps gardeners, horticulturists, and researchers understand the relationship ",
                "between soil conditions and plant success. By collecting real-world soil data from locations ",
                "where specific plants grow, we build a reference database that can guide planting decisions."),

              h4(class = "mt-4", "How It Works"),
              tags$ol(
                tags$li(tags$strong("Submit soil data"), " \u2014 Enter soil test results along with the species growing in that soil"),
                tags$li(tags$strong("Include location"), " \u2014 Geocode your sample location for ",
                        tags$a(href = "https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states",
                               target = "_blank", "EPA Level IV ecoregion"),
                        " analysis"),
                tags$li(tags$strong("Analyze patterns"), " \u2014 View pH distributions, nutrient levels, texture profiles, and success rates"),
                tags$li(tags$strong("Discover plants"), " \u2014 Find species that thrive in similar soil conditions or match your soil profile")
              ),

              h4(class = "mt-4", "Discover Plants for Your Soil"),
              p("One of edaphic flora's most powerful features is plant discovery:"),
              tags$ul(
                tags$li(tags$strong("Find Plants"), " \u2014 Enter your soil test values and get recommendations for species ",
                        "that have thrived in similar conditions. Great for planning new plantings."),
                tags$li(tags$strong("Similar Species"), " \u2014 When viewing a species in Analysis, see other plants that grow ",
                        "in comparable soil conditions. Useful for companion planting and diversification.")
              ),
              p(class = "small text-muted",
                "Recommendations are based on real user data, not just reference ranges. We show success rates and ",
                "optimal growing conditions so you can make informed decisions."),
             p(class = "small text-muted mt-2 fst-italic",
               icon("leaf", class = "text-success"), " ",
               tags$strong("Tip:"), " When exploring recommendations, prioritize native plants! ",
               "They support local ecosystems, require less maintenance, and help avoid introducing ",
               "invasive species. Always verify a species is appropriate for your region before planting."),

              h4(class = "mt-4", "Getting Started"),
              p("Head to the ", tags$strong("Data Entry"), " tab to submit your first soil sample. ",
                "You can enter data for multiple species that share the same soil conditions."),
              p("Use the ", tags$strong("Analysis"), " tab to explore existing data, or try ",
                tags$strong("Find Plants"), " to discover species for your soil."),

              # Help links - NOT namespaced so main app observers can handle navigation
              div(class = "mt-3 p-3 border rounded",
                  tags$small(class = "text-muted d-block mb-2", icon("question-circle"), " Need help understanding the fields?"),
                  tags$ul(class = "mb-0 small",
                    tags$li(actionLink("help_link_soil", "Soil Properties", class = "text-decoration-none"),
                            " \u2014 pH, organic matter, texture explained"),
                    tags$li(actionLink("help_link_nutrients", "Nutrient Guide", class = "text-decoration-none"),
                            " \u2014 Macro and micronutrient reference ranges"),
                    tags$li(actionLink("help_link_performance", "Plant Performance", class = "text-decoration-none"),
                            " \u2014 Outcome, sun exposure, and hydrology definitions")
                  )
              ),

              div(class = "mt-4 p-3 bg-light rounded",
                  tags$small(class = "text-muted",
                             icon("info-circle"), " ",
                             tags$strong("Data Usage: "),
                             "All submitted data is shared under CC BY-NC 4.0. ",
                             "By contributing, you help build a free resource for the gardening and research community.")),

              div(class = "mt-3 text-center",
                  tags$small(class = "text-muted",
                             "Found a bug or have feedback? ",
                             tags$a(href = "https://github.com/todd-testerman/EdaphicFlora/issues",
                                    target = "_blank", "Report an issue"),
                             " or email ",
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
        div(class = "text-center p-2",
            div(class = "fs-4 fw-bold", style = "color: #7A9A86;", value),
            div(class = "text-muted", style = "font-size: 0.7rem;", icon(icon_name), " ", label))
      }

      # 2x2 grid layout for stats
      div(class = "row g-0 border-bottom pb-2 mb-2",
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
