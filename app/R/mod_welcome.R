# R/mod_welcome.R - Welcome page module
# Landing page with stats and sample locations map

# ---------------------------
# UI
# ---------------------------

welcomeUI <- function(id) {
  ns <- NS(id)

  nav_panel_hidden(
    value = "Welcome",
    layout_columns(
      col_widths = c(8, 4),

      # Main welcome content
      card(
        card_header(
          class = "bg-transparent border-0 pt-2 welcome-hero",
          # Beta banner - slim single line
          div(class = "alert alert-warning mb-2 mx-3 py-1 px-3", style = "font-size: 0.85rem;",
              icon("flask", class = "me-1", style = "color: #7A9A86;"),
              tags$strong("Beta"), " \u2014 desktop optimized. ",
              tags$a(href = "mailto:edaphicflora@gmail.com?subject=Edaphic%20Flora%20Beta%20Feedback",
                     style = "color: #7A9A86; font-weight: 500;",
                     "Send feedback", icon("envelope", class = "ms-1 fa-xs"))
          ),
          div(class = "text-center",
              tags$img(src = "readme_header.svg",
                       alt = "Edaphic Flora",
                       class = "welcome-brand-logo",
                       style = "max-width: 100%; width: 560px; height: auto;")
          )
        ),
        card_body(
          div(class = "px-md-4",
              h4("What is Edaphic Flora?"),
              p("Edaphic Flora helps gardeners, horticulturists, and researchers understand the relationship ",
                "between soil conditions and plant success. By collecting real-world soil data from locations ",
                "where specific plants grow, we build a reference database that can guide planting decisions."),

              h4(class = "mt-3", "How It Works"),
              # Visual step cards
              div(class = "row g-2 mb-3 stagger-reveal",
                div(class = "col-md-4",
                    div(class = "text-center p-2 h-100 step-card",
                        icon("flask", class = "fa-lg mb-1", style = "color: #7A9A86;"),
                        div(class = "step-card-title", "1. Submit Soil Data"),
                        tags$small(class = "text-muted", style = "font-size: 0.8rem;", "Upload a lab report or enter results manually")
                    )
                ),
                div(class = "col-md-4",
                    div(class = "text-center p-2 h-100 step-card",
                        icon("chart-line", class = "fa-lg mb-1", style = "color: #7A9A86;"),
                        div(class = "step-card-title", "2. Explore Analysis"),
                        tags$small(class = "text-muted", style = "font-size: 0.8rem;", "See pH, nutrients, and performance patterns")
                    )
                ),
                div(class = "col-md-4",
                    div(class = "text-center p-2 h-100 step-card",
                        icon("seedling", class = "fa-lg mb-1", style = "color: #7A9A86;"),
                        div(class = "step-card-title", "3. Grow Your Impact"),
                        tags$small(class = "text-muted", style = "font-size: 0.8rem;", "Discover what wildlife your garden supports")
                    )
                )
              ),

              h4(class = "mt-2", "Getting Started"),
              div(class = "d-flex flex-column flex-sm-row gap-2 mb-2",
                  actionButton("welcome_submit_data",
                               label = tagList(icon("flask"), " I have a soil report"),
                               class = "btn-lg",
                               style = "background-color: #7A9A86; color: white; border: none; font-family: 'Montserrat', sans-serif; font-weight: 600;"),
                  actionButton("welcome_browse_data",
                               label = tagList(icon("chart-line"), " I'm exploring species data"),
                               class = "btn-lg",
                               style = "background-color: #D39B35; color: white; border: none; font-family: 'Montserrat', sans-serif; font-weight: 600;")
              ),

              # Location callout + help links side by side
              div(class = "row g-2 mb-2",
                div(class = "col-md-7",
                  div(class = "p-2 rounded h-100",
                    style = "background-color: rgba(122, 154, 134, 0.1); border-left: 3px solid #7A9A86; font-size: 0.9rem;",
                    icon("user-gear", style = "color: #7A9A86;"), " ",
                    tags$strong("Set your home location:"),
                    " Click your name in the top right to set your zip code. ",
                    "This enables native status and nearby sample features."
                  )
                ),
                div(class = "col-md-5",
                  div(class = "p-2 border rounded h-100", style = "font-size: 0.9rem;",
                    tags$span(class = "text-muted d-block mb-1", icon("question-circle"), " Need help?"),
                    actionLink("help_link_soil", "Soil Properties", class = "text-decoration-none"),
                    " \u00b7 ",
                    actionLink("help_link_nutrients", "Nutrients", class = "text-decoration-none"),
                    " \u00b7 ",
                    actionLink("help_link_performance", "Performance", class = "text-decoration-none")
                  )
                )
              ),

              div(class = "p-2 bg-light rounded text-center", style = "font-size: 0.85rem;",
                  tags$span(class = "text-muted",
                             icon("info-circle"), " ",
                             tags$strong("Data Usage: "),
                             "All submitted data is shared under CC BY-NC 4.0."))
          )
        )
      ),

      # Quick stats sidebar
      card(
        card_header(icon("chart-simple"), "Database Stats"),
        card_body(
          # Beta test data notice
          div(class = "alert alert-info py-2 px-2 mb-3", style = "font-size: 0.75rem;",
              icon("info-circle"), " ",
              tags$strong("Beta Note:"), " Community data is growing. ",
              "Contribute soil samples to improve species coverage."),
          # Stats skeleton (shown instantly in initial HTML, replaced by server)
          div(id = ns("stats_skeleton"),
              div(class = "row g-0 border-bottom pb-2 mb-2",
                  lapply(list(
                    list(icon = "flask", label = "Samples"),
                    list(icon = "seedling", label = "Species"),
                    list(icon = "users", label = "Contributors"),
                    list(icon = "map", label = "Ecoregions")
                  ), function(s) {
                    div(class = paste0("col-6", if (s$icon %in% c("flask", "users")) " border-end" else "",
                                       if (s$icon %in% c("flask", "seedling")) " border-bottom" else ""),
                        div(class = "stat-card",
                            div(class = "stat-number placeholder-glow",
                                span(class = "placeholder col-4")),
                            div(class = "stat-label", icon(s$icon), " ", s$label)))
                  })
              )
          ),
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
    ns <- session$ns

    # Cached stats — starts NULL, populated after initial page render
    cached_stats <- reactiveVal(NULL)

    # Defer DB query until after first page render so skeleton shows immediately
    session$onFlushed(function() {
      later::later(function() {
        fetch_stats()
      }, delay = 0.1)
    }, once = TRUE)

    # Also re-fetch when data changes (e.g., after new submission)
    observeEvent(data_changed(), {
      fetch_stats()
    }, ignoreInit = TRUE)

    fetch_stats <- function() {
      tryCatch({
        res <- dbGetQuery(pool, "
          SELECT COUNT(*)::int AS samples,
                 COUNT(DISTINCT species)::int AS species,
                 COUNT(DISTINCT created_by)::int AS users,
                 COUNT(DISTINCT CASE WHEN ecoregion_l4 IS NOT NULL THEN ecoregion_l4 END)::int AS ecoregions
          FROM soil_samples
        ")
        cached_stats(list(
          samples = res$samples[1] %||% 0L,
          species = res$species[1] %||% 0L,
          users = res$users[1] %||% 0L,
          ecoregions = res$ecoregions[1] %||% 0L
        ))
      }, error = function(e) {
        message("Welcome stats error: ", e$message)
        cached_stats(list(samples = 0, species = 0, users = 0, ecoregions = 0))
      })
    }

    # Replace skeleton with real stats once query returns
    output$stats <- renderUI({
      stats <- cached_stats()
      if (is.null(stats)) return(NULL)

      stat_box <- function(value, label, icon_name) {
        div(class = "stat-card",
            div(class = "stat-number", value),
            div(class = "stat-label", icon(icon_name), " ", label))
      }

      tagList(
        # Hide the static skeleton
        tags$script(HTML(sprintf(
          "document.getElementById('%s').style.display='none';",
          ns("stats_skeleton")
        ))),
        # Real stats
        div(class = "row g-0 border-bottom pb-2 mb-2 stagger-reveal",
          div(class = "col-6 border-end border-bottom", stat_box(stats$samples, "Samples", "flask")),
          div(class = "col-6 border-bottom", stat_box(stats$species, "Species", "seedling")),
          div(class = "col-6 border-end", stat_box(stats$users, "Contributors", "users")),
          div(class = "col-6", stat_box(stats$ecoregions, "Ecoregions", "map"))
        ),
        seed_database_ui(stats$samples)
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
