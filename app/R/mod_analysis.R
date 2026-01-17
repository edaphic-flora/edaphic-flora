# R/mod_analysis.R - Analysis tab Shiny module

# ---------------------------
# Analysis Module UI
# ---------------------------

analysisUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Analysis",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        class = "p-3",
        selectizeInput(
          ns("species_select"),
          label = "Select Species",
          choices = NULL,
          options = list(
            placeholder = "Start typing a species name...",
            maxOptions = 100
          )
        ),
        uiOutput(ns("species_summary")),
        uiOutput(ns("reference_badges"))
      ),
      navset_card_tab(
        id = ns("analysis_tabs"),
        nav_panel(
          title = "Summary",
          icon = icon("list"),
          uiOutput(ns("summary_ui"))
        ),
        nav_panel(
          title = "pH Distribution",
          icon = icon("chart-bar"),
          uiOutput(ns("ph_plot_ui"))
        ),
        nav_panel(
          title = "pH vs Organic Matter",
          icon = icon("chart-line"),
          uiOutput(ns("ph_om_plot_ui"))
        ),
        nav_panel(
          title = "Nutrients",
          icon = icon("seedling"),
          uiOutput(ns("nutrient_plot_ui"))
        ),
        nav_panel(
          title = "Correlations",
          icon = icon("table"),
          uiOutput(ns("heatmap_ui"))
        ),
        nav_panel(
          title = "Soil Texture",
          icon = icon("mountain"),
          uiOutput(ns("texture_plot_ui"))
        ),
        nav_panel(
          title = "Map",
          icon = icon("map"),
          uiOutput(ns("map_ui"))
        ),
        nav_panel(
          title = "USDA Traits",
          icon = icon("book"),
          uiOutput(ns("traits_ui"))
        ),
        nav_panel(
          title = "Raw Data",
          icon = icon("table"),
          DTOutput(ns("raw_data"))
        )
      )
    )
  )
}

# ---------------------------
# Analysis Module Server
# ---------------------------

analysisServer <- function(id, species_db, get_all_samples, get_species_data, edaphic_colors, theme_edaphic) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update species choices from database
    observe({
      all_data <- get_all_samples()
      if (nrow(all_data) > 0) {
        species_in_db <- sort(unique(all_data$species))
        updateSelectizeInput(session, "species_select",
                             choices = species_in_db,
                             server = TRUE)
      }
    })

    # Get filtered data for selected species
    species_data <- reactive({
      req(input$species_select)
      get_species_data(input$species_select)
    })

    # Species summary in sidebar
    output$species_summary <- renderUI({
      req(input$species_select)
      data <- species_data()
      if (nrow(data) == 0) return(NULL)

      div(
        class = "mt-3 p-2 bg-light rounded small",
        tags$strong(input$species_select),
        tags$br(),
        sprintf("%d sample%s", nrow(data), if (nrow(data) == 1) "" else "s"),
        if (!all(is.na(data$ph))) {
          tagList(
            tags$br(),
            sprintf("pH: %.1f - %.1f", min(data$ph, na.rm = TRUE), max(data$ph, na.rm = TRUE))
          )
        }
      )
    })

    # Reference badges for USDA data
    output$reference_badges <- renderUI({
      req(input$species_select)

      usda_data <- tryCatch(
        get_usda_characteristics(input$species_select),
        error = function(e) NULL
      )

      wetland_data <- tryCatch(
        get_wetland_indicator(input$species_select),
        error = function(e) NULL
      )

      badges <- tagList()

      if (!is.null(usda_data) && nrow(usda_data) > 0) {
        badges <- tagList(
          badges,
          span(class = "badge bg-success me-1", icon("leaf"), " USDA Traits")
        )
      }

      if (!is.null(wetland_data) && nrow(wetland_data) > 0) {
        indicator <- wetland_data$indicator_code[1]
        badge_class <- switch(
          indicator,
          "OBL" = "bg-primary",
          "FACW" = "bg-info",
          "FAC" = "bg-secondary",
          "FACU" = "bg-warning",
          "UPL" = "bg-light text-dark",
          "bg-secondary"
        )
        badges <- tagList(
          badges,
          span(class = paste("badge", badge_class), icon("water"), " ", indicator)
        )
      }

      if (length(badges) > 0) {
        div(class = "mt-2", badges)
      }
    })

    # Summary tab
    output$summary_ui <- renderUI({
      req(input$species_select)
      data <- species_data()
      if (nrow(data) == 0) {
        return(div(class = "text-muted p-3", "No data available for this species."))
      }

      tableOutput(ns("summary_stats"))
    })

    output$summary_stats <- renderTable({
      req(input$species_select)
      data <- species_data()
      if (nrow(data) == 0) return(NULL)

      numeric_cols <- c("ph", "organic_matter", "nitrate_ppm", "ammonium_ppm",
                        "phosphorus_ppm", "potassium_ppm", "calcium_ppm",
                        "magnesium_ppm", "soluble_salts_ppm")

      stats <- data.frame(
        Metric = character(),
        Min = numeric(),
        Mean = numeric(),
        Median = numeric(),
        Max = numeric(),
        N = integer(),
        stringsAsFactors = FALSE
      )

      nice_names <- c(
        ph = "pH",
        organic_matter = "Organic Matter (%)",
        nitrate_ppm = "Nitrate (ppm)",
        ammonium_ppm = "Ammonium (ppm)",
        phosphorus_ppm = "Phosphorus (ppm)",
        potassium_ppm = "Potassium (ppm)",
        calcium_ppm = "Calcium (ppm)",
        magnesium_ppm = "Magnesium (ppm)",
        soluble_salts_ppm = "Soluble Salts (ppm)"
      )

      for (col in numeric_cols) {
        if (col %in% names(data)) {
          vals <- data[[col]]
          vals <- vals[!is.na(vals)]
          if (length(vals) > 0) {
            stats <- rbind(stats, data.frame(
              Metric = nice_names[col],
              Min = round(min(vals), 2),
              Mean = round(mean(vals), 2),
              Median = round(median(vals), 2),
              Max = round(max(vals), 2),
              N = length(vals),
              stringsAsFactors = FALSE
            ))
          }
        }
      }

      stats
    }, striped = TRUE, hover = TRUE)

    # pH Distribution plot
    output$ph_plot_ui <- renderUI({
      req(input$species_select)
      data <- species_data()
      if (nrow(data) == 0 || all(is.na(data$ph))) {
        return(div(class = "text-muted p-3", "No pH data available."))
      }
      plotlyOutput(ns("ph_plot"), height = "400px")
    })

    output$ph_plot <- renderPlotly({
      req(input$species_select)
      data <- species_data()
      req(nrow(data) > 0, !all(is.na(data$ph)))

      p <- ggplot(data, aes(x = ph)) +
        geom_histogram(binwidth = 0.25, fill = edaphic_colors$sage, color = "white", alpha = 0.8) +
        labs(
          title = paste("pH Distribution -", input$species_select),
          x = "pH",
          y = "Count"
        ) +
        theme_edaphic() +
        scale_x_continuous(limits = c(3, 10), breaks = seq(3, 10, 1))

      # Add USDA reference range if available
      usda_data <- tryCatch(get_usda_characteristics(input$species_select), error = function(e) NULL)
      if (!is.null(usda_data) && nrow(usda_data) > 0) {
        ph_min <- usda_data$ph_minimum[1]
        ph_max <- usda_data$ph_maximum[1]
        if (!is.na(ph_min) && !is.na(ph_max)) {
          p <- p + annotate("rect", xmin = ph_min, xmax = ph_max,
                           ymin = -Inf, ymax = Inf,
                           fill = edaphic_colors$brown, alpha = 0.2)
        }
      }

      ggplotly(p, tooltip = c("x", "y"))
    })

    # pH vs Organic Matter scatter
    output$ph_om_plot_ui <- renderUI({
      req(input$species_select)
      data <- species_data()
      if (nrow(data) == 0 || all(is.na(data$ph)) || all(is.na(data$organic_matter))) {
        return(div(class = "text-muted p-3", "Insufficient data for this plot."))
      }
      plotlyOutput(ns("ph_om_plot"), height = "400px")
    })

    output$ph_om_plot <- renderPlotly({
      req(input$species_select)
      data <- species_data()
      data <- data[!is.na(data$ph) & !is.na(data$organic_matter), ]
      req(nrow(data) > 0)

      p <- ggplot(data, aes(x = ph, y = organic_matter)) +
        geom_point(color = edaphic_colors$sage, size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = edaphic_colors$brown, fill = edaphic_colors$brown, alpha = 0.2) +
        labs(
          title = paste("pH vs Organic Matter -", input$species_select),
          x = "pH",
          y = "Organic Matter (%)"
        ) +
        theme_edaphic()

      ggplotly(p, tooltip = c("x", "y"))
    })

    # Nutrient boxplot
    output$nutrient_plot_ui <- renderUI({
      req(input$species_select)
      data <- species_data()
      nutrient_cols <- c("nitrate_ppm", "phosphorus_ppm", "potassium_ppm",
                         "calcium_ppm", "magnesium_ppm")
      has_data <- any(sapply(nutrient_cols, function(col) {
        col %in% names(data) && !all(is.na(data[[col]]))
      }))
      if (!has_data) {
        return(div(class = "text-muted p-3", "No nutrient data available."))
      }
      plotlyOutput(ns("nutrient_plot"), height = "400px")
    })

    output$nutrient_plot <- renderPlotly({
      req(input$species_select)
      data <- species_data()

      nutrient_cols <- c("nitrate_ppm", "phosphorus_ppm", "potassium_ppm",
                         "calcium_ppm", "magnesium_ppm")
      nice_names <- c("Nitrate", "Phosphorus", "Potassium", "Calcium", "Magnesium")

      plot_data <- data.frame()
      for (i in seq_along(nutrient_cols)) {
        col <- nutrient_cols[i]
        if (col %in% names(data)) {
          vals <- data[[col]]
          vals <- vals[!is.na(vals)]
          if (length(vals) > 0) {
            plot_data <- rbind(plot_data, data.frame(
              Nutrient = nice_names[i],
              Value = vals
            ))
          }
        }
      }

      req(nrow(plot_data) > 0)

      p <- ggplot(plot_data, aes(x = Nutrient, y = Value, fill = Nutrient)) +
        geom_boxplot(alpha = 0.7) +
        labs(
          title = paste("Nutrient Levels -", input$species_select),
          x = "",
          y = "Concentration (ppm)"
        ) +
        theme_edaphic() +
        theme(legend.position = "none") +
        scale_fill_manual(values = c(
          edaphic_colors$sage, edaphic_colors$brown, edaphic_colors$charcoal,
          edaphic_colors$limestone, edaphic_colors$sage_light
        ))

      ggplotly(p, tooltip = c("y"))
    })

    # Correlation heatmap
    output$heatmap_ui <- renderUI({
      req(input$species_select)
      data <- species_data()
      if (nrow(data) < 3) {
        return(div(class = "text-muted p-3", "At least 3 samples needed for correlation analysis."))
      }
      plotlyOutput(ns("heatmap_plot"), height = "500px")
    })

    output$heatmap_plot <- renderPlotly({
      req(input$species_select)
      data <- species_data()
      req(nrow(data) >= 3)

      numeric_cols <- c("ph", "organic_matter", "nitrate_ppm", "phosphorus_ppm",
                        "potassium_ppm", "calcium_ppm", "magnesium_ppm")
      numeric_cols <- numeric_cols[numeric_cols %in% names(data)]

      cor_data <- data[, numeric_cols, drop = FALSE]
      cor_data <- cor_data[complete.cases(cor_data), ]

      req(nrow(cor_data) >= 3)

      cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

      # Convert to long format for ggplot
      cor_df <- as.data.frame(as.table(cor_matrix))
      names(cor_df) <- c("Var1", "Var2", "Correlation")

      nice_names <- c(
        ph = "pH", organic_matter = "Org. Matter", nitrate_ppm = "Nitrate",
        phosphorus_ppm = "Phosphorus", potassium_ppm = "Potassium",
        calcium_ppm = "Calcium", magnesium_ppm = "Magnesium"
      )

      cor_df$Var1 <- nice_names[as.character(cor_df$Var1)]
      cor_df$Var2 <- nice_names[as.character(cor_df$Var2)]

      p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(Correlation, 2)), size = 3) +
        scale_fill_gradient2(low = edaphic_colors$brown, mid = "white",
                             high = edaphic_colors$sage, midpoint = 0,
                             limits = c(-1, 1)) +
        labs(title = paste("Correlation Matrix -", input$species_select)) +
        theme_edaphic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_blank())

      ggplotly(p, tooltip = c("x", "y", "fill"))
    })

    # Soil texture ternary diagram
    output$texture_plot_ui <- renderUI({
      req(input$species_select)
      data <- species_data()
      has_texture <- all(c("texture_sand", "texture_silt", "texture_clay") %in% names(data))
      if (!has_texture) {
        return(div(class = "text-muted p-3", "No texture data available."))
      }
      texture_data <- data[!is.na(data$texture_sand) & !is.na(data$texture_silt) & !is.na(data$texture_clay), ]
      if (nrow(texture_data) == 0) {
        return(div(class = "text-muted p-3", "No texture data available."))
      }
      plotOutput(ns("texture_plot"), height = "500px")
    })

    output$texture_plot <- renderPlot({
      req(input$species_select)
      data <- species_data()

      texture_data <- data[!is.na(data$texture_sand) & !is.na(data$texture_silt) & !is.na(data$texture_clay), ]
      req(nrow(texture_data) > 0)

      ggtern(texture_data, aes(x = texture_sand, y = texture_clay, z = texture_silt)) +
        geom_point(color = edaphic_colors$sage, size = 4, alpha = 0.7) +
        labs(
          title = paste("Soil Texture -", input$species_select),
          x = "Sand (%)",
          y = "Clay (%)",
          z = "Silt (%)"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          tern.axis.title = element_text(size = 10)
        )
    })

    # Map
    output$map_ui <- renderUI({
      req(input$species_select)
      data <- species_data()
      has_coords <- all(c("location_lat", "location_long") %in% names(data))
      if (!has_coords) {
        return(div(class = "text-muted p-3", "No location data available."))
      }
      map_data <- data[!is.na(data$location_lat) & !is.na(data$location_long), ]
      if (nrow(map_data) == 0) {
        return(div(class = "text-muted p-3", "No location data available."))
      }
      leafletOutput(ns("map_plot"), height = "500px")
    })

    output$map_plot <- renderLeaflet({
      req(input$species_select)
      data <- species_data()

      map_data <- data[!is.na(data$location_lat) & !is.na(data$location_long), ]
      req(nrow(map_data) > 0)

      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~location_long,
          lat = ~location_lat,
          radius = 8,
          color = edaphic_colors$sage,
          fillOpacity = 0.7,
          popup = ~paste0(
            "<strong>", species, "</strong><br>",
            if (!is.na(ph)) paste0("pH: ", ph, "<br>") else "",
            if (!is.na(ecoregion_l4)) paste0("Ecoregion: ", ecoregion_l4) else ""
          )
        )
    })

    # USDA Traits tab
    output$traits_ui <- renderUI({
      req(input$species_select)

      usda_data <- tryCatch(
        get_usda_characteristics(input$species_select),
        error = function(e) NULL
      )

      if (is.null(usda_data) || nrow(usda_data) == 0) {
        return(div(
          class = "text-muted p-3",
          icon("info-circle"), " No USDA trait data available for this species."
        ))
      }

      tableOutput(ns("traits_table"))
    })

    output$traits_table <- renderTable({
      req(input$species_select)

      usda_data <- tryCatch(
        get_usda_characteristics(input$species_select),
        error = function(e) NULL
      )

      if (is.null(usda_data) || nrow(usda_data) == 0) return(NULL)

      # Format for display
      traits <- data.frame(
        Trait = character(),
        Value = character(),
        stringsAsFactors = FALSE
      )

      trait_map <- list(
        "pH Range" = c("ph_minimum", "ph_maximum"),
        "Drought Tolerance" = "drought_tolerance",
        "Shade Tolerance" = "shade_tolerance",
        "Moisture Use" = "moisture_use",
        "Precipitation (in)" = c("precip_minimum", "precip_maximum"),
        "Temperature (°F)" = c("temp_min_f", "temp_max_f"),
        "Root Depth (in)" = "root_depth_minimum",
        "Height (ft)" = c("height_at_base_age_minimum", "height_at_base_age_maximum"),
        "Growth Rate" = "growth_rate",
        "Lifespan" = "lifespan",
        "Nitrogen Fixation" = "nitrogen_fixation",
        "Resprout Ability" = "resprout_ability",
        "Fire Tolerance" = "fire_tolerance",
        "Salinity Tolerance" = "salinity_tolerance"
      )

      for (trait_name in names(trait_map)) {
        cols <- trait_map[[trait_name]]
        if (length(cols) == 2) {
          # Range
          min_val <- usda_data[[cols[1]]][1]
          max_val <- usda_data[[cols[2]]][1]
          if (!is.na(min_val) && !is.na(max_val)) {
            traits <- rbind(traits, data.frame(Trait = trait_name, Value = paste(min_val, "-", max_val)))
          } else if (!is.na(min_val)) {
            traits <- rbind(traits, data.frame(Trait = trait_name, Value = as.character(min_val)))
          } else if (!is.na(max_val)) {
            traits <- rbind(traits, data.frame(Trait = trait_name, Value = as.character(max_val)))
          }
        } else {
          # Single value
          val <- usda_data[[cols]][1]
          if (!is.na(val) && nzchar(as.character(val))) {
            traits <- rbind(traits, data.frame(Trait = trait_name, Value = as.character(val)))
          }
        }
      }

      traits
    }, striped = TRUE, hover = TRUE)

    # Raw data table
    output$raw_data <- renderDT({
      req(input$species_select)
      data <- species_data()

      display_cols <- c("species", "cultivar", "date", "ph", "organic_matter",
                        "nitrate_ppm", "phosphorus_ppm", "potassium_ppm",
                        "texture_class", "ecoregion_l4")
      display_cols <- display_cols[display_cols %in% names(data)]

      datatable(
        data[, display_cols, drop = FALSE],
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # Return selected species for other modules to use
    reactive(input$species_select)
  })
}
