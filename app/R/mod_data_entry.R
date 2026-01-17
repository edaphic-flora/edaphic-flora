# R/mod_data_entry.R - Data Entry tab Shiny module

# ---------------------------
# Data Entry Module UI
# ---------------------------

dataEntryUI <- function(id, species_db, texture_classes) {
  ns <- NS(id)

  nav_panel(
    title = "Data Entry",
    icon = icon("plus-circle"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        class = "p-3",
        id = ns("entry_sidebar"),
        accordion(
          id = ns("entry_accordion"),
          open = c("species_panel"),

          # Species Selection Panel
          accordion_panel(
            title = "Species Selection",
            value = "species_panel",
            icon = icon("leaf"),
            selectizeInput(
              ns("species"),
              label = "Species (up to 20)",
              choices = NULL,
              multiple = TRUE,
              options = list(
                maxItems = 20,
                placeholder = "Start typing species name...",
                create = FALSE,
                maxOptions = 100
              )
            ),
            uiOutput(ns("species_count_indicator"))
          ),

          # PDF Upload Panel (conditional)
          accordion_panel(
            title = "Upload Soil Report",
            value = "upload_panel",
            icon = icon("file-pdf"),
            fileInput(ns("pdf_upload"), NULL,
                      accept = c("application/pdf", ".pdf", ".rtf", ".txt",
                                 "image/png", "image/jpeg", "image/gif", "image/webp",
                                 ".png", ".jpg", ".jpeg", ".gif", ".webp")),
            uiOutput(ns("pdf_extract_status")),
            div(class = "small text-muted mt-1",
                "Supports PDF, RTF, TXT, and images (PNG, JPG).")
          ),

          # Soil Properties Panel
          accordion_panel(
            title = "Soil Properties",
            value = "soil_panel",
            icon = icon("flask"),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("ph"), "pH", value = NA, min = 0, max = 14, step = 0.1),
              numericInput(ns("organic_matter"), "Organic Matter (%)", value = NA, min = 0, max = 100, step = 0.1)
            ),
            selectInput(ns("organic_matter_class"), "Organic Matter Class",
                        choices = c("", "Very Low", "Low", "Medium Low", "Medium",
                                    "Medium High", "High", "Very High")),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("cec"), "CEC (meq/100g)", value = NA, min = 0, step = 0.1),
              numericInput(ns("soluble_salts"), "Soluble Salts (ppm)", value = NA, min = 0, step = 1)
            )
          ),

          # Macronutrients Panel
          accordion_panel(
            title = "Macronutrients",
            value = "macro_panel",
            icon = icon("seedling"),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("nitrate"), "Nitrate (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("ammonium"), "Ammonium (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("phosphorus"), "Phosphorus (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("potassium"), "Potassium (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("calcium"), "Calcium (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("magnesium"), "Magnesium (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("sulfur"), "Sulfur (ppm)", value = NA, min = 0, step = 1)
            )
          ),

          # Micronutrients Panel
          accordion_panel(
            title = "Micronutrients",
            value = "micro_panel",
            icon = icon("atom"),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("iron"), "Iron (ppm)", value = NA, min = 0, step = 0.1),
              numericInput(ns("manganese"), "Manganese (ppm)", value = NA, min = 0, step = 0.1),
              numericInput(ns("zinc"), "Zinc (ppm)", value = NA, min = 0, step = 0.1),
              numericInput(ns("boron"), "Boron (ppm)", value = NA, min = 0, step = 0.01),
              numericInput(ns("copper"), "Copper (ppm)", value = NA, min = 0, step = 0.1)
            )
          ),

          # Soil Texture Panel
          accordion_panel(
            title = "Soil Texture",
            value = "texture_panel",
            icon = icon("mountain"),
            radioButtons(ns("texture_input_type"), "Input Method:",
                         choices = c("Percentages" = "pct", "Classification" = "class"),
                         selected = "pct", inline = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'pct'", ns("texture_input_type")),
              layout_column_wrap(
                width = 1/3,
                numericInput(ns("sand"), "Sand %", value = NA, min = 0, max = 100),
                numericInput(ns("silt"), "Silt %", value = NA, min = 0, max = 100),
                numericInput(ns("clay"), "Clay %", value = NA, min = 0, max = 100)
              ),
              uiOutput(ns("texture_validation"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'class'", ns("texture_input_type")),
              selectInput(ns("texture_class"), "Texture Class", choices = c("", texture_classes))
            )
          ),

          # Location Panel
          accordion_panel(
            title = "Location",
            value = "location_panel",
            icon = icon("map-marker-alt"),
            textInput(ns("zipcode"), "ZIP Code (US)", placeholder = "Enter 5-digit ZIP"),
            uiOutput(ns("zipcode_status")),
            hr(),
            textInput(ns("address"), "Or Enter Address", placeholder = "123 Main St, City, State"),
            actionButton(ns("geocode"), "Geocode Address", class = "btn-sm btn-secondary mb-2"),
            uiOutput(ns("geocode_status")),
            hr(),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("latitude"), "Latitude", value = NA, step = 0.0001),
              numericInput(ns("longitude"), "Longitude", value = NA, step = 0.0001)
            ),
            textOutput(ns("ecoregion_display"))
          ),

          # Metadata Panel
          accordion_panel(
            title = "Sample Details",
            value = "metadata_panel",
            icon = icon("calendar"),
            dateInput(ns("sample_date"), "Sample Date", value = Sys.Date()),
            textInput(ns("photo_url"), "Photo URL (optional)", placeholder = "https://..."),
            textAreaInput(ns("notes"), "Notes", rows = 3, placeholder = "Optional notes...")
          )
        ),

        # Per-species fields (rendered dynamically)
        uiOutput(ns("per_species_fields")),

        # Submit button
        actionButton(ns("submit"), "Submit Sample", class = "btn-primary btn-lg w-100 mt-3", icon = icon("save"))
      ),

      # Main content area - Recent entries
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div("Recent Entries"),
          div(class = "small text-muted", textOutput(ns("entry_count"), inline = TRUE))
        ),
        card_body(
          DTOutput(ns("recent_entries"))
        )
      )
    )
  )
}

# ---------------------------
# Data Entry Module Server
# ---------------------------

dataEntryServer <- function(id, species_db, current_user, is_admin, get_all_samples, add_sample,
                            get_ecoregion, classify_texture, eco_sf, eco_grid, zipcode_db,
                            edaphic_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize species selectize with choices
    observe({
      updateSelectizeInput(
        session, "species",
        choices = species_db$taxon_name,
        server = TRUE
      )
    })

    # Species count indicator
    output$species_count_indicator <- renderUI({
      n <- length(input$species)
      if (n == 0) return(NULL)
      div(class = "small text-muted mt-1",
          sprintf(" %d species selected - %d record%s will be created",
                  n, n, if (n == 1) "" else "s"))
    })

    # Entry count
    output$entry_count <- renderText({
      data <- get_all_samples(limit = 1000)
      sprintf("%d entries", nrow(data))
    })

    # Recent entries table
    output$recent_entries <- renderDT({
      data <- get_all_samples(limit = 50)
      if (nrow(data) == 0) return(NULL)

      display_cols <- c("species", "ph", "organic_matter", "date", "ecoregion_l4")
      display_cols <- display_cols[display_cols %in% names(data)]

      datatable(
        data[, display_cols, drop = FALSE],
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tip'
        ),
        rownames = FALSE,
        selection = 'none'
      )
    })

    # Texture validation
    output$texture_validation <- renderUI({
      sand <- input$sand
      silt <- input$silt
      clay <- input$clay

      if (is.na(sand) && is.na(silt) && is.na(clay)) return(NULL)

      total <- sum(c(sand, silt, clay), na.rm = TRUE)

      if (total == 0) return(NULL)

      if (abs(total - 100) > 1) {
        div(class = "text-warning small", icon("exclamation-triangle"),
            sprintf(" Total: %.1f%% (should be ~100%%)", total))
      } else {
        div(class = "text-success small", icon("check-circle"),
            sprintf(" Total: %.1f%%", total))
      }
    })

    # ZIP code lookup
    observeEvent(input$zipcode, {
      zip <- trimws(input$zipcode)
      if (nchar(zip) != 5 || !grepl("^[0-9]{5}$", zip)) {
        output$zipcode_status <- renderUI(NULL)
        return()
      }

      if (is.null(zipcode_db)) {
        output$zipcode_status <- renderUI({
          div(class = "text-warning small", icon("exclamation-triangle"), " ZIP lookup unavailable")
        })
        return()
      }

      match <- zipcode_db[zipcode_db$zipcode == zip, ]
      if (nrow(match) > 0) {
        updateNumericInput(session, "latitude", value = match$lat[1])
        updateNumericInput(session, "longitude", value = match$lng[1])
        output$zipcode_status <- renderUI({
          div(class = "text-success small", icon("check-circle"), " Coordinates set from ZIP")
        })
      } else {
        output$zipcode_status <- renderUI({
          div(class = "text-warning small", icon("exclamation-triangle"), " ZIP not found")
        })
      }
    })

    # Geocode address
    observeEvent(input$geocode, {
      req(input$address)

      output$geocode_status <- renderUI({
        div(class = "text-info small", icon("spinner", class = "fa-spin"), " Geocoding...")
      })

      result <- tryCatch({
        tidygeocoder::geo(input$address, method = "osm")
      }, error = function(e) NULL)

      if (!is.null(result) && !is.na(result$lat) && !is.na(result$long)) {
        updateNumericInput(session, "latitude", value = result$lat)
        updateNumericInput(session, "longitude", value = result$long)
        output$geocode_status <- renderUI({
          div(class = "text-success small", icon("check-circle"), " Address geocoded")
        })
      } else {
        output$geocode_status <- renderUI({
          div(class = "text-warning small", icon("exclamation-triangle"), " Could not geocode address")
        })
      }
    })

    # Ecoregion display
    output$ecoregion_display <- renderText({
      lat <- input$latitude
      lon <- input$longitude

      if (is.na(lat) || is.na(lon)) return("")

      eco <- get_ecoregion(lat, lon, eco_sf, eco_grid)
      if (!is.null(eco$name) && nzchar(eco$name)) {
        paste("Ecoregion:", eco$name)
      } else {
        ""
      }
    })

    # Return values for parent module to use
    list(
      species = reactive(input$species),
      submit_clicked = reactive(input$submit)
    )
  })
}
