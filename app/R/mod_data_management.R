# R/mod_data_management.R - Data management module
# Export and CSV import functionality

# ---------------------------
# UI
# ---------------------------

dataManagementUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Import/Export",
    icon = icon("database"),
    layout_columns(
      col_widths = c(12),

      card(
        card_header(icon("file-export"), "Export Data"),
        card_body(
          p("Download all soil sample data as a CSV file."),
          downloadButton(ns("export_data"), "Export All Data", class = "btn-primary")
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
            id = ns("import_accordion"),
            open = FALSE,
            accordion_panel(
              title = "Show Import Options",
              value = "import_panel",
              icon = icon("upload"),
              p(class = "text-muted mb-3",
                "Use CSV import for bulk historical data. For new entries, ",
                "use the Data Entry tab which supports multiple species per soil sample."),

              div(class = "mb-3 p-3 bg-light rounded",
                tags$strong("Required columns:"),
                tags$code(class = "d-block mt-1 small",
                  "species, ph, organic_matter, texture_sand, texture_silt, texture_clay, location_lat, location_long"),
                tags$small(class = "text-muted d-block mt-2",
                  "Optional: cultivar, outcome, sun_exposure, site_hydrology, nitrate_ppm, phosphorus_ppm, ",
                  "potassium_ppm, calcium_ppm, magnesium_ppm, date, notes, and more. ",
                  "Download the template to see all available columns.")
              ),

              layout_column_wrap(
                width = 1/2,
                fileInput(ns("csv_import"), "Upload CSV File",
                          accept = c("text/csv", ".csv"),
                          buttonLabel = "Browse...",
                          placeholder = "No file selected"),
                div(class = "pt-4",
                  downloadButton(ns("download_template"), "Download Template", class = "btn-outline-secondary btn-sm")
                )
              ),

              uiOutput(ns("import_results"))
            )
          )
        )
      )
    )
  )
}

# ---------------------------
# Server
# ---------------------------

dataManagementServer <- function(id, pool, current_user, data_changed, soil_data_template) {
  moduleServer(id, function(input, output, session) {

    # Store import results for display
    import_results <- reactiveVal(NULL)

    # Import results display
    output$import_results <- renderUI({
      results <- import_results()
      if (is.null(results)) return(NULL)

      status_class <- if (results$success) "alert-success" else "alert-danger"
      status_icon <- if (results$success) "check-circle" else "exclamation-circle"

      div(class = paste("alert", status_class, "mt-3"),
        div(class = "d-flex align-items-start",
          icon(status_icon, class = "me-2 mt-1"),
          div(
            tags$strong(results$message),
            if (length(results$warnings) > 0) {
              div(class = "mt-2 small",
                tags$strong("Warnings:"),
                tags$ul(class = "mb-0 mt-1",
                  lapply(results$warnings, function(w) tags$li(w))
                )
              )
            },
            if (!is.null(results$species_added) && length(results$species_added) > 0) {
              div(class = "mt-2 small",
                tags$strong("Species imported: "),
                paste(head(results$species_added, 10), collapse = ", "),
                if (length(results$species_added) > 10) paste0(" and ", length(results$species_added) - 10, " more...")
              )
            }
          )
        )
      )
    })

    # Download template
    output$download_template <- downloadHandler(
      filename = function() "soil_data_template.csv",
      content  = function(file) write.csv(soil_data_template[0, ], file, row.names = FALSE)
    )

    # Export all data
    output$export_data <- downloadHandler(
      filename = function() paste0("soil_data_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        data <- db_get_all_samples()
        write.csv(data, file, row.names = FALSE)
        u <- current_user()
        if (!is.null(u)) {
          db_audit_log("export", "soil_samples", NULL, u$user_uid, sprintf("CSV export: %d records", nrow(data)))
        }
      }
    )

    # CSV Import
    observeEvent(input$csv_import, {
      req(input$csv_import)
      u <- current_user()
      if (is.null(u)) {
        import_results(list(success = FALSE, message = "Please sign in to import data.", warnings = c()))
        return()
      }

      tryCatch({
        imported <- read.csv(input$csv_import$datapath, stringsAsFactors = FALSE)

        required_cols <- c("species", "ph", "organic_matter", "texture_sand",
                           "texture_silt", "texture_clay", "location_lat", "location_long")
        miss <- setdiff(required_cols, names(imported))

        if (length(miss) > 0) {
          import_results(list(
            success = FALSE,
            message = paste("Missing required columns:", paste(miss, collapse = ", ")),
            warnings = c()
          ))
          return()
        }

        # Type validation for numeric columns
        numeric_cols <- c("ph", "organic_matter", "texture_sand", "texture_silt", "texture_clay",
                          "location_lat", "location_long", "nitrate_ppm", "ammonium_ppm",
                          "phosphorus_ppm", "potassium_ppm", "calcium_ppm", "magnesium_ppm",
                          "sulfur_ppm", "iron_ppm", "manganese_ppm", "zinc_ppm", "copper_ppm",
                          "boron_ppm", "cec_meq", "soluble_salts_ppm")
        warnings <- c()
        for (col in intersect(numeric_cols, names(imported))) {
          original <- imported[[col]]
          imported[[col]] <- suppressWarnings(as.numeric(imported[[col]]))
          bad_count <- sum(is.na(imported[[col]]) & !is.na(original) & original != "")
          if (bad_count > 0) {
            warnings <- c(warnings, sprintf("%s: %d non-numeric values converted to NA", col, bad_count))
          }
        }

        # Validate coordinate bounds
        if ("location_lat" %in% names(imported)) {
          bad_lat <- sum(imported$location_lat < -90 | imported$location_lat > 90, na.rm = TRUE)
          if (bad_lat > 0) warnings <- c(warnings, sprintf("Latitude: %d values out of range (-90 to 90)", bad_lat))
        }
        if ("location_long" %in% names(imported)) {
          bad_lon <- sum(imported$location_long < -180 | imported$location_long > 180, na.rm = TRUE)
          if (bad_lon > 0) warnings <- c(warnings, sprintf("Longitude: %d values out of range (-180 to 180)", bad_lon))
        }

        # Validate pH range
        if ("ph" %in% names(imported)) {
          bad_ph <- sum(imported$ph < 0 | imported$ph > 14, na.rm = TRUE)
          if (bad_ph > 0) warnings <- c(warnings, sprintf("pH: %d values out of range (0 to 14)", bad_ph))
        }

        count <- 0
        species_added <- c()
        for (i in seq_len(nrow(imported))) {
          row <- imported[i, ]
          row$created_by <- u$user_uid
          if (db_add_sample(row)) {
            count <- count + 1
            if (!is.na(row$species) && !(row$species %in% species_added)) {
              species_added <- c(species_added, row$species)
            }
          }
        }

        import_results(list(
          success = TRUE,
          message = sprintf("Successfully imported %d of %d samples.", count, nrow(imported)),
          warnings = warnings,
          species_added = species_added
        ))

        db_audit_log("import", "soil_samples", NULL, u$user_uid, sprintf("CSV import: %d records", count))
        data_changed(data_changed() + 1)
      }, error = function(e) {
        import_results(list(
          success = FALSE,
          message = paste("Import error:", e$message),
          warnings = c()
        ))
      })
    })
  })
}
