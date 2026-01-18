# R/mod_data_management.R - Data management module
# Export and CSV import functionality

# ---------------------------
# UI
# ---------------------------

dataManagementUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Data",
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
              fileInput(ns("csv_import"), "Upload CSV File",
                        accept = c("text/csv", ".csv"),
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              downloadButton(ns("download_template"), "Download Template", class = "btn-outline-secondary btn-sm")
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
            warnings <- c(warnings, sprintf("%s: %d non-numeric values", col, bad_count))
          }
        }

        # Validate coordinate bounds
        if ("location_lat" %in% names(imported)) {
          bad_lat <- sum(imported$location_lat < -90 | imported$location_lat > 90, na.rm = TRUE)
          if (bad_lat > 0) warnings <- c(warnings, sprintf("latitude: %d out of range (-90 to 90)", bad_lat))
        }
        if ("location_long" %in% names(imported)) {
          bad_lon <- sum(imported$location_long < -180 | imported$location_long > 180, na.rm = TRUE)
          if (bad_lon > 0) warnings <- c(warnings, sprintf("longitude: %d out of range (-180 to 180)", bad_lon))
        }

        # Validate pH range
        if ("ph" %in% names(imported)) {
          bad_ph <- sum(imported$ph < 0 | imported$ph > 14, na.rm = TRUE)
          if (bad_ph > 0) warnings <- c(warnings, sprintf("pH: %d out of range (0 to 14)", bad_ph))
        }

        if (length(warnings) > 0) {
          showNotification(
            paste("Data warnings:", paste(warnings, collapse = "; ")),
            type = "warning", duration = 10
          )
        }

        count <- 0
        for (i in seq_len(nrow(imported))) {
          row <- imported[i, ]
          row$created_by <- u$user_uid
          if (db_add_sample(row)) count <- count + 1
        }

        showNotification(sprintf("Imported %d samples successfully!", count), type = "message")
        db_audit_log("import", "soil_samples", NULL, u$user_uid, sprintf("CSV import: %d records", count))
        data_changed(data_changed() + 1)
      }, error = function(e) {
        showNotification(paste("Import error:", e$message), type = "error")
      })
    })
  })
}
