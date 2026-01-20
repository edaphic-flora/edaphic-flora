# R/mod_data_entry.R - Data Entry tab Shiny module
# Handles soil sample data collection with PDF extraction, species selection,
# location geocoding, and form submission.

# ---------------------------
# UI
# ---------------------------

dataEntryUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Data Entry",
    icon = icon("plus-circle"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Add Soil Sample",
        width = 380,
        bg = "#f8f9fa",

        # Soil Report Upload Section (conditional on API key availability)
        uiOutput(ns("pdf_upload_section")),

        # Reuse Previous Soil Data (shown if user has previous entries)
        uiOutput(ns("reuse_soil_section")),

        # Species (always visible - most important)
        selectizeInput(ns("species"), "Plant Species", choices = NULL, multiple = TRUE,
                       options = list(maxItems = 20, maxOptions = 100,
                                      placeholder = "Type to search species...")),
        uiOutput(ns("species_count_indicator")),

        # Per-species metadata (dynamic based on selected species)
        uiOutput(ns("per_species_fields")),

        helpText(class = "text-muted small",
                 "Tip: Select multiple species if they share the same soil sample."),

        accordion(
          id = ns("form_sections"),
          open = "soil_props",

          # Soil Properties
          accordion_panel(
            title = "Soil Properties",
            value = "soil_props",
            icon = icon("flask"),
            numericInput(ns("ph"), "Soil pH", value = NA, min = 0, max = 14, step = 0.1),
            numericInput(ns("organic_matter"), "Organic Matter (%)", value = NA, min = 0, max = 100, step = 0.1),
            selectInput(ns("organic_matter_class"), "Organic Matter (Qualitative)",
                        choices = c("Select if no % available" = "",
                                    "Very Low" = "Very Low",
                                    "Low" = "Low",
                                    "Medium Low" = "Medium Low",
                                    "Medium" = "Medium",
                                    "Medium High" = "Medium High",
                                    "High" = "High",
                                    "Very High" = "Very High")),
            numericInput(ns("cec"), "Cation Exchange Capacity (meq/100g)", value = NA, min = 0, step = 0.1),
            numericInput(ns("soluble_salts"), "Soluble Salts (ppm)", value = NA, min = 0)
          ),

          # Macronutrients
          accordion_panel(
            title = "Macronutrients (ppm)",
            value = "macronutrients",
            icon = icon("leaf"),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("nitrate"), "Nitrate (N)", value = NA, min = 0),
              numericInput(ns("ammonium"), "Ammonium (N)", value = NA, min = 0),
              numericInput(ns("phosphorus"), "Phosphorus (P)", value = NA, min = 0),
              numericInput(ns("potassium"), "Potassium (K)", value = NA, min = 0),
              numericInput(ns("calcium"), "Calcium (Ca)", value = NA, min = 0),
              numericInput(ns("magnesium"), "Magnesium (Mg)", value = NA, min = 0),
              numericInput(ns("sulfur"), "Sulfur (S)", value = NA, min = 0)
            )
          ),

          # Micronutrients
          accordion_panel(
            title = "Micronutrients (ppm)",
            value = "micronutrients",
            icon = icon("seedling"),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("iron"), "Iron (Fe)", value = NA, min = 0),
              numericInput(ns("manganese"), "Manganese (Mn)", value = NA, min = 0),
              numericInput(ns("zinc"), "Zinc (Zn)", value = NA, min = 0),
              numericInput(ns("copper"), "Copper (Cu)", value = NA, min = 0),
              numericInput(ns("boron"), "Boron (B)", value = NA, min = 0, step = 0.1)
            )
          ),

          # Texture
          accordion_panel(
            title = "Soil Texture",
            value = "texture",
            icon = icon("mountain"),
            radioButtons(ns("texture_input_type"), "Input Method:",
                         choices = c("Percentages" = "pct", "Classification" = "class"),
                         inline = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'pct'", ns("texture_input_type")),
              layout_column_wrap(
                width = 1/3,
                numericInput(ns("sand"), "Sand %", value = 33, min = 0, max = 100),
                numericInput(ns("silt"), "Silt %", value = 33, min = 0, max = 100),
                numericInput(ns("clay"), "Clay %", value = 34, min = 0, max = 100)
              ),
              uiOutput(ns("texture_validation"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'class'", ns("texture_input_type")),
              selectInput(ns("texture_class"), "Texture Class", choices = NULL)  # Populated in server
            )
          ),

          # Location
          accordion_panel(
            title = "Location",
            value = "location",
            icon = icon("map-marker-alt"),
            textInput(ns("street"), "Street Address (optional)", "",
                      placeholder = "123 Main St"),
            textInput(ns("zipcode"), "Zip Code", "",
                      placeholder = "Enter 5-digit zip"),
            div(class = "small mb-3", uiOutput(ns("location_status"))),
            layout_column_wrap(
              width = 1/2,
              textInput(ns("city"), "City/Town", ""),
              selectInput(ns("state"), "State", choices = state.name, selected = "New York")
            ),
            layout_column_wrap(
              width = 1/2,
              numericInput(ns("latitude"), "Latitude", value = NA, min = -90, max = 90, step = 0.0001),
              numericInput(ns("longitude"), "Longitude", value = NA, min = -180, max = 180, step = 0.0001)
            ),
            tags$small(class = "text-muted",
                       "Coordinates auto-fill from zip code, or enter manually.")
          ),

          # Additional
          accordion_panel(
            title = "Additional Info",
            value = "additional",
            icon = icon("info-circle"),
            dateInput(ns("date"), "Sample Date", value = Sys.Date()),
            textAreaInput(ns("notes"), "Notes", "", height = "80px",
                          placeholder = "General notes about this soil sample...")
          )
        ),

        hr(),
        actionButton(ns("submit"), "Submit Sample", class = "btn-primary btn-lg w-100",
                     icon = icon("paper-plane"))
      ),

      # Main content - recent entries
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span(icon("clock"), "Recent Entries"),
          span(class = "badge bg-secondary", textOutput(ns("entry_count"), inline = TRUE))
        ),
        card_body(
          DTOutput(ns("recent_entries"))
        )
      )
    )
  )
}

# ---------------------------
# Server
# ---------------------------

dataEntryServer <- function(id, pool, species_db, zipcode_db, soil_texture_classes,
                            current_user, is_admin, data_changed, lookup_ecoregion,
                            pdf_extract_limit, beta_features = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive trigger to refresh extraction status after each extraction
    extraction_trigger <- reactiveVal(0)

    # State for soil data reuse
    user_soil_profiles <- reactiveVal(NULL)  # Cached soil profiles for picker

    # --- PDF Upload Section (conditional on API key) ---
    output$pdf_upload_section <- renderUI({
      if (!is_pdf_extraction_available()) return(NULL)

      div(
        class = "mb-3 p-3 border rounded bg-light",
        div(class = "d-flex align-items-center mb-2",
            icon("file-lines", class = "text-success me-2"),
            strong("Upload Soil Report"),
            span(class = "badge bg-info ms-2", "Beta")),
        fileInput(ns("pdf_upload"), NULL,
                  accept = c("application/pdf", ".pdf", ".rtf", ".txt",
                             "image/png", "image/jpeg", "image/gif", "image/webp",
                             ".png", ".jpg", ".jpeg", ".gif", ".webp"),
                  buttonLabel = "Choose File",
                  placeholder = "No file selected"),
        uiOutput(ns("pdf_extract_status")),
        helpText(class = "text-muted small",
                 "Upload a soil report to auto-fill the form. ",
                 "Supports PDF, RTF, TXT, and images (PNG, JPG).")
      )
    })

    # --- Reuse Previous Soil Data Section ---
    output$reuse_soil_section <- renderUI({
      u <- current_user()
      if (is.null(u)) return(NULL)

      # Check if user has any previous entries
      profiles <- db_get_user_soil_profiles(u$user_uid, limit = 5)
      user_soil_profiles(profiles)  # Cache for modal

      if (nrow(profiles) == 0) return(NULL)

      div(
        class = "mb-3",
        actionLink(ns("show_reuse_modal"),
                   label = tagList(icon("recycle"), " Use previous soil data"),
                   class = "text-success"),
        tags$small(class = "text-muted d-block",
                   sprintf("You have %d saved soil test%s",
                           nrow(profiles), if (nrow(profiles) == 1) "" else "s"))
      )
    })

    # Show reuse modal when link clicked
    observeEvent(input$show_reuse_modal, {
      profiles <- user_soil_profiles()
      if (is.null(profiles) || nrow(profiles) == 0) {
        showNotification("No previous soil data found.", type = "warning")
        return()
      }

      # Build choices for the picker
      choices <- lapply(seq_len(nrow(profiles)), function(i) {
        p <- profiles[i, ]
        label <- sprintf("pH %.1f | OM %.1f%% | %s (from %s)",
                         p$ph %||% NA, p$organic_matter %||% NA,
                         p$texture_class %||% "Unknown texture",
                         format(as.Date(p$date), "%b %d, %Y"))
        list(id = p$id, label = label)
      })

      showModal(modalDialog(
        title = span(icon("recycle"), " Use Previous Soil Data"),
        size = "m",
        easyClose = TRUE,
        footer = modalButton("Cancel"),

        p("Select a previous soil test to auto-fill the form:"),

        radioButtons(ns("reuse_profile_choice"), NULL,
                     choices = setNames(
                       sapply(choices, function(x) x$id),
                       sapply(choices, function(x) x$label)
                     )),

        actionButton(ns("apply_reuse"), "Apply Soil Data",
                     class = "btn-primary mt-3", icon = icon("check"))
      ))
    })

    # Apply selected soil data to form
    observeEvent(input$apply_reuse, {
      selected_id <- as.integer(input$reuse_profile_choice)
      soil_data <- db_get_soil_data_by_id(selected_id)

      if (is.null(soil_data)) {
        showNotification("Could not load soil data.", type = "error")
        return()
      }

      # Fill form fields
      if (!is.null(soil_data$ph) && !is.na(soil_data$ph)) {
        updateNumericInput(session, "ph", value = soil_data$ph)
      }
      if (!is.null(soil_data$organic_matter) && !is.na(soil_data$organic_matter)) {
        updateNumericInput(session, "organic_matter", value = soil_data$organic_matter)
      }
      if (!is.null(soil_data$organic_matter_class) && !is.na(soil_data$organic_matter_class)) {
        updateSelectInput(session, "organic_matter_class", selected = soil_data$organic_matter_class)
      }
      if (!is.null(soil_data$cec_meq) && !is.na(soil_data$cec_meq)) {
        updateNumericInput(session, "cec", value = soil_data$cec_meq)
      }
      if (!is.null(soil_data$soluble_salts_ppm) && !is.na(soil_data$soluble_salts_ppm)) {
        updateNumericInput(session, "soluble_salts", value = soil_data$soluble_salts_ppm)
      }

      # Macronutrients
      if (!is.null(soil_data$nitrate_ppm) && !is.na(soil_data$nitrate_ppm)) {
        updateNumericInput(session, "nitrate", value = soil_data$nitrate_ppm)
      }
      if (!is.null(soil_data$ammonium_ppm) && !is.na(soil_data$ammonium_ppm)) {
        updateNumericInput(session, "ammonium", value = soil_data$ammonium_ppm)
      }
      if (!is.null(soil_data$phosphorus_ppm) && !is.na(soil_data$phosphorus_ppm)) {
        updateNumericInput(session, "phosphorus", value = soil_data$phosphorus_ppm)
      }
      if (!is.null(soil_data$potassium_ppm) && !is.na(soil_data$potassium_ppm)) {
        updateNumericInput(session, "potassium", value = soil_data$potassium_ppm)
      }
      if (!is.null(soil_data$calcium_ppm) && !is.na(soil_data$calcium_ppm)) {
        updateNumericInput(session, "calcium", value = soil_data$calcium_ppm)
      }
      if (!is.null(soil_data$magnesium_ppm) && !is.na(soil_data$magnesium_ppm)) {
        updateNumericInput(session, "magnesium", value = soil_data$magnesium_ppm)
      }
      if (!is.null(soil_data$sulfur_ppm) && !is.na(soil_data$sulfur_ppm)) {
        updateNumericInput(session, "sulfur", value = soil_data$sulfur_ppm)
      }

      # Micronutrients
      if (!is.null(soil_data$iron_ppm) && !is.na(soil_data$iron_ppm)) {
        updateNumericInput(session, "iron", value = soil_data$iron_ppm)
      }
      if (!is.null(soil_data$manganese_ppm) && !is.na(soil_data$manganese_ppm)) {
        updateNumericInput(session, "manganese", value = soil_data$manganese_ppm)
      }
      if (!is.null(soil_data$zinc_ppm) && !is.na(soil_data$zinc_ppm)) {
        updateNumericInput(session, "zinc", value = soil_data$zinc_ppm)
      }
      if (!is.null(soil_data$copper_ppm) && !is.na(soil_data$copper_ppm)) {
        updateNumericInput(session, "copper", value = soil_data$copper_ppm)
      }
      if (!is.null(soil_data$boron_ppm) && !is.na(soil_data$boron_ppm)) {
        updateNumericInput(session, "boron", value = soil_data$boron_ppm)
      }

      # Texture
      if (!is.null(soil_data$texture_class) && !is.na(soil_data$texture_class)) {
        updateRadioButtons(session, "texture_input_type", selected = "class")
        updateSelectInput(session, "texture_class", selected = soil_data$texture_class)
      } else if (!is.null(soil_data$texture_sand) && !is.na(soil_data$texture_sand)) {
        updateRadioButtons(session, "texture_input_type", selected = "pct")
        updateNumericInput(session, "sand", value = soil_data$texture_sand)
        updateNumericInput(session, "silt", value = soil_data$texture_silt)
        updateNumericInput(session, "clay", value = soil_data$texture_clay)
      }

      removeModal()
      showNotification("Soil data applied! Now select species and submit.",
                       type = "message", duration = 4)
    })

    # --- Species dropdown population ---
    observe({
      updateSelectizeInput(session, "species",
                           choices = sort(species_db$taxon_name),
                           selected = character(0),
                           server = TRUE,
                           options = list(
                             maxItems = 20,
                             maxOptions = 100,
                             placeholder = "Type to search species..."
                           ))
    })

    # --- Texture class dropdown ---
    observe({
      updateSelectInput(session, "texture_class", choices = soil_texture_classes$Texture)
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

    # --- PDF Extraction Status ---
    output$pdf_extract_status <- renderUI({
      # Depend on trigger to refresh after each extraction
      extraction_trigger()

      u <- current_user()
      if (is.null(u)) {
        return(div(class = "text-muted small", "Sign in to use PDF extraction"))
      }

      # Admin has unlimited extractions
      if (is_admin()) {
        return(div(class = "text-success small", icon("infinity"), " Unlimited extractions (admin)"))
      }

      remaining <- db_get_remaining_extractions(u$user_uid, pdf_extract_limit)
      if (remaining > 0) {
        div(class = "text-muted small",
            icon("clock"), sprintf(" %d extraction%s remaining today", remaining, if (remaining == 1) "" else "s"))
      } else {
        div(class = "text-warning small",
            icon("exclamation-triangle"), " Daily limit reached. Try again tomorrow.")
      }
    })

    # --- PDF Upload Handler ---
    observeEvent(input$pdf_upload, {
      req(input$pdf_upload)
      u <- current_user()

      if (is.null(u)) {
        showNotification("Please sign in to use PDF extraction.", type = "error")
        return()
      }

      # Check rate limit (admin bypass)
      if (!is_admin() && !db_can_extract(u$user_uid, pdf_extract_limit)) {
        showNotification("Daily extraction limit reached. Try again tomorrow.", type = "warning")
        return()
      }

      # Show processing notification
      notif_id <- showNotification("Extracting data from report...", type = "message", duration = NULL)

      # Perform extraction (synchronous)
      result <- extract_soil_data_from_pdf(input$pdf_upload$datapath)

      removeNotification(notif_id)

      if (result$success) {
        # Log the extraction and refresh status display
        db_log_extraction(u$user_uid, input$pdf_upload$name, result$tokens_used)
        extraction_trigger(extraction_trigger() + 1)

        # Populate form fields
        data <- result$data

        # Soil Properties
        if (!is.null(data$ph)) updateNumericInput(session, "ph", value = data$ph)
        if (!is.null(data$organic_matter)) updateNumericInput(session, "organic_matter", value = data$organic_matter)
        if (!is.null(data$organic_matter_class) && nzchar(data$organic_matter_class)) {
          updateSelectInput(session, "organic_matter_class", selected = data$organic_matter_class)
        }
        if (!is.null(data$cec_meq)) updateNumericInput(session, "cec", value = data$cec_meq)
        if (!is.null(data$soluble_salts_ppm)) updateNumericInput(session, "soluble_salts", value = data$soluble_salts_ppm)

        # Macronutrients
        if (!is.null(data$nitrate_ppm)) updateNumericInput(session, "nitrate", value = data$nitrate_ppm)
        if (!is.null(data$ammonium_ppm)) updateNumericInput(session, "ammonium", value = data$ammonium_ppm)
        if (!is.null(data$phosphorus_ppm)) updateNumericInput(session, "phosphorus", value = data$phosphorus_ppm)
        if (!is.null(data$potassium_ppm)) updateNumericInput(session, "potassium", value = data$potassium_ppm)
        if (!is.null(data$calcium_ppm)) updateNumericInput(session, "calcium", value = data$calcium_ppm)
        if (!is.null(data$magnesium_ppm)) updateNumericInput(session, "magnesium", value = data$magnesium_ppm)
        if (!is.null(data$sulfur_ppm)) updateNumericInput(session, "sulfur", value = data$sulfur_ppm)

        # Micronutrients
        if (!is.null(data$iron_ppm)) updateNumericInput(session, "iron", value = data$iron_ppm)
        if (!is.null(data$manganese_ppm)) updateNumericInput(session, "manganese", value = data$manganese_ppm)
        if (!is.null(data$zinc_ppm)) updateNumericInput(session, "zinc", value = data$zinc_ppm)
        if (!is.null(data$boron_ppm)) updateNumericInput(session, "boron", value = data$boron_ppm)
        if (!is.null(data$copper_ppm)) updateNumericInput(session, "copper", value = data$copper_ppm)

        # Texture - handle both percentage and classification inputs
        has_percentages <- !is.null(data$texture_sand) || !is.null(data$texture_silt) || !is.null(data$texture_clay)
        has_class <- !is.null(data$texture_class) && nzchar(data$texture_class)

        if (has_percentages) {
          # If we have percentages, use percentage mode
          updateRadioButtons(session, "texture_input_type", selected = "pct")
          if (!is.null(data$texture_sand)) updateNumericInput(session, "sand", value = data$texture_sand)
          if (!is.null(data$texture_silt)) updateNumericInput(session, "silt", value = data$texture_silt)
          if (!is.null(data$texture_clay)) updateNumericInput(session, "clay", value = data$texture_clay)
        } else if (has_class) {
          # If we only have class (no percentages), use classification mode
          updateRadioButtons(session, "texture_input_type", selected = "class")
          updateSelectInput(session, "texture_class", selected = data$texture_class)
        }

        # Date
        if (!is.null(data$sample_date)) {
          tryCatch({
            updateDateInput(session, "date", value = as.Date(data$sample_date))
          }, error = function(e) {})
        }

        # Notes - append extraction info
        notes_text <- ""
        if (!is.null(data$lab_name)) notes_text <- paste0("Lab: ", data$lab_name)
        if (!is.null(data$sample_id) && nzchar(data$sample_id)) {
          notes_text <- paste(notes_text, paste0("Sample ID: ", data$sample_id), sep = if (nzchar(notes_text)) "; " else "")
        }
        if (!is.null(data$notes) && nzchar(data$notes)) {
          notes_text <- paste(notes_text, data$notes, sep = if (nzchar(notes_text)) "; " else "")
        }
        if (nzchar(notes_text)) {
          current_notes <- input$notes
          new_notes <- if (nzchar(current_notes)) paste(current_notes, notes_text, sep = "\n") else notes_text
          updateTextAreaInput(session, "notes", value = new_notes)
        }

        # Handle extraction warnings
        if (!is.null(data$extraction_warnings) && length(data$extraction_warnings) > 0) {
          warnings_list <- unlist(data$extraction_warnings)
          warning_msg <- paste(warnings_list, collapse = "; ")
          showNotification(
            paste("Extraction warnings:", warning_msg),
            type = "warning",
            duration = 10
          )
        }

        showNotification("Data extracted! Please review values before submitting.", type = "message", duration = 5)

      } else {
        # Log full error for debugging
        message("PDF extraction error: ", result$error)
        # Show user-friendly message
        error_msg <- if (grepl("API key", result$error, ignore.case = TRUE)) {
          "AI extraction service not configured. Please enter values manually."
        } else if (grepl("timeout|timed out", result$error, ignore.case = TRUE)) {
          "Extraction timed out. Try a smaller file or enter values manually."
        } else if (grepl("file|format|read", result$error, ignore.case = TRUE)) {
          "Could not read file. Please ensure it's a valid PDF, image, or text file."
        } else {
          "Could not extract data from this file. Please enter values manually."
        }
        showNotification(error_msg, type = "error", duration = 8)
      }
    })

    # --- Per-species metadata fields ---
    output$per_species_fields <- renderUI({
      species_list <- input$species
      if (is.null(species_list) || length(species_list) == 0) return(NULL)

      # Create a collapsible card for per-species details
      card(
        class = "mb-3",
        card_header(
          class = "py-2 d-flex justify-content-between align-items-center",
          span(icon("list"), " Species Details",
               tags$small(class = "text-muted ms-2", "(per species)")),
          tags$small(class = "text-muted",
                     title = "See Help > Field Guide for field definitions",
                     icon("question-circle"), " Field definitions in Help")
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
                textInput(ns(paste0("cultivar_", sp_id)), "Cultivar",
                          placeholder = "e.g., 'Forest Pansy'"),
                div(
                  selectInput(ns(paste0("outcome_", sp_id)), "Outcome",
                              choices = c("Select..." = "", "Thriving" = "Thriving",
                                          "Established" = "Established",
                                          "Struggling" = "Struggling",
                                          "Failed/Died" = "Failed/Died"),
                              selected = ""),
                  tags$small(class = "text-muted", style = "margin-top: -10px; display: block;",
                             "How is this plant performing?")
                )
              ),
              layout_column_wrap(
                width = 1/2,
                div(
                  selectInput(ns(paste0("sun_", sp_id)), "Sun Exposure",
                              choices = c("Select..." = "", "Full Sun" = "Full Sun",
                                          "Part Sun" = "Part Sun",
                                          "Part Shade" = "Part Shade",
                                          "Full Shade" = "Full Shade"),
                              selected = ""),
                  tags$small(class = "text-muted", style = "margin-top: -10px; display: block;",
                             "Light at planting site")
                ),
                div(
                  selectInput(ns(paste0("hydrology_", sp_id)), "Site Hydrology",
                              choices = c("Select..." = "", "Dry/Xeric" = "Dry",
                                          "Mesic" = "Mesic",
                                          "Wet/Hydric" = "Wet"),
                              selected = ""),
                  tags$small(class = "text-muted", style = "margin-top: -10px; display: block;",
                             "Soil moisture conditions")
                )
              ),
              textInput(ns(paste0("inat_", sp_id)), "iNaturalist URL",
                        placeholder = "https://www.inaturalist.org/observations/..."),
              tags$small(class = "text-muted", style = "margin-top: -10px; display: block;",
                         "Optional. Helps verify plant ID for data quality. Mark on iNaturalist as ",
                         tags$em("Captive/Cultivated"), " for planted specimens or ",
                         tags$em("Wild"), " if it appeared spontaneously.")
            )
          })
        )
      )
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

    # --- Recent entries table ---
    output$recent_entries <- renderDT({
      data_changed()
      u <- current_user()
      user_id <- if (!is.null(u)) u$user_uid else ""
      admin_user <- is_admin()

      dat <- db_get_all_samples(limit = 50)  # Limit at SQL level for performance
      if (nrow(dat) == 0) return(NULL)

      display <- dat %>%
        select(id, species, outcome, ph, organic_matter, texture_class, date, created_by) %>%
        mutate(date = as.character(date),
               outcome = ifelse(is.na(outcome), "", outcome))

      # Add action buttons for user's own entries (or all entries for admin)
      # Note: Using global (non-namespaced) input IDs for edit/delete
      # These are handled by the edit/delete handlers in app.R
      display$actions <- sapply(seq_len(nrow(display)), function(i) {
        is_owner <- !is.na(display$created_by[i]) && display$created_by[i] == user_id
        if (is_owner || admin_user) {
          sprintf(
            "<button class=\"btn btn-sm btn-outline-primary me-1\" onclick=\"Shiny.setInputValue('edit_entry', %d, {priority: 'event'})\"><i class=\"fa fa-edit\"></i></button><button class=\"btn btn-sm btn-outline-danger\" onclick=\"Shiny.setInputValue('delete_entry', %d, {priority: 'event'})\"><i class=\"fa fa-trash\"></i></button>",
            display$id[i], display$id[i]
          )
        } else {
          ""
        }
      })

      # Remove created_by from display
      display <- display %>% select(-created_by)

      datatable(display,
                options = list(pageLength = 10, dom = 'tip', scrollX = TRUE),
                rownames = FALSE,
                escape = FALSE,  # Allow HTML in actions column
                colnames = c("ID", "Species", "Outcome", "pH", "OM %", "Texture", "Date", "Actions"))
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

    # --- Auto Location Lookup ---
    cached_zip_result <- reactiveVal(NULL)

    # Helper function to perform geocoding (called after delay so spinner shows)
    do_geocode <- function(zip_result, zip_clean, street) {
      # Build address for geocoding
      address <- if (nzchar(trimws(street))) {
        paste0(street, ", ", zip_result$city, ", ", zip_result$state, " ", zip_clean)
      } else {
        paste0(zip_result$city, ", ", zip_result$state, " ", zip_clean)
      }

      tryCatch({
        res <- geo(address = address, method = "osm")

        if (!is.null(res) && nrow(res) > 0 && !is.na(res$lat[1])) {
          # Geocoding succeeded
          updateNumericInput(session, "latitude", value = round(res$lat[1], 6))
          updateNumericInput(session, "longitude", value = round(res$long[1], 6))

          # Lookup ecoregion
          eco <- tryCatch(lookup_ecoregion(res$lat[1], res$long[1]),
                          error = function(e) list(name = NA, code = NA))

          output$location_status <- renderUI({
            div(class = "text-success",
                icon("check-circle"),
                sprintf(" %s, %s", zip_result$city, zip_result$state),
                if (!is.null(eco$l4_name) && !is.na(eco$l4_name)) {
                  tags$small(class = "text-muted d-block",
                             icon("map"), " ", eco$l4_name)
                } else if (!is.null(eco$name) && !is.na(eco$name)) {
                  tags$small(class = "text-muted d-block",
                             icon("map"), " ", eco$name)
                }
            )
          })
        } else {
          # Geocoding failed, fall back to zipcode centroid
          if (!is.na(zip_result$latitude) && !is.na(zip_result$longitude)) {
            updateNumericInput(session, "latitude", value = round(zip_result$latitude, 6))
            updateNumericInput(session, "longitude", value = round(zip_result$longitude, 6))

            output$location_status <- renderUI({
              div(class = "text-success",
                  icon("check-circle"),
                  sprintf(" %s, %s (approximate)", zip_result$city, zip_result$state))
            })
          } else {
            output$location_status <- renderUI({
              div(class = "text-warning",
                  icon("exclamation-triangle"),
                  sprintf(" %s, %s - coordinates unavailable", zip_result$city, zip_result$state))
            })
          }
        }
      }, error = function(e) {
        message("Geocoding error: ", e$message)
        # Fall back to zipcode centroid on error
        if (!is.na(zip_result$latitude) && !is.na(zip_result$longitude)) {
          updateNumericInput(session, "latitude", value = round(zip_result$latitude, 6))
          updateNumericInput(session, "longitude", value = round(zip_result$longitude, 6))

          output$location_status <- renderUI({
            div(class = "text-success",
                icon("check-circle"),
                sprintf(" %s, %s (approximate)", zip_result$city, zip_result$state))
          })
        } else {
          output$location_status <- renderUI({
            div(class = "text-warning",
                icon("exclamation-triangle"),
                " Could not get coordinates. Enter manually if needed.")
          })
        }
      })
    }

    # When zipcode changes, show spinner then geocode after delay
    observeEvent(input$zipcode, {
      zip <- input$zipcode
      zip_clean <- gsub("[^0-9]", "", zip)

      if (nchar(zip_clean) != 5) {
        output$location_status <- renderUI(NULL)
        cached_zip_result(NULL)
        return()
      }

      # Lookup city/state from zipcode database (fast, local)
      zip_result <- lookup_zipcode(zip_clean, zipcode_db)

      if (is.null(zip_result)) {
        output$location_status <- renderUI({
          div(class = "text-warning",
              icon("exclamation-triangle"),
              " Zip code not found. Enter city/state manually.")
        })
        cached_zip_result(NULL)
        return()
      }

      # Auto-fill city and state immediately
      state_full <- state.name[match(zip_result$state, state.abb)]
      if (is.na(state_full)) state_full <- zip_result$state

      updateTextInput(session, "city", value = zip_result$city)
      updateSelectInput(session, "state", selected = state_full)
      cached_zip_result(zip_result)

      # Show spinner
      output$location_status <- renderUI({
        div(class = "text-info",
            icon("spinner", class = "fa-spin"),
            sprintf(" Looking up coordinates for %s, %s...", zip_result$city, zip_result$state))
      })

      # Schedule geocoding after brief delay so spinner can render
      street <- input$street
      later::later(function() {
        do_geocode(zip_result, zip_clean, street)
      }, delay = 0.1)
    }, ignoreInit = TRUE)

    # When street address changes (and we have a zip), re-geocode
    observeEvent(input$street, {
      zip_result <- cached_zip_result()
      if (is.null(zip_result)) return()

      # Only re-geocode if street has content
      street <- trimws(input$street)
      if (!nzchar(street)) return()

      zip_clean <- gsub("[^0-9]", "", input$zipcode)

      # Show spinner
      output$location_status <- renderUI({
        div(class = "text-info",
            icon("spinner", class = "fa-spin"),
            " Refining coordinates with street address...")
      })

      # Schedule geocoding after brief delay so spinner can render
      later::later(function() {
        do_geocode(zip_result, zip_clean, street)
      }, delay = 0.1)
    }, ignoreInit = TRUE)

    # --- Submit sample ---

    # Helper function to perform the actual submission
    perform_submit <- function(reuse_soil_data = NULL) {
      u <- current_user()
      if (is.null(u)) {
        showNotification("Please sign in to submit data.", type = "error")
        return()
      }

      species_list <- input$species

      # Calculate shared values once
      eco <- tryCatch(lookup_ecoregion(input$latitude, input$longitude),
                      error = function(e) list(name = NA, code = NA))

      # Get soil chemistry from form (form may have been pre-filled via "Use previous soil data")
      ph_val <- input$ph
      om_val <- input$organic_matter
      om_class_val <- if (nzchar(input$organic_matter_class)) input$organic_matter_class else NA
      cec_val <- input$cec
      salts_val <- input$soluble_salts
      nitrate_val <- input$nitrate
      ammonium_val <- input$ammonium
      phosphorus_val <- input$phosphorus
      potassium_val <- input$potassium
      calcium_val <- input$calcium
      magnesium_val <- input$magnesium
      sulfur_val <- input$sulfur
      iron_val <- input$iron
      manganese_val <- input$manganese
      zinc_val <- input$zinc
      copper_val <- input$copper
      boron_val <- input$boron

      # Calculate texture
      if (input$texture_input_type == "pct") {
        texture_class_val <- classify_texture(input$sand, input$silt, input$clay, soil_texture_classes)
        sand_val <- input$sand
        silt_val <- input$silt
        clay_val <- input$clay
      } else {
        texture_class_val <- input$texture_class
        texture_pcts <- get_texture_percentages(input$texture_class, soil_texture_classes)
        sand_val <- texture_pcts$sand
        silt_val <- texture_pcts$silt
        clay_val <- texture_pcts$clay
      }

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
          ph = ph_val,
          organic_matter = om_val,
          organic_matter_class = om_class_val,
          cec_meq = cec_val,
          soluble_salts_ppm = salts_val,
          nitrate_ppm = nitrate_val,
          ammonium_ppm = ammonium_val,
          phosphorus_ppm = phosphorus_val,
          potassium_ppm = potassium_val,
          calcium_ppm = calcium_val,
          magnesium_ppm = magnesium_val,
          sulfur_ppm = sulfur_val,
          iron_ppm = iron_val,
          manganese_ppm = manganese_val,
          zinc_ppm = zinc_val,
          copper_ppm = copper_val,
          boron_ppm = boron_val,
          texture_class = texture_class_val,
          texture_sand = sand_val,
          texture_silt = silt_val,
          texture_clay = clay_val,
          ecoregion_l4 = eco$l4_name %||% eco$name,
          ecoregion_l4_code = eco$l4_code %||% eco$code,
          ecoregion_l3 = eco$l3_name,
          ecoregion_l3_code = eco$l3_code,
          ecoregion_l2 = eco$l2_name,
          ecoregion_l2_code = eco$l2_code,
          location_lat = input$latitude,
          location_long = input$longitude,
          notes = input$notes,
          date = input$date,
          created_by = u$user_uid,
          stringsAsFactors = FALSE
        )

        sample_id <- db_add_sample(new_data)
        if (!is.null(sample_id)) {
          success_count <- success_count + 1
          db_audit_log("create", "soil_samples", sample_id, u$user_uid, sprintf("species: %s", sp))
        }
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
    }

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

      # Validate species are in the database
      invalid_species <- species_list[!species_list %in% species_db$taxon_name]
      if (length(invalid_species) > 0) {
        showNotification(
          paste("Invalid species:", paste(invalid_species[1:min(3, length(invalid_species))], collapse = ", "),
                if (length(invalid_species) > 3) "..." else ""),
          type = "error"
        )
        return()
      }

      # Require at least pH or organic matter
      has_ph <- !is.null(input$ph) && !is.na(input$ph)
      has_om <- !is.null(input$organic_matter) && !is.na(input$organic_matter)
      if (!has_ph && !has_om) {
        showNotification("Please enter at least pH or organic matter percentage.", type = "error")
        return()
      }

      if (input$texture_input_type == "pct") {
        if (abs((input$sand + input$silt + input$clay) - 100) > 0.1) {
          showNotification("Soil texture percentages must sum to 100%", type = "error")
          return()
        }
      }

      # Validate coordinates if provided
      lat <- input$latitude
      lon <- input$longitude
      if (!is.null(lat) && !is.na(lat)) {
        if (lat < -90 || lat > 90) {
          showNotification("Latitude must be between -90 and 90 degrees", type = "error")
          return()
        }
      }
      if (!is.null(lon) && !is.na(lon)) {
        if (lon < -180 || lon > 180) {
          showNotification("Longitude must be between -180 and 180 degrees", type = "error")
          return()
        }
      }

      # Proceed with submit
      perform_submit(reuse_soil_data = NULL)
    })

  })
}
