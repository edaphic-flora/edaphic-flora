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
    div(class = "container-fluid", style = "max-width: 960px; margin: 0 auto; padding: 1.5rem 1rem;",

      # --- "View my entries" link (top-right) ---
      div(class = "d-flex justify-content-end mb-2",
          actionLink(ns("show_my_data"),
                     span(icon("table"), " View my entries",
                          textOutput(ns("entry_count"), inline = TRUE)),
                     class = "text-muted small")
      ),

      # --- Wizard Section (full width, primary focus) ---
      div(class = "card mb-4",
          div(class = "card-body",
              h5("Add Soil Sample", class = "card-title mb-3",
                 style = "font-family: 'Montserrat', sans-serif; color: #373D3C;"),

              # Wizard step indicator
              uiOutput(ns("wizard_steps_ui")),

              # Wizard content (step-specific form content)
              uiOutput(ns("wizard_content")),

              # Wizard navigation buttons (static to avoid renderUI click-counter reset)
              shinyjs::useShinyjs(),
              div(class = "d-flex justify-content-between mt-3",
                  div(id = ns("wizard_back_container"), style = "display:none;",
                      actionButton(ns("wizard_back"), "Back", class = "btn-outline-secondary",
                                   icon = icon("arrow-left"))),
                  div(id = ns("wizard_next_container"), style = "display:none;",
                      actionButton(ns("wizard_next"), "Next", class = "btn-primary",
                                   icon = icon("arrow-right")))
              )
          ),

          # --- Hidden form fields (always in DOM so values persist across steps) ---
          # These are the actual inputs that hold form state. They're in hidden divs
          # and their values are synced to/from the wizard step UIs via server logic.
          div(style = "display: none;",
            # Soil Properties
            numericInput(ns("ph"), "pH", value = NA, min = 0, max = 14, step = 0.1),
            numericInput(ns("organic_matter"), "OM", value = NA, min = 0, max = 100, step = 0.1),
            selectInput(ns("organic_matter_class"), "OM Class",
                        choices = c("", "Very Low", "Low", "Medium Low", "Medium",
                                    "Medium High", "High", "Very High")),
            numericInput(ns("cec"), "CEC", value = NA, min = 0, step = 0.1),
            numericInput(ns("soluble_salts"), "Salts", value = NA, min = 0),
            # Macronutrients
            numericInput(ns("nitrate"), "N", value = NA, min = 0),
            numericInput(ns("ammonium"), "NH4", value = NA, min = 0),
            numericInput(ns("phosphorus"), "P", value = NA, min = 0),
            numericInput(ns("potassium"), "K", value = NA, min = 0),
            numericInput(ns("calcium"), "Ca", value = NA, min = 0),
            numericInput(ns("magnesium"), "Mg", value = NA, min = 0),
            numericInput(ns("sulfur"), "S", value = NA, min = 0),
            # Micronutrients
            numericInput(ns("iron"), "Fe", value = NA, min = 0),
            numericInput(ns("manganese"), "Mn", value = NA, min = 0),
            numericInput(ns("zinc"), "Zn", value = NA, min = 0),
            numericInput(ns("copper"), "Cu", value = NA, min = 0),
            numericInput(ns("boron"), "B", value = NA, min = 0, step = 0.1),
            # Texture
            radioButtons(ns("texture_input_type"), "Texture", choices = c("pct", "class"), selected = "class"),
            numericInput(ns("sand"), "Sand", value = 33, min = 0, max = 100),
            numericInput(ns("silt"), "Silt", value = 33, min = 0, max = 100),
            numericInput(ns("clay"), "Clay", value = 34, min = 0, max = 100),
            selectInput(ns("texture_class"), "Texture Class", choices = NULL),
            # Location
            textInput(ns("street"), "Street", ""),
            textInput(ns("zipcode"), "Zip", ""),
            textInput(ns("city"), "City", ""),
            selectInput(ns("state"), "State", choices = state.name, selected = "New York"),
            numericInput(ns("latitude"), "Lat", value = NA, min = -90, max = 90, step = 0.0001),
            numericInput(ns("longitude"), "Lon", value = NA, min = -180, max = 180, step = 0.0001),
            # Additional
            dateInput(ns("date"), "Date", value = Sys.Date()),
            textAreaInput(ns("notes"), "Notes", "", height = "80px")
          )
      ),

      # --- My Data Section (hidden by default, shown via link or after submit) ---
      shinyjs::hidden(
        div(id = ns("my_data_section"),
            style = "max-width: 1400px; margin: 0 auto;",
          div(class = "card",
            div(class = "card-header d-flex justify-content-between align-items-center",
                span(icon("database"), " My Data"),
                actionLink(ns("hide_my_data"), span(icon("times"), " Close"),
                           class = "text-muted small")
            ),
            div(class = "card-body p-0",
              # Stats and filters row
              div(class = "p-3 bg-light border-bottom",
                  layout_column_wrap(
                    width = 1/4,
                    fill = FALSE,
                    # Stats
                    uiOutput(ns("my_data_stats")),
                    # Species filter
                    textInput(ns("filter_species"), NULL,
                              placeholder = "Search species..."),
                    # Outcome filter
                    selectInput(ns("filter_outcome"), NULL,
                                choices = c("All Outcomes" = "",
                                            "Thriving", "Established",
                                            "Struggling", "Failed/Died")),
                    # Actions
                    div(class = "d-flex gap-2 align-items-end",
                        actionButton(ns("clear_my_filters"), "Clear",
                                     class = "btn-outline-secondary btn-sm"),
                        downloadButton(ns("export_my_data"), "Export CSV",
                                       class = "btn-success btn-sm")
                    )
                  )
              ),
              # Bulk action bar (shown when rows selected)
              uiOutput(ns("bulk_action_bar")),
              # Full data table
              DTOutput(ns("all_entries_table"))
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

dataEntryServer <- function(id, pool, species_db, zipcode_db, soil_texture_classes,
                            current_user, is_admin, data_changed, lookup_ecoregion,
                            pdf_extract_limit, beta_features = list(), user_prefs = NULL,
                            species_search_index = NULL, common_name_db = NULL,
                            experience_level = NULL, show_my_data_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive trigger to refresh extraction status after each extraction
    extraction_trigger <- reactiveVal(0)

    # State for soil data reuse
    user_soil_profiles <- reactiveVal(NULL)  # Cached soil profiles for picker

    # State for batch plant upload
    plant_list_data <- reactiveVal(NULL)  # Parsed plant list from CSV upload

    # Pending soil data for step 2 (from PDF extraction or soil reuse).
    # Stores normalized values keyed by hidden input names so the step 2 renderUI
    # can read them synchronously, avoiding the Shiny flush timing issue where
    # updateNumericInput messages haven't round-tripped before renderUI fires.
    pending_soil_data <- reactiveVal(NULL)

    # Wizard state
    wizard_step <- reactiveVal(1L)

    # Get current experience level
    get_exp_level <- reactive({
      if (is.reactive(experience_level)) experience_level() else "casual"
    })

    is_casual <- reactive({ get_exp_level() == "casual" })

    # --- Wizard Step Indicator ---
    output$wizard_steps_ui <- renderUI({
      step <- wizard_step()
      step_data <- list(
        list(num = 1, label = "Soil Source"),
        list(num = 2, label = "Review Data"),
        list(num = 3, label = "Add Plants")
      )

      div(class = "wizard-steps mb-3",
          div(class = "d-flex justify-content-between align-items-center position-relative",
              style = "padding: 0 10px;",
              # Connecting line
              div(style = "position: absolute; top: 14px; left: 30px; right: 30px; height: 2px; background: #e5e7eb; z-index: 0;"),
              lapply(step_data, function(s) {
                state <- if (s$num < step) "completed" else if (s$num == step) "active" else "pending"
                bg <- switch(state,
                  completed = "background: #7A9A86; color: white;",
                  active = "background: #D39B35; color: white;",
                  "background: #e5e7eb; color: #8B9A8E;"
                )
                div(class = "text-center", style = "z-index: 1;",
                    div(style = paste0("width: 28px; height: 28px; border-radius: 50%; ",
                                       "display: flex; align-items: center; justify-content: center; ",
                                       "font-family: 'Montserrat', sans-serif; font-size: 0.75rem; ",
                                       "font-weight: 600; margin: 0 auto; ", bg),
                        if (state == "completed") icon("check", style = "font-size: 0.7rem;") else s$num),
                    tags$small(class = "d-block mt-1",
                               style = paste0("font-family: 'Montserrat', sans-serif; font-size: 0.65rem; ",
                                              "color: ", if (state == "active") "#D39B35" else "#8B9A8E", ";"),
                               s$label)
                )
              })
          )
      )
    })

    # --- Wizard Content ---
    output$wizard_content <- renderUI({
      step <- wizard_step()
      casual <- is_casual()

      switch(as.character(step),
        # Step 1: Soil Source
        "1" = tagList(
          # Lab test confirmation gate
          div(class = "mb-3 p-3 border rounded lab-confirm-gate",
              style = "background-color: #F7F4E8; border-color: #D39B35 !important;",
              checkboxInput(ns("lab_test_confirm"),
                            tags$span(style = "font-weight: 600; font-family: 'Montserrat', sans-serif;",
                                      "I confirm this data comes from a certified soil testing laboratory"),
                            value = FALSE),
              tags$small(class = "text-muted d-block mt-1",
                         "Most state ",
                         tags$a(href = "https://www.nifa.usda.gov/about-nifa/what-we-do/extension/cooperative-extension-system",
                                target = "_blank", "cooperative extension"),
                         " offices offer free or low-cost soil testing. Home test kits lack the accuracy needed for meaningful comparisons. ",
                         actionLink(ns("link_to_field_guide"), "See our soil testing guide",
                                    style = "color: #7A9A86; font-weight: 500;"),
                         ".")
          ),

          # Entry options (greyed out until lab test confirmed)
          div(id = ns("entry_options_wrapper"),
              style = "opacity: 0.5; pointer-events: none;",

              h6(class = "fw-bold mb-3", style = "font-family: 'Montserrat', sans-serif; color: #373D3C;",
                 "How would you like to enter soil data?"),

              # Option 1: Upload soil report
              uiOutput(ns("pdf_upload_section")),

              # Option 2: Reuse previous soil data
              uiOutput(ns("reuse_soil_section")),

              # Option 3: Enter manually
              actionButton(ns("enter_manually"), "Enter Manually",
                           class = "btn-outline-primary w-100 mb-2",
                           icon = icon("keyboard")),

              tags$small(class = "text-muted d-block text-center",
                         "You can also skip ahead if you already have data ready.")
          )
        ),

        # Step 2: Review Soil Data
        "2" = {
          # Helper: read from pending_soil_data (PDF/reuse) first, fall back to hidden inputs.
          # This avoids the Shiny flush timing issue where updateNumericInput messages
          # haven't round-tripped to the client before this renderUI fires.
          psd <- isolate(pending_soil_data())
          sv <- function(name) {
            val <- psd[[name]]
            if (!is.null(val) && !is.na(val)) return(val)
            isolate(input[[name]])
          }

          tagList(
          h6(class = "fw-bold mb-2", style = "font-family: 'Montserrat', sans-serif; color: #373D3C;",
             "Review Soil Data"),
          tags$small(class = "text-muted d-block mb-3",
                     "Verify and edit the values below."),

          # Soil Properties (always shown)
          div(class = "mb-3",
              tags$label(class = "form-label", `for` = ns("ph_step2"), "Soil pH"),
              numericInput(ns("ph_step2"), NULL, value = sv("ph"), min = 0, max = 14, step = 0.1),
              tags$label(class = "form-label", `for` = ns("om_step2"), "Organic Matter (%)"),
              numericInput(ns("om_step2"), NULL, value = sv("organic_matter"), min = 0, max = 100, step = 0.1),
              selectInput(ns("om_class_step2"), "Organic Matter (Qualitative)",
                          choices = c("Select if no % available" = "",
                                      "Very Low", "Low", "Medium Low", "Medium",
                                      "Medium High", "High", "Very High"),
                          selected = sv("organic_matter_class"))
          ),

          # Macronutrients (N/P/K always shown)
          div(class = "mb-3",
              tags$label(class = "form-label fw-bold", icon("leaf"), " Key Nutrients (ppm)"),
              layout_column_wrap(
                width = 1/3,
                numericInput(ns("nitrate_step2"), "Nitrate (N)", value = sv("nitrate"), min = 0),
                numericInput(ns("phosphorus_step2"), "Phosphorus (P)", value = sv("phosphorus"), min = 0),
                numericInput(ns("potassium_step2"), "Potassium (K)", value = sv("potassium"), min = 0)
              )
          ),

          # Advanced fields (hidden in casual mode, expandable)
          if (casual) {
            div(
              id = ns("advanced_fields_wrapper"),
              tags$details(
                tags$summary(
                  style = "cursor: pointer; font-weight: 500; color: #7A9A86; font-size: 0.9rem;",
                  icon("sliders"), " Advanced Fields"
                ),
                div(class = "mt-2",
                    # CEC + Salts
                    layout_column_wrap(
                      width = 1/2,
                      numericInput(ns("cec_step2"), "CEC (meq/100g)", value = sv("cec"), min = 0, step = 0.1),
                      numericInput(ns("salts_step2"), "Soluble Salts (ppm)", value = sv("soluble_salts"), min = 0)
                    ),
                    # Additional macros
                    tags$label(class = "form-label fw-bold mt-2", "More Macronutrients (ppm)"),
                    layout_column_wrap(
                      width = 1/2,
                      numericInput(ns("ammonium_step2"), "Ammonium (N)", value = sv("ammonium"), min = 0),
                      numericInput(ns("calcium_step2"), "Calcium (Ca)", value = sv("calcium"), min = 0),
                      numericInput(ns("magnesium_step2"), "Magnesium (Mg)", value = sv("magnesium"), min = 0),
                      numericInput(ns("sulfur_step2"), "Sulfur (S)", value = sv("sulfur"), min = 0)
                    ),
                    # Micronutrients
                    tags$label(class = "form-label fw-bold mt-2", "Micronutrients (ppm)"),
                    layout_column_wrap(
                      width = 1/2,
                      numericInput(ns("iron_step2"), "Iron (Fe)", value = sv("iron"), min = 0),
                      numericInput(ns("manganese_step2"), "Manganese (Mn)", value = sv("manganese"), min = 0),
                      numericInput(ns("zinc_step2"), "Zinc (Zn)", value = sv("zinc"), min = 0),
                      numericInput(ns("copper_step2"), "Copper (Cu)", value = sv("copper"), min = 0),
                      numericInput(ns("boron_step2"), "Boron (B)", value = sv("boron"), min = 0, step = 0.1)
                    ),
                    # Texture percentages
                    tags$label(class = "form-label fw-bold mt-2", "Texture Percentages"),
                    layout_column_wrap(
                      width = 1/3,
                      numericInput(ns("sand_step2"), "Sand %", value = sv("sand"), min = 0, max = 100),
                      numericInput(ns("silt_step2"), "Silt %", value = sv("silt"), min = 0, max = 100),
                      numericInput(ns("clay_step2"), "Clay %", value = sv("clay"), min = 0, max = 100)
                    ),
                    uiOutput(ns("texture_validation"))
                )
              )
            )
          } else {
            # Enthusiast mode: show all fields inline
            tagList(
              # CEC + Salts
              layout_column_wrap(
                width = 1/2,
                numericInput(ns("cec_step2"), "CEC (meq/100g)", value = sv("cec"), min = 0, step = 0.1),
                numericInput(ns("salts_step2"), "Soluble Salts (ppm)", value = sv("soluble_salts"), min = 0)
              ),
              # Additional macros
              tags$label(class = "form-label fw-bold mt-2", icon("leaf"), " More Macronutrients (ppm)"),
              layout_column_wrap(
                width = 1/2,
                numericInput(ns("ammonium_step2"), "Ammonium (N)", value = sv("ammonium"), min = 0),
                numericInput(ns("calcium_step2"), "Calcium (Ca)", value = sv("calcium"), min = 0),
                numericInput(ns("magnesium_step2"), "Magnesium (Mg)", value = sv("magnesium"), min = 0),
                numericInput(ns("sulfur_step2"), "Sulfur (S)", value = sv("sulfur"), min = 0)
              ),
              # Micronutrients
              tags$label(class = "form-label fw-bold mt-2", icon("seedling"), " Micronutrients (ppm)"),
              layout_column_wrap(
                width = 1/2,
                numericInput(ns("iron_step2"), "Iron (Fe)", value = sv("iron"), min = 0),
                numericInput(ns("manganese_step2"), "Manganese (Mn)", value = sv("manganese"), min = 0),
                numericInput(ns("zinc_step2"), "Zinc (Zn)", value = sv("zinc"), min = 0),
                numericInput(ns("copper_step2"), "Copper (Cu)", value = sv("copper"), min = 0),
                numericInput(ns("boron_step2"), "Boron (B)", value = sv("boron"), min = 0, step = 0.1)
              ),
              # Texture percentages
              tags$label(class = "form-label fw-bold mt-2", icon("mountain"), " Texture Percentages"),
              layout_column_wrap(
                width = 1/3,
                numericInput(ns("sand_step2"), "Sand %", value = sv("sand"), min = 0, max = 100),
                numericInput(ns("silt_step2"), "Silt %", value = sv("silt"), min = 0, max = 100),
                numericInput(ns("clay_step2"), "Clay %", value = sv("clay"), min = 0, max = 100)
              ),
              uiOutput(ns("texture_validation"))
            )
          },

          # Texture class (always visible)
          div(class = "mb-3",
              tags$label(class = "form-label", icon("mountain"), " Texture Class"),
              selectInput(ns("texture_class_step2"), NULL,
                          choices = soil_texture_classes$Texture,
                          selected = sv("texture_class"))
          )
        )},

        # Step 3: Add Plants + Submit
        "3" = tagList(
          h6(class = "fw-bold mb-2", style = "font-family: 'Montserrat', sans-serif; color: #373D3C;",
             "Add Plants & Submit"),
          tags$small(class = "text-muted d-block mb-3",
                     "Select the plants growing in this soil, then submit."),

          # Batch Plant Upload (Beta feature)
          uiOutput(ns("plant_list_upload_section")),

          # Species selection
          uiOutput(ns("species_input_section")),

          # Per-species metadata
          uiOutput(ns("per_species_fields")),

          # Plant list preview
          uiOutput(ns("plant_list_preview_section")),

          # Location
          div(class = "mb-3 p-2 border rounded",
              tags$label(class = "form-label fw-bold", icon("map-marker-alt"), " Location"),
              uiOutput(ns("use_saved_location_section")),
              textInput(ns("street_step3"), "Street Address (optional)", value = isolate(input$street),
                        placeholder = "123 Main St"),
              textInput(ns("zipcode_step3"), "Zip Code", value = isolate(input$zipcode),
                        placeholder = "Enter 5-digit zip"),
              div(class = "small mb-2", uiOutput(ns("location_status"))),
              layout_column_wrap(
                width = 1/2,
                textInput(ns("city_step3"), "City/Town", value = isolate(input$city)),
                selectInput(ns("state_step3"), "State", choices = state.name,
                            selected = isolate(input$state))
              ),
              layout_column_wrap(
                width = 1/2,
                numericInput(ns("lat_step3"), "Latitude", value = isolate(input$latitude),
                             min = -90, max = 90, step = 0.0001),
                numericInput(ns("lon_step3"), "Longitude", value = isolate(input$longitude),
                             min = -180, max = 180, step = 0.0001)
              ),
              tags$small(class = "text-muted",
                         "Coordinates auto-fill from zip code, or enter manually.")
          ),

          # Additional info
          div(class = "mb-3",
              dateInput(ns("date_step3"), "Sample Date", value = isolate(input$date)),
              textAreaInput(ns("notes_step3"), "Notes", value = isolate(input$notes),
                            height = "80px", placeholder = "General notes about this soil sample...")
          ),

          hr(),
          actionButton(ns("submit"), "Submit Sample", class = "btn-primary btn-lg w-100",
                       icon = icon("paper-plane"))
        )
      )
    })

    # --- Wizard Navigation Buttons (show/hide static buttons) ---
    observe({
      step <- wizard_step()
      shinyjs::toggle("wizard_back_container", condition = step > 1)
      shinyjs::toggle("wizard_next_container", condition = step < 3)
    })

    # --- Wizard Navigation Handlers ---
    observeEvent(input$wizard_next, {
      current <- wizard_step()
      if (current < 3) {
        # Guard: require lab test confirmation before leaving step 1
        if (current == 1 && !isTRUE(input$lab_test_confirm)) {
          showNotification("Please confirm your data comes from a certified lab.", type = "warning")
          return()
        }
        # Sync step 2 fields to hidden inputs before advancing
        if (current == 2) sync_step2_to_hidden()
        wizard_step(current + 1L)
      }
    })

    observeEvent(input$wizard_back, {
      current <- wizard_step()
      if (current > 1) {
        # Sync step 3 fields to hidden inputs before going back
        if (current == 3) sync_step3_to_hidden()
        # Sync step 2 fields before going back to step 1
        if (current == 2) sync_step2_to_hidden()
        wizard_step(current - 1L)
      }
    })

    # --- Lab test confirmation toggle ---
    observeEvent(input$lab_test_confirm, {
      confirmed <- isTRUE(input$lab_test_confirm)
      if (confirmed) {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').style.opacity = '1'; document.getElementById('%s').style.pointerEvents = 'auto';",
          session$ns("entry_options_wrapper"), session$ns("entry_options_wrapper")
        ))
      } else {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').style.opacity = '0.5'; document.getElementById('%s').style.pointerEvents = 'none';",
          session$ns("entry_options_wrapper"), session$ns("entry_options_wrapper")
        ))
      }
    })

    # Field Guide link — handled in app.R via namespaced input

    # Enter manually -> go to step 2
    observeEvent(input$enter_manually, {
      if (!isTRUE(input$lab_test_confirm)) {
        showNotification("Please confirm your data comes from a certified lab.", type = "warning")
        return()
      }
      wizard_step(2L)
    })

    # --- Sync functions: copy wizard step inputs to hidden form inputs ---
    sync_step2_to_hidden <- function() {
      # Soil properties
      if (!is.null(input$ph_step2)) updateNumericInput(session, "ph", value = input$ph_step2)
      if (!is.null(input$om_step2)) updateNumericInput(session, "organic_matter", value = input$om_step2)
      if (!is.null(input$om_class_step2)) updateSelectInput(session, "organic_matter_class", selected = input$om_class_step2)
      if (!is.null(input$cec_step2)) updateNumericInput(session, "cec", value = input$cec_step2)
      if (!is.null(input$salts_step2)) updateNumericInput(session, "soluble_salts", value = input$salts_step2)
      # Macronutrients
      if (!is.null(input$nitrate_step2)) updateNumericInput(session, "nitrate", value = input$nitrate_step2)
      if (!is.null(input$ammonium_step2)) updateNumericInput(session, "ammonium", value = input$ammonium_step2)
      if (!is.null(input$phosphorus_step2)) updateNumericInput(session, "phosphorus", value = input$phosphorus_step2)
      if (!is.null(input$potassium_step2)) updateNumericInput(session, "potassium", value = input$potassium_step2)
      if (!is.null(input$calcium_step2)) updateNumericInput(session, "calcium", value = input$calcium_step2)
      if (!is.null(input$magnesium_step2)) updateNumericInput(session, "magnesium", value = input$magnesium_step2)
      if (!is.null(input$sulfur_step2)) updateNumericInput(session, "sulfur", value = input$sulfur_step2)
      # Micronutrients
      if (!is.null(input$iron_step2)) updateNumericInput(session, "iron", value = input$iron_step2)
      if (!is.null(input$manganese_step2)) updateNumericInput(session, "manganese", value = input$manganese_step2)
      if (!is.null(input$zinc_step2)) updateNumericInput(session, "zinc", value = input$zinc_step2)
      if (!is.null(input$copper_step2)) updateNumericInput(session, "copper", value = input$copper_step2)
      if (!is.null(input$boron_step2)) updateNumericInput(session, "boron", value = input$boron_step2)
      # Texture
      if (!is.null(input$sand_step2)) updateNumericInput(session, "sand", value = input$sand_step2)
      if (!is.null(input$silt_step2)) updateNumericInput(session, "silt", value = input$silt_step2)
      if (!is.null(input$clay_step2)) updateNumericInput(session, "clay", value = input$clay_step2)
      if (!is.null(input$texture_class_step2)) updateSelectInput(session, "texture_class", selected = input$texture_class_step2)
      # Set texture input type based on whether percentages differ from defaults
      has_pct <- !is.null(input$sand_step2) && !is.na(input$sand_step2) &&
                 !(input$sand_step2 == 33 && input$silt_step2 == 33 && input$clay_step2 == 34)
      updateRadioButtons(session, "texture_input_type", selected = if (has_pct) "pct" else "class")
      # Clear pending data now that step 2 values are synced to hidden inputs
      pending_soil_data(NULL)
    }

    sync_step3_to_hidden <- function() {
      # Location
      if (!is.null(input$street_step3)) updateTextInput(session, "street", value = input$street_step3)
      if (!is.null(input$zipcode_step3)) updateTextInput(session, "zipcode", value = input$zipcode_step3)
      if (!is.null(input$city_step3)) updateTextInput(session, "city", value = input$city_step3)
      if (!is.null(input$state_step3)) updateSelectInput(session, "state", selected = input$state_step3)
      if (!is.null(input$lat_step3)) updateNumericInput(session, "latitude", value = input$lat_step3)
      if (!is.null(input$lon_step3)) updateNumericInput(session, "longitude", value = input$lon_step3)
      # Additional
      if (!is.null(input$date_step3)) updateDateInput(session, "date", value = input$date_step3)
      if (!is.null(input$notes_step3)) updateTextAreaInput(session, "notes", value = input$notes_step3)
    }

    # (Texture class step 2 choices are set directly in the renderUI)

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
      # Guard: require lab test confirmation
      if (!isTRUE(input$lab_test_confirm)) {
        showNotification("Please confirm your data comes from a certified lab.", type = "warning")
        return()
      }

      selected_id <- as.integer(input$reuse_profile_choice)
      soil_data <- db_get_soil_data_by_id(selected_id)

      if (is.null(soil_data)) {
        showNotification("Could not load soil data.", type = "error")
        return()
      }

      # Store normalized values for step 2 renderUI (avoids flush timing issue)
      pending_soil_data(list(
        ph = soil_data$ph,
        organic_matter = soil_data$organic_matter,
        organic_matter_class = soil_data$organic_matter_class,
        cec = soil_data$cec_meq,
        soluble_salts = soil_data$soluble_salts_ppm,
        nitrate = soil_data$nitrate_ppm,
        ammonium = soil_data$ammonium_ppm,
        phosphorus = soil_data$phosphorus_ppm,
        potassium = soil_data$potassium_ppm,
        calcium = soil_data$calcium_ppm,
        magnesium = soil_data$magnesium_ppm,
        sulfur = soil_data$sulfur_ppm,
        iron = soil_data$iron_ppm,
        manganese = soil_data$manganese_ppm,
        zinc = soil_data$zinc_ppm,
        copper = soil_data$copper_ppm,
        boron = soil_data$boron_ppm,
        sand = soil_data$texture_sand,
        silt = soil_data$texture_silt,
        clay = soil_data$texture_clay,
        texture_class = soil_data$texture_class
      ))

      # Fill form fields (hidden inputs, for submit handler)
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
      wizard_step(2L)  # Auto-advance to review step
      showNotification("Soil data applied! Review values below, then continue to add plants.",
                       type = "message", duration = 4)
    })

    # --- Auto-fill location from saved preferences ---
    # When user has saved prefs and zipcode field is empty, auto-fill
    observe({
      prefs <- if (is.reactive(user_prefs)) user_prefs() else user_prefs
      if (is.null(prefs) || is.null(prefs$home_zipcode) || !nzchar(prefs$home_zipcode)) return()

      # Only auto-fill if zipcode is currently empty
      current_zip <- isolate(input$zipcode)
      if (!is.null(current_zip) && nzchar(current_zip)) return()

      # Fill the location fields
      updateTextInput(session, "zipcode", value = prefs$home_zipcode)
    })

    # --- Use Saved Location Section ---
    output$use_saved_location_section <- renderUI({
      # Get user preferences (reactive if provided, or NULL)
      prefs <- if (is.reactive(user_prefs)) user_prefs() else user_prefs

      # Don't show if no preferences set
      if (is.null(prefs) || is.null(prefs$home_zipcode) || !nzchar(prefs$home_zipcode)) {
        return(NULL)
      }

      div(
        class = "mb-3 p-2 bg-light rounded border",
        div(
          class = "d-flex justify-content-between align-items-center",
          span(
            icon("home", class = "text-success me-1"),
            tags$small(sprintf("%s, %s", prefs$home_city %||% "", prefs$home_state %||% ""))
          ),
          actionLink(ns("use_saved_location"), "Use this location",
                     class = "text-success small")
        )
      )
    })

    # Apply saved location to form
    observeEvent(input$use_saved_location, {
      prefs <- if (is.reactive(user_prefs)) user_prefs() else user_prefs

      if (is.null(prefs) || is.null(prefs$home_zipcode)) {
        showNotification("No saved location found.", type = "warning")
        return()
      }

      # Fill location fields (both hidden and step 3 visible)
      updateTextInput(session, "zipcode", value = prefs$home_zipcode)
      updateTextInput(session, "zipcode_step3", value = prefs$home_zipcode)
      updateTextInput(session, "city", value = prefs$home_city %||% "")
      updateTextInput(session, "city_step3", value = prefs$home_city %||% "")

      # Convert state abbreviation to full name for the select input
      state_abbrevs <- c(
        AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California",
        CO = "Colorado", CT = "Connecticut", DE = "Delaware", FL = "Florida", GA = "Georgia",
        HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa",
        KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
        MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi", MO = "Missouri",
        MT = "Montana", NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", NJ = "New Jersey",
        NM = "New Mexico", NY = "New York", NC = "North Carolina", ND = "North Dakota", OH = "Ohio",
        OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina",
        SD = "South Dakota", TN = "Tennessee", TX = "Texas", UT = "Utah", VT = "Vermont",
        VA = "Virginia", WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming",
        DC = "District of Columbia"
      )
      state_full <- state_abbrevs[prefs$home_state] %||% prefs$home_state
      if (!is.null(state_full)) {
        updateSelectInput(session, "state", selected = state_full)
        updateSelectInput(session, "state_step3", selected = state_full)
      }

      # Fill coordinates (both hidden and step 3 visible)
      if (!is.null(prefs$home_lat) && !is.na(prefs$home_lat)) {
        updateNumericInput(session, "latitude", value = round(prefs$home_lat, 6))
        updateNumericInput(session, "lat_step3", value = round(prefs$home_lat, 6))
      }
      if (!is.null(prefs$home_long) && !is.na(prefs$home_long)) {
        updateNumericInput(session, "longitude", value = round(prefs$home_long, 6))
        updateNumericInput(session, "lon_step3", value = round(prefs$home_long, 6))
      }

      # Update location status
      output$location_status <- renderUI({
        div(class = "text-success",
            icon("check-circle"),
            sprintf(" %s, %s (from saved location)", prefs$home_city %||% "", prefs$home_state %||% ""))
      })

      showNotification(sprintf("Location set to %s, %s",
                               prefs$home_city %||% "", prefs$home_state %||% ""),
                       type = "message", duration = 3)
    })

    # --- Batch Plant Upload Section (Beta feature) ---
    output$plant_list_upload_section <- renderUI({
      # Only show if batch_plant_upload feature is enabled
      if (!isTRUE(beta_features$batch_plant_upload)) return(NULL)

      div(
        class = "mb-3 p-3 border rounded bg-light",
        div(class = "d-flex align-items-center mb-2",
            icon("seedling", class = "text-success me-2"),
            strong("Batch Plant Upload"),
            span(class = "badge bg-info ms-2", "Beta")),
        fileInput(ns("plant_list_upload"), NULL,
                  accept = c(".csv", ".xlsx", ".xls",
                             "text/csv", "application/vnd.ms-excel",
                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                  buttonLabel = "Choose File",
                  placeholder = "No file selected"),
        div(class = "d-flex justify-content-between align-items-center",
            downloadLink(ns("download_plant_template"), "Download template",
                         class = "small text-success"),
            # Clear button (shown when data is loaded)
            uiOutput(ns("clear_plant_list_btn"))
        ),
        helpText(class = "text-muted small",
                 "Upload a CSV with your plants to bulk add them. ",
                 "All plants will share the same soil data.")
      )
    })

    # Clear plant list button
    output$clear_plant_list_btn <- renderUI({
      if (is.null(plant_list_data())) return(NULL)
      actionLink(ns("clear_plant_list"), "Clear list",
                 class = "small text-danger")
    })

    # Clear plant list handler
    observeEvent(input$clear_plant_list, {
      plant_list_data(NULL)
      showNotification("Plant list cleared.", type = "message")
    })

    # Template download handler
    output$download_plant_template <- downloadHandler(
      filename = "plant_list_template.csv",
      content = function(file) {
        template <- data.frame(
          species = c("Acer rubrum", "Echinacea purpurea", "Baptisia australis"),
          cultivar = c("October Glory", "", "Purple Smoke"),
          outcome = c("Thriving", "Established", ""),
          sun_exposure = c("Full Sun", "Part Shade", "Full Sun"),
          site_hydrology = c("Mesic", "Dry", "Mesic"),
          inat_url = c("", "", ""),
          notes = c("Example entry - delete this row", "Another example", "")
        )
        write.csv(template, file, row.names = FALSE)
      }
    )

    # Plant list upload handler
    observeEvent(input$plant_list_upload, {
      req(input$plant_list_upload)

      result <- parse_plant_list(input$plant_list_upload$datapath, species_db, max_rows = 100)

      if (!is.null(result$error)) {
        showNotification(result$error, type = "error", duration = 8)
        plant_list_data(NULL)
        return()
      }

      if (nrow(result$valid) == 0) {
        showNotification("No valid species found in file.", type = "error")
        plant_list_data(NULL)
        return()
      }

      # Store the parsed data
      plant_list_data(result)

      # Show warnings for invalid species
      if (length(result$invalid) > 0) {
        invalid_display <- if (length(result$invalid) > 3) {
          paste(c(result$invalid[1:3], sprintf("... and %d more", length(result$invalid) - 3)), collapse = ", ")
        } else {
          paste(result$invalid, collapse = ", ")
        }
        showNotification(
          sprintf("%d species not found and will be skipped: %s", length(result$invalid), invalid_display),
          type = "warning",
          duration = 10
        )
      }

      showNotification(
        sprintf("Loaded %d plants from file.", nrow(result$valid)),
        type = "message"
      )
    })

    # --- Species input section (always visible, can add to batch upload) ---
    output$species_input_section <- renderUI({
      pld <- plant_list_data()
      has_batch <- !is.null(pld) && nrow(pld$valid) > 0

      tagList(
        selectizeInput(ns("species"),
                       if (has_batch) "Add More Species (Optional)" else "Plant Species",
                       choices = NULL, multiple = TRUE,
                       options = list(maxItems = 20, maxOptions = 100,
                                      placeholder = if (has_batch) "Add species to CSV list..." else "Type to search species...")),
        uiOutput(ns("species_count_indicator")),
        if (!has_batch) {
          helpText(class = "text-muted small",
                   "Tip: Select multiple species if they share the same soil sample.")
        }
      )
    })

    # --- Plant list preview section (shown when batch upload is active) ---
    output$plant_list_preview_section <- renderUI({
      pld <- plant_list_data()
      if (is.null(pld) || nrow(pld$valid) == 0) return(NULL)

      valid_df <- pld$valid
      n_csv <- nrow(valid_df)
      n_manual <- length(input$species)
      # Don't double-count species in both
      n_manual_extra <- length(setdiff(input$species, valid_df$species))

      div(
        class = "mb-3",
        div(class = "d-flex justify-content-between align-items-center mb-2",
            span(class = "fw-bold text-success",
                 icon("check-circle"),
                 sprintf(" %d plants from CSV", n_csv)),
            if (length(pld$invalid) > 0) {
              span(class = "text-warning small",
                   icon("exclamation-triangle"),
                   sprintf(" %d skipped", length(pld$invalid)))
            }
        ),
        # Compact preview table
        div(
          class = "border rounded p-2 bg-white",
          style = "max-height: 200px; overflow-y: auto;",
          tags$table(
            class = "table table-sm table-striped mb-0",
            tags$thead(
              tags$tr(
                tags$th("Species"),
                tags$th("Cultivar"),
                tags$th("Outcome")
              )
            ),
            tags$tbody(
              lapply(seq_len(min(nrow(valid_df), 10)), function(i) {
                tags$tr(
                  tags$td(class = "text-truncate", style = "max-width: 150px;",
                          valid_df$species[i]),
                  tags$td(if (is.na(valid_df$cultivar[i])) "-" else valid_df$cultivar[i]),
                  tags$td(if (is.na(valid_df$outcome[i])) "-" else valid_df$outcome[i])
                )
              }),
              if (nrow(valid_df) > 10) {
                tags$tr(
                  tags$td(colspan = 3, class = "text-muted text-center small",
                          sprintf("... and %d more", nrow(valid_df) - 10))
                )
              }
            )
          )
        )
      )
    })

    # --- Species dropdown population ---
    observe({
      step <- wizard_step()
      pld <- plant_list_data()
      has_batch <- !is.null(pld) && nrow(pld$valid) > 0

      # Only send update when step 3 is active (the selectizeInput exists)
      req(step == 3)

      # Use search index with common names if available, else fall back to plain species list
      choices <- if (!is.null(species_search_index) && length(species_search_index) > 0) {
        species_search_index
      } else {
        sort(species_db$taxon_name)
      }

      updateSelectizeInput(session, "species",
                           choices = choices,
                           selected = character(0),
                           server = TRUE,
                           options = list(
                             maxItems = 20,
                             maxOptions = 100,
                             placeholder = if (has_batch) "Type common or Latin name..." else "Type common or Latin name..."
                           ))
    })

    # --- Texture class dropdown ---
    observe({
      updateSelectInput(session, "texture_class", choices = soil_texture_classes$Texture)
    })

    # --- Species count indicator ---
    output$species_count_indicator <- renderUI({
      n_manual <- length(input$species)
      pld <- plant_list_data()
      n_csv <- if (!is.null(pld) && nrow(pld$valid) > 0) nrow(pld$valid) else 0

      if (n_manual == 0) return(NULL)

      # Show additional species count when batch upload is active
      if (n_csv > 0) {
        div(class = "alert alert-info py-2 mb-2",
            icon("plus-circle"),
            sprintf(" +%d additional species (%d total records)",
                    n_manual, n_csv + n_manual))
      } else {
        div(class = "alert alert-info py-2 mb-2",
            icon("seedling"),
            sprintf(" %d species selected - %d record%s will be created",
                    n_manual, n_manual, if (n_manual == 1) "" else "s"))
      }
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

        # Store normalized values for step 2 renderUI (avoids flush timing issue)
        pending_soil_data(list(
          ph = data$ph,
          organic_matter = data$organic_matter,
          organic_matter_class = data$organic_matter_class,
          cec = data$cec_meq,
          soluble_salts = data$soluble_salts_ppm,
          nitrate = data$nitrate_ppm,
          ammonium = data$ammonium_ppm,
          phosphorus = data$phosphorus_ppm,
          potassium = data$potassium_ppm,
          calcium = data$calcium_ppm,
          magnesium = data$magnesium_ppm,
          sulfur = data$sulfur_ppm,
          iron = data$iron_ppm,
          manganese = data$manganese_ppm,
          zinc = data$zinc_ppm,
          copper = data$copper_ppm,
          boron = data$boron_ppm,
          sand = data$texture_sand,
          silt = data$texture_silt,
          clay = data$texture_clay,
          texture_class = data$texture_class
        ))

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

        # Guard: require lab test confirmation before advancing
        if (!isTRUE(input$lab_test_confirm)) {
          showNotification("Please confirm your data comes from a certified lab before continuing.", type = "warning")
          return()
        }
        wizard_step(2L)  # Auto-advance to review step
        showNotification("Data extracted! Review values below, then continue to add plants.", type = "message", duration = 5)

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

    # --- Entry count (user's own entries) ---
    output$entry_count <- renderText({
      data_changed()
      u <- current_user()
      if (is.null(u)) return("")
      entries <- all_user_entries()
      n <- nrow(entries)
      if (n == 0) "" else paste0("(", n, ")")
    })

    # --- Toggle My Data section ---
    observeEvent(input$show_my_data, {
      shinyjs::toggle("my_data_section")
    })

    observeEvent(input$hide_my_data, {
      shinyjs::hide("my_data_section")
    })

    # External trigger from navbar "My Data" link
    if (!is.null(show_my_data_trigger)) {
      observeEvent(show_my_data_trigger(), {
        shinyjs::show("my_data_section")
      }, ignoreInit = TRUE)
    }

    # --- All My Data (full user data with filters) ---

    # Get all user entries
    all_user_entries <- reactive({
      data_changed()
      u <- current_user()
      if (is.null(u)) return(data.frame())

      tryCatch({
        query <- "
          SELECT id, species, cultivar, outcome, sun_exposure, site_hydrology,
                 ph, organic_matter, texture_class,
                 location_lat, location_long, date, created_at, notes, created_by
          FROM soil_samples
          WHERE created_by = $1
          ORDER BY created_at DESC
        "
        dbGetQuery(pool, query, params = list(u$user_uid))
      }, error = function(e) {
        message("Error fetching user entries: ", e$message)
        data.frame()
      })
    })

    # Filtered entries for "All My Data" tab
    filtered_my_entries <- reactive({
      entries <- all_user_entries()
      if (nrow(entries) == 0) return(entries)

      # Species filter
      if (!is.null(input$filter_species) && nzchar(input$filter_species)) {
        pattern <- tolower(input$filter_species)
        entries <- entries[grepl(pattern, tolower(entries$species)), ]
      }

      # Outcome filter
      if (!is.null(input$filter_outcome) && nzchar(input$filter_outcome)) {
        entries <- entries[!is.na(entries$outcome) &
                            entries$outcome == input$filter_outcome, ]
      }

      entries
    })

    # Stats summary
    output$my_data_stats <- renderUI({
      entries <- all_user_entries()
      if (nrow(entries) == 0) {
        return(div(class = "text-muted small", "No entries yet"))
      }

      n_entries <- nrow(entries)
      n_species <- length(unique(entries$species))

      # Success rate
      outcomes <- entries$outcome[!is.na(entries$outcome)]
      if (length(outcomes) > 0) {
        success <- sum(outcomes %in% c("Thriving", "Established"))
        success_pct <- round(100 * success / length(outcomes))
        success_text <- span(
          class = if (success_pct >= 50) "text-success" else "text-warning",
          paste0(success_pct, "% success")
        )
      } else {
        success_text <- NULL
      }

      div(class = "small",
          strong(n_entries), " entries", tags$br(),
          strong(n_species), " species",
          if (!is.null(success_text)) tagList(" · ", success_text)
      )
    })

    # All entries table
    output$all_entries_table <- renderDT({
      entries <- filtered_my_entries()
      u <- current_user()
      user_id <- if (!is.null(u)) u$user_uid else ""
      admin_user <- is_admin()

      if (nrow(entries) == 0) {
        return(datatable(data.frame(Message = "No entries found"),
                         options = list(dom = 't'),
                         rownames = FALSE))
      }

      # Helper to format outcome as badge
      format_outcome <- function(outcome) {
        if (is.na(outcome) || outcome == "") return("-")
        badge_class <- switch(outcome,
          "Thriving" = "outcome-thriving",
          "Established" = "outcome-established",
          "Struggling" = "outcome-struggling",
          "Failed/Died" = "outcome-failed",
          ""
        )
        sprintf('<span class="outcome-badge %s">%s</span>', badge_class, outcome)
      }

      # Italicize species names; casual users get a separate Common Name column
      is_casual <- !is.null(experience_level) && identical(experience_level(), "casual")
      # Build genus-species keyed lookup (common_name_db stores full author citations)
      cn_lookup <- NULL
      if (is_casual && !is.null(common_name_db) && nrow(common_name_db) > 0) {
        gs_keys <- vapply(strsplit(common_name_db$scientific_name, " "), function(x) {
          paste(head(x, 2), collapse = " ")
        }, character(1))
        cn_lookup <- stats::setNames(common_name_db$common_name, gs_keys)
        cn_lookup <- cn_lookup[!duplicated(names(cn_lookup))]
      }

      display <- entries %>%
        mutate(
          Date = format(date, "%Y-%m-%d"),
          Outcome = sapply(outcome, format_outcome),
          pH = ifelse(is.na(ph), "-", as.character(round(ph, 1))),
          OM = ifelse(is.na(organic_matter), "-", paste0(round(organic_matter, 1), "%")),
          Texture = ifelse(is.na(texture_class), "-", texture_class),
          Cultivar = ifelse(is.na(cultivar) | cultivar == "", "-",
                           sprintf("<strong>%s</strong>", htmltools::htmlEscape(cultivar))),
          species = sapply(species, function(sp) {
            if (is.na(sp) || sp == "") "-" else sprintf("<em>%s</em>", htmltools::htmlEscape(sp))
          })
        )

      # Add Common Name column for casual users
      if (!is.null(cn_lookup)) {
        display$common_name <- vapply(entries$species, function(sp) {
          if (is.na(sp) || sp == "") return("-")
          cn <- cn_lookup[sp]
          if (!is.na(cn) && nzchar(cn)) tools::toTitleCase(tolower(cn)) else "-"
        }, character(1))
      }

      # Add action buttons
      display$Actions <- sapply(seq_len(nrow(display)), function(i) {
        is_owner <- !is.na(display$created_by[i]) && display$created_by[i] == user_id
        if (is_owner || admin_user) {
          sprintf(
            "<button class=\"btn btn-sm btn-outline-primary me-1\" title=\"Edit entry %d\" aria-label=\"Edit entry %d\" onclick=\"Shiny.setInputValue('edit_entry', %d, {priority: 'event'})\"><i class=\"fa fa-edit\"></i></button><button class=\"btn btn-sm btn-outline-danger\" title=\"Delete entry %d\" aria-label=\"Delete entry %d\" onclick=\"Shiny.setInputValue('delete_entry', %d, {priority: 'event'})\"><i class=\"fa fa-trash\"></i></button>",
            display$id[i], display$id[i], display$id[i], display$id[i], display$id[i], display$id[i]
          )
        } else {
          ""
        }
      })

      if (!is.null(cn_lookup)) {
        display <- display %>%
          select(ID = id, `Common Name` = common_name, Species = species,
                 Cultivar, Outcome, pH, OM, Texture, Date, Actions)
        date_col_idx <- 8
      } else {
        display <- display %>%
          select(ID = id, Species = species, Cultivar, Outcome, pH, OM, Texture, Date, Actions)
        date_col_idx <- 7
      }

      datatable(
        display,
        selection = "multiple",
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          order = list(list(date_col_idx, 'desc')),
          columnDefs = list(
            list(visible = FALSE, targets = 0),  # Hide ID column
            list(className = "text-nowrap", targets = "_all")
          )
        ),
        rownames = FALSE,
        escape = FALSE,
        class = "table table-striped table-hover"
      )
    })

    # Bulk action bar
    output$bulk_action_bar <- renderUI({
      selected <- input$all_entries_table_rows_selected
      if (is.null(selected) || length(selected) == 0) return(NULL)

      div(class = "p-2 bg-warning-subtle border-bottom d-flex align-items-center gap-2",
          icon("check-square"),
          sprintf("%d selected", length(selected)),
          actionButton(ns("bulk_delete"), "Delete Selected",
                       class = "btn-outline-danger btn-sm ms-auto",
                       icon = icon("trash"))
      )
    })

    # Clear filters
    observeEvent(input$clear_my_filters, {
      updateTextInput(session, "filter_species", value = "")
      updateSelectInput(session, "filter_outcome", selected = "")
    })

    # Export CSV
    output$export_my_data <- downloadHandler(
      filename = function() {
        paste0("my_edaphic_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        entries <- all_user_entries()
        # Remove internal columns
        entries <- entries %>% select(-created_by)
        write.csv(entries, file, row.names = FALSE)
      }
    )

    # Bulk delete
    observeEvent(input$bulk_delete, {
      selected <- input$all_entries_table_rows_selected
      if (length(selected) == 0) return()

      entries <- filtered_my_entries()
      entry_ids <- entries$id[selected]

      showModal(modalDialog(
        title = span(icon("exclamation-triangle"), " Confirm Delete"),
        size = "s",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_bulk_delete"), "Delete", class = "btn-danger")
        ),
        p(sprintf("Are you sure you want to delete %d entr%s?",
                  length(entry_ids),
                  if (length(entry_ids) == 1) "y" else "ies")),
        p(class = "text-muted small", "This action cannot be undone.")
      ))
    })

    # Confirm bulk delete
    observeEvent(input$confirm_bulk_delete, {
      selected <- input$all_entries_table_rows_selected
      entries <- filtered_my_entries()
      entry_ids <- entries$id[selected]

      u <- current_user()
      if (is.null(u)) {
        showNotification("Please sign in.", type = "error")
        removeModal()
        return()
      }

      success_count <- 0
      for (eid in entry_ids) {
        result <- tryCatch({
          db_delete_sample(eid, u$user_uid, is_admin = is_admin())
        }, error = function(e) FALSE)

        if (isTRUE(result)) {
          success_count <- success_count + 1
          db_audit_log("delete", "soil_samples", eid, u$user_uid, "bulk delete")
        }
      }

      removeModal()

      if (success_count > 0) {
        showNotification(sprintf("Deleted %d entr%s.",
                                 success_count,
                                 if (success_count == 1) "y" else "ies"),
                         type = "message")
        data_changed(data_changed() + 1)
      } else {
        showNotification("Failed to delete entries.", type = "error")
      }
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
      has_street <- nzchar(trimws(street))
      street_failed <- FALSE
      final_lat <- NULL
      final_lon <- NULL
      is_approximate <- FALSE

      tryCatch({
        # Step 1: If street provided, try full address first
        if (has_street) {
          full_address <- paste0(street, ", ", zip_result$city, ", ", zip_result$state, " ", zip_clean)
          res <- geo(address = full_address, method = "osm")

          if (!is.null(res) && nrow(res) > 0 && !is.na(res$lat[1])) {
            final_lat <- res$lat[1]
            final_lon <- res$long[1]
          } else {
            street_failed <- TRUE
          }
        }

        # Step 2: If no street or street failed, use city/state/zip
        if (is.null(final_lat)) {
          city_address <- paste0(zip_result$city, ", ", zip_result$state, " ", zip_clean)
          res <- geo(address = city_address, method = "osm")

          if (!is.null(res) && nrow(res) > 0 && !is.na(res$lat[1])) {
            final_lat <- res$lat[1]
            final_lon <- res$long[1]
            is_approximate <- TRUE
          }
        }

        # Step 3: If still nothing, fall back to zip centroid
        if (is.null(final_lat) && !is.na(zip_result$latitude) && !is.na(zip_result$longitude)) {
          final_lat <- zip_result$latitude
          final_lon <- zip_result$longitude
          is_approximate <- TRUE
        }

        # Update coordinates (both hidden and step 3 visible)
        if (!is.null(final_lat)) {
          updateNumericInput(session, "latitude", value = round(final_lat, 6))
          updateNumericInput(session, "longitude", value = round(final_lon, 6))
          updateNumericInput(session, "lat_step3", value = round(final_lat, 6))
          updateNumericInput(session, "lon_step3", value = round(final_lon, 6))

          # Lookup ecoregion
          eco <- tryCatch(lookup_ecoregion(final_lat, final_lon),
                          error = function(e) list(name = NA, code = NA))

          # Build status message
          location_text <- sprintf("%s, %s", zip_result$city, zip_result$state)
          if (is_approximate) {
            location_text <- paste0(location_text, " (approximate)")
          }

          output$location_status <- renderUI({
            tagList(
              # Show street not found warning if applicable
              if (street_failed) {
                div(class = "text-warning mb-1",
                    icon("exclamation-triangle"),
                    " Street address not found, using zip code location")
              },
              div(class = "text-success",
                  icon("check-circle"), " ", location_text,
                  if (!is.null(eco$l4_name) && !is.na(eco$l4_name)) {
                    tags$small(class = "text-muted d-block",
                               icon("map"), " ", eco$l4_name)
                  } else if (!is.null(eco$name) && !is.na(eco$name)) {
                    tags$small(class = "text-muted d-block",
                               icon("map"), " ", eco$name)
                  }
              )
            )
          })
        } else {
          output$location_status <- renderUI({
            div(class = "text-warning",
                icon("exclamation-triangle"),
                sprintf(" %s, %s - coordinates unavailable", zip_result$city, zip_result$state))
          })
        }
      }, error = function(e) {
        message("Geocoding error: ", e$message)
        # Fall back to zipcode centroid on error
        if (!is.na(zip_result$latitude) && !is.na(zip_result$longitude)) {
          updateNumericInput(session, "latitude", value = round(zip_result$latitude, 6))
          updateNumericInput(session, "longitude", value = round(zip_result$longitude, 6))
          updateNumericInput(session, "lat_step3", value = round(zip_result$latitude, 6))
          updateNumericInput(session, "lon_step3", value = round(zip_result$longitude, 6))

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

      # Auto-fill city and state immediately (both hidden and step 3 visible)
      state_full <- state.name[match(zip_result$state, state.abb)]
      if (is.na(state_full)) state_full <- zip_result$state

      updateTextInput(session, "city", value = zip_result$city)
      updateSelectInput(session, "state", selected = state_full)
      updateTextInput(session, "city_step3", value = zip_result$city)
      updateSelectInput(session, "state_step3", selected = state_full)
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

    # --- Sync step 3 location inputs to hidden inputs (triggers existing geocoding) ---
    observeEvent(input$zipcode_step3, {
      updateTextInput(session, "zipcode", value = input$zipcode_step3)
    }, ignoreInit = TRUE)
    observeEvent(input$street_step3, {
      updateTextInput(session, "street", value = input$street_step3)
    }, ignoreInit = TRUE)
    observeEvent(input$city_step3, {
      updateTextInput(session, "city", value = input$city_step3)
    }, ignoreInit = TRUE)
    observeEvent(input$state_step3, {
      updateSelectInput(session, "state", selected = input$state_step3)
    }, ignoreInit = TRUE)
    observeEvent(input$lat_step3, {
      updateNumericInput(session, "latitude", value = input$lat_step3)
    }, ignoreInit = TRUE)
    observeEvent(input$lon_step3, {
      updateNumericInput(session, "longitude", value = input$lon_step3)
    }, ignoreInit = TRUE)

    # --- Submit sample ---

    # Helper function to perform the actual submission
    perform_submit <- function(reuse_soil_data = NULL) {
      u <- current_user()
      if (is.null(u)) {
        showNotification("Please sign in to submit data.", type = "error")
        return()
      }

      # Check for batch upload and/or manual species
      pld <- plant_list_data()
      has_batch <- !is.null(pld) && nrow(pld$valid) > 0
      manual_species <- input$species
      has_manual <- !is.null(manual_species) && length(manual_species) > 0

      # Build combined species list and batch data
      if (has_batch) {
        batch_data <- pld$valid  # Data frame with all plant metadata
        batch_species <- pld$valid$species
      } else {
        batch_data <- NULL
        batch_species <- character(0)
      }

      # Manual species (not in batch) will use input fields for metadata
      manual_only <- if (has_manual) setdiff(manual_species, batch_species) else character(0)

      # Combined species list: batch first, then manual additions
      species_list <- c(batch_species, manual_only)

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
      n_batch <- length(batch_species)

      for (i in seq_along(species_list)) {
        sp <- species_list[i]

        # Get per-species metadata from batch upload or dynamic inputs
        if (i <= n_batch) {
          # From CSV upload (first n_batch species)
          cultivar_val <- batch_data$cultivar[i]
          outcome_val <- batch_data$outcome[i]
          sun_val <- batch_data$sun_exposure[i]
          hydro_val <- batch_data$site_hydrology[i]
          inat_val <- batch_data$inat_url[i]
          notes_val <- batch_data$notes[i]
          # Combine CSV notes with form notes if both exist
          if (!is.na(notes_val) && nzchar(input$notes)) {
            notes_val <- paste(input$notes, notes_val, sep = " | ")
          } else if (is.na(notes_val)) {
            notes_val <- input$notes
          }
        } else {
          # From dynamic inputs (manual additions after batch)
          cultivar_val <- get_sp_input("cultivar", sp)
          outcome_val <- get_sp_input("outcome", sp)
          sun_val <- get_sp_input("sun", sp)
          hydro_val <- get_sp_input("hydrology", sp)
          inat_val <- get_sp_input("inat", sp)
          notes_val <- input$notes
        }

        new_data <- data.frame(
          species = sp,
          cultivar = cultivar_val,
          outcome = outcome_val,
          sun_exposure = sun_val,
          site_hydrology = hydro_val,
          inat_url = inat_val,
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
          notes = notes_val,
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

        # Show My Data section so user can see their new entry
        shinyjs::show("my_data_section")

        # Reset form - clear both batch and manual selections
        if (has_batch) {
          plant_list_data(NULL)  # Clear batch upload data
        }
        if (has_manual) {
          updateSelectizeInput(session, "species", selected = character(0))
        }

        # Reset wizard to step 1
        wizard_step(1L)
      } else {
        showNotification("Error adding samples. Please try again.", type = "error")
      }
    }

    observeEvent(input$submit, {
      # Sync step 2 and step 3 fields to hidden inputs before validation
      sync_step2_to_hidden()
      sync_step3_to_hidden()

      u <- current_user()
      if (is.null(u)) {
        showNotification("Please sign in to submit data.", type = "error")
        return()
      }

      # Check for batch upload and/or manual species selection
      pld <- plant_list_data()
      has_batch <- !is.null(pld) && nrow(pld$valid) > 0
      manual_species <- input$species
      has_manual <- !is.null(manual_species) && length(manual_species) > 0

      # Validate manual species if any
      if (has_manual) {
        invalid_species <- manual_species[!manual_species %in% species_db$taxon_name]
        if (length(invalid_species) > 0) {
          showNotification(
            paste("Invalid species:", paste(invalid_species[1:min(3, length(invalid_species))], collapse = ", "),
                  if (length(invalid_species) > 3) "..." else ""),
            type = "error"
          )
          return()
        }
      }

      # Must have at least one species from either source
      if (!has_batch && !has_manual) {
        showNotification("Please select at least one species or upload a plant list.", type = "error")
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
