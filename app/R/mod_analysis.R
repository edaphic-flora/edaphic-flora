# R/mod_analysis.R - Analysis tab Shiny module
# Species data visualization with multiple analysis tabs, filters, and exports

# ---------------------------
# UI
# ---------------------------

analysisUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Analysis",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Select Species",
        width = 300,
        selectizeInput(ns("analysis_species"), "Species", choices = NULL, width = "100%",
                       options = list(
                         create = TRUE, persist = FALSE, maxOptions = 50,
                         openOnFocus = FALSE, closeAfterSelect = TRUE, selectOnTab = TRUE,
                         placeholder = 'Type a species name...'
                       )),
        hr(),
        uiOutput(ns("species_summary")),
        uiOutput(ns("reference_badges")),
        uiOutput(ns("native_status_badge")),
        hr(),

        # --- Filter Controls ---
        tags$details(
          open = NA,  # Start collapsed
          tags$summary(
            style = "cursor: pointer; font-weight: 500; color: #7A9A86;",
            icon("filter"), " Filter Data"
          ),
          div(
            class = "mt-2",
            selectizeInput(ns("filter_outcome"), "Outcome",
                           choices = c("All" = "", "Thriving" = "Thriving",
                                       "Established" = "Established",
                                       "Struggling" = "Struggling",
                                       "Failed/Died" = "Failed/Died"),
                           selected = "",
                           options = list(placeholder = "All", plugins = list("clear_button"))),
            selectizeInput(ns("filter_sun"), "Sun Exposure",
                           choices = c("All" = "", "Full Sun" = "Full Sun",
                                       "Part Sun" = "Part Sun",
                                       "Part Shade" = "Part Shade",
                                       "Full Shade" = "Full Shade"),
                           selected = "",
                           options = list(placeholder = "All", plugins = list("clear_button"))),
            selectizeInput(ns("filter_hydrology"), "Site Hydrology",
                           choices = c("All" = "", "Dry/Xeric" = "Dry",
                                       "Mesic" = "Mesic",
                                       "Wet/Hydric" = "Wet"),
                           selected = "",
                           options = list(placeholder = "All", plugins = list("clear_button"))),
            uiOutput(ns("filter_cultivar_ui")),
            div(class = "d-flex justify-content-between align-items-center mt-2",
                uiOutput(ns("filter_status")),
                actionLink(ns("clear_all_filters"), "Clear All", icon = icon("times"),
                           class = "text-muted small")
            )
          )
        ),
        uiOutput(ns("reference_msg")),
        # Download button for current species data
        uiOutput(ns("download_species_ui"))
      ),

      navset_card_tab(
        id = ns("analysis_tabs"),
        full_screen = TRUE,

        nav_panel(
          title = "Summary",
          icon = icon("table"),
          uiOutput(ns("summary_ui"))
        ),
        nav_panel(
          title = "pH",
          icon = icon("chart-bar"),
          uiOutput(ns("ph_plot_ui"))
        ),
        nav_panel(
          title = "pH vs OM",
          icon = icon("circle-nodes"),
          uiOutput(ns("ph_om_plot_ui"))
        ),
        nav_panel(
          title = "Nutrients",
          icon = icon("leaf"),
          uiOutput(ns("nutrient_plot_ui"))
        ),
        nav_panel(
          title = "Correlations",
          icon = icon("project-diagram"),
          uiOutput(ns("heatmap_ui"))
        ),
        nav_panel(
          title = "Soil Texture",
          icon = icon("mountain"),
          uiOutput(ns("texture_plot_ui"))
        ),
        nav_panel(
          title = "Geography",
          icon = icon("globe-americas"),
          uiOutput(ns("geography_ui"))
        ),
        nav_panel(
          title = "Performance",
          icon = icon("seedling"),
          uiOutput(ns("performance_ui"))
        ),
        nav_panel(
          title = "Similar Species",
          icon = icon("shuffle"),
          withSpinner(uiOutput(ns("similar_species_ui")), type = 6, color = "#7A9A86")
        ),
        nav_panel(
          title = "Raw Data",
          icon = icon("database"),
          card_body(DTOutput(ns("raw_data")))
        ),
        nav_panel(
          title = "USDA Traits",
          icon = icon("book"),
          uiOutput(ns("traits_ui"))
        )
      )
    )
  )
}

# ---------------------------
# Server
# ---------------------------

analysisServer <- function(id, pool, data_changed, state_grid, is_prod,
                           edaphic_colors, theme_edaphic, scale_color_edaphic, scale_fill_edaphic) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper for empty states
    empty_state <- function(icon_name, title, message) {
      div(class = "empty-state",
          tags$i(class = paste("fa", paste0("fa-", icon_name))),
          h5(title),
          p(class = "text-muted", message))
    }

    # --- Species dropdown population ---
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

      chip <- function(lbl, val, href = NULL) {
        if (is.null(val)) return(NULL)
        if (length(val) == 0) return(NULL)
        val <- val[1]
        if (is.na(val) || !nzchar(as.character(val))) return(NULL)
        badge <- tags$span(class = "badge bg-light text-dark me-1 mb-1",
                           style = "font-size: 0.75rem;",
                           sprintf("%s: %s", lbl, val))
        if (!is.null(href) && nzchar(href)) {
          tags$a(href = href, target = "_blank",
                 style = "text-decoration: none;",
                 title = paste("View on", lbl, "website"),
                 tags$span(class = "badge bg-light text-dark me-1 mb-1",
                           style = "font-size: 0.75rem; cursor: pointer; border: 1px solid #27ae60;",
                           sprintf("%s: %s ", lbl, val),
                           icon("external-link-alt", class = "fa-xs", style = "color: #27ae60;")))
        } else {
          badge
        }
      }

      safe_val <- function(x) {
        if (is.null(x) || length(x) == 0) return(NA)
        x[1]
      }

      usda_url <- function(symbol) {
        if (is.na(symbol) || !nzchar(symbol)) return(NULL)
        sprintf("https://plants.usda.gov/plant-profile/%s", symbol)
      }

      if (!ref$has_traits) return(NULL)

      tr <- ref$traits[1, , drop = FALSE]
      symbol <- safe_val(tr$usda_symbol)
      ph_min <- safe_val(tr$soil_ph_min)
      ph_max <- safe_val(tr$soil_ph_max)

      div(
        class = "reference-section mt-2 mb-2 p-2 rounded",
        style = "background-color: #f8f9fa; border: 1px solid #e9ecef;",
        div(class = "d-flex justify-content-between align-items-center mb-2",
            tags$span(class = "small fw-bold text-muted", icon("book-open"), " USDA Reference"),
            tags$label(class = "d-flex align-items-center mb-0", style = "cursor: pointer;",
              tags$input(type = "checkbox", id = ns("show_usda_ref"), checked = "checked",
                         class = "form-check-input me-1", style = "margin-top: 0;"),
              tags$span(class = "small text-muted", "Overlay")
            )
        ),
        div(class = "d-flex flex-wrap gap-1",
            chip("USDA", symbol, href = usda_url(symbol)),
            chip("pH", if (!is.na(ph_min) && !is.na(ph_max)) sprintf("%.1f-%.1f", ph_min, ph_max) else NA),
            chip("Shade", safe_val(tr$shade_tolerance)),
            chip("Drought", safe_val(tr$drought_tolerance))
        )
      )
    })

    # --- Native status badge (dev only) ---
    output$native_status_badge <- renderUI({
      if (is_prod || is.null(state_grid)) return(NULL)

      sp <- input$analysis_species %||% ""
      if (!nzchar(sp)) return(NULL)

      dat <- db_get_species_data(sp)
      if (nrow(dat) == 0) return(NULL)

      valid_coords <- dat %>%
        filter(!is.na(location_lat) & !is.na(location_long))

      if (nrow(valid_coords) == 0) return(NULL)

      user_states <- get_states_from_coords(
        valid_coords$location_lat,
        valid_coords$location_long,
        state_grid
      )

      if (length(user_states) == 0) return(NULL)

      status <- get_native_status_summary(sp, user_states, pool)

      badge_class <- switch(status$summary,
        "native" = "bg-success",
        "introduced" = "bg-warning text-dark",
        "mixed" = "bg-info text-dark",
        "bg-secondary"
      )

      badge_icon <- switch(status$summary,
        "native" = icon("check-circle"),
        "introduced" = icon("exclamation-triangle"),
        "mixed" = icon("info-circle"),
        icon("question-circle")
      )

      badge_text <- switch(status$summary,
        "native" = "Native to your area",
        "introduced" = "Introduced in your area",
        "mixed" = "Mixed native status",
        "Native status unknown"
      )

      detail_text <- NULL
      if (status$summary == "mixed" && (length(status$native_states) > 0 || length(status$introduced_states) > 0)) {
        parts <- c()
        if (length(status$native_states) > 0) {
          parts <- c(parts, paste0("Native: ", paste(status$native_states, collapse = ", ")))
        }
        if (length(status$introduced_states) > 0) {
          parts <- c(parts, paste0("Introduced: ", paste(status$introduced_states, collapse = ", ")))
        }
        detail_text <- paste(parts, collapse = " | ")
      }

      div(
        class = "native-status-section mt-2 p-2 rounded",
        style = switch(status$summary,
          "native" = "background-color: #f0fdf4; border: 1px solid #86efac;",
          "introduced" = "background-color: #fefce8; border: 1px solid #fde047;",
          "mixed" = "background-color: #ecfeff; border: 1px solid #67e8f9;",
          "background-color: #f3f4f6; border: 1px solid #d1d5db;"
        ),
        div(
          class = "d-flex align-items-center gap-2",
          tags$span(
            class = paste("badge", badge_class),
            badge_icon, " ", badge_text
          )
        ),
        if (!is.null(detail_text)) {
          div(class = "small text-muted mt-1", detail_text)
        }
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

    # --- Species summary sidebar ---
    output$species_summary <- renderUI({
      req(input$analysis_species, input$analysis_species != "")
      dat <- db_get_species_data(input$analysis_species)
      if (nrow(dat) == 0) return(NULL)

      n_samples <- nrow(dat)

      success_rate <- NA
      if ("outcome" %in% names(dat) && sum(!is.na(dat$outcome)) > 0) {
        outcomes <- dat$outcome[!is.na(dat$outcome)]
        n_success <- sum(outcomes %in% c("Thriving", "Established"))
        success_rate <- round(n_success / length(outcomes) * 100)
      }

      n_ecoregions <- length(unique(dat$ecoregion_l4[!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4)]))

      ph_range <- if (sum(!is.na(dat$ph)) > 0) {
        sprintf("%.1f - %.1f", min(dat$ph, na.rm = TRUE), max(dat$ph, na.rm = TRUE))
      } else "-"

      div(class = "small",
        div(class = "d-flex justify-content-between align-items-center mb-2 pb-2 border-bottom",
          span(class = "fw-bold", style = "color: #7A9A86;", paste(n_samples, "samples")),
          if (!is.na(success_rate)) {
            span(class = if (success_rate >= 70) "text-success" else if (success_rate >= 50) "text-warning" else "text-danger",
                 title = "Success rate: % of samples with Thriving or Established outcomes",
                 style = "cursor: help; border-bottom: 1px dotted currentColor;",
                 paste0(success_rate, "% success"))
          }
        ),
        div(class = "text-muted",
          div(class = "d-flex justify-content-between", span("pH range"), span(ph_range)),
          div(class = "d-flex justify-content-between", span("Ecoregions"), span(n_ecoregions)),
          if (sum(!is.na(dat$organic_matter)) > 0) {
            div(class = "d-flex justify-content-between",
                span("Avg OM"),
                span(paste0(round(mean(dat$organic_matter, na.rm = TRUE), 1), "%")))
          }
        )
      )
    })

    # ---------------------------
    # Filtered Analysis Data
    # ---------------------------

    # Cultivar filter UI
    output$filter_cultivar_ui <- renderUI({
      sp <- input$analysis_species %||% ""
      if (!nzchar(sp)) return(NULL)

      dat <- db_get_species_data(sp)
      cultivars <- unique(dat$cultivar[!is.na(dat$cultivar) & nzchar(dat$cultivar)])

      if (length(cultivars) == 0) return(NULL)

      cultivars <- sort(cultivars)
      choices <- c("All" = "", setNames(cultivars, cultivars))

      selectizeInput(ns("filter_cultivar"), "Cultivar",
                     choices = choices,
                     selected = "",
                     options = list(placeholder = "All", plugins = list("clear_button")))
    })

    # Clear all filters button
    observeEvent(input$clear_all_filters, {
      updateSelectizeInput(session, "filter_outcome", selected = "")
      updateSelectizeInput(session, "filter_sun", selected = "")
      updateSelectizeInput(session, "filter_hydrology", selected = "")
      updateSelectizeInput(session, "filter_cultivar", selected = "")
    })

    # Reactive for filtered species data
    filtered_species_data <- reactive({
      sp <- input$analysis_species %||% ""
      if (!nzchar(sp)) return(data.frame())

      dat <- db_get_species_data(sp)
      if (nrow(dat) == 0) return(dat)

      if (nzchar(input$filter_outcome %||% "")) {
        dat <- dat[!is.na(dat$outcome) & dat$outcome == input$filter_outcome, , drop = FALSE]
      }

      if (nzchar(input$filter_sun %||% "")) {
        dat <- dat[!is.na(dat$sun_exposure) & dat$sun_exposure == input$filter_sun, , drop = FALSE]
      }

      if (nzchar(input$filter_hydrology %||% "")) {
        dat <- dat[!is.na(dat$site_hydrology) & dat$site_hydrology == input$filter_hydrology, , drop = FALSE]
      }

      if (nzchar(input$filter_cultivar %||% "")) {
        dat <- dat[!is.na(dat$cultivar) & dat$cultivar == input$filter_cultivar, , drop = FALSE]
      }

      dat
    })

    # Filter status display
    output$filter_status <- renderUI({
      sp <- input$analysis_species %||% ""
      if (!nzchar(sp)) return(NULL)

      total <- nrow(db_get_species_data(sp))
      filtered <- nrow(filtered_species_data())

      if (total == filtered) {
        span(class = "small text-muted", icon("check"), sprintf(" Showing all %d", total))
      } else {
        span(class = "small text-info", icon("filter"),
             sprintf(" Showing %d of %d", filtered, total))
      }
    })

    # Download button UI
    output$download_species_ui <- renderUI({
      sp <- input$analysis_species %||% ""
      if (!nzchar(sp)) return(NULL)

      dat <- filtered_species_data()
      if (nrow(dat) == 0) return(NULL)

      div(class = "mt-3 pt-3 border-top",
        downloadButton(ns("download_species_csv"), "Download CSV",
                       class = "btn-sm btn-outline-secondary w-100",
                       icon = icon("download"))
      )
    })

    # Download handler
    output$download_species_csv <- downloadHandler(
      filename = function() {
        sp <- input$analysis_species %||% "species"
        clean_name <- gsub("[^a-zA-Z0-9]", "_", sp)
        paste0("edaphic_flora_", clean_name, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        dat <- filtered_species_data()
        export_cols <- c("species", "cultivar", "outcome", "sun_exposure", "site_hydrology",
                         "ph", "organic_matter", "texture_class", "texture_sand", "texture_silt", "texture_clay",
                         "nitrate_ppm", "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
                         "calcium_ppm", "magnesium_ppm", "cec_meq", "soluble_salts_ppm",
                         "location_lat", "location_long", "ecoregion_l4", "date", "notes")
        export_cols <- export_cols[export_cols %in% names(dat)]
        write.csv(dat[, export_cols, drop = FALSE], file, row.names = FALSE, na = "")
      }
    )

    # ---------------------------
    # Similar Species Matching
    # ---------------------------

    get_all_species_profiles <- reactive({
      data_changed()

      species_counts <- tryCatch({
        dbGetQuery(pool, "
          SELECT species, COUNT(*) as n
          FROM soil_samples
          GROUP BY species
          HAVING COUNT(*) >= 10
          ORDER BY COUNT(*) DESC
        ")
      }, error = function(e) data.frame())

      if (nrow(species_counts) == 0) return(list())

      profiles <- list()
      for (sp in species_counts$species) {
        dat <- db_get_species_data(sp)
        profiles[[sp]] <- calc_species_profile(dat)
        profiles[[sp]]$species <- sp
      }

      profiles
    })

    output$similar_species_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("shuffle", "No Species Selected", "Choose a species to find similar ones"))
      }

      current_sp <- input$analysis_species
      current_dat <- filtered_species_data()

      if (nrow(current_dat) < 3) {
        return(empty_state("database", "Insufficient Data",
                           "Need at least 3 samples to find similar species"))
      }

      current_profile <- calc_species_profile(current_dat)
      all_profiles <- get_all_species_profiles()

      all_profiles <- all_profiles[names(all_profiles) != current_sp]

      if (length(all_profiles) == 0) {
        return(empty_state("seedling", "No Comparison Data",
                           "No other species have enough samples (10+) for comparison"))
      }

      similarities <- sapply(all_profiles, function(p) calc_similarity(current_profile, p))
      similarities <- sort(similarities, decreasing = TRUE)

      top_matches <- head(similarities, 10)

      tagList(
        div(class = "mb-3",
          tags$small(class = "text-muted",
            "Species with similar soil conditions to ", tags$strong(current_sp),
            ". Based on pH, organic matter, texture, and nutrients from user-submitted data."
          )
        ),

        card(
          class = "mb-3",
          card_header(class = "py-2", tags$small(icon("crosshairs"), " Current Profile: ", tags$strong(current_sp))),
          card_body(
            class = "py-2",
            div(class = "d-flex flex-wrap gap-3 small",
              if (!is.na(current_profile$ph_mean)) span(class = "text-muted", "pH: ", tags$strong(sprintf("%.1f", current_profile$ph_mean))),
              if (!is.na(current_profile$om_mean)) span(class = "text-muted", "OM: ", tags$strong(sprintf("%.1f%%", current_profile$om_mean))),
              if (!is.null(current_profile$texture_class)) span(class = "text-muted", "Texture: ", tags$strong(current_profile$texture_class)),
              span(class = "text-muted", "Samples: ", tags$strong(current_profile$n_samples))
            )
          )
        ),

        div(class = "similar-species-list",
          lapply(names(top_matches), function(sp) {
            score <- round(top_matches[sp])
            profile <- all_profiles[[sp]]

            score_color <- if (score >= 80) "#27ae60" else if (score >= 60) "#7A9A86" else if (score >= 40) "#f39c12" else "#95a5a6"

            card(
              class = "mb-2",
              card_body(
                class = "py-2 px-3",
                div(class = "d-flex justify-content-between align-items-center",
                  div(
                    tags$strong(sp),
                    div(class = "small text-muted",
                      paste(profile$n_samples, "samples"),
                      if (!is.null(profile$success_rate)) {
                        span(title = "Success rate: % of samples with Thriving or Established outcomes",
                             style = "cursor: help; border-bottom: 1px dotted currentColor;",
                             paste0(" - ", round(profile$success_rate), "% success"))
                      },
                      if (!is.null(profile$best_sun)) paste0(" - ", profile$best_sun),
                      if (!is.null(profile$best_hydrology)) paste0(" - ", profile$best_hydrology)
                    )
                  ),
                  div(class = "text-end",
                    tags$span(class = "badge fs-6", style = paste0("background-color:", score_color),
                              title = "Soil similarity: weighted comparison of pH, OM, texture, and nutrients",
                              paste0(score, "% match")),
                    div(class = "small text-muted mt-1",
                      if (!is.na(profile$ph_mean)) sprintf("pH %.1f", profile$ph_mean) else "",
                      if (!is.na(profile$om_mean)) sprintf(" - %.1f%% OM", profile$om_mean) else ""
                    )
                  )
                )
              )
            )
          })
        ),

        if (length(top_matches) > 0) {
          tagList(
            div(class = "mt-3 small text-muted",
              icon("info-circle"), " Match scores based on similarity in pH (30%), organic matter (20%), ",
              "texture (15%), and nutrient levels (35%). Only species with 10+ samples shown."
            ),
           div(class = "mt-2 p-2 bg-light rounded small text-muted",
             icon("exclamation-triangle"), tags$strong(" Important: "),
             "Soil chemistry is just one factor in plant success. These matches do not account for ",
             "climate/hardiness zones, drainage, microclimate, or pest pressure. ",
             tags$strong("Prioritize native plants"), " and verify species are not invasive in your area before planting. ",
             tags$a(href = "https://www.invasivespeciesinfo.gov/", target = "_blank", "Check invasive species lists.")
           )
          )
        }
      )
    })

    # ---------------------------
    # Summary Tab
    # ---------------------------

    output$summary_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("search", "No Species Selected", "Choose a species from the sidebar"))
      }
      dat <- filtered_species_data()
      if (nrow(dat) == 0) {
        return(empty_state("database", "No Data", paste("No samples for", input$analysis_species, "(with current filters)")))
      }

      n_samples <- nrow(dat)
      n_cultivars <- length(unique(dat$cultivar[!is.na(dat$cultivar) & nzchar(dat$cultivar)]))

      outcome_counts <- if ("outcome" %in% names(dat)) table(dat$outcome[!is.na(dat$outcome)]) else NULL
      has_outcomes <- !is.null(outcome_counts) && length(outcome_counts) > 0

      if (has_outcomes) {
        n_success <- sum(outcome_counts[names(outcome_counts) %in% c("Thriving", "Established")], na.rm = TRUE)
        n_total_outcome <- sum(outcome_counts)
        success_rate <- if (n_total_outcome > 0) round(n_success / n_total_outcome * 100) else NA
      } else {
        success_rate <- NA
      }

      tagList(
        layout_column_wrap(
          width = 1/4,
          card(
            class = "text-center",
            card_body(
              tags$h2(class = "mb-0", n_samples),
              tags$small(class = "text-muted", "Samples")
            )
          ),
          card(
            class = "text-center",
            card_body(
              tags$h2(class = "mb-0", if (!is.na(success_rate)) paste0(success_rate, "%") else "-"),
              tags$small(class = "text-muted",
                         title = "% of samples rated Thriving or Established",
                         style = "cursor: help; border-bottom: 1px dotted currentColor;",
                         "Success Rate")
            )
          ),
          card(
            class = "text-center",
            card_body(
              tags$h2(class = "mb-0", if (n_cultivars > 0) n_cultivars else "-"),
              tags$small(class = "text-muted", "Cultivars")
            )
          ),
          card(
            class = "text-center",
            card_body(
              tags$h2(class = "mb-0", length(unique(dat$ecoregion_l4[!is.na(dat$ecoregion_l4)]))),
              tags$small(class = "text-muted", "Ecoregions")
            )
          )
        ),

        layout_column_wrap(
          width = 1/2,
          class = "mt-3",
          card(
            card_header(icon("flask"), " Soil Chemistry"),
            card_body(tableOutput(ns("summary_soil_stats")))
          ),
          card(
            card_header(icon("seedling"), " Performance & Conditions"),
            card_body(uiOutput(ns("summary_performance")))
          )
        )
      )
    })

    output$summary_soil_stats <- renderTable({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      if (nrow(dat) == 0) return(NULL)

      stats <- list()

      if (sum(!is.na(dat$ph)) > 0) {
        stats[["pH"]] <- sprintf("%.1f (%.1f - %.1f)",
                                  mean(dat$ph, na.rm = TRUE),
                                  min(dat$ph, na.rm = TRUE),
                                  max(dat$ph, na.rm = TRUE))
      }
      if (sum(!is.na(dat$organic_matter)) > 0) {
        stats[["Organic Matter"]] <- sprintf("%.1f%% (%.1f - %.1f)",
                                              mean(dat$organic_matter, na.rm = TRUE),
                                              min(dat$organic_matter, na.rm = TRUE),
                                              max(dat$organic_matter, na.rm = TRUE))
      }
      if (sum(!is.na(dat$texture_class)) > 0) {
        top_textures <- names(sort(table(dat$texture_class), decreasing = TRUE))[1:min(2, length(unique(dat$texture_class)))]
        stats[["Texture"]] <- paste(top_textures, collapse = ", ")
      }
      if (sum(!is.na(dat$nitrate_ppm)) > 0) {
        stats[["Nitrate (ppm)"]] <- sprintf("%.0f avg", mean(dat$nitrate_ppm, na.rm = TRUE))
      }
      if (sum(!is.na(dat$phosphorus_ppm)) > 0) {
        stats[["Phosphorus (ppm)"]] <- sprintf("%.0f avg", mean(dat$phosphorus_ppm, na.rm = TRUE))
      }
      if (sum(!is.na(dat$potassium_ppm)) > 0) {
        stats[["Potassium (ppm)"]] <- sprintf("%.0f avg", mean(dat$potassium_ppm, na.rm = TRUE))
      }

      if (length(stats) == 0) return(data.frame(Parameter = "No data", Value = "-"))

      data.frame(Parameter = names(stats), Value = unlist(stats), stringsAsFactors = FALSE)
    }, striped = TRUE, hover = TRUE, width = "100%", colnames = FALSE)

    output$summary_performance <- renderUI({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      if (nrow(dat) == 0) return(NULL)

      outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
      outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                          "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")

      outcome_html <- if (sum(!is.na(dat$outcome)) > 0) {
        oc <- table(factor(dat$outcome, levels = outcome_order))
        oc <- oc[oc > 0]
        div(
          tags$strong("Outcomes:"),
          tags$div(class = "d-flex flex-wrap gap-2 mt-1",
            lapply(names(oc), function(o) {
              span(class = "badge", style = paste0("background-color:", outcome_colors[o]),
                   paste(o, "(", oc[o], ")"))
            })
          )
        )
      } else {
        tags$p(class = "text-muted small", "No outcome data recorded")
      }

      sun_order <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade")
      sun_html <- if (sum(!is.na(dat$sun_exposure)) > 0) {
        sc <- table(factor(dat$sun_exposure, levels = sun_order))
        sc <- sc[sc > 0]
        div(class = "mt-3",
          tags$strong("Sun Exposure:"),
          tags$div(class = "mt-1",
            paste(sapply(names(sc), function(s) paste0(s, " (", sc[s], ")")), collapse = " - ")
          )
        )
      } else NULL

      hydro_order <- c("Dry", "Mesic", "Wet")
      hydro_html <- if (sum(!is.na(dat$site_hydrology)) > 0) {
        hc <- table(factor(dat$site_hydrology, levels = hydro_order))
        hc <- hc[hc > 0]
        div(class = "mt-3",
          tags$strong("Site Hydrology:"),
          tags$div(class = "mt-1",
            paste(sapply(names(hc), function(h) paste0(h, " (", hc[h], ")")), collapse = " - ")
          )
        )
      } else NULL

      cultivar_html <- if (sum(!is.na(dat$cultivar) & nzchar(dat$cultivar)) > 0) {
        cc <- sort(table(dat$cultivar[!is.na(dat$cultivar) & nzchar(dat$cultivar)]), decreasing = TRUE)
        div(class = "mt-3",
          tags$strong("Cultivars:"),
          tags$div(class = "mt-1",
            paste(sapply(names(cc), function(c) paste0("'", c, "' (", cc[c], ")")), collapse = " - ")
          )
        )
      } else NULL

      ecoregion_html <- if (sum(!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4)) > 0) {
        ec <- sort(table(dat$ecoregion_l4[!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4)]), decreasing = TRUE)
        if (length(ec) > 3) {
          shown <- head(ec, 3)
          label <- paste(c(sapply(names(shown), function(e) paste0(e, " (", shown[e], ")")),
                           paste0("+ ", length(ec) - 3, " more")), collapse = " - ")
        } else {
          label <- paste(sapply(names(ec), function(e) paste0(e, " (", ec[e], ")")), collapse = " - ")
        }
        div(class = "mt-3",
          tags$strong(icon("map-location-dot"), " Ecoregions:"),
          tags$div(class = "mt-1 small", label)
        )
      } else NULL

      tagList(outcome_html, sun_html, hydro_html, cultivar_html, ecoregion_html)
    })

    # ---------------------------
    # pH Distribution Tab
    # ---------------------------

    output$ph_plot_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("chart-bar", "No Species Selected", "Choose a species from the sidebar"))
      }
      dat <- filtered_species_data()
      if (nrow(dat) == 0) {
        tr <- get_usda_traits_for_name(input$analysis_species, pool)
        if (is.null(tr) || nrow(tr) == 0 || is.na(tr$soil_ph_min) || is.na(tr$soil_ph_max)) {
          return(empty_state("database", "No Data", "No samples (with current filters) or USDA pH reference available"))
        }
      }
      tagList(
        plotlyOutput(ns("ph_plot"), height = "500px"),
        tags$p(class = "text-muted small mt-2 px-3",
               icon("info-circle"), " ",
               "This histogram shows the distribution of pH values from user-submitted samples. ",
               "The green shaded region (if visible) indicates the USDA reference pH range for this species. ",
               "Soil pH affects nutrient availability; most plants prefer slightly acidic to neutral conditions (6.0-7.0).")
      )
    })

    output$ph_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      show_usda <- tryCatch(isTRUE(input$show_usda_ref), error = function(e) TRUE)
      tr <- if (show_usda) get_usda_traits_for_name(input$analysis_species, pool) else NULL
      has_usda_ph <- !is.null(tr) && nrow(tr) > 0 && !is.na(tr$soil_ph_min) && !is.na(tr$soil_ph_max)

      if (nrow(dat) == 0) {
        if (!has_usda_ph) return(NULL)
        # Empty state - show USDA range with text label
        p <- ggplot() +
          annotate("text", x = (tr$soil_ph_min[1] + tr$soil_ph_max[1]) / 2, y = 0.5,
                   label = sprintf("USDA pH: %.1f - %.1f", tr$soil_ph_min[1], tr$soil_ph_max[1]),
                   color = edaphic_colors$dark, size = 4) +
          xlim(0, 14) + ylim(0, 1) +
          labs(title = paste("pH Reference Range -", input$analysis_species),
               subtitle = "No sample data - showing USDA reference range",
               x = "Soil pH", y = "") +
          theme_edaphic() +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

        pl <- ggplotly(p) %>% config(displayModeBar = TRUE, displaylogo = FALSE)
        # Add USDA range as filled polygon trace
        ph_min <- as.numeric(tr$soil_ph_min[1])
        ph_max <- as.numeric(tr$soil_ph_max[1])
        pl <- pl %>% add_trace(
          x = c(ph_min, ph_max, ph_max, ph_min, ph_min),
          y = c(0, 0, 1, 1, 0),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = "rgba(122, 154, 134, 0.3)",
          line = list(width = 0),
          name = "USDA pH Range",
          hoverinfo = "text",
          text = sprintf("USDA Reference: pH %.1f - %.1f", ph_min, ph_max),
          showlegend = FALSE
        )
        return(pl)
      }

      p <- ggplot(dat, aes(x = ph)) +
        geom_histogram(bins = 15, fill = edaphic_colors$primary, color = "white", alpha = 0.85) +
        geom_vline(xintercept = mean(dat$ph, na.rm = TRUE),
                   color = edaphic_colors$danger, linetype = "dashed", linewidth = 1)

      p <- p + labs(title = paste("pH Distribution -", input$analysis_species),
                    subtitle = paste("n =", nrow(dat), "| Mean =", round(mean(dat$ph, na.rm = TRUE), 2),
                                     if (has_usda_ph) paste("| USDA:", tr$soil_ph_min[1], "-", tr$soil_ph_max[1]) else ""),
                    x = "Soil pH", y = "Count") +
        theme_edaphic()

      # Convert to plotly
      pl <- ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE)

      # Add USDA range as a filled polygon trace (shapes don't render reliably with ggplotly)
      if (has_usda_ph) {
        ph_min <- as.numeric(tr$soil_ph_min[1])
        ph_max <- as.numeric(tr$soil_ph_max[1])
        message(sprintf("Adding USDA overlay: pH %.1f - %.1f", ph_min, ph_max))

        # Get y-axis range from histogram to create full-height rectangle
        hist_data <- hist(dat$ph, breaks = 15, plot = FALSE)
        y_max <- max(hist_data$counts) * 1.05

        pl <- pl %>% add_trace(
          x = c(ph_min, ph_max, ph_max, ph_min, ph_min),
          y = c(0, 0, y_max, y_max, 0),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = "rgba(122, 154, 134, 0.25)",
          line = list(width = 0),
          name = "USDA pH Range",
          hoverinfo = "text",
          text = sprintf("USDA Reference: pH %.1f - %.1f", ph_min, ph_max),
          showlegend = FALSE
        )
      }

      pl
    })

    # ---------------------------
    # pH vs OM Tab
    # ---------------------------

    output$ph_om_plot_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("circle-nodes", "No Species Selected", "Choose a species from the sidebar"))
      }
      dat <- filtered_species_data()
      if (nrow(dat) == 0) {
        tr <- get_usda_traits_for_name(input$analysis_species, pool)
        if (is.null(tr) || nrow(tr) == 0 || is.na(tr$soil_ph_min) || is.na(tr$soil_ph_max)) {
          return(empty_state("database", "No Data", "No samples (with current filters) or USDA pH reference available"))
        }
      }
      tagList(
        plotlyOutput(ns("ph_om_plot"), height = "500px"),
        tags$p(class = "text-muted small mt-2 px-3",
               icon("info-circle"), " ",
               "This scatter plot shows the relationship between soil pH and organic matter content. ",
               "Points are colored by plant outcome (if available). ",
               "The green shaded region shows the USDA reference pH range if available.")
      )
    })

    output$ph_om_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      show_usda <- tryCatch(isTRUE(input$show_usda_ref), error = function(e) TRUE)
      tr <- if (show_usda) get_usda_traits_for_name(input$analysis_species, pool) else NULL
      has_usda_ph <- !is.null(tr) && nrow(tr) > 0 && !is.na(tr$soil_ph_min) && !is.na(tr$soil_ph_max)

      if (nrow(dat) == 0) {
        if (!has_usda_ph) return(NULL)
        p <- ggplot() +
          annotate("text", x = (tr$soil_ph_min[1] + tr$soil_ph_max[1]) / 2, y = 50,
                   label = sprintf("USDA pH: %.1f - %.1f", tr$soil_ph_min[1], tr$soil_ph_max[1]),
                   color = edaphic_colors$dark, size = 4) +
          xlim(0, 14) + ylim(0, 100) +
          labs(title = paste("pH vs Organic Matter -", input$analysis_species),
               subtitle = "No sample data - showing USDA pH reference range",
               x = "Soil pH", y = "Organic Matter (%)") +
          theme_edaphic()

        pl <- ggplotly(p) %>% config(displayModeBar = TRUE, displaylogo = FALSE)
        ph_min <- as.numeric(tr$soil_ph_min[1])
        ph_max <- as.numeric(tr$soil_ph_max[1])
        pl <- pl %>% add_trace(
          x = c(ph_min, ph_max, ph_max, ph_min, ph_min),
          y = c(0, 0, 100, 100, 0),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = "rgba(122, 154, 134, 0.3)",
          line = list(width = 0),
          name = "USDA pH Range",
          hoverinfo = "text",
          text = sprintf("USDA Reference: pH %.1f - %.1f", ph_min, ph_max),
          showlegend = FALSE
        )
        return(pl)
      }

      cor_val <- cor(dat$ph, dat$organic_matter, use = "complete.obs")
      has_outcome <- sum(!is.na(dat$outcome)) > 0

      p <- ggplot(dat, aes(x = ph, y = organic_matter)) +
        geom_smooth(method = "lm", se = TRUE, color = edaphic_colors$dark,
                    fill = edaphic_colors$light, alpha = 0.3, linetype = "dashed")

      if (has_outcome) {
        outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
        outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                            "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
        dat$outcome <- factor(dat$outcome, levels = outcome_order)
        dat <- dat[order(dat$outcome), ]
        p <- p +
          geom_point(aes(color = outcome,
                         text = paste0("pH: ", ph, "\nOM: ", organic_matter, "%\nOutcome: ", outcome)),
                     size = 3, alpha = 0.8) +
          scale_color_manual(values = outcome_colors, limits = outcome_order, breaks = outcome_order, drop = FALSE, na.value = "#95a5a6") +
          labs(color = "Outcome")
      } else {
        p <- p +
          geom_point(aes(color = texture_class,
                         text = paste0("pH: ", ph, "\nOM: ", organic_matter, "%\nTexture: ", texture_class)),
                     size = 3, alpha = 0.7) +
          scale_color_edaphic() +
          labs(color = "Texture")
      }

      p <- p +
        labs(title = paste("pH vs Organic Matter -", input$analysis_species),
             subtitle = paste("r =", round(cor_val, 3),
                              if (has_usda_ph) paste("| USDA pH:", tr$soil_ph_min[1], "-", tr$soil_ph_max[1]) else ""),
             x = "Soil pH", y = "Organic Matter (%)") +
        theme_edaphic()

     plt <- ggplotly(p, tooltip = "text") %>%
       config(displayModeBar = TRUE, displaylogo = FALSE)

     if (has_outcome && length(plt$x$data) > 0) {
       trace_names <- sapply(plt$x$data, function(t) if (!is.null(t$name)) t$name else "")
       desired_order <- c()
       for (outcome_name in outcome_order) {
         idx <- which(trace_names == outcome_name)
         if (length(idx) > 0) desired_order <- c(desired_order, idx)
       }
       other_indices <- setdiff(seq_along(plt$x$data), desired_order)
       new_order <- c(other_indices, desired_order)
       if (length(new_order) == length(plt$x$data)) {
         plt$x$data <- plt$x$data[new_order]
       }
     }

     # Add USDA pH range as filled polygon trace (shapes don't render reliably with ggplotly)
     if (has_usda_ph) {
       ph_min <- as.numeric(tr$soil_ph_min[1])
       ph_max <- as.numeric(tr$soil_ph_max[1])

       # Get y-axis range from data
       y_max <- max(dat$organic_matter, na.rm = TRUE) * 1.1

       plt <- plt %>% add_trace(
         x = c(ph_min, ph_max, ph_max, ph_min, ph_min),
         y = c(0, 0, y_max, y_max, 0),
         type = "scatter",
         mode = "lines",
         fill = "toself",
         fillcolor = "rgba(122, 154, 134, 0.2)",
         line = list(width = 0),
         name = "USDA pH Range",
         hoverinfo = "text",
         text = sprintf("USDA Reference: pH %.1f - %.1f", ph_min, ph_max),
         showlegend = FALSE
       )
     }

     plt
    })

    # ---------------------------
    # Nutrients Tab
    # ---------------------------

    output$nutrient_plot_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("leaf", "No Species Selected", "Choose a species from the sidebar"))
      }
      dat <- filtered_species_data()
      if (nrow(dat) == 0) {
        return(empty_state("database", "No Data", "No samples match current filters"))
      }
      tagList(
        plotlyOutput(ns("nutrient_plot"), height = "500px"),
        tags$p(class = "text-muted small mt-2 px-3",
               icon("info-circle"), " ",
               "Box plots showing the distribution of nutrient concentrations (ppm) across all samples. ",
               "N-P-K (nitrogen, phosphorus, potassium) are primary macronutrients. Ca, Mg, and S are secondary macronutrients. ",
               "Fe, Mn, Zn, Cu, and B are micronutrients needed in smaller quantities. Only nutrients with data are shown.")
      )
    })

    output$nutrient_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      if (nrow(dat) == 0) return(NULL)

      nutrient_cols <- c("nitrate_ppm", "ammonium_ppm", "phosphorus_ppm", "potassium_ppm",
                         "calcium_ppm", "magnesium_ppm", "sulfur_ppm",
                         "iron_ppm", "manganese_ppm", "zinc_ppm", "copper_ppm", "boron_ppm")
      nutrient_cols <- intersect(nutrient_cols, names(dat))

      nutrient_data <- dat %>%
        select(all_of(nutrient_cols)) %>%
        pivot_longer(everything(), names_to = "nutrient", values_to = "value") %>%
        filter(!is.na(value)) %>%
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

    # ---------------------------
    # Correlations Tab
    # ---------------------------

    output$heatmap_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("project-diagram", "No Species Selected", "Choose a species from the sidebar"))
      }
      dat <- filtered_species_data()
      if (nrow(dat) < 3) {
        return(empty_state("database", "Insufficient Data", "Need at least 3 samples for correlations"))
      }
      tagList(
        plotlyOutput(ns("heatmap_plot"), height = "550px"),
        tags$p(class = "text-muted small mt-2 px-3",
               icon("info-circle"), " ",
               "Correlation matrix showing relationships between soil parameters. Blue indicates positive correlation (when one increases, so does the other); ",
               "red indicates negative correlation. Values range from -1 (perfect negative) to +1 (perfect positive). ",
               "Strong correlations (|r| > 0.7) suggest linked soil processes.")
      )
    })

    output$heatmap_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
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

    # ---------------------------
    # Texture Tab
    # ---------------------------

    output$texture_plot_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("mountain", "No Species Selected", "Choose a species from the sidebar"))
      }
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
      if (nrow(dat) == 0) {
        return(empty_state("database", "No Texture Data", "No samples with texture data matching filters"))
      }
      tagList(
        plotOutput(ns("texture_plot"), height = "550px"),
        tags$p(class = "text-muted small mt-2 px-3",
               icon("info-circle"), " ",
               "Ternary diagram showing soil texture composition (% sand, silt, and clay). ",
               "Points are colored by USDA texture class. Soil texture affects drainage, water retention, and root penetration. ",
               "Sandy soils drain quickly; clay soils retain water and nutrients but may become compacted.")
      )
    })

    output$texture_plot <- renderPlot({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$texture_sand) & !is.na(dat$texture_silt) & !is.na(dat$texture_clay), ]
      if (nrow(dat) == 0) return(NULL)

      has_outcome <- "outcome" %in% names(dat) && sum(!is.na(dat$outcome)) >= 2

      p <- ggtern(dat, aes(x = texture_sand, y = texture_silt, z = texture_clay)) +
        theme_bw() + theme_showarrows() +
        xlab("Sand (%)") + ylab("Silt (%)") + zlab("Clay (%)")

      if (has_outcome) {
        outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
        outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                            "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
        dat$outcome <- factor(dat$outcome, levels = outcome_order)
        p <- p +
          geom_point(aes(color = outcome), size = 4, alpha = 0.8) +
          scale_color_manual(values = outcome_colors, breaks = outcome_order, na.value = "#95a5a6") +
          labs(title = paste("Soil Texture -", input$analysis_species),
               subtitle = paste(nrow(dat), "samples (colored by outcome)"),
               color = "Outcome")
      } else {
        p <- p +
          geom_point(aes(color = texture_class), size = 4, alpha = 0.7) +
          scale_color_edaphic() +
          labs(title = paste("Soil Texture -", input$analysis_species),
               subtitle = paste(nrow(dat), "samples"),
               color = "Texture")
      }

      p <- p +
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

    # ---------------------------
    # ---------------------------
    # Geography Tab (combined Map, Ecoregions, Hardiness)
    # ---------------------------

    output$geography_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("globe-americas", "No Species Selected", "Choose a species from the sidebar"))
      }

      navset_card_pill(
        id = ns("geography_tabs"),

        nav_panel(
          title = "Sample Map",
          icon = icon("map-marker-alt"),
          uiOutput(ns("map_content_ui"))
        ),
        nav_panel(
          title = "Ecoregions",
          icon = icon("layer-group"),
          uiOutput(ns("ecoregion_content_ui"))
        )
      )
    })

    # Sample Map sub-tab content
    output$map_content_ui <- renderUI({
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$location_lat) & !is.na(dat$location_long), ]
      if (nrow(dat) == 0) {
        return(empty_state("map-marker-alt", "No Location Data", "No samples with coordinates matching filters"))
      }

      has_outcome <- sum(!is.na(dat$outcome)) > 0

      tagList(
        leafletOutput(ns("map_plot"), height = "500px"),
        if (has_outcome) {
          tags$p(class = "text-muted small mt-2 px-3",
                 icon("info-circle"), " ",
                 "Markers colored by outcome: ",
                 tags$span(style = "color: #27ae60;", icon("circle"), " Thriving"), " ",
                 tags$span(style = "color: #7A9A86;", icon("circle"), " Established"), " ",
                 tags$span(style = "color: #f39c12;", icon("circle"), " Struggling"), " ",
                 tags$span(style = "color: #e74c3c;", icon("circle"), " Failed/Died"), " ",
                 tags$span(style = "color: #999;", icon("circle"), " No outcome data"))
        } else {
          tags$p(class = "text-muted small mt-2 px-3",
                 icon("info-circle"), " ",
                 "Geographic distribution of sample locations. Click markers to view soil data. ",
                 "Add outcome data to samples to see color-coded success rates.")
        }
      )
    })

    output$map_plot <- renderLeaflet({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$location_lat) & !is.na(dat$location_long), ]
      if (nrow(dat) == 0) return(NULL)

      outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                          "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
      has_outcome <- sum(!is.na(dat$outcome)) > 0

      dat$popup <- paste0(
        "<strong>", dat$species, "</strong><br>",
        "<b>pH:</b> ", dat$ph, "<br>",
        "<b>OM:</b> ", dat$organic_matter, "%<br>",
        "<b>Texture:</b> ", dat$texture_class, "<br>",
        ifelse(!is.na(dat$outcome), paste0("<b>Outcome:</b> ", dat$outcome, "<br>"), ""),
        ifelse(!is.na(dat$sun_exposure), paste0("<b>Sun:</b> ", dat$sun_exposure, "<br>"), ""),
        ifelse(!is.na(dat$site_hydrology), paste0("<b>Hydrology:</b> ", dat$site_hydrology, "<br>"), ""),
        "<b>Date:</b> ", dat$date,
        ifelse(!is.na(dat$ecoregion_l4), paste0("<br><b>Ecoregion:</b> ", dat$ecoregion_l4), "")
      )

      if (has_outcome) {
        dat$marker_color <- sapply(dat$outcome, function(o) {
          if (is.na(o)) edaphic_colors$muted else outcome_colors[o]
        })
      } else {
        dat$marker_color <- edaphic_colors$accent
      }

      leaflet(dat) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~location_long, lat = ~location_lat,
          radius = 8, color = edaphic_colors$primary,
          fillColor = ~marker_color, fillOpacity = 0.7,
          stroke = TRUE, weight = 2, popup = ~popup
        ) %>%
        setView(lng = mean(dat$location_long), lat = mean(dat$location_lat), zoom = 5)
    })

    # ---------------------------
    # Ecoregions sub-tab
    # ---------------------------

    output$ecoregion_content_ui <- renderUI({
      dat <- filtered_species_data()
      dat_eco <- dat[!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4), ]
      if (nrow(dat_eco) == 0) {
        return(empty_state("globe-americas", "No Ecoregion Data",
                           "Ecoregion lookup is currently disabled in production. Samples without coordinates cannot be assigned ecoregions."))
      }

      div(
        style = "max-height: 600px; overflow-y: auto; padding-right: 5px;",
        # Header with EPA link
        div(class = "mb-3 d-flex align-items-center justify-content-between",
            tags$span(class = "text-muted small",
                      icon("layer-group"), " Showing EPA Level IV Ecoregions"),
            tags$a(
              href = epa_ecoregion_url(),
              target = "_blank",
              class = "btn btn-sm btn-outline-secondary",
              icon("external-link-alt"), " EPA Ecoregions"
            )
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Sample Distribution by Ecoregion"),
            card_body(withSpinner(plotlyOutput(ns("ecoregion_dist_plot"), height = "250px"), type = 6, color = "#7A9A86"))
          ),
          card(
            card_header(uiOutput(ns("ecoregion_second_card_header"))),
            card_body(uiOutput(ns("ecoregion_second_card_body")))
          )
        ),
        uiOutput(ns("ecoregion_details_card")),
        tags$p(class = "text-muted small mt-2 px-3",
               icon("info-circle"), " ",
               "EPA Level IV ecoregions represent areas with similar ecosystems, geology, soils, vegetation, and climate. ",
               tags$a(href = epa_ecoregion_url(), target = "_blank", "Learn more about EPA ecoregions."))
      )
    })

    # Helper to get ecoregion data (L4)
    ecoregion_data_by_level <- reactive({
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4), ]
      if (nrow(dat) == 0) return(NULL)

      dat$ecoregion_display <- dat$ecoregion_l4
      dat$ecoregion_code <- dat$ecoregion_l4_code
      dat
    })

    # Second card header (dynamic)
    output$ecoregion_second_card_header <- renderUI({
      dat <- ecoregion_data_by_level()
      has_outcome <- !is.null(dat) && sum(!is.na(dat$outcome)) > 0
      if (has_outcome) "Success Rate by Ecoregion" else "Ecoregion Summary"
    })

    # Second card body (dynamic)
    output$ecoregion_second_card_body <- renderUI({
      dat <- ecoregion_data_by_level()
      if (is.null(dat)) return(NULL)
      has_outcome <- sum(!is.na(dat$outcome)) > 0
      if (has_outcome) {
        withSpinner(plotlyOutput(ns("ecoregion_success_plot"), height = "250px"), type = 6, color = "#7A9A86")
      } else {
        tableOutput(ns("ecoregion_table"))
      }
    })

    # Ecoregion details card
    output$ecoregion_details_card <- renderUI({
      dat <- filtered_species_data()
      dat_eco <- dat[!is.na(dat$ecoregion_l4) & nzchar(dat$ecoregion_l4), ]
      if (nrow(dat_eco) == 0) return(NULL)

      # Check if L3/L2 columns exist in the data
      has_l3 <- "ecoregion_l3" %in% names(dat_eco)
      has_l2 <- "ecoregion_l2" %in% names(dat_eco)

      # Build ecoregion hierarchy data (include L3/L2 if available)
      if (has_l3 && has_l2) {
        eco_summary <- dat_eco %>%
          group_by(ecoregion_l4, ecoregion_l4_code, ecoregion_l3, ecoregion_l3_code, ecoregion_l2, ecoregion_l2_code) %>%
          summarize(
            n = n(),
            avg_ph = round(mean(ph, na.rm = TRUE), 1),
            success_rate = if (sum(!is.na(outcome)) > 0) {
              round(sum(outcome %in% c("Thriving", "Established"), na.rm = TRUE) / sum(!is.na(outcome)) * 100, 0)
            } else NA_real_,
            .groups = "drop"
          ) %>%
          arrange(desc(n))
      } else if (has_l3) {
        eco_summary <- dat_eco %>%
          group_by(ecoregion_l4, ecoregion_l4_code, ecoregion_l3, ecoregion_l3_code) %>%
          summarize(
            n = n(),
            avg_ph = round(mean(ph, na.rm = TRUE), 1),
            success_rate = if (sum(!is.na(outcome)) > 0) {
              round(sum(outcome %in% c("Thriving", "Established"), na.rm = TRUE) / sum(!is.na(outcome)) * 100, 0)
            } else NA_real_,
            .groups = "drop"
          ) %>%
          mutate(ecoregion_l2 = NA_character_, ecoregion_l2_code = NA_character_) %>%
          arrange(desc(n))
      } else {
        eco_summary <- dat_eco %>%
          group_by(ecoregion_l4, ecoregion_l4_code) %>%
          summarize(
            n = n(),
            avg_ph = round(mean(ph, na.rm = TRUE), 1),
            success_rate = if (sum(!is.na(outcome)) > 0) {
              round(sum(outcome %in% c("Thriving", "Established"), na.rm = TRUE) / sum(!is.na(outcome)) * 100, 0)
            } else NA_real_,
            .groups = "drop"
          ) %>%
          mutate(ecoregion_l3 = NA_character_, ecoregion_l3_code = NA_character_,
                 ecoregion_l2 = NA_character_, ecoregion_l2_code = NA_character_) %>%
          arrange(desc(n))
      }

      # Build accordion items
      accordion_items <- lapply(seq_len(min(nrow(eco_summary), 8)), function(i) {
        row <- eco_summary[i, ]
        code <- if (!is.na(row$ecoregion_l4_code)) row$ecoregion_l4_code else ""

        # Use stored L3/L2 data if available, otherwise fall back to parsing
        if (!is.na(row$ecoregion_l3) && nzchar(row$ecoregion_l3)) {
          # Use stored L2 if available, otherwise derive from L3 code
          l2_name <- if (!is.na(row$ecoregion_l2) && nzchar(row$ecoregion_l2)) {
            row$ecoregion_l2
          } else {
            get_l2_name(row$ecoregion_l3_code)
          }
          hierarchy <- list(
            l3_name = row$ecoregion_l3,
            l3_code = row$ecoregion_l3_code,
            l2_name = l2_name
          )
        } else {
          # Try parsing from code first
          hierarchy <- build_ecoregion_hierarchy(row$ecoregion_l4, code)

          # If parsing failed (no L3 found), check if L4 name is actually an L3 name
          if (is.na(hierarchy$l3_name) || !nzchar(hierarchy$l3_name %||% "")) {
            # Reverse lookup: find L3 code from name
            l3_codes <- names(EPA_L3_ECOREGIONS)
            l3_match <- l3_codes[sapply(l3_codes, function(c) {
              EPA_L3_ECOREGIONS[[c]] == row$ecoregion_l4
            })]
            if (length(l3_match) > 0) {
              # The "L4" field actually contains L3 data
              l3_code <- l3_match[1]
              hierarchy <- list(
                l3_name = row$ecoregion_l4,  # The stored name IS the L3 name
                l3_code = l3_code,
                l2_name = get_l2_name(l3_code)
              )
            }
          }
        }

        accordion_panel(
          title = div(
            class = "d-flex justify-content-between align-items-center w-100",
            span(row$ecoregion_l4),
            tags$span(class = "badge bg-secondary", paste0(row$n, " samples"))
          ),
          value = paste0("eco_", i),
          div(
            class = "ps-2",
            # Show hierarchy section
            div(
              class = "mb-2",
              tags$small(class = "text-muted", icon("sitemap"), " Hierarchy:"),
              div(class = "ps-3 border-start border-2",
                  style = "border-color: #7A9A86 !important;",
                  if (!is.na(hierarchy$l2_name)) {
                    tags$div(class = "small", tags$strong("II:"), " ", hierarchy$l2_name)
                  },
                  if (!is.na(hierarchy$l3_name)) {
                    tags$div(class = "small", tags$strong("III:"), " ", hierarchy$l3_name,
                             if (!is.na(hierarchy$l3_code)) tags$span(class = "text-muted", paste0(" (", hierarchy$l3_code, ")")))
                  },
                  tags$div(class = "small", tags$strong("IV:"), " ", row$ecoregion_l4,
                           if (nzchar(code)) tags$span(class = "text-muted", paste0(" (", code, ")")))
              )
            ),
            div(class = "mb-2",
                tags$small(class = "text-muted", "Avg pH: "),
                tags$span(row$avg_ph)
            ),
            if (!is.na(row$success_rate)) {
              div(class = "mb-2",
                  tags$small(class = "text-muted", "Success Rate: "),
                  tags$span(
                    class = if (row$success_rate >= 70) "text-success" else if (row$success_rate >= 40) "text-warning" else "text-danger",
                    paste0(row$success_rate, "%")
                  )
              )
            },
            tags$a(
              href = epa_ecoregion_url(),
              target = "_blank",
              class = "small",
              icon("external-link-alt"), " EPA Ecoregion Resources"
            )
          )
        )
      })

      card(
        class = "mt-3",
        card_header(span(icon("layer-group"), " Level IV Ecoregion Details")),
        card_body(
          tags$p(class = "text-muted small mb-3",
                 "Click an ecoregion to see its hierarchy (Level II \u2192 III \u2192 IV)."),
          do.call(accordion, c(
            list(id = ns("ecoregion_accordion"), open = FALSE),
            accordion_items
          ))
        )
      )
    })

    output$ecoregion_dist_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- ecoregion_data_by_level()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)

      eco_counts <- as.data.frame(table(dat$ecoregion_display))
      names(eco_counts) <- c("Ecoregion", "Count")
      eco_counts <- eco_counts[order(-eco_counts$Count), ]
      eco_counts <- head(eco_counts, 10)  # Top 10 ecoregions
      eco_counts$Ecoregion <- factor(eco_counts$Ecoregion, levels = rev(eco_counts$Ecoregion))

      p <- ggplot(eco_counts, aes(x = Ecoregion, y = Count)) +
        geom_col(fill = edaphic_colors$accent, alpha = 0.85) +
        coord_flip() +
        labs(x = NULL, y = "Number of Samples", title = NULL) +
        theme_edaphic() +
        theme(axis.text.y = element_text(size = 9))

      ggplotly(p, tooltip = c("y")) %>%
        layout(margin = list(l = 10, r = 10, t = 10, b = 40))
    })

    output$ecoregion_success_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- ecoregion_data_by_level()
      if (is.null(dat)) return(NULL)
      dat <- dat[!is.na(dat$outcome), ]
      if (nrow(dat) < 3) return(NULL)

      eco_stats <- dat %>%
        group_by(ecoregion_display) %>%
        summarize(
          n = n(),
          success_rate = sum(outcome %in% c("Thriving", "Established")) / n() * 100,
          .groups = "drop"
        ) %>%
        filter(n >= 2) %>%  # At least 2 samples per ecoregion
        arrange(desc(success_rate)) %>%
        head(10)

      if (nrow(eco_stats) == 0) return(NULL)

      eco_stats$ecoregion_display <- factor(eco_stats$ecoregion_display,
                                             levels = rev(eco_stats$ecoregion_display))

      p <- ggplot(eco_stats, aes(x = ecoregion_display, y = success_rate, text = paste0("Samples: ", n))) +
        geom_col(aes(fill = success_rate), alpha = 0.85) +
        scale_fill_gradient(low = "#e74c3c", high = "#27ae60", guide = "none") +
        coord_flip() +
        labs(x = NULL, y = "Success Rate (%)", title = NULL) +
        theme_edaphic() +
        theme(axis.text.y = element_text(size = 9)) +
        ylim(0, 100)

      ggplotly(p, tooltip = c("y", "text")) %>%
        layout(margin = list(l = 10, r = 10, t = 10, b = 40))
    })

    output$ecoregion_table <- renderTable({
      req(input$analysis_species, input$analysis_species != "")
      dat <- ecoregion_data_by_level()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)

      eco_stats <- dat %>%
        group_by(Ecoregion = ecoregion_display) %>%
        summarize(
          Samples = n(),
          `Avg pH` = round(mean(ph, na.rm = TRUE), 1),
          `Avg OM%` = round(mean(organic_matter, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        arrange(desc(Samples)) %>%
        head(10)

      eco_stats
    }, striped = TRUE, hover = TRUE, width = "100%")

    # Ecoregion map
    # ---------------------------
    # Performance Tab
    # ---------------------------

    output$performance_ui <- renderUI({
      if (is.null(input$analysis_species) || input$analysis_species == "") {
        return(empty_state("seedling", "No Species Selected", "Choose a species from the sidebar"))
      }
      dat <- filtered_species_data()
      if (nrow(dat) == 0) {
        return(empty_state("seedling", "No Data", "No samples match current filters"))
      }

      has_outcome <- sum(!is.na(dat$outcome)) > 0
      has_sun <- sum(!is.na(dat$sun_exposure)) > 0
      has_hydro <- sum(!is.na(dat$site_hydrology)) > 0

      if (!has_outcome && !has_sun && !has_hydro) {
        return(div(
          class = "p-4 text-center",
          icon("info-circle", class = "fa-2x text-muted mb-3"),
          tags$h5("No Performance Data Yet"),
          tags$p(class = "text-muted",
                 "This tab shows plant outcome, sun exposure, and hydrology data. ",
                 "Add this information when entering new samples to see performance analysis.")
        ))
      }

      tagList(
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Outcome Distribution"),
            card_body(withSpinner(plotlyOutput(ns("performance_outcome_plot"), height = "300px"), type = 6, color = "#7A9A86"))
          ),
          card(
            card_header(icon("lightbulb"), " Key Insights"),
            card_body(withSpinner(uiOutput(ns("performance_insights")), type = 6, color = "#7A9A86"))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Sun Exposure"),
            card_body(withSpinner(plotlyOutput(ns("performance_sun_plot"), height = "250px"), type = 6, color = "#7A9A86"))
          ),
          card(
            card_header("Site Hydrology"),
            card_body(withSpinner(plotlyOutput(ns("performance_hydro_plot"), height = "250px"), type = 6, color = "#7A9A86"))
          )
        ),
        if (has_outcome) {
          tagList(
            tags$h5(class = "mt-4 mb-3", icon("search"), " Success Factors"),
            tags$p(class = "text-muted small mb-3",
                   "Compare soil conditions between thriving and struggling/failed plants to identify optimal ranges."),
            layout_column_wrap(
              width = 1/2,
              card(
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  span(icon("chart-bar"), " Parameter by Outcome"),
                  selectInput(ns("success_factor_param"), NULL,
                              choices = c("pH" = "ph",
                                          "Organic Matter (%)" = "organic_matter",
                                          "Clay Content (%)" = "texture_clay",
                                          "Sand Content (%)" = "texture_sand",
                                          "Nitrate (ppm)" = "nitrate_ppm",
                                          "Phosphorus (ppm)" = "phosphorus_ppm",
                                          "Potassium (ppm)" = "potassium_ppm",
                                          "Calcium (ppm)" = "calcium_ppm"),
                              selected = "ph",
                              width = "180px")
                ),
                card_body(plotlyOutput(ns("success_factor_plot"), height = "320px"))
              ),
              if (has_sun && has_hydro) {
                card(
                  card_header(icon("th"), " Success Matrix: Sun x Hydrology"),
                  card_body(
                    plotlyOutput(ns("success_matrix_plot"), height = "320px"),
                    tags$p(class = "text-muted small mt-2",
                           "Success rate (% Thriving + Established) by condition. Darker green = higher success.")
                  )
                )
              } else {
                card(
                  card_header(icon("info-circle"), " More Data Needed"),
                  card_body(
                    class = "text-center text-muted",
                    tags$p("Record sun exposure and hydrology data to see the Success Matrix visualization.")
                  )
                )
              }
            )
          )
        }
      )
    })

    # Performance outcome plot
    output$performance_outcome_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$outcome), ]
      if (nrow(dat) == 0) return(plotly_empty())

      outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
      outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                          "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")

      dat$outcome <- factor(dat$outcome, levels = outcome_order)

      p <- ggplot(dat, aes(x = outcome, fill = outcome)) +
        geom_bar() +
        scale_fill_manual(values = outcome_colors) +
        labs(x = NULL, y = "Count") +
        theme_edaphic() +
        theme(legend.position = "none")

      ggplotly(p, tooltip = c("y")) %>%
        config(displayModeBar = FALSE)
    })

    # Performance sun plot
    output$performance_sun_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$sun_exposure), ]
      if (nrow(dat) == 0) return(plotly_empty())

      sun_order <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade")
      dat$sun_exposure <- factor(dat$sun_exposure, levels = sun_order)

      # Count and add outcome breakdown if available
      has_outcome <- sum(!is.na(dat$outcome)) > 0

      if (has_outcome) {
        outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
        outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                            "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
        dat$outcome <- factor(dat$outcome, levels = outcome_order)

        p <- ggplot(dat, aes(x = sun_exposure, fill = outcome)) +
          geom_bar(position = "stack") +
          scale_fill_manual(values = outcome_colors, drop = FALSE) +
          labs(x = NULL, y = "Count", fill = "Outcome") +
          theme_edaphic() +
          theme(legend.position = "bottom", legend.title = element_blank())
      } else {
        p <- ggplot(dat, aes(x = sun_exposure)) +
          geom_bar(fill = edaphic_colors$primary, alpha = 0.8) +
          labs(x = NULL, y = "Count") +
          theme_edaphic()
      }

      ggplotly(p, tooltip = c("y")) %>%
        config(displayModeBar = FALSE)
    })

    # Performance hydrology plot
    output$performance_hydro_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$site_hydrology), ]
      if (nrow(dat) == 0) return(plotly_empty())

      hydro_order <- c("Dry", "Mesic", "Wet")
      dat$site_hydrology <- factor(dat$site_hydrology, levels = hydro_order)

      has_outcome <- sum(!is.na(dat$outcome)) > 0

      if (has_outcome) {
        outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
        outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                            "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
        dat$outcome <- factor(dat$outcome, levels = outcome_order)

        p <- ggplot(dat, aes(x = site_hydrology, fill = outcome)) +
          geom_bar(position = "stack") +
          scale_fill_manual(values = outcome_colors, drop = FALSE) +
          labs(x = NULL, y = "Count", fill = "Outcome") +
          theme_edaphic() +
          theme(legend.position = "bottom", legend.title = element_blank())
      } else {
        p <- ggplot(dat, aes(x = site_hydrology)) +
          geom_bar(fill = edaphic_colors$primary, alpha = 0.8) +
          labs(x = NULL, y = "Count") +
          theme_edaphic()
      }

      ggplotly(p, tooltip = c("y")) %>%
        config(displayModeBar = FALSE)
    })

    # Performance insights
    output$performance_insights <- renderUI({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      if (nrow(dat) == 0) return(NULL)

      insights <- list()

      # Overall success rate
      if (sum(!is.na(dat$outcome)) > 0) {
        outcomes <- dat$outcome[!is.na(dat$outcome)]
        n_success <- sum(outcomes %in% c("Thriving", "Established"))
        success_rate <- round(n_success / length(outcomes) * 100)
        insights <- c(insights, list(div(
          class = "mb-2",
          icon("chart-pie", class = "text-success me-2"),
          span(title = "Success rate = (Thriving + Established) / Total samples with outcomes",
               style = "cursor: help; border-bottom: 1px dotted currentColor;",
               sprintf("Overall success rate: %d%%", success_rate)),
          span(class = "text-muted small", sprintf(" (%d of %d)", n_success, length(outcomes)))
        )))
      }

      # Best sun exposure
      if (sum(!is.na(dat$sun_exposure)) > 0 && sum(!is.na(dat$outcome)) > 0) {
        sun_success <- dat %>%
          filter(!is.na(sun_exposure) & !is.na(outcome)) %>%
          group_by(sun_exposure) %>%
          summarize(
            n = n(),
            success = sum(outcome %in% c("Thriving", "Established")),
            rate = success / n * 100,
            .groups = "drop"
          ) %>%
          filter(n >= 2) %>%
          arrange(desc(rate))

        if (nrow(sun_success) > 0) {
          best_sun <- sun_success$sun_exposure[1]
          best_rate <- round(sun_success$rate[1])
          insights <- c(insights, list(div(
            class = "mb-2",
            icon("sun", class = "text-warning me-2"),
            sprintf("Best in %s (%d%% success)", best_sun, best_rate)
          )))
        }
      }

      # Best hydrology
      if (sum(!is.na(dat$site_hydrology)) > 0 && sum(!is.na(dat$outcome)) > 0) {
        hydro_success <- dat %>%
          filter(!is.na(site_hydrology) & !is.na(outcome)) %>%
          group_by(site_hydrology) %>%
          summarize(
            n = n(),
            success = sum(outcome %in% c("Thriving", "Established")),
            rate = success / n * 100,
            .groups = "drop"
          ) %>%
          filter(n >= 2) %>%
          arrange(desc(rate))

        if (nrow(hydro_success) > 0) {
          best_hydro <- hydro_success$site_hydrology[1]
          best_rate <- round(hydro_success$rate[1])
          insights <- c(insights, list(div(
            class = "mb-2",
            icon("droplet", class = "text-info me-2"),
            sprintf("Best in %s conditions (%d%% success)", best_hydro, best_rate)
          )))
        }
      }

      # pH insight
      if (sum(!is.na(dat$ph)) > 0 && sum(!is.na(dat$outcome)) > 0) {
        successful <- dat %>% filter(outcome %in% c("Thriving", "Established"), !is.na(ph))
        struggling <- dat %>% filter(outcome %in% c("Struggling", "Failed/Died"), !is.na(ph))

        if (nrow(successful) >= 2 && nrow(struggling) >= 2) {
          avg_success_ph <- mean(successful$ph)
          avg_struggle_ph <- mean(struggling$ph)

          if (abs(avg_success_ph - avg_struggle_ph) > 0.3) {
            insights <- c(insights, list(div(
              class = "mb-2",
              icon("flask", class = "text-primary me-2"),
              sprintf("Successful plants: pH %.1f avg vs %.1f for struggling",
                      avg_success_ph, avg_struggle_ph)
            )))
          }
        }
      }

      if (length(insights) == 0) {
        return(div(class = "text-muted", "Not enough data for insights yet."))
      }

      tagList(insights)
    })

    # Success factor plot
    output$success_factor_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      req(input$success_factor_param)
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$outcome), ]
      if (nrow(dat) == 0) return(plotly_empty())

      param <- input$success_factor_param
      if (!(param %in% names(dat))) return(plotly_empty())

      dat <- dat[!is.na(dat[[param]]), ]
      if (nrow(dat) < 3) return(plotly_empty())

      outcome_order <- c("Thriving", "Established", "Struggling", "Failed/Died")
      outcome_colors <- c("Thriving" = "#27ae60", "Established" = "#7A9A86",
                          "Struggling" = "#f39c12", "Failed/Died" = "#e74c3c")
      dat$outcome <- factor(dat$outcome, levels = outcome_order)

      param_label <- switch(param,
        "ph" = "pH",
        "organic_matter" = "Organic Matter (%)",
        "texture_clay" = "Clay Content (%)",
        "texture_sand" = "Sand Content (%)",
        "nitrate_ppm" = "Nitrate (ppm)",
        "phosphorus_ppm" = "Phosphorus (ppm)",
        "potassium_ppm" = "Potassium (ppm)",
        "calcium_ppm" = "Calcium (ppm)",
        param
      )

      p <- ggplot(dat, aes_string(x = "outcome", y = param, fill = "outcome")) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_manual(values = outcome_colors) +
        labs(x = NULL, y = param_label) +
        theme_edaphic() +
        theme(legend.position = "none")

      ggplotly(p, tooltip = c("y")) %>%
        config(displayModeBar = FALSE)
    })

    # Success matrix plot
    output$success_matrix_plot <- renderPlotly({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      dat <- dat[!is.na(dat$outcome) & !is.na(dat$sun_exposure) & !is.na(dat$site_hydrology), ]
      if (nrow(dat) < 5) return(plotly_empty())

      sun_levels <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade")
      hydro_levels <- c("Dry", "Mesic", "Wet")

      matrix_data <- dat %>%
        group_by(sun_exposure, site_hydrology) %>%
        summarize(
          n = n(),
          success = sum(outcome %in% c("Thriving", "Established")),
          success_rate = success / n * 100,
          label = sprintf("%d%%\n(n=%d)", round(success_rate), n),
          .groups = "drop"
        )

      matrix_data$sun_exposure <- factor(matrix_data$sun_exposure, levels = sun_levels)
      matrix_data$site_hydrology <- factor(matrix_data$site_hydrology, levels = hydro_levels)

      p <- ggplot(matrix_data, aes(x = site_hydrology, y = sun_exposure, fill = success_rate)) +
        geom_tile(color = "white", size = 1) +
        geom_text(aes(label = label), color = "white", fontface = "bold", size = 3.5) +
        scale_fill_gradient(low = "#e74c3c", high = "#27ae60", na.value = "#cccccc",
                            limits = c(0, 100), name = "Success %") +
        labs(x = "Site Hydrology", y = "Sun Exposure") +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.text = element_text(size = 10))

      ggplotly(p, tooltip = c("fill")) %>%
        config(displayModeBar = FALSE)
    })

    # ---------------------------
    # Raw Data Tab
    # ---------------------------

    output$raw_data <- renderDT({
      req(input$analysis_species, input$analysis_species != "")
      dat <- filtered_species_data()
      if (nrow(dat) == 0) return(NULL)
      datatable(dat, options = list(scrollX = TRUE, pageLength = 15))
    })

    # ---------------------------
    # USDA Traits Tab
    # ---------------------------

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
          tableOutput(ns("traits_table"))
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

      add_section <- function(label) {
        result <<- rbind(result, data.frame(Trait = label, Value = "", stringsAsFactors = FALSE))
      }

      # -- Identification --
      add_section("Identification")
      add_row("USDA Symbol", tr$usda_symbol[1])
      add_row("Scientific Name", tr$scientific_name[1])
      if ("usda_group" %in% names(tr)) add_row("Group", tr$usda_group[1])
      if ("native_status" %in% names(tr)) add_row("Native Status", tr$native_status[1])

      # -- Growth Characteristics --
      add_section("Growth Characteristics")
      if ("duration" %in% names(tr)) add_row("Duration", tr$duration[1])
      if ("growth_habit" %in% names(tr)) add_row("Growth Habit", tr$growth_habit[1])
      if ("growth_rate" %in% names(tr)) add_row("Growth Rate", tr$growth_rate[1])
      if ("lifespan" %in% names(tr)) add_row("Lifespan", tr$lifespan[1])
      if ("height_mature_ft" %in% names(tr) && !is.na(tr$height_mature_ft[1])) {
        add_row("Mature Height", sprintf("%.0f ft", tr$height_mature_ft[1]))
      }
      if ("root_depth_min_in" %in% names(tr) && !is.na(tr$root_depth_min_in[1])) {
        add_row("Minimum Root Depth", sprintf("%.0f in", tr$root_depth_min_in[1]))
      }
      if ("leaf_retention" %in% names(tr)) add_row("Leaf Retention", tr$leaf_retention[1])

      # -- Soil Requirements --
      add_section("Soil Requirements")
      ph_min <- tr$soil_ph_min[1]
      ph_max <- tr$soil_ph_max[1]
      if (!is.null(ph_min) && !is.na(ph_min) && !is.null(ph_max) && !is.na(ph_max)) {
        add_row("Soil pH Range", sprintf("%.1f - %.1f", ph_min, ph_max))
      }
      if ("soil_texture_adapted" %in% names(tr)) add_row("Adapted Soil Textures", tr$soil_texture_adapted[1])
      if ("nitrogen_fixation" %in% names(tr)) add_row("Nitrogen Fixation", tr$nitrogen_fixation[1])
      if ("caco3_tolerance" %in% names(tr)) add_row("CaCO3 Tolerance", tr$caco3_tolerance[1])

      # -- Climate & Tolerances --
      add_section("Climate & Tolerances")
      if ("precip_min_in" %in% names(tr) && !is.na(tr$precip_min_in[1]) && !is.na(tr$precip_max_in[1])) {
        add_row("Precipitation Range", sprintf("%.0f - %.0f in/yr", tr$precip_min_in[1], tr$precip_max_in[1]))
      } else if ("precipitation_min_mm" %in% names(tr) && !is.na(tr$precipitation_min_mm[1])) {
        precip_min_in <- round(tr$precipitation_min_mm[1] / 25.4, 1)
        precip_max_in <- round(tr$precipitation_max_mm[1] / 25.4, 1)
        add_row("Precipitation Range", sprintf("%.1f - %.1f in/yr", precip_min_in, precip_max_in))
      }
      if ("temp_min_f" %in% names(tr) && !is.na(tr$temp_min_f[1])) {
        add_row("Minimum Temperature", sprintf("%.0f\u00b0F", tr$temp_min_f[1]))
      } else if ("min_temp_c" %in% names(tr) && !is.na(tr$min_temp_c[1])) {
        temp_f <- as.integer(round(tr$min_temp_c[1] * 9 / 5 + 32))
        add_row("Minimum Temperature", sprintf("%d\u00b0F", temp_f))
      }
      if ("moisture_use" %in% names(tr)) add_row("Moisture Use", tr$moisture_use[1])
      add_row("Shade Tolerance", tr$shade_tolerance[1])
      add_row("Drought Tolerance", tr$drought_tolerance[1])
      add_row("Salinity Tolerance", tr$salinity_tolerance[1])
      if ("fire_tolerance" %in% names(tr)) add_row("Fire Tolerance", tr$fire_tolerance[1])
      if ("anaerobic_tolerance" %in% names(tr)) add_row("Anaerobic Tolerance", tr$anaerobic_tolerance[1])

      # -- Ornamental Characteristics --
      add_section("Ornamental Characteristics")
      if ("bloom_period" %in% names(tr)) add_row("Bloom Period", tr$bloom_period[1])
      if ("fruit_seed_period" %in% names(tr)) add_row("Fruit/Seed Period", tr$fruit_seed_period[1])
      if ("flower_color" %in% names(tr)) add_row("Flower Color", tr$flower_color[1])
      if ("foliage_color" %in% names(tr)) add_row("Foliage Color", tr$foliage_color[1])

      # Section headers (rows with empty Value)
      section_labels <- c("Identification", "Growth Characteristics", "Soil Requirements",
                          "Climate & Tolerances", "Ornamental Characteristics")

      # Remove empty section headers (sections with no data after them)
      if (nrow(result) > 0) {
        is_section <- result$Trait %in% section_labels
        keep <- rep(TRUE, nrow(result))
        for (i in seq_len(nrow(result) - 1)) {
          if (is_section[i] && is_section[i + 1]) {
            keep[i] <- FALSE
          }
        }
        # Check last row - if it's a section header, remove it
        if (nrow(result) > 0 && is_section[nrow(result)]) {
          keep[nrow(result)] <- FALSE
        }
        result <- result[keep, , drop = FALSE]
      }

      result
    }, striped = TRUE, hover = TRUE, width = "100%")

  })
}
