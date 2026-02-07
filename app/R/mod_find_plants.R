# R/mod_find_plants.R - Find Plants module
# Plant recommendation engine based on soil profile matching

# ---------------------------
# Constants
# ---------------------------

# Maximum number of species to recommend
MAX_RECOMMENDATIONS <- 15

# pH tolerance for range matching (± this value outside species range still counts)
PH_RANGE_TOLERANCE <- 0.5

# Score multipliers for calculating match scores
PH_SCORE_MULTIPLIER <- 20     # Higher = more sensitive to pH differences
OM_SCORE_MULTIPLIER <- 8      # Higher = more sensitive to OM differences

# Penalty score for pH outside species range
PH_OUT_OF_RANGE_PENALTY <- 30

# Minimum samples required for species to appear in recommendations
MIN_SAMPLES_FOR_RECOMMENDATIONS <- 10

# ---------------------------
# Helper Functions
# ---------------------------

#' Calculate match score between user input and species profile
#' @param user_profile List with user's soil values
#' @param species_profile List from calc_species_profile
#' @return Numeric match score 0-100
calc_user_match <- function(user_profile, species_profile) {
  if (is.null(species_profile)) return(0)

  scores <- c()
  weights <- c()

  # pH match (weight: 35)
  if (!is.na(user_profile$ph) && !is.na(species_profile$ph_mean)) {
    # Check if user pH is within species range (or close to mean)
    ph_in_range <- user_profile$ph >= (species_profile$ph_min - PH_RANGE_TOLERANCE) &&
                   user_profile$ph <= (species_profile$ph_max + PH_RANGE_TOLERANCE)
    if (ph_in_range) {
      ph_diff <- abs(user_profile$ph - species_profile$ph_mean)
      ph_score <- max(0, 100 - ph_diff * PH_SCORE_MULTIPLIER)
    } else {
      ph_score <- PH_OUT_OF_RANGE_PENALTY  # Penalty for being outside range
    }
    scores <- c(scores, ph_score)
    weights <- c(weights, 35)
  }

  # OM match (weight: 20)
  if (!is.na(user_profile$om) && !is.na(species_profile$om_mean)) {
    om_diff <- abs(user_profile$om - species_profile$om_mean)
    om_score <- max(0, 100 - om_diff * OM_SCORE_MULTIPLIER)
    scores <- c(scores, om_score)
    weights <- c(weights, 20)
  }

  # Texture match (weight: 15)
  if (!is.null(user_profile$texture) && nzchar(user_profile$texture) &&
      !is.null(species_profile$texture_class)) {
    texture_score <- if (user_profile$texture == species_profile$texture_class) 100 else 50
    scores <- c(scores, texture_score)
    weights <- c(weights, 15)
  }

  # Nutrient matches (weight: 6 each, total 30)
  nutrient_map <- list(
    nitrate = "nitrate_mean",
    phosphorus = "phosphorus_mean",
    potassium = "potassium_mean",
    calcium = "calcium_mean",
    magnesium = "magnesium_mean"
  )

  for (user_param in names(nutrient_map)) {
    species_param <- nutrient_map[[user_param]]
    if (!is.na(user_profile[[user_param]]) && !is.null(species_profile[[species_param]])) {
      ratio <- user_profile[[user_param]] / max(species_profile[[species_param]], 0.1)
      if (ratio > 1) ratio <- 1 / ratio
      nutrient_score <- ratio * 100
      scores <- c(scores, nutrient_score)
      weights <- c(weights, 6)
    }
  }

  if (length(scores) == 0) return(0)
  sum(scores * weights) / sum(weights)
}

#' Get plant recommendations for a given soil profile
#' @param soil_profile List with ph, om, texture, nitrate, phosphorus, potassium, calcium, magnesium
#' @param pool Database connection pool
#' @param max_results Maximum number of results to return
#' @return List with user_profile and matches, or list with error
get_recommendations <- function(soil_profile, pool, max_results = MAX_RECOMMENDATIONS) {
  if (is.null(soil_profile$ph) || is.na(soil_profile$ph)) {
    return(list(error = "A pH value is required for recommendations."))
  }

  # Get species with enough samples
  species_counts <- tryCatch({
    dbGetQuery(pool, sprintf("
      SELECT species, COUNT(*) as n
      FROM soil_samples
      GROUP BY species
      HAVING COUNT(*) >= %d
      ORDER BY COUNT(*) DESC
    ", MIN_SAMPLES_FOR_RECOMMENDATIONS))
  }, error = function(e) data.frame())

  if (nrow(species_counts) == 0) {
    return(list(error = sprintf(
      "No species have enough data (%d+ samples) for recommendations yet.",
      MIN_SAMPLES_FOR_RECOMMENDATIONS
    )))
  }

  # Build profiles and score each species
  matches <- list()
  for (sp in species_counts$species) {
    tryCatch({
      dat <- db_get_species_data(sp)
      sp_profile <- calc_species_profile(dat)
      if (!is.null(sp_profile)) {
        sp_profile$species <- sp
        score <- calc_user_match(soil_profile, sp_profile)
        matches[[length(matches) + 1]] <- list(
          species = sp,
          score = score,
          profile = sp_profile
        )
      }
    }, error = function(e) {
      message("Profile calculation error for '", sp, "': ", conditionMessage(e))
    })
  }

  if (length(matches) == 0) {
    return(list(error = "Could not calculate recommendations. Try again later."))
  }

  # Sort by score descending, take top N
  matches <- matches[order(sapply(matches, function(x) -x$score))]
  matches <- head(matches, max_results)

  list(
    user_profile = soil_profile,
    matches = matches
  )
}

#' Render a single recommendation card with common name support
#' @param m Match list (species, score, profile)
#' @param common_name_db Data frame with scientific_name and common_name columns, or NULL
#' @return Shiny tag
recommendation_card_ui <- function(m, common_name_db = NULL) {
  score <- round(m$score)
  profile <- m$profile

  # Score color
  score_color <- if (score >= 80) "#27ae60" else if (score >= 60) "#7A9A86" else if (score >= 40) "#f39c12" else "#95a5a6"

  # Success rate color
  success_color <- if (!is.null(profile$success_rate)) {
    if (profile$success_rate >= 70) "text-success" else if (profile$success_rate >= 50) "text-warning" else "text-danger"
  } else "text-muted"

  # Look up common name
  common_name <- NULL
  if (!is.null(common_name_db) && nrow(common_name_db) > 0) {
    idx <- match(m$species, common_name_db$scientific_name)
    if (!is.na(idx) && nzchar(common_name_db$common_name[idx])) {
      common_name <- tools::toTitleCase(tolower(common_name_db$common_name[idx]))
    } else {
      # Genus-species fallback
      gs <- paste(head(strsplit(m$species, " ")[[1]], 2), collapse = " ")
      idx2 <- match(gs, common_name_db$scientific_name)
      if (!is.na(idx2) && nzchar(common_name_db$common_name[idx2])) {
        common_name <- tools::toTitleCase(tolower(common_name_db$common_name[idx2]))
      }
    }
  }

  # Build match reasons
  reasons <- c()
  if (!is.na(profile$ph_mean)) {
    ph_diff <- if (!is.null(m$score)) abs(profile$ph_mean - 7.0) else NA
    reasons <- c(reasons, sprintf("pH %.1f\u2013%.1f", profile$ph_min, profile$ph_max))
  }
  if (!is.na(profile$om_mean)) {
    reasons <- c(reasons, sprintf("OM %.1f%%", profile$om_mean))
  }
  if (!is.null(profile$texture_class)) {
    reasons <- c(reasons, profile$texture_class)
  }

  card(
    class = "mb-2",
    card_body(
      class = "py-3 px-3",
      div(class = "d-flex justify-content-between align-items-start",
        div(style = "flex: 1;",
          # Species name + common name + score badge
          div(class = "d-flex align-items-center gap-2 mb-1",
            tags$strong(class = "fs-5", style = "font-family: 'JetBrains Mono', monospace;", m$species),
            tags$span(class = "badge", style = paste0("background-color:", score_color),
                      title = "Soil match: how closely this species' optimal conditions match your soil profile",
                      paste0(score, "% match"))
          ),
          # Common name
          if (!is.null(common_name)) {
            div(class = "text-muted mb-1", style = "font-size: 0.9rem;", common_name)
          },
          div(class = "small text-muted mb-2",
            paste(profile$n_samples, "samples"),
            if (!is.null(profile$success_rate)) {
              span(class = success_color,
                   title = "Success rate: % of samples with Thriving or Established outcomes",
                   style = "cursor: help; border-bottom: 1px dotted currentColor;",
                   paste0(" \u00b7 ", round(profile$success_rate), "% success rate"))
            }
          ),
          # Optimal conditions
          if (!is.null(profile$best_sun) || !is.null(profile$best_hydrology)) {
            div(class = "small",
              tags$strong("Best conditions: "),
              if (!is.null(profile$best_sun)) span(class = "badge bg-light text-dark me-1", profile$best_sun),
              if (!is.null(profile$best_hydrology)) span(class = "badge bg-light text-dark", profile$best_hydrology)
            )
          }
        ),
        # Soil comparison
        div(class = "text-end small text-muted", style = "min-width: 120px;",
          if (!is.na(profile$ph_mean)) div(sprintf("pH: %.1f (%.1f\u2013%.1f)", profile$ph_mean, profile$ph_min, profile$ph_max)),
          if (!is.na(profile$om_mean)) div(sprintf("OM: %.1f%%", profile$om_mean)),
          if (!is.null(profile$texture_class)) div(profile$texture_class)
        )
      )
    )
  )
}

# ---------------------------
# UI
# ---------------------------

findPlantsUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Find Plants",
    icon = icon("magnifying-glass-location"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Your Soil Profile",
        width = 320,
        bg = "#f8f9fa",

        # Soil Data Source toggle
        div(class = "mb-3 p-2 border rounded",
          tags$label(class = "d-block fw-semibold mb-2", style = "font-size: 0.9rem;",
                     icon("database"), " Soil Data Source"),
          radioButtons(ns("soil_source"), NULL,
                       choices = c("My Soil Test" = "manual", "Nearby Samples" = "nearby"),
                       selected = "manual", inline = TRUE)
        ),

        # --- Manual input panel ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("soil_source")),

          p(class = "small text-muted mb-3",
            "Enter your soil test values to find species that thrive in similar conditions."),

          # Soil Report Upload Section (conditional on API key availability)
          if (is_pdf_extraction_available()) {
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
                       "Supports PDF, RTF, TXT, and images.")
            )
          },

          # pH input
          numericInput(ns("ph"), "pH", value = NA, min = 3, max = 10, step = 0.1),

          # Organic matter input
          numericInput(ns("om"), "Organic Matter (%)", value = NA, min = 0, max = 50, step = 0.5),

          # Texture input
          selectInput(ns("texture"), "Soil Texture",
                      choices = c("Any" = "",
                                  "Sand", "Loamy Sand", "Sandy Loam", "Loam",
                                  "Silt Loam", "Silt", "Sandy Clay Loam",
                                  "Clay Loam", "Silty Clay Loam", "Sandy Clay",
                                  "Silty Clay", "Clay"),
                      selected = ""),

          hr(),

          # Optional nutrient inputs (collapsible)
          tags$details(
            open = NA,
            tags$summary(
              style = "cursor: pointer; font-weight: 500; color: #7A9A86;",
              icon("flask"), " Nutrient Values (Optional)"
            ),
            div(class = "mt-2",
              numericInput(ns("nitrate"), "Nitrate (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("phosphorus"), "Phosphorus (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("potassium"), "Potassium (ppm)", value = NA, min = 0, step = 1),
              numericInput(ns("calcium"), "Calcium (ppm)", value = NA, min = 0, step = 10),
              numericInput(ns("magnesium"), "Magnesium (ppm)", value = NA, min = 0, step = 10)
            )
          )
        ),

        # --- Nearby samples panel ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'nearby'", ns("soil_source")),

          p(class = "small text-muted mb-3",
            "Get recommendations based on soil data from nearby locations. ",
            "Enter coordinates or use your saved location."),

          numericInput(ns("nearby_lat"), "Latitude", value = NA, min = -90, max = 90, step = 0.001),
          numericInput(ns("nearby_lon"), "Longitude", value = NA, min = -180, max = 180, step = 0.001),
          sliderInput(ns("nearby_radius"), "Search Radius (miles)",
                      min = 1, max = MAX_NEIGHBOR_RADIUS_MILES,
                      value = DEFAULT_NEIGHBOR_RADIUS_MILES, step = 1),

          # Status of nearby data
          uiOutput(ns("nearby_status"))
        ),

        hr(),

        actionButton(ns("find_btn"), "Find Matching Plants",
                     class = "btn-success w-100", icon = icon("search")),

        hr(),
        div(class = "small text-muted",
          icon("lightbulb"), " Tip: At minimum, enter pH for best results. ",
          "More values = more accurate matches."
        )
      ),

      # Main content area (with loading spinner during calculations)
      withSpinner(uiOutput(ns("results")), type = 6, color = "#7A9A86")
    )
  )
}

# ---------------------------
# Server
# ---------------------------

findPlantsServer <- function(id, pool, current_user, is_admin, data_changed,
                             pdf_extract_limit, common_name_db = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store results
    results_data <- reactiveVal(NULL)

    # Pre-populate nearby lat/lon from user prefs
    observe({
      u <- current_user()
      if (is.null(u)) return()
      prefs <- tryCatch(db_get_user_prefs(u$user_uid, pool), error = function(e) NULL)
      if (!is.null(prefs) && !is.null(prefs$home_lat) && !is.na(prefs$home_lat)) {
        updateNumericInput(session, "nearby_lat", value = prefs$home_lat)
        updateNumericInput(session, "nearby_lon", value = prefs$home_long)
      }
    }) |> bindEvent(current_user(), once = TRUE)

    # Nearby neighbor profile (reactive, updates when inputs change)
    neighbor_profile <- reactive({
      req(input$soil_source == "nearby")
      lat <- input$nearby_lat
      lon <- input$nearby_lon
      radius <- input$nearby_radius

      if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
        return(list(has_data = FALSE, status = "no_location"))
      }

      u <- current_user()
      exclude_uid <- if (!is.null(u)) u$user_uid else NULL

      nearby_df <- db_get_nearby_samples(lat, lon, radius, pool, exclude_uid)
      calc_neighbor_profile(nearby_df)
    })

    # Render nearby status
    output$nearby_status <- renderUI({
      if (input$soil_source != "nearby") return(NULL)

      lat <- input$nearby_lat
      lon <- input$nearby_lon
      if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
        return(div(class = "alert alert-info py-2 small",
                   icon("map-pin"), " Enter coordinates or save a zip code in your profile to auto-fill."))
      }

      np <- neighbor_profile()

      if (!np$has_data) {
        return(div(class = "alert alert-warning py-2 small",
                   icon("exclamation-triangle"), " No samples found within ",
                   input$nearby_radius, " miles. Try increasing the radius."))
      }

      if (np$status == "early_access") {
        return(early_access_ui(np$n_samples, np$n_contributors))
      }

      sp <- np$soil_profile
      tagList(
        div(class = "alert alert-success py-2 small mb-2",
            icon("check-circle"), " Found ",
            tags$strong(np$n_samples), " samples from ",
            tags$strong(np$n_contributors), " contributors nearby."),
        div(class = "small border rounded p-2",
          tags$strong("Neighbor Soil Profile:"),
          div(class = "mt-1",
            if (!is.na(sp$ph)) span(class = "me-2", "pH: ", tags$strong(round(sp$ph, 1))),
            if (!is.na(sp$om)) span(class = "me-2", "OM: ", tags$strong(paste0(round(sp$om, 1), "%"))),
            if (!is.null(sp$texture)) div(class = "mt-1", "Texture: ", tags$strong(sp$texture))
          )
        )
      )
    })

    # PDF Upload Handler
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
        # Log the extraction
        db_log_extraction(u$user_uid, input$pdf_upload$name, result$tokens_used)

        # Populate form fields
        data <- result$data

        # Core soil properties
        if (!is.null(data$ph)) updateNumericInput(session, "ph", value = data$ph)
        if (!is.null(data$organic_matter)) updateNumericInput(session, "om", value = data$organic_matter)

        # Texture - map to texture class
        if (!is.null(data$texture_class) && nzchar(data$texture_class)) {
          updateSelectInput(session, "texture", selected = data$texture_class)
        }

        # Nutrients
        if (!is.null(data$nitrate_ppm)) updateNumericInput(session, "nitrate", value = data$nitrate_ppm)
        if (!is.null(data$phosphorus_ppm)) updateNumericInput(session, "phosphorus", value = data$phosphorus_ppm)
        if (!is.null(data$potassium_ppm)) updateNumericInput(session, "potassium", value = data$potassium_ppm)
        if (!is.null(data$calcium_ppm)) updateNumericInput(session, "calcium", value = data$calcium_ppm)
        if (!is.null(data$magnesium_ppm)) updateNumericInput(session, "magnesium", value = data$magnesium_ppm)

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

        showNotification("Data extracted! Click 'Find Matching Plants' to see recommendations.", type = "message", duration = 5)

      } else {
        showNotification(paste("Extraction failed:", result$error), type = "error", duration = 8)
      }
    })

    # Status output for PDF extraction (shows remaining extractions)
    output$pdf_extract_status <- renderUI({
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
            icon("clock"), sprintf(" %d extractions remaining today", remaining))
      } else {
        div(class = "text-warning small",
            icon("exclamation-triangle"), " Daily limit reached. Try again tomorrow.")
      }
    })

    # Handle Find Plants button click
    observeEvent(input$find_btn, {
      source_mode <- input$soil_source

      if (source_mode == "nearby") {
        # --- Nearby Samples mode ---
        np <- neighbor_profile()

        if (!np$has_data || np$status == "no_location") {
          results_data(list(error = "Enter your coordinates to search for nearby samples."))
          return()
        }

        if (np$status == "early_access") {
          results_data(list(error = sprintf(
            "Not enough nearby data yet (%d samples from %d contributors). Need %d samples from %d contributors.",
            np$n_samples, np$n_contributors,
            MIN_SAMPLES_FOR_PUBLIC_STATS, MIN_CONTRIBUTORS_FOR_PUBLIC_STATS
          )))
          return()
        }

        # Use neighbor profile as soil input
        soil_input <- np$soil_profile
        recs <- get_recommendations(soil_input, pool)

        if (!is.null(recs$error)) {
          results_data(recs)
        } else {
          recs$source <- "nearby"
          recs$neighbor_info <- list(
            n_samples = np$n_samples,
            n_contributors = np$n_contributors,
            radius = input$nearby_radius
          )
          results_data(recs)
        }

      } else {
        # --- Manual input mode ---
        user_profile <- list(
          ph = input$ph,
          om = input$om,
          texture = input$texture,
          nitrate = input$nitrate,
          phosphorus = input$phosphorus,
          potassium = input$potassium,
          calcium = input$calcium,
          magnesium = input$magnesium
        )

        if (is.na(user_profile$ph)) {
          results_data(list(error = "Please enter at least a pH value to find matching plants."))
          return()
        }

        recs <- get_recommendations(user_profile, pool)
        if (!is.null(recs$error)) {
          results_data(recs)
        } else {
          recs$source <- "manual"
          results_data(recs)
        }
      }
    })

    # Render results
    output$results <- renderUI({
      results <- results_data()

      if (is.null(results)) {
        return(
          div(class = "text-center py-5",
            tags$i(class = "fa fa-seedling fa-3x text-muted mb-3"),
            h4("Find Plants for Your Soil"),
            p(class = "text-muted",
              "Enter your soil test values in the sidebar and click ",
              tags$strong("Find Matching Plants"), " to discover species that thrive in similar conditions."
            ),
            div(class = "mt-4 p-3 bg-light rounded text-start", style = "max-width: 550px; margin: 0 auto;",
              tags$small(class = "text-muted",
                tags$strong("How it works:"), tags$br(),
                "We compare your soil profile against real-world data from successful plantings. ",
                "Species are ranked by how closely their optimal conditions match your soil, ",
                "with success rates and growing conditions shown for each recommendation."
              ),
              tags$hr(class = "my-2"),
              tags$small(class = "text-muted",
                tags$strong("Keep in mind:"), tags$br(),
               "Recommendations are based on soil chemistry only. ",
               tags$strong("Prioritize native plants"), " and verify species are not invasive in your area. ",
               "Also check climate compatibility and other site factors before planting. ",
                "See the FAQ for details on limitations."
              )
            )
          )
        )
      }

      if (!is.null(results$error)) {
        return(
          div(class = "alert alert-warning",
            icon("exclamation-triangle"), " ", results$error
          )
        )
      }

      user_profile <- results$user_profile
      matches <- results$matches
      source <- results$source %||% "manual"

      tagList(
        # Source banner for nearby samples
        if (source == "nearby" && !is.null(results$neighbor_info)) {
          div(class = "alert alert-info mb-3 small",
              icon("map-location-dot"), " Recommendations based on ",
              tags$strong(results$neighbor_info$n_samples), " nearby samples from ",
              tags$strong(results$neighbor_info$n_contributors), " contributors within ",
              results$neighbor_info$radius, " miles.")
        },

        # User's soil profile summary
        card(
          class = "mb-3",
          card_header(class = "py-2 bg-success text-white",
            icon("flask"),
            if (source == "nearby") " Neighbor Soil Profile" else " Your Soil Profile"
          ),
          card_body(
            class = "py-2",
            div(class = "d-flex flex-wrap gap-3",
              if (!is.na(user_profile$ph)) span("pH: ", tags$strong(user_profile$ph)),
              if (!is.na(user_profile$om)) span("OM: ", tags$strong(paste0(user_profile$om, "%"))),
              if (nzchar(user_profile$texture %||% "")) span("Texture: ", tags$strong(user_profile$texture)),
              if (!is.na(user_profile$nitrate %||% NA)) span("N: ", tags$strong(paste0(user_profile$nitrate, " ppm"))),
              if (!is.na(user_profile$phosphorus %||% NA)) span("P: ", tags$strong(paste0(user_profile$phosphorus, " ppm"))),
              if (!is.na(user_profile$potassium %||% NA)) span("K: ", tags$strong(paste0(user_profile$potassium, " ppm")))
            )
          )
        ),

        # Results header
        div(class = "d-flex justify-content-between align-items-center mb-3",
          h5(class = "mb-0", icon("leaf"), " Recommended Species"),
          span(class = "text-muted small", length(matches), " matches found")
        ),

        # Results list
        if (length(matches) == 0) {
          div(class = "alert alert-info",
            "No species found matching your soil profile. Try adjusting your values or entering fewer parameters."
          )
        } else {
          div(class = "recommendations-list",
            lapply(matches, function(m) {
              recommendation_card_ui(m, common_name_db)
            })
          )
        },

        # Footer note
        div(class = "mt-3 small text-muted",
          icon("info-circle"), " Recommendations based on user-submitted data from successful plantings. ",
          "Match scores consider pH (35%), organic matter (20%), texture (15%), and nutrients (30%). ",
          sprintf("Only species with %d+ samples are shown.", MIN_SAMPLES_FOR_RECOMMENDATIONS)
        ),

        # Important caveats
        div(class = "mt-3 p-3 border rounded",
          div(class = "small",
            tags$strong(icon("exclamation-triangle"), " Important Limitations"),
            p(class = "text-muted mt-2 mb-2",
              "These recommendations are based on soil chemistry only. Before planting, also consider:"),
            tags$ul(class = "text-muted mb-2",
              tags$li(tags$strong("Climate compatibility"), " \u2014 Check hardiness zones for your location"),
              tags$li(tags$strong("Site conditions"), " \u2014 Drainage, light exposure, and microclimate matter beyond soil chemistry"),
              tags$li(tags$strong("Water needs"), " \u2014 Rainfall patterns and irrigation availability")
            ),
           # Native species callout
           div(class = "mt-2 p-2 bg-light rounded border-start border-success border-3",
             tags$strong(icon("leaf", class = "text-success"), " Prioritize Native Plants"),
             p(class = "text-muted mb-1 mt-1",
               "Native plants support local ecosystems, require less maintenance, and are adapted to your climate. ",
               "Before planting any species, verify it is not invasive in your region."),
             tags$a(href = "https://www.invasivespeciesinfo.gov/", target = "_blank", class = "small",
                    "Check invasive species lists ", icon("external-link-alt"))
           ),
            p(class = "text-muted mb-0",
              "Use these recommendations as a starting point for research, not as planting guarantees. ",
              "See the ", actionLink(ns("faq_link"), "FAQ"), " for more details on how matching works."
            )
          )
        )
      )
    })

    # Link to FAQ - returns the action to parent
    observeEvent(input$faq_link, {
      # Use session$sendCustomMessage or return value for navigation
      # For now, we'll handle this in the main app
    })

    # Return FAQ link click for parent to handle navigation
    return(reactive(input$faq_link))
  })
}
