# R/mod_my_garden.R - My Garden Wildlife Dashboard
# Shows what wildlife a user's garden plants support via interactive donut charts

# ---------------------------
# Constants
# ---------------------------

# User-friendly family display names + blurbs with familiar species
FAMILY_INFO <- list(
  # Lepidoptera families
  Nymphalidae = list(name = "Brushfoots", blurb = "Monarchs, Painted Ladies, Red Admirals"),
  Papilionidae = list(name = "Swallowtails", blurb = "Tiger Swallowtails, Black Swallowtails, Spicebush Swallowtails"),
  Pieridae = list(name = "Whites & Sulphurs", blurb = "Cabbage Whites, Clouded Sulphurs, Orange Sulphurs"),
  Lycaenidae = list(name = "Blues & Hairstreaks", blurb = "Spring Azures, Gray Hairstreaks, Eastern Tailed-Blues"),
  Hesperiidae = list(name = "Skippers", blurb = "Silver-Spotted Skippers, Fiery Skippers, Cloudywings"),
  Noctuidae = list(name = "Owlet Moths", blurb = "Underwings, daggers, cutworms \u2014 the largest moth family"),
  Geometridae = list(name = "Geometer Moths", blurb = "Inchworms, peppered moths, emerald moths"),
  Erebidae = list(name = "Tiger Moths & Kin", blurb = "Isabella Tiger Moths (woolly bears), lichen moths"),
  Arctiidae = list(name = "Tiger Moths", blurb = "Woolly bears, Isabella moths, garden tiger moths"),
  Sphingidae = list(name = "Sphinx Moths", blurb = "Hummingbird moths, tomato hornworms, hawk moths"),
  Saturniidae = list(name = "Giant Silk Moths", blurb = "Luna Moths, Cecropia Moths, Polyphemus Moths"),
  Tortricidae = list(name = "Leafroller Moths", blurb = "Spruce budworms, codling moths, oak leafrollers"),
  Notodontidae = list(name = "Prominent Moths", blurb = "Variable oakleaf moths, walnut caterpillar moths"),
  Lasiocampidae = list(name = "Tent Caterpillar Moths", blurb = "Eastern tent caterpillars, forest tent caterpillars"),
  Pyralidae = list(name = "Snout Moths", blurb = "Meal moths, wax moths, bee moths"),
  Gracillariidae = list(name = "Leaf Miner Moths", blurb = "Tiny moths whose larvae mine inside leaves"),
  Gelechiidae = list(name = "Twirler Moths", blurb = "Leaf-tying and stem-boring micro-moths"),
  Oecophoridae = list(name = "Concealer Moths", blurb = "Small moths often associated with dead leaves and bark"),
  Sesiidae = list(name = "Clearwing Moths", blurb = "Wasp-mimicking moths with transparent wings"),
  Coleophoridae = list(name = "Casebearer Moths", blurb = "Larvae build portable cases from leaf fragments"),
  Nepticulidae = list(name = "Pygmy Moths", blurb = "Among the smallest moths, larvae mine in leaves"),
  Bucculatrigidae = list(name = "Ribbed Cocoon Makers", blurb = "Micro-moths with distinctive ribbed cocoons"),
  Tisheriidae = list(name = "Trumpet Leaf Miner Moths", blurb = "Trumpet-shaped leaf mines on oaks and other trees"),
  Cosmopterigidae = list(name = "Cosmet Moths", blurb = "Colorful micro-moths, some are stem borers"),
  Lymantriidae = list(name = "Tussock Moths", blurb = "White-marked tussock moths, spongy moths"),
  Yponomeutidae = list(name = "Ermine Moths", blurb = "Small white moths with black spots"),
  Pterophoridae = list(name = "Plume Moths", blurb = "Distinctive T-shaped moths with feathery wings"),
  Psychidae = list(name = "Bagworm Moths", blurb = "Larvae build camouflaged bags from plant material"),
  Heliozelidae = list(name = "Shield Bearer Moths", blurb = "Metallic micro-moths, larvae mine in leaves"),
  Drepanidae = list(name = "Hooktip Moths", blurb = "Arched hooktip, rose hooktip"),
  Limacodidae = list(name = "Slug Caterpillar Moths", blurb = "Saddleback caterpillars, hag moths"),
  Crambidae = list(name = "Grass Moths", blurb = "Sod webworms, grape leaffolder moths"),
  Choreutidae = list(name = "Metalmark Moths", blurb = "Day-flying moths with metallic wing patterns"),
  Cossidae = list(name = "Carpenter Moths", blurb = "Large moths whose larvae bore in wood"),
  Megalopygidae = list(name = "Flannel Moths", blurb = "Fluffy 'puss caterpillars' with hidden stinging spines"),
  Zygaenidae = list(name = "Smoky Moths", blurb = "Day-flying moths, some with bright warning colors"),
  Hepialidae = list(name = "Ghost Moths", blurb = "Swift moths with hovering courtship flights at dusk"),
  Satyridae = list(name = "Satyrs & Wood-Nymphs", blurb = "Common Wood-Nymphs, Little Wood-Satyrs"),
  Danaidae = list(name = "Milkweed Butterflies", blurb = "Monarchs, Queens \u2014 milkweed specialists"),
  Riodinidae = list(name = "Metalmarks", blurb = "Northern Metalmarks \u2014 small, jewel-like butterflies"),
  # Bee families
  Andrenidae = list(name = "Mining Bees", blurb = "Solitary ground-nesters, important spring pollinators"),
  Halictidae = list(name = "Sweat Bees", blurb = "Metallic green bees, tiny but prolific pollinators"),
  Megachilidae = list(name = "Leafcutter & Mason Bees", blurb = "Osmia (orchard bees), leafcutters that nest in stems"),
  Colletidae = list(name = "Plasterer Bees", blurb = "Cellophane bees that line nests with a waterproof secretion"),
  Apidae = list(name = "Long-Tongued Bees", blurb = "Bumble bees, carpenter bees, cuckoo bees, digger bees"),
  Melittidae = list(name = "Melittid Bees", blurb = "Oil-collecting bees, often specialists on specific flowers"),
  # Bird families
  Parulidae = list(name = "Warblers", blurb = "Yellow Warblers, Black-and-White Warblers"),
  Fringillidae = list(name = "Finches", blurb = "Goldfinches, Purple Finches, House Finches"),
  Icteridae = list(name = "Blackbirds & Orioles", blurb = "Baltimore Orioles, Red-winged Blackbirds"),
  Turdidae = list(name = "Thrushes", blurb = "American Robins, Wood Thrushes, Bluebirds"),
  Corvidae = list(name = "Crows & Jays", blurb = "Blue Jays, American Crows"),
  Picidae = list(name = "Woodpeckers", blurb = "Downy Woodpeckers, Red-bellied Woodpeckers, Flickers"),
  Trochilidae = list(name = "Hummingbirds", blurb = "Ruby-throated Hummingbirds"),
  Paridae = list(name = "Chickadees & Titmice", blurb = "Black-capped Chickadees, Tufted Titmice"),
  Sittidae = list(name = "Nuthatches", blurb = "White-breasted Nuthatches, Red-breasted Nuthatches"),
  Vireonidae = list(name = "Vireos", blurb = "Red-eyed Vireos, Warbling Vireos"),
  Bombycillidae = list(name = "Waxwings", blurb = "Cedar Waxwings \u2014 voracious fruit eaters"),
  Mimidae = list(name = "Mockingbirds & Thrashers", blurb = "Northern Mockingbirds, Gray Catbirds, Brown Thrashers"),
  Passerellidae = list(name = "New World Sparrows", blurb = "Song Sparrows, White-throated Sparrows, Towhees"),
  Cardinalidae = list(name = "Cardinals & Grosbeaks", blurb = "Northern Cardinals, Rose-breasted Grosbeaks, Indigo Buntings"),
  Phasianidae = list(name = "Grouse & Quail", blurb = "Ruffed Grouse, Wild Turkeys, Bobwhites"),
  Columbidae = list(name = "Doves", blurb = "Mourning Doves"),
  Tyrannidae = list(name = "Flycatchers", blurb = "Eastern Phoebes, Great Crested Flycatchers")
)

# Wikipedia base URL for family pages
WIKI_BASE <- "https://en.wikipedia.org/wiki/"

#' Get user-friendly family label
family_label <- function(family) {
  info <- FAMILY_INFO[[family]]
  if (!is.null(info)) info$name else family
}

#' Get family blurb
family_blurb <- function(family) {
  info <- FAMILY_INFO[[family]]
  if (!is.null(info)) info$blurb else NULL
}

#' Get Wikipedia URL for a family
family_wiki_url <- function(family) {
  paste0(WIKI_BASE, family)
}

#' Get iNaturalist taxon page URL for a family
#' Uses the name-based slug format (e.g., /taxa/49530-Geometridae)
family_inat_url <- function(family) {
  # iNat supports direct name-based URLs like /taxa/Familie-name
  paste0("https://www.inaturalist.org/taxa/", family)
}

# Donut chart colors
DONUT_COVERED_COLOR <- "#7A9A86"
DONUT_GAP_COLOR <- "#e8e5da"

# Source attribution text used on each tab
SOURCE_ATTRIBUTION <- tags$div(class = "text-muted small mt-4 p-3 rounded",
  style = "background: rgba(122,154,134,0.05); border: 1px solid rgba(122,154,134,0.15);",
  tags$strong("Sources & Methodology"),
  tags$ul(class = "mb-1 mt-1", style = "font-size: 0.85rem;",
    tags$li("Lepidoptera host plant associations adapted from Tallamy & Shropshire (2009), ",
            "Tallamy et al. (2020), and the National Wildlife Federation's Native Plant Finder."),
    tags$li("Specialist bee data adapted from Fowler (2016) and Jarrod Fowler's specialist bee compilations."),
    tags$li("Bird\u2013plant associations adapted from Audubon's Plants for Birds database and ",
            "Tallamy (2021) ", tags$em("The Nature of Oaks"), ".")
  ),
  tags$strong("A Note on Geography", class = "d-block mt-2"),
  tags$p(class = "mb-1", style = "font-size: 0.85rem;",
    "The wildlife data shown here is compiled primarily from eastern US research and represents ",
    "species documented across the broader region \u2014 not filtered to a specific state. ",
    "The actual wildlife species present in your area will vary based on geography, habitat, ",
    "and local conditions. Species counts should be interpreted as ", tags$em("potential"), " associations ",
    "rather than guarantees of local presence. For example, a moth species documented as using oaks ",
    "as a host plant may occur in the Southeast but not the Upper Midwest."),
  tags$p(class = "mb-1", style = "font-size: 0.85rem;",
    "Family-level totals (e.g., \u201c133/781 Owlet Moths\u201d) reflect your garden\u2019s coverage of ",
    "the full documented species pool. Your actual local impact is likely a meaningful subset of these numbers."),
  tags$small(class = "text-muted", "Data curated by Edaphic Flora. Not for redistribution.")
)

# Life form filter choices for gap recommendations
LIFE_FORM_CHOICES <- c("All", "Tree", "Shrub", "Perennial", "Grass/Sedge", "Vine", "Fern")

# ---------------------------
# UI
# ---------------------------

myGardenUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "My Garden",
    icon = icon("seedling"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        bg = "#f8f9fa",
        title = NULL,

        # Garden summary header
        uiOutput(ns("garden_summary")),

        hr(),

        # Scrollable species list
        div(style = "max-height: 400px; overflow-y: auto;",
          uiOutput(ns("species_list"))
        ),

        hr(),

        # Add a Plant CTA
        actionButton(ns("add_plant_btn"), "Add a Plant",
                     class = "btn-primary w-100",
                     icon = icon("plus"))
      ),

      # Main content
      navset_card_tab(
        id = ns("garden_tabs"),

        nav_panel("Overview", icon = icon("chart-pie"),
          uiOutput(ns("overview_content"))
        ),

        nav_panel("Butterflies",
          uiOutput(ns("butterfly_content"))
        ),

        nav_panel("Moths",
          uiOutput(ns("moth_content"))
        ),

        nav_panel("Specialist Bees",
          uiOutput(ns("bee_content"))
        ),

        nav_panel("Birds",
          uiOutput(ns("bird_content"))
        )
      )
    )
  )
}

# ---------------------------
# Server
# ---------------------------

myGardenServer <- function(id, pool, current_user, data_changed,
                            user_prefs, common_name_db, experience_level = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive: user's garden species ---
    garden_species <- reactive({
      data_changed()
      u <- current_user()
      if (is.null(u)) return(character())
      db_get_user_garden_species(u$user_uid, pool)
    })

    # --- Reactive: all wildlife species (for total counts) ---
    all_wildlife <- reactive({
      db_get_all_wildlife_species(pool)
    })

    # --- Reactive: wildlife coverage for user's garden ---
    wildlife_coverage <- reactive({
      sp <- garden_species()
      if (length(sp) == 0) return(data.frame())
      db_get_wildlife_coverage(sp, pool)
    })

    # --- Reactive: per-family summary stats ---
    coverage_summary <- reactive({
      cov <- wildlife_coverage()
      all_ws <- all_wildlife()
      db_get_wildlife_summary(cov, all_ws)
    })

    # --- Reactive: gap recommendations (depends on life_form filter) ---
    gap_recs <- reactive({
      cov <- wildlife_coverage()
      prefs <- user_prefs()
      user_state <- if (!is.null(prefs)) prefs$home_state else NULL
      lf_filter <- input$life_form_filter

      covered_codes <- if (nrow(cov) > 0) unique(cov$plant_species_code) else character()
      db_get_wildlife_gap_recs(covered_codes, user_state, pool,
                                life_form_filter = lf_filter)
    })

    # --- Reactive: top 3 gap genera per family (single query, cached) ---
    family_gap_genera <- reactive({
      cov <- wildlife_coverage()
      covered_codes <- if (nrow(cov) > 0) unique(cov$plant_species_code) else character()

      tryCatch({
        con <- poolCheckout(pool)
        on.exit(poolReturn(con), add = TRUE)

        dbExecute(con, "DROP TABLE IF EXISTS tmp_fam_gap_codes")
        dbExecute(con, "CREATE TEMP TABLE tmp_fam_gap_codes (species_code VARCHAR(50))")
        if (length(covered_codes) > 0) {
          dbWriteTable(con, "tmp_fam_gap_codes",
                       data.frame(species_code = covered_codes, stringsAsFactors = FALSE),
                       append = TRUE, temporary = TRUE, row.names = FALSE)
        }

        # Get top 3 genera per family ranked by species count, in one query
        result <- dbGetQuery(con, "
          WITH gap_impact AS (
            SELECT ws.family, wp.genus,
                   COUNT(DISTINCT ws.wildlife_id) AS species_supported
            FROM ref_wildlife_plants wp
            JOIN ref_wildlife_interactions wi ON wi.plant_species_code = wp.species_code
            JOIN ref_wildlife_species ws ON ws.wildlife_id = wi.wildlife_id
            WHERE NOT EXISTS (SELECT 1 FROM tmp_fam_gap_codes cc WHERE cc.species_code = wp.species_code)
              AND ws.family IS NOT NULL AND ws.family != ''
            GROUP BY ws.family, wp.genus
          ),
          ranked AS (
            SELECT family, genus, species_supported,
                   ROW_NUMBER() OVER (PARTITION BY family ORDER BY species_supported DESC) AS rn
            FROM gap_impact
          )
          SELECT family, genus, species_supported
          FROM ranked
          WHERE rn <= 3
          ORDER BY family, rn
        ")

        # Convert to a list keyed by family
        gap_map <- list()
        if (nrow(result) > 0) {
          for (fam in unique(result$family)) {
            fam_rows <- result[result$family == fam, ]
            gap_map[[fam]] <- sapply(fam_rows$genus, function(g) {
              cn <- get_genus_common_name(g, common_name_db)
              if (!is.null(cn)) cn else g
            }, USE.NAMES = FALSE)
          }
        }
        gap_map
      }, error = function(e) {
        message("Error computing family gap genera: ", e$message)
        list()
      })
    })

    # --- Pre-compute genus+epithet lookup for common names ---
    cn_lookup <- if (!is.null(common_name_db) && nrow(common_name_db) > 0) {
      # Extract genus+epithet (first two words) from scientific_name for matching
      gs_keys <- sub("^(\\S+ \\S+).*", "\\1", common_name_db$scientific_name)
      cn_vals <- tools::toTitleCase(tolower(common_name_db$common_name))
      stats::setNames(cn_vals, gs_keys)
    } else {
      character()
    }

    # --- Helper: look up common name ---
    get_common_name <- function(scientific_name) {
      if (length(cn_lookup) == 0) return(NULL)
      # Extract genus+epithet from input
      gs <- paste(head(strsplit(scientific_name, " ")[[1]], 2), collapse = " ")
      cn <- cn_lookup[gs]
      if (!is.na(cn) && nzchar(cn)) cn else NULL
    }

    # --- Sidebar: Garden Summary ---
    output$garden_summary <- renderUI({
      sp <- garden_species()
      n <- length(sp)

      tagList(
        div(class = "text-center",
          tags$i(class = "fa fa-seedling fa-2x mb-2", style = "color: #7A9A86;"),
          h5(style = "font-family: 'Montserrat', sans-serif; margin-bottom: 0.25rem;",
             "Your Garden"),
          div(class = "stat-number", n),
          div(class = "stat-label", ifelse(n == 1, "species", "species"))
        )
      )
    })

    # --- Sidebar: Species List ---
    output$species_list <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) {
        return(div(class = "text-muted text-center small py-3", "No plants yet"))
      }

      tags$ul(class = "list-unstyled",
        lapply(sp, function(s) {
          cn <- get_common_name(s)
          tags$li(class = "py-1 border-bottom",
            div(
              if (!is.null(cn)) {
                tagList(
                  tags$span(style = "font-size: 0.9rem;", cn),
                  tags$br(),
                  tags$small(class = "species-name text-muted", s)
                )
              } else {
                tags$span(class = "species-name", style = "font-size: 0.85rem;", s)
              }
            )
          )
        })
      )
    })

    # --- Add Plant button -> navigate to Data Entry ---
    observeEvent(input$add_plant_btn, {
      session$sendCustomMessage("navigateTab", "Data Entry")
    })

    # --- Helper: render a single donut chart ---
    # supported_plants: char vector of garden plant names filling this slice
    # gap_plants: char vector of recommended plant genera to fill the gap
    render_donut <- function(covered, total, label,
                              supported_plants = NULL, gap_plants = NULL) {
      gap <- max(0, total - covered)

      # Build hover text
      supported_hover <- "Supported"
      if (!is.null(supported_plants) && length(supported_plants) > 0) {
        plant_list <- paste(head(supported_plants, 6), collapse = "<br>")
        if (length(supported_plants) > 6) {
          plant_list <- paste0(plant_list, "<br>+ ", length(supported_plants) - 6, " more")
        }
        supported_hover <- paste0("<b>Your plants supporting this family:</b><br>", plant_list)
      }

      gap_hover <- "Gap"
      if (!is.null(gap_plants) && length(gap_plants) > 0 && gap > 0) {
        gap_list <- paste(head(gap_plants, 5), collapse = "<br>")
        gap_hover <- paste0("<b>Plants to add:</b><br>", gap_list)
      } else if (gap > 0) {
        gap_hover <- paste0(gap, " species not yet supported")
      }

      plot_ly(
        labels = c("Supported", "Gap"),
        values = c(covered, gap),
        type = "pie",
        hole = 0.65,
        textinfo = "none",
        hoverinfo = "text",
        text = c(supported_hover, gap_hover),
        marker = list(colors = c(DONUT_COVERED_COLOR, DONUT_GAP_COLOR)),
        showlegend = FALSE
      ) %>%
        layout(
          annotations = list(
            list(text = sprintf("<b>%d/%d</b>", as.integer(covered), as.integer(total)),
                 showarrow = FALSE, font = list(size = 14, family = "Montserrat"))
          ),
          title = list(text = label, font = list(size = 12, family = "Montserrat"),
                       y = 0.02, yanchor = "bottom"),
          margin = list(l = 10, r = 10, t = 10, b = 40),
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        ) %>%
        config(displayModeBar = FALSE)
    }

    # --- Helper: get garden plants that support a given family ---
    get_plants_for_family <- function(family, coverage_df) {
      if (is.null(coverage_df) || nrow(coverage_df) == 0) return(character())
      fam_rows <- coverage_df[coverage_df$wildlife_family == family, ]
      plants <- unique(fam_rows$garden_species)
      # Convert to common names where available
      sapply(plants, function(p) {
        cn <- get_common_name(p)
        if (!is.null(cn)) cn else p
      }, USE.NAMES = FALSE)
    }


    # --- Overview Tab ---
    output$overview_content <- renderUI({
      sp <- garden_species()

      if (length(sp) == 0) {
        return(div(class = "empty-state",
          tags$i(class = "fa fa-seedling"),
          h5("Your garden is empty!"),
          p("Submit soil data for your plants to see what wildlife they support."),
          actionButton(ns("overview_add_plant"), "Add Your First Plant",
                       class = "btn-primary mt-3", icon = icon("plus"))
        ))
      }

      summary <- coverage_summary()
      cov <- wildlife_coverage()

      low_data_callout <- NULL
      if (length(sp) <= 2) {
        low_data_callout <- div(class = "alert alert-info mb-3",
          tags$i(class = "fa fa-lightbulb me-2"),
          "Add more plants to unlock better wildlife analysis."
        )
      }

      # Calculate totals per type
      type_totals <- list()
      for (wtype in names(summary)) {
        s <- summary[[wtype]]
        type_totals[[wtype]] <- list(
          covered = sum(s$species_covered),
          total = sum(s$total_species)
        )
      }

      # Merge Skipper into Butterfly
      if (!is.null(type_totals[["Skipper"]])) {
        if (is.null(type_totals[["Butterfly"]])) type_totals[["Butterfly"]] <- list(covered = 0, total = 0)
        type_totals[["Butterfly"]]$covered <- type_totals[["Butterfly"]]$covered + type_totals[["Skipper"]]$covered
        type_totals[["Butterfly"]]$total <- type_totals[["Butterfly"]]$total + type_totals[["Skipper"]]$total
        type_totals[["Skipper"]] <- NULL
      }

      display_types <- c("Moth", "Butterfly", "Bee", "Bird")
      display_types <- display_types[display_types %in% names(type_totals)]
      type_labels <- list(Moth = "Moths", Butterfly = "Butterflies",
                          Bee = "Specialist Bees", Bird = "Birds")

      tagList(
        low_data_callout,

        # Summary donut row
        div(class = "row g-3 mb-4",
          lapply(display_types, function(wtype) {
            div(class = "col-lg-3 col-md-6 col-12",
              card(class = "text-center h-100",
                card_body(class = "p-3",
                  plotlyOutput(ns(paste0("overview_donut_", tolower(wtype))),
                               height = "200px", width = "100%"),
                  h6(class = "mt-2 mb-0", style = "font-family: 'Montserrat', sans-serif;",
                     type_labels[[wtype]] %||% wtype)
                )
              )
            )
          })
        ),

        # Total wildlife impact
        if (nrow(cov) > 0) {
          n_wildlife <- length(unique(cov$wildlife_id))
          div(class = "text-center mb-4 p-3 rounded",
            style = "background: linear-gradient(135deg, rgba(122,154,134,0.1) 0%, rgba(122,154,134,0.05) 100%);",
            div(class = "stat-number", n_wildlife),
            div(class = "stat-label", "wildlife species supported by your garden")
          )
        },

        # Gap Recommendations header + life_form filter
        div(class = "d-flex justify-content-between align-items-center mt-4 mb-3",
          h5(class = "mb-0", style = "font-family: 'Montserrat', sans-serif;",
             tags$i(class = "fa fa-lightbulb me-2", style = "color: #D39B35;"),
             "Recommended Additions"),
          div(style = "min-width: 140px;",
            selectInput(ns("life_form_filter"), NULL,
                        choices = LIFE_FORM_CHOICES, selected = "All",
                        width = "140px")
          )
        ),
        uiOutput(ns("gap_recs_panel")),

        SOURCE_ATTRIBUTION
      )
    })

    # Overview donut renderers
    observe({
      summary <- coverage_summary()

      # Merge Skipper into Butterfly for overview donuts
      if (!is.null(summary[["Skipper"]]) && !is.null(summary[["Butterfly"]])) {
        summary[["Butterfly"]] <- rbind(summary[["Butterfly"]], summary[["Skipper"]])
      } else if (!is.null(summary[["Skipper"]])) {
        summary[["Butterfly"]] <- summary[["Skipper"]]
      }

      display_types <- c("Moth", "Butterfly", "Bee", "Bird")
      display_types <- display_types[display_types %in% names(summary)]

      for (wtype in display_types) {
        local({
          my_type <- wtype
          s <- summary[[my_type]]
          covered <- sum(s$species_covered)
          total <- sum(s$total_species)

          output[[paste0("overview_donut_", tolower(my_type))]] <- renderPlotly({
            render_donut(covered, total, "")
          })
        })
      }
    })

    # --- Butterflies Tab ---
    output$butterfly_content <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) return(empty_garden_ui(ns, "butterfly"))

      summary <- coverage_summary()
      bfly_parts <- Filter(Negate(is.null), summary[c("Butterfly", "Skipper")])
      combined <- if (length(bfly_parts) > 0) do.call(rbind, bfly_parts) else data.frame()
      if (nrow(combined) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No butterfly data found for your garden plants."))
      }
      combined <- combined[order(-combined$species_covered), ]
      tagList(render_family_section(combined, "butterfly", ns), SOURCE_ATTRIBUTION)
    })

    # --- Moths Tab ---
    output$moth_content <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) return(empty_garden_ui(ns, "moth"))

      summary <- coverage_summary()
      moth_df <- summary[["Moth"]]
      if (is.null(moth_df) || nrow(moth_df) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No moth data found for your garden plants."))
      }
      moth_df <- moth_df[order(-moth_df$species_covered), ]
      tagList(render_family_section(moth_df, "moth", ns), SOURCE_ATTRIBUTION)
    })

    # --- Specialist Bees Tab ---
    output$bee_content <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) return(empty_garden_ui(ns, "bee"))

      summary <- coverage_summary()
      bee_df <- summary[["Bee"]]
      if (is.null(bee_df) || nrow(bee_df) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No specialist bee data found for your garden plants."))
      }
      bee_df <- bee_df[order(-bee_df$species_covered), ]
      tagList(render_family_section(bee_df, "bee", ns), SOURCE_ATTRIBUTION)
    })

    # --- Birds Tab ---
    output$bird_content <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) return(empty_garden_ui(ns, "bird"))

      summary <- coverage_summary()
      bird_df <- summary[["Bird"]]
      if (is.null(bird_df) || nrow(bird_df) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No bird data found for your garden plants."))
      }
      bird_df <- bird_df[order(-bird_df$species_covered), ]
      tagList(render_family_section(bird_df, "bird", ns), SOURCE_ATTRIBUTION)
    })

    # --- Dynamic family donut renderers (with hover tooltips) ---
    observe({
      summary <- coverage_summary()
      cov <- wildlife_coverage()
      gap_genera <- family_gap_genera()

      render_tab_donuts <- function(type_keys, prefix) {
        parts <- Filter(Negate(is.null), summary[type_keys])
        combined <- if (length(parts) > 0) do.call(rbind, parts) else data.frame()
        if (nrow(combined) == 0) return()
        combined <- combined[order(-combined$species_covered), ]

        output[[paste0(prefix, "_summary_donut")]] <- renderPlotly({
          render_donut(sum(combined$species_covered), sum(combined$total_species), "")
        })

        for (i in seq_len(nrow(combined))) {
          local({
            my_i <- i
            row <- combined[my_i, ]
            fam <- row$family

            # Garden plants supporting this family (R-only)
            sup_plants <- get_plants_for_family(fam, cov)
            # Gap genera from cached reactive
            gap_plants <- gap_genera[[fam]]

            output[[paste0(prefix, "_family_", my_i)]] <- renderPlotly({
              render_donut(row$species_covered, row$total_species, "",
                           supported_plants = sup_plants,
                           gap_plants = gap_plants)
            })
          })
        }
      }

      render_tab_donuts(c("Butterfly", "Skipper"), "butterfly")
      render_tab_donuts("Moth", "moth")
      render_tab_donuts("Bee", "bee")
      render_tab_donuts("Bird", "bird")
    })

    # --- Gap Recommendations Panel ---
    output$gap_recs_panel <- renderUI({
      recs <- gap_recs()

      if (is.null(recs) || nrow(recs) == 0) {
        prefs <- user_prefs()
        if (is.null(prefs) || is.null(prefs$home_state) || !nzchar(prefs$home_state)) {
          return(div(class = "alert alert-info",
            tags$i(class = "fa fa-map-pin me-2"),
            "Set your home location (zip code in the navbar) to get native plant recommendations."
          ))
        }
        lf <- input$life_form_filter
        if (!is.null(lf) && lf != "All") {
          return(div(class = "text-muted text-center py-3",
                     sprintf("No %s recommendations available. Try a different plant type.", tolower(lf))))
        }
        return(div(class = "text-muted text-center py-3",
                   "No additional plant recommendations available."))
      }

      div(class = "stagger-reveal",
        lapply(seq_len(nrow(recs)), function(i) {
          r <- recs[i, ]
          genus <- r$genus
          genus_common <- get_genus_common_name(genus, common_name_db)

          # Life form badge
          lf <- r$life_form
          lf_badge <- if (!is.null(lf) && !is.na(lf) && nzchar(lf)) {
            tags$span(class = "badge bg-light text-dark", style = "font-size: 0.7rem;", lf)
          }

          card(class = "mb-2",
            card_body(class = "py-2 px-3",
              div(class = "d-flex justify-content-between align-items-start",
                div(style = "flex: 1;",
                  div(class = "d-flex align-items-center gap-2 flex-wrap mb-1",
                    if (!is.null(genus_common)) {
                      tags$strong(style = "font-size: 1.05rem;", genus_common)
                    } else {
                      tags$strong(class = "species-name",
                                  tags$em(genus), " species")
                    },
                    if (isTRUE(r$is_keystone_genus)) {
                      tags$span(class = "badge",
                                style = "background: #D39B35; font-size: 0.7rem;",
                                title = "Keystone genus \u2014 disproportionately important for wildlife",
                                "Keystone")
                    },
                    lf_badge
                  ),
                  div(class = "text-muted mb-1",
                      style = "font-size: 0.8rem; font-family: 'JetBrains Mono', monospace; font-style: italic;",
                      genus)
                ),
                div(class = "text-end small", style = "min-width: 140px;",
                  div(style = "color: #7A9A86; font-weight: 600;",
                      sprintf("+%d wildlife species", as.integer(r$new_wildlife_count))),
                  if (!is.na(r$lep_count) && r$lep_count > 0)
                    div(class = "text-muted", sprintf("%d moths/butterflies", as.integer(r$lep_count))),
                  if (!is.na(r$bee_count) && r$bee_count > 0)
                    div(class = "text-muted", sprintf("%d bees", as.integer(r$bee_count))),
                  if (!is.na(r$bird_count) && r$bird_count > 0)
                    div(class = "text-muted", sprintf("%d birds", as.integer(r$bird_count)))
                )
              )
            )
          )
        })
      )
    })

    # Navigation from Add Plant buttons
    observeEvent(input$overview_add_plant, {
      session$sendCustomMessage("navigateTab", "Data Entry")
    })
    observeEvent(input$butterfly_add_plant, {
      session$sendCustomMessage("navigateTab", "Data Entry")
    })
    observeEvent(input$moth_add_plant, {
      session$sendCustomMessage("navigateTab", "Data Entry")
    })
    observeEvent(input$bee_add_plant, {
      session$sendCustomMessage("navigateTab", "Data Entry")
    })
    observeEvent(input$bird_add_plant, {
      session$sendCustomMessage("navigateTab", "Data Entry")
    })

    invisible(NULL)
  })
}

# ---------------------------
# Helper Functions
# ---------------------------

# Manual overrides where USDA genus-level names are obscure
GENUS_COMMON_OVERRIDES <- list(
  Vaccinium = "Blueberries",
  Carya = "Hickories",
  Populus = "Poplars",
  Prunus = "Cherries & Plums",
  Celtis = "Hackberries",
  Nyssa = "Tupelos",
  Amelanchier = "Serviceberries",
  Viburnum = "Viburnums",
  Rhus = "Sumacs",
  Ilex = "Hollies",
  Liriodendron = "Tulip Trees",
  Lindera = "Spicebushes",
  Cercis = "Redbuds",
  Cephalanthus = "Buttonbushes",
  Asclepias = "Milkweeds",
  Monarda = "Bee Balms",
  Pycnanthemum = "Mountain Mints",
  Eutrochium = "Joe-Pye Weeds",
  Vernonia = "Ironweeds",
  Zizia = "Golden Alexanders",
  Helianthus = "Sunflowers",
  Echinacea = "Coneflowers",
  Liatris = "Blazing Stars",
  Agastache = "Hyssops",
  Baptisia = "Wild Indigos",
  Lobelia = "Lobelias",
  Chelone = "Turtleheads",
  Penstemon = "Beardtongues",
  Schizachyrium = "Little Bluestems",
  Panicum = "Switchgrasses",
  Andropogon = "Bluestems",
  Sporobolus = "Dropseed Grasses"
)

#' Get a genus-level common name (plural) from common_name_db
#' Prefers manual overrides, then genus-level USDA entries, then shortest species match.
#' Pluralizes the result (e.g., "oak" -> "Oaks", "willow" -> "Willows").
get_genus_common_name <- function(genus, common_name_db) {
  if (is.null(genus)) return(NULL)

  # Check manual overrides first
  override <- GENUS_COMMON_OVERRIDES[[genus]]
  if (!is.null(override)) return(override)

  if (is.null(common_name_db) || nrow(common_name_db) == 0) return(NULL)

  # Strategy 1: Look for genus-level entry (scientific_name starts with genus but
  # second word is an authority like "L." or "Mill.", not a species epithet)
  genus_pattern <- paste0("^", genus, " ")
  genus_matches <- grep(genus_pattern, common_name_db$scientific_name, ignore.case = TRUE)

  best_name <- NULL
  for (idx in genus_matches) {
    sn <- common_name_db$scientific_name[idx]
    cn <- common_name_db$common_name[idx]
    if (is.na(cn) || !nzchar(cn)) next

    # Check if this is a genus-level entry (second word contains "." — authority marker)
    words <- strsplit(sn, " ")[[1]]
    if (length(words) >= 2 && grepl("\\.", words[2])) {
      best_name <- cn
      break
    }
  }

  # Strategy 2: If no genus-level entry found, use most common/shortest common name
  if (is.null(best_name) && length(genus_matches) > 0) {
    valid <- genus_matches[!is.na(common_name_db$common_name[genus_matches]) &
                           nzchar(common_name_db$common_name[genus_matches])]
    if (length(valid) > 0) {
      # Pick the shortest common name (most likely to be the generic one)
      names <- common_name_db$common_name[valid]
      best_name <- names[which.min(nchar(names))]
    }
  }

  if (is.null(best_name)) return(NULL)

  # Pluralize and title-case
  name <- tools::toTitleCase(tolower(best_name))

  # Simple pluralization: add "s" if not already plural
  if (!grepl("s$", name)) {
    # Handle special cases
    if (grepl("ry$", name)) {
      name <- sub("ry$", "ries", name)
    } else if (grepl("sh$|ch$|x$", name)) {
      name <- paste0(name, "es")
    } else {
      name <- paste0(name, "s")
    }
  }

  name
}

#' Empty garden state UI
empty_garden_ui <- function(ns, prefix) {
  div(class = "empty-state",
    tags$i(class = "fa fa-seedling"),
    h5("Your garden is empty!"),
    p("Submit soil data for your plants to see wildlife coverage."),
    actionButton(ns(paste0(prefix, "_add_plant")), "Add a Plant",
                 class = "btn-primary mt-3", icon = icon("plus"))
  )
}

#' Render a family section with summary donut and per-family grid
render_family_section <- function(summary_df, prefix, ns) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(div(class = "text-muted text-center py-4", "No data available"))
  }

  total_covered <- sum(summary_df$species_covered)
  total_all <- sum(summary_df$total_species)

  tagList(
    # Summary donut
    div(class = "text-center mb-4",
      div(style = "max-width: 250px; margin: 0 auto;",
        plotlyOutput(ns(paste0(prefix, "_summary_donut")),
                     height = "220px", width = "100%")
      ),
      h6(class = "mt-2", style = "font-family: 'Montserrat', sans-serif;",
         sprintf("%d of %d species supported", total_covered, total_all))
    ),

    # Per-family donut grid with enriched cards
    div(class = "row g-3 wildlife-donut-grid",
      lapply(seq_len(nrow(summary_df)), function(i) {
        row <- summary_df[i, ]
        if (row$total_species == 0) return(NULL)

        fam <- row$family
        display_name <- family_label(fam)
        blurb <- family_blurb(fam)
        wiki_url <- family_wiki_url(fam)
        inat_url <- family_inat_url(fam)

        div(class = "col-lg-3 col-md-4 col-sm-6 col-12",
          card(class = "h-100",
            card_body(class = "p-2 text-center",
              plotlyOutput(ns(paste0(prefix, "_family_", i)),
                           height = "180px", width = "100%"),
              # Family name as clickable Wikipedia link
              tags$a(href = wiki_url, target = "_blank",
                     class = "d-block mt-1",
                     style = "font-family: 'Montserrat', sans-serif; font-size: 0.75rem; font-weight: 600; color: #373D3C; text-decoration: none;",
                     display_name,
                     tags$i(class = "fa fa-external-link-alt ms-1",
                            style = "font-size: 0.6rem; opacity: 0.4;")),
              tags$small(class = "text-muted d-block",
                         sprintf("%d / %d", row$species_covered, row$total_species)),
              # Familiar species blurb
              if (!is.null(blurb)) {
                tags$small(class = "text-muted d-block mt-1",
                           style = "font-size: 0.7rem; line-height: 1.3; font-style: italic;",
                           blurb)
              },
              # iNaturalist photo link
              tags$a(href = inat_url, target = "_blank",
                     class = "d-inline-block mt-1",
                     style = "font-size: 0.65rem; color: #7A9A86;",
                     tags$i(class = "fa fa-camera me-1"),
                     "Photos")
            )
          )
        )
      })
    )
  )
}
