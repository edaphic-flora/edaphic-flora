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
  Depressariidae = list(name = "Flat-Body Moths", blurb = "Agonopterix and kin \u2014 larvae feed in flower heads and rolled leaves"),
  Choreutidae = list(name = "Metalmark Moths", blurb = "Day-flying moths with metallic wing patterns"),
  Cossidae = list(name = "Carpenter Moths", blurb = "Large moths whose larvae bore in wood"),
  Megalopygidae = list(name = "Flannel Moths", blurb = "Fluffy 'puss caterpillars' with hidden stinging spines"),
  Zygaenidae = list(name = "Smoky Moths", blurb = "Day-flying moths, some with bright warning colors"),
  Hepialidae = list(name = "Ghost Moths", blurb = "Swift moths with hovering courtship flights at dusk"),
  Blastobasidae = list(name = "Scavenger Moths", blurb = "Small moths whose larvae feed on seeds and detritus"),
  Thyatiridae = list(name = "Lutestring Moths", blurb = "Subtly patterned moths, often on birch and willow"),
  Tineidae = list(name = "Fungus & Clothes Moths", blurb = "Larvae feed on fungi, lichens, and organic debris"),
  Elachistidae = list(name = "Grass-Miner Moths", blurb = "Tiny moths whose larvae mine in grass blades"),
  Lyonetiidae = list(name = "Liner Moths", blurb = "Minute moths with narrow fringed wings"),
  Acrolophidae = list(name = "Tube Moths", blurb = "Larvae build silk tubes in soil and leaf litter"),
  Scythrididae = list(name = "Flower Moths", blurb = "Small day-flying moths often found on flowers"),
  Eriocraniidae = list(name = "Sparkling Archaic Moths", blurb = "Primitive metallic moths, early spring fliers"),
  Mimallonidae = list(name = "Sack-Bearer Moths", blurb = "Caterpillars build leaf cases, resembling bagworms"),
  Incurvariidae = list(name = "Leafcutter Moths", blurb = "Females cut leaf discs to protect their eggs"),
  Epiplemidae = list(name = "Tropical Geometers", blurb = "Geometrid relatives with scalloped wing edges"),
  Carposinidae = list(name = "Fruitworm Moths", blurb = "Larvae bore into fruits and seeds"),
  Ypsolophidae = list(name = "Ypsolophid Moths", blurb = "Small moths, larvae on trees and shrubs"),
  Momphidae = list(name = "Mompha Moths", blurb = "Tiny moths whose larvae mine in evening primrose stems"),
  Apatelodidae = list(name = "American Silkworm Moths", blurb = "Fuzzy moths related to silk moths"),
  Schreckensteiniidae = list(name = "Bristle-Legged Moths", blurb = "Distinctive spiny-legged micro-moths"),
  Libytheidae = list(name = "Snout Butterflies", blurb = "American Snouts \u2014 long palps resemble a snout"),
  Symmocidae = list(name = "Symmocid Moths", blurb = "Obscure micro-moths, mostly bark and litter feeders"),
  Douglasiidae = list(name = "Douglas Moths", blurb = "Tiny leaf-mining moths on mints and composites"),
  Epermeniidae = list(name = "Fringe-Tufted Moths", blurb = "Small moths with raised scale tufts on hindwings"),
  Copromorphidae = list(name = "Tropical Fruitworm Moths", blurb = "Rare family, larvae in fruits and seeds"),
  Plutellidae = list(name = "Diamondback Moths", blurb = "Includes the widespread diamondback moth"),
  Urodidae = list(name = "False Burnet Moths", blurb = "Rare moths related to burnets"),
  Adelidae = list(name = "Fairy Moths", blurb = "Tiny metallic moths with extremely long antennae"),
  Thyrididae = list(name = "Window-Winged Moths", blurb = "Translucent wing patches like tiny stained glass"),
  Prodoxidae = list(name = "Yucca Moths", blurb = "Famous pollinators of yucca plants"),
  Galacticidae = list(name = "Galactic Moths", blurb = "Rare micro-moths, poorly known biology"),
  Satyridae = list(name = "Satyrs & Wood-Nymphs", blurb = "Common Wood-Nymphs, Little Wood-Satyrs"),
  Danaidae = list(name = "Milkweed Butterflies", blurb = "Monarchs, Queens \u2014 milkweed specialists"),
  Riodinidae = list(name = "Metalmarks", blurb = "Northern Metalmarks \u2014 small, jewel-like butterflies"),
  # Bird families missing blurbs
  Anatidae = list(name = "Ducks & Geese", blurb = "Wood Ducks, Mallards \u2014 waterfowl that eat seeds and acorns"),
  Sturnidae = list(name = "Starlings", blurb = "European Starlings \u2014 introduced, abundant fruit eaters"),
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

# Source attribution builder (state-aware)
build_source_attribution <- function(state_code = NULL) {
  base_sources <- tags$ul(class = "mb-1 mt-1", style = "font-size: 0.85rem;",
    tags$li("Lepidoptera host plant associations adapted from Tallamy & Shropshire (2009), ",
            "Tallamy et al. (2020), and the National Wildlife Federation's Native Plant Finder."),
    tags$li("Specialist bee data adapted from Fowler (2016) and Jarrod Fowler's specialist bee compilations."),
    tags$li("Bird\u2013plant associations adapted from Audubon's Plants for Birds database and ",
            "Tallamy (2021) ", tags$em("The Nature of Oaks"), ".")
  )
  if (!is.null(state_code) && nzchar(state_code)) {
    occurrence_sources <- tags$ul(class = "mb-1", style = "font-size: 0.85rem;",
      tags$li("Butterfly and bee state occurrence data from ",
              tags$a("GBIF.org", href = "https://www.gbif.org", target = "_blank"),
              ", licensed under CC BY."),
      tags$li("Bird state occurrence data from ",
              tags$a("eBird", href = "https://ebird.org", target = "_blank"),
              " (Cornell Lab of Ornithology).")
    )
    geo_note <- tags$p(class = "mb-1", style = "font-size: 0.85rem;",
      sprintf("Wildlife totals are filtered to species confirmed in %s ", state_code),
      "based on GBIF and eBird occurrence records. ",
      "Only families with at least one species recorded in your state are shown. ",
      "Plant\u2013wildlife interaction data is strongest for the eastern US; ",
      "western states may show lower coverage as research expands.")
  } else {
    occurrence_sources <- NULL
    geo_note <- tags$p(class = "mb-1", style = "font-size: 0.85rem;",
      "The wildlife data shown here is compiled primarily from eastern US research and represents ",
      "species documented across the broader region \u2014 not filtered to a specific state. ",
      "Set your home state in preferences for state-level filtering.")
  }
  tags$div(class = "text-muted small mt-4 p-3 rounded",
    style = "background: rgba(122,154,134,0.05); border: 1px solid rgba(122,154,134,0.15);",
    tags$strong("Sources & Methodology"),
    base_sources,
    occurrence_sources,
    tags$strong("A Note on Geography", class = "d-block mt-2"),
    geo_note,
    tags$small(class = "text-muted", "Data curated by Edaphic Flora. Not for redistribution.")
  )
}

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

        # Introduced + invasive collapsible sections
        uiOutput(ns("nonnative_sections")),

        hr(),

        # Home location
        uiOutput(ns("location_section")),

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
                            user_prefs, common_name_db, experience_level = NULL,
                            zipcode_db = NULL, prefs_changed = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive: user's garden species ---
    garden_species <- reactive({
      data_changed()
      u <- current_user()
      if (is.null(u)) return(character())
      db_get_user_garden_species(u$user_uid, pool)
    })

    # --- Reactive: user's state for wildlife presence filtering ---
    user_state <- reactive({
      prefs <- user_prefs()
      if (!is.null(prefs)) prefs$home_state else NULL
    })

    # --- Reactive: check if state has presence data (for fallback) ---
    state_has_presence_data <- reactive({
      st <- user_state()
      if (is.null(st) || !nzchar(st)) return(FALSE)
      tryCatch({
        n <- dbGetQuery(pool,
          "SELECT COUNT(*)::int AS n FROM ref_wildlife_state_presence WHERE state_code = $1",
          params = list(st)
        )$n[1]
        !is.null(n) && n >= 50
      }, error = function(e) FALSE)
    })

    # --- Reactive: effective state_code for queries (NULL if no/low data) ---
    effective_state <- reactive({
      st <- user_state()
      if (!is.null(st) && nzchar(st) && state_has_presence_data()) st else NULL
    })

    # --- Reactive: all wildlife species (for total counts, state-filtered when available) ---
    all_wildlife <- reactive({
      db_get_all_wildlife_species(pool, effective_state())
    })

    # --- Reactive: wildlife coverage for user's garden (state-filtered when available) ---
    wildlife_coverage <- reactive({
      sp <- garden_species()
      if (length(sp) == 0) return(data.frame())
      db_get_wildlife_coverage(sp, pool, effective_state())
    })

    # --- Reactive: per-family summary stats ---
    coverage_summary <- reactive({
      cov <- wildlife_coverage()
      all_ws <- all_wildlife()
      db_get_wildlife_summary(cov, all_ws)
    })

    # --- Reactive: classify every garden species (non-natives only listed) ---
    # Composes the existing cached helpers and attaches species-level wildlife counts.
    # Splits into introduced (non-invasive) and invasive views downstream.
    nonnative_summary <- reactive({
      sp <- garden_species()
      prefs <- user_prefs()
      st <- effective_state()
      empty <- data.frame(
        species = character(0), common_name = character(0),
        native_status = character(0), state_code = character(0),
        state_name = character(0),
        is_invasive_in_state = logical(0), is_federal = logical(0),
        invasive_designation = character(0), other_states = integer(0),
        lep_count = integer(0), bee_count = integer(0),
        bird_count = integer(0), total_count = integer(0),
        stringsAsFactors = FALSE
      )
      if (length(sp) == 0) return(empty)

      rows <- lapply(sp, function(s) {
        nat <- get_native_status_for_user(s, prefs, pool)
        if (!nat$status %in% c("introduced", "introduced_na", "both")) return(NULL)
        inv <- get_invasive_status(s, nat$state_code, pool)
        data.frame(
          species = s,
          common_name = get_common_name(s) %||% NA_character_,
          native_status = nat$status,
          state_code = nat$state_code %||% NA_character_,
          state_name = nat$state_name %||% NA_character_,
          is_invasive_in_state = isTRUE(inv$in_user_state) || isTRUE(inv$is_federal),
          is_federal = isTRUE(inv$is_federal),
          invasive_designation = inv$user_state_designation %||% NA_character_,
          other_states = length(setdiff(inv$states_listed, toupper(nat$state_code %||% ""))),
          stringsAsFactors = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) return(empty)
      df <- do.call(rbind, rows)

      counts <- db_get_species_level_wildlife_counts(df$species, pool, st)
      df <- merge(df, counts, by = "species", all.x = TRUE, sort = FALSE)
      df$lep_count[is.na(df$lep_count)] <- 0L
      df$bee_count[is.na(df$bee_count)] <- 0L
      df$bird_count[is.na(df$bird_count)] <- 0L
      df$total_count[is.na(df$total_count)] <- 0L

      df <- df[order(-df$total_count, df$species), , drop = FALSE]
      rownames(df) <- NULL
      df
    })

    introduced_plants <- reactive({
      df <- nonnative_summary()
      df[!df$is_invasive_in_state, , drop = FALSE]
    })

    invasive_plants <- reactive({
      df <- nonnative_summary()
      df[df$is_invasive_in_state, , drop = FALSE]
    })

    # Native + unknown garden species — what shows in the main sidebar species list.
    native_garden_species <- reactive({
      all_sp <- garden_species()
      flagged <- nonnative_summary()$species
      setdiff(all_sp, flagged)
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

    # --- Sidebar: Species List (grouped by genus) ---
    # Shows native + unclassified plants only. Introduced and invasive plants
    # are surfaced in the dropdowns below.
    output$species_list <- renderUI({
      if (length(garden_species()) == 0) {
        return(div(class = "text-muted text-center small py-3", "No plants yet"))
      }
      sp <- native_garden_species()
      if (length(sp) == 0) {
        return(div(class = "text-muted text-center small py-3 fst-italic",
                   "All recorded plants are non-native — see below."))
      }

      # Group species by genus (first word of scientific name)
      genera <- sapply(strsplit(sp, " "), `[`, 1)
      genus_groups <- split(sp, genera)
      # Sort genera alphabetically
      genus_groups <- genus_groups[order(names(genus_groups))]

      tagList(
        lapply(names(genus_groups), function(genus) {
          species_in_genus <- genus_groups[[genus]]
          genus_cn <- get_genus_common_name(genus, common_name_db)
          genus_label <- if (!is.null(genus_cn)) genus_cn else genus

          if (length(species_in_genus) == 1) {
            # Single species in genus — show inline, no dropdown
            s <- species_in_genus[1]
            cn <- get_common_name(s)
            div(class = "py-1 border-bottom",
              if (!is.null(cn)) {
                tagList(
                  tags$span(style = "font-size: 0.85rem;", cn),
                  tags$br(),
                  tags$small(class = "species-name text-muted", s)
                )
              } else {
                tags$span(class = "species-name", style = "font-size: 0.85rem;", s)
              }
            )
          } else {
            # Multiple species — collapsible genus group
            group_id <- paste0("genus_group_", gsub("[^a-zA-Z]", "", genus))
            div(class = "border-bottom",
              # Genus header (clickable)
              div(class = "py-1 d-flex align-items-center",
                  style = "cursor: pointer;",
                  onclick = sprintf(
                    "var el=document.getElementById('%s'); var arr=document.getElementById('%s_arr');
                     if(el.style.display==='none'){el.style.display='block';arr.className='fa fa-chevron-up';}
                     else{el.style.display='none';arr.className='fa fa-chevron-down';}", group_id, group_id),
                tags$i(id = paste0(group_id, "_arr"), class = "fa fa-chevron-down",
                       style = "font-size: 0.6rem; color: #7A9A86; width: 12px;"),
                tags$span(style = "font-size: 0.85rem; font-weight: 500; margin-left: 4px;",
                          genus_label),
                tags$span(class = "badge rounded-pill bg-light text-muted ms-auto",
                          style = "font-size: 0.7rem;",
                          length(species_in_genus))
              ),
              # Species list (collapsed by default)
              div(id = group_id, style = "display: none; padding-left: 16px;",
                lapply(species_in_genus, function(s) {
                  cn <- get_common_name(s)
                  epithet <- paste(strsplit(s, " ")[[1]][-1], collapse = " ")
                  div(class = "py-1",
                      style = "font-size: 0.8rem; border-bottom: 1px solid #f5f2e9;",
                    if (!is.null(cn)) {
                      tagList(
                        tags$span(cn),
                        tags$br(),
                        tags$small(class = "species-name text-muted", s)
                      )
                    } else {
                      tags$span(class = "species-name", s)
                    }
                  )
                })
              )
            )
          }
        })
      )
    })

    # --- Add Plant button -> navigate to Data Entry ---
    observeEvent(input$add_plant_btn, {
      session$sendCustomMessage("navigateTab", "Data Entry")
    })

    # --- Sidebar: Location section ---
    output$location_section <- renderUI({
      prefs <- user_prefs()
      has_location <- !is.null(prefs) && !is.null(prefs$home_state) && nzchar(prefs$home_state)
      if (has_location) {
        div(
          div(class = "d-flex align-items-center justify-content-between",
            tags$span(
              tags$i(class = "fa fa-map-marker-alt me-1", style = "color: #7A9A86;"),
              tags$strong(sprintf("%s, %s", prefs$home_city %||% "", prefs$home_state),
                          style = "font-size: 0.85rem;")
            ),
            actionLink(ns("change_location"), "Change",
                       style = "font-size: 0.75rem; color: #7A9A86;")
          )
        )
      } else {
        div(
          tags$label("Set Home Location", class = "form-label",
                     style = "font-size: 0.85rem; font-weight: 600;"),
          tags$small(class = "text-muted d-block mb-2",
                     "Enter your zip code for local wildlife data"),
          div(class = "d-flex gap-2",
            textInput(ns("garden_zipcode"), NULL, placeholder = "e.g., 55401",
                      width = "120px"),
            actionButton(ns("save_zipcode"), "Set",
                         class = "btn-sm btn-outline-primary",
                         style = "height: 38px;")
          )
        )
      }
    })

    observeEvent(input$change_location, {
      prefs <- user_prefs()
      output$location_section <- renderUI({
        div(
          tags$label("Update Location", class = "form-label",
                     style = "font-size: 0.85rem; font-weight: 600;"),
          div(class = "d-flex gap-2",
            textInput(ns("garden_zipcode"), NULL,
                      value = if (!is.null(prefs)) prefs$home_zipcode %||% "" else "",
                      placeholder = "e.g., 55401", width = "120px"),
            actionButton(ns("save_zipcode"), "Set",
                         class = "btn-sm btn-outline-primary",
                         style = "height: 38px;")
          )
        )
      })
    })

    observeEvent(input$save_zipcode, {
      zip <- input$garden_zipcode
      if (is.null(zip) || nchar(gsub("[^0-9]", "", zip)) != 5) {
        showNotification("Please enter a 5-digit zip code", type = "warning", duration = 3)
        return()
      }
      u <- current_user()
      if (is.null(u)) {
        showNotification("Sign in to save your location", type = "warning", duration = 3)
        return()
      }
      loc <- if (!is.null(zipcode_db)) lookup_zipcode(zip, zipcode_db) else NULL
      if (is.null(loc)) {
        showNotification("Zip code not found", type = "error", duration = 3)
        return()
      }
      success <- db_set_user_prefs(
        user_id = u$user_uid, zipcode = zip, city = loc$city, state = loc$state,
        lat = loc$latitude, lon = loc$longitude, pool = pool
      )
      if (success) {
        if (!is.null(prefs_changed)) prefs_changed(prefs_changed() + 1)
        showNotification(sprintf("Location set to %s, %s", loc$city, loc$state),
                         type = "message", duration = 3)
      } else {
        showNotification("Failed to save location", type = "error", duration = 3)
      }
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
    # Excludes introduced (non-invasive) species by name per product rule —
    # their interactions still contribute to the family count, but the species
    # itself shouldn't appear in the per-family plant list.
    get_plants_for_family <- function(family, coverage_df) {
      if (is.null(coverage_df) || nrow(coverage_df) == 0) return(character())
      fam_rows <- coverage_df[coverage_df$wildlife_family == family, ]
      plants <- unique(fam_rows$garden_species)
      # Drop any species classified as introduced (non-invasive). Invasives are
      # already excluded upstream in db_get_wildlife_coverage.
      hidden <- introduced_plants()$species
      plants <- setdiff(plants, hidden)
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
      eff_st <- effective_state()

      low_data_callout <- NULL
      if (length(sp) <= 2) {
        low_data_callout <- div(class = "alert alert-info mb-3",
          tags$i(class = "fa fa-lightbulb me-2"),
          "Add more plants to unlock better wildlife analysis."
        )
      }

      # State indicator
      state_indicator <- if (!is.null(eff_st) && nzchar(eff_st)) {
        div(class = "alert alert-success py-2 px-3 mb-3 d-flex align-items-center",
          style = "background: rgba(122,154,134,0.1); border-color: rgba(122,154,134,0.3); color: #373D3C;",
          tags$i(class = "fa fa-map-marker-alt me-2", style = "color: #7A9A86;"),
          tags$span(sprintf("Showing wildlife confirmed in %s", eff_st),
                    style = "font-size: 0.85rem;")
        )
      } else {
        NULL
      }

      # Calculate totals per type (already state-filtered by SQL)
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
        state_indicator,

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

        build_source_attribution(eff_st)
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
      eff_st <- effective_state()
      bfly_parts <- Filter(Negate(is.null), summary[c("Butterfly", "Skipper")])
      combined <- if (length(bfly_parts) > 0) do.call(rbind, bfly_parts) else data.frame()
      if (nrow(combined) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No butterfly data found for your garden plants."))
      }
      combined <- combined[order(-combined$species_covered), ]
      tagList(
        render_family_section(combined, "butterfly", ns, state_code = eff_st),
        build_source_attribution(eff_st)
      )
    })

    # --- Moths Tab ---
    output$moth_content <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) return(empty_garden_ui(ns, "moth"))

      summary <- coverage_summary()
      eff_st <- effective_state()
      moth_df <- summary[["Moth"]]
      if (is.null(moth_df) || nrow(moth_df) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No moth data found for your garden plants."))
      }
      moth_df <- moth_df[order(-moth_df$species_covered), ]
      tagList(
        render_family_section(moth_df, "moth", ns, state_code = eff_st),
        build_source_attribution(eff_st)
      )
    })

    # --- Specialist Bees Tab ---
    output$bee_content <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) return(empty_garden_ui(ns, "bee"))

      summary <- coverage_summary()
      eff_st <- effective_state()
      bee_df <- summary[["Bee"]]
      if (is.null(bee_df) || nrow(bee_df) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No specialist bee data found for your garden plants."))
      }
      bee_df <- bee_df[order(-bee_df$species_covered), ]
      tagList(
        render_family_section(bee_df, "bee", ns, state_code = eff_st),
        build_source_attribution(eff_st)
      )
    })

    # --- Birds Tab ---
    output$bird_content <- renderUI({
      sp <- garden_species()
      if (length(sp) == 0) return(empty_garden_ui(ns, "bird"))

      summary <- coverage_summary()
      eff_st <- effective_state()
      bird_df <- summary[["Bird"]]
      if (is.null(bird_df) || nrow(bird_df) == 0) {
        return(div(class = "text-muted text-center py-4",
                   "No bird data found for your garden plants."))
      }
      bird_df <- bird_df[order(-bird_df$species_covered), ]
      tagList(
        render_family_section(bird_df, "bird", ns, state_code = eff_st),
        build_source_attribution(eff_st)
      )
    })

    # --- Sidebar: Introduced + Invasive collapsible dropdowns ---
    output$nonnative_sections <- renderUI({
      if (length(garden_species()) == 0) return(NULL)
      intro_df <- introduced_plants()
      inv_df   <- invasive_plants()
      if (nrow(intro_df) == 0 && nrow(inv_df) == 0) return(NULL)

      tagList(
        if (nrow(inv_df) > 0) render_sidebar_dropdown("invasive", inv_df, ns),
        if (nrow(intro_df) > 0) render_sidebar_dropdown("introduced", intro_df, ns)
      )
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

      prefs <- user_prefs()
      user_state <- if (!is.null(prefs)) prefs$home_state else NULL

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

          # Pre-fetch native species for this genus (rendered as static HTML)
          species_panel_id <- paste0("gap_native_", gsub("[^a-zA-Z]", "", genus), "_", i)
          species_df <- if (!is.null(user_state) && nzchar(user_state)) {
            db_get_native_species_for_genus(genus, user_state, pool)
          } else {
            data.frame()
          }

          # When only 1 native species in user's state, show that species' name
          # instead of the genus common name (e.g., "Bearberry" not "Manzanitas")
          if (nrow(species_df) == 1 && !is.na(species_df$common_name[1]) &&
              nzchar(species_df$common_name[1])) {
            genus_common <- tools::toTitleCase(tolower(species_df$common_name[1]))
          }

          native_content <- if (!is.null(user_state) && nzchar(user_state)) {
            if (nrow(species_df) > 0) {
              tagList(
                tags$small(class = "text-muted d-block mb-2",
                           tags$strong(sprintf("Native %s in %s", genus, user_state)),
                           sprintf(" \u2014 %d species", nrow(species_df))),
                div(class = "row g-1",
                  lapply(seq_len(nrow(species_df)), function(j) {
                    sp <- species_df[j, ]
                    cn <- if (!is.na(sp$common_name) && nzchar(sp$common_name)) {
                      tools::toTitleCase(tolower(sp$common_name))
                    } else NULL
                    div(class = "col-md-6",
                      div(class = "d-flex align-items-baseline py-1 px-2",
                          style = "border-bottom: 1px solid #f0ede3; font-size: 0.85rem;",
                        if (!is.null(cn)) {
                          tagList(
                            tags$span(style = "color: #5D7A6A; font-weight: 500;", cn),
                            tags$span(class = "species-name text-muted ms-2",
                                      style = "font-size: 0.78rem;", sp$species_name)
                          )
                        } else {
                          tags$span(class = "species-name",
                                    style = "font-size: 0.78rem; color: #5D7A6A;",
                                    sp$species_name)
                        }
                      )
                    )
                  })
                )
              )
            } else {
              div(class = "text-muted small py-1",
                  sprintf("No %s species recorded as native to %s.", genus, user_state))
            }
          } else {
            div(class = "text-muted small py-1",
                tags$i(class = "fa fa-map-pin me-1"),
                "Set your home location to see native species.")
          }

          card(class = "mb-2",
            card_body(class = "py-2 px-3",
              # Main row — clickable to expand
              tags$div(
                class = "d-flex justify-content-between align-items-start",
                style = "cursor: pointer;",
                onclick = sprintf("
                  var panel = document.getElementById('%s');
                  var arrow = document.getElementById('%s_arrow');
                  if (panel.style.display === 'none') {
                    panel.style.display = 'block';
                    arrow.classList.remove('fa-chevron-down');
                    arrow.classList.add('fa-chevron-up');
                  } else {
                    panel.style.display = 'none';
                    arrow.classList.remove('fa-chevron-up');
                    arrow.classList.add('fa-chevron-down');
                  }
                ", species_panel_id, species_panel_id),
                div(style = "flex: 1;",
                  div(class = "d-flex align-items-center gap-2 flex-wrap mb-1",
                    tags$i(id = paste0(species_panel_id, "_arrow"),
                           class = "fa fa-chevron-down",
                           style = "font-size: 0.7rem; color: #7A9A86; transition: transform 0.2s;"),
                    if (!is.null(genus_common)) {
                      tags$strong(style = "font-size: 1.05rem;", genus_common)
                    } else {
                      tags$strong(class = "species-name",
                                  tags$em(genus), " species")
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
              ),

              # Expandable native species panel (pre-rendered, hidden)
              div(id = species_panel_id, style = "display: none;",
                div(class = "mt-2 pt-2 border-top", native_content)
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
  Sporobolus = "Dropseed Grasses",
  Campsis = "Trumpet Creepers",
  Clematis = "Clematis",
  Rhododendron = "Rhododendrons & Azaleas",
  Wisteria = "Wisterias",
  Celastrus = "Bittersweets",
  Parthenocissus = "Virginia Creepers",
  Lonicera = "Honeysuckles",
  Tsuga = "Hemlocks",
  Ulmus = "Elms",
  Diospyros = "Persimmons"
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

  # Strategy 2: If no genus-level entry found, extract shared words across species
  if (is.null(best_name) && length(genus_matches) > 0) {
    valid <- genus_matches[!is.na(common_name_db$common_name[genus_matches]) &
                           nzchar(common_name_db$common_name[genus_matches])]
    if (length(valid) > 0) {
      names <- tolower(common_name_db$common_name[valid])
      if (length(names) >= 2) {
        word_lists <- strsplit(names, "\\s+")
        shared <- Reduce(intersect, word_lists)
        shared <- shared[nchar(shared) >= 3]
        if (length(shared) > 0) {
          best_name <- paste(shared, collapse = " ")
        }
      }
      if (is.null(best_name)) {
        last_words <- sapply(strsplit(names, "\\s+"), tail, 1)
        word_freq <- table(last_words)
        most_common <- names(which.max(word_freq))
        if (word_freq[[most_common]] >= max(length(names) * 0.5, 1)) {
          best_name <- most_common
        } else {
          best_name <- names[which.min(nchar(names))]
        }
      }
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

#' Render a sidebar dropdown for non-native plants ('introduced' or 'invasive').
#' Sized and styled to fit alongside the existing genus-grouped species list.
#' @param kind 'introduced' or 'invasive' — drives header label, color, and footer copy.
#' @param df Data frame from `introduced_plants()` / `invasive_plants()`.
#' @param ns Namespace function.
render_sidebar_dropdown <- function(kind, df, ns) {
  is_invasive <- identical(kind, "invasive")
  group_id <- paste0("nonnative_", kind, "_group")
  header_color <- if (is_invasive) "#b02a37" else "#7A6A55"
  header_icon <- if (is_invasive) "exclamation-triangle" else "plane-arrival"
  header_label <- if (is_invasive) "Invasive" else "Introduced"

  rows <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    cn <- if (!is.na(row$common_name)) row$common_name else NULL
    state_url <- get_state_invasive_url(row$state_code)

    label_block <- if (!is.null(cn)) {
      tagList(
        tags$span(style = "font-size: 0.85rem;", cn),
        tags$br(),
        tags$small(class = "species-name text-muted", row$species)
      )
    } else {
      tags$span(class = "species-name", style = "font-size: 0.85rem;", row$species)
    }

    detail_block <- if (is_invasive) {
      # Invasive: show designation + removal-info link, no wildlife counts
      designation <- if (isTRUE(row$is_federal)) "Federal Noxious Weed"
                     else (row$invasive_designation %||% "Invasive")
      tagList(
        tags$small(class = "d-block mt-1", style = "color: #b02a37;",
          icon("ban", class = "me-1"), designation),
        if (!is.null(state_url)) {
          tags$small(class = "d-block",
            tags$a(href = state_url, target = "_blank",
              style = "color: #b02a37; text-decoration: none; font-size: 0.7rem;",
              icon("book-open", class = "me-1"),
              "Removal & management",
              icon("up-right-from-square", class = "ms-1",
                   style = "font-size: 0.6rem;"))
          )
        }
      )
    } else {
      # Introduced (non-invasive): raw lep / bee / bird counts under the species name
      parts <- c()
      if (row$lep_count > 0)  parts <- c(parts, sprintf("%d lep", row$lep_count))
      if (row$bee_count > 0)  parts <- c(parts, sprintf("%d bee", row$bee_count))
      if (row$bird_count > 0) parts <- c(parts, sprintf("%d bird", row$bird_count))
      label <- if (length(parts) > 0) {
        sprintf("supports %s species", paste(parts, collapse = ", "))
      } else {
        "no documented species-level wildlife"
      }
      tags$small(class = "d-block text-muted mt-1",
        style = "font-size: 0.7rem;",
        icon("paw", class = "me-1", style = "color: #7A9A86;"),
        label)
    }

    div(class = "py-1 px-1",
        style = "font-size: 0.8rem; border-bottom: 1px solid #f5f2e9;",
      label_block,
      detail_block
    )
  })

  div(class = "border-bottom",
    div(class = "py-1 d-flex align-items-center",
        style = sprintf("cursor: pointer; color: %s;", header_color),
        onclick = sprintf(
          "var el=document.getElementById('%s'); var arr=document.getElementById('%s_arr');
           if(el.style.display==='none'){el.style.display='block';arr.className='fa fa-chevron-up';}
           else{el.style.display='none';arr.className='fa fa-chevron-down';}", group_id, group_id),
      tags$i(id = paste0(group_id, "_arr"), class = "fa fa-chevron-down",
             style = sprintf("font-size: 0.6rem; color: %s; width: 12px;", header_color)),
      tags$i(class = paste0("fa fa-", header_icon, " ms-1 me-1"),
             style = sprintf("font-size: 0.7rem; color: %s;", header_color)),
      tags$span(style = "font-size: 0.85rem; font-weight: 500;",
                header_label),
      tags$span(class = "badge rounded-pill ms-auto",
                style = sprintf("font-size: 0.7rem; background-color: %s; color: #FFFFFF;", header_color),
                nrow(df))
    ),
    div(id = group_id, style = "display: none; padding-left: 16px;",
      rows
    )
  )
}

#' Render a family section with summary donut and per-family grid
render_family_section <- function(summary_df, prefix, ns, state_code = NULL) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(div(class = "text-muted text-center py-4", "No data available"))
  }

  total_covered <- sum(summary_df$species_covered)
  total_all <- sum(summary_df$total_species)
  has_state <- !is.null(state_code) && nzchar(state_code)

  summary_label <- if (has_state) {
    sprintf("%d of %d species in %s supported by your garden", total_covered, total_all, state_code)
  } else {
    sprintf("%d of %d species supported", total_covered, total_all)
  }

  tagList(
    # Summary donut
    div(class = "text-center mb-4",
      div(style = "max-width: 250px; margin: 0 auto;",
        plotlyOutput(ns(paste0(prefix, "_summary_donut")),
                     height = "220px", width = "100%")
      ),
      h6(class = "mt-2", style = "font-family: 'Montserrat', sans-serif;",
         summary_label)
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
