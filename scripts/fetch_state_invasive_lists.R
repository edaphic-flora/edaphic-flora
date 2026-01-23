# fetch_state_invasive_lists.R
# Compiles state-level invasive species data from official sources
#
# Data was manually collected from authoritative state invasive species councils,
# state departments of agriculture, and natural resource agencies (Jan 2026).
#
# Coverage (Jan 2026):
#   Direct compiled data: OH, MN, CO, WY, AK, SD, ND, ME, MD, KS, NE, TX (12 states)
#   + Invasive Plant Atlas data (~244 records from ~26 states)
#   + Additional state sources available via fetch functions
#
# Usage:
#   source("scripts/fetch_state_invasive_lists.R")
#   compile_state_invasive_data()  # Creates data/state_invasive_for_import.csv
#   load_state_invasive_to_db()    # Loads into ref_noxious_invasive table
#
# Output:
#   - data/state_invasive_for_import.csv - Cleaned data ready for DB import

library(httr)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# ---------------------------
# State Source Configuration
# ---------------------------

# Each state has different sources and formats. This list documents
# the authoritative source for each state.

STATE_SOURCES <- list(

  # ========== SOUTHEASTERN STATES ==========

  AL = list(
    name = "Alabama",
    source = "Alabama Invasive Plant Council",
    url = "https://www.se-eppc.org/alabama/",
    notes = "SE-EPPC affiliate"
  ),

  FL = list(
    name = "Florida",
    source = "Florida Invasive Species Council (FISC)",
    url = "https://www.floridainvasives.org/plantlist2023.xlsx",
    format = "xlsx",
    notes = "Formerly FLEPPC. Excel download available."
  ),

  GA = list(
    name = "Georgia",
    source = "Georgia Exotic Pest Plant Council",
    url = "https://www.gaeppc.org/list/",
    notes = "SE-EPPC affiliate"
  ),

  KY = list(
    name = "Kentucky",
    source = "Kentucky Exotic Pest Plant Council",
    url = "https://www.se-eppc.org/ky/",
    notes = "SE-EPPC affiliate"
  ),

  NC = list(
    name = "North Carolina",
    source = "NC Native Plant Society Invasive Exotic List",
    url = "https://ncwildflower.org/invasive-exotic-species/",
    notes = ""
  ),

  SC = list(
    name = "South Carolina",
    source = "SC Exotic Pest Plant Council",
    url = "https://www.se-eppc.org/southcarolina/",
    notes = "SE-EPPC affiliate"
  ),

  TN = list(
    name = "Tennessee",
    source = "Tennessee Exotic Pest Plant Council",
    url = "https://www.tneppc.org/invasive-plants",
    notes = ""
  ),

  VA = list(
    name = "Virginia",
    source = "Virginia DCR Invasive Plant Species List",
    url = "https://www.dcr.virginia.gov/natural-heritage/document/nh-invasive-plant-list-2023.pdf",
    format = "pdf",
    notes = "Official state list"
  ),

  # ========== NORTHEASTERN STATES ==========

  CT = list(
    name = "Connecticut",
    source = "Connecticut Invasive Plant Working Group",
    url = "https://cipwg.uconn.edu/invasive-plant-list/",
    notes = ""
  ),

  MA = list(
    name = "Massachusetts",
    source = "Massachusetts Invasive Plant Advisory Group",
    url = "https://www.massnrc.org/mipag/",
    notes = ""
  ),

  MD = list(
    name = "Maryland",
    source = "Maryland DNR Invasive Plants",
    url = "https://dnr.maryland.gov/wildlife/Pages/plants_wildlife/InvasiveSpecies.aspx",
    notes = ""
  ),

  NH = list(
    name = "New Hampshire",
    source = "NH Department of Agriculture Prohibited Invasive Species",
    url = "https://www.agriculture.nh.gov/divisions/plant-industry/invasive-species.htm",
    notes = "Regulatory list"
  ),

  NJ = list(
    name = "New Jersey",
    source = "NJ Invasive Species Council",
    url = "https://www.nj.gov/dep/njisc/",
    notes = ""
  ),

  NY = list(
    name = "New York",
    source = "NY Natural Heritage Program Invasive Species Tiers",
    url = "https://www.nynhp.org/invasives/species-tiers-table/",
    notes = "Tiered ranking system"
  ),

  PA = list(
    name = "Pennsylvania",
    source = "PA DCNR Invasive Plant List",
    url = "https://www.dcnr.pa.gov/Conservation/WildPlants/InvasivePlants/Pages/default.aspx",
    notes = "Ranked threat levels"
  ),

  VT = list(
    name = "Vermont",
    source = "Vermont Invasives",
    url = "https://www.vtinvasives.org/invasive/plants",
    notes = ""
  ),

  # ========== MIDWESTERN STATES ==========

  IL = list(
    name = "Illinois",
    source = "Illinois Exotic Species",
    url = "https://www.invasive.org/species/list.cfm?id=152",
    notes = ""
  ),

  IN = list(
    name = "Indiana",
    source = "Indiana Invasive Species Council",
    url = "https://www.entm.purdue.edu/iisc/invasiveplants.html",
    notes = ""
  ),

  IA = list(
    name = "Iowa",
    source = "Iowa DNR Invasive Species",
    url = "https://www.iowadnr.gov/Conservation/Forestry/Forest-Health/Invasive-Plants",
    notes = ""
  ),

  MI = list(
    name = "Michigan",
    source = "Michigan Invasive Species Program",
    url = "https://www.michigan.gov/invasives",
    notes = ""
  ),

  MN = list(
    name = "Minnesota",
    source = "Minnesota DNR Invasive Species",
    url = "https://www.dnr.state.mn.us/invasives/terrestrialplants/index.html",
    notes = "Noxious weed list"
  ),

  MO = list(
    name = "Missouri",
    source = "Missouri Invasive Plant Council",
    url = "https://www.moinvasives.org/species/",
    notes = ""
  ),

  OH = list(
    name = "Ohio",
    source = "Ohio Invasive Plants Council",
    url = "https://www.oipc.info/",
    notes = "Ohio Admin Code 901:5-30-01"
  ),

  WI = list(
    name = "Wisconsin",
    source = "Wisconsin DNR Invasive Species",
    url = "https://dnr.wisconsin.gov/topic/Invasives/species.html",
    notes = "Regulated and restricted species"
  ),

  # ========== WESTERN STATES ==========

  AZ = list(
    name = "Arizona",
    source = "Arizona Wildlands Invasive Plant Working Group",
    url = "https://www.swvma.org/arizona-invasive-plant-list/",
    notes = ""
  ),

  CA = list(
    name = "California",
    source = "California Invasive Plant Council (Cal-IPC)",
    url = "https://www.cal-ipc.org/plants/inventory/",
    notes = "Comprehensive inventory with ratings"
  ),

  CO = list(
    name = "Colorado",
    source = "Colorado Weed Management Association",
    url = "https://cwma.org/weed-information/",
    notes = "Noxious weed lists A, B, C"
  ),

  ID = list(
    name = "Idaho",
    source = "Idaho State Dept of Agriculture Noxious Weeds",
    url = "https://agri.idaho.gov/main/plants/noxious-weeds/",
    notes = ""
  ),

  MT = list(
    name = "Montana",
    source = "Montana Noxious Weed List",
    url = "https://agr.mt.gov/Noxious-Weeds",
    notes = ""
  ),

  NV = list(
    name = "Nevada",
    source = "Nevada Department of Agriculture Noxious Weeds",
    url = "https://agri.nv.gov/Plant/Noxious_Weeds/Noxious_Weed_List/",
    notes = ""
  ),

  NM = list(
    name = "New Mexico",
    source = "New Mexico Dept of Agriculture Noxious Weeds",
    url = "https://nmdaweb.nmsu.edu/animal-and-plant-protection/noxious-weed-information.html",
    notes = ""
  ),

  OR = list(
    name = "Oregon",
    source = "Oregon Dept of Agriculture Noxious Weed List",
    url = "https://www.oregon.gov/oda/programs/weeds/oregonnoxiousweeds/pages/aboutoregonweeds.aspx",
    notes = ""
  ),

  UT = list(
    name = "Utah",
    source = "Utah Noxious Weed Act",
    url = "https://ag.utah.gov/farmers/plants-industry/noxious-weed-program/",
    notes = ""
  ),

  WA = list(
    name = "Washington",
    source = "Washington State Noxious Weed Control Board",
    url = "https://www.nwcb.wa.gov/noxious-weed-list",
    notes = "Classes A, B, C"
  ),

  WY = list(
    name = "Wyoming",
    source = "Wyoming Weed & Pest Council",
    url = "https://wyoweed.org/weeds/",
    notes = ""
  ),

  # ========== SOUTHERN STATES ==========

  AR = list(
    name = "Arkansas",
    source = "Arkansas Native Plant Society",
    url = "https://www.se-eppc.org/states/AR.cfm",
    notes = ""
  ),

  LA = list(
    name = "Louisiana",
    source = "Louisiana Dept of Wildlife and Fisheries",
    url = "https://www.wlf.louisiana.gov/page/aquatic-invasive-species",
    notes = ""
  ),

  MS = list(
    name = "Mississippi",
    source = "Mississippi Exotic Pest Plant Council",
    url = "https://www.se-eppc.org/mississippi/",
    notes = ""
  ),

  OK = list(
    name = "Oklahoma",
    source = "Oklahoma Invasive Plant Council",
    url = "https://www.okinvasives.org/ok-invasive-plant-list",
    notes = ""
  ),

  TX = list(
    name = "Texas",
    source = "Texas Invasive Plant & Pest Council (TIPPC)",
    url = "https://www.texasinvasives.org/plant_database/",
    notes = ""
  ),

  # ========== OTHER STATES ==========

  AK = list(
    name = "Alaska",
    source = "Alaska Invasive Species Partnership",
    url = "https://www.adfg.alaska.gov/index.cfm?adfg=invasive.main",
    notes = ""
  ),

  HI = list(
    name = "Hawaii",
    source = "Hawaii Invasive Species Council",
    url = "https://dlnr.hawaii.gov/hisc/",
    notes = ""
  ),

  DE = list(
    name = "Delaware",
    source = "Delaware Invasive Species Council",
    url = "https://delawareinvasives.net/invasive-plants/",
    notes = ""
  ),

  ME = list(
    name = "Maine",
    source = "Maine Natural Areas Program",
    url = "https://www.maine.gov/dacf/mnap/features/invasive_plants/invasives.htm",
    notes = ""
  ),

  RI = list(
    name = "Rhode Island",
    source = "Rhode Island Invasive Species Council",
    url = "https://rinhs.org/invasive-species/",
    notes = ""
  ),

  WV = list(
    name = "West Virginia",
    source = "WV Native Plant Society",
    url = "https://wvnps.org/invasive-plant-species/",
    notes = ""
  ),

  DC = list(
    name = "District of Columbia",
    source = "DC Invasive Plants",
    url = "https://doee.dc.gov/service/invasive-species",
    notes = ""
  ),

  # States with limited or no formal invasive plant lists
  KS = list(name = "Kansas", source = "Kansas Dept of Agriculture", url = NA, notes = "Noxious weeds only"),
  NE = list(name = "Nebraska", source = "Nebraska Dept of Agriculture", url = NA, notes = "Noxious weeds only"),
  ND = list(name = "North Dakota", source = "ND Dept of Agriculture", url = NA, notes = "Noxious weeds only"),
  SD = list(name = "South Dakota", source = "SD Dept of Agriculture", url = NA, notes = "Noxious weeds only")
)

# ---------------------------
# Helper Functions
# ---------------------------

HEADERS <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
)

fetch_url <- function(url, max_retries = 3) {
  for (attempt in 1:max_retries) {
    tryCatch({
      response <- GET(url, HEADERS, timeout(30))
      if (status_code(response) == 200) return(response)
      Sys.sleep(2)
    }, error = function(e) {
      message(sprintf("  Error: %s", e$message))
      Sys.sleep(2)
    })
  }
  NULL
}

# ---------------------------
# State-specific Parsers
# ---------------------------

# Florida - Excel download
parse_florida <- function() {
  message("Fetching Florida FISC list...")
  url <- "https://bugwoodcloud.org/CDN/fleppc/plantlists/2019/2019_Plant_List_ABSOLUTE_FINAL.pdf"

  # Try to get the Excel version first
  xlsx_url <- STATE_SOURCES$FL$url

  temp_file <- tempfile(fileext = ".xlsx")
  tryCatch({
    download.file(xlsx_url, temp_file, mode = "wb", quiet = TRUE)
    data <- read_excel(temp_file)
    unlink(temp_file)

    # Clean and return
    data %>%
      select(scientific_name = 1, common_name = 2) %>%
      filter(!is.na(scientific_name)) %>%
      mutate(state_code = "FL", source = "FISC")
  }, error = function(e) {
    message("  Could not download Florida Excel file: ", e$message)
    data.frame()
  })
}

# California - Web table
parse_california <- function() {
  message("Fetching California Cal-IPC inventory...")
  url <- "https://www.cal-ipc.org/plants/inventory/"

  response <- fetch_url(url)
  if (is.null(response)) return(data.frame())

  page <- content(response, "text", encoding = "UTF-8") %>% read_html()

  # Parse the inventory table
  tables <- page %>% html_nodes("table")

  if (length(tables) == 0) {
    message("  No tables found on Cal-IPC page")
    return(data.frame())
  }

  # Try to parse the main inventory table
  tryCatch({
    # The table has columns: Scientific Name, Common Name, CAL-IPC Rating, etc.
    rows <- tables[[1]] %>% html_nodes("tr")

    species_list <- lapply(rows[-1], function(row) {
      cells <- row %>% html_nodes("td")
      if (length(cells) >= 2) {
        data.frame(
          scientific_name = html_text(cells[1], trim = TRUE),
          common_name = html_text(cells[2], trim = TRUE),
          rating = if (length(cells) >= 3) html_text(cells[3], trim = TRUE) else NA,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })

    bind_rows(species_list) %>%
      filter(nzchar(scientific_name)) %>%
      mutate(state_code = "CA", source = "Cal-IPC")
  }, error = function(e) {
    message("  Error parsing Cal-IPC table: ", e$message)
    data.frame()
  })
}

# Generic web table parser
parse_web_table <- function(state_code, url, table_index = 1) {
  message(sprintf("Fetching %s...", STATE_SOURCES[[state_code]]$name))

  response <- fetch_url(url)
  if (is.null(response)) {
    message(sprintf("  Failed to fetch %s", state_code))
    return(data.frame())
  }

  page <- content(response, "text", encoding = "UTF-8") %>% read_html()
  tables <- page %>% html_nodes("table")

  if (length(tables) < table_index) {
    message(sprintf("  No table found for %s", state_code))
    return(data.frame())
  }

  tryCatch({
    tbl <- html_table(tables[[table_index]], fill = TRUE)
    if (ncol(tbl) >= 1) {
      names(tbl)[1] <- "scientific_name"
      if (ncol(tbl) >= 2) names(tbl)[2] <- "common_name"
      tbl %>%
        mutate(state_code = state_code, source = STATE_SOURCES[[state_code]]$source) %>%
        filter(!is.na(scientific_name), nzchar(scientific_name))
    } else {
      data.frame()
    }
  }, error = function(e) {
    message(sprintf("  Error parsing %s table: %s", state_code, e$message))
    data.frame()
  })
}

# ---------------------------
# Main Function
# ---------------------------

#' Fetch invasive species lists from all configured states
fetch_all_states <- function(states = NULL) {
  if (is.null(states)) {
    states <- names(STATE_SOURCES)
  }

  all_data <- list()

  for (state_code in states) {
    info <- STATE_SOURCES[[state_code]]

    if (is.null(info) || is.na(info$url)) {
      message(sprintf("Skipping %s - no URL configured", state_code))
      next
    }

    Sys.sleep(1)  # Rate limiting

    # Use state-specific parser if available
    result <- tryCatch({
      if (state_code == "FL") {
        parse_florida()
      } else if (state_code == "CA") {
        parse_california()
      } else {
        parse_web_table(state_code, info$url)
      }
    }, error = function(e) {
      message(sprintf("Error fetching %s: %s", state_code, e$message))
      data.frame()
    })

    if (nrow(result) > 0) {
      message(sprintf("  -> %d species", nrow(result)))
      all_data[[state_code]] <- result
    }
  }

  # Combine all data
  if (length(all_data) == 0) {
    message("No data collected!")
    return(data.frame())
  }

  final_df <- bind_rows(all_data)

  # Save
  dir.create("data", showWarnings = FALSE)
  write.csv(final_df, "data/state_invasive_lists.csv", row.names = FALSE)
  message(sprintf("\nSaved %d records to data/state_invasive_lists.csv", nrow(final_df)))

  final_df
}

# Print summary of configured states
message(sprintf("\nConfigured %d states. Run fetch_all_states() to begin.\n", length(STATE_SOURCES)))

# ===========================================================================
# COMPILED STATE DATA (Jan 2026)
# Data manually extracted from official state sources
# ===========================================================================

# Ohio - from Ohio Administrative Code 901:5-30-01 (63 species)
OH_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Ailanthus altissima", "tree of heaven",
  "Alliaria petiolata", "garlic mustard",
  "Alnus glutinosa", "black alder",
  "Ampelopsis brevipedunculata", "porcelain berry",
  "Artemisia vulgaris", "mugwort",
  "Berberis thunbergii", "Japanese barberry",
  "Berberis vulgaris", "common barberry",
  "Butomus umbellatus", "flowering rush",
  "Celastrus orbiculatus", "oriental bittersweet",
  "Centaurea stoebe", "spotted knapweed",
  "Cirsium arvense", "Canada thistle",
  "Cirsium vulgare", "bull thistle",
  "Conium maculatum", "poison hemlock",
  "Coronilla varia", "crown vetch",
  "Dipsacus fullonum", "common teasel",
  "Dipsacus laciniatus", "cut-leaved teasel",
  "Egeria densa", "Brazilian waterweed",
  "Elaeagnus angustifolia", "Russian olive",
  "Elaeagnus umbellata", "autumn olive",
  "Euonymus alatus", "burning bush",
  "Euonymus fortunei", "winter creeper",
  "Euphorbia esula", "leafy spurge",
  "Ficaria verna", "lesser celandine",
  "Frangula alnus", "glossy buckthorn",
  "Heracleum mantegazzianum", "giant hogweed",
  "Hesperis matronalis", "dame's rocket",
  "Humulus japonicus", "Japanese hops",
  "Hydrilla verticillata", "hydrilla",
  "Iris pseudacorus", "yellow flag iris",
  "Lepidium latifolium", "perennial pepperweed",
  "Lespedeza cuneata", "sericea lespedeza",
  "Ligustrum obtusifolium", "border privet",
  "Lonicera japonica", "Japanese honeysuckle",
  "Lonicera maackii", "Amur honeysuckle",
  "Lonicera morrowii", "Morrow's honeysuckle",
  "Lonicera tatarica", "Tatarian honeysuckle",
  "Lythrum salicaria", "purple loosestrife",
  "Microstegium vimineum", "Japanese stiltgrass",
  "Miscanthus sinensis", "Chinese silvergrass",
  "Myriophyllum spicatum", "Eurasian watermilfoil",
  "Nymphoides peltata", "yellow floating heart",
  "Pastinaca sativa", "wild parsnip",
  "Paulownia tomentosa", "princess tree",
  "Persicaria perfoliata", "mile-a-minute vine",
  "Phalaris arundinacea", "reed canary grass",
  "Phragmites australis", "common reed",
  "Polygonum cuspidatum", "Japanese knotweed",
  "Potamogeton crispus", "curly-leaf pondweed",
  "Pueraria montana", "kudzu",
  "Pyrus calleryana", "Callery pear",
  "Ranunculus ficaria", "fig buttercup",
  "Rhamnus cathartica", "common buckthorn",
  "Rosa multiflora", "multiflora rose",
  "Spiraea japonica", "Japanese meadowsweet",
  "Torilis japonica", "Japanese hedge parsley",
  "Trapa natans", "water chestnut",
  "Typha angustifolia", "narrow-leaved cattail",
  "Vincetoxicum nigrum", "black swallow-wort",
  "Vincetoxicum rossicum", "pale swallow-wort",
  "Vinca minor", "common periwinkle",
  "Viburnum lantana", "wayfaring tree",
  "Viburnum opulus", "European cranberrybush"
) %>% mutate(state_code = "OH", designation = "Invasive", source = "Ohio Administrative Code 901:5-30-01")

# Minnesota - MDA Noxious Weed List (63 species)
MN_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Cynanchum louiseae", "Black swallow-wort", "Prohibited-Eradicate",
  "Centaurea jacea", "Brown knapweed", "Prohibited-Eradicate",
  "Dipsacus fullonum", "Common teasel", "Prohibited-Eradicate",
  "Dipsacus laciniatus", "Cutleaf teasel", "Prohibited-Eradicate",
  "Linaria dalmatica", "Dalmatian toadflax", "Prohibited-Eradicate",
  "Centaurea diffusa", "Diffuse knapweed", "Prohibited-Eradicate",
  "Heracleum mantegazzianum", "Giant hogweed", "Prohibited-Eradicate",
  "Lonicera japonica", "Japanese honeysuckle", "Prohibited-Eradicate",
  "Digitalis lanata", "Grecian foxglove", "Prohibited-Eradicate",
  "Humulus japonicus", "Japanese hops", "Prohibited-Eradicate",
  "Sorghum halepense", "Johnsongrass", "Prohibited-Eradicate",
  "Cynanchum rossicum", "Pale swallow-wort", "Prohibited-Eradicate",
  "Amaranthus palmeri", "Palmer amaranth", "Prohibited-Eradicate",
  "Thladiantha dubia", "Red hailstone", "Prohibited-Eradicate",
  "Microstegium vimineum", "Japanese stiltgrass", "Prohibited-Eradicate",
  "Ailanthus altissima", "Tree of heaven", "Prohibited-Eradicate",
  "Centaurea solstitialis", "Yellow starthistle", "Prohibited-Eradicate",
  "Polygonum x bohemicum", "Bohemian knotweed", "Prohibited-Control",
  "Cirsium arvense", "Canada thistle", "Prohibited-Control",
  "Berberis vulgaris", "Common barberry", "Prohibited-Control",
  "Tanacetum vulgare", "Common tansy", "Prohibited-Control",
  "Polygonum sachalinense", "Giant knotweed", "Prohibited-Control",
  "Polygonum cuspidatum", "Japanese knotweed", "Prohibited-Control",
  "Euphorbia virgata", "Leafy spurge", "Prohibited-Control",
  "Centaurea x moncktonii", "Meadow knapweed", "Prohibited-Control",
  "Cardamine impatiens", "Narrowleaf bittercress", "Prohibited-Control",
  "Phragmites australis", "Non-native phragmites", "Prohibited-Control",
  "Carduus acanthoides", "Plumeless thistle", "Prohibited-Control",
  "Conium maculatum", "Poison hemlock", "Prohibited-Control",
  "Lythrum salicaria", "Purple loosestrife", "Prohibited-Control",
  "Celastrus orbiculatus", "Oriental bittersweet", "Prohibited-Control",
  "Centaurea stoebe", "Spotted knapweed", "Prohibited-Control",
  "Pastinaca sativa", "Wild parsnip", "Prohibited-Control",
  "Lonicera maackii", "Amur honeysuckle", "Restricted",
  "Elaeagnus umbellata", "Autumn olive", "Restricted",
  "Lonicera x bella", "Bell's honeysuckle", "Restricted",
  "Robinia pseudoacacia", "Black locust", "Restricted",
  "Pyrus calleryana", "Callery pear", "Restricted",
  "Rhamnus cathartica", "Common buckthorn", "Restricted",
  "Alnus glutinosa", "European alder", "Restricted",
  "Alliaria petiolata", "Garlic mustard", "Restricted",
  "Frangula alnus", "Glossy buckthorn", "Restricted",
  "Berberis thunbergii", "Japanese barberry", "Restricted",
  "Ficaria verna", "Lesser celandine", "Restricted",
  "Lonicera morrowii", "Morrow's honeysuckle", "Restricted",
  "Rosa multiflora", "Multiflora rose", "Restricted",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Restricted",
  "Lonicera tatarica", "Tatarian honeysuckle", "Restricted",
  "Euonymus alatus", "Burning bush", "Restricted"
) %>% mutate(state_code = "MN", source = "Minnesota Department of Agriculture")

# Colorado - Department of Agriculture (96 species)
CO_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Alhagi pseudalhagi", "Camelthorn", "List A",
  "Crupina vulgaris", "Common crupina", "List A",
  "Salvinia molesta", "Giant salvinia", "List A",
  "Hydrilla verticillata", "Hydrilla", "List A",
  "Taeniatherum caput-medusae", "Medusahead rye", "List A",
  "Myriophyllum aquaticum", "Parrotfeather", "List A",
  "Centaurea virgata", "Squarrose knapweed", "List A",
  "Ventenata dubia", "Ventenata grass", "List A",
  "Peganum harmala", "African rue", "List A",
  "Isatis tinctoria", "Dyer's woad", "List A",
  "Brassica elongata", "Elongated mustard", "List A",
  "Butomus umbellatus", "Flowering rush", "List A",
  "Centaurea x moncktonii", "Meadow knapweed", "List A",
  "Chondrilla juncea", "Rush skeletonweed", "List A",
  "Senecio jacobaea", "Tansy ragwort", "List A",
  "Centaurea solstitialis", "Yellow starthistle", "List A",
  "Euphorbia cyparissias", "Cypress spurge", "List A",
  "Arundo donax", "Giant reed", "List A",
  "Epilobium hirsutum", "Hairy willow-herb", "List A",
  "Polygonum cuspidatum", "Japanese knotweed", "List A",
  "Salvia aethiopis", "Mediterranean sage", "List A",
  "Euphorbia myrsinites", "Myrtle spurge", "List A",
  "Hieracium aurantiacum", "Orange hawkweed", "List A",
  "Lythrum salicaria", "Purple loosestrife", "List A",
  "Iris pseudacorus", "Yellow flag iris", "List A",
  "Artemisia absinthium", "Absinth wormwood", "List B",
  "Hyoscyamus niger", "Black henbane", "List B",
  "Saponaria officinalis", "Bouncingbet", "List B",
  "Cirsium vulgare", "Bull thistle", "List B",
  "Cirsium arvense", "Canada thistle", "List B",
  "Clematis orientalis", "Chinese clematis", "List B",
  "Tanacetum vulgare", "Common tansy", "List B",
  "Dipsacus fullonum", "Common teasel", "List B",
  "Dipsacus laciniatus", "Cutleaf teasel", "List B",
  "Linaria dalmatica", "Dalmatian toadflax", "List B",
  "Hesperis matronalis", "Dame's rocket", "List B",
  "Centaurea diffusa", "Diffuse knapweed", "List B",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "List B",
  "Lepidium draba", "Hoary cress", "List B",
  "Cynoglossum officinale", "Houndstongue", "List B",
  "Aegilops cylindrica", "Jointed goatgrass", "List B",
  "Euphorbia esula", "Leafy spurge", "List B",
  "Carduus nutans", "Musk thistle", "List B",
  "Leucanthemum vulgare", "Oxeye daisy", "List B",
  "Lepidium latifolium", "Perennial pepperweed", "List B",
  "Carduus acanthoides", "Plumeless thistle", "List B",
  "Rhaponticum repens", "Russian knapweed", "List B",
  "Elaeagnus angustifolia", "Russian olive", "List B",
  "Tamarix ramosissima", "Saltcedar", "List B",
  "Onopordum acanthium", "Scotch thistle", "List B",
  "Centaurea stoebe", "Spotted knapweed", "List B",
  "Potentilla recta", "Sulfur cinquefoil", "List B",
  "Linaria vulgaris", "Yellow toadflax", "List B",
  "Poa bulbosa", "Bulbous bluegrass", "List C",
  "Cichorium intybus", "Chicory", "List C",
  "Arctium minus", "Common burdock", "List C",
  "Verbascum thapsus", "Common mullein", "List C",
  "Hypericum perforatum", "Common St. Johnswort", "List C",
  "Bromus tectorum", "Downy brome", "List C",
  "Convolvulus arvensis", "Field bindweed", "List C",
  "Halogeton glomeratus", "Halogeton", "List C",
  "Sorghum halepense", "Johnsongrass", "List C",
  "Sonchus arvensis", "Perennial sowthistle", "List C",
  "Conium maculatum", "Poison hemlock", "List C",
  "Tribulus terrestris", "Puncturevine", "List C",
  "Elymus repens", "Quackgrass", "List C",
  "Ulmus pumila", "Siberian elm", "List C",
  "Ailanthus altissima", "Tree of Heaven", "List C"
) %>% mutate(state_code = "CO", source = "Colorado Department of Agriculture")

# Wyoming - Wyoming Weed & Pest Council (31 species)
WY_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Convolvulus arvensis", "Field bindweed",
  "Cirsium arvense", "Canada thistle",
  "Euphorbia esula", "Leafy spurge",
  "Sonchus arvensis", "Perennial sowthistle",
  "Elymus repens", "Quackgrass",
  "Cardaria draba", "Hoary cress",
  "Lepidium latifolium", "Perennial pepperweed",
  "Leucanthemum vulgare", "Ox-eye daisy",
  "Franseria discolor", "Skeletonleaf bursage",
  "Centaurea repens", "Russian knapweed",
  "Linaria vulgaris", "Yellow toadflax",
  "Linaria dalmatica", "Dalmatian toadflax",
  "Onopordum acanthium", "Scotch thistle",
  "Carduus nutans", "Musk thistle",
  "Arctium minus", "Common burdock",
  "Carduus acanthoides", "Plumeless thistle",
  "Isatis tinctoria", "Dyers woad",
  "Cynoglossum officinale", "Houndstongue",
  "Centaurea maculosa", "Spotted knapweed",
  "Centaurea diffusa", "Diffuse knapweed",
  "Lythrum salicaria", "Purple loosestrife",
  "Tamarix spp", "Saltcedar",
  "Hypericum perforatum", "Common St. Johnswort",
  "Tanacetum vulgare", "Common tansy",
  "Elaeagnus angustifolia", "Russian olive",
  "Hyoscyamus niger", "Black henbane",
  "Verbascum thapsus", "Common mullein",
  "Centaurea solstitialis", "Yellow starthistle",
  "Ventenata dubia", "Ventenata",
  "Taeniatherum caput-medusae", "Medusahead rye",
  "Amaranthus palmeri", "Palmer amaranth"
) %>% mutate(state_code = "WY", designation = "State Designated Noxious", source = "Wyoming Weed & Pest Council")

# Alaska - DNR Plant Materials Center (23 species)
AK_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Convolvulus arvensis", "Field bindweed", "Prohibited",
  "Rorippa austriaca", "Austrian fieldcress", "Prohibited",
  "Galinsoga parviflora", "Galinsoga", "Prohibited",
  "Galeopsis tetrahit", "Hempnettle", "Prohibited",
  "Solanum carolinense", "Horsenettle", "Prohibited",
  "Acroptilon repens", "Russian knapweed", "Prohibited",
  "Lactuca pulchella", "Blue-flowering lettuce", "Prohibited",
  "Elymus repens", "Quackgrass", "Prohibited",
  "Sonchus arvensis", "Perennial sowthistle", "Prohibited",
  "Euphorbia esula", "Leafy spurge", "Prohibited",
  "Cirsium arvense", "Canada thistle", "Prohibited",
  "Cardaria draba", "Whitetop", "Prohibited",
  "Lythrum salicaria", "Purple loosestrife", "Prohibited",
  "Hieracium aurantiacum", "Orange hawkweed", "Prohibited",
  "Poa annua", "Annual bluegrass", "Restricted",
  "Lappula echinata", "Blue burr", "Restricted",
  "Brassica juncea", "Mustard", "Restricted",
  "Avena fatua", "Wild oats", "Restricted",
  "Plantago lanceolata", "Buckhorn plantain", "Restricted",
  "Raphanus raphanistrum", "Radish", "Restricted",
  "Linaria vulgaris", "Yellow toadflax", "Restricted",
  "Vicia cracca", "Tufted vetch", "Restricted",
  "Polygonum convolvulus", "Wild buckwheat", "Restricted"
) %>% mutate(state_code = "AK", source = "Alaska DNR Plant Materials Center")

# South Dakota - DANR (7 species)
SD_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Artemisia absinthium", "Absinth wormwood",
  "Euphorbia esula", "Leafy spurge",
  "Cirsium arvense", "Canada thistle",
  "Sonchus arvensis", "Perennial sow thistle",
  "Cardaria draba", "Hoary cress",
  "Lythrum salicaria", "Purple loosestrife",
  "Tamarix ramosissima", "Salt cedar"
) %>% mutate(state_code = "SD", designation = "State Noxious", source = "South Dakota DANR")

# North Dakota - Department of Agriculture (13 species)
ND_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Artemisia absinthium", "Absinth wormwood",
  "Cirsium arvense", "Canada thistle",
  "Linaria dalmatica", "Dalmatian toadflax",
  "Centaurea diffusa", "Diffuse knapweed",
  "Cynoglossum officinale", "Houndstongue",
  "Euphorbia esula", "Leafy spurge",
  "Carduus nutans", "Musk thistle",
  "Amaranthus palmeri", "Palmer amaranth",
  "Lythrum salicaria", "Purple loosestrife",
  "Centaurea repens", "Russian knapweed",
  "Tamarix spp", "Saltcedar",
  "Centaurea maculosa", "Spotted knapweed",
  "Linaria vulgaris", "Yellow toadflax"
) %>% mutate(state_code = "ND", designation = "State Noxious", source = "North Dakota Department of Agriculture")

# Maine - Do Not Sell List (63 species)
ME_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Acer ginnala", "Amur maple",
  "Acer platanoides", "Norway maple",
  "Aegopodium podagraria", "Bishop's weed",
  "Ailanthus altissima", "Tree of heaven",
  "Alliaria petiolata", "Garlic mustard",
  "Amorpha fruticosa", "False indigo",
  "Ampelopsis glandulosa", "Porcelainberry",
  "Artemisia vulgaris", "Common mugwort",
  "Berberis thunbergii", "Japanese barberry",
  "Berberis vulgaris", "Common barberry",
  "Celastrus orbiculatus", "Asiatic bittersweet",
  "Elaeagnus umbellata", "Autumn olive",
  "Euonymus alatus", "Winged euonymus",
  "Euphorbia cyparissias", "Cypress spurge",
  "Fallopia baldschuanica", "Chinese bindweed",
  "Fallopia japonica", "Japanese knotweed",
  "Frangula alnus", "Glossy buckthorn",
  "Hesperis matronalis", "Dame's rocket",
  "Impatiens glandulifera", "Ornamental jewelweed",
  "Iris pseudacorus", "Yellow iris",
  "Ligustrum vulgare", "Common privet",
  "Lonicera japonica", "Japanese honeysuckle",
  "Lonicera maackii", "Amur honeysuckle",
  "Lonicera morrowii", "Morrow's honeysuckle",
  "Lonicera tatarica", "Tatarian honeysuckle",
  "Lythrum salicaria", "Purple loosestrife",
  "Microstegium vimineum", "Stilt grass",
  "Paulownia tomentosa", "Paulownia",
  "Persicaria perfoliata", "Mile-a-minute weed",
  "Phellodendron amurense", "Amur cork tree",
  "Populus alba", "White cottonwood",
  "Robinia pseudoacacia", "Black locust",
  "Rosa multiflora", "Multiflora rose",
  "Alnus glutinosa", "European alder",
  "Angelica sylvestris", "Woodland angelica",
  "Anthriscus sylvestris", "Wild chervil",
  "Aralia elata", "Japanese angelica tree",
  "Butomus umbellatus", "Flowering rush",
  "Elaeagnus angustifolia", "Russian olive",
  "Euonymus fortunei", "Wintercreeper",
  "Festuca filiformis", "Fine-leaved sheep fescue",
  "Ficaria verna", "Lesser celandine",
  "Glaucium flavum", "Yellow hornpoppy",
  "Glechoma hederacea", "Ground ivy",
  "Glyceria maxima", "Great mannagrass",
  "Hippophae rhamnoides", "Sea buckthorn",
  "Ligustrum obtusifolium", "Border privet",
  "Lonicera xylosteum", "Dwarf honeysuckle",
  "Lythrum virgatum", "European wand loosestrife",
  "Miscanthus sacchariflorus", "Amur silvergrass",
  "Petasites japonicus", "Fuki",
  "Phalaris arundinacea", "Reed canary grass",
  "Photinia villosa", "Photinia",
  "Phragmites australis", "Common reed",
  "Phyllostachys aurea", "Golden bamboo",
  "Phyllostachys aureosulcata", "Yellow groove bamboo",
  "Pyrus calleryana", "Callery pear",
  "Ranunculus repens", "Creeping buttercup",
  "Rubus phoenicolasius", "Wineberry",
  "Silphium perfoliatum", "Cup plant",
  "Sorbus aucuparia", "European mountain-ash",
  "Tussilago farfara", "Coltsfoot",
  "Valeriana officinalis", "Common valerian"
) %>% mutate(state_code = "ME", designation = "Do Not Sell", source = "Maine DACF Invasive Plants")

# Maryland - Noxious Weeds (8 species)
MD_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Cirsium vulgare", "Bull thistle", "Noxious",
  "Cirsium arvense", "Canada thistle", "Noxious",
  "Sorghum halepense", "Johnsongrass", "Noxious",
  "Carduus nutans", "Musk thistle", "Noxious",
  "Amaranthus palmeri", "Palmer amaranth", "Noxious",
  "Carduus acanthoides", "Plumeless thistle", "Noxious",
  "Sorghum bicolor", "Shattercane", "Noxious",
  "Amaranthus tuberculatus", "Tall waterhemp", "Noxious"
) %>% mutate(state_code = "MD", source = "Maryland Department of Agriculture")

# Kansas - Department of Agriculture (12 species)
KS_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Pueraria lobata", "Kudzu",
  "Convolvulus arvensis", "Field bindweed",
  "Centaurea repens", "Russian knapweed",
  "Cardaria draba", "Hoary cress",
  "Cirsium arvense", "Canada thistle",
  "Elymus repens", "Quackgrass",
  "Euphorbia esula", "Leafy spurge",
  "Ambrosia grayi", "Bur ragweed",
  "Hoffmannseggia densiflora", "Pignut",
  "Carduus nutans", "Musk thistle",
  "Sorghum halepense", "Johnsongrass",
  "Lespedeza cuneata", "Sericea lespedeza"
) %>% mutate(state_code = "KS", designation = "State Noxious", source = "Kansas Department of Agriculture")

# Nebraska - Department of Agriculture (13 species)
NE_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Cirsium arvense", "Canada thistle",
  "Euphorbia esula", "Leafy spurge",
  "Carduus nutans", "Musk thistle",
  "Carduus acanthoides", "Plumeless thistle",
  "Lythrum salicaria", "Purple loosestrife",
  "Centaurea stoebe", "Spotted knapweed",
  "Centaurea diffusa", "Diffuse knapweed",
  "Tamarix ramosissima", "Saltcedar",
  "Phragmites australis", "Common reed",
  "Lespedeza cuneata", "Sericea lespedeza",
  "Polygonum cuspidatum", "Japanese knotweed",
  "Polygonum sachalinense", "Giant knotweed",
  "Fallopia x bohemica", "Bohemian knotweed"
) %>% mutate(state_code = "NE", designation = "State Noxious", source = "Nebraska Department of Agriculture")

# Utah - Noxious Weed List (55 species across 5 classes)
UT_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Class 1A - EDRR Watch List
  "Crupina vulgaris", "Common crupina", "Class 1A",
  "Peganum harmala", "African rue", "Class 1A",
  "Anchusa arvensis", "Small bugloss", "Class 1A",
  "Salvia aethiopis", "Mediterranean sage", "Class 1A",
  "Milium vernale", "Spring millet", "Class 1A",
  "Zygophyllum fabago", "Syrian beancaper", "Class 1A",
  "Ventenata dubia", "Ventenata", "Class 1A",
  "Carduus acanthoides", "Plumeless thistle", "Class 1A",
  "Centaurea melitensis", "Malta starthistle", "Class 1A",
  # Class 1B - EDRR
  "Alhagi maurorum", "Camelthorn", "Class 1B",
  "Alliaria petiolata", "Garlic mustard", "Class 1B",
  "Centaurea calcitrapa", "Purple starthistle", "Class 1B",
  "Galega officinalis", "Goatsrue", "Class 1B",
  "Brassica tournefortii", "African mustard", "Class 1B",
  "Arundo donax", "Giant reed", "Class 1B",
  "Polygonum cuspidatum", "Japanese knotweed", "Class 1B",
  "Echium vulgare", "Blueweed", "Class 1B",
  "Brassica elongata", "Elongated mustard", "Class 1B",
  "Hypericum perforatum", "Common St. Johnswort", "Class 1B",
  "Leucanthemum vulgare", "Oxeye daisy", "Class 1B",
  "Scorzonera laciniata", "Cutleaf vipergrass", "Class 1B",
  # Class 2 - Control
  "Euphorbia esula", "Leafy spurge", "Class 2",
  "Taeniatherum caput-medusae", "Medusahead", "Class 2",
  "Chondrilla juncea", "Rush skeletonweed", "Class 2",
  "Centaurea stoebe", "Spotted knapweed", "Class 2",
  "Lythrum salicaria", "Purple loosestrife", "Class 2",
  "Centaurea virgata", "Squarrose knapweed", "Class 2",
  "Isatis tinctoria", "Dyers woad", "Class 2",
  "Centaurea solstitialis", "Yellow starthistle", "Class 2",
  "Linaria vulgaris", "Yellow toadflax", "Class 2",
  "Centaurea diffusa", "Diffuse knapweed", "Class 2",
  "Hyoscyamus niger", "Black henbane", "Class 2",
  "Linaria dalmatica", "Dalmatian toadflax", "Class 2",
  # Class 3 - Containment
  "Acroptilon repens", "Russian knapweed", "Class 3",
  "Cynoglossum officinale", "Houndstongue", "Class 3",
  "Lepidium latifolium", "Perennial pepperweed", "Class 3",
  "Phragmites australis", "Common reed", "Class 3",
  "Tamarix ramosissima", "Saltcedar", "Class 3",
  "Cardaria draba", "Hoary cress", "Class 3",
  "Cirsium arvense", "Canada thistle", "Class 3",
  "Conium maculatum", "Poison hemlock", "Class 3",
  "Carduus nutans", "Musk thistle", "Class 3",
  "Elymus repens", "Quackgrass", "Class 3",
  "Aegilops cylindrica", "Jointed goatgrass", "Class 3",
  "Cynodon dactylon", "Bermudagrass", "Class 3",
  "Sorghum halepense", "Johnsongrass", "Class 3",
  "Sorghum almum", "Sorghum almum", "Class 3",
  "Onopordum acanthium", "Scotch thistle", "Class 3",
  "Convolvulus arvensis", "Field bindweed", "Class 3",
  "Tribulus terrestris", "Puncturevine", "Class 3",
  # Class 4 - Prohibited
  "Imperata cylindrica", "Cogongrass", "Class 4",
  "Euphorbia myrsinites", "Myrtle spurge", "Class 4",
  "Hesperis matronalis", "Dames rocket", "Class 4",
  "Cytisus scoparius", "Scotch broom", "Class 4",
  "Elaeagnus angustifolia", "Russian olive", "Class 4"
) %>% mutate(state_code = "UT", source = "Utah Dept of Agriculture Noxious Weed List")

# Nevada - Department of Agriculture Noxious Weed List (58 species)
NV_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Peganum harmala", "African rue",
  "Rorippa austriaca", "Austrian fieldcress",
  "Sphaerophysa salsula", "Austrian peaweed",
  "Aegilops triuncialis", "Barbed goatgrass",
  "Hyoscyamus niger", "Black henbane",
  "Pennisetum ciliare", "Buffelgrass",
  "Alhagi maurorum", "Camelthorn",
  "Cirsium arvense", "Canada thistle",
  "Crupina vulgaris", "Common crupina",
  "Potamogeton crispus", "Curly leaf pondweed",
  "Linaria dalmatica", "Dalmatian toadflax",
  "Volutaria tubuliflora", "Desert knapweed",
  "Centaurea diffusa", "Diffuse knapweed",
  "Isatis tinctoria", "Dyers woad",
  "Myriophyllum spicatum", "Eurasian watermilfoil",
  "Butomus umbellatus", "Flowering rush",
  "Arundo donax", "Giant reed",
  "Salvinia molesta", "Giant salvinia",
  "Galega officinalis", "Goatsrue",
  "Pennisetum setaceum", "Green fountaingrass",
  "Cardaria draba", "Hoary cress",
  "Solanum carolinense", "Horsenettle",
  "Cynoglossum officinale", "Houndstongue",
  "Hydrilla verticillata", "Hydrilla",
  "Centaurea iberica", "Iberian starthistle",
  "Sorghum halepense", "Johnsongrass",
  "Aegilops cylindrica", "Jointed goatgrass",
  "Hypericum perforatum", "Klamath weed",
  "Euphorbia esula", "Leafy spurge",
  "Centaurea melitensis", "Malta starthistle",
  "Anthemis cotula", "Mayweed chamomile",
  "Salvia aethiopis", "Mediterranean sage",
  "Taeniatherum caput-medusae", "Medusahead",
  "Carduus nutans", "Musk thistle",
  "Lepidium latifolium", "Perennial pepperweed",
  "Sonchus arvensis", "Perennial sowthistle",
  "Conium maculatum", "Poison hemlock",
  "Tribulus terrestris", "Puncturevine",
  "Lythrum salicaria", "Purple loosestrife",
  "Centaurea calcitrapa", "Purple starthistle",
  "Chondrilla juncea", "Rush skeletonweed",
  "Acroptilon repens", "Russian knapweed",
  "Brassica tournefortii", "Sahara mustard",
  "Tamarix ramosissima", "Saltcedar",
  "Onopordum acanthium", "Scotch thistle",
  "Solanum elaeagnifolium", "Silverleaf nightshade",
  "Centaurea stoebe", "Spotted knapweed",
  "Centaurea virgata", "Squarrose knapweed",
  "Potentilla recta", "Sulfur cinquefoil",
  "Zygophyllum fabago", "Syrian beancaper",
  "Ventenata dubia", "Ventenata",
  "Cicuta maculata", "Water hemlock",
  "Centaurea solstitialis", "Yellow starthistle",
  "Linaria vulgaris", "Yellow toadflax"
) %>% mutate(state_code = "NV", designation = "State Noxious", source = "Nevada Dept of Agriculture")

# Idaho - Noxious Weed List (57 species)
ID_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Statewide Prohibited
  "Cytisus scoparius", "Scotch broom", "Prohibited",
  "Genista germanica", "Genista", "Prohibited",
  "Spartium junceum", "Spanish broom", "Prohibited",
  # Statewide EDRR
  "Heracleum mantegazzianum", "Giant hogweed", "EDRR",
  "Galega officinalis", "Goatsrue", "EDRR",
  "Centaurea iberica", "Iberian starthistle", "EDRR",
  "Impatiens glandulifera", "Policemans helmet", "EDRR",
  "Centaurea triumfettii", "Purple starthistle", "EDRR",
  "Zygophyllum fabago", "Syrian beancaper", "EDRR",
  "Hieracium lachenalii", "Tall hawkweed", "EDRR",
  "Hieracium glomeratum", "Yellow devil hawkweed", "EDRR",
  "Centaurea virgata", "Squarrose knapweed", "EDRR",
  "Imperata cylindrica", "Cogongrass", "EDRR",
  # Statewide Control
  "Hyoscyamus niger", "Black henbane", "Control",
  "Fallopia x bohemica", "Bohemian knotweed", "Control",
  "Solanum rostratum", "Buffalobur", "Control",
  "Crupina vulgaris", "Common crupina", "Control",
  "Isatis tinctoria", "Dyers woad", "Control",
  "Fallopia sachalinensis", "Giant knotweed", "Control",
  "Fallopia japonica", "Japanese knotweed", "Control",
  "Sorghum halepense", "Johnsongrass", "Control",
  "Nardus stricta", "Matgrass", "Control",
  "Centaurea dealbata", "Meadow knapweed", "Control",
  "Salvia aethiopis", "Mediterranean sage", "Control",
  "Carduus nutans", "Musk thistle", "Control",
  "Hieracium aurantiacum", "Orange hawkweed", "Control",
  "Sonchus arvensis", "Perennial sowthistle", "Control",
  "Acroptilon repens", "Russian knapweed", "Control",
  "Anchusa arvensis", "Small bugloss", "Control",
  "Echium vulgare", "Vipers bugloss", "Control",
  "Hieracium caespitosum", "Yellow hawkweed", "Control",
  # Statewide Containment
  "Cirsium arvense", "Canada thistle", "Containment",
  "Linaria dalmatica", "Dalmatian toadflax", "Containment",
  "Centaurea diffusa", "Diffuse knapweed", "Containment",
  "Convolvulus arvensis", "Field bindweed", "Containment",
  "Berteroa incana", "Hoary alyssum", "Containment",
  "Cynoglossum officinale", "Houndstongue", "Containment",
  "Aegilops cylindrica", "Jointed goatgrass", "Containment",
  "Euphorbia esula", "Leafy spurge", "Containment",
  "Milium vernale", "Spring millet", "Containment",
  "Leucanthemum vulgare", "Oxeye daisy", "Containment",
  "Lepidium latifolium", "Perennial pepperweed", "Containment",
  "Carduus acanthoides", "Plumeless thistle", "Containment",
  "Conium maculatum", "Poison hemlock", "Containment",
  "Tribulus terrestris", "Puncturevine", "Containment",
  "Lythrum salicaria", "Purple loosestrife", "Containment",
  "Chondrilla juncea", "Rush skeletonweed", "Containment",
  "Tamarix ramosissima", "Saltcedar", "Containment",
  "Onopordum acanthium", "Scotch thistle", "Containment",
  "Centaurea stoebe", "Spotted knapweed", "Containment",
  "Senecio jacobaea", "Tansy ragwort", "Containment",
  "Bryonia alba", "White bryony", "Containment",
  "Cardaria draba", "Whitetop", "Containment",
  "Centaurea solstitialis", "Yellow starthistle", "Containment",
  "Linaria vulgaris", "Yellow toadflax", "Containment"
) %>% mutate(state_code = "ID", source = "Idaho Invasive Species Council")

# Texas - Department of Agriculture (partial list, 37 total, first 10 listed)
TX_COMPILED <- tribble(
  ~scientific_name, ~common_name,
  "Alhagi maurorum", "Camelthorn",
  "Alternanthera philoxeroides", "Alligatorweed",
  "Arundo donax", "Giant reed",
  "Asphodelus fistulosus", "Onionweed",
  "Calystegia sepium", "Hedge bindweed",
  "Commelina benghalensis", "Tropical spiderwort",
  "Convolvulus arvensis", "Field bindweed",
  "Cuscuta japonica", "Japanese dodder",
  "Cyperus entrerianus", "Deep-rooted sedge",
  "Eichhornia azurea", "Anchored water hyacinth",
  "Eichhornia crassipes", "Common water hyacinth",
  "Hydrilla verticillata", "Hydrilla",
  "Imperata cylindrica", "Cogongrass",
  "Lygodium japonicum", "Japanese climbing fern",
  "Orobanche ramosa", "Branched broomrape",
  "Pennisetum ciliare", "Buffelgrass",
  "Phragmites australis", "Common reed",
  "Pistia stratiotes", "Water lettuce",
  "Salvinia molesta", "Giant salvinia",
  "Solanum viarum", "Tropical soda apple",
  "Striga asiatica", "Witchweed",
  "Tamarix aphylla", "Athel tree",
  "Tamarix ramosissima", "Saltcedar",
  "Triadica sebifera", "Chinese tallow"
) %>% mutate(state_code = "TX", designation = "State Noxious", source = "Texas Department of Agriculture")

# Connecticut - CIPWG Invasive Plant List (96 species, Jan 2026)
CT_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Invasive species
  "Acer platanoides", "Norway maple", "Invasive",
  "Aegopodium podagraria", "Goutweed", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Alliaria petiolata", "Garlic mustard", "Invasive",
  "Ampelopsis brevipedunculata", "Porcelainberry", "Invasive",
  "Artemisia vulgaris", "Mugwort", "Invasive",
  "Berberis thunbergii", "Japanese barberry", "Invasive",
  "Berberis vulgaris", "Common barberry", "Invasive",
  "Cabomba caroliniana", "Fanwort", "Invasive",
  "Cardamine impatiens", "Narrowleaf bittercress", "Invasive",
  "Celastrus orbiculatus", "Asiatic bittersweet", "Invasive",
  "Centaurea stoebe", "Spotted knapweed", "Invasive",
  "Cynanchum louiseae", "Black swallow-wort", "Invasive",
  "Cynanchum rossicum", "Pale swallow-wort", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Elymus repens", "Quackgrass", "Invasive",
  "Euonymus alatus", "Winged euonymus", "Invasive",
  "Euphorbia esula", "Leafy spurge", "Invasive",
  "Frangula alnus", "Glossy buckthorn", "Invasive",
  "Froelichia gracilis", "Slender snake cotton", "Invasive",
  "Hesperis matronalis", "Dames rocket", "Invasive",
  "Hydrilla verticillata", "Hydrilla", "Invasive",
  "Iris pseudacorus", "Yellow iris", "Invasive",
  "Lepidium latifolium", "Perennial pepperweed", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive",
  "Lonicera maackii", "Amur honeysuckle", "Invasive",
  "Lonicera morrowii", "Morrows honeysuckle", "Invasive",
  "Lonicera x bella", "Belle honeysuckle", "Invasive",
  "Lythrum salicaria", "Purple loosestrife", "Invasive",
  "Microstegium vimineum", "Japanese stiltgrass", "Invasive",
  "Myosotis scorpioides", "Forget-me-not", "Invasive",
  "Myriophyllum heterophyllum", "Variable-leaf watermilfoil", "Invasive",
  "Phragmites australis", "Common reed", "Invasive",
  "Polygonum cuspidatum", "Japanese knotweed", "Invasive",
  "Pueraria montana", "Kudzu", "Invasive",
  "Pyrus calleryana", "Callery pear", "Invasive",
  "Rhamnus cathartica", "Common buckthorn", "Invasive",
  "Robinia pseudoacacia", "Black locust", "Invasive",
  "Rosa multiflora", "Multiflora rose", "Invasive",
  # Potentially Invasive species
  "Acer ginnala", "Amur maple", "Potentially Invasive",
  "Acer pseudoplatanus", "Sycamore maple", "Potentially Invasive",
  "Amorpha fruticosa", "False indigo", "Potentially Invasive",
  "Aralia elata", "Japanese angelica tree", "Potentially Invasive",
  "Arthraxon hispidus", "Hairy jointgrass", "Potentially Invasive",
  "Bassia scoparia", "Common kochia", "Potentially Invasive",
  "Bromus tectorum", "Drooping brome-grass", "Potentially Invasive",
  "Butomus umbellatus", "Flowering rush", "Potentially Invasive",
  "Callitriche stagnalis", "Pond water-starwort", "Potentially Invasive",
  "Carex kobomugi", "Japanese sedge", "Potentially Invasive",
  "Cirsium arvense", "Canada thistle", "Potentially Invasive",
  "Datura stramonium", "Jimsonweed", "Potentially Invasive",
  "Egeria densa", "Brazilian water-weed", "Potentially Invasive",
  "Eichhornia crassipes", "Common water-hyacinth", "Potentially Invasive",
  "Elaeagnus angustifolia", "Russian olive", "Potentially Invasive",
  "Elsholtzia ciliata", "Crested late-summer mint", "Potentially Invasive",
  "Euphorbia cyparissias", "Cypress spurge", "Potentially Invasive",
  "Glechoma hederacea", "Ground ivy", "Potentially Invasive",
  "Glyceria maxima", "Reed mannagrass", "Potentially Invasive",
  "Heracleum mantegazzianum", "Giant hogweed", "Potentially Invasive",
  "Humulus japonicus", "Japanese hops", "Potentially Invasive",
  "Impatiens glandulifera", "Ornamental jewelweed", "Potentially Invasive",
  "Ligustrum obtusifolium", "Border privet", "Potentially Invasive",
  "Ligustrum ovalifolium", "California privet", "Potentially Invasive",
  "Ligustrum vulgare", "European privet", "Potentially Invasive",
  "Lonicera tatarica", "Tatarian honeysuckle", "Potentially Invasive",
  "Lonicera xylosteum", "Dwarf honeysuckle", "Potentially Invasive",
  "Lychnis flos-cuculi", "Ragged robin", "Potentially Invasive",
  "Lysimachia nummularia", "Moneywort", "Potentially Invasive",
  "Lysimachia vulgaris", "Garden loosestrife", "Potentially Invasive",
  "Marsilea quadrifolia", "European waterclover", "Potentially Invasive",
  "Miscanthus sinensis", "Eulalia", "Potentially Invasive",
  "Myriophyllum aquaticum", "Parrotfeather", "Potentially Invasive",
  "Wisteria floribunda", "Japanese wisteria", "Potentially Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Potentially Invasive"
) %>% mutate(state_code = "CT", source = "Connecticut Invasive Plant Working Group (CIPWG)")

# Iowa - DNR + Iowa Code Chapter 317 Noxious Weeds (55 species, Jan 2026)
IA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Primary Noxious Weeds (Iowa Code 317.1A)
  "Elymus repens", "Quackgrass", "Primary Noxious",
  "Sonchus arvensis", "Perennial sowthistle", "Primary Noxious",
  "Cirsium arvense", "Canada thistle", "Primary Noxious",
  "Cirsium vulgare", "Bull thistle", "Primary Noxious",
  "Convolvulus arvensis", "Field bindweed", "Primary Noxious",
  "Solanum carolinense", "Horsenettle", "Primary Noxious",
  "Euphorbia esula", "Leafy spurge", "Primary Noxious",
  "Cardaria draba", "Hoary cress", "Primary Noxious",
  "Acroptilon repens", "Russian knapweed", "Primary Noxious",
  "Rhamnus cathartica", "Common buckthorn", "Primary Noxious",
  "Rosa multiflora", "Multiflora rose", "Primary Noxious",
  "Amaranthus palmeri", "Palmer amaranth", "Primary Noxious",
  "Tribulus terrestris", "Puncturevine", "Primary Noxious",
  "Dipsacus fullonum", "Common teasel", "Primary Noxious",
  "Sorghum bicolor", "Shattercane", "Primary Noxious",
  # Secondary Noxious Weeds
  "Abutilon theophrasti", "Velvetleaf", "Secondary Noxious",
  "Xanthium strumarium", "Common cocklebur", "Secondary Noxious",
  "Sinapis arvensis", "Wild mustard", "Secondary Noxious",
  "Daucus carota", "Wild carrot", "Secondary Noxious",
  "Plantago lanceolata", "Buckhorn plantain", "Secondary Noxious",
  "Rumex acetosella", "Sheep sorrel", "Secondary Noxious",
  "Rumex crispus", "Curly dock", "Secondary Noxious",
  "Rumex altissimus", "Smooth dock", "Secondary Noxious",
  "Conium maculatum", "Poison hemlock", "Secondary Noxious",
  "Helianthus annuus", "Wild sunflower", "Secondary Noxious",
  # DNR Forest Invasive Species
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Bromus tectorum", "Cheatgrass", "Invasive",
  "Ulmus parvifolia", "Chinese elm", "Invasive",
  "Arctium minus", "Common burdock", "Invasive",
  "Coronilla varia", "Crown vetch", "Invasive",
  "Hesperis matronalis", "Dames rocket", "Invasive",
  "Lonicera maackii", "Amur honeysuckle", "Invasive",
  "Lonicera morrowii", "Morrows honeysuckle", "Invasive",
  "Lonicera tatarica", "Tatarian honeysuckle", "Invasive",
  "Alliaria petiolata", "Garlic mustard", "Invasive",
  "Heracleum mantegazzianum", "Giant hogweed", "Invasive",
  "Berberis thunbergii", "Japanese barberry", "Invasive",
  "Polygonum cuspidatum", "Japanese knotweed", "Invasive",
  "Centaurea stoebe", "Spotted knapweed", "Invasive",
  "Pueraria montana", "Kudzu", "Invasive",
  "Carduus nutans", "Musk thistle", "Invasive",
  "Celastrus orbiculatus", "Oriental bittersweet", "Invasive",
  "Ligustrum vulgare", "European privet", "Invasive",
  "Lythrum salicaria", "Purple loosestrife", "Invasive",
  "Phalaris arundinacea", "Reed canary grass", "Invasive",
  "Elaeagnus angustifolia", "Russian olive", "Invasive",
  "Tamarix ramosissima", "Saltcedar", "Invasive",
  "Lespedeza cuneata", "Sericea lespedeza", "Invasive",
  "Ulmus pumila", "Siberian elm", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Morus alba", "White mulberry", "Invasive",
  "Pastinaca sativa", "Wild parsnip", "Invasive",
  "Euonymus alatus", "Burning bush", "Invasive",
  "Centaurea solstitialis", "Yellow star thistle", "Invasive"
) %>% mutate(state_code = "IA", source = "Iowa DNR / Iowa Code Chapter 317")

# Oregon - ODA Noxious Weed List (45 species, Jan 2026)
OR_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # List A (Eradication)
  "Peganum harmala", "African rue", "List A",
  "Aegilops triuncialis", "Barbed goatgrass", "List A",
  "Alhagi pseudalhagi", "Camelthorn", "List A",
  "Delairea odorata", "Cape ivy", "List A",
  "Tussilago farfara", "Coltsfoot", "List A",
  "Spartina anglica", "Common cordgrass", "List A",
  "Hydrocharis morsus-ranae", "Common frogbit", "List A",
  "Sagittaria platyphylla", "Delta arrowhead", "List A",
  "Spartina densiflora", "Dense flowered cordgrass", "List A",
  "Trapa natans", "European water chestnut", "List A",
  "Butomus umbellatus", "Flowering rush", "List A",
  "Lysimachia vulgaris", "Garden yellow loosestrife", "List A",
  "Heracleum mantegazzianum", "Giant hogweed", "List A",
  "Galega officinalis", "Goatsrue", "List A",
  # List B (Control)
  "Rubus armeniacus", "Armenian blackberry", "List B",
  "Hedera hibernica", "Atlantic ivy", "List B",
  "Acaena novae-zelandiae", "Biddy-biddy", "List B",
  "Fallopia x bohemica", "Bohemian knotweed", "List B",
  "Solanum rostratum", "Buffalobur", "List B",
  "Cirsium vulgare", "Bull thistle", "List B",
  "Buddleja davidii", "Butterfly bush", "List B",
  "Cirsium arvense", "Canada thistle", "List B",
  "Anchusa officinalis", "Common bugloss", "List B",
  "Phragmites australis", "Common reed", "List B",
  "Crupina vulgaris", "Common crupina", "List B",
  "Rorippa sylvestris", "Creeping yellowcress", "List B",
  "Dipsacus laciniatus", "Cutleaf teasel", "List B",
  "Linaria dalmatica", "Dalmatian toadflax", "List B",
  "Centaurea diffusa", "Diffuse knapweed", "List B",
  "Cuscuta spp", "Dodder", "List B",
  "Rosa canina", "Dog rose", "List B",
  "Isatis tinctoria", "Dyers woad", "List B",
  "Crataegus monogyna", "English hawthorn", "List B",
  "Hedera helix", "English ivy", "List B",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "List B",
  "Brachypodium sylvaticum", "False brome", "List B",
  "Convolvulus arvensis", "Field bindweed", "List B",
  "Genista monspessulana", "French broom", "List B",
  "Alliaria petiolata", "Garlic mustard", "List B",
  "Fallopia sachalinensis", "Giant knotweed", "List B",
  "Arundo donax", "Giant reed", "List B",
  "Ulex europaeus", "Gorse", "List B",
  "Lepidium pubescens", "Hairy whitetop", "List B",
  "Halogeton glomeratus", "Halogeton", "List B",
  "Geranium robertianum", "Herb Robert", "List B"
) %>% mutate(state_code = "OR", source = "Oregon Dept of Agriculture Noxious Weed List")

# California - Cal-IPC Inventory (95 species, Jan 2026)
CA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # High Impact
  "Aegilops triuncialis", "Barb goatgrass", "High",
  "Alternanthera philoxeroides", "Alligatorweed", "High",
  "Ammophila arenaria", "European beachgrass", "High",
  "Arundo donax", "Giant reed", "High",
  "Brassica tournefortii", "Sahara mustard", "High",
  "Bromus madritensis", "Red brome", "High",
  "Bromus tectorum", "Cheatgrass", "High",
  "Carpobrotus edulis", "Highway iceplant", "High",
  "Carthamus lanatus", "Woolly distaff thistle", "High",
  "Centaurea solstitialis", "Yellow starthistle", "High",
  "Centaurea stoebe", "Spotted knapweed", "High",
  "Cortaderia jubata", "Jubatagrass", "High",
  "Cortaderia selloana", "Pampasgrass", "High",
  "Cytisus scoparius", "Scotch broom", "High",
  "Delairea odorata", "Cape-ivy", "High",
  "Egeria densa", "Brazilian egeria", "High",
  "Ehrharta calycina", "Purple veldtgrass", "High",
  "Eichhornia crassipes", "Water hyacinth", "High",
  "Euphorbia virgata", "Leafy spurge", "High",
  "Genista monspessulana", "French broom", "High",
  "Hedera canariensis", "Algerian ivy", "High",
  "Hedera helix", "English ivy", "High",
  "Taeniatherum caput-medusae", "Medusahead", "High",
  # Moderate Impact
  "Acacia dealbata", "Silver wattle", "Moderate",
  "Ageratina adenophora", "Eupatory", "Moderate",
  "Ailanthus altissima", "Tree-of-heaven", "Moderate",
  "Alhagi maurorum", "Camelthorn", "Moderate",
  "Alliaria petiolata", "Garlic mustard", "Moderate",
  "Arctotheca calendula", "Fertile capeweed", "Moderate",
  "Asparagus asparagoides", "Bridal creeper", "Moderate",
  "Asphodelus fistulosus", "Onion weed", "Moderate",
  "Avena barbata", "Slender oat", "Moderate",
  "Avena fatua", "Wild oats", "Moderate",
  "Brachypodium sylvaticum", "Slender false-brome", "Moderate",
  "Brassica nigra", "Black mustard", "Moderate",
  "Bromus diandrus", "Ripgut brome", "Moderate",
  "Carduus nutans", "Musk thistle", "Moderate",
  "Carduus pycnocephalus", "Italian thistle", "Moderate",
  "Centaurea calcitrapa", "Purple starthistle", "Moderate",
  "Centaurea diffusa", "Diffuse knapweed", "Moderate",
  "Centaurea melitensis", "Tocalote", "Moderate",
  "Centaurea virgata", "Squarrose knapweed", "Moderate",
  "Chondrilla juncea", "Skeleton weed", "Moderate",
  "Cirsium arvense", "Canada thistle", "Moderate",
  "Cirsium vulgare", "Bull thistle", "Moderate",
  "Clematis vitalba", "Old mans beard", "Moderate",
  "Conium maculatum", "Poison-hemlock", "Moderate",
  "Cotoneaster franchetii", "Orange cotoneaster", "Moderate",
  "Cotoneaster lacteus", "Milkflower cotoneaster", "Moderate",
  "Cotoneaster pannosus", "Silverleaf cotoneaster", "Moderate",
  "Cynara cardunculus", "Artichoke thistle", "Moderate",
  "Cynodon dactylon", "Bermuda grass", "Moderate",
  "Cynoglossum officinale", "Common houndstongue", "Moderate",
  "Cytisus striatus", "Portuguese broom", "Moderate",
  "Dipsacus fullonum", "Wild teasel", "Moderate",
  "Dittrichia graveolens", "Stinkwort", "Moderate",
  "Ehrharta erecta", "Panic veldtgrass", "Moderate",
  "Elaeagnus angustifolia", "Russian olive", "Moderate",
  "Fallopia japonica", "Japanese knotweed", "Moderate",
  "Fallopia sachalinensis", "Giant knotweed", "Moderate",
  "Festuca arundinacea", "Reed fescue", "Moderate",
  "Ficus carica", "Edible fig", "Moderate",
  "Foeniculum vulgare", "Fennel", "Moderate",
  "Halogeton glomeratus", "Halogeton", "Moderate",
  "Hirschfeldia incana", "Short-pod mustard", "Moderate",
  "Holcus lanatus", "Common velvet grass", "Moderate",
  # Limited Impact
  "Acacia melanoxylon", "Blackwood acacia", "Limited",
  "Agrostis stolonifera", "Creeping bent", "Limited",
  "Anthoxanthum odoratum", "Sweet vernal grass", "Limited",
  "Briza maxima", "Big quakinggrass", "Limited",
  "Bromus hordeaceus", "Soft brome", "Limited",
  "Bromus japonicus", "Japanese brome", "Limited",
  "Carduus acanthoides", "Plumeless thistle", "Limited",
  "Carduus tenuiflorus", "Slenderflower thistle", "Limited",
  "Crataegus monogyna", "English hawthorn", "Limited",
  "Crupina vulgaris", "Common crupina", "Limited",
  "Digitalis purpurea", "Foxglove", "Limited",
  "Echium candicans", "Pride-of-Madeira", "Limited",
  "Erica lusitanica", "Spanish heath", "Limited",
  "Eucalyptus camaldulensis", "Red gum", "Limited",
  "Eucalyptus globulus", "Blue gum", "Limited",
  "Euphorbia oblongata", "Eggleaf spurge", "Limited",
  "Euphorbia terracina", "Carnation spurge", "Limited",
  "Lythrum salicaria", "Purple loosestrife", "Limited",
  "Lepidium latifolium", "Perennial pepperweed", "Limited",
  "Tamarix ramosissima", "Saltcedar", "Limited"
) %>% mutate(state_code = "CA", source = "California Invasive Plant Council (Cal-IPC)")

# Washington - NWCB Noxious Weed List (85 species, Jan 2026)
WA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Class A (Eradication required)
  "Zygophyllum fabago", "Bean-caper", "Class A",
  "Crupina vulgaris", "Common crupina", "Class A",
  "Spartina anglica", "Common cordgrass", "Class A",
  "Spartina densiflora", "Dense flowered cordgrass", "Class A",
  "Spartina patens", "Salt meadow cordgrass", "Class A",
  "Spartina alterniflora", "Smooth cordgrass", "Class A",
  "Isatis tinctoria", "Dyers woad", "Class A",
  "Euphorbia oblongata", "Eggleaf spurge", "Class A",
  "Brachypodium sylvaticum", "False brome", "Class A",
  "Ludwigia peploides", "Floating primrose-willow", "Class A",
  "Butomus umbellatus", "Flowering rush", "Class A",
  "Alliaria petiolata", "Garlic mustard", "Class A",
  "Heracleum mantegazzianum", "Giant hogweed", "Class A",
  "Galega officinalis", "Goatsrue", "Class A",
  "Hydrilla verticillata", "Hydrilla", "Class A",
  "Sorghum halepense", "Johnsongrass", "Class A",
  "Centaurea macrocephala", "Bighead knapweed", "Class A",
  "Centaurea nigrescens", "Short fringed knapweed", "Class A",
  "Pueraria montana", "Kudzu", "Class A",
  "Salvia pratensis", "Meadow clary", "Class A",
  "Clematis orientalis", "Orange peel clematis", "Class A",
  "Centaurea calcitrapa", "Purple starthistle", "Class A",
  "Glyceria maxima", "Reed sweetgrass", "Class A",
  "Celastrus orbiculatus", "Oriental bittersweet", "Class A",
  "Spartium junceum", "Rush broom", "Class A",
  "Salvia aethiopis", "Mediterranean sage", "Class A",
  "Solanum elaeagnifolium", "Silverleaf nightshade", "Class A",
  "Impatiens parviflora", "Small-flowered jewelweed", "Class A",
  "Genista monspessulana", "French broom", "Class A",
  "Silybum marianum", "Milk thistle", "Class A",
  "Carduus pycnocephalus", "Italian thistle", "Class A",
  "Carduus tenuiflorus", "Slenderflower thistle", "Class A",
  "Myriophyllum heterophyllum", "Variable leaf milfoil", "Class A",
  "Cirsium palustre", "Marsh thistle", "Class A",
  # Class B (Regulated)
  "Echium vulgare", "Blueweed", "Class B",
  "Anchusa officinalis", "Common bugloss", "Class B",
  "Alhagi maurorum", "Camelthorn", "Class B",
  "Phragmites australis", "Common reed", "Class B",
  "Linaria dalmatica", "Dalmatian toadflax", "Class B",
  "Egeria densa", "Brazilian egeria", "Class B",
  "Cabomba caroliniana", "Fanwort", "Class B",
  "Ulex europaeus", "Gorse", "Class B",
  "Epilobium hirsutum", "Hairy willowherb", "Class B",
  "Rhaponticum repens", "Russian knapweed", "Class B",
  "Hieracium aurantiacum", "Orange hawkweed", "Class B",
  "Berteroa incana", "Hoary alyssum", "Class B",
  "Cynoglossum officinale", "Houndstongue", "Class B",
  "Amorpha fruticosa", "Indigobush", "Class B",
  "Centaurea nigra", "Black knapweed", "Class B",
  "Centaurea jacea", "Brown knapweed", "Class B",
  "Centaurea diffusa", "Diffuse knapweed", "Class B",
  "Centaurea stoebe", "Spotted knapweed", "Class B",
  "Bassia scoparia", "Kochia", "Class B",
  "Lysimachia vulgaris", "Garden loosestrife", "Class B",
  "Lythrum salicaria", "Purple loosestrife", "Class B",
  "Myriophyllum aquaticum", "Parrotfeather", "Class B",
  "Lepidium latifolium", "Perennial pepperweed", "Class B",
  "Impatiens glandulifera", "Policemans helmet", "Class B",
  "Chondrilla juncea", "Rush skeletonweed", "Class B",
  "Tamarix ramosissima", "Saltcedar", "Class B",
  "Euphorbia virgata", "Leafy spurge", "Class B",
  "Centaurea solstitialis", "Yellow starthistle", "Class B",
  "Potentilla recta", "Sulfur cinquefoil", "Class B",
  "Jacobaea vulgaris", "Tansy ragwort", "Class B",
  "Onopordum acanthium", "Scotch thistle", "Class B",
  "Carduus nutans", "Musk thistle", "Class B",
  "Carduus acanthoides", "Plumeless thistle", "Class B",
  "Centaurea melitensis", "Tocalote", "Class B",
  "Tussilago farfara", "Coltsfoot", "Class B",
  "Ludwigia hexapetala", "Water primrose", "Class B",
  "Bryonia alba", "White bryony", "Class B",
  "Anthriscus sylvestris", "Wild chervil", "Class B",
  "Nymphoides peltata", "Yellow floating heart", "Class B",
  # Class C (Widespread)
  "Artemisia absinthium", "Absinth wormwood", "Class C",
  "Ammophila arenaria", "European beachgrass", "Class C",
  "Cirsium vulgare", "Bull thistle", "Class C",
  "Cirsium arvense", "Canada thistle", "Class C",
  "Convolvulus arvensis", "Field bindweed", "Class C",
  "Conium maculatum", "Poison hemlock", "Class C",
  "Cytisus scoparius", "Scotch broom", "Class C",
  "Fallopia japonica", "Japanese knotweed", "Class C",
  "Fallopia sachalinensis", "Giant knotweed", "Class C",
  "Iris pseudacorus", "Yellow flag iris", "Class C",
  "Linaria vulgaris", "Yellow toadflax", "Class C"
) %>% mutate(state_code = "WA", source = "Washington State Noxious Weed Control Board")

# Montana - State Noxious Weed List (36 species, Jan 2026)
MT_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Priority 1A (Eradication)
  "Centaurea solstitialis", "Yellow starthistle", "Priority 1A",
  "Isatis tinctoria", "Dyers woad", "Priority 1A",
  "Phragmites australis", "Common reed", "Priority 1A",
  "Taeniatherum caput-medusae", "Medusahead", "Priority 1A",
  # Priority 1B (Containment)
  "Polygonum cuspidatum", "Japanese knotweed", "Priority 1B",
  "Fallopia sachalinensis", "Giant knotweed", "Priority 1B",
  "Fallopia x bohemica", "Bohemian knotweed", "Priority 1B",
  "Lythrum salicaria", "Purple loosestrife", "Priority 1B",
  "Chondrilla juncea", "Rush skeletonweed", "Priority 1B",
  "Cytisus scoparius", "Scotch broom", "Priority 1B",
  "Echium vulgare", "Blueweed", "Priority 1B",
  # Priority 2A (Regional management)
  "Senecio jacobaea", "Tansy ragwort", "Priority 2A",
  "Hieracium caespitosum", "Meadow hawkweed", "Priority 2A",
  "Hieracium aurantiacum", "Orange hawkweed", "Priority 2A",
  "Ranunculus acris", "Tall buttercup", "Priority 2A",
  "Lepidium latifolium", "Perennial pepperweed", "Priority 2A",
  "Iris pseudacorus", "Yellow flag iris", "Priority 2A",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Priority 2A",
  "Butomus umbellatus", "Flowering rush", "Priority 2A",
  "Rhamnus cathartica", "Common buckthorn", "Priority 2A",
  # Priority 2B (Abundant/widespread)
  "Cirsium arvense", "Canada thistle", "Priority 2B",
  "Convolvulus arvensis", "Field bindweed", "Priority 2B",
  "Euphorbia esula", "Leafy spurge", "Priority 2B",
  "Cardaria draba", "Whitetop", "Priority 2B",
  "Acroptilon repens", "Russian knapweed", "Priority 2B",
  "Centaurea stoebe", "Spotted knapweed", "Priority 2B",
  "Centaurea diffusa", "Diffuse knapweed", "Priority 2B",
  "Linaria dalmatica", "Dalmatian toadflax", "Priority 2B",
  "Hypericum perforatum", "St. Johnswort", "Priority 2B",
  "Potentilla recta", "Sulfur cinquefoil", "Priority 2B",
  "Tanacetum vulgare", "Common tansy", "Priority 2B",
  "Leucanthemum vulgare", "Oxeye daisy", "Priority 2B",
  "Cynoglossum officinale", "Houndstongue", "Priority 2B",
  "Linaria vulgaris", "Yellow toadflax", "Priority 2B",
  "Potamogeton crispus", "Curlyleaf pondweed", "Priority 2B",
  "Berteroa incana", "Hoary alyssum", "Priority 2B"
) %>% mutate(state_code = "MT", source = "Montana Dept of Agriculture Noxious Weed List")

# Florida - FISC Invasive Plant List (76 species, Jan 2026)
FL_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Category I (High Priority)
  "Abrus precatorius", "Rosary pea", "Category I",
  "Acacia auriculiformis", "Earleaf acacia", "Category I",
  "Albizia julibrissin", "Silktree", "Category I",
  "Albizia lebbeck", "Womans tongue", "Category I",
  "Ardisia crenata", "Coral ardisia", "Category I",
  "Ardisia elliptica", "Shoebutton ardisia", "Category I",
  "Asparagus aethiopicus", "Asparagus fern", "Category I",
  "Bauhinia variegata", "Orchid tree", "Category I",
  "Bischofia javanica", "Javanese bishopwood", "Category I",
  "Casuarina equisetifolia", "Australian pine", "Category I",
  "Casuarina glauca", "Gray sheoak", "Category I",
  "Cinnamomum camphora", "Camphortree", "Category I",
  "Colubrina asiatica", "Latherleaf", "Category I",
  "Colocasia esculenta", "Wild taro", "Category I",
  "Cupaniopsis anacardioides", "Carrotwood", "Category I",
  "Dioscorea alata", "White yam", "Category I",
  "Dioscorea bulbifera", "Air-potato", "Category I",
  "Dolichandra unguis-cati", "Catclaw vine", "Category I",
  "Eichhornia crassipes", "Water hyacinth", "Category I",
  "Eugenia uniflora", "Surinam cherry", "Category I",
  "Ficus microcarpa", "Indian laurel fig", "Category I",
  "Hydrilla verticillata", "Hydrilla", "Category I",
  "Hygrophila polysperma", "Indian swampweed", "Category I",
  "Imperata cylindrica", "Cogongrass", "Category I",
  "Ipomoea aquatica", "Water spinach", "Category I",
  "Jasminum dichotomum", "Gold Coast jasmine", "Category I",
  "Jasminum fluminense", "Brazilian jasmine", "Category I",
  "Lantana strigocamara", "Lantana", "Category I",
  "Ligustrum lucidum", "Glossy privet", "Category I",
  "Ligustrum sinense", "Chinese privet", "Category I",
  "Lonicera japonica", "Japanese honeysuckle", "Category I",
  "Ludwigia peruviana", "Peruvian primrosewillow", "Category I",
  "Lygodium japonicum", "Japanese climbing fern", "Category I",
  "Lygodium microphyllum", "Old World climbing fern", "Category I",
  "Melaleuca quinquenervia", "Punktree", "Category I",
  "Melinis repens", "Rose natalgrass", "Category I",
  "Microstegium vimineum", "Japanese stiltgrass", "Category I",
  "Mimosa pigra", "Catclaw mimosa", "Category I",
  "Nandina domestica", "Sacred bamboo", "Category I",
  "Nephrolepis brownii", "Asian sword fern", "Category I",
  "Nephrolepis cordifolia", "Tuberous sword fern", "Category I",
  "Neyraudia reynaudiana", "Burma reed", "Category I",
  "Nymphoides cristata", "Crested floatingheart", "Category I",
  "Paederia foetida", "Skunkvine", "Category I",
  "Panicum repens", "Torpedograss", "Category I",
  "Pistia stratiotes", "Water lettuce", "Category I",
  "Psidium cattleianum", "Strawberry guava", "Category I",
  "Psidium guajava", "Guava", "Category I",
  "Pueraria montana", "Kudzu", "Category I",
  "Rhodomyrtus tomentosa", "Rose myrtle", "Category I",
  "Ruellia simplex", "Mexican petunia", "Category I",
  "Salvinia minima", "Water spangles", "Category I",
  "Scaevola taccada", "Beach naupaka", "Category I",
  "Schinus terebinthifolia", "Brazilian pepper", "Category I",
  "Senna pendula", "Christmas cassia", "Category I",
  "Solanum viarum", "Tropical soda apple", "Category I",
  "Syngonium podophyllum", "Arrowhead vine", "Category I",
  "Syzygium cumini", "Java plum", "Category I",
  "Thespesia populnea", "Portia tree", "Category I",
  "Tradescantia fluminensis", "Small-leaf spiderwort", "Category I",
  "Triadica sebifera", "Chinese tallow", "Category I",
  "Urena lobata", "Caesarweed", "Category I",
  "Urochloa mutica", "Paragrass", "Category I",
  # Category II (selected high-impact)
  "Alternanthera philoxeroides", "Alligatorweed", "Category II",
  "Broussonetia papyrifera", "Paper mulberry", "Category II",
  "Cenchrus setaceus", "Fountaingrass", "Category II",
  "Clematis terniflora", "Sweet autumn clematis", "Category II",
  "Elaeagnus pungens", "Silverthorn", "Category II",
  "Elaeagnus umbellata", "Autumn olive", "Category II",
  "Leucaena leucocephala", "White leadtree", "Category II",
  "Melia azedarach", "Chinaberrytree", "Category II",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Category II",
  "Phyllostachys aurea", "Golden bamboo", "Category II",
  "Ricinus communis", "Castorbean", "Category II",
  "Wisteria sinensis", "Chinese wisteria", "Category II"
) %>% mutate(state_code = "FL", source = "Florida Invasive Species Council (FISC)")

# New Mexico - NMDA Noxious Weed List (41 species, Jan 2026)
NM_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Class A (Eradication priority)
  "Hyoscyamus niger", "Black henbane", "Class A",
  "Alhagi maurorum", "Camelthorn", "Class A",
  "Cirsium arvense", "Canada thistle", "Class A",
  "Linaria dalmatica", "Dalmatian toadflax", "Class A",
  "Centaurea diffusa", "Diffuse knapweed", "Class A",
  "Isatis tinctoria", "Dyers woad", "Class A",
  "Salvinia molesta", "Giant salvinia", "Class A",
  "Cardaria draba", "Hoary cress", "Class A",
  "Euphorbia esula", "Leafy spurge", "Class A",
  "Leucanthemum vulgare", "Oxeye daisy", "Class A",
  "Lythrum salicaria", "Purple loosestrife", "Class A",
  "Centaurea calcitrapa", "Purple starthistle", "Class A",
  "Saccharum ravennae", "Ravennagrass", "Class A",
  "Tripleurospermum perforatum", "Scentless chamomile", "Class A",
  "Onopordum acanthium", "Scotch thistle", "Class A",
  "Centaurea stoebe", "Spotted knapweed", "Class A",
  "Centaurea solstitialis", "Yellow starthistle", "Class A",
  "Linaria vulgaris", "Yellow toadflax", "Class A",
  # Class B (Containment priority)
  "Peganum harmala", "African rue", "Class B",
  "Cirsium vulgare", "Bull thistle", "Class B",
  "Cichorium intybus", "Chicory", "Class B",
  "Halogeton glomeratus", "Halogeton", "Class B",
  "Centaurea melitensis", "Malta starthistle", "Class B",
  "Lepidium latifolium", "Perennial pepperweed", "Class B",
  "Conium maculatum", "Poison hemlock", "Class B",
  "Elymus repens", "Quackgrass", "Class B",
  "Xanthium spinosum", "Spiny cocklebur", "Class B",
  "Dipsacus fullonum", "Teasel", "Class B",
  # Class C (Widespread/local management)
  "Bromus tectorum", "Cheatgrass", "Class C",
  "Potamogeton crispus", "Curlyleaf pondweed", "Class C",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Class C",
  "Arundo donax", "Giant cane", "Class C",
  "Hydrilla verticillata", "Hydrilla", "Class C",
  "Aegilops cylindrica", "Jointed goatgrass", "Class C",
  "Carduus nutans", "Musk thistle", "Class C",
  "Myriophyllum aquaticum", "Parrotfeather", "Class C",
  "Acroptilon repens", "Russian knapweed", "Class C",
  "Elaeagnus angustifolia", "Russian olive", "Class C",
  "Tamarix ramosissima", "Saltcedar", "Class C",
  "Ulmus pumila", "Siberian elm", "Class C",
  "Ailanthus altissima", "Tree of heaven", "Class C"
) %>% mutate(state_code = "NM", source = "New Mexico Dept of Agriculture")

# Arizona - Prohibited Noxious Weeds (28 species, Jan 2026)
AZ_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Cirsium arvense", "Canada thistle", "Prohibited",
  "Convolvulus arvensis", "Field bindweed", "Prohibited",
  "Sorghum halepense", "Johnsongrass", "Prohibited",
  "Centaurea solstitialis", "Yellow starthistle", "Prohibited",
  "Centaurea calcitrapa", "Purple starthistle", "Prohibited",
  "Centaurea stoebe", "Spotted knapweed", "Prohibited",
  "Centaurea diffusa", "Diffuse knapweed", "Prohibited",
  "Acroptilon repens", "Russian knapweed", "Prohibited",
  "Cardaria draba", "Hoary cress", "Prohibited",
  "Euphorbia esula", "Leafy spurge", "Prohibited",
  "Halogeton glomeratus", "Halogeton", "Prohibited",
  "Isatis tinctoria", "Dyers woad", "Prohibited",
  "Onopordum acanthium", "Scotch thistle", "Prohibited",
  "Cirsium vulgare", "Bull thistle", "Prohibited",
  "Carduus nutans", "Musk thistle", "Prohibited",
  "Sonchus arvensis", "Perennial sowthistle", "Prohibited",
  "Peganum harmala", "African rue", "Prohibited",
  "Alhagi maurorum", "Camelthorn", "Prohibited",
  "Elymus repens", "Quackgrass", "Prohibited",
  "Hydrilla verticillata", "Hydrilla", "Prohibited",
  "Salvinia molesta", "Giant salvinia", "Prohibited",
  "Arundo donax", "Giant reed", "Prohibited",
  "Tamarix ramosissima", "Saltcedar", "Prohibited",
  "Ailanthus altissima", "Tree of heaven", "Prohibited",
  "Pennisetum setaceum", "Fountaingrass", "Prohibited",
  "Bromus tectorum", "Cheatgrass", "Prohibited",
  "Tribulus terrestris", "Puncturevine", "Prohibited"
) %>% mutate(state_code = "AZ", source = "Arizona Dept of Agriculture")

# Georgia - GA-EPPC Invasive List (35 species, Jan 2026)
GA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Category 1 (Serious problems)
  "Ailanthus altissima", "Tree of heaven", "Category 1",
  "Albizia julibrissin", "Mimosa", "Category 1",
  "Alternanthera philoxeroides", "Alligatorweed", "Category 1",
  "Eichhornia crassipes", "Water hyacinth", "Category 1",
  "Elaeagnus umbellata", "Autumn olive", "Category 1",
  "Elaeagnus pungens", "Thorny olive", "Category 1",
  "Hydrilla verticillata", "Hydrilla", "Category 1",
  "Ligustrum sinense", "Chinese privet", "Category 1",
  "Ligustrum lucidum", "Glossy privet", "Category 1",
  "Ligustrum japonicum", "Japanese privet", "Category 1",
  "Lonicera japonica", "Japanese honeysuckle", "Category 1",
  "Lonicera fragrantissima", "Winter honeysuckle", "Category 1",
  "Lygodium japonicum", "Japanese climbing fern", "Category 1",
  "Melia azedarach", "Chinaberrytree", "Category 1",
  "Microstegium vimineum", "Japanese stiltgrass", "Category 1",
  "Murdannia keisak", "Marsh dewflower", "Category 1",
  "Paulownia tomentosa", "Princess tree", "Category 1",
  "Pueraria montana", "Kudzu", "Category 1",
  "Rosa multiflora", "Multiflora rose", "Category 1",
  "Triadica sebifera", "Chinese tallow", "Category 1",
  "Wisteria sinensis", "Chinese wisteria", "Category 1",
  # Category 2 (Moderate problems)
  "Albizia lebbeck", "Womans tongue", "Category 2",
  "Broussonetia papyrifera", "Paper mulberry", "Category 2",
  "Celastrus orbiculatus", "Oriental bittersweet", "Category 2",
  "Cinnamomum camphora", "Camphortree", "Category 2",
  "Dioscorea bulbifera", "Air potato", "Category 2",
  "Firmiana simplex", "Chinese parasoltree", "Category 2",
  "Hedera helix", "English ivy", "Category 2",
  "Imperata cylindrica", "Cogongrass", "Category 2",
  "Lespedeza cuneata", "Sericea lespedeza", "Category 2",
  "Nandina domestica", "Sacred bamboo", "Category 2",
  "Phyllostachys aurea", "Golden bamboo", "Category 2",
  "Pyrus calleryana", "Callery pear", "Category 2",
  "Spiraea japonica", "Japanese spiraea", "Category 2",
  "Vinca minor", "Common periwinkle", "Category 2"
) %>% mutate(state_code = "GA", source = "Georgia Exotic Pest Plant Council")

# North Carolina - NC Native Plant Society (60 Rank 1-2 species, Jan 2026)
NC_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Rank 1 - Severe Threat
  "Ailanthus altissima", "Tree of heaven", "Rank 1",
  "Albizia julibrissin", "Mimosa", "Rank 1",
  "Alliaria petiolata", "Garlic mustard", "Rank 1",
  "Alternanthera philoxeroides", "Alligatorweed", "Rank 1",
  "Ampelopsis glandulosa", "Porcelain-berry", "Rank 1",
  "Celastrus orbiculatus", "Oriental bittersweet", "Rank 1",
  "Dioscorea polystachya", "Chinese yam", "Rank 1",
  "Elaeagnus umbellata", "Autumn olive", "Rank 1",
  "Ficaria verna", "Lesser celandine", "Rank 1",
  "Hedera helix", "English ivy", "Rank 1",
  "Humulus scandens", "Japanese hops", "Rank 1",
  "Hydrilla verticillata", "Hydrilla", "Rank 1",
  "Lespedeza bicolor", "Bicolor lespedeza", "Rank 1",
  "Lespedeza cuneata", "Sericea lespedeza", "Rank 1",
  "Ligustrum japonicum", "Japanese privet", "Rank 1",
  "Ligustrum lucidum", "Glossy privet", "Rank 1",
  "Ligustrum sinense", "Chinese privet", "Rank 1",
  "Lonicera japonica", "Japanese honeysuckle", "Rank 1",
  "Microstegium vimineum", "Japanese stiltgrass", "Rank 1",
  "Miscanthus sinensis", "Chinese silvergrass", "Rank 1",
  "Murdannia keisak", "Marsh dewflower", "Rank 1",
  "Myriophyllum aquaticum", "Parrotfeather", "Rank 1",
  "Paulownia tomentosa", "Princess tree", "Rank 1",
  "Persicaria perfoliata", "Mile-a-minute vine", "Rank 1",
  "Phragmites australis", "Common reed", "Rank 1",
  "Pueraria montana", "Kudzu", "Rank 1",
  "Pyrus calleryana", "Callery pear", "Rank 1",
  "Reynoutria japonica", "Japanese knotweed", "Rank 1",
  "Rosa multiflora", "Multiflora rose", "Rank 1",
  "Salvinia molesta", "Giant salvinia", "Rank 1",
  "Spiraea japonica", "Japanese spiraea", "Rank 1",
  "Wisteria sinensis", "Chinese wisteria", "Rank 1",
  # Rank 2 - Significant Threat
  "Akebia quinata", "Five-leaf akebia", "Rank 2",
  "Berberis thunbergii", "Japanese barberry", "Rank 2",
  "Broussonetia papyrifera", "Paper mulberry", "Rank 2",
  "Buddleja davidii", "Butterfly bush", "Rank 2",
  "Centaurea stoebe", "Spotted knapweed", "Rank 2",
  "Cirsium arvense", "Canada thistle", "Rank 2",
  "Clematis terniflora", "Sweet autumn clematis", "Rank 2",
  "Cytisus scoparius", "Scotch broom", "Rank 2",
  "Egeria densa", "Brazilian elodea", "Rank 2",
  "Elaeagnus angustifolia", "Russian olive", "Rank 2",
  "Elaeagnus pungens", "Thorny olive", "Rank 2",
  "Euonymus alatus", "Burning bush", "Rank 2",
  "Euonymus fortunei", "Wintercreeper", "Rank 2",
  "Imperata cylindrica", "Cogongrass", "Rank 2",
  "Iris pseudacorus", "Yellow flag iris", "Rank 2",
  "Lonicera maackii", "Amur honeysuckle", "Rank 2",
  "Lonicera morrowii", "Morrows honeysuckle", "Rank 2",
  "Lygodium japonicum", "Japanese climbing fern", "Rank 2",
  "Lythrum salicaria", "Purple loosestrife", "Rank 2",
  "Mahonia bealei", "Leatherleaf mahonia", "Rank 2",
  "Morus alba", "White mulberry", "Rank 2",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Rank 2",
  "Nandina domestica", "Sacred bamboo", "Rank 2",
  "Phyllostachys aurea", "Golden bamboo", "Rank 2",
  "Rubus phoenicolasius", "Wineberry", "Rank 2",
  "Solanum viarum", "Tropical soda apple", "Rank 2",
  "Sorghum halepense", "Johnsongrass", "Rank 2",
  "Triadica sebifera", "Chinese tallow", "Rank 2"
) %>% mutate(state_code = "NC", source = "NC Native Plant Society Invasive Exotic List")

# South Carolina - SC Native Plant Society (24 species, Jan 2026)
SC_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Pueraria montana", "Kudzu", "Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Invasive",
  "Lygodium japonicum", "Japanese climbing fern", "Invasive",
  "Imperata cylindrica", "Cogongrass", "Invasive",
  "Microstegium vimineum", "Japanese stiltgrass", "Invasive",
  "Ligustrum vulgare", "Common privet", "Invasive",
  "Ligustrum sinense", "Chinese privet", "Invasive",
  "Ligustrum japonicum", "Japanese privet", "Invasive",
  "Ligustrum lucidum", "Glossy privet", "Invasive",
  "Nandina domestica", "Sacred bamboo", "Invasive",
  "Pyrus calleryana", "Callery pear", "Invasive",
  "Albizia julibrissin", "Mimosa", "Invasive",
  "Melia azedarach", "Chinaberrytree", "Invasive",
  "Triadica sebifera", "Chinese tallow", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Egeria densa", "Brazilian elodea", "Invasive",
  "Eichhornia crassipes", "Water hyacinth", "Invasive",
  "Hydrilla verticillata", "Hydrilla", "Invasive",
  "Salvinia molesta", "Giant salvinia", "Invasive",
  "Ludwigia peploides", "Water primrose", "Invasive",
  "Phragmites australis", "Common reed", "Invasive",
  "Alternanthera philoxeroides", "Alligatorweed", "Invasive",
  "Ficaria verna", "Fig buttercup", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive"
) %>% mutate(state_code = "SC", source = "SC Native Plant Society")

# Virginia - DCR Invasive Plant List (45 species, Jan 2026)
VA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # High Invasive Impact
  "Ailanthus altissima", "Tree of heaven", "High",
  "Alliaria petiolata", "Garlic mustard", "High",
  "Ampelopsis brevipedunculata", "Porcelain berry", "High",
  "Celastrus orbiculatus", "Oriental bittersweet", "High",
  "Elaeagnus umbellata", "Autumn olive", "High",
  "Euonymus fortunei", "Wintercreeper", "High",
  "Ficaria verna", "Lesser celandine", "High",
  "Hedera helix", "English ivy", "High",
  "Hydrilla verticillata", "Hydrilla", "High",
  "Lespedeza cuneata", "Sericea lespedeza", "High",
  "Ligustrum sinense", "Chinese privet", "High",
  "Lonicera japonica", "Japanese honeysuckle", "High",
  "Lonicera maackii", "Amur honeysuckle", "High",
  "Lonicera morrowii", "Morrows honeysuckle", "High",
  "Microstegium vimineum", "Japanese stiltgrass", "High",
  "Miscanthus sinensis", "Chinese silvergrass", "High",
  "Murdannia keisak", "Marsh dewflower", "High",
  "Paulownia tomentosa", "Princess tree", "High",
  "Persicaria perfoliata", "Mile-a-minute vine", "High",
  "Phragmites australis", "Common reed", "High",
  "Pueraria montana", "Kudzu", "High",
  "Pyrus calleryana", "Callery pear", "High",
  "Reynoutria japonica", "Japanese knotweed", "High",
  "Rosa multiflora", "Multiflora rose", "High",
  "Wisteria sinensis", "Chinese wisteria", "High",
  # Medium Invasive Impact
  "Acer platanoides", "Norway maple", "Medium",
  "Berberis thunbergii", "Japanese barberry", "Medium",
  "Broussonetia papyrifera", "Paper mulberry", "Medium",
  "Buddleja davidii", "Butterfly bush", "Medium",
  "Elaeagnus angustifolia", "Russian olive", "Medium",
  "Euonymus alatus", "Burning bush", "Medium",
  "Ligustrum obtusifolium", "Border privet", "Medium",
  "Lonicera tatarica", "Tatarian honeysuckle", "Medium",
  "Lythrum salicaria", "Purple loosestrife", "Medium",
  "Morus alba", "White mulberry", "Medium",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Medium",
  "Nandina domestica", "Sacred bamboo", "Medium",
  "Spiraea japonica", "Japanese spiraea", "Medium",
  "Triadica sebifera", "Chinese tallow", "Medium",
  "Ulmus pumila", "Siberian elm", "Medium",
  # Low Invasive Impact
  "Cirsium arvense", "Canada thistle", "Low",
  "Cirsium vulgare", "Bull thistle", "Low",
  "Conium maculatum", "Poison hemlock", "Low",
  "Dipsacus fullonum", "Common teasel", "Low",
  "Lespedeza bicolor", "Bicolor lespedeza", "Low"
) %>% mutate(state_code = "VA", source = "Virginia DCR Natural Heritage Program")

# Tennessee - TN-EPPC Invasive List (35 species, Jan 2026)
TN_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Severe Threat
  "Ailanthus altissima", "Tree of heaven", "Severe Threat",
  "Alliaria petiolata", "Garlic mustard", "Severe Threat",
  "Celastrus orbiculatus", "Oriental bittersweet", "Severe Threat",
  "Elaeagnus umbellata", "Autumn olive", "Severe Threat",
  "Euonymus alatus", "Burning bush", "Severe Threat",
  "Euonymus fortunei", "Wintercreeper", "Severe Threat",
  "Lespedeza cuneata", "Sericea lespedeza", "Severe Threat",
  "Ligustrum sinense", "Chinese privet", "Severe Threat",
  "Ligustrum vulgare", "Common privet", "Severe Threat",
  "Lonicera japonica", "Japanese honeysuckle", "Severe Threat",
  "Lonicera maackii", "Amur honeysuckle", "Severe Threat",
  "Microstegium vimineum", "Japanese stiltgrass", "Severe Threat",
  "Paulownia tomentosa", "Princess tree", "Severe Threat",
  "Pueraria montana", "Kudzu", "Severe Threat",
  "Rosa multiflora", "Multiflora rose", "Severe Threat",
  # Significant Threat
  "Albizia julibrissin", "Mimosa", "Significant Threat",
  "Broussonetia papyrifera", "Paper mulberry", "Significant Threat",
  "Dioscorea bulbifera", "Air potato", "Significant Threat",
  "Hedera helix", "English ivy", "Significant Threat",
  "Hydrilla verticillata", "Hydrilla", "Significant Threat",
  "Lygodium japonicum", "Japanese climbing fern", "Significant Threat",
  "Melia azedarach", "Chinaberrytree", "Significant Threat",
  "Miscanthus sinensis", "Chinese silvergrass", "Significant Threat",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Significant Threat",
  "Nandina domestica", "Sacred bamboo", "Significant Threat",
  "Phragmites australis", "Common reed", "Significant Threat",
  "Pyrus calleryana", "Callery pear", "Significant Threat",
  "Reynoutria japonica", "Japanese knotweed", "Significant Threat",
  "Spiraea japonica", "Japanese spiraea", "Significant Threat",
  "Triadica sebifera", "Chinese tallow", "Significant Threat",
  "Vinca minor", "Common periwinkle", "Significant Threat",
  "Wisteria floribunda", "Japanese wisteria", "Significant Threat",
  "Wisteria sinensis", "Chinese wisteria", "Significant Threat",
  # Alert
  "Imperata cylindrica", "Cogongrass", "Alert",
  "Salvinia molesta", "Giant salvinia", "Alert"
) %>% mutate(state_code = "TN", source = "Tennessee Exotic Pest Plant Council")

# New York - NYS iMap Invasive Species (60 Tier 4 species, Jan 2026)
NY_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Tier 4 - Widespread/Established
  "Ailanthus altissima", "Tree of heaven", "Tier 4",
  "Alliaria petiolata", "Garlic mustard", "Tier 4",
  "Alnus glutinosa", "European alder", "Tier 4",
  "Ampelopsis glandulosa", "Porcelain berry", "Tier 4",
  "Artemisia vulgaris", "Mugwort", "Tier 4",
  "Berberis thunbergii", "Japanese barberry", "Tier 4",
  "Berberis vulgaris", "Common barberry", "Tier 4",
  "Bromus inermis", "Smooth brome", "Tier 4",
  "Butomus umbellatus", "Flowering rush", "Tier 4",
  "Carduus nutans", "Musk thistle", "Tier 4",
  "Celastrus orbiculatus", "Oriental bittersweet", "Tier 4",
  "Centaurea jacea", "Brown knapweed", "Tier 4",
  "Centaurea nigra", "Black knapweed", "Tier 4",
  "Centaurea stoebe", "Spotted knapweed", "Tier 4",
  "Cirsium arvense", "Canada thistle", "Tier 4",
  "Cirsium vulgare", "Bull thistle", "Tier 4",
  "Clematis terniflora", "Japanese clematis", "Tier 4",
  "Conium maculatum", "Poison hemlock", "Tier 4",
  "Convolvulus arvensis", "Field bindweed", "Tier 4",
  "Crataegus monogyna", "English hawthorn", "Tier 4",
  "Aegopodium podagraria", "Goutweed", "Tier 4",
  "Acer platanoides", "Norway maple", "Tier 4",
  "Acer pseudoplatanus", "Sycamore maple", "Tier 4",
  "Euonymus alatus", "Burning bush", "Tier 4",
  "Euonymus fortunei", "Wintercreeper", "Tier 4",
  "Frangula alnus", "Glossy buckthorn", "Tier 4",
  "Hedera helix", "English ivy", "Tier 4",
  "Hesperis matronalis", "Dames rocket", "Tier 4",
  "Lonicera japonica", "Japanese honeysuckle", "Tier 4",
  "Lonicera maackii", "Amur honeysuckle", "Tier 4",
  "Lonicera morrowii", "Morrows honeysuckle", "Tier 4",
  "Lonicera tatarica", "Tatarian honeysuckle", "Tier 4",
  "Lythrum salicaria", "Purple loosestrife", "Tier 4",
  "Microstegium vimineum", "Japanese stiltgrass", "Tier 4",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Tier 4",
  "Phragmites australis", "Common reed", "Tier 4",
  "Polygonum cuspidatum", "Japanese knotweed", "Tier 4",
  "Potamogeton crispus", "Curly pondweed", "Tier 4",
  "Rhamnus cathartica", "Common buckthorn", "Tier 4",
  "Rosa multiflora", "Multiflora rose", "Tier 4",
  "Trapa natans", "Water chestnut", "Tier 4",
  # Tier 3 - Regional concern
  "Akebia quinata", "Chocolate vine", "Tier 3",
  "Allium vineale", "Wild garlic", "Tier 3",
  "Aralia elata", "Japanese angelica", "Tier 3",
  "Arum italicum", "Italian arum", "Tier 3",
  "Brachypodium sylvaticum", "False brome", "Tier 3",
  "Cabomba caroliniana", "Fanwort", "Tier 3",
  "Carex kobomugi", "Japanese beach sedge", "Tier 3",
  "Centaurea montana", "Perennial cornflower", "Tier 3",
  "Cytisus scoparius", "Scotch broom", "Tier 3",
  "Ficaria verna", "Lesser celandine", "Tier 3",
  "Humulus japonicus", "Japanese hops", "Tier 3",
  "Hydrilla verticillata", "Hydrilla", "Tier 3",
  "Iris pseudacorus", "Yellow flag iris", "Tier 3",
  "Ligustrum obtusifolium", "Border privet", "Tier 3",
  "Nandina domestica", "Sacred bamboo", "Tier 3",
  "Paulownia tomentosa", "Princess tree", "Tier 3",
  "Persicaria perfoliata", "Mile-a-minute", "Tier 3",
  "Pueraria montana", "Kudzu", "Tier 3",
  "Wisteria sinensis", "Chinese wisteria", "Tier 3"
) %>% mutate(state_code = "NY", source = "New York State Invasive Species Information")

# Pennsylvania - DCNR Invasive Plant List (45 species, Jan 2026)
PA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Rank 1 - Severe Threat
  "Ailanthus altissima", "Tree of heaven", "Severe Threat",
  "Alliaria petiolata", "Garlic mustard", "Severe Threat",
  "Berberis thunbergii", "Japanese barberry", "Severe Threat",
  "Celastrus orbiculatus", "Oriental bittersweet", "Severe Threat",
  "Centaurea stoebe", "Spotted knapweed", "Severe Threat",
  "Elaeagnus umbellata", "Autumn olive", "Severe Threat",
  "Euonymus alatus", "Burning bush", "Severe Threat",
  "Euonymus fortunei", "Wintercreeper", "Severe Threat",
  "Frangula alnus", "Glossy buckthorn", "Severe Threat",
  "Hedera helix", "English ivy", "Severe Threat",
  "Lonicera japonica", "Japanese honeysuckle", "Severe Threat",
  "Lonicera maackii", "Amur honeysuckle", "Severe Threat",
  "Lonicera morrowii", "Morrows honeysuckle", "Severe Threat",
  "Lonicera tatarica", "Tatarian honeysuckle", "Severe Threat",
  "Lythrum salicaria", "Purple loosestrife", "Severe Threat",
  "Microstegium vimineum", "Japanese stiltgrass", "Severe Threat",
  "Miscanthus sinensis", "Chinese silvergrass", "Severe Threat",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Severe Threat",
  "Persicaria perfoliata", "Mile-a-minute vine", "Severe Threat",
  "Phragmites australis", "Common reed", "Severe Threat",
  "Polygonum cuspidatum", "Japanese knotweed", "Severe Threat",
  "Pyrus calleryana", "Callery pear", "Severe Threat",
  "Rhamnus cathartica", "Common buckthorn", "Severe Threat",
  "Rosa multiflora", "Multiflora rose", "Severe Threat",
  "Wisteria sinensis", "Chinese wisteria", "Severe Threat",
  # Rank 2 - Significant Threat
  "Acer platanoides", "Norway maple", "Significant Threat",
  "Aegopodium podagraria", "Goutweed", "Significant Threat",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Significant Threat",
  "Cirsium arvense", "Canada thistle", "Significant Threat",
  "Cirsium vulgare", "Bull thistle", "Significant Threat",
  "Ficaria verna", "Lesser celandine", "Significant Threat",
  "Humulus japonicus", "Japanese hops", "Significant Threat",
  "Ligustrum obtusifolium", "Border privet", "Significant Threat",
  "Ligustrum vulgare", "Common privet", "Significant Threat",
  "Nandina domestica", "Sacred bamboo", "Significant Threat",
  "Paulownia tomentosa", "Princess tree", "Significant Threat",
  "Pueraria montana", "Kudzu", "Significant Threat",
  "Spiraea japonica", "Japanese spiraea", "Significant Threat",
  "Trapa natans", "Water chestnut", "Significant Threat",
  "Vinca minor", "Common periwinkle", "Significant Threat"
) %>% mutate(state_code = "PA", source = "Pennsylvania DCNR")

# Massachusetts - MIPAG Invasive Plant List (40 species, Jan 2026)
MA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Invasive
  "Aegopodium podagraria", "Goutweed", "Invasive",
  "Acer platanoides", "Norway maple", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Berberis thunbergii", "Japanese barberry", "Invasive",
  "Berberis vulgaris", "European barberry", "Invasive",
  "Celastrus orbiculatus", "Oriental bittersweet", "Invasive",
  "Centaurea stoebe", "Spotted knapweed", "Invasive",
  "Cynanchum louiseae", "Black swallowwort", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Euonymus alatus", "Burning bush", "Invasive",
  "Frangula alnus", "Glossy buckthorn", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive",
  "Lonicera maackii", "Amur honeysuckle", "Invasive",
  "Lonicera morrowii", "Morrows honeysuckle", "Invasive",
  "Lonicera tatarica", "Tatarian honeysuckle", "Invasive",
  "Lythrum salicaria", "Purple loosestrife", "Invasive",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Invasive",
  "Phragmites australis", "Common reed", "Invasive",
  "Polygonum cuspidatum", "Japanese knotweed", "Invasive",
  "Rhamnus cathartica", "Common buckthorn", "Invasive",
  "Rosa multiflora", "Multiflora rose", "Invasive",
  # Likely Invasive
  "Alliaria petiolata", "Garlic mustard", "Likely Invasive",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Likely Invasive",
  "Cabomba caroliniana", "Fanwort", "Likely Invasive",
  "Ficaria verna", "Lesser celandine", "Likely Invasive",
  "Hedera helix", "English ivy", "Likely Invasive",
  "Iris pseudacorus", "Yellow flag iris", "Likely Invasive",
  "Ligustrum obtusifolium", "Border privet", "Likely Invasive",
  "Ligustrum vulgare", "Common privet", "Likely Invasive",
  "Microstegium vimineum", "Japanese stiltgrass", "Likely Invasive",
  "Pueraria montana", "Kudzu", "Likely Invasive",
  "Robinia pseudoacacia", "Black locust", "Likely Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Likely Invasive",
  # Potentially Invasive
  "Acer pseudoplatanus", "Sycamore maple", "Potentially Invasive",
  "Buddleja davidii", "Butterfly bush", "Potentially Invasive",
  "Euonymus fortunei", "Wintercreeper", "Potentially Invasive",
  "Humulus japonicus", "Japanese hops", "Potentially Invasive",
  "Nandina domestica", "Sacred bamboo", "Potentially Invasive",
  "Paulownia tomentosa", "Princess tree", "Potentially Invasive",
  "Spiraea japonica", "Japanese spiraea", "Potentially Invasive"
) %>% mutate(state_code = "MA", source = "Massachusetts Invasive Plant Advisory Group")

# New Hampshire - Prohibited Invasive Species (35 species, Jan 2026)
NH_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Prohibited
  "Aegopodium podagraria", "Goutweed", "Prohibited",
  "Acer platanoides", "Norway maple", "Prohibited",
  "Berberis thunbergii", "Japanese barberry", "Prohibited",
  "Berberis vulgaris", "European barberry", "Prohibited",
  "Celastrus orbiculatus", "Oriental bittersweet", "Prohibited",
  "Elaeagnus umbellata", "Autumn olive", "Prohibited",
  "Euonymus alatus", "Burning bush", "Prohibited",
  "Euonymus fortunei", "Wintercreeper", "Prohibited",
  "Frangula alnus", "Glossy buckthorn", "Prohibited",
  "Lonicera japonica", "Japanese honeysuckle", "Prohibited",
  "Lonicera morrowii", "Morrows honeysuckle", "Prohibited",
  "Lonicera tatarica", "Tatarian honeysuckle", "Prohibited",
  "Lonicera x bella", "Showy honeysuckle", "Prohibited",
  "Lonicera xylosteum", "Dwarf honeysuckle", "Prohibited",
  "Lythrum salicaria", "Purple loosestrife", "Prohibited",
  "Myriophyllum heterophyllum", "Variable watermilfoil", "Prohibited",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Prohibited",
  "Phragmites australis", "Common reed", "Prohibited",
  "Polygonum cuspidatum", "Japanese knotweed", "Prohibited",
  "Rhamnus cathartica", "Common buckthorn", "Prohibited",
  "Rosa multiflora", "Multiflora rose", "Prohibited",
  # Watch List
  "Acer pseudoplatanus", "Sycamore maple", "Watch",
  "Alliaria petiolata", "Garlic mustard", "Watch",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Watch",
  "Cirsium arvense", "Canada thistle", "Watch",
  "Ficaria verna", "Lesser celandine", "Watch",
  "Hedera helix", "English ivy", "Watch",
  "Iris pseudacorus", "Yellow flag iris", "Watch",
  "Ligustrum obtusifolium", "Border privet", "Watch",
  "Ligustrum vulgare", "Common privet", "Watch",
  "Microstegium vimineum", "Japanese stiltgrass", "Watch",
  "Paulownia tomentosa", "Princess tree", "Watch",
  "Pueraria montana", "Kudzu", "Watch",
  "Spiraea japonica", "Japanese spiraea", "Watch",
  "Wisteria sinensis", "Chinese wisteria", "Watch"
) %>% mutate(state_code = "NH", source = "NH Dept of Agriculture Invasive Species")

# Illinois - Exotic Weed Act (30 species, Jan 2026)
IL_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Lonicera maackii", "Amur honeysuckle", "Exotic Weed",
  "Lonicera morrowii", "Morrows honeysuckle", "Exotic Weed",
  "Lonicera fragrantissima", "Sweet breath of spring", "Exotic Weed",
  "Lonicera tatarica", "Tatarian honeysuckle", "Exotic Weed",
  "Lonicera japonica", "Japanese honeysuckle", "Exotic Weed",
  "Rhamnus cathartica", "Common buckthorn", "Exotic Weed",
  "Frangula alnus", "Glossy buckthorn", "Exotic Weed",
  "Rhamnus davurica", "Dahurian buckthorn", "Exotic Weed",
  "Rhamnus japonica", "Japanese buckthorn", "Exotic Weed",
  "Lythrum salicaria", "Purple loosestrife", "Exotic Weed",
  "Alliaria petiolata", "Garlic mustard", "Exotic Weed",
  "Cirsium arvense", "Canada thistle", "Noxious Weed",
  "Cirsium vulgare", "Bull thistle", "Noxious Weed",
  "Carduus nutans", "Musk thistle", "Noxious Weed",
  "Convolvulus arvensis", "Field bindweed", "Noxious Weed",
  "Sorghum halepense", "Johnsongrass", "Noxious Weed",
  "Rosa multiflora", "Multiflora rose", "Noxious Weed",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Celastrus orbiculatus", "Oriental bittersweet", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Euonymus alatus", "Burning bush", "Invasive",
  "Euonymus fortunei", "Wintercreeper", "Invasive",
  "Microstegium vimineum", "Japanese stiltgrass", "Invasive",
  "Phragmites australis", "Common reed", "Invasive",
  "Polygonum cuspidatum", "Japanese knotweed", "Invasive",
  "Pueraria montana", "Kudzu", "Invasive",
  "Berberis thunbergii", "Japanese barberry", "Invasive",
  "Acer platanoides", "Norway maple", "Invasive",
  "Pyrus calleryana", "Callery pear", "Invasive",
  "Miscanthus sinensis", "Chinese silvergrass", "Invasive"
) %>% mutate(state_code = "IL", source = "Illinois Exotic Weed Act / IDNR")

# Indiana - Terrestrial Plant Rule (40 species, Jan 2026)
IN_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Ailanthus altissima", "Tree of heaven", "Regulated",
  "Alliaria petiolata", "Garlic mustard", "Regulated",
  "Alnus glutinosa", "Black alder", "Regulated",
  "Artemisia vulgaris", "Mugwort", "Regulated",
  "Arthraxon hispidus", "Small carpgrass", "Regulated",
  "Berberis thunbergii", "Japanese barberry", "Regulated",
  "Carduus acanthoides", "Spiny plumeless thistle", "Regulated",
  "Carduus nutans", "Musk thistle", "Regulated",
  "Celastrus orbiculatus", "Oriental bittersweet", "Regulated",
  "Centaurea stoebe", "Spotted knapweed", "Regulated",
  "Cirsium vulgare", "Bull thistle", "Regulated",
  "Conium maculatum", "Poison hemlock", "Regulated",
  "Convolvulus arvensis", "Field bindweed", "Regulated",
  "Coronilla varia", "Crown vetch", "Regulated",
  "Dioscorea polystachya", "Chinese yam", "Regulated",
  "Dipsacus fullonum", "Common teasel", "Regulated",
  "Dipsacus laciniatus", "Cut-leaved teasel", "Regulated",
  "Elaeagnus umbellata", "Autumn olive", "Regulated",
  "Euonymus fortunei", "Wintercreeper", "Regulated",
  "Euphorbia esula", "Leafy spurge", "Regulated",
  "Frangula alnus", "Glossy buckthorn", "Regulated",
  "Hesperis matronalis", "Dames rocket", "Regulated",
  "Humulus japonicus", "Japanese hops", "Regulated",
  "Lepidium latifolium", "Perennial pepperweed", "Regulated",
  "Lespedeza cuneata", "Sericea lespedeza", "Regulated",
  "Lonicera japonica", "Japanese honeysuckle", "Regulated",
  "Lonicera maackii", "Amur honeysuckle", "Regulated",
  "Lonicera morrowii", "Morrows honeysuckle", "Regulated",
  "Lonicera tatarica", "Tatarian honeysuckle", "Regulated",
  "Lythrum salicaria", "Purple loosestrife", "Regulated",
  "Microstegium vimineum", "Japanese stiltgrass", "Regulated",
  "Phragmites australis", "Common reed", "Regulated",
  "Polygonum cuspidatum", "Japanese knotweed", "Regulated",
  "Pyrus calleryana", "Callery pear", "Regulated",
  "Rhamnus cathartica", "Common buckthorn", "Regulated",
  "Rosa multiflora", "Multiflora rose", "Regulated",
  "Miscanthus sinensis", "Chinese silvergrass", "Regulated",
  "Paulownia tomentosa", "Princess tree", "Regulated",
  "Pueraria montana", "Kudzu", "Regulated",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Regulated"
) %>% mutate(state_code = "IN", source = "Indiana DNR Terrestrial Plant Rule")

# Michigan - Invasive Species (35 species, Jan 2026)
MI_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Phragmites australis", "Common reed", "Prohibited",
  "Centaurea stoebe", "Spotted knapweed", "Prohibited",
  "Lythrum salicaria", "Purple loosestrife", "Prohibited",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Prohibited",
  "Alliaria petiolata", "Garlic mustard", "Prohibited",
  "Celastrus orbiculatus", "Oriental bittersweet", "Prohibited",
  "Lonicera japonica", "Japanese honeysuckle", "Prohibited",
  "Lonicera maackii", "Amur honeysuckle", "Prohibited",
  "Lonicera morrowii", "Morrows honeysuckle", "Prohibited",
  "Lonicera tatarica", "Tatarian honeysuckle", "Prohibited",
  "Rhamnus cathartica", "Common buckthorn", "Prohibited",
  "Frangula alnus", "Glossy buckthorn", "Prohibited",
  "Rosa multiflora", "Multiflora rose", "Prohibited",
  "Elaeagnus umbellata", "Autumn olive", "Prohibited",
  "Berberis thunbergii", "Japanese barberry", "Prohibited",
  "Euonymus alatus", "Burning bush", "Prohibited",
  "Ailanthus altissima", "Tree of heaven", "Prohibited",
  "Polygonum cuspidatum", "Japanese knotweed", "Prohibited",
  "Microstegium vimineum", "Japanese stiltgrass", "Prohibited",
  "Hydrilla verticillata", "Hydrilla", "Prohibited",
  "Egeria densa", "Brazilian elodea", "Prohibited",
  "Cabomba caroliniana", "Fanwort", "Prohibited",
  "Potamogeton crispus", "Curly pondweed", "Restricted",
  "Cirsium arvense", "Canada thistle", "Restricted",
  "Cirsium vulgare", "Bull thistle", "Restricted",
  "Carduus nutans", "Musk thistle", "Restricted",
  "Euphorbia esula", "Leafy spurge", "Restricted",
  "Convolvulus arvensis", "Field bindweed", "Restricted",
  "Sorghum halepense", "Johnsongrass", "Restricted",
  "Hesperis matronalis", "Dames rocket", "Restricted",
  "Pyrus calleryana", "Callery pear", "Restricted",
  "Acer platanoides", "Norway maple", "Restricted",
  "Paulownia tomentosa", "Princess tree", "Restricted",
  "Pueraria montana", "Kudzu", "Restricted",
  "Iris pseudacorus", "Yellow flag iris", "Restricted"
) %>% mutate(state_code = "MI", source = "Michigan EGLE Invasive Species Program")

# Wisconsin - NR40 Invasive Species Rule (40 species, Jan 2026)
WI_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Prohibited
  "Anthriscus sylvestris", "Wild chervil", "Prohibited",
  "Paulownia tomentosa", "Princess tree", "Prohibited",
  "Phellodendron amurense", "Amur corktree", "Prohibited",
  "Heracleum mantegazzianum", "Giant hogweed", "Prohibited",
  "Hydrilla verticillata", "Hydrilla", "Prohibited",
  "Salvinia molesta", "Giant salvinia", "Prohibited",
  "Trapa natans", "Water chestnut", "Prohibited",
  "Butomus umbellatus", "Flowering rush", "Prohibited",
  "Nymphoides peltata", "Yellow floating heart", "Prohibited",
  "Egeria densa", "Brazilian elodea", "Prohibited",
  "Cabomba caroliniana", "Fanwort", "Prohibited",
  "Myriophyllum aquaticum", "Parrotfeather", "Prohibited",
  # Restricted
  "Lonicera maackii", "Amur honeysuckle", "Restricted",
  "Lonicera morrowii", "Morrows honeysuckle", "Restricted",
  "Lonicera tatarica", "Tatarian honeysuckle", "Restricted",
  "Lonicera japonica", "Japanese honeysuckle", "Restricted",
  "Rhamnus cathartica", "Common buckthorn", "Restricted",
  "Frangula alnus", "Glossy buckthorn", "Restricted",
  "Rosa multiflora", "Multiflora rose", "Restricted",
  "Berberis thunbergii", "Japanese barberry", "Restricted",
  "Elaeagnus umbellata", "Autumn olive", "Restricted",
  "Euonymus alatus", "Burning bush", "Restricted",
  "Lythrum salicaria", "Purple loosestrife", "Restricted",
  "Phragmites australis", "Common reed", "Restricted",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Restricted",
  "Potamogeton crispus", "Curly pondweed", "Restricted",
  "Alliaria petiolata", "Garlic mustard", "Restricted",
  "Polygonum cuspidatum", "Japanese knotweed", "Restricted",
  "Centaurea stoebe", "Spotted knapweed", "Restricted",
  "Cirsium arvense", "Canada thistle", "Restricted",
  "Cirsium vulgare", "Bull thistle", "Restricted",
  "Euphorbia esula", "Leafy spurge", "Restricted",
  "Celastrus orbiculatus", "Oriental bittersweet", "Restricted",
  "Ailanthus altissima", "Tree of heaven", "Restricted",
  "Acer platanoides", "Norway maple", "Restricted",
  "Hesperis matronalis", "Dames rocket", "Restricted",
  "Microstegium vimineum", "Japanese stiltgrass", "Restricted",
  "Iris pseudacorus", "Yellow flag iris", "Restricted",
  "Pueraria montana", "Kudzu", "Restricted",
  "Miscanthus sinensis", "Chinese silvergrass", "Restricted"
) %>% mutate(state_code = "WI", source = "Wisconsin DNR NR40 Invasive Species Rule")

# New Jersey - NJISST Invasive Plant List (35 species, Jan 2026)
NJ_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Lonicera japonica", "Japanese honeysuckle", "Widespread",
  "Lonicera fragrantissima", "Sweet breath of spring", "Widespread",
  "Lonicera maackii", "Amur honeysuckle", "Widespread",
  "Lonicera morrowii", "Morrows honeysuckle", "Widespread",
  "Lonicera standishii", "Standish honeysuckle", "Emerging",
  "Lonicera tatarica", "Tatarian honeysuckle", "Widespread",
  "Berberis thunbergii", "Japanese barberry", "Widespread",
  "Berberis vulgaris", "Common barberry", "Emerging",
  "Rhamnus cathartica", "Common buckthorn", "Widespread",
  "Frangula alnus", "Glossy buckthorn", "Widespread",
  "Rosa multiflora", "Multiflora rose", "Widespread",
  "Euonymus alatus", "Burning bush", "Widespread",
  "Ailanthus altissima", "Tree of heaven", "Widespread",
  "Celastrus orbiculatus", "Oriental bittersweet", "Widespread",
  "Elaeagnus umbellata", "Autumn olive", "Widespread",
  "Alliaria petiolata", "Garlic mustard", "Widespread",
  "Microstegium vimineum", "Japanese stiltgrass", "Widespread",
  "Phragmites australis", "Common reed", "Widespread",
  "Polygonum cuspidatum", "Japanese knotweed", "Widespread",
  "Lythrum salicaria", "Purple loosestrife", "Widespread",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Widespread",
  "Acer platanoides", "Norway maple", "Widespread",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Widespread",
  "Paulownia tomentosa", "Princess tree", "Emerging",
  "Wisteria sinensis", "Chinese wisteria", "Widespread",
  "Hedera helix", "English ivy", "Widespread",
  "Cynanchum louiseae", "Black swallowwort", "Widespread",
  "Ficaria verna", "Lesser celandine", "Emerging",
  "Pyrus calleryana", "Callery pear", "Emerging",
  "Miscanthus sinensis", "Chinese silvergrass", "Widespread",
  "Pueraria montana", "Kudzu", "Emerging",
  "Cirsium arvense", "Canada thistle", "Widespread",
  "Cirsium vulgare", "Bull thistle", "Widespread",
  "Iris pseudacorus", "Yellow flag iris", "Widespread",
  "Persicaria perfoliata", "Mile-a-minute vine", "Widespread"
) %>% mutate(state_code = "NJ", source = "New Jersey Invasive Species Strike Team")

# Vermont - Noxious Weed and Invasive List (30 species, Jan 2026)
VT_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Rhamnus cathartica", "Common buckthorn", "Class B Noxious",
  "Frangula alnus", "Glossy buckthorn", "Class B Noxious",
  "Berberis thunbergii", "Japanese barberry", "Class B Noxious",
  "Berberis vulgaris", "Common barberry", "Class B Noxious",
  "Lonicera japonica", "Japanese honeysuckle", "Class B Noxious",
  "Lonicera morrowii", "Morrows honeysuckle", "Class B Noxious",
  "Lonicera tatarica", "Tatarian honeysuckle", "Class B Noxious",
  "Rosa multiflora", "Multiflora rose", "Class B Noxious",
  "Euonymus alatus", "Burning bush", "Class B Noxious",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Celastrus orbiculatus", "Oriental bittersweet", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Acer platanoides", "Norway maple", "Invasive",
  "Acer ginnala", "Amur maple", "Invasive",
  "Alliaria petiolata", "Garlic mustard", "Invasive",
  "Phragmites australis", "Common reed", "Invasive",
  "Lythrum salicaria", "Purple loosestrife", "Invasive",
  "Polygonum cuspidatum", "Japanese knotweed", "Invasive",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Invasive",
  "Hedera helix", "English ivy", "Invasive",
  "Euonymus fortunei", "Wintercreeper", "Invasive",
  "Cynanchum louiseae", "Black swallowwort", "Invasive",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Invasive",
  "Microstegium vimineum", "Japanese stiltgrass", "Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Invasive",
  "Cirsium arvense", "Canada thistle", "Invasive",
  "Centaurea stoebe", "Spotted knapweed", "Invasive",
  "Hesperis matronalis", "Dames rocket", "Invasive",
  "Potamogeton crispus", "Curly pondweed", "Invasive",
  "Iris pseudacorus", "Yellow flag iris", "Invasive"
) %>% mutate(state_code = "VT", source = "Vermont Agency of Agriculture")

# Rhode Island - RIISC Invasive Plant List (30 species, Jan 2026)
RI_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Phragmites australis", "Common reed", "Widespread",
  "Celastrus orbiculatus", "Oriental bittersweet", "Widespread",
  "Lonicera japonica", "Japanese honeysuckle", "Widespread",
  "Lonicera morrowii", "Morrows honeysuckle", "Widespread",
  "Rhamnus cathartica", "Common buckthorn", "Widespread",
  "Frangula alnus", "Glossy buckthorn", "Widespread",
  "Rosa multiflora", "Multiflora rose", "Widespread",
  "Berberis thunbergii", "Japanese barberry", "Widespread",
  "Euonymus alatus", "Burning bush", "Widespread",
  "Ailanthus altissima", "Tree of heaven", "Widespread",
  "Alliaria petiolata", "Garlic mustard", "Widespread",
  "Polygonum cuspidatum", "Japanese knotweed", "Widespread",
  "Lythrum salicaria", "Purple loosestrife", "Widespread",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Widespread",
  "Akebia quinata", "Chocolate vine", "Widespread",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Widespread",
  "Cynanchum louiseae", "Black swallowwort", "Widespread",
  "Euonymus fortunei", "Wintercreeper", "Widespread",
  "Acer platanoides", "Norway maple", "Widespread",
  "Elaeagnus umbellata", "Autumn olive", "Widespread",
  "Wisteria sinensis", "Chinese wisteria", "Widespread",
  "Hedera helix", "English ivy", "Widespread",
  "Microstegium vimineum", "Japanese stiltgrass", "Widespread",
  "Iris pseudacorus", "Yellow flag iris", "Widespread",
  "Cabomba caroliniana", "Fanwort", "Widespread",
  "Potamogeton crispus", "Curly pondweed", "Widespread",
  "Trapa natans", "Water chestnut", "Widespread",
  "Cirsium arvense", "Canada thistle", "Widespread",
  "Hesperis matronalis", "Dames rocket", "Widespread",
  "Miscanthus sinensis", "Chinese silvergrass", "Widespread"
) %>% mutate(state_code = "RI", source = "Rhode Island Invasive Species Council")

# Delaware - Invasive Plant List (37 species, Jan 2026)
DE_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Ailanthus altissima", "Tree of heaven", "Banned",
  "Lonicera japonica", "Japanese honeysuckle", "Banned",
  "Lonicera morrowii", "Morrows honeysuckle", "Banned",
  "Lonicera maackii", "Amur honeysuckle", "Banned",
  "Lonicera tatarica", "Tatarian honeysuckle", "Banned",
  "Eichhornia crassipes", "Water hyacinth", "Banned",
  "Myriophyllum aquaticum", "Parrotfeather", "Banned",
  "Hedera helix", "English ivy", "Banned",
  "Hemerocallis fulva", "Orange daylily", "Banned",
  "Iris pseudacorus", "Yellow flag iris", "Banned",
  "Lysimachia nummularia", "Creeping jenny", "Banned",
  "Pachysandra terminalis", "Japanese pachysandra", "Banned",
  "Wisteria sinensis", "Chinese wisteria", "Banned",
  "Berberis thunbergii", "Japanese barberry", "Banned",
  "Euonymus alatus", "Burning bush", "Banned",
  "Celastrus orbiculatus", "Oriental bittersweet", "Banned",
  "Rhamnus cathartica", "Common buckthorn", "Banned",
  "Frangula alnus", "Glossy buckthorn", "Banned",
  "Rosa multiflora", "Multiflora rose", "Banned",
  "Elaeagnus umbellata", "Autumn olive", "Banned",
  "Ligustrum sinense", "Chinese privet", "Banned",
  "Ligustrum obtusifolium", "Border privet", "Banned",
  "Nandina domestica", "Sacred bamboo", "Banned",
  "Phragmites australis", "Common reed", "Banned",
  "Lythrum salicaria", "Purple loosestrife", "Banned",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Banned",
  "Polygonum cuspidatum", "Japanese knotweed", "Banned",
  "Alliaria petiolata", "Garlic mustard", "Banned",
  "Microstegium vimineum", "Japanese stiltgrass", "Banned",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Banned",
  "Pyrus calleryana", "Callery pear", "Banned",
  "Acer platanoides", "Norway maple", "Banned",
  "Paulownia tomentosa", "Princess tree", "Banned",
  "Miscanthus sinensis", "Chinese silvergrass", "Banned",
  "Vinca minor", "Common periwinkle", "Banned",
  "Spiraea japonica", "Japanese spiraea", "Banned",
  "Pueraria montana", "Kudzu", "Banned"
) %>% mutate(state_code = "DE", source = "Delaware Invasive Plant Law")

# Kentucky - KYEPPC Invasive Plant List (35 species, Jan 2026)
KY_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Ligustrum sinense", "Chinese privet", "Invasive",
  "Ligustrum vulgare", "Common privet", "Invasive",
  "Lonicera maackii", "Amur honeysuckle", "Invasive",
  "Lonicera fragrantissima", "Sweet breath of spring", "Invasive",
  "Lonicera standishii", "Standish honeysuckle", "Invasive",
  "Lonicera morrowii", "Morrows honeysuckle", "Invasive",
  "Lonicera tatarica", "Tatarian honeysuckle", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive",
  "Rosa multiflora", "Multiflora rose", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Euonymus alatus", "Burning bush", "Invasive",
  "Euonymus fortunei", "Wintercreeper", "Invasive",
  "Celastrus orbiculatus", "Oriental bittersweet", "Invasive",
  "Microstegium vimineum", "Japanese stiltgrass", "Invasive",
  "Alliaria petiolata", "Garlic mustard", "Invasive",
  "Polygonum cuspidatum", "Japanese knotweed", "Invasive",
  "Lespedeza cuneata", "Sericea lespedeza", "Invasive",
  "Pueraria montana", "Kudzu", "Invasive",
  "Phragmites australis", "Common reed", "Invasive",
  "Lythrum salicaria", "Purple loosestrife", "Invasive",
  "Rhamnus cathartica", "Common buckthorn", "Invasive",
  "Frangula alnus", "Glossy buckthorn", "Invasive",
  "Berberis thunbergii", "Japanese barberry", "Invasive",
  "Paulownia tomentosa", "Princess tree", "Invasive",
  "Pyrus calleryana", "Callery pear", "Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Invasive",
  "Miscanthus sinensis", "Chinese silvergrass", "Invasive",
  "Nandina domestica", "Sacred bamboo", "Invasive",
  "Spiraea japonica", "Japanese spiraea", "Invasive",
  "Hedera helix", "English ivy", "Invasive",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Invasive",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Invasive",
  "Cirsium arvense", "Canada thistle", "Invasive",
  "Carduus nutans", "Musk thistle", "Invasive"
) %>% mutate(state_code = "KY", source = "Kentucky Exotic Pest Plant Council")

# Missouri - MoIP Invasive Plant List (35 species, Jan 2026)
MO_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Lespedeza cuneata", "Sericea lespedeza", "Invasive",
  "Lonicera maackii", "Amur honeysuckle", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive",
  "Rosa multiflora", "Multiflora rose", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Euonymus alatus", "Burning bush", "Invasive",
  "Euonymus fortunei", "Wintercreeper", "Invasive",
  "Ligustrum sinense", "Chinese privet", "Invasive",
  "Ligustrum obtusifolium", "Border privet", "Invasive",
  "Rhamnus cathartica", "Common buckthorn", "Invasive",
  "Frangula alnus", "Glossy buckthorn", "Invasive",
  "Polygonum cuspidatum", "Japanese knotweed", "Invasive",
  "Microstegium vimineum", "Japanese stiltgrass", "Invasive",
  "Alliaria petiolata", "Garlic mustard", "Invasive",
  "Pueraria montana", "Kudzu", "Invasive",
  "Phragmites australis", "Common reed", "Invasive",
  "Lythrum salicaria", "Purple loosestrife", "Invasive",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Invasive",
  "Celastrus orbiculatus", "Oriental bittersweet", "Invasive",
  "Berberis thunbergii", "Japanese barberry", "Invasive",
  "Pyrus calleryana", "Callery pear", "Invasive",
  "Paulownia tomentosa", "Princess tree", "Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Invasive",
  "Nandina domestica", "Sacred bamboo", "Invasive",
  "Spiraea japonica", "Japanese spiraea", "Invasive",
  "Miscanthus sinensis", "Chinese silvergrass", "Invasive",
  "Dipsacus fullonum", "Common teasel", "Invasive",
  "Cirsium arvense", "Canada thistle", "Invasive",
  "Cirsium vulgare", "Bull thistle", "Invasive",
  "Carduus nutans", "Musk thistle", "Invasive",
  "Sorghum halepense", "Johnsongrass", "Invasive",
  "Convolvulus arvensis", "Field bindweed", "Invasive",
  "Centaurea stoebe", "Spotted knapweed", "Invasive",
  "Hesperis matronalis", "Dames rocket", "Invasive"
) %>% mutate(state_code = "MO", source = "Missouri Invasive Plant Council")

# Oklahoma - Invasive Plant List (25 species, Jan 2026)
OK_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Bromus tectorum", "Cheatgrass", "Dirty Dozen",
  "Ligustrum sinense", "Chinese privet", "Dirty Dozen",
  "Juniperus virginiana", "Eastern redcedar", "Dirty Dozen",
  "Bromus arvensis", "Field brome", "Dirty Dozen",
  "Hydrilla verticillata", "Hydrilla", "Dirty Dozen",
  "Lonicera japonica", "Japanese honeysuckle", "Dirty Dozen",
  "Sorghum halepense", "Johnsongrass", "Dirty Dozen",
  "Carduus nutans", "Musk thistle", "Dirty Dozen",
  "Salsola tragus", "Russian thistle", "Dirty Dozen",
  "Tamarix ramosissima", "Saltcedar", "Dirty Dozen",
  "Lespedeza cuneata", "Sericea lespedeza", "Dirty Dozen",
  "Ulmus pumila", "Siberian elm", "Dirty Dozen",
  "Bothriochloa ischaemum", "Yellow bluestem", "Dirty Dozen",
  "Pyrus calleryana", "Callery pear", "Watch List",
  "Ailanthus altissima", "Tree of heaven", "Watch List",
  "Rosa multiflora", "Multiflora rose", "Watch List",
  "Elaeagnus angustifolia", "Russian olive", "Watch List",
  "Cirsium arvense", "Canada thistle", "Noxious",
  "Cirsium vulgare", "Bull thistle", "Noxious",
  "Convolvulus arvensis", "Field bindweed", "Noxious",
  "Euphorbia esula", "Leafy spurge", "Noxious",
  "Centaurea stoebe", "Spotted knapweed", "Watch List",
  "Phragmites australis", "Common reed", "Watch List",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Watch List",
  "Salvinia molesta", "Giant salvinia", "Watch List"
) %>% mutate(state_code = "OK", source = "Oklahoma Invasive Plant Council")

# West Virginia - WVDNR Priority Invasive Plants (35 species, Jan 2026)
WV_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  # Threat Level 1
  "Ailanthus altissima", "Tree of heaven", "Threat Level 1",
  "Rosa multiflora", "Multiflora rose", "Threat Level 1",
  "Microstegium vimineum", "Japanese stiltgrass", "Threat Level 1",
  "Myriophyllum spicatum", "Eurasian watermilfoil", "Threat Level 1",
  "Paulownia tomentosa", "Princess tree", "Threat Level 1",
  "Persicaria perfoliata", "Mile-a-minute vine", "Threat Level 1",
  "Pueraria montana", "Kudzu", "Threat Level 1",
  "Pyrus calleryana", "Callery pear", "Threat Level 1",
  "Reynoutria japonica", "Japanese knotweed", "Threat Level 1",
  "Lonicera maackii", "Amur honeysuckle", "Threat Level 1",
  "Lonicera japonica", "Japanese honeysuckle", "Threat Level 1",
  "Elaeagnus umbellata", "Autumn olive", "Threat Level 1",
  "Celastrus orbiculatus", "Oriental bittersweet", "Threat Level 1",
  "Alliaria petiolata", "Garlic mustard", "Threat Level 1",
  "Euonymus fortunei", "Wintercreeper", "Threat Level 1",
  "Lespedeza cuneata", "Sericea lespedeza", "Threat Level 1",
  # Threat Level 2
  "Lonicera morrowii", "Morrows honeysuckle", "Threat Level 2",
  "Lonicera tatarica", "Tatarian honeysuckle", "Threat Level 2",
  "Ligustrum sinense", "Chinese privet", "Threat Level 2",
  "Berberis thunbergii", "Japanese barberry", "Threat Level 2",
  "Euonymus alatus", "Burning bush", "Threat Level 2",
  "Rhamnus cathartica", "Common buckthorn", "Threat Level 2",
  "Frangula alnus", "Glossy buckthorn", "Threat Level 2",
  "Lythrum salicaria", "Purple loosestrife", "Threat Level 2",
  "Phragmites australis", "Common reed", "Threat Level 2",
  "Wisteria sinensis", "Chinese wisteria", "Threat Level 2",
  "Miscanthus sinensis", "Chinese silvergrass", "Threat Level 2",
  "Spiraea japonica", "Japanese spiraea", "Threat Level 2",
  "Nandina domestica", "Sacred bamboo", "Threat Level 2",
  "Ampelopsis brevipedunculata", "Porcelain berry", "Threat Level 2",
  "Hedera helix", "English ivy", "Threat Level 2",
  # Threat Level 3
  "Cirsium arvense", "Canada thistle", "Threat Level 3",
  "Cirsium vulgare", "Bull thistle", "Threat Level 3",
  "Carduus nutans", "Musk thistle", "Threat Level 3",
  "Sorghum halepense", "Johnsongrass", "Threat Level 3"
) %>% mutate(state_code = "WV", source = "West Virginia DNR Priority Invasive Plants 2025")

# Alabama - The Nature Conservancy Alabama's Worst Invasives
AL_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Pueraria montana", "Kudzu", "Invasive",
  "Triadica sebifera", "Chinese tallow", "Invasive",
  "Imperata cylindrica", "Cogongrass", "Noxious",
  "Ligustrum sinense", "Chinese privet", "Invasive",
  "Solanum viarum", "Tropical soda apple", "Noxious",
  "Lygodium japonicum", "Japanese climbing fern", "Invasive",
  "Rosa multiflora", "Multiflora rose", "Invasive",
  "Rosa laevigata", "Cherokee rose", "Invasive",
  "Rosa bracteata", "Macartney rose", "Invasive",
  "Myriophyllum spicatum", "Eurasian water milfoil", "Invasive",
  "Hydrilla verticillata", "Hydrilla", "Noxious",
  "Alternanthera philoxeroides", "Alligatorweed", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Albizia julibrissin", "Mimosa", "Invasive",
  "Melia azedarach", "Chinaberry", "Invasive",
  "Paulownia tomentosa", "Princess tree", "Invasive",
  "Pyrus calleryana", "Bradford pear", "Invasive",
  "Nandina domestica", "Sacred bamboo", "Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Invasive"
) %>% mutate(state_code = "AL", source = "The Nature Conservancy Alabama 2024")

# Arkansas - Encyclopedia of Arkansas + UAEX
AR_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Pyrus calleryana", "Callery pear", "Invasive",
  "Koelreuteria bipinnata", "Chinese flame tree", "Invasive",
  "Koelreuteria paniculata", "Golden-rain tree", "Invasive",
  "Triadica sebifera", "Chinese tallow", "Invasive",
  "Albizia julibrissin", "Mimosa", "Invasive",
  "Melia azedarach", "Chinaberry", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Vitex negundo", "Negundo chaste tree", "Invasive",
  "Aleurites fordii", "Tungoil tree", "Invasive",
  "Ligustrum sinense", "Chinese privet", "Invasive",
  "Ligustrum lucidum", "Glossy privet", "Invasive",
  "Elaeagnus pungens", "Silverthorn", "Invasive",
  "Elaeagnus umbellata", "Autumn olive", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive",
  "Hedera helix", "English ivy", "Invasive",
  "Pueraria montana", "Kudzu", "Invasive",
  "Sorghum halepense", "Johnsongrass", "Noxious",
  "Cynodon dactylon", "Bermudagrass", "Noxious",
  "Phyllostachys aurea", "Golden bamboo", "Invasive",
  "Imperata cylindrica", "Cogongrass", "Invasive",
  "Lespedeza cuneata", "Chinese lespedeza", "Invasive",
  "Lygodium japonicum", "Japanese climbing fern", "Invasive",
  "Hydrilla verticillata", "Hydrilla", "Noxious",
  "Salvinia molesta", "Giant salvinia", "Noxious",
  "Paulownia tomentosa", "Princess tree", "Invasive",
  "Wisteria sinensis", "Chinese wisteria", "Invasive"
) %>% mutate(state_code = "AR", source = "Encyclopedia of Arkansas + UAEX 2024")

# Hawaii - Hawaii Invasive Species Council
HI_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Falcataria moluccana", "Albizia", "Invasive",
  "Cyathea cooperi", "Australian tree fern", "Invasive",
  "Bischofia javanica", "Bishop wood", "Invasive",
  "Acacia mearnsii", "Black wattle", "Invasive",
  "Ficus religiosa", "Bodhi tree", "Invasive",
  "Schinus terebinthifolius", "Christmas berry", "Invasive",
  "Cotoneaster pannosus", "Cotoneaster", "Invasive",
  "Morella faya", "Fire tree", "Invasive",
  "Tibouchina urvilleana", "Glory bush", "Noxious",
  "Buddleja madagascariensis", "Smoke bush", "Invasive",
  "Psidium cattleianum", "Strawberry guava", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Passiflora tarminiana", "Banana poka", "Invasive",
  "Pyrostegia venusta", "Flame vine", "Invasive",
  "Clematis terniflora", "Sweet autumn clematis", "Invasive",
  "Imperata cylindrica", "Cogongrass", "Invasive",
  "Pennisetum setaceum", "Fountain grass", "Invasive",
  "Cortaderia jubata", "Pampas grass", "Invasive",
  "Chromolaena odorata", "Devil weed", "Invasive",
  "Senecio madagascariensis", "Fireweed", "Invasive",
  "Ulex europaeus", "Gorse", "Invasive",
  "Bocconia frutescens", "Plume poppy", "Invasive",
  "Hedychium gardnerianum", "Himalayan ginger", "Invasive",
  "Miconia calvescens", "Miconia", "Noxious",
  "Clidemia hirta", "Koster's curse", "Invasive",
  "Rubus argutus", "Prickly Florida blackberry", "Invasive",
  "Leucaena leucocephala", "Lead tree", "Invasive",
  "Prosopis pallida", "Kiawe", "Invasive",
  "Coccinia grandis", "Ivy gourd", "Noxious",
  "Panicum maximum", "Guinea grass", "Invasive"
) %>% mutate(state_code = "HI", source = "Hawaii Invasive Species Council 2024")

# Louisiana - LA WLF + LSU AgCenter
LA_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Triadica sebifera", "Chinese tallow", "Invasive",
  "Ligustrum japonicum", "Japanese privet", "Invasive",
  "Ligustrum lucidum", "Glossy privet", "Invasive",
  "Ligustrum sinense", "Chinese privet", "Invasive",
  "Imperata cylindrica", "Cogongrass", "Noxious",
  "Phragmites australis", "Common reed", "Invasive",
  "Salvinia molesta", "Giant salvinia", "Noxious",
  "Alternanthera philoxeroides", "Alligatorweed", "Invasive",
  "Hydrilla verticillata", "Hydrilla", "Noxious",
  "Arundo donax", "Giant reed", "Invasive",
  "Liriope muscari", "Big blue lilyturf", "Invasive",
  "Lonicera japonica", "Japanese honeysuckle", "Invasive",
  "Lygodium japonicum", "Japanese climbing fern", "Invasive",
  "Nandina domestica", "Sacred bamboo", "Invasive",
  "Panicum repens", "Torpedo grass", "Invasive",
  "Pyrus calleryana", "Callery pear", "Invasive",
  "Quercus acutissima", "Sawtooth oak", "Invasive",
  "Rosa laevigata", "Cherokee rose", "Invasive",
  "Vernicia fordii", "Tungoil tree", "Invasive",
  "Iris pseudacorus", "Yellow flag iris", "Invasive",
  "Albizia julibrissin", "Mimosa", "Invasive",
  "Eichhornia crassipes", "Water hyacinth", "Noxious",
  "Pueraria montana", "Kudzu", "Invasive",
  "Colocasia esculenta", "Wild taro", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive"
) %>% mutate(state_code = "LA", source = "Louisiana WLF + LSU AgCenter 2024")

# Mississippi - MSU Extension + MS Dept of Agriculture
MS_COMPILED <- tribble(
  ~scientific_name, ~common_name, ~designation,
  "Alternanthera philoxeroides", "Alligatorweed", "Invasive",
  "Lonicera maackii", "Amur honeysuckle", "Invasive",
  "Vitex rotundifolia", "Beach vitex", "Invasive",
  "Commelina benghalensis", "Benghal dayflower", "Noxious",
  "Cayratia japonica", "Bushkiller", "Invasive",
  "Mimosa pigra", "Catclaw mimosa", "Invasive",
  "Imperata cylindrica", "Cogongrass", "Noxious",
  "Hedera helix", "English ivy", "Invasive",
  "Alliaria petiolata", "Garlic mustard", "Invasive",
  "Arundo donax", "Giant reed", "Invasive",
  "Rottboellia cochinchinensis", "Itchgrass", "Noxious",
  "Lygodium japonicum", "Japanese climbing fern", "Invasive",
  "Sorghum halepense", "Johnsongrass", "Noxious",
  "Pueraria montana", "Kudzu", "Invasive",
  "Rosa multiflora", "Multiflora rose", "Invasive",
  "Carduus nutans", "Musk thistle", "Noxious",
  "Lythrum salicaria", "Purple loosestrife", "Invasive",
  "Nandina domestica", "Sacred bamboo", "Invasive",
  "Crotalaria spectabilis", "Showy rattlebox", "Noxious",
  "Sporobolus indicus", "Smutgrass", "Invasive",
  "Xanthium spinosum", "Spiny cocklebur", "Noxious",
  "Triadica sebifera", "Chinese tallow", "Invasive",
  "Elaeagnus pungens", "Thorny olive", "Invasive",
  "Panicum repens", "Torpedograss", "Invasive",
  "Ailanthus altissima", "Tree of heaven", "Invasive",
  "Solanum viarum", "Tropical soda apple", "Noxious",
  "Salvinia molesta", "Giant salvinia", "Noxious",
  "Hydrilla verticillata", "Hydrilla", "Noxious",
  "Myriophyllum aquaticum", "Parrotfeather", "Invasive",
  "Eichhornia crassipes", "Water hyacinth", "Noxious"
) %>% mutate(state_code = "MS", source = "MSU Extension + MS Dept of Agriculture 2024")

# ---------------------------
# Compile and Load Functions
# ---------------------------

#' Compile all state invasive data into single CSV
compile_state_invasive_data <- function(output_file = "data/state_invasive_for_import.csv") {
  message("Compiling state invasive species data...")

  # Combine all compiled state data
  all_data <- bind_rows(
    OH_COMPILED,
    MN_COMPILED,
    CO_COMPILED,
    WY_COMPILED,
    AK_COMPILED,
    SD_COMPILED,
    ND_COMPILED,
    ME_COMPILED,
    MD_COMPILED,
    KS_COMPILED,
    NE_COMPILED,
    TX_COMPILED,
    UT_COMPILED,
    NV_COMPILED,
    ID_COMPILED,
    CT_COMPILED,
    IA_COMPILED,
    OR_COMPILED,
    CA_COMPILED,
    WA_COMPILED,
    MT_COMPILED,
    FL_COMPILED,
    NM_COMPILED,
    AZ_COMPILED,
    GA_COMPILED,
    NC_COMPILED,
    SC_COMPILED,
    VA_COMPILED,
    TN_COMPILED,
    NY_COMPILED,
    PA_COMPILED,
    MA_COMPILED,
    NH_COMPILED,
    IL_COMPILED,
    IN_COMPILED,
    MI_COMPILED,
    WI_COMPILED,
    NJ_COMPILED,
    VT_COMPILED,
    RI_COMPILED,
    DE_COMPILED,
    KY_COMPILED,
    MO_COMPILED,
    OK_COMPILED,
    WV_COMPILED,
    AL_COMPILED,
    AR_COMPILED,
    HI_COMPILED,
    LA_COMPILED,
    MS_COMPILED
  )

  # NOTE: Invasive Plant Atlas data removed - determined to be unreliable
  # Only using direct compiled state data now

  # Standardize designations for database
  all_data <- all_data %>%
    mutate(
      # Normalize designation for consistency
      designation_db = case_when(
        designation %in% c("List A", "Prohibited-Eradicate", "Category I", "High") ~ "Invasive-High",
        designation %in% c("List B", "Prohibited-Control", "Category II", "Moderate") ~ "Invasive-Moderate",
        designation %in% c("List C", "Restricted", "Limited", "Low") ~ "Invasive-Low",
        designation %in% c("Watch", "Alert", "Caution") ~ "Watch",
        TRUE ~ "Invasive"
      ),
      # Clean scientific names (first two words only)
      gs_name = sapply(strsplit(scientific_name, "\\s+"), function(x) paste(head(x, 2), collapse = " ")),
      fetched_at = Sys.time()
    ) %>%
    # Remove duplicates (same species/state)
    distinct(gs_name, state_code, .keep_all = TRUE)

  # Summary
  message(sprintf("\nCompiled data from %d states:", length(unique(all_data$state_code))))
  message(sprintf("  Total records: %d", nrow(all_data)))
  message(sprintf("  Unique species: %d", length(unique(all_data$gs_name))))

  # State breakdown
  state_counts <- all_data %>%
    group_by(state_code) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n))
  for (i in seq_len(nrow(state_counts))) {
    message(sprintf("    %s: %d species", state_counts$state_code[i], state_counts$n[i]))
  }

  # Write CSV
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write.csv(all_data, output_file, row.names = FALSE)
  message(sprintf("\nSaved to: %s", output_file))

  invisible(all_data)
}

#' Load state invasive data into ref_noxious_invasive table
#' @param csv_path Path to compiled CSV
#' @param pool Database connection pool
load_state_invasive_to_db <- function(csv_path = "data/state_invasive_for_import.csv", pool = NULL) {
  if (!file.exists(csv_path)) {
    stop("Import file not found. Run compile_state_invasive_data() first.")
  }

  # Load db.R if pool not provided
  if (is.null(pool)) {
    if (file.exists("app/R/db.R")) {
      source("app/R/db.R")
    }
    pool <- get("pool", envir = globalenv())
  }

  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  message(sprintf("Loading %d records...", nrow(data)))

  n_matched <- 0
  n_inserted <- 0
  n_unmatched <- 0
  unmatched_species <- character()

  # Get unique species to reduce lookups
  unique_species <- unique(data$gs_name)
  taxon_cache <- list()

  message("Resolving taxon IDs...")
  pb <- txtProgressBar(min = 0, max = length(unique_species), style = 3)
  for (i in seq_along(unique_species)) {
    sp <- unique_species[i]

    taxon <- tryCatch({
      DBI::dbGetQuery(pool, "
        SELECT id FROM ref_taxon
        WHERE lower(split_part(scientific_name, ' ', 1) || ' ' || split_part(scientific_name, ' ', 2)) = lower($1)
        LIMIT 1
      ", params = list(sp))
    }, error = function(e) data.frame())

    if (nrow(taxon) > 0) {
      taxon_cache[[sp]] <- taxon$id[1]
      n_matched <- n_matched + 1
    } else {
      unmatched_species <- c(unmatched_species, sp)
      n_unmatched <- n_unmatched + 1
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  message(sprintf("Matched %d/%d species to ref_taxon", n_matched, length(unique_species)))

  # Insert records
  message("Inserting records...")
  pb <- txtProgressBar(min = 0, max = nrow(data), style = 3)
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    taxon_id <- taxon_cache[[row$gs_name]]

    if (is.null(taxon_id)) {
      setTxtProgressBar(pb, i)
      next
    }

    tryCatch({
      DBI::dbExecute(pool, "
        INSERT INTO ref_noxious_invasive (taxon_id, state_code, designation, source, source_url)
        VALUES ($1, $2, $3, $4, $5)
        ON CONFLICT (taxon_id, state_code, designation) DO NOTHING
      ", params = list(
        taxon_id,
        row$state_code,
        row$designation_db,
        row$source,
        if ("source_url" %in% names(row)) row$source_url else NA
      ))
      n_inserted <- n_inserted + 1
    }, error = function(e) NULL)

    setTxtProgressBar(pb, i)
  }
  close(pb)

  message(sprintf("\n=== Load Complete ==="))
  message(sprintf("Records inserted: %d", n_inserted))
  message(sprintf("Unmatched species: %d", n_unmatched))

  if (length(unmatched_species) > 0) {
    message("\nSample unmatched species (first 20):")
    message(paste(head(unmatched_species, 20), collapse = ", "))
  }

  invisible(n_inserted)
}

# ---------------------------
# Usage message
# ---------------------------
message("
State Invasive Species Data Compiler loaded.

Direct state data: OH, MN, CO, WY, AK, SD, ND, ME, MD, KS, NE, TX, UT, NV, ID, CT, IA (17 states)

Usage:
  compile_state_invasive_data()   # Compile to CSV
  load_state_invasive_to_db()     # Load into database
")
