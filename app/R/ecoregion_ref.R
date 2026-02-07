# R/ecoregion_ref.R - EPA Ecoregion reference data and helpers

# EPA Level III Ecoregions (Continental US)
# These are the numeric codes (1-84) used in Level IV codes like "65a"
# Source: https://www.epa.gov/eco-research/ecoregions
EPA_L3_ECOREGIONS <- list(
  "1" = "Coast Range",
  "2" = "Puget Lowland",
  "3" = "Willamette Valley",
  "4" = "Cascades",
  "5" = "Sierra Nevada",
  "6" = "Central California Foothills and Coastal Mountains",
  "7" = "Central California Valley",
  "8" = "Southern California Mountains",
  "9" = "Eastern Cascades Slopes and Foothills",
 "10" = "Columbia Plateau",
 "11" = "Blue Mountains",
 "12" = "Snake River Plain",
 "13" = "Central Basin and Range",
 "14" = "Mojave Basin and Range",
 "15" = "Northern Rockies",
 "16" = "Idaho Batholith",
 "17" = "Middle Rockies",
 "18" = "Wyoming Basin",
 "19" = "Wasatch and Uinta Mountains",
 "20" = "Colorado Plateaus",
 "21" = "Southern Rockies",
 "22" = "Arizona/New Mexico Plateau",
 "23" = "Arizona/New Mexico Mountains",
 "24" = "Chihuahuan Deserts",
 "25" = "High Plains",
 "26" = "Southwestern Tablelands",
 "27" = "Central Great Plains",
 "28" = "Flint Hills",
 "29" = "Central Oklahoma/Texas Plains",
 "30" = "Edwards Plateau",
 "31" = "Southern Texas Plains",
 "32" = "Texas Blackland Prairies",
 "33" = "East Central Texas Plains",
 "34" = "Western Gulf Coastal Plain",
 "35" = "South Central Plains",
 "36" = "Ouachita Mountains",
 "37" = "Arkansas Valley",
 "38" = "Boston Mountains",
 "39" = "Ozark Highlands",
 "40" = "Central Irregular Plains",
 "41" = "Canadian Rockies",
 "42" = "Northwestern Glaciated Plains",
 "43" = "Northwestern Great Plains",
 "44" = "Nebraska Sand Hills",
 "45" = "Piedmont",
 "46" = "Northern Glaciated Plains",
 "47" = "Western Corn Belt Plains",
 "48" = "Lake Agassiz Plain",
 "49" = "Northern Minnesota Wetlands",
 "50" = "Northern Lakes and Forests",
 "51" = "North Central Hardwood Forests",
 "52" = "Driftless Area",
 "53" = "Southeastern Wisconsin Till Plains",
 "54" = "Central Corn Belt Plains",
 "55" = "Eastern Corn Belt Plains",
 "56" = "Southern Michigan/Northern Indiana Drift Plains",
 "57" = "Huron/Erie Lake Plains",
 "58" = "Northeastern Highlands",
 "59" = "Northeastern Coastal Zone",
 "60" = "Northern Appalachian Plateau and Uplands",
 "61" = "Erie Drift Plain",
 "62" = "North Central Appalachians",
 "63" = "Middle Atlantic Coastal Plain",
 "64" = "Northern Piedmont",
 "65" = "Southeastern Plains",
 "66" = "Blue Ridge",
 "67" = "Ridge and Valley",
 "68" = "Southwestern Appalachians",
 "69" = "Central Appalachians",
 "70" = "Western Allegheny Plateau",
 "71" = "Interior Plateau",
 "72" = "Interior River Valleys and Hills",
 "73" = "Mississippi Alluvial Plain",
 "74" = "Mississippi Valley Loess Plains",
 "75" = "Southern Coastal Plain",
 "76" = "Southern Florida Coastal Plain",
 "77" = "North Cascades",
 "78" = "Klamath Mountains/California High North Coast Range",
 "79" = "Madrean Archipelago",
 "80" = "Northern Basin and Range",
 "81" = "Sonoran Basin and Range",
 "82" = "Laurentian Plains and Hills",
 "83" = "Eastern Great Lakes Lowlands",
 "84" = "Atlantic Coastal Pine Barrens"
)

# EPA Level II Ecoregion mappings (L3 code ranges to L2 names)
# Level II groups multiple Level III ecoregions
EPA_L2_MAPPINGS <- list(
  # Western regions
  "1" = "Marine West Coast Forest", "2" = "Marine West Coast Forest", "3" = "Marine West Coast Forest",
  "4" = "Western Cordillera", "5" = "Western Cordillera", "77" = "Western Cordillera", "78" = "Western Cordillera",
  "6" = "Mediterranean California", "7" = "Mediterranean California", "8" = "Mediterranean California",
  "9" = "Western Cordillera", "10" = "Cold Deserts", "11" = "Western Cordillera",
  "12" = "Cold Deserts", "13" = "Cold Deserts", "14" = "Warm Deserts", "80" = "Cold Deserts",
  "15" = "Western Cordillera", "16" = "Western Cordillera", "17" = "Western Cordillera",
  "18" = "Cold Deserts", "19" = "Western Cordillera", "20" = "Cold Deserts", "21" = "Western Cordillera",
  "22" = "South Central Semi-Arid Prairies", "23" = "Western Cordillera", "79" = "Warm Deserts",
  "24" = "Warm Deserts", "81" = "Warm Deserts",
  # Great Plains
  "25" = "Great Plains", "26" = "South Central Semi-Arid Prairies", "27" = "Great Plains",
  "28" = "Great Plains", "29" = "South Central Semi-Arid Prairies", "30" = "South Central Semi-Arid Prairies",
  "31" = "South Central Semi-Arid Prairies", "32" = "Texas-Louisiana Coastal Plain", "33" = "Texas-Louisiana Coastal Plain",
  "34" = "Texas-Louisiana Coastal Plain", "35" = "Southeastern USA Plains",
  # Central/Midwest
  "36" = "Ozark/Ouachita-Appalachian Forests", "37" = "Ozark/Ouachita-Appalachian Forests",
  "38" = "Ozark/Ouachita-Appalachian Forests", "39" = "Ozark/Ouachita-Appalachian Forests",
  "40" = "Temperate Prairies", "41" = "Western Cordillera",
  "42" = "Great Plains", "43" = "Great Plains", "44" = "Great Plains",
  "46" = "Temperate Prairies", "47" = "Temperate Prairies", "48" = "Temperate Prairies",
  "49" = "Mixed Wood Shield", "50" = "Mixed Wood Shield", "51" = "Temperate Prairies",
  "52" = "Temperate Prairies", "53" = "Temperate Prairies", "54" = "Temperate Prairies", "55" = "Temperate Prairies",
  "56" = "Temperate Prairies", "57" = "Temperate Prairies",
  # Eastern regions
  "58" = "Atlantic Highlands", "59" = "Atlantic Highlands", "82" = "Mixed Wood Shield", "83" = "Mixed Wood Shield",
  "60" = "Mixed Wood Plains", "61" = "Mixed Wood Plains", "62" = "Mixed Wood Plains",
  "63" = "Southeastern USA Plains", "64" = "Mixed Wood Plains", "84" = "Atlantic Highlands",
  "45" = "Southeastern USA Plains", "65" = "Southeastern USA Plains",
  "66" = "Ozark/Ouachita-Appalachian Forests", "67" = "Ozark/Ouachita-Appalachian Forests",
  "68" = "Ozark/Ouachita-Appalachian Forests", "69" = "Ozark/Ouachita-Appalachian Forests",
  "70" = "Ozark/Ouachita-Appalachian Forests", "71" = "Ozark/Ouachita-Appalachian Forests",
  "72" = "Ozark/Ouachita-Appalachian Forests", "73" = "Mississippi Alluvial and Southeast USA Coastal Plains",
  "74" = "Mississippi Alluvial and Southeast USA Coastal Plains",
  "75" = "Mississippi Alluvial and Southeast USA Coastal Plains",
  "76" = "Everglades"
)

#' Parse EPA L4 ecoregion code to extract hierarchy
#' @param code L4 ecoregion code (e.g., "65a", "45c")
#' @return List with l2_name, l3_code, l3_name, l4_code
parse_ecoregion_code <- function(code) {
  if (is.null(code) || is.na(code) || !nzchar(code)) {
    return(list(l2_name = NA, l3_code = NA, l3_name = NA, l4_code = NA))
  }

  code <- trimws(as.character(code))

  # EPA L4 codes are typically "65a", "45c" format
  # The numeric part is the L3 code, the letter is the L4 subdivision
  if (grepl("^\\d+[a-zA-Z]?$", code)) {
    l3_code <- gsub("[a-zA-Z].*$", "", code)
    l3_name <- get_l3_name(l3_code)
    l2_name <- get_l2_name(l3_code)
    list(
      l2_name = l2_name,
      l3_code = l3_code,
      l3_name = l3_name,
      l4_code = code
    )
  } else {
    list(l2_name = NA, l3_code = NA, l3_name = NA, l4_code = code)
  }
}

#' Get L3 ecoregion name from code
#' @param l3_code Level 3 code (numeric, e.g., "65")
#' @return Character name or NA
get_l3_name <- function(l3_code) {
  if (is.null(l3_code) || is.na(l3_code)) return(NA_character_)
  EPA_L3_ECOREGIONS[[as.character(l3_code)]] %||% NA_character_
}

#' Get L2 ecoregion name from L3 code
#' @param l3_code Level 3 code (numeric, e.g., "65")
#' @return Character name or NA
get_l2_name <- function(l3_code) {
  if (is.null(l3_code) || is.na(l3_code)) return(NA_character_)
  EPA_L2_MAPPINGS[[as.character(l3_code)]] %||% NA_character_
}

#' Build EPA ecoregion reference URL
#' @return URL to EPA ecoregion resources
epa_ecoregion_url <- function() {
  "https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states"
}

#' Build ecoregion hierarchy display
#' @param l4_name Level 4 ecoregion name
#' @param l4_code Level 4 ecoregion code
#' @return List with hierarchy info for display
build_ecoregion_hierarchy <- function(l4_name, l4_code) {
  parsed <- parse_ecoregion_code(l4_code)

  list(
    l4_name = l4_name,
    l4_code = l4_code,
    l2_name = parsed$l2_name,
    l3_code = parsed$l3_code,
    l3_name = parsed$l3_name
  )
}

#' Derive ecoregion display name at specified level
#' @param l4_code Level 4 code (e.g., "65a")
#' @param l4_name Level 4 name
#' @param level One of "L2", "L3", "L4"
#' @return Display name for the ecoregion at that level
get_ecoregion_at_level <- function(l4_code, l4_name, level) {
  if (level == "L4") return(l4_name)

  parsed <- parse_ecoregion_code(l4_code)

  if (level == "L3") {
    if (!is.na(parsed$l3_name)) return(parsed$l3_name)
    if (!is.na(parsed$l3_code)) return(paste0("Region ", parsed$l3_code))
    return(l4_name)  # Fallback
  }

  if (level == "L2") {
    if (!is.na(parsed$l2_name)) return(parsed$l2_name)
    return("Unknown Region")
  }

  l4_name
}

# Note: %||% operator defined in R/helpers.R (canonical definition)
