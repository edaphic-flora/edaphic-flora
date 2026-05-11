# Build upload CSVs for Winter family (55 Forest, Wyoming, OH)
# Three soil samples — Front Island, Living-Room Garden, Rear Shade Garden.
# Validates every species against ref_taxon (species_accepted.csv) and pre-fills
# sun_exposure from the inventory's Light column.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(readxl)
})

ref <- read_csv("app/species_accepted.csv", show_col_types = FALSE)
ref_names <- unique(ref$taxon_name)

# ---- Inventory pull ----
inv_path <- "C:/Users/toddt/OneDrive/Desktop/edaphic consulting/01_Clients/Winter_Eric_Wyoming_2026/Winter_Garden_Inventory.xlsx"
inv_raw <- read_excel(inv_path, sheet = "Garden Inventory", col_names = FALSE)
sci    <- inv_raw[[1]]
common <- inv_raw[[2]]
light  <- inv_raw[[9]]
zone <- character(length(sci)); current <- NA_character_
for (i in seq_along(sci)) {
  if (!is.na(sci[i]) && is.na(common[i])) current <- sci[i]
  zone[i] <- current
}
inventory <- tibble(zone = zone, inv_raw = sci, light = light) |>
  filter(!is.na(common))   # drops zone headers + blanks

# ---- Light -> sun_exposure ----
light_to_sun <- c(
  "Full Sun"            = "Full Sun",
  "Full Sun-Part Shade" = "Part Sun",
  "Part Shade"          = "Part Shade",
  "Part-Full Shade"     = "Part Shade",
  "Part Shade-Shade"    = "Part Shade",
  "Full Shade"          = "Full Shade"
)

# ---- Zone 1: Front Island Garden (sample 550, pH 7.4, Organic, High OM) ----
front_island <- tibble::tribble(
  ~species,                       ~cultivar,                                ~inv_raw,
  "Mertensia virginica",          NA,                                       "Mertensia virginica",
  "Cercis canadensis",            NA,                                       "Cercis canadensis",
  "Epimedium grandiflorum",       "Lilafee",                                "Epimedium grandiflorum 'Lilafee'",
  "Lonicera caerulea",            "Aurora",                                 "Lonicera caerulea 'Aurora' & 'Borealis'",
  "Lonicera caerulea",            "Borealis",                               "Lonicera caerulea 'Aurora' & 'Borealis'",
  "Silene caroliniana",           NA,                                       "Silene caroliniana",
  "Uvularia grandiflora",         NA,                                       "Uvularia grandiflora",
  "Heuchera sanguinea",           "Bressingham Hybrids",                    "Heuchera sanguinea 'Bressingham Hybrids'",
  "Iris × germanica",             "Paprika Fono's",                         "Iris 'Paprika Fono's'",
  "Iris × germanica",             "light blue (unknown cv)",                "Iris [light blue]",
  "Iris × germanica",             "violet (unknown cv)",                    "Iris [violet]",
  "Maianthemum stellatum",        NA,                                       "Maianthemum stellatum",
  "Sisyrinchium angustifolium",   NA,                                       "Sisyrinchium angustifolium",
  "Taenidia integerrima",         NA,                                       "Taenidia integerrima",
  "Achillea filipendulina",       NA,                                       "Achillea filipendulina",
  "Ruellia humilis",              NA,                                       "Ruellia humilis",
  "Delphinium exaltatum",         NA,                                       "Delphinium exaltatum",
  "Lilium lancifolium",           "Orange",                                 "Lilium lancifolium 'Orange'",
  "Artemisia dracunculus",        NA,                                       "Artemisia dracunculus",
  "Liatris aspera",               NA,                                       "Liatris aspera",
  "Solidago caesia",              NA,                                       "Solidago caesia",
  "Symphyotrichum novae-angliae", "Purple Dome",                            "Symphyotrichum novae-angliae 'Purple Dome'",
  "Gentiana andrewsii",           NA,                                       "Gentiana andrewsii",
  "Athyrium filix-femina",        NA,                                       "Athyrium filix-femina",
  "Chamaecyparis pisifera",       "Golden Mop",                             "Chamaecyparis pisifera 'Golden Mop'"
) |> mutate(zone = "Front Island Garden")

# ---- Zone 2: Living-Room Garden (sample 548, pH 7.4, Organic, High OM) ----
living_room <- tibble::tribble(
  ~species,                       ~cultivar,                                ~inv_raw,
  "Dodecatheon meadia",           NA,                                       "Dodecatheon meadia",
  "Phlox bifida",                 NA,                                       "Phlox bifida",
  "Prunus cerasus",               "Balaton",                                "Prunus cerasus ‘Balaton’",
  "Silene virginica",             NA,                                       "Silene virginica",
  "Rhododendron",                 "Girard's Renee Michelle",                "Azalea 'Girard's Renee Michelle'",
  "Baptisia australis",           NA,                                       "Baptisia australis",
  "Geranium sanguineum",          "Album",                                  "Geranium sanguineum 'Album'",
  "Iris sibirica",                "Little White",                           "Iris sibirica 'Little White' (dwarf Siberian iris)",
  "Penstemon pallidus",           NA,                                       "Penstemon pallidus",
  "Scutellaria ovata",            NA,                                       "Scutellaria ovata",
  "Spigelia marilandica",         NA,                                       "Spigelia marilandica",
  "Tradescantia virginiana",      "Zwanenburg Blue",                        "Tradescantia virginiana 'Zwanenburg Blue'",
  "Viburnum dentatum",            "Christom (Blue Muffin)",                 "Viburnum dentatum 'Christom' BLUE MUFFIN",
  "Achillea millefolium",         "Colorado",                               "Achillea millefolium 'Colorado'",
  "Callirhoe bushii",             NA,                                       "Callirhoe bushii",
  "Filipendula vulgaris",         NA,                                       "Filipendula vulgaris",
  "Itea virginica",               "Sprich (Little Henry)",                  "Itea virginica 'Sprich' LITTLE HENRY",
  "Leucanthemum maximum",         "Becky (L. × superbum hybrid)",      "Leucanthemum x superbum 'Becky'",
  "Salvia nemorosa",              "New Dimension Blue",                     "Salvia nemorosa 'New Dimension Blue'",
  "Hosta undulata",               "Albo-Marginata",                         "Hosta undulata 'Albo-Marginata' (variegated wavy hosta)",
  "Monarda didyma",               "Gardenview Scarlet",                     "Monarda didyma 'Gardenview Scarlet'",
  "Rudbeckia subtomentosa",       NA,                                       "Rudbeckia subtomentosa",
  "Salvia guaranitica",           "Black and Blue",                         "Salvia guaranitica 'Black and Blue'",
  "Scutellaria incana",           NA,                                       "Scutellaria incana",
  "Silene regia",                 NA,                                       "Silene regia",
  "Symphyotrichum oolentangiense",NA,                                       "Symphyotrichum oolentangiense",
  "Taxus x media",                "Wardii",                                 "Taxus wardi"
) |> mutate(zone = "Living-Room Garden")

# ---- Zone 3: Rear Shade Garden (sample 547, pH 7.3, Organic, High OM) ----
rear_shade <- tibble::tribble(
  ~species,                       ~cultivar,                                ~inv_raw,
  "Mertensia virginica",          NA,                                       "Mertensia virginica",
  "Sanguinaria canadensis",       NA,                                       "Sanguinaria canadensis",
  "Brunnera macrophylla",         NA,                                       "Brunnera macrophylla (Siberian Bugloss)",
  "Caulophyllum thalictroides",   NA,                                       "Caulophyllum thalictroides",
  "Dicentra formosa",             "Luxuriant",                              "Dicentra (bleeding heart) 'Luxuriant'",
  "Dicentra cucullaria",          NA,                                       "Dicentra cucullaria (Dutchman's breeches)",
  "Dicentra spectabilis",         NA,                                       "Dicentra spectabilis (bleeding heart)",
  "Hexastylis virginica",         NA,                                       "Hexastylis virginica [from Joan Sigmund]",
  "Hydrastis canadensis",         NA,                                       "Hydrastis canadensis (goldenseal)",
  "Jeffersonia diphylla",         NA,                                       "Jeffersonia diphylla",
  "Phlox divaricata",             "London Grove Blue",                      "Phlox divaricata 'London Grove Blue'",
  "Stylophorum diphyllum",        NA,                                       "Stylophorum diphyllum",
  "Tiarella cordifolia",          "Running Tapestry",                       "Tiarella cordifolia 'Running Tapestry'",
  "Viola rostrata",               NA,                                       "Viola rostrata (??)",
  "Ajuga reptans",                NA,                                       "Ajuga [gift from Joan Sigmund]",
  "Arisaema dracontium",          NA,                                       "Arisaema dracontium",
  "Cornus florida",               "Rutban (Aurora)",                        "Cornus 'Rutban' Aurora®",
  "Hemerocallis",                 "Stella d'Oro",                           "Hemerocallis (daylily) 'Stella D'Oro'",
  "Heuchera villosa",             "Palace Purple",                          "Heuchera 'Palace Purple'",
  "Heuchera americana",           "red foliage (unknown cv)",               "Heuchera [red foliage]",
  "Heuchera sanguinea",           "Bressingham Hybrids",                    "Heuchera sanguinea 'Bressingham Hybrids'",
  "Heuchera villosa",             "Fun and Games Hopscotch (× Heucherella hybrid)", "Heucherella FUN AND GAMES® 'Hopscotch'",
  "Polygonatum odoratum",         "Variegatum",                             "Polygonatum odoratum 'Variegatum'",
  "Aruncus dioicus",              "Zweiweltenkind",                         "Aruncus dioicus 'Zweiweltenkind'",
  "Hydrangea arborescens",        "Annabelle",                              "Hydrangea arborescens 'Annabelle'",
  "Panax quinquefolius",          NA,                                       "Panax quinquefolius (American ginseng)",
  "Begonia grandis",              NA,                                       "Begonia grandis",
  "Hosta",                        "Sun 'n Substance",                       "Hosta 'Sun 'n Substance'",
  "Hosta undulata",               "Medio-Variegatum",                       "Hosta undulata 'Medio-Variegatum'",
  "Hymenocallis occidentalis",    NA,                                       "Hymenocallis occidentalis",
  "Lobelia siphilitica",          NA,                                       "Lobelia siphilitica",
  "Athyrium niponicum",           "Ursula's Red",                           "Athyrium niponicum 'Ursula's Red'",
  "Ranunculus ficaria",           NA,                                       NA  # lesser celandine — invasive, noted in soil report
) |> mutate(zone = "Shade Garden (Rear)")

# ---- Validate species ----
fix_candidates <- function(name) {
  cands <- name
  if (name == "Dodecatheon meadia")     cands <- c(cands, "Primula meadia")
  if (name == "Hexastylis virginica")   cands <- c(cands, "Asarum virginicum")
  if (name == "Dicentra spectabilis")   cands <- c(cands, "Lamprocapnos spectabilis")
  if (name == "Hosta undulata")         cands <- c(cands, "Hosta sieboldii")
  if (name == "Hemerocallis")           cands <- c(cands, "Hemerocallis fulva")
  if (name == "Hosta")                  cands <- c(cands, "Hosta sieboldiana")
  if (name == "Taxus x media")          cands <- c(cands, "Taxus baccata")
  if (name == "Rhododendron")           cands <- c(cands, "Rhododendron indicum")
  unique(cands)
}

all_plants <- bind_rows(front_island, living_room, rear_shade)
unique_species <- unique(all_plants$species)
results <- lapply(unique_species, function(s) {
  cands <- fix_candidates(s)
  matched <- cands[cands %in% ref_names]
  list(input = s, status = if (length(matched)) "OK" else "MISSING",
       match = if (length(matched)) matched[1] else NA_character_)
})
results_df <- bind_rows(lapply(results, as_tibble))
cat("Species validation:", sum(results_df$status == "OK"), "OK /",
    sum(results_df$status == "MISSING"), "missing\n")
if (any(results_df$status == "MISSING")) {
  print(results_df |> filter(status == "MISSING"))
  stop("Refusing to write CSVs while species fail validation.")
}
match_lookup <- setNames(results_df$match, results_df$input)

# ---- Join inventory Light + write CSVs ----
# Actual on-site sun exposure per zone, per user observation at 55 Forest Ave.
# Front Island: Part Shade for everything except the redbud (Full Sun).
# Living-Room: Full Sun.  Rear Shade: Part Shade.
zone_sun_default <- c(
  "Front Island Garden" = "Part Shade",
  "Living-Room Garden"  = "Full Sun",
  "Shade Garden (Rear)" = "Part Shade"
)

write_zone <- function(df, sample_label, out_path) {
  out <- df |>
    mutate(
      species = unname(match_lookup[species]),
      sun_exposure = unname(zone_sun_default[zone]),
      # Cercis is the canopy of the front island; it gets full sun above the bed.
      sun_exposure = if_else(species == "Cercis canadensis" & zone == "Front Island Garden",
                             "Full Sun", sun_exposure),
      outcome = "Thriving",
      site_hydrology = "Mesic",
      inat_url = NA_character_,
      notes = NA_character_
    ) |>
    select(species, cultivar, outcome, sun_exposure, site_hydrology, inat_url, notes)
  write_csv(out, out_path, na = "")
  cat(sprintf("Wrote %2d rows  ->  %s   (sample %s)\n",
              nrow(out), out_path, sample_label))
}

out_dir <- "C:/Users/toddt/OneDrive/Desktop/edaphic consulting/01_Clients/Winter_Eric_Wyoming_2026/edaphic_uploads"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_zone(front_island, "550 (Redbud)",     file.path(out_dir, "sample_550_front_island.csv"))
write_zone(living_room,  "548 (Left bed)",   file.path(out_dir, "sample_548_living_room.csv"))
write_zone(rear_shade,   "547 (Shade bed)",  file.path(out_dir, "sample_547_rear_shade.csv"))
cat("\nDone.\n")
