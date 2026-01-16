# R/helpers.R - Helper functions for soil analysis

library(sf)

# ---------------------------
# Ecoregion Lookup
# ---------------------------

get_ecoregion <- function(lat, long, eco_sf) {
  if (is.null(lat) || is.null(long) || is.na(lat) || is.na(long)) {
    return(list(name = NA_character_, code = NA_character_))
  }
  tryCatch({
    pt <- st_as_sf(data.frame(long = long, lat = lat), coords = c("long", "lat"), crs = 4326)
    inters <- st_intersects(eco_sf, pt, sparse = FALSE)
    if (any(inters)) {
      idx <- which(inters)[1]
      list(name = as.character(eco_sf$us_l4name[idx]),
           code = as.character(eco_sf$us_l4code[idx]))
    } else {
      list(name = NA_character_, code = NA_character_)
    }
  }, error = function(e) {
    list(name = NA_character_, code = NA_character_)
  })
}

# ---------------------------
# Soil Texture Functions
# ---------------------------

get_texture_percentages <- function(texture_class, soil_texture_classes) {
  cd <- soil_texture_classes[soil_texture_classes$Texture == texture_class, ]
  if (!nrow(cd)) return(NULL)
  sand_mid <- mean(c(cd$Sand_Min, cd$Sand_Max))
  silt_mid <- mean(c(cd$Silt_Min, cd$Silt_Max))
  clay_mid <- mean(c(cd$Clay_Min, cd$Clay_Max))
  total <- sand_mid + silt_mid + clay_mid
  list(
    sand = sand_mid * (100 / total),
    silt = silt_mid * (100 / total),
    clay = clay_mid * (100 / total)
  )
}

classify_texture <- function(sand, silt, clay, soil_texture_classes) {
  if (abs((sand + silt + clay) - 100) > 0.1) {
    return("Error: Percentages must sum to 100%")
  }
  for (i in seq_len(nrow(soil_texture_classes))) {
    if (clay >= (soil_texture_classes$Clay_Min[i] - 0.1) &&
        clay <= (soil_texture_classes$Clay_Max[i] + 0.1) &&
        silt >= (soil_texture_classes$Silt_Min[i] - 0.1) &&
        silt <= (soil_texture_classes$Silt_Max[i] + 0.1) &&
        sand >= (soil_texture_classes$Sand_Min[i] - 0.1) &&
        sand <= (soil_texture_classes$Sand_Max[i] + 0.1)) {
      return(as.character(soil_texture_classes$Texture[i]))
    }
  }
  "Unclassified"
}
