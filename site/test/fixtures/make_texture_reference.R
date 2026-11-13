# Regenerates texture_reference.csv and texture_midpoints.csv from the Shiny
# app's own texture code, so site/netlify/lib/texture.mjs can be tested
# against it. Run from the repo root:
#   Rscript site/test/fixtures/make_texture_reference.R

pick <- function(file, names) {
  env <- new.env()
  for (e in parse(file)) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% names) eval(e, env)
  }
  env
}
h <- pick("app/R/helpers.R", c("TEXTURE_TOLERANCE", "get_texture_percentages", "classify_texture"))
d <- pick("app/R/data.R", "soil_texture_classes")
for (n in ls(h)) assign(n, get(n, h))
tc <- d$soil_texture_classes

grid <- expand.grid(sand = 0:100, clay = 0:100)
grid <- grid[grid$sand + grid$clay <= 100, ]
grid$silt <- 100 - grid$sand - grid$clay
grid$class <- mapply(function(s, si, c) classify_texture(s, si, c, tc), grid$sand, grid$silt, grid$clay)
write.csv(grid[, c("sand", "silt", "clay", "class")], "site/test/fixtures/texture_reference.csv", row.names = FALSE)

mid <- do.call(rbind, lapply(tc$Texture, function(cl) {
  p <- get_texture_percentages(cl, tc)
  data.frame(class = cl, sand = p$sand, silt = p$silt, clay = p$clay)
}))
write.csv(mid, "site/test/fixtures/texture_midpoints.csv", row.names = FALSE)
cat(nrow(grid), "grid points,", length(unique(grid$class)), "classes\n")
