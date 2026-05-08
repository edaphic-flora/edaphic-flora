# Parse-only check for the modified R sources. No DB connection needed.
files <- c(
  "app/R/helpers.R",
  "app/R/db.R",
  "app/R/mod_my_garden.R",
  "app/R/mod_analysis.R"
)
for (f in files) {
  out <- tryCatch({
    parse(file = f)
    "ok"
  }, error = function(e) paste0("PARSE ERROR: ", conditionMessage(e)))
  cat(sprintf("%-30s  %s\n", f, out))
}
