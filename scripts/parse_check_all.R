files <- list.files("app", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
errs <- 0
for (f in files) {
  ok <- tryCatch({ parse(f); TRUE },
                 error = function(e) { cat(sprintf("FAIL %s: %s\n", f, conditionMessage(e))); FALSE })
  if (!ok) errs <- errs + 1
}
cat(sprintf("\nParsed %d files, %d errors\n", length(files), errs))
