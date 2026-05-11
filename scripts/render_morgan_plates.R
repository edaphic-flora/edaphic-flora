suppressPackageStartupMessages({ library(pdftools) })
f <- "C:/Users/toddt/OneDrive/Desktop/edaphic consulting/02_Analysis/Soil_Reports/CAES_Morgan_Method_Manual_B541R.pdf"
out_dir <- "C:/Users/toddt/OneDrive/Desktop/edaphic flora/data/morgan_plates"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# Plates I-VII appear in the appendix — render pages 56-66 to find them
pages <- 50:66
files <- pdf_convert(f, format = "png", pages = pages, dpi = 200,
                     filenames = file.path(out_dir, sprintf("page_%02d.png", pages)))
cat("Rendered", length(files), "pages to", out_dir, "\n")
print(files)
