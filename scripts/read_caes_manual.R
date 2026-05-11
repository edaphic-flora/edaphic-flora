suppressPackageStartupMessages({ library(pdftools); library(tesseract) })
f <- "C:/Users/toddt/OneDrive/Desktop/edaphic consulting/02_Analysis/Soil_Reports/CAES_Morgan_Method_Manual_B541R.pdf"
out <- "C:/Users/toddt/OneDrive/Desktop/edaphic flora/data/caes_morgan_manual.txt"

# OCR every page
cat("OCR'ing 67 pages...\n")
txt <- pdftools::pdf_ocr_text(f, language = "eng", dpi = 300)
writeLines(txt, out)
cat("Wrote:", out, " (", nchar(paste(txt, collapse='')), "chars)\n")
cat("\n=== Page 1 ===\n")
cat(txt[1])
