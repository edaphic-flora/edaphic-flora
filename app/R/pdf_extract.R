# R/pdf_extract.R - Soil report extraction using Claude API
# Supports: PDF, RTF, TXT, PNG, JPG, GIF, WebP

library(httr2)
library(jsonlite)

# ---------------------------
# Configuration
# ---------------------------

extract_config <- list(
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  daily_limit = as.integer(Sys.getenv("PDF_EXTRACT_DAILY_LIMIT", "5")),
  model = "claude-sonnet-4-20250514",
  max_tokens = 2000
)

# Supported file types
SUPPORTED_EXTENSIONS <- c("pdf", "rtf", "txt", "png", "jpg", "jpeg", "gif", "webp")

# MIME types for images
IMAGE_MIME_TYPES <- list(

  png = "image/png",
  jpg = "image/jpeg",
  jpeg = "image/jpeg",
  gif = "image/gif",
  webp = "image/webp"
)

# ---------------------------
# Extraction Prompt
# ---------------------------

EXTRACTION_PROMPT <- "
You are extracting soil test data from a soil report. Extract the following fields if present.
Return ONLY a valid JSON object with these exact field names (use null for missing values):

{
  \"ph\": <number or null>,
  \"organic_matter\": <number as percentage or null>,
  \"organic_matter_class\": <string like 'Low', 'Medium', 'High', 'Medium High' or null>,
  \"nitrate_ppm\": <number or null>,
  \"ammonium_ppm\": <number or null>,
  \"phosphorus_ppm\": <number or null>,
  \"potassium_ppm\": <number or null>,
  \"calcium_ppm\": <number or null>,
  \"magnesium_ppm\": <number or null>,
  \"sulfur_ppm\": <number or null>,
  \"iron_ppm\": <number or null>,
  \"manganese_ppm\": <number or null>,
  \"zinc_ppm\": <number or null>,
  \"boron_ppm\": <number or null>,
  \"copper_ppm\": <number or null>,
  \"cec_meq\": <number as meq/100g or null>,
  \"soluble_salts_ppm\": <number or null>,
  \"texture_sand\": <number as percentage or null>,
  \"texture_silt\": <number as percentage or null>,
  \"texture_clay\": <number as percentage or null>,
  \"texture_class\": <string like 'Sandy Loam', 'Clay', 'Loam' or null>,
  \"sample_date\": <string as YYYY-MM-DD or null>,
  \"lab_name\": <string or null>,
  \"sample_id\": <string - lab sample number/ID if present or null>,
  \"notes\": <string with any additional relevant info or null>,
  \"extraction_warnings\": <array of strings describing any data quality issues or null>
}

Important extraction rules:
- If values are given as 'Qualitative (numeric ppm)' like 'Medium High (50 ppm)', extract BOTH the number (50) AND include the qualitative description where applicable
- For organic matter: extract the numeric percentage if available, AND always extract the qualitative class (Low/Medium/High/etc.) if given
- Texture: if only a texture class name is given (e.g., 'Sandy Loam') without percentages, that's fine - extract the class name
- CEC should be in meq/100g
- If a value is given as a range, use the midpoint
- Extract numeric values without units

Multi-sample reports:
- If the report contains MULTIPLE soil samples, extract ONLY THE FIRST sample's data
- Add a warning to extraction_warnings like: 'Report contains X samples; only first sample extracted'

Add to extraction_warnings if:
- Multiple samples exist in report
- Any values seem unusual or potentially misread
- Units are ambiguous
- Data appears incomplete

Return ONLY the JSON object, no other text.
"

# ---------------------------
# File Type Detection
# ---------------------------

get_file_extension <- function(file_path) {
  ext <- tools::file_ext(file_path)
  tolower(ext)
}

is_supported_format <- function(file_path) {
  ext <- get_file_extension(file_path)
  ext %in% SUPPORTED_EXTENSIONS
}

# ---------------------------
# File to Base64
# ---------------------------

file_to_base64 <- function(file_path) {
  tryCatch({
    raw_data <- readBin(file_path, "raw", file.info(file_path)$size)
    base64enc::base64encode(raw_data)
  }, error = function(e) {
    message("Error reading file: ", e$message)
    NULL
  })
}

# ---------------------------
# RTF to Text Conversion
# ---------------------------

rtf_to_text <- function(file_path) {
  tryCatch({
    # Try using striprtf package if available
    if (requireNamespace("striprtf", quietly = TRUE)) {
      text <- striprtf::read_rtf(file_path)
      return(paste(text, collapse = "\n"))
    }

    # Fallback: basic RTF stripping (removes common RTF control words)
    raw_text <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    text <- paste(raw_text, collapse = "\n")

    # Remove RTF header and control words
    text <- gsub("\\{\\\\rtf[^}]*\\}", "", text)
    text <- gsub("\\\\[a-z]+[0-9]*\\s?", " ", text)
    text <- gsub("\\{|\\}", "", text)
    text <- gsub("\\s+", " ", text)
    trimws(text)
  }, error = function(e) {
    message("Error converting RTF: ", e$message)
    NULL
  })
}

# ---------------------------
# Text File Reading
# ---------------------------

read_text_file <- function(file_path) {
  tryCatch({
    text <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    paste(text, collapse = "\n")
  }, error = function(e) {
    message("Error reading text file: ", e$message)
    NULL
  })
}

# ---------------------------
# Build API Message Content
# ---------------------------

build_message_content <- function(file_path) {
  ext <- get_file_extension(file_path)

  if (ext == "pdf") {
    # PDF: send as document
    base64_data <- file_to_base64(file_path)
    if (is.null(base64_data)) return(NULL)

    list(
      list(
        type = "document",
        source = list(
          type = "base64",
          media_type = "application/pdf",
          data = base64_data
        )
      ),
      list(type = "text", text = EXTRACTION_PROMPT)
    )

  } else if (ext %in% c("png", "jpg", "jpeg", "gif", "webp")) {
    # Image: send as image
    base64_data <- file_to_base64(file_path)
    if (is.null(base64_data)) return(NULL)

    mime_type <- IMAGE_MIME_TYPES[[ext]]

    list(
      list(
        type = "image",
        source = list(
          type = "base64",
          media_type = mime_type,
          data = base64_data
        )
      ),
      list(type = "text", text = EXTRACTION_PROMPT)
    )

  } else if (ext == "rtf") {
    # RTF: convert to text and send as text
    text_content <- rtf_to_text(file_path)
    if (is.null(text_content) || !nzchar(text_content)) return(NULL)

    list(
      list(
        type = "text",
        text = paste0("Here is the soil report content:\n\n", text_content, "\n\n", EXTRACTION_PROMPT)
      )
    )

  } else if (ext == "txt") {
    # Plain text: read and send as text
    text_content <- read_text_file(file_path)
    if (is.null(text_content) || !nzchar(text_content)) return(NULL)

    list(
      list(
        type = "text",
        text = paste0("Here is the soil report content:\n\n", text_content, "\n\n", EXTRACTION_PROMPT)
      )
    )

  } else {
    NULL
  }
}

# ---------------------------
# Claude API Call
# ---------------------------

extract_soil_data <- function(file_path) {
  # Check API key
  if (!nzchar(extract_config$api_key)) {
    return(list(
      success = FALSE,
      error = "Anthropic API key not configured. Please set ANTHROPIC_API_KEY in .Renviron"
    ))
  }

  # Check file format
  if (!is_supported_format(file_path)) {
    ext <- get_file_extension(file_path)
    return(list(
      success = FALSE,
      error = paste0("Unsupported file format: .", ext,
                     ". Supported formats: PDF, RTF, TXT, PNG, JPG, GIF, WebP")
    ))
  }

  # Build message content based on file type
  message_content <- build_message_content(file_path)
  if (is.null(message_content)) {
    return(list(
      success = FALSE,
      error = "Failed to read file content"
    ))
  }

  # Build API request
  tryCatch({
    response <- request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        "x-api-key" = extract_config$api_key,
        "anthropic-version" = "2023-06-01",
        "content-type" = "application/json"
      ) |>
      req_body_json(list(
        model = extract_config$model,
        max_tokens = extract_config$max_tokens,
        messages = list(
          list(
            role = "user",
            content = message_content
          )
        )
      )) |>
      req_timeout(60) |>
      req_perform()

    # Parse response
    result <- resp_body_json(response)

    if (!is.null(result$content) && length(result$content) > 0) {
      json_text <- result$content[[1]]$text

      # Try to parse the JSON response
      extracted <- tryCatch({
        fromJSON(json_text)
      }, error = function(e) {
        # Try to extract JSON from response if wrapped in other text
        json_match <- regmatches(json_text, regexpr("\\{[^{}]*\\}", json_text, perl = TRUE))
        if (length(json_match) > 0) {
          fromJSON(json_match[1])
        } else {
          NULL
        }
      })

      if (!is.null(extracted)) {
        # Get token usage for logging
        tokens_used <- NULL
        if (!is.null(result$usage)) {
          tokens_used <- result$usage$input_tokens + result$usage$output_tokens
        }

        return(list(
          success = TRUE,
          data = extracted,
          tokens_used = tokens_used
        ))
      } else {
        return(list(
          success = FALSE,
          error = "Failed to parse extraction results"
        ))
      }
    } else {
      return(list(
        success = FALSE,
        error = "No content in API response"
      ))
    }

  }, error = function(e) {
    error_msg <- e$message

    # Try to get more specific error from API response
    if (inherits(e, "httr2_http_error")) {
      tryCatch({
        body <- resp_body_json(e$response)
        if (!is.null(body$error$message)) {
          error_msg <- body$error$message
        }
      }, error = function(e2) {})
    }

    return(list(
      success = FALSE,
      error = paste("API error:", error_msg)
    ))
  })
}

# Backward compatibility alias
extract_soil_data_from_pdf <- extract_soil_data

# ---------------------------
# Helper: Check if extraction is available
# ---------------------------

is_pdf_extraction_available <- function() {
  nzchar(extract_config$api_key)
}

