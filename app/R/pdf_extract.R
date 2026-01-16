# R/pdf_extract.R - PDF soil report extraction using Claude API

library(httr2)
library(jsonlite)

# ---------------------------
# Configuration
# ---------------------------

pdf_extract_config <- list(
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  daily_limit = as.integer(Sys.getenv("PDF_EXTRACT_DAILY_LIMIT", "5")),
  model = "claude-sonnet-4-20250514",
  max_tokens = 2000
)

# ---------------------------
# Extraction Prompt
# ---------------------------

EXTRACTION_PROMPT <- "
You are extracting soil test data from a PDF report. Extract the following fields if present.
Return ONLY a valid JSON object with these exact field names (use null for missing values):

{
  \"ph\": <number or null>,
  \"organic_matter\": <number as percentage or null>,
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
  \"texture_class\": <string like 'Sandy Loam' or null>,
  \"sample_date\": <string as YYYY-MM-DD or null>,
  \"lab_name\": <string or null>,
  \"notes\": <string with any additional relevant info or null>
}

Important:
- Extract numeric values only (no units in the numbers)
- For organic matter, use the percentage value
- CEC should be in meq/100g
- If a value is given as a range, use the midpoint
- If texture percentages don't add to 100, still extract what's available
- Return ONLY the JSON object, no other text
"

# ---------------------------
# PDF to Base64
# ---------------------------

pdf_to_base64 <- function(file_path) {
  tryCatch({
    raw_data <- readBin(file_path, "raw", file.info(file_path)$size)
    base64enc::base64encode(raw_data)
  }, error = function(e) {
    message("Error reading PDF: ", e$message)
    NULL
  })
}

# ---------------------------
# Claude API Call
# ---------------------------

extract_soil_data_from_pdf <- function(file_path) {
  # Check API key
  if (!nzchar(pdf_extract_config$api_key)) {
    return(list(
      success = FALSE,
      error = "Anthropic API key not configured. Please set ANTHROPIC_API_KEY in .Renviron"
    ))
  }

  # Convert PDF to base64
  pdf_base64 <- pdf_to_base64(file_path)
  if (is.null(pdf_base64)) {
    return(list(
      success = FALSE,
      error = "Failed to read PDF file"
    ))
  }

  # Build API request
  tryCatch({
    response <- request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        "x-api-key" = pdf_extract_config$api_key,
        "anthropic-version" = "2023-06-01",
        "content-type" = "application/json"
      ) |>
      req_body_json(list(
        model = pdf_extract_config$model,
        max_tokens = pdf_extract_config$max_tokens,
        messages = list(
          list(
            role = "user",
            content = list(
              list(
                type = "document",
                source = list(
                  type = "base64",
                  media_type = "application/pdf",
                  data = pdf_base64
                )
              ),
              list(
                type = "text",
                text = EXTRACTION_PROMPT
              )
            )
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

# ---------------------------
# Helper: Check if extraction is available
# ---------------------------

is_pdf_extraction_available <- function() {
  nzchar(pdf_extract_config$api_key)
}
