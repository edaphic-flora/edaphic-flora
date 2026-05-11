# Look up Eric Winter's user_uid via the polished API package.
suppressPackageStartupMessages({
  library(polished)
})

readRenviron("app/.Renviron")
api_key  <- Sys.getenv("POLISHED_API_KEY")
app_name <- Sys.getenv("POLISHED_APP_NAME")

stopifnot(api_key != "", app_name != "")
cat("App:", app_name, "\n")

cat("polished functions exposed:\n")
print(grep("^get_", ls("package:polished"), value = TRUE))

app_uid <- "5190f174-f77a-40e6-aceb-aff72479a07c"
cat("app_uid:", app_uid, "\n")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
res <- get_app_users(app_uid = app_uid, api_key = api_key)
cat("class:", paste(class(res), collapse=","), "\n")
cat("names:", paste(names(res), collapse=","), "\n")
# polished_api_res wraps an httr response — inner response is in $response
inner <- res$response %||% res
body_text <- rawToChar(inner$content)
body <- jsonlite::fromJSON(body_text, simplifyVector = TRUE, flatten = TRUE)
cat("body class:", class(body), " names:", paste(names(body), collapse=","), "\n")
str(body, max.level = 2)
if (is.data.frame(body)) {
  print(body[, intersect(c("user_uid","email","is_admin","invite_status","created_at"), names(body))])
} else if (is.list(body) && !is.null(body$users)) {
  print(body$users)
} else {
  cat("Raw body text (first 1500 chars):\n", substr(body_text, 1, 1500), "\n")
}
cat("Class:", class(users), "  rows:", if (is.data.frame(users)) nrow(users) else length(users), "\n")
if (is.data.frame(users)) {
  cat("Columns:", paste(names(users), collapse = ", "), "\n\n")
  cat("== Search wintereric22 / winter / eric22 ==\n")
  hit <- users[grepl("wintereric22|winter|eric22", users$email, ignore.case = TRUE), ]
  print(hit)
  cat("\n== Recent 10 users (any) ==\n")
  print(utils::head(users[order(users$created_at, decreasing = TRUE), ], 10))
} else {
  str(users)
}
