# auth_smoke.R  — minimal & fixed
if (file.exists(".Renviron")) readRenviron(".Renviron")

# keep origin stable
if (interactive()) {
  options(shiny.launch.browser = TRUE)
  options(shiny.host = "127.0.0.1")
  options(shiny.port = 7420)
}

stopifnot(
  nzchar(Sys.getenv("POLISHED_APP_NAME")),
  nzchar(Sys.getenv("POLISHED_API_KEY")),
  nzchar(Sys.getenv("FIREBASE_API_KEY")),
  nzchar(Sys.getenv("FIREBASE_AUTH_DOMAIN")),
  nzchar(Sys.getenv("FIREBASE_PROJECT_ID"))
)

library(shiny)
library(bslib)
library(polished)

firebase_cfg <- list(
  apiKey     = Sys.getenv("FIREBASE_API_KEY"),
  authDomain = Sys.getenv("FIREBASE_AUTH_DOMAIN"),
  projectId  = Sys.getenv("FIREBASE_PROJECT_ID")
)

polished::polished_config(
  app_name  = Sys.getenv("POLISHED_APP_NAME"),
  api_key   = Sys.getenv("POLISHED_API_KEY"),
  firebase_config   = firebase_cfg,
  sign_in_providers = c("email","google"),
  is_invite_required = FALSE,
  is_email_verification_required = FALSE
)

ui_core <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  card(
    card_header("Auth Smoke Test"),
    p("If sign-in succeeds, your user object appears below:"),
    verbatimTextOutput("who")
  )
)

ui <- polished::secure_ui(ui_core)
server_inner <- function(input, output, session) {
  output$who <- renderPrint({
    f <- session$userData$user
    if (is.null(f)) return("no user function yet (not authed)")
    u <- f()
    if (is.null(u)) return("not signed in")
    str(u, max.level = 1)
  })
}
server <- polished::secure_server(server_inner)

shinyApp(ui, server)
