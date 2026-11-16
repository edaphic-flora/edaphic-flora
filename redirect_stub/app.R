# Redirect page for the retired Shiny app. It replaces the app at both
# shinyapps.io addresses and sends every visitor to edaphicflora.org.
# Self-contained: no database, no sign-in, no keys. Fine on the shinyapps Free plan.
#
# Deploy at launch, once edaphicflora.org is live (run from the repo root in R):
#
#   rsconnect::deployApp("redirect_stub", appName = "edaphic-flora",
#                        account = "edaphicflora", forceUpdate = TRUE)
#   rsconnect::deployApp("redirect_stub", appName = "edaphic-flora",
#                        account = "toddtesterman", forceUpdate = TRUE)
#
# The first replaces the beta app itself; the second updates the older address,
# which until now pointed at the beta. After that the edaphicflora account can
# drop to the Free plan.

library(shiny)

NEW_URL <- "https://edaphicflora.org/"

ui <- fluidPage(
  title = "Edaphic Flora has moved",

  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Search engines should keep the new address, not this page
    tags$meta(name = "robots", content = "noindex"),
    tags$link(rel = "canonical", href = NEW_URL),
    # Works without JavaScript: goes to the new site after 4 seconds
    tags$meta(`http-equiv` = "refresh", content = paste0("4; url=", NEW_URL)),
    tags$style(HTML("
      html, body { height: 100%; margin: 0; }
      body {
        background: #F7F4E8;
        font-family: 'Helvetica Neue', Arial, sans-serif;
        color: #373D3C;
        display: flex; align-items: center; justify-content: center;
      }
      .moved-card {
        max-width: 480px; margin: 16px; padding: 32px 28px; text-align: center;
        background: #EFEAD6; border-left: 4px solid #7A9A86;
        border-radius: 8px; box-shadow: 0 2px 12px rgba(0,0,0,0.06);
      }
      .moved-title { font-size: 1.4rem; font-weight: 600; margin: 0 0 12px 0; }
      .moved-body { font-size: 1rem; line-height: 1.5; margin: 0 0 12px 0; }
      .moved-cta {
        display: inline-block; margin-top: 8px; padding: 12px 22px;
        background: #373D3C; color: #F7F4E8; text-decoration: none;
        border-radius: 6px; font-weight: 600;
      }
      .moved-cta:hover { background: #2B302F; color: #F7F4E8; }
      .moved-url { font-size: 0.9rem; color: #5b6360; margin-top: 16px; word-break: break-all; }
    ")),
    # Sooner when JavaScript is on (long enough to read the message)
    tags$script(HTML(sprintf(
      "setTimeout(function() { window.location.replace('%s'); }, 3000);",
      NEW_URL
    )))
  ),

  div(class = "moved-card",
      h1(class = "moved-title", "Edaphic Flora has moved"),
      p(class = "moved-body",
        "We've moved to a new home. You'll be redirected automatically. If not, use the button below."),
      p(class = "moved-body",
        "Your account and your soil tests moved too: sign in there with the same email."),
      tags$a(class = "moved-cta", href = NEW_URL, "Go to edaphicflora.org"),
      div(class = "moved-url", NEW_URL)
  )
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
