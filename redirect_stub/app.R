# Redirect stub for toddtesterman.shinyapps.io/edaphic-flora/
# Bounces any incoming traffic to the new edaphicflora.shinyapps.io URL.
# Self-contained — no DB, no auth, no API keys; safe on shinyapps Free tier.

library(shiny)

NEW_URL <- "https://edaphicflora.shinyapps.io/edaphic-flora/"

ui <- fluidPage(
  title = "Edaphic Flora has moved",

  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Fallback redirect if JS is disabled — fires after 1s so the user sees the message.
    tags$meta(`http-equiv` = "refresh",
              content = paste0("1; url=", NEW_URL)),
    tags$link(rel = "canonical", href = NEW_URL),
    tags$style(HTML(sprintf("
      html, body { height: 100%%; margin: 0; }
      body {
        background: #F7F4E8;
        font-family: 'Helvetica Neue', Arial, sans-serif;
        color: #373D3C;
        display: flex; align-items: center; justify-content: center;
      }
      .moved-card {
        max-width: 480px; padding: 32px 28px; text-align: center;
        background: white; border-left: 4px solid #7A9A86;
        border-radius: 8px; box-shadow: 0 2px 12px rgba(0,0,0,0.06);
      }
      .moved-title {
        font-size: 1.4rem; font-weight: 600; margin: 0 0 12px 0;
      }
      .moved-body { font-size: 1rem; line-height: 1.5; margin: 0 0 20px 0; }
      .moved-cta {
        display: inline-block; padding: 12px 22px;
        background: #7A9A86; color: white; text-decoration: none;
        border-radius: 6px; font-weight: 600;
      }
      .moved-cta:hover { background: #5D7A6A; color: white; }
      .moved-url {
        font-family: 'Courier New', monospace; font-size: 0.85rem;
        color: #6c757d; margin-top: 16px; word-break: break-all;
      }
    "))),
    # Immediate JS redirect (preferred when JS is enabled).
    tags$script(HTML(sprintf(
      "setTimeout(function() { window.location.replace('%s'); }, 800);",
      NEW_URL
    )))
  ),

  div(class = "moved-card",
      h1(class = "moved-title", "Edaphic Flora has moved"),
      p(class = "moved-body",
        "We've moved to a new home. You'll be redirected automatically — ",
        "if not, click below."),
      tags$a(class = "moved-cta", href = NEW_URL, "Take me there →"),
      div(class = "moved-url", NEW_URL)
  )
)

server <- function(input, output, session) {
  # Belt-and-suspenders: server-side redirect via session JS injection.
  session$sendCustomMessage("redirect", NEW_URL)
}

shinyApp(ui = ui, server = server)
