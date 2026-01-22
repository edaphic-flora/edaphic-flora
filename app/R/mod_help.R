# R/mod_help.R - Help menu module (Field Guide + FAQ)
# Static content, no server logic required

# ---------------------------
# UI
# ---------------------------

helpUI <- function(id) {
  ns <- NS(id)

  nav_menu(
    title = "Help",
    icon = icon("circle-question"),
    align = "right",

    # Field Guide
    nav_panel(
      title = "Field Guide",
      icon = icon("book-open"),
      layout_columns(
        col_widths = 12,
        card(
          card_header(icon("book-open"), "Field Guide"),
          card_body(
            class = "px-md-5",

            h4("Soil Properties"),
            tags$dl(class = "row",
              tags$dt(class = "col-sm-3", "Soil pH"),
              tags$dd(class = "col-sm-9", "Measures acidity/alkalinity on a scale of 0-14. Most plants prefer 6.0-7.0. ",
                      "Values below 7 are acidic; above 7 are alkaline."),
              tags$dt(class = "col-sm-3", "Organic Matter (%)"),
              tags$dd(class = "col-sm-9", "Decomposed plant/animal material. Higher values (3-5%) indicate fertile soil ",
                      "with good water retention and microbial activity.")
            ),

            h4(class = "mt-4", "Nutrients (ppm)"),
            tags$dl(class = "row",
              tags$dt(class = "col-sm-3", "Nitrate (NO\u2083)"),
              tags$dd(class = "col-sm-9", "Primary nitrogen source for plants. Optimal: 25-50 ppm for most crops."),
              tags$dt(class = "col-sm-3", "Phosphorus (P)"),
              tags$dd(class = "col-sm-9", "Essential for root development and flowering. Optimal: 25-50 ppm."),
              tags$dt(class = "col-sm-3", "Potassium (K)"),
              tags$dd(class = "col-sm-9", "Supports overall plant health and disease resistance. Optimal: 150-250 ppm."),
              tags$dt(class = "col-sm-3", "Calcium (Ca)"),
              tags$dd(class = "col-sm-9", "Structural component of cell walls. Optimal: 1000-2000 ppm."),
              tags$dt(class = "col-sm-3", "Magnesium (Mg)"),
              tags$dd(class = "col-sm-9", "Central atom in chlorophyll. Optimal: 50-120 ppm."),
              tags$dt(class = "col-sm-3", "Soluble Salts"),
              tags$dd(class = "col-sm-9", "Total dissolved salts. High values (>1000 ppm) can stress plants.")
            ),

            h4(class = "mt-4", "Soil Texture"),
            p("Texture is determined by the proportions of sand, silt, and clay particles:"),
            tags$ul(
              tags$li(tags$strong("Sand"), " (0.05-2mm): Large particles, drains quickly, low nutrient retention"),
              tags$li(tags$strong("Silt"), " (0.002-0.05mm): Medium particles, good moisture retention"),
              tags$li(tags$strong("Clay"), " (<0.002mm): Tiny particles, holds water and nutrients but may drain poorly")
            ),
            p("The texture triangle in the Analysis tab shows your samples plotted by their sand/silt/clay percentages."),

            h4(class = "mt-4", "Plant Performance"),
            p("Track how plants perform in different conditions to identify optimal growing environments:"),
            tags$dl(class = "row",
              tags$dt(class = "col-sm-3", "Outcome"),
              tags$dd(class = "col-sm-9",
                      tags$ul(class = "mb-0",
                        tags$li(tags$strong("Thriving"), " \u2014 Vigorous growth, healthy appearance, performing above expectations"),
                        tags$li(tags$strong("Established"), " \u2014 Healthy and stable, meeting expectations for the species"),
                        tags$li(tags$strong("Struggling"), " \u2014 Showing stress signs (yellowing, stunting, poor growth) but alive"),
                        tags$li(tags$strong("Failed/Died"), " \u2014 Plant did not survive or was removed due to poor performance")
                      )),
              tags$dt(class = "col-sm-3", "Sun Exposure"),
              tags$dd(class = "col-sm-9",
                      tags$ul(class = "mb-0",
                        tags$li(tags$strong("Full Sun"), " \u2014 6+ hours of direct sunlight per day"),
                        tags$li(tags$strong("Part Sun"), " \u2014 4-6 hours of direct sunlight, usually morning sun"),
                        tags$li(tags$strong("Part Shade"), " \u2014 2-4 hours of direct sunlight, or filtered light throughout day"),
                        tags$li(tags$strong("Full Shade"), " \u2014 Less than 2 hours of direct sunlight, mostly indirect light")
                      )),
              tags$dt(class = "col-sm-3", "Site Hydrology"),
              tags$dd(class = "col-sm-9",
                      tags$ul(class = "mb-0",
                        tags$li(tags$strong("Dry/Xeric"), " \u2014 Well-drained soil that dries quickly; rarely wet"),
                        tags$li(tags$strong("Mesic"), " \u2014 Average moisture; soil stays evenly moist but not wet"),
                        tags$li(tags$strong("Wet/Hydric"), " \u2014 Frequently saturated; may have standing water seasonally")
                      ))
            ),

            div(class = "mt-3 p-3 bg-light rounded border-start border-primary border-3",
              tags$strong(icon("lightbulb"), " Note on Annuals and Biennials"),
              p(class = "text-muted mb-2 mt-2",
                "For annuals and biennials that naturally complete their lifecycle (flowering, setting seed, then dying), ",
                "do ", tags$strong("not"), " record these as 'Failed/Died'\u2014this is normal, expected behavior."),
              p(class = "text-muted mb-0",
                "Instead, assess the outcome based on how well the plant performed ", tags$em("during"), " its lifecycle: ",
                "Did it thrive, establish normally, or struggle before completing its cycle? ",
                "If you have historical data on how the species fared in your landscape, use that to guide your rating. ",
                "Alternatively, you may choose not to submit data for completed-lifecycle plants if you're unsure how to rate them.")
            ),

            h4(class = "mt-4", "Analysis Charts Explained"),
            tags$dl(class = "row",
              tags$dt(class = "col-sm-3", "pH Distribution"),
              tags$dd(class = "col-sm-9", "Histogram showing the range of pH values for a species. ",
                      "The green shaded area shows the USDA reference pH range when available."),
              tags$dt(class = "col-sm-3", "pH vs Organic Matter"),
              tags$dd(class = "col-sm-9", "Scatter plot exploring the relationship between soil acidity and organic content. ",
                      "Points are colored by outcome when available, otherwise by texture class."),
              tags$dt(class = "col-sm-3", "Performance Tab"),
              tags$dd(class = "col-sm-9", "Shows outcome distributions and success rates by growing conditions. ",
                      "The Success Matrix reveals which sun/hydrology combinations yield best results."),
              tags$dt(class = "col-sm-3", "Correlations"),
              tags$dd(class = "col-sm-9", "Heatmap showing statistical correlations between soil parameters. ",
                      "Red = positive correlation, Blue = negative correlation.")
            ),

            h4(class = "mt-4", "External Resources"),
            tags$ul(
              tags$li(tags$a(href = "https://plants.usda.gov/", target = "_blank", "USDA PLANTS Database"),
                      " - Comprehensive plant information including characteristics and distributions"),
              tags$li(tags$a(href = "https://soilhealthlab.cals.cornell.edu/", target = "_blank", "Cornell Soil Health Lab"),
                      " - Comprehensive soil health assessment and testing"),
              tags$li(tags$a(href = "https://ohioline.osu.edu/factsheet/agf-0514", target = "_blank", "Interpreting Soil Test Reports (Ohio State)"),
                      " - Guide to understanding lab results"),
              tags$li(tags$a(href = "https://websoilsurvey.nrcs.usda.gov/", target = "_blank", "USDA Web Soil Survey"),
                      " - Detailed soil information by location"),
              tags$li(tags$a(href = "https://powo.science.kew.org/", target = "_blank", "Plants of the World Online (Kew)"),
                      " - Authoritative plant taxonomy and nomenclature"),
              tags$li(tags$a(href = "https://www.inaturalist.org/", target = "_blank", "iNaturalist"),
                      " - Citizen science platform for species observations")
            ),

            div(class = "mt-4 p-3 bg-light rounded",
                tags$small(class = "text-muted",
                           icon("book"), " ",
                           tags$strong("References: "),
                           "Nutrient guidelines adapted from university extension publications. ",
                           "Species data from WCVP (Royal Botanic Gardens, Kew) and USDA PLANTS Database."))
          )
        )
      )
    ),

    # FAQ
    nav_panel(
      title = "FAQ",
      icon = icon("circle-question"),
      layout_columns(
        col_widths = 12,
        card(
          card_header(icon("circle-question"), "Frequently Asked Questions"),
          card_body(
            class = "px-md-5",

            h5("Where should my soil test come from?"),
            p(class = "text-muted mb-4",
              "Use results from a professional soil testing lab (",
              tags$a(href = "https://www.nifa.usda.gov/grants/programs/cooperative-extension-system", target = "_blank", "university extension services"),
              ", commercial labs) or a reliable home soil test kit. For best results, collect samples from the root zone of the plants you're recording. ",
              "See the ", tags$a(href = "https://soilhealthlab.cals.cornell.edu/", target = "_blank", "Cornell Soil Health Lab"),
              " for detailed sampling guidance."),

            h5("Can I submit data for plants that died or failed?"),
            p(class = "text-muted mb-4",
              "Yes! Data from unsuccessful plantings is valuable\u2014it helps identify soil conditions that certain species ",
              "struggle with. Use the Outcome dropdown (Thriving, Established, Struggling, or Failed/Died) for each species."),

            h5("What if I don't have all the soil test values?"),
            p(class = "text-muted mb-4",
              "Submit what you have! Leave fields blank if you don't have that data\u2014they'll be excluded from analysis ",
              "rather than treated as zeros. pH and organic matter are the most valuable parameters to include."),

            h5("How does the soil report upload work?"),
            p(class = "text-muted mb-4",
              "You can upload soil test reports (PDF, RTF, TXT, or image files) and the app will automatically extract ",
              "values to pre-fill the form. This uses AI to read your report - always review the extracted values before submitting. ",
              tags$strong("Tip for multi-sample reports:"), " If your report contains multiple samples, use your computer's ",
              "screenshot/snipping tool to crop just the sample you want, then upload the cropped image."),

            h5("How are ecoregions determined?"),
            p(class = "text-muted mb-4",
              "When you enter coordinates, the app automatically identifies the ",
              tags$a(href = "https://www.epa.gov/eco-research/ecoregions", target = "_blank", "EPA Level IV ecoregion"),
              ". This provides ecological context\u2014plants in the same ecoregion often face similar soil, climate, and vegetation conditions."),

            h5("What is USDA reference data?"),
            p(class = "text-muted mb-4",
              "The ", tags$a(href = "https://plants.usda.gov/", target = "_blank", "USDA PLANTS Database"),
              " provides characteristics for many species including preferred pH range, drought tolerance, and shade tolerance. ",
              "When available, this reference data is shown alongside your samples for comparison."),

            h5("Can I edit or delete my submissions?"),
            p(class = "text-muted mb-4",
              "Yes! In the Data Entry tab, you'll see edit and delete buttons next to your own entries in the Recent Entries table. ",
              "Click edit to modify the entry in a popup form, or delete to remove it (with confirmation)."),

            h5("How is my data used?"),
            p(class = "text-muted mb-4",
              "All submitted data is shared under a CC BY-NC 4.0 license. It's freely available for non-commercial use ",
              "by researchers, gardeners, and educators. Your email is stored for attribution but not publicly displayed."),

            h5("What species can I enter?"),
            p(class = "text-muted mb-4",
              "The species search uses the ",
              tags$a(href = "https://powo.science.kew.org/", target = "_blank", "World Checklist of Vascular Plants (WCVP)"),
              ", which includes over 360,000 accepted plant species. If you can't find a species, try the scientific name without the author citation."),

            h5("What do the Outcome options mean?"),
            p(class = "text-muted mb-4",
              tags$strong("Thriving"), " \u2014 Vigorous growth, flowering/fruiting well, spreading or self-seeding. ",
              tags$strong("Established"), " \u2014 Healthy and stable, growing as expected. ",
              tags$strong("Struggling"), " \u2014 Alive but showing stress (yellowing, poor growth, pest issues). ",
              tags$strong("Failed/Died"), " \u2014 Plant died or was removed due to poor performance. ",
              "Recording unsuccessful plantings is just as valuable as successes!"),

            h5("How should I record annuals or biennials that completed their lifecycle?"),
            p(class = "text-muted mb-4",
              "For annuals and biennials that naturally die after flowering and setting seed, rate them based on how well they performed ",
              tags$em("during"), " their lifecycle\u2014not based on the fact that they died afterward. A sunflower that grew vigorously, ",
              "bloomed beautifully, and set abundant seed should be rated 'Thriving' or 'Established', not 'Failed/Died'. ",
              "Reserve 'Failed/Died' for plants that died prematurely or never established properly. ",
              "If you're unsure how to rate a completed-lifecycle plant, consider using historical data on how that species typically ",
              "performs in your landscape, or simply choose not to submit data for that particular planting."),

            h5("What does the Performance tab show?"),
            p(class = "text-muted mb-4",
              "The Performance tab analyzes plant success patterns. It shows outcome distributions, identifies which sun exposure ",
              "and hydrology conditions have the best success rates, and lets you compare soil parameters (pH, nutrients, etc.) ",
              "between thriving and struggling plants. The Key Insights panel summarizes the best conditions for each species."),

            h5("Why should I prioritize native plants?"),
            div(class = "text-muted mb-4",
              p("Native plants offer significant ecological and practical benefits:"),
              tags$ul(
                tags$li(tags$strong("Ecosystem support"), " \u2014 Native plants provide food and habitat for local pollinators, birds, and wildlife that co-evolved with them"),
                tags$li(tags$strong("Lower maintenance"), " \u2014 Once established, natives are adapted to local conditions and typically require less water, fertilizer, and pest control"),
                tags$li(tags$strong("Soil health"), " \u2014 Native species support beneficial soil microorganisms and fungi networks"),
                tags$li(tags$strong("Resilience"), " \u2014 They're better adapted to local climate extremes, pests, and diseases")
              ),
              p(class = "mt-2", tags$strong("Please avoid planting invasive species. "),
                "Some plants recommended based on soil chemistry may be invasive in your region. Invasive species can ",
                "escape cultivation, displace native plants, and harm local ecosystems. Before planting any species, ",
                "check resources like the ", tags$a(href = "https://www.invasivespeciesinfo.gov/", target = "_blank", "USDA Invasive Species Information Center"),
                " or your state's invasive species list.")
            ),

            h5("Found a bug or have a suggestion?"),
            p(class = "text-muted mb-4",
              "We welcome feedback! If you encounter any issues, have feature requests, or want to contribute, please ",
              tags$a(href = "https://github.com/todd-testerman/EdaphicFlora/issues", target = "_blank", "open an issue on GitHub"),
              ". You can also reach us at ",
              tags$a(href = "mailto:edaphicflora@gmail.com", "edaphicflora@gmail.com"), "."),

            hr(class = "my-4"),
            div(class = "text-center text-muted small",
              tags$a(href = "https://github.com/todd-testerman/EdaphicFlora/blob/main/PRIVACY.md", target = "_blank", "Privacy Policy"),
              span(class = "mx-2", "|"),
              tags$a(href = "https://github.com/todd-testerman/EdaphicFlora/blob/main/TERMS.md", target = "_blank", "Terms of Service"),
              span(class = "mx-2", "|"),
              tags$a(href = "https://github.com/todd-testerman/EdaphicFlora/issues", target = "_blank", "Report Issue"),
              span(class = "mx-2", "|"),
              tags$a(href = "https://github.com/todd-testerman/EdaphicFlora", target = "_blank", "GitHub"),
              span(class = "mx-2", "|"),
              tags$a(href = "https://buymeacoffee.com/toddtesterman", target = "_blank", "Support")
            )
          )
        )
      )
    )
  )
}

# ---------------------------
# Server (no logic needed for static content)
# ---------------------------

helpServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed - purely static content
  })
}
