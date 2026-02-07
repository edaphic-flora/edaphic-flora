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

            h4("How to Get a Soil Test", id = "guide-soil-testing"),
            p("Edaphic Flora requires data from a certified soil testing laboratory. Lab tests use precise chemical ",
              "extractions and calibrated instruments that home test kits and DIY probes simply cannot match. ",
              "Without lab-grade accuracy, comparisons across sites and contributors are unreliable. ",
              "Most state cooperative extension offices offer free or low-cost soil testing."),

            tags$ol(
              tags$li(
                tags$strong("Find a lab"),
                p(class = "mb-1",
                  "Your best option is usually your state\u2019s ",
                  tags$a(href = "https://www.nifa.usda.gov/grants/programs/cooperative-extension-system",
                         target = "_blank", "cooperative extension service"),
                  ", which offers subsidized testing tailored to your region. Commercial labs such as ",
                  tags$a(href = "https://loganlabs.com/", target = "_blank", "Logan Labs"),
                  " and the ",
                  tags$a(href = "https://soilhealthlab.cals.cornell.edu/", target = "_blank", "Cornell Soil Health Lab"),
                  " are also excellent choices.")
              ),
              tags$li(
                tags$strong("Collect your sample"),
                p(class = "mb-1",
                  "Take 6\u201310 sub-samples from the area around your plant(s), each from the top 6\u20138 inches of soil. ",
                  "Mix them thoroughly in a clean bucket to create one composite sample. Avoid sampling right after ",
                  "fertilizing or heavy rain.")
              ),
              tags$li(
                tags$strong("Prepare and ship"),
                p(class = "mb-1",
                  "Air-dry the composite sample (spread it on newspaper for 24\u201348 hours), then bag about 2 cups ",
                  "and mail it to the lab with their submission form. Most labs return results within 1\u20132 weeks.")
              ),
              tags$li(
                tags$strong("Record your results"),
                p(class = "mb-1",
                  "Upload your lab report PDF in the Data Entry tab, or enter the values manually. ",
                  "The app will extract pH, nutrients, organic matter, and texture data automatically from most lab formats.")
              )
            ),

            div(class = "p-3 bg-light rounded border-start border-3",
                style = "border-color: #7A9A86 !important;",
                tags$strong(icon("building-columns"), " Find a Lab Near You"),
                p(class = "text-muted mb-0 mt-1",
                  "The USDA Cooperative Extension System has offices in nearly every county. ",
                  tags$a(href = "https://www.nifa.usda.gov/land-grant-colleges-and-universities-partner-website-directory",
                         target = "_blank", "Find your state\u2019s extension service \u2192"),
                  " They can recommend labs, help interpret results, and provide local growing advice.")
            ),

            hr(),

            h4("Soil Properties", id = "guide-soil-properties"),
            tags$dl(class = "row",
              tags$dt(class = "col-sm-3", "Soil pH"),
              tags$dd(class = "col-sm-9", "Measures acidity/alkalinity on a scale of 0-14. Most plants prefer 6.0-7.0. ",
                      "Values below 7 are acidic; above 7 are alkaline."),
              tags$dt(class = "col-sm-3", "Organic Matter (%)"),
              tags$dd(class = "col-sm-9", "Decomposed plant/animal material. Higher values (3-5%) indicate fertile soil ",
                      "with good water retention and microbial activity.")
            ),

            h4(class = "mt-4", "Nutrients (ppm)", id = "guide-nutrients"),
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

            h4(class = "mt-4", "Plant Performance", id = "guide-plant-performance"),
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
              "Use results from a certified soil testing laboratory (",
              tags$a(href = "https://www.nifa.usda.gov/grants/programs/cooperative-extension-system", target = "_blank", "university extension services"),
              ", commercial labs like Logan Labs or Cornell). Home test kits and DIY probes lack the accuracy needed for ",
              "meaningful cross-site comparisons. For best results, collect samples from the root zone of the plants you're recording. ",
              "See the Field Guide section above for step-by-step soil sampling guidance."),

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
              "We welcome feedback! If you encounter any issues or have feature requests, please reach out at ",
              tags$a(href = "mailto:edaphicflora@gmail.com", "edaphicflora@gmail.com"), "."),

            hr(class = "my-4"),
            div(class = "text-center text-muted small",
              tags$a(href = "mailto:edaphicflora@gmail.com?subject=Edaphic%20Flora%20Feedback", "Send Feedback"),
              span(class = "mx-2", "|"),
              tags$a(href = "https://buymeacoffee.com/toddtesterman", target = "_blank", "Support")
            )
          )
        )
      )
    ),

    # Caveats & Limitations
    nav_panel(
      title = "Caveats & Limitations",
      icon = icon("triangle-exclamation"),
      layout_columns(
        col_widths = 12,
        card(
          card_header(icon("triangle-exclamation"), "Caveats & Limitations"),
          card_body(
            class = "px-md-5",

            p(class = "lead",
              "Edaphic Flora is a tool for exploration, not prescription. It reveals patterns in real-world soil data, ",
              "but those patterns come with important limitations you should understand before acting on them."),

            h4(class = "mt-4", "Soil Is One Factor Among Many"),
            p("A plant\u2019s success depends on far more than soil chemistry. Microclimate, drainage, root competition from ",
              "neighboring plants, maintenance history, pest and disease pressure, mycorrhizal associations, and dozens of ",
              "other factors all play a role. Two sites with identical soil test results can produce very different outcomes ",
              "for the same species. Soil data is a valuable piece of the puzzle, but never the whole picture."),

            h4(class = "mt-4", "Sample Size Limitations"),
            p("Many species in the database have only a handful of soil samples. With small sample sizes, a single unusual ",
              "observation can skew averages and ranges significantly. Patterns that appear strong with 3\u20135 samples may ",
              "disappear or reverse with 50. The app flags species with limited data as \u201cEarly Access\u201d and requires ",
              "minimum sample counts before displaying public statistics, but even these thresholds are conservative. ",
              "Treat small-sample results as preliminary observations, not established ranges."),

            h4(class = "mt-4", "Geographic and Contributor Bias"),
            p("Crowdsourced data reflects where contributors live and garden. If most samples for a species come from one ",
              "region, the data will reflect that region\u2019s soil conditions\u2014not the full range of soils where the species ",
              "can thrive. A species showing a narrow pH range in the database may simply mean that most contributors ",
              "garden in similar soils, not that the species has narrow pH tolerance. As the dataset grows and diversifies, ",
              "these biases will diminish."),

            h4(class = "mt-4", "Temporal Variation"),
            p("Soil chemistry is not static. pH, nutrient levels, and organic matter fluctuate with the seasons, ",
              "rainfall patterns, amendment history, and even microbial activity. A soil test captures a snapshot in time. ",
              "A sample taken in spring after snowmelt may look very different from one taken in late summer. ",
              "When comparing samples, keep in mind that timing matters."),

            h4(class = "mt-4", "Correlation Is Not Causation"),
            p("If thriving specimens of a species tend to appear in high-calcium soils, that does not mean calcium is driving ",
              "success. High-calcium soils are often alkaline, well-drained limestone-derived soils with their own suite of ",
              "characteristics. The species may be responding to pH, drainage, or soil structure rather than calcium itself. ",
              "The app shows correlations in the data\u2014interpreting the underlying mechanisms requires agronomic knowledge ",
              "and, ideally, controlled experiments."),

            h4(class = "mt-4", "Responsible Use"),
            div(class = "p-3 bg-light rounded border-start border-3",
                style = "border-color: #D39B35 !important;",
                tags$ul(class = "mb-0",
                  tags$li("This tool is ", tags$strong("not a substitute for professional advice"), ". For high-stakes ",
                          "decisions (restoration projects, commercial plantings, land management), consult a soil scientist, ",
                          "agronomist, or your local cooperative extension."),
                  tags$li(class = "mt-2",
                          tags$strong("Always check invasive species lists"), " before planting. A species that thrives in your soil ",
                          "conditions may be invasive in your state. The app shows invasive status badges where data is available, ",
                          "but coverage is not exhaustive."),
                  tags$li(class = "mt-2",
                          tags$strong("Prefer native species"), " whenever possible. Native plants support local ecosystems, ",
                          "pollinators, and wildlife in ways that non-natives cannot replicate.")
                )
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
