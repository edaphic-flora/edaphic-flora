# R/theme.R - Custom theme and color palette for Edaphic Flora

library(ggplot2)

# ---------------------------
# Color Palette
# ---------------------------

# Brand palette from Edaphic Garden Consulting
# Sage: #7A9A86, Charcoal: #373D3C, Limestone: #F7F4E8
edaphic_colors <- list(

# Primary colors (brand)
  primary   = "#373D3C",
  secondary = "#7A9A86",
  accent    = "#7A9A86",

# UI colors
  success = "#7A9A86",
  warning = "#D4A574",
  danger  = "#C17C74",
  info    = "#6B8E9F",

# Neutrals
  light = "#F7F4E8",
  dark  = "#373D3C",
  muted = "#8B9A8E",

# Soil gradient (for continuous scales)
  soil = c("#DEB887", "#D2691E", "#A0522D", "#8B4513", "#654321"),

# Categorical palette (for discrete scales, brand-harmonious)
# 12 colors to support all nutrient types in charts
  categorical = c(
    "#7A9A86",
    "#373D3C",
    "#6B8E9F",
    "#A67B5B",
    "#8B9A8E",
    "#D4A574",
    "#5D7A6A",
    "#9B8B7A",
    "#4A5D5A",
    "#C4B7A6",
    "#7B6B5A",
    "#5A7A8B"
  )
)

# ---------------------------
# ggplot2 Theme
# ---------------------------
theme_edaphic <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Plot title and subtitle
      plot.title = element_text(
        size = rel(1.3),
        face = "bold",
        color = edaphic_colors$dark,
        hjust = 0,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = rel(1),
        color = edaphic_colors$muted,
        hjust = 0,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        size = rel(0.8),
        color = edaphic_colors$muted,
        hjust = 1
      ),

      # Axis
      axis.title = element_text(
        size = rel(1),
        face = "bold",
        color = edaphic_colors$dark
      ),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(
        size = rel(0.9),
        color = edaphic_colors$dark
      ),

      # Legend
      legend.title = element_text(
        face = "bold",
        color = edaphic_colors$dark,
        size = rel(0.95)
      ),
      legend.text = element_text(
        color = edaphic_colors$dark,
        size = rel(0.85)
      ),
      legend.position = "bottom",
      legend.box = "horizontal",

      # Panel
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),

      # Facets
      strip.text = element_text(
        face = "bold",
        color = edaphic_colors$dark,
        size = rel(1)
      ),
      strip.background = element_rect(fill = edaphic_colors$light, color = NA),

      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ---------------------------
# Color Scales
# ---------------------------

scale_color_edaphic <- function(palette = "categorical", ...) {
  if (palette == "categorical") {
    scale_color_manual(values = edaphic_colors$categorical, ...)
  } else if (palette == "soil") {
    scale_color_gradientn(colors = edaphic_colors$soil, ...)
  } else {
    scale_color_manual(values = edaphic_colors$categorical, ...)
  }
}

scale_fill_edaphic <- function(palette = "categorical", ...) {
  if (palette == "categorical") {
    scale_fill_manual(values = edaphic_colors$categorical, ...)
  } else if (palette == "soil") {
    scale_fill_gradientn(colors = edaphic_colors$soil, ...)
  } else {
    scale_fill_manual(values = edaphic_colors$categorical, ...)
  }
}

# ---------------------------
# bslib Theme
# ---------------------------

edaphic_bs_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = edaphic_colors$accent,
    secondary = edaphic_colors$secondary,
    success = edaphic_colors$success,
    info = edaphic_colors$info,
    warning = edaphic_colors$warning,
    danger = edaphic_colors$danger,
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Quicksand"),
    "navbar-bg" = edaphic_colors$primary,
    "card-border-radius" = "0.5rem"
  )
}

# ---------------------------
# Custom CSS
# ---------------------------

edaphic_css <- function() {
  tags$style(HTML("
    /* Card styling */
    .card {
      transition: box-shadow 0.2s ease;
      border: 1px solid #e0e0e0;
    }
    .card:hover {
      box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    }

    /* Sidebar styling */
    .sidebar {
      background-color: #f8f9fa;
    }

    /* Accordion styling */
    .accordion-button:not(.collapsed) {
      background-color: rgba(39, 174, 96, 0.1);
      color: #7A9A86;
    }
    .accordion-button:focus {
      box-shadow: 0 0 0 0.2rem rgba(39, 174, 96, 0.25);
    }

    /* Form styling */
    .form-label {
      font-weight: 600;
      color: #373D3C;
      margin-bottom: 0.3rem;
    }
    .form-control:focus, .form-select:focus {
      border-color: #7A9A86;
      box-shadow: 0 0 0 0.2rem rgba(39, 174, 96, 0.25);
    }

    /* Button styling */
    .btn-primary {
      background-color: #7A9A86;
      border-color: #7A9A86;
    }
    .btn-primary:hover {
      background-color: #6A8A76;
      border-color: #6A8A76;
    }

    /* Empty state */
    .empty-state {
      padding: 3rem 1rem;
      text-align: center;
      color: #6c757d;
    }
    .empty-state i {
      font-size: 3rem;
      margin-bottom: 1rem;
      opacity: 0.5;
    }

    /* Analysis tabs */
    .nav-tabs .nav-link.active {
      border-bottom: 3px solid #7A9A86;
      font-weight: 600;
    }
    .nav-tabs .nav-link {
      padding: 0.5rem 0.75rem;
      font-size: 0.875rem;
    }
    .navset-card-tab .nav-tabs {
      flex-wrap: nowrap;
      overflow-x: auto;
    }

    /* Navbar user info */
    .navbar-text {
      color: rgba(255,255,255,0.8) !important;
    }

    /* Species search highlight */
    .selectize-dropdown-content .active {
      background-color: rgba(39, 174, 96, 0.15);
    }

    /* Brand name styling - uses Quicksand to match logo */
    .brand-name {
      font-family: 'Quicksand', sans-serif;
      letter-spacing: 1px;
      text-transform: lowercase;
    }
    .brand-name-edaphic {
      font-weight: 600;
      color: #7A9A86;
    }
    .brand-name-flora {
      font-weight: 400;
      color: #373D3C;
    }
    /* Navbar brand text - both white on dark navbar */
    .navbar .brand-name-edaphic {
      color: rgba(255,255,255,0.9);
    }
    .navbar .brand-name-flora {
      color: rgba(255,255,255,0.9);
    }

    /* Welcome page brand styling */
    .welcome-brand {
      font-family: 'Quicksand', sans-serif;
      font-size: 2.5rem;
      letter-spacing: 2px;
      text-transform: lowercase;
    }
    .welcome-brand .brand-name-edaphic {
      font-weight: 600;
      color: #7A9A86;
    }
    .welcome-brand .brand-name-flora {
      font-weight: 400;
      color: #7A9A86;
    }
  "))
}
