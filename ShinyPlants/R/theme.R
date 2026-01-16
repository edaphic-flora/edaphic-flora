# R/theme.R - Custom theme and color palette for Edaphic Flora

library(ggplot2)

# ---------------------------
# Color Palette
# ---------------------------

# Earth-toned palette appropriate for soil/plant application
edaphic_colors <- list(

# Primary colors
  primary   = "#2c3e50",
  secondary = "#8B4513",
  accent    = "#27ae60",

# UI colors
  success = "#27ae60",
  warning = "#f39c12",
  danger  = "#e74c3c",
  info    = "#3498db",

# Neutrals
  light = "#ecf0f1",
  dark  = "#34495e",
  muted = "#95a5a6",

# Soil gradient (for continuous scales)
  soil = c("#DEB887", "#D2691E", "#A0522D", "#8B4513", "#654321"),

# Categorical palette (for discrete scales)
  categorical = c(
    "#2c3e50",
    "#27ae60",
    "#3498db",
    "#9b59b6",
    "#e74c3c",
    "#f39c12",
    "#1abc9c",
    "#e67e22",
    "#34495e",
    "#16a085"
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
    heading_font = bslib::font_google("Inter"),
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
      color: #27ae60;
    }
    .accordion-button:focus {
      box-shadow: 0 0 0 0.2rem rgba(39, 174, 96, 0.25);
    }

    /* Form styling */
    .form-label {
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 0.3rem;
    }
    .form-control:focus, .form-select:focus {
      border-color: #27ae60;
      box-shadow: 0 0 0 0.2rem rgba(39, 174, 96, 0.25);
    }

    /* Button styling */
    .btn-primary {
      background-color: #27ae60;
      border-color: #27ae60;
    }
    .btn-primary:hover {
      background-color: #219a52;
      border-color: #219a52;
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
      border-bottom: 3px solid #27ae60;
      font-weight: 600;
    }

    /* Navbar user info */
    .navbar-text {
      color: rgba(255,255,255,0.8) !important;
    }

    /* Species search highlight */
    .selectize-dropdown-content .active {
      background-color: rgba(39, 174, 96, 0.15);
    }
  "))
}
