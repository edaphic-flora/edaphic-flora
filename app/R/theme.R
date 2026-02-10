# R/theme.R - Custom theme and color palette for Edaphic Flora
# UI/UX Refresh: "Botanical Field Journal" aesthetic

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
  warning = "#D39B35",
  danger  = "#C17C74",
  info    = "#6B8E9F",

# Brand accent
  gold = "#D39B35",  # Horizon Gold - for CTAs and highlights

# Neutrals
  light = "#F7F4E8",
  dark  = "#373D3C",
  muted = "#5F7268",

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
  ),

# Outcome colors for badges
  outcome_thriving = "#5D7A6A",
  outcome_established = "#7A9A86",
  outcome_struggling = "#B8956A",
  outcome_failed = "#A66A62"
)

# ---------------------------
# ggplot2 Theme
# ---------------------------
theme_edaphic <- function(base_size = 12, base_family = "Rokkitt") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Plot title and subtitle
      plot.title = element_text(
        family = "Montserrat",
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
      panel.background = element_rect(fill = "#F7F4E8", color = NA),
      plot.background = element_rect(fill = "#F7F4E8", color = NA),

      # Facets
      strip.text = element_text(
        family = "Montserrat",
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
    # Typography: Rokkitt (body), Montserrat (headings), JetBrains Mono (data)
    # Per brand guidelines: Montserrat = "The Manager", Rokkitt = "The Narrative"
    base_font = bslib::font_google("Rokkitt", wght = "300..700"),
    heading_font = bslib::font_google("Montserrat", wght = "400..700"),
    code_font = bslib::font_google("JetBrains Mono"),
    "navbar-bg" = edaphic_colors$primary,
    "card-border-radius" = "12px",
    "border-radius" = "8px",
    "btn-border-radius" = "8px"
  )
}

# ---------------------------
# Custom CSS
# ---------------------------

edaphic_css <- function() {
  tags$style(HTML("
    /* ===========================================
       1. TYPOGRAPHY
       =========================================== */

    /* Import fonts - Brand Standard: Baumans (logo), Montserrat (headings), Rokkitt (body), JetBrains Mono (data) */
    @import url('https://fonts.googleapis.com/css2?family=Baumans&family=Montserrat:wght@400;500;600;700&family=Rokkitt:wght@300;400;500;600&family=JetBrains+Mono:ital,wght@0,400;0,500;1,400&display=swap');

    body {
      font-family: 'Rokkitt', Georgia, serif;
      font-weight: 300;
      font-size: 1.05rem;
      color: #373D3C;
      line-height: 1.65;
    }

    h1, h2, h3, h4, h5, h6,
    .h1, .h2, .h3, .h4, .h5, .h6 {
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      color: #373D3C;
    }

    /* Species names - italic monospace for scientific feel */
    .species-name,
    .dataTable tbody td:nth-child(2) {
      font-family: 'JetBrains Mono', monospace;
      font-style: italic;
      font-size: 0.95em;
      letter-spacing: -0.02em;
    }

    /* Data values - monospace for alignment */
    .data-value,
    .dataTable tbody td:nth-child(4),
    .dataTable tbody td:nth-child(5),
    .dataTable tbody td:nth-child(6) {
      font-family: 'JetBrains Mono', monospace;
      font-size: 0.9em;
    }

    /* ===========================================
       2. CARDS & CONTAINERS
       =========================================== */

    .card {
      border: none;
      border-radius: 12px;
      box-shadow: 0 1px 3px rgba(55, 61, 60, 0.08);
      overflow: hidden;
      transition: box-shadow 0.25s ease, transform 0.25s ease;
      background: #F7F4E8;
    }

    .card:hover {
      box-shadow: 0 8px 24px rgba(55, 61, 60, 0.12);
      transform: translateY(-2px);
    }

    .card-header {
      background: linear-gradient(135deg, #F7F4E8 0%, #f5f2e5 100%);
      border-bottom: none;
      border-left: 4px solid #7A9A86;
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      padding: 1rem 1.25rem;
    }

    .card-body {
      padding: 1.5rem;
    }

    /* Sidebar styling */
    .sidebar {
      background: linear-gradient(180deg, #f8f9fa 0%, #F7F4E8 100%);
      border-right: 1px solid rgba(122, 154, 134, 0.2);
    }

    /* ===========================================
       3. NAVBAR
       =========================================== */

    .navbar {
      border-bottom: 3px solid #7A9A86;
      box-shadow: 0 2px 8px rgba(55, 61, 60, 0.1);
    }

    .navbar .brand-name {
      font-family: 'Baumans', sans-serif;
      letter-spacing: 1px;
      text-transform: lowercase;
      transition: all 0.3s ease;
    }

    .navbar .brand-name:hover svg {
      filter: drop-shadow(0 0 8px rgba(122, 154, 134, 0.6));
    }

    .navbar .brand-name-edaphic,
    .navbar .brand-name-flora {
      font-family: 'Baumans', sans-serif;
      color: rgba(255,255,255,0.95);
    }

    .navbar-text {
      color: rgba(255,255,255,0.85) !important;
      font-family: 'Rokkitt', serif;
    }

    /* Navbar zip code area */
    .navbar .badge.bg-success {
      background: rgba(122, 154, 134, 0.9) !important;
      font-family: 'JetBrains Mono', monospace;
      font-size: 0.8rem;
      padding: 0.4em 0.8em;
      border-radius: 12px;
    }

    /* ===========================================
       4. ANALYSIS TAB NAVIGATION (Pill Style)
       =========================================== */

    /* Target analysis tabs specifically via wrapper class */
    .analysis-tabs-container .card-header,
    .analysis-tabs-container .bslib-card > .card-header {
      padding: 0.25rem !important;
      background: #f8f9fa !important;
    }

    .analysis-tabs-container .nav-tabs,
    .analysis-tabs-container .card-header-tabs,
    .analysis-tabs-container ul.nav.nav-tabs {
      border-bottom: none !important;
      padding: 0.2rem !important;
      background: #f8f9fa !important;
      border-radius: 10px 10px 0 0 !important;
      flex-wrap: nowrap !important;
      overflow-x: auto !important;
      gap: 2px !important;
      margin: 0 !important;
    }

    .analysis-tabs-container .nav-tabs .nav-item {
      flex-shrink: 0 !important;
    }

    .analysis-tabs-container .nav-tabs .nav-link,
    .analysis-tabs-container .nav-tabs > li > a,
    .analysis-tabs-container .card-header-tabs .nav-link {
      border: 1px solid transparent !important;
      border-radius: 10px !important;
      padding: 0.3rem 0.55rem !important;
      font-family: 'Montserrat', sans-serif !important;
      font-weight: 500 !important;
      font-size: 0.8rem !important;
      color: #373D3C !important;
      background: transparent !important;
      transition: all 0.2s ease !important;
      white-space: nowrap !important;
      margin: 0 !important;
    }

    .analysis-tabs-container .nav-tabs .nav-link:hover {
      background: rgba(122, 154, 134, 0.1) !important;
      border-color: rgba(122, 154, 134, 0.3) !important;
      color: #5D7A6A !important;
    }

    .analysis-tabs-container .nav-tabs .nav-link.active,
    .analysis-tabs-container .nav-tabs .nav-link[aria-selected='true'] {
      background: linear-gradient(135deg, #7A9A86 0%, #6A8A76 100%) !important;
      color: white !important;
      border-color: #7A9A86 !important;
      box-shadow: 0 2px 4px rgba(122, 154, 134, 0.3) !important;
    }

    .analysis-tabs-container .nav-tabs .nav-link i,
    .analysis-tabs-container .nav-tabs .nav-link .fa {
      margin-right: 0.15rem !important;
      opacity: 0.8 !important;
      font-size: 0.65rem !important;
    }

    .analysis-tabs-container .nav-tabs .nav-link.active i {
      opacity: 1 !important;
    }

    /* ===========================================
       5. FORMS & INPUTS
       =========================================== */

    .form-label {
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      color: #373D3C;
      margin-bottom: 0.4rem;
      font-size: 0.9rem;
    }

    .form-control, .form-select {
      border: 1px solid #d1d5db;
      border-radius: 8px;
      padding: 0.6rem 0.9rem;
      transition: all 0.2s ease;
      font-family: 'Rokkitt', serif;
    }

    .form-control:focus, .form-select:focus {
      border-color: #7A9A86;
      box-shadow: 0 0 0 3px rgba(122, 154, 134, 0.15);
      outline: none;
    }

    .form-control::placeholder {
      color: #9ca3af;
      font-style: italic;
    }

    /* Selectize styling */
    .selectize-input {
      border-radius: 8px !important;
      border: 1px solid #d1d5db !important;
      padding: 0.5rem 0.75rem !important;
      font-family: 'Rokkitt', serif !important;
    }

    .selectize-input.focus {
      border-color: #7A9A86 !important;
      box-shadow: 0 0 0 3px rgba(122, 154, 134, 0.15) !important;
    }

    .selectize-dropdown-content .active {
      background-color: rgba(122, 154, 134, 0.15);
      color: #373D3C;
    }

    .selectize-dropdown-content .option {
      font-family: 'JetBrains Mono', monospace;
      font-style: italic;
      font-size: 0.9em;
    }

    /* Accordion styling */
    .accordion-button {
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      padding: 0.875rem 1.25rem;
      background: #f8f9fa;
      border-radius: 8px !important;
    }

    .accordion-button:not(.collapsed) {
      background: linear-gradient(135deg, rgba(122, 154, 134, 0.1) 0%, rgba(122, 154, 134, 0.05) 100%);
      color: #5D7A6A;
      box-shadow: none;
    }

    .accordion-button:focus {
      box-shadow: 0 0 0 3px rgba(122, 154, 134, 0.2);
      border-color: #7A9A86;
    }

    .accordion-button::after {
      transition: transform 0.3s ease;
    }

    .accordion-item {
      border: 1px solid rgba(122, 154, 134, 0.2);
      border-radius: 8px !important;
      margin-bottom: 0.5rem;
      overflow: hidden;
    }

    /* Form section completion indicator */
    .accordion-button.section-complete::before {
      content: '\\2713';
      position: absolute;
      right: 3rem;
      color: #7A9A86;
      font-weight: bold;
      font-size: 1.1rem;
    }

    /* Form progress bar */
    .form-progress {
      height: 4px;
      background: #e5e7eb;
      border-radius: 2px;
      overflow: hidden;
      margin-bottom: 1rem;
    }

    .form-progress-fill {
      height: 100%;
      background: linear-gradient(90deg, #7A9A86 0%, #5D7A6A 100%);
      transition: width 0.4s ease;
      border-radius: 2px;
    }

    /* ===========================================
       6. BUTTONS
       =========================================== */

    .btn {
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      padding: 0.6rem 1.25rem;
      border-radius: 8px;
      transition: all 0.2s ease;
    }

    .btn-primary {
      background: linear-gradient(135deg, #7A9A86 0%, #6A8A76 100%);
      border: none;
      box-shadow: 0 2px 4px rgba(122, 154, 134, 0.3);
    }

    .btn-primary:hover {
      background: linear-gradient(135deg, #6A8A76 0%, #5D7A6A 100%);
      transform: translateY(-1px);
      box-shadow: 0 4px 8px rgba(122, 154, 134, 0.4);
    }

    .btn-primary:active {
      transform: scale(0.98) translateY(0);
      box-shadow: 0 1px 2px rgba(122, 154, 134, 0.3);
    }

    .btn-outline-primary {
      color: #7A9A86;
      border-color: #7A9A86;
    }

    .btn-outline-primary:hover {
      background: #7A9A86;
      border-color: #7A9A86;
      color: white;
    }

    .btn-outline-secondary {
      border-color: #d1d5db;
      color: #6b7280;
    }

    .btn-outline-secondary:hover {
      background: #f3f4f6;
      border-color: #9ca3af;
      color: #374151;
    }

    /* ===========================================
       7. TABLES (DataTables)
       =========================================== */

    .dataTables_wrapper {
      font-family: 'Rokkitt', serif;
    }

    .dataTable {
      border-collapse: separate;
      border-spacing: 0;
    }

    .dataTable thead th {
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      color: #373D3C;
      background: #F7F4E8;
      border-bottom: 2px solid #7A9A86;
      padding: 0.875rem 1rem;
      font-size: 0.85rem;
      text-transform: uppercase;
      letter-spacing: 0.03em;
    }

    .dataTable tbody tr {
      transition: background-color 0.15s ease;
    }

    .dataTable tbody tr:nth-child(even) {
      background-color: rgba(247, 244, 232, 0.4);
    }

    .dataTable tbody tr:hover {
      background-color: rgba(122, 154, 134, 0.08);
    }

    .dataTable tbody td {
      padding: 0.75rem 1rem;
      border-bottom: 1px solid #e5e7eb;
      vertical-align: middle;
    }

    /* Outcome badges */
    .outcome-badge {
      display: inline-block;
      padding: 0.3em 0.7em;
      border-radius: 12px;
      font-family: 'Montserrat', sans-serif;
      font-size: 0.75rem;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.02em;
    }

    .outcome-thriving {
      background: rgba(93, 122, 106, 0.15);
      color: #4A6358;
    }

    .outcome-established {
      background: rgba(122, 154, 134, 0.15);
      color: #5D7A6A;
    }

    .outcome-struggling {
      background: rgba(212, 165, 116, 0.2);
      color: #9A7A4A;
    }

    .outcome-failed {
      background: rgba(193, 124, 116, 0.2);
      color: #8B5A52;
    }

    /* Table action buttons */
    .dataTable .btn-sm {
      padding: 0.25rem 0.5rem;
      font-size: 0.8rem;
      border-radius: 6px;
    }

    /* Pagination styling */
    .dataTables_paginate .paginate_button {
      font-family: 'Montserrat', sans-serif;
      border-radius: 6px !important;
      margin: 0 2px;
    }

    .dataTables_paginate .paginate_button.current {
      background: #7A9A86 !important;
      border-color: #7A9A86 !important;
      color: white !important;
    }

    /* ===========================================
       8. WELCOME PAGE
       =========================================== */

    .welcome-hero {
      background: linear-gradient(135deg, rgba(247,244,232,0.97) 0%, rgba(247,244,232,0.92) 100%);
      border-bottom: 2px solid #7A9A86;
      position: relative;
      padding-top: 0.5rem !important;
      padding-bottom: 0.75rem !important;
    }

    /* Subtle grain texture overlay */
    .welcome-hero::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background-image: url(\"data:image/svg+xml,%3Csvg viewBox='0 0 200 200' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='noiseFilter'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.65' numOctaves='3' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23noiseFilter)'/%3E%3C/svg%3E\");
      opacity: 0.03;
      pointer-events: none;
    }

    /* Alpha banner - white background for contrast against limestone hero */
    .welcome-hero .alert-warning {
      background: white !important;
      border: 1px solid #D39B35 !important;
      border-left: 4px solid #D39B35 !important;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06);
      margin-bottom: 0.75rem !important;
      padding: 0.75rem !important;
    }

    .welcome-brand {
      font-family: 'Baumans', sans-serif;
      font-size: 2rem;
      letter-spacing: 2px;
      text-transform: lowercase;
      animation: fadeInUp 0.4s ease;
      margin-bottom: 0.25rem !important;
    }

    .welcome-brand .brand-name-edaphic {
      font-family: 'Baumans', sans-serif;
      color: #7A9A86;
    }

    .welcome-brand .brand-name-flora {
      font-family: 'Baumans', sans-serif;
      color: #5D7A6A;
    }

    /* Welcome page - compact layout */
    .welcome-hero + .card-body {
      padding-top: 0.75rem !important;
      padding-bottom: 0.75rem !important;
    }

    .welcome-hero + .card-body h4 {
      font-size: 1.1rem;
      margin-bottom: 0.4rem;
      margin-top: 0.75rem !important;
    }

    .welcome-hero + .card-body h4:first-child {
      margin-top: 0 !important;
    }

    .welcome-hero + .card-body p {
      font-size: 1rem;
      margin-bottom: 0.5rem;
    }

    /* Stats cards in sidebar */
    .stat-card {
      text-align: center;
      padding: 1rem 0.5rem;
      transition: transform 0.2s ease;
    }

    .stat-card:hover {
      transform: scale(1.02);
    }

    .stat-number {
      font-family: 'Montserrat', sans-serif;
      font-size: 2rem;
      font-weight: 700;
      background: linear-gradient(135deg, #7A9A86 0%, #5D7A6A 100%);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
      line-height: 1.2;
    }

    .stat-label {
      font-family: 'Montserrat', sans-serif;
      font-size: 0.7rem;
      color: #5F7268;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      margin-top: 0.25rem;
    }

    /* ===========================================
       9. EMPTY STATES
       =========================================== */

    .empty-state {
      padding: 3rem 2rem;
      text-align: center;
      color: #5F7268;
      animation: fadeIn 0.4s ease;
    }

    .empty-state i {
      font-size: 3.5rem;
      margin-bottom: 1.25rem;
      opacity: 0.4;
      color: #7A9A86;
    }

    .empty-state h5 {
      font-family: 'Montserrat', sans-serif;
      color: #373D3C;
      margin-bottom: 0.5rem;
    }

    .empty-state p {
      font-family: 'Rokkitt', serif;
      font-size: 0.95rem;
      max-width: 320px;
      margin: 0 auto;
    }

    /* ===========================================
       10. ALERTS & NOTIFICATIONS
       =========================================== */

    .alert {
      border: none;
      border-radius: 10px;
      border-left: 4px solid;
      font-family: 'Rokkitt', serif;
    }

    .alert-info {
      background: rgba(107, 142, 159, 0.1);
      border-left-color: #6B8E9F;
      color: #4A6B7A;
    }

    .alert-warning {
      background: rgba(212, 165, 116, 0.1);
      border-left-color: #D4A574;
      color: #8B6B3A;
    }

    .alert-success {
      background: rgba(122, 154, 134, 0.1);
      border-left-color: #7A9A86;
      color: #4A6B5A;
    }

    .alert-danger {
      background: rgba(193, 124, 116, 0.1);
      border-left-color: #C17C74;
      color: #8B4A42;
    }

    /* Shiny notifications */
    .shiny-notification {
      animation: slideInRight 0.3s ease;
      border-radius: 10px;
      border-left: 4px solid #7A9A86;
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      font-family: 'Rokkitt', serif;
    }

    @keyframes slideInRight {
      from {
        transform: translateX(100%);
        opacity: 0;
      }
      to {
        transform: translateX(0);
        opacity: 1;
      }
    }

    /* ===========================================
       11. BADGES
       =========================================== */

    .badge {
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      padding: 0.4em 0.75em;
      border-radius: 12px;
    }

    .badge.bg-info {
      background: rgba(107, 142, 159, 0.9) !important;
    }

    .badge.bg-warning {
      background: rgba(212, 165, 116, 0.9) !important;
      color: #373D3C !important;
    }

    .badge.bg-light {
      background: #F7F4E8 !important;
      color: #373D3C !important;
      border: 1px solid #e5e7eb;
    }

    /* Reference badges (USDA data) */
    .reference-section {
      background: linear-gradient(135deg, #f8f9fa 0%, #F7F4E8 100%);
      border: 1px solid rgba(122, 154, 134, 0.2);
    }

    .reference-section .badge {
      font-family: 'JetBrains Mono', monospace;
      font-size: 0.7rem;
      font-weight: 500;
    }

    /* ===========================================
       12. LOADING STATES
       =========================================== */

    .spinner-border {
      color: #7A9A86;
    }

    /* Shinycssloaders spinner override */
    .shiny-spinner-output-container {
      position: relative;
    }

    .shiny-spinner-placeholder {
      color: #7A9A86;
    }

    /* ===========================================
       13. ANIMATIONS
       =========================================== */

    @keyframes fadeIn {
      from { opacity: 0; }
      to { opacity: 1; }
    }

    @keyframes fadeInUp {
      from {
        opacity: 0;
        transform: translateY(20px);
      }
      to {
        opacity: 1;
        transform: translateY(0);
      }
    }

    /* Staggered reveal for lists */
    .stagger-reveal > * {
      animation: fadeInUp 0.4s ease backwards;
    }
    .stagger-reveal > *:nth-child(1) { animation-delay: 0.1s; }
    .stagger-reveal > *:nth-child(2) { animation-delay: 0.2s; }
    .stagger-reveal > *:nth-child(3) { animation-delay: 0.3s; }
    .stagger-reveal > *:nth-child(4) { animation-delay: 0.4s; }
    .stagger-reveal > *:nth-child(5) { animation-delay: 0.5s; }

    /* ===========================================
       14. MISCELLANEOUS
       =========================================== */

    /* Help text styling */
    .text-muted {
      color: #5F7268 !important;
    }

    small.text-muted, .small.text-muted {
      font-size: 0.85rem;
    }

    /* Links */
    a {
      color: #7A9A86;
      text-decoration: none;
      transition: color 0.2s ease;
    }

    a:hover {
      color: #5D7A6A;
      text-decoration: underline;
    }

    /* Action links */
    .action-link, .text-success {
      color: #7A9A86 !important;
    }

    .action-link:hover, .text-success:hover {
      color: #5D7A6A !important;
    }

    /* Modal styling */
    .modal-content {
      border: none;
      border-radius: 12px;
      box-shadow: 0 20px 60px rgba(0,0,0,0.2);
    }

    .modal-header {
      background: #F7F4E8;
      border-bottom: none;
      border-radius: 12px 12px 0 0;
      padding: 1.25rem 1.5rem;
    }

    .modal-title {
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
    }

    .modal-body {
      padding: 1.5rem;
    }

    .modal-footer {
      border-top: 1px solid #e5e7eb;
      padding: 1rem 1.5rem;
    }

    /* Horizontal rule */
    hr {
      border-color: rgba(122, 154, 134, 0.2);
      opacity: 1;
    }

    /* Details/Summary (filter sections) */
    details summary {
      font-family: 'Montserrat', sans-serif;
      cursor: pointer;
      padding: 0.5rem 0;
      transition: color 0.2s ease;
    }

    details summary:hover {
      color: #5D7A6A;
    }

    details[open] summary {
      color: #7A9A86;
    }

    /* Map container */
    .leaflet-container {
      border-radius: 8px;
      box-shadow: inset 0 0 0 1px rgba(122, 154, 134, 0.2);
    }

    /* Plotly chart containers */
    .plotly {
      border-radius: 8px;
    }

    /* Dev badge */
    .badge.bg-warning.text-dark {
      font-family: 'JetBrains Mono', monospace;
      font-size: 0.7rem;
      padding: 0.3em 0.6em;
    }

    /* How it works step cards (welcome page) */
    .step-card {
      background: linear-gradient(135deg, rgba(122,154,134,0.1) 0%, rgba(122,154,134,0.05) 100%);
      border-radius: 10px;
    }
    .step-card-title {
      font-family: 'Montserrat', sans-serif;
      font-size: 0.9rem;
      font-weight: 600;
    }

    /* Wizard step indicator */
    .wizard-steps {
      padding: 0 0.25rem;
    }

    /* Early Access banner */
    .early-access-banner {
      background: linear-gradient(135deg, rgba(211, 155, 53, 0.08) 0%, rgba(211, 155, 53, 0.04) 100%);
      border: 1px solid rgba(211, 155, 53, 0.3);
      border-left: 4px solid #D39B35;
      border-radius: 8px;
      padding: 0.75rem 1rem;
      margin-bottom: 1rem;
    }

    .early-access-banner .progress {
      height: 6px;
      border-radius: 3px;
      background: rgba(211, 155, 53, 0.15);
    }

    .early-access-banner .progress-bar {
      background: linear-gradient(90deg, #D39B35 0%, #B8841E 100%);
      border-radius: 3px;
    }

    /* Seed database banner (welcome page) */
    .seed-database-banner {
      background: linear-gradient(135deg, rgba(122, 154, 134, 0.08) 0%, rgba(122, 154, 134, 0.04) 100%);
      border: 1px solid rgba(122, 154, 134, 0.3);
      border-left: 4px solid #7A9A86;
      border-radius: 8px;
      padding: 0.75rem 1rem;
    }

    .seed-database-banner .progress {
      height: 8px;
      border-radius: 4px;
      background: rgba(122, 154, 134, 0.15);
    }

    .seed-database-banner .progress-bar {
      background: linear-gradient(90deg, #7A9A86 0%, #5D7A6A 100%);
      border-radius: 4px;
    }

    /* Lab confirmation checkbox: inline */
    .lab-confirm-gate .form-check,
    .lab-confirm-gate .checkbox label {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    .lab-confirm-gate .form-check-input,
    .lab-confirm-gate .checkbox input[type='checkbox'] {
      margin-top: 0;
      flex-shrink: 0;
    }

    /* ===========================
       RESPONSIVE / MOBILE
       =========================== */

    /* --- Tablet (≤768px) --- */
    @media (max-width: 768px) {
      .user-dropdown-menu {
        max-width: calc(100vw - 20px) !important;
        right: 0 !important;
      }

      .dataTables_wrapper {
        overflow-x: auto;
        -webkit-overflow-scrolling: touch;
      }

      .plotly, .shiny-plot-output {
        height: 400px !important;
      }

      /* Plotly: hide mode bar, tighten margins */
      .plotly .modebar-container {
        display: none !important;
      }

      .bslib-sidebar-layout > .sidebar {
        width: 260px !important;
      }

      .card-body {
        padding: 1rem !important;
      }

      .card-header {
        padding: 0.75rem 1rem !important;
      }

      /* Analysis tabs: scrollable with visible hint */
      .analysis-tabs-container .nav-tabs,
      .analysis-tabs-container .card-header-tabs,
      .analysis-tabs-container ul.nav.nav-tabs {
        -webkit-overflow-scrolling: touch;
        scrollbar-width: thin;
        /* Fade hint on right edge */
        mask-image: linear-gradient(to right, black 85%, transparent 100%);
        -webkit-mask-image: linear-gradient(to right, black 85%, transparent 100%);
      }

      .analysis-tabs-container .nav-tabs .nav-link,
      .analysis-tabs-container .card-header-tabs .nav-link {
        font-size: 0.8rem !important;
        padding: 0.4rem 0.6rem !important;
      }
    }

    /* --- Phone (≤576px) --- */
    @media (max-width: 576px) {
      /* Touch targets: minimum 44px per WCAG */
      .btn, .action-button, .dropdown-item {
        min-height: 44px;
        padding-top: 0.5rem !important;
        padding-bottom: 0.5rem !important;
      }

      .form-control, .form-select, .selectize-input {
        min-height: 44px;
        font-size: 16px !important; /* prevents iOS auto-zoom on focus */
      }

      /* Navbar zip input: compact */
      #nav_zipcode {
        width: 60px !important;
      }

      /* Sidebar: full-width overlay */
      .bslib-sidebar-layout > .sidebar {
        width: 100% !important;
        max-width: 100% !important;
      }

      /* Plots: compact, legend below chart */
      .plotly, .shiny-plot-output {
        height: 350px !important;
      }

      .plotly .legend {
        font-size: 10px !important;
      }

      /* Plotly title: smaller */
      .plotly .gtitle {
        font-size: 13px !important;
      }

      /* Cards: tighter padding */
      .card-body {
        padding: 0.75rem !important;
      }

      .card-header {
        padding: 0.5rem 0.75rem !important;
      }

      /* DataTables: smaller text, tighter cells */
      .dataTables_wrapper {
        font-size: 0.85rem;
      }

      .dataTable thead th {
        padding: 0.5rem !important;
        font-size: 0.8rem;
      }

      .dataTable tbody td {
        padding: 0.5rem !important;
        font-size: 0.85rem;
      }

      /* Modals: near full-width */
      .modal-dialog {
        margin: 0.5rem !important;
        max-width: calc(100vw - 1rem) !important;
      }

      /* Typography scale-down */
      h1 { font-size: 1.5rem !important; }
      h2 { font-size: 1.3rem !important; }
      h3 { font-size: 1.15rem !important; }

      /* Welcome hero: compact */
      .welcome-hero img {
        max-height: 120px;
      }

      /* Stat cards on welcome page */
      .stat-card {
        padding: 0.5rem 0.25rem !important;
      }

      .stat-number {
        font-size: 1.5rem !important;
      }
    }

    /* --- Landscape mobile (short viewport) --- */
    @media (max-height: 480px) and (orientation: landscape) {
      .plotly, .shiny-plot-output {
        height: 280px !important;
      }
    }
  "))
}

# ---------------------------
# Reusable UI Components
# ---------------------------

#' Early Access Data banner with progress bar
#' @param n_samples Current number of samples
#' @param n_contributors Current number of contributors
#' @param min_samples Required samples threshold
#' @param min_contributors Required contributors threshold
#' @return Shiny UI tag
early_access_ui <- function(n_samples, n_contributors,
                            min_samples = MIN_SAMPLES_FOR_PUBLIC_STATS,
                            min_contributors = MIN_CONTRIBUTORS_FOR_PUBLIC_STATS) {
  sample_pct <- min(100, round(n_samples / min_samples * 100))
  contrib_pct <- min(100, round(n_contributors / min_contributors * 100))

  div(class = "early-access-banner",
      div(class = "d-flex align-items-center mb-2",
          tags$i(class = "fa fa-flask me-2", style = "color: #D39B35;"),
          tags$strong("Early Access Data", style = "color: #8B6B3A; font-family: 'Montserrat', sans-serif; font-size: 0.85rem;")
      ),
      tags$small(class = "text-muted d-block mb-1",
                 sprintf("This species needs more data for reliable statistics. Help by contributing samples!")),
      div(class = "d-flex align-items-center mb-1",
          tags$small(class = "text-muted me-2", style = "min-width: 80px;",
                     sprintf("Samples: %d/%d", n_samples, min_samples)),
          div(class = "progress flex-grow-1",
              div(class = "progress-bar", role = "progressbar",
                  style = sprintf("width: %d%%;", sample_pct),
                  `aria-valuenow` = n_samples, `aria-valuemin` = 0, `aria-valuemax` = min_samples))
      ),
      div(class = "d-flex align-items-center",
          tags$small(class = "text-muted me-2", style = "min-width: 80px;",
                     sprintf("People: %d/%d", n_contributors, min_contributors)),
          div(class = "progress flex-grow-1",
              div(class = "progress-bar", role = "progressbar",
                  style = sprintf("width: %d%%;", contrib_pct),
                  `aria-valuenow` = n_contributors, `aria-valuemin` = 0, `aria-valuemax` = min_contributors))
      )
  )
}

#' Seed the database banner for welcome page
#' @param total_samples Current total samples in database
#' @param min_total Required total threshold
#' @return Shiny UI tag
seed_database_ui <- function(total_samples, min_total = MIN_TOTAL_SAMPLES_FOR_SITE_STATS) {
  pct <- min(100, round(total_samples / min_total * 100))

  div(class = "seed-database-banner",
      div(class = "d-flex align-items-center mb-2",
          tags$i(class = "fa fa-seedling me-2", style = "color: #7A9A86;"),
          tags$strong("Help Us Seed the Database", style = "color: #4A6B5A; font-family: 'Montserrat', sans-serif; font-size: 0.85rem;")
      ),
      tags$small(class = "text-muted d-block mb-1",
                 "Every soil sample helps build better plant recommendations."),
      div(class = "d-flex align-items-center",
          tags$small(class = "text-muted me-2", style = "min-width: 80px;",
                     sprintf("%d/%d samples", total_samples, min_total)),
          div(class = "progress flex-grow-1",
              div(class = "progress-bar", role = "progressbar",
                  style = sprintf("width: %d%%;", pct),
                  `aria-valuenow` = total_samples, `aria-valuemin` = 0, `aria-valuemax` = min_total))
      )
  )
}
