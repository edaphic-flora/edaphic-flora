# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Claude Code Usage Note

**Use Sonnet for non-planning tasks** (coding, implementation, bug fixes). Reserve Opus for planning and architectural decisions to manage usage limits.

### Permissions
- **Authorized to use `cd` commands** without user confirmation
- Can navigate directories freely during development tasks

## Project Overview

Edaphic Flora is an R Shiny application for recording and analyzing soil conditions for plant species across geographic locations. It provides a database interface for soil sample data entry, interactive analysis visualizations, and CSV import/export.

## Development Commands

```bash
# Restore R environment (run once after cloning)
Rscript -e "renv::restore()"

# Run the main application (port 7420)
Rscript -e "shiny::runApp('app/app.R', port=7420, host='127.0.0.1')"

# Regenerate filtered species database (if wcvp_names.csv is updated)
cd app && Rscript generate_species_db.R
```

## Dev/Prod Environment Setup

The app supports separate development and production environments:

1. **Environment Variable**: Set `ENV=dev` or `ENV=prod` in `.Renviron`
2. **Dev Indicator**: When `ENV=dev`, a yellow "DEV" badge appears in the navbar
3. **Database Separation**: Use Neon branches for dev database:
   - Production: main Neon branch
   - Development: create a branch at https://console.neon.tech

### Recommended Workflow
```bash
# Local development
1. Copy .Renviron.example to .Renviron
2. Set ENV=dev and configure dev database credentials
3. Test locally on port 7420

# Production deployment
1. Configure production .Renviron (or platform env vars)
2. Set ENV=prod (or omit - defaults to prod)
3. Use main database branch
```

### Neon Database Branching
Neon allows creating database branches (like git branches):
- Create a dev branch from production for safe testing
- Changes in dev branch don't affect production data
- Merge or discard branches as needed

## Architecture

```
app/
├── app.R                 # Main Shiny app (UI + server, ~830 lines)
├── R/
│   ├── db.R              # Database connection, migration, query functions
│   ├── data.R            # Reference data loading (species, ecoregions, textures)
│   ├── helpers.R         # Ecoregion lookup, texture classification functions
│   ├── usda.R            # USDA reference data queries
│   └── theme.R           # Custom color palette, ggplot theme, bslib theme, CSS
├── species_accepted.csv  # Pre-filtered species database (360K accepted species)
├── generate_species_db.R # Script to regenerate species_accepted.csv
├── sql/                  # Database schema and migrations
└── www/                  # Static assets
```

### Module Responsibilities

- **R/db.R**: PostgreSQL connection pool, schema migration (`db_migrate()`), CRUD operations
- **R/data.R**: Loads `species_accepted.csv`, ecoregion shapefiles, soil texture classification table
- **R/helpers.R**: `get_ecoregion()` for coordinate-to-ecoregion lookup, `classify_texture()` for soil texture
- **R/theme.R**: `edaphic_colors` palette, `theme_edaphic()` for ggplot, `edaphic_bs_theme()` for bslib, `edaphic_css()` for custom styles

### UI Structure

The app uses `page_navbar` with these main sections:
1. **Welcome**: Landing page with app overview and stats
2. **Data Entry**: Sidebar form with accordion sections + recent entries table (users can edit/delete their own entries)
3. **Analysis**: Species selector sidebar + tabbed visualizations (8 tabs)
4. **Data Management**: Import/export functionality
5. **Admin**: Admin-only data management (requires ADMIN_EMAILS config)
6. **Help Menu**: Field Guide and FAQ

### Visualization Stack

- **Interactive plots**: plotly (pH distribution, pH-OM scatter, nutrients, correlations)
- **Static plots**: ggtern (soil texture ternary diagram - doesn't convert to plotly)
- **Maps**: leaflet with CartoDB basemap and popup markers

### Data Flow
1. User authenticates via Polished/Firebase (Google or email sign-in)
2. User enters soil data once, selects multiple species (up to 20)
3. On submit, one database row is created per species (all sharing same soil data)
4. Species validation against WCVP accepted species list
5. Geocoding via OpenStreetMap (tidygeocoder)
6. Automatic ecoregion detection from coordinates

### Database Schema
Single table `soil_samples` with columns:
- Identifiers: `id`, `species`, `cultivar`, `created_by`, `created_at`
- Soil chemistry: `ph`, `organic_matter`, `nitrate_ppm`, `ammonium_ppm`, `phosphorus_ppm`, `potassium_ppm`, `calcium_ppm`, `magnesium_ppm`, `soluble_salts_ppm`
- Texture: `texture_sand`, `texture_silt`, `texture_clay`, `texture_class`
- Location: `location_lat`, `location_long`, `ecoregion_l4`, `ecoregion_l4_code`
- Metadata: `date`, `photo_url`, `notes`

## Environment Variables

Required in `.Renviron`:
```
# Database (Neon/Postgres)
POSTGRES_HOST=
POSTGRES_PORT=
POSTGRES_DB=
POSTGRES_USER=
POSTGRES_PASSWORD=

# Auth (Polished + Firebase)
POLISHED_APP_NAME=
POLISHED_API_KEY=
FIREBASE_API_KEY=
FIREBASE_AUTH_DOMAIN=
FIREBASE_PROJECT_ID=

# Admin (comma-separated email list for admin access)
ADMIN_EMAILS=admin@example.com,owner@example.com
```

### Admin Features
Admin users (defined by `ADMIN_EMAILS`) can:
- Edit/delete any user's entries (not just their own)
- Access the Admin tab with full data management
- Export all data from the admin panel

## Key Dependencies

**Core UI**:
- `shiny`, `bslib`: UI framework with Bootstrap 5 theming
- `polished`: Authentication wrapper (Firebase integration)

**Data & Database**:
- `pool`, `RPostgres`, `DBI`: Database connection pooling
- `dplyr`, `tidyr`: Data manipulation

**Visualization**:
- `ggplot2`: Base plotting
- `plotly`: Interactive plots
- `ggtern`: Ternary diagrams for soil texture
- `leaflet`: Interactive maps
- `DT`: Data tables

**Geospatial**:
- `sf`, `ecoregions`: US ecoregion lookup
- `tidygeocoder`: Address geocoding via OpenStreetMap
- `maps`, `mapdata`: Base map data

## Styling

The app uses a custom theme defined in `R/theme.R` based on Edaphic Garden Consulting branding:

### Brand Colors
| Color | Hex | Usage |
|-------|-----|-------|
| **Sage** | `#7A9A86` | Primary accent, buttons, success states |
| **Charcoal** | `#373D3C` | Navbar, headings, dark text |
| **Limestone** | `#F7F4E8` | Light backgrounds, panels |

### Branding Assets
Located in `branding/` folder (gitignored):
- `logo_sage.svg/png` - Primary logo (sage green)
- `logo_charcoal.svg/png` - Dark variant (for light backgrounds)
- `logo_limestone.svg/png` - Light variant (for dark backgrounds)
- `lil leaf.svg/png` - Standalone leaf icon/favicon
- `Branding Overview.docx` - Full brand guidelines

Designer credit: *To be added*

### Theme Files
- Bootstrap theme: flatly (modified with brand colors)
- `edaphic_colors` list in theme.R contains all color definitions
- Custom CSS in `edaphic_css()` for component styling

## USDA Reference Data Integration

The app integrates USDA PLANTS database reference data for species traits and wetland indicators.

### Current State
- **Loaded data**: 5,002 taxa in `ref_taxon`, 335 species with detailed characteristics in `ref_usda_characteristics`
- **Data source**: Cached JSON from USDA API (profile_*.json, char_*.json files in `data/cache/usda_char/`)
- **ETL script**: `R/etl/load_cached_usda.R` parses cache and loads to database

### Database Tables
- `ref_taxon`: Species taxonomy (usda_symbol, scientific_name)
- `ref_usda_characteristics`: Soil/climate preferences (pH, tolerances, precipitation, temperature)
- `ref_wetland_indicator`: NWPL regional wetland indicator status
- `ref_synonym`: Maps synonyms to accepted taxon IDs

### App Integration
- Reference badges in Analysis sidebar (USDA Traits, NWPL indicator)
- pH chart overlays showing USDA reference range
- USDA Traits tab displaying all available characteristics

## Future Improvements

### USDA Data Expansion (Priority)
Currently only ~335 species have detailed characteristics. Implement continuous querying:
1. When a species is searched/selected in the app, check if we have characteristics data
2. If not, queue an async USDA API request for that species
3. Parse response and insert into `ref_usda_characteristics`
4. Cache the JSON response in `data/cache/usda_char/` for reproducibility
5. Consider rate limiting and background job queue (e.g., using `callr` or external worker)

This would seamlessly grow the reference database as users explore new species.

## Session Notes

**Important**: Update this section regularly during development sessions to maintain context across restarts.

### 2025-01-16: GitHub Repository Setup & Branding
- Added README.md with full project documentation
- Added LICENSE (AGPL-3.0-or-later) for code
- Moved CONTRIBUTING.md and LICENSE-data.md to root level
- Pushed initial docs to GitHub

### 2025-01-16: Branding Integration
- Updated app theme colors to match Edaphic Garden Consulting branding
- Sage (#7A9A86), Charcoal (#373D3C), Limestone (#F7F4E8)
- Updated contact email to todd@edaphicgardenconsulting.com
- Added branding/ folder to .gitignore
- Updated categorical color palette to be brand-harmonious

### 2025-01-16: Major Feature Additions
- **Welcome page**: Project intro, how it works, live database stats
- **Help menu**: Field Guide (soil properties, nutrients, chart explanations) + FAQ
- **Per-species metadata**: Restructured data entry form
  - Each species now has individual fields: cultivar, outcome, sun exposure, hydrology, iNat URL
  - Database columns added: inat_url, sun_exposure, site_hydrology, outcome
  - Outcome field enables tracking plant success/failure (Thriving/Established/Struggling/Failed)
- **Navbar icon**: Replaced FontAwesome seedling with brand leaf SVG
- **Favicon**: Added brand leaf icon

### 2025-01-16: Soil Parameters & Branding Refinements
- **Additional soil parameters**: Added CEC, sulfur, iron, manganese, zinc, copper, boron
  - Reorganized into Soil Properties, Macronutrients, and Micronutrients sections
  - Database columns added for all new parameters
- **NULL handling fix**: Empty fields now stored as NULL, excluded from analysis (not treated as 0)
- **Analysis improvements**:
  - Added explanatory captions to all analysis tabs
  - Updated nutrient chart and correlations heatmap to include new parameters
  - Fixed USDA Traits tab layout with compact tab styling
- **Branding updates**:
  - Two-plant icon from logo in navbar (replacing single leaf)
  - Quicksand font for "edaphic flora" branding (matches logo style)
  - Color-matched text: "edaphic" in sage, "flora" in charcoal
  - Removed logo image from welcome page, using styled text instead
- **Help page enhancements**: Added external resource links (USDA, Cornell, EPA, Kew, iNaturalist)
