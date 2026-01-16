# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Claude Code Usage Note

**Use Sonnet for non-planning tasks** (coding, implementation, bug fixes). Reserve Opus for planning and architectural decisions to manage usage limits.

## Project Overview

ShinyPlants ("Edaphic Flora") is an R Shiny application for recording and analyzing soil conditions for plant species across geographic locations. It provides a database interface for soil sample data entry, interactive analysis visualizations, and CSV import/export.

## Development Commands

```bash
# Restore R environment (run once after cloning)
Rscript -e "renv::restore()"

# Run the main application (port 7420)
Rscript -e "shiny::runApp('ShinyPlants/app.R', port=7420, host='127.0.0.1')"

# Regenerate filtered species database (if wcvp_names.csv is updated)
cd ShinyPlants && Rscript generate_species_db.R
```

## Architecture

```
ShinyPlants/
├── app.R                 # Main Shiny app (UI + server, ~830 lines)
├── R/
│   ├── db.R              # Database connection, migration, query functions
│   ├── data.R            # Reference data loading (species, ecoregions, textures)
│   ├── helpers.R         # Ecoregion lookup, texture classification functions
│   └── theme.R           # Custom color palette, ggplot theme, bslib theme, CSS
├── species_accepted.csv  # Pre-filtered species database (360K accepted species)
├── generate_species_db.R # Script to regenerate species_accepted.csv
└── www/                  # Static assets
```

### Module Responsibilities

- **R/db.R**: PostgreSQL connection pool, schema migration (`db_migrate()`), CRUD operations
- **R/data.R**: Loads `species_accepted.csv`, ecoregion shapefiles, soil texture classification table
- **R/helpers.R**: `get_ecoregion()` for coordinate-to-ecoregion lookup, `classify_texture()` for soil texture
- **R/theme.R**: `edaphic_colors` palette, `theme_edaphic()` for ggplot, `edaphic_bs_theme()` for bslib, `edaphic_css()` for custom styles

### UI Structure

The app uses `page_navbar` with three main tabs:
1. **Data Entry**: Sidebar form with accordion sections + recent entries table
2. **Analysis**: Species selector sidebar + tabbed visualizations (8 tabs)
3. **Data Management**: Import/export functionality

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
```

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

The app uses a custom earth-toned theme defined in `R/theme.R`:
- Primary green: `#27ae60` (plants/growth)
- Secondary brown: `#8B4513` (soil)
- Dark slate: `#2c3e50` (navbar, text)
- Bootstrap theme: flatly (modified)

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
