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

# Run automated tests
cd tests/testthat && Rscript -e "testthat::test_dir('.', reporter = 'summary')"

# Regenerate filtered species database (if wcvp_names.csv is updated)
cd app && Rscript generate_species_db.R
```

### Port Management (Important!)
**Do NOT launch the Shiny app from Claude Code.** Port conflicts are common and difficult to resolve remotely. The user will run the app from RStudio instead.

If port issues occur, use these commands in PowerShell:
```powershell
# Check what's using port 7420
netstat -ano | findstr ":7420"

# Kill by PID (replace 12345 with actual PID)
taskkill /F /PID 12345

# Or kill all R processes
Get-Process -Name Rscript, rsession, R -ErrorAction SilentlyContinue | Stop-Process -Force
```

TIME_WAIT connections (PID 0) will clear automatically in 1-2 minutes - just use a different port temporarily.

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
├── app.R                 # Main Shiny app (UI + server, ~4100 lines)
├── R/
│   ├── db.R              # Database connection, migration, query functions
│   ├── data.R            # Reference data loading (species, ecoregions, textures)
│   ├── helpers.R         # Ecoregion lookup, texture classification functions
│   ├── usda.R            # USDA reference data queries
│   ├── theme.R           # Custom color palette, ggplot theme, bslib theme, CSS
│   ├── pdf_extract.R     # AI-powered soil report extraction (Claude API)
│   ├── mod_analysis.R    # Analysis tab Shiny module (template)
│   └── mod_data_entry.R  # Data entry tab Shiny module (template)
├── species_accepted.csv  # Pre-filtered species database (360K accepted species)
├── generate_species_db.R # Script to regenerate species_accepted.csv
├── fetch_usda_data.R     # Batch USDA data fetcher script
├── sql/                  # Database schema and migrations
└── www/                  # Static assets
```

### Module Responsibilities

- **R/db.R**: PostgreSQL connection pool, schema migration (`db_migrate()`), CRUD operations, audit logging
- **R/data.R**: Loads `species_accepted.csv`, ecoregion shapefiles, soil texture classification table
- **R/helpers.R**: `get_ecoregion()` for coordinate-to-ecoregion lookup, `classify_texture()` for soil texture
- **R/theme.R**: `edaphic_colors` palette, `theme_edaphic()` for ggplot, `edaphic_bs_theme()` for bslib, `edaphic_css()` for custom styles
- **R/pdf_extract.R**: Claude API integration for extracting soil data from PDFs, images, RTF files
- **R/mod_analysis.R**: Shiny module template for analysis tab (not yet integrated into app.R)
- **R/mod_data_entry.R**: Shiny module template for data entry tab (not yet integrated into app.R)

### Modularization Status (COMPLETE)

All 7 modules have been extracted from app.R:

| Module | Lines | Description |
|--------|-------|-------------|
| **mod_help.R** | 309 | Field Guide + FAQ |
| **mod_welcome.R** | 193 | Landing page with stats and map |
| **mod_admin.R** | 114 | Admin-only data management |
| **mod_data_management.R** | 223 | Export and CSV import |
| **mod_find_plants.R** | 499 | Species recommendation engine |
| **mod_data_entry.R** | 704 | Soil sample data collection |
| **mod_analysis.R** | 1,733 | Species data visualization |
| **app.R** | 568 | Core app (routing, auth, edit handlers) |

Total: ~4,343 lines (previously ~4,100 in app.R alone)

### UI Structure

The app uses `page_navbar` with these main sections:
1. **Welcome**: Landing page with app overview, stats, and help links
2. **Data Entry**: Sidebar form with accordion sections + recent entries table (users can edit/delete their own entries)
3. **Analysis**: Species selector sidebar with filters (Outcome, Sun, Hydrology, Cultivar) + tabbed visualizations:
   - Summary, pH Distribution, Organic Matter, Texture, Nutrients, Correlations, Map, Performance (new), USDA Traits
4. **Data Management**: Import/export functionality
5. **Admin**: Admin-only data management (requires ADMIN_EMAILS config)
6. **Help Menu**: Field Guide (with Soil Properties, Nutrients, Plant Performance definitions) and FAQ

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

### shinyapps.io Deployment (Free Tier)

**Important:** The shinyapps.io free tier does NOT allow setting environment variables through the dashboard. The workaround is to include the `.Renviron` file directly in the deployment:

1. The `app/.Renviron` file contains all production credentials
2. This file is gitignored but MUST be present locally for deployment
3. When deploying via `rsconnect::deployApp()`, the .Renviron is uploaded with the app
4. Set `ENV=prod` in .Renviron before deploying to production

**Never commit `.Renviron` to git** - it contains sensitive credentials.

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

### Memory Optimization (for shinyapps.io Free Tier)

The free tier has 1GB RAM. Current optimizations:

1. **Ecoregion lookup disabled** in production (saves ~95MB)
2. **Species database** - supports RDS format (faster load, smaller footprint)
3. **DB pool size** - configurable via `DB_POOL_SIZE` env var (default: 5)
4. **Grid-based lookups** - ecoregion_grid.rds (~2MB) instead of shapefile

**To convert CSV files to RDS (recommended):**
```bash
cd app && Rscript scripts/convert_to_rds.R
```

This converts species_accepted.csv (21MB) to RDS (~8MB) for faster loading.

### Ecoregion Lookup (Disabled in Production)
The ecoregion lookup feature is currently disabled in production due to memory constraints on shinyapps.io's free tier. The `ecoregions` package loads a large shapefile (~95MB in memory) that causes OOM crashes. Even simplified versions (~93MB) exceed limits.

**Current behavior**: Ecoregion fields (`ecoregion_l4`, `ecoregion_l4_code`) are saved as NA in production.

**Solutions to re-enable**:
1. **Pre-computed grid lookup (recommended)** - Generate a CSV mapping lat/long grid cells to ecoregions. At runtime, round coordinates to nearest cell and do simple table lookup. Minimal memory usage.
2. Use an external API for ecoregion lookup (e.g., EPA services)
3. Upgrade to a paid shinyapps.io plan with more memory (2GB+)

**Development workflow note**: Avoid small incremental changes followed by 5-10 minute redeployments. Batch fixes together and test locally before deploying.

### USDA Data Expansion (Priority)
Currently only ~335 species have detailed characteristics. Implement continuous querying:
1. When a species is searched/selected in the app, check if we have characteristics data
2. If not, queue an async USDA API request for that species
3. Parse response and insert into `ref_usda_characteristics`
4. Cache the JSON response in `data/cache/usda_char/` for reproducibility
5. Consider rate limiting and background job queue (e.g., using `callr` or external worker)

### Analysis Tab Enhancements (Completed 2025-01-17)
The per-species metadata fields (outcome, sun_exposure, site_hydrology) are now fully visualized:
1. **Outcome distribution** - Stacked bar chart on Performance tab
2. **Sun exposure breakdown** - Stacked bar chart on Performance tab
3. **Hydrology analysis** - Stacked bar chart on Performance tab
4. **Cross-tab filters** - Filter sidebar with Outcome, Sun Exposure, Hydrology, and Cultivar dropdowns (with clear buttons)
5. **Success factors** - Selectable box plots comparing soil parameters by outcome (pH, OM, clay, nutrients, etc.)
6. **Key insights** - Auto-generated insights panel showing overall success rate, best conditions, and conditions to avoid
7. **Summary page** - Metric cards with sample count, success rate, cultivar count, and location count + detail sections

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

### 2025-01-17: Performance Analysis & UI Improvements
- **Test data**: Created `scripts/insert_test_data.R` to insert 25 samples for Acer rubrum and Agastache foeniculum with varied outcomes
- **Performance tab**: New tab with stacked bar charts for Outcome, Sun Exposure, and Hydrology distributions
- **Success Factors**: Selectable box plot comparing soil parameters by outcome (8 options: pH, OM, clay, N, P, K, Ca, Mg)
- **Key Insights panel**: Auto-generated insights showing overall success rate, best sun/hydrology conditions, pH patterns, and conditions to avoid
- **Filter enhancements**:
  - Changed dropdowns to selectizeInput with clear_button plugin (X buttons)
  - Added "Clear All" link to reset filters
  - Added dynamic Cultivar filter that populates based on selected species
  - Ordered outcome legends: Thriving → Established → Struggling → Failed/Died
- **Summary page redesign**: 4 metric cards (Samples, Success Rate, Cultivars, Locations) + Soil Chemistry and Performance detail cards
- **Reference Data section**: Cleaned up styling with compact card layout and inline toggle
- **Welcome page help links**: Added navigation links to Field Guide for Soil Properties, Nutrients, and Plant Performance definitions
- **Help section**: Added Plant Performance definitions (Outcome, Sun Exposure, Site Hydrology)
### 2025-01-17: Find Plants, Similar Species & Native Plant Guidance

**New Features:**

1. **Find Plants Page** (new top-level nav)
   - Users enter soil test values (pH required, plus OM, texture, nutrients optional)
   - Matches against species with 10+ samples in database
   - Weighted scoring: pH 35%, OM 20%, texture 15%, nutrients 30%
   - Shows up to 15 recommended species with match scores, success rates, and optimal conditions
   - PDF/image upload support to auto-fill form from soil reports

2. **Similar Species Tab** (in Analysis)
   - Shows species with similar soil profiles to the selected species
   - Uses weighted similarity scoring (pH 30%, OM 20%, texture 15%, nutrients 35%)
   - Displays top 10 matches with soil comparisons and success rates
   - Useful for companion planting and finding alternatives

3. **Species Data Preview Enhancement**
   - Analysis sidebar now shows sample count with color-coded success rate
   - Displays pH range, ecoregion count, and average OM at a glance

4. **Download Button**
   - Added CSV export to Analysis sidebar
   - Respects current filters, exports key columns
   - Filename: `edaphic_flora_[species]_[date].csv`

5. **PDF Import on Find Plants**
   - Same soil report upload feature as Data Entry
   - Auto-fills pH, OM, texture, and nutrient fields
   - Supports PDF, RTF, TXT, and images (PNG, JPG, etc.)

**Documentation & Native Species Guidance:**

- **Welcome page**: Added "Prioritize Native Plants" tip with ecosystem benefits
- **FAQ**: New section "Why should I prioritize native plants?" covering:
  - Ecosystem support, lower maintenance, soil health, resilience
  - Warning about invasive species with link to USDA resources
- **Find Plants caveats**: Prominent "Prioritize Native Plants" callout box with invasive species check link
- **Similar Species caveats**: Updated to emphasize native plants and invasive species verification
- **Find Plants empty state**: Updated "Keep in mind" text with native plant guidance

**Bug Fixes:**

- **pH vs OM legend order**: Fixed plotly legend to show Thriving → Established → Struggling → Failed/Died
  - Data sorted by outcome factor level before plotting
  - Plotly traces reordered after conversion to ensure correct legend order

**Technical Notes:**

- `calc_species_profile()` - Calculates soil profile stats for a species
- `calc_similarity()` - Weighted scoring between two species profiles
- `calc_user_match()` - Matches user input against species profiles
- `get_all_species_profiles()` - Reactive that queries species with 10+ samples
- All recommendation features require minimum 10 samples per species for statistical confidence

### 2025-01-17: Native Status Toggle Feature (DEV ONLY)

**New Feature: Native Status Badge in Analysis Sidebar**

**Status: DEV ONLY** - Currently disabled in production. The USDA data is regional (L48/AK/HI) rather than state-specific, making it not granular enough to be useful yet. Will re-enable when state-level or county-level data is available.

Shows whether a selected species is native or introduced in the user's area, based on the lat/long coordinates from submitted soil samples.

**Implementation:**

1. **Database Schema** (`R/db.R`):
   - New table `ref_state_distribution` with columns: taxon_id, state_code, native_status, source
   - Indexes on taxon_id and state_code for fast lookups

2. **State Grid Generator** (`generate_state_grid.R`):
   - Pre-computes lat/long to US state mapping using `tigris` package
   - Creates `data/state_grid.rds` (~500KB) for production use
   - Run: `cd app && Rscript generate_state_grid.R`

3. **Data Loading Functions** (`R/data.R`):
   - `load_state_grid()` - Loads pre-computed state grid
   - `get_state_from_grid(lat, lon, grid)` - Single coordinate lookup
   - `get_states_from_coords(lats, lons, grid)` - Batch lookup

4. **Native Status Lookup** (`R/usda.R`):
   - `get_native_status_for_state(gs_name, state_code, pool)` - Single state lookup with caching
   - `get_native_status_summary(gs_name, state_codes, pool)` - Multi-state summary
   - Cache environment `.usda_cache$native_status` for performance

5. **ETL Script** (`R/etl/usda_state_dist_etl.R`):
   - Loads USDA state distribution data into `ref_state_distribution`
   - Supports wide format (state columns) or long format (State, Native Status columns)
   - Run: `source("R/etl/usda_state_dist_etl.R"); usda_state_dist_etl_run()`

6. **UI Badge** (`app.R`):
   - Shows in Analysis sidebar below USDA Reference section
   - Color-coded: green (native), yellow (introduced), blue (mixed), gray (unknown)
   - Mixed status shows state breakdown (e.g., "Native: PA, NY | Introduced: CA")

**Badge Display:**

- **Native**: Green badge with checkmark - "Native to your area"
- **Introduced**: Yellow badge with warning - "Introduced in your area"
- **Mixed**: Blue badge with info icon - "Mixed native status" + state breakdown
- **Unknown**: Gray badge with question mark - "Native status unknown"

**Files Created/Modified:**

| File | Action |
|------|--------|
| `app/R/db.R` | Added ref_state_distribution migration |
| `app/R/data.R` | Added state grid loading functions |
| `app/R/usda.R` | Added native status lookup with caching |
| `app/generate_state_grid.R` | Created - state grid generator |
| `app/R/etl/usda_state_dist_etl.R` | Created - USDA distribution ETL |
| `app/app.R` | Added UI output and reactive badge |

**Setup Required:**

1. Generate state grid: `cd app && Rscript generate_state_grid.R`
2. Download USDA state distribution data (PLANTS checklist)
3. Run ETL: `source("R/etl/usda_state_dist_etl.R"); usda_state_dist_etl_run("data/raw/usda/state_dist.csv")`

### 2025-01-17: Code Cleanup & Modularization Planning

**Modularization Plan:**
- Created comprehensive plan at `app/docs/MODULARIZATION_PLAN.md`
- Identifies 7 modules with dependencies, line counts, and testing checkpoints
- Recommended implementation order from lowest to highest risk
- Estimated ~15 hours total work

**Code Cleanup:**
- Removed unused `pdf_extract_config` alias from `R/pdf_extract.R`
- Removed commented-out async imports from `app.R` (lines 31-33)
- Added named constants to replace magic numbers:
  - `TEXTURE_TOLERANCE` (0.1) in `R/helpers.R`
  - `GRID_RESOLUTION` (0.1) and `GRID_MATCH_TOLERANCE` (0.001) in `R/data.R`
- Updated functions to use new constants

### 2025-01-17: App Modularization (Phase 1)

**Completed Module Extractions:**

Extracted 5 modules from app.R, reducing it from ~4,100 lines to ~3,019 lines:

1. **mod_help.R** (273 lines)
   - Static Field Guide and FAQ content
   - No server logic needed
   - Fixed broken USDA Web Soil Survey link (was 404)

2. **mod_welcome.R** (186 lines)
   - Welcome page with database stats and sample locations map
   - Dependencies: pool, data_changed

3. **mod_admin.R** (107 lines)
   - Admin panel with lock screen for non-admins
   - All entries table with edit/delete buttons
   - Edit/delete buttons trigger global handlers (not namespaced)

4. **mod_data_management.R** (147 lines)
   - Export all data as CSV
   - Download template
   - CSV import with validation

5. **mod_find_plants.R** (471 lines)
   - Find Plants UI with soil profile form
   - PDF upload extraction support
   - `calc_user_match()` function for user-to-species matching
   - Results rendering with match scores and recommendations

**Shared Functions Moved to helpers.R:**
- `calc_species_profile()` - Calculate soil profile stats for a species
- `calc_similarity()` - Calculate similarity between two profiles
- Used by both Find Plants module and Similar Species (Analysis tab)

**Module Pattern:**
```r
# UI function
moduleUI <- function(id) {
  ns <- NS(id)
  nav_panel(title = "...", ...)
}

# Server function
moduleServer <- function(id, pool, current_user, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Server logic
  })
}
```

**Remaining Modules (Higher Risk):**
- mod_data_entry.R (~800 lines) - Complex form with PDF upload, species selection
- mod_analysis.R (~1,200 lines) - Multiple tabs, many reactive dependencies

**Known Issues:**
- PDF upload error for non-admin users was reported but couldn't reproduce without error message
- Code reviewed and appears correct with proper error handling

**Next Steps:**
- Run `Rscript scripts/convert_to_rds.R` to optimize data file loading
- Continue USDA data expansion with `fetch_usda_batch()`
- Monitor shinyapps.io memory usage in dashboard

### 2025-01-20: Reuse Previous Soil Data Feature

**Status:** PRODUCTION READY - Available to all users with previous entries.

**Feature:** Users can quickly reuse soil chemistry from a previous entry instead of re-entering all values. A "Use previous soil data" link appears in the Data Entry sidebar if the user has submitted samples before.

**User Flow:**
1. User clicks "Use previous soil data" link in sidebar
2. Modal shows their distinct soil profiles (grouped by pH/OM/texture)
3. User selects one and clicks "Apply Soil Data"
4. Form fields are auto-filled with that soil data
5. User selects species and submits as normal

**Implementation:**

1. **Database Functions** (`R/db.R`):
   - `db_get_user_soil_profiles(user_id, limit)` - Gets distinct soil profiles for picker
   - `db_get_soil_data_by_id(entry_id)` - Gets soil chemistry from specific entry

2. **UI Components** (`R/mod_data_entry.R`):
   - `reuse_soil_section` - Shows link + count of saved soil tests
   - Modal with radio buttons for profile selection
   - `apply_reuse` handler fills all form fields

**Files Modified:**
- `app/R/db.R` - Added soil profile query functions
- `app/R/mod_data_entry.R` - Added reuse UI section, modal, and apply handler

**Testing Checklist:**
- [x] Link hidden for users with no previous entries
- [x] Link shows count of saved soil tests
- [x] Modal displays distinct profiles with pH/OM/texture summary
- [x] "Apply Soil Data" fills all soil chemistry fields
- [x] Texture properly switches to class mode when applied
- [x] User can still edit values after applying
