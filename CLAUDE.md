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

All 6 modules have been extracted from app.R:

| Module | Lines | Description |
|--------|-------|-------------|
| **mod_help.R** | 309 | Field Guide + FAQ |
| **mod_welcome.R** | 193 | Landing page with stats and map |
| **mod_admin.R** | 114 | Admin-only data management |
| **mod_data_management.R** | 223 | Export and CSV import |
| **mod_find_plants.R** | 499 | Species recommendation engine (hidden until data threshold met) |
| **mod_data_entry.R** | 1,307 | Soil sample data collection + user data management (Recent/All My Data tabs) |
| **mod_analysis.R** | 1,733 | Species data visualization |
| **app.R** | 600 | Core app (routing, auth, edit handlers) |

Total: ~4,978 lines (previously 7 modules, mod_my_data.R merged into mod_data_entry.R)

### UI Structure

The app uses `page_navbar` with these main sections:
1. **Welcome**: Landing page with app overview, stats, and help links
2. **Data Entry**: Sidebar form with accordion sections + tabbed data view:
   - **Recent**: Last 10 entries from all users with edit/delete buttons for own entries
   - **All My Data**: Full user data with filters, stats, bulk delete, and CSV export
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

### Current State (Completed 2025-01-22)
- **Database**: 93,915 entries in `ref_usda_traits` table (Neon Postgres) - queried at runtime
- **Local cache**: JSON backups of API responses (~26,000 files in `data/cache_usda/`) - not used by app, only for debugging/reprocessing
- **ETL script**: `fetch_usda_data.R` handles fetching, caching, and database insertion
- **Fetch tracking**: `ref_usda_traits.fetch_status` column tracks success/no_data/failed per species

### Database Tables
- `ref_taxon`: Species taxonomy (usda_symbol, scientific_name)
- `ref_usda_traits`: 93,915 entries - soil/climate preferences (pH, tolerances, precipitation, temperature, growth habit, etc.)
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

### USDA Data Expansion (Completed 2025-01-22)
All available USDA species data has been fetched. See Session Notes for details.

**Cache file note**: The `.rconnectignore` file is unreliable. Keep cache files outside the `app/` folder (at `data/cache_usda/`) to prevent deployment issues. Delete `app/data/cache/` before deploying if it exists.

### Location Geocoding Fallback Improvement
When street address geocoding fails or returns inaccurate coordinates:
1. Show a brief error message indicating the street wasn't found
2. Fall back to zip code centroid coordinates (currently falls back to vague/inaccurate coords)
3. Consider showing "(approximate)" indicator when using zip centroid vs street-level precision

### User Data History & Management (Completed 2025-01-20)
Implemented as "All My Data" subtab within Data Entry. See Session Notes for 2025-01-20.

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

### 2025-01-20: Batch Plant Upload Feature

**Status:** DEV ONLY - Available when ENV=dev. Enable for production by setting `batch_plant_upload = TRUE` in BETA_FEATURES.

**Feature:** Users can upload a CSV file with multiple plants instead of entering them one-by-one. All plants share the same soil data from the form.

**User Flow:**
1. User uploads a CSV file with plant data
2. Preview table shows valid plants and any that were skipped (invalid species)
3. Species dropdown remains visible - user can add more plants manually
4. User fills in soil data, location, etc.
5. On submit, all plants from CSV + manual selections are created with shared soil data
6. CSV data and manual selections are cleared after successful submit

**CSV Template Format:**
```csv
species,cultivar,outcome,sun_exposure,site_hydrology,inat_url,notes
Acer rubrum,October Glory,Thriving,Full Sun,Mesic,,Planted 2022
Echinacea purpurea,,Established,Part Shade,Dry,,
```

**Valid Values:**
- `outcome`: Thriving, Established, Struggling, Failed/Died (case-insensitive)
- `sun_exposure`: Full Sun, Part Sun, Part Shade, Full Shade
- `site_hydrology`: Dry, Mesic, Wet

**Implementation:**

1. **Parser Function** (`R/helpers.R`):
   - `parse_plant_list(file_path, species_db, max_rows)` - Parses CSV/Excel files
   - Validates species against WCVP database
   - Normalizes column names and enum values
   - Returns list with $valid, $invalid, $error

2. **UI Components** (`R/mod_data_entry.R`):
   - File upload input with template download link
   - Preview table showing plants ready to submit
   - Clear list button to reset
   - Species input hidden when CSV is active

3. **Beta Feature Flag** (`app.R`):
   - `BETA_FEATURES$batch_plant_upload` - Set to `is_dev` (dev mode only)

**Files Modified:**
- `app/R/helpers.R` - Added `parse_plant_list()` function
- `app/R/mod_data_entry.R` - Added upload UI, preview, handlers, modified submit
- `app/app.R` - Updated BETA_FEATURES config

**Testing Checklist:**
- [ ] Template download works
- [ ] CSV upload shows preview table
- [ ] Invalid species shown as skipped with warning
- [ ] Valid species count displayed
- [ ] Clear list button works
- [ ] Manual species input hidden when CSV loaded
- [ ] Submit creates correct records with soil data
- [ ] Per-plant metadata (cultivar, outcome, etc.) saved correctly
- [ ] CSV notes combined with form notes
- [ ] Plant list cleared after successful submit
- [ ] Works with PDF extraction (soil) + CSV (plants) together

### 2025-01-20: Combined Data Entry + My Data Tabs

**Status:** COMPLETE - The separate "My Data" tab has been merged into the Data Entry tab.

**Changes:**
- Data Entry tab now uses `navset_card_tab` with two subtabs:
  - **Recent**: Shows last 10 entries from all users (same as before)
  - **All My Data**: Full user data management with filters, stats, bulk delete, and CSV export
- Removed standalone `mod_my_data.R` module
- Removed `myDataUI` and `myDataServer` calls from app.R

**Features in "All My Data" subtab:**
- Stats summary (total entries, species count, success rate)
- Species search filter
- Outcome filter dropdown
- Bulk selection and delete
- CSV export of user's data
- Edit/delete buttons per row (triggers global handlers in app.R)

**Files Modified:**
- `app/R/mod_data_entry.R` - Added All My Data UI and server logic
- `app/app.R` - Removed My Data module references
- `app/R/mod_my_data.R` - Deleted (functionality merged)

**User Experience:**
- Fewer top-level tabs = simpler navigation
- All data entry and personal data management in one place
- Users can easily switch between adding new entries and viewing/managing existing data

### 2025-01-22: Alpha Deployment Fixes

**Status:** DEPLOYED - Alpha version updated on shinyapps.io

**Changes Made:**

1. **Removed Find Plants/Similar Species from public docs**
   - These features are hidden in alpha (require 10+ samples per species threshold)
   - Removed references from Welcome page and FAQ
   - Feature code remains intact in `mod_find_plants.R` and `mod_analysis.R` for future re-enable

2. **Fixed deployment bundle size issue**
   - Moved `app/data/cache/` to `data/cache_usda/` (project root, outside app/)
   - 18,510 USDA cache files were exceeding shinyapps.io 10,000 file limit
   - Added `app/.rconnectignore` to exclude cache, raw data, and ETL scripts

3. **Updated alpha banner for private repo**
   - Removed GitHub issue links (repo is private)
   - Changed feedback email to edaphicflora@gmail.com
   - Added desktop optimization notice: "Optimized for desktop — mobile viewing may be limited"

4. **Simplified Help page footer**
   - Removed Privacy/Terms/GitHub links
   - Kept only "Send Feedback | Support" links

5. **Created alpha feedback tracking**
   - New `feedback/` folder (gitignored, local only)
   - `ALPHA_FEEDBACK.md` for tracking tester feedback, bugs, and feature requests

**Files Modified:**
- `app/R/mod_welcome.R` - Removed Find Plants section, updated alpha banner
- `app/R/mod_help.R` - Removed FAQ entries, simplified footer
- `app/.rconnectignore` - New file for deployment exclusions
- `.gitignore` - Added feedback folder

**Next Session:**
- Monitor alpha tester feedback in `feedback/ALPHA_FEEDBACK.md`
- Re-enable Find Plants/Similar Species once data threshold is met

### 2025-01-22: USDA Data Scraping Complete

**Status:** COMPLETE - All available USDA data has been fetched and stored in the database.

**Summary:**
- Successfully scraped all fetchable species from USDA PLANTS API
- **93,915 entries** stored in `ref_usda_traits` table (Neon Postgres)
- App queries database at runtime - cache files are backups only, not used by app

**Cache Locations (backups only, not needed for deployment):**
| Location | Files | Purpose |
|----------|-------|---------|
| `data/cache_usda/` | ~19,800 | Backup JSON responses at project root |
| `app/data/cache/usda_char/` | ~6,200 | Old cache location (can be deleted) |

**Deployment Notes:**
- `.rconnectignore` is unreliable - don't depend on it
- **Before deploying**: Delete `app/data/cache/` folder to avoid bundle size issues
- Cache files are NOT needed - all data is in the database

**Retry Commands (if needed in future):**
```r
setwd("app")
source("fetch_usda_data.R")
usda_stats()                              # Check progress
fetch_usda_batch(limit=100, retry_errors=TRUE)  # Retry failures
```

**Fetch Status Values:**
- `success` - Has USDA trait data
- `no_data` - Species exists in USDA but has no traits
- `api_error`, `profile_failed`, `no_plant_id` - Failed (can retry)

### 2025-01-22: Native/Introduced Species Badges

**Status:** IMPLEMENTED - Badges now show on Analysis page for all species with USDA data.

**Features:**
- **Native badge** (green): Shows "Native to N. America" for species native to any North American region
- **Introduced badge** (yellow): Shows "Introduced" for species not native to North America
- **Both badge** (blue): Shows "Native & Introduced" for species that are native in some regions, introduced in others
- **Hover tooltips**: Shows which specific regions (L48, Alaska, Hawaii, Canada, etc.)

**Implementation:**
- `parse_native_status_na()` in `R/usda.R` - Parses the `native_status` field from `ref_usda_traits`
- `get_native_status_na()` in `R/usda.R` - Cached lookup for species native status
- Updated `native_status_badge` in `R/mod_analysis.R` - Now works in production (removed DEV-only restriction)

**Data source:** `ref_usda_traits.native_status` field (e.g., "L48, N, Native" or "CAN, L48, I, Introduced")

**Region codes:**
- L48 = Lower 48 US States
- AK = Alaska
- HI = Hawaii
- CAN = Canada
- PR = Puerto Rico
- VI = US Virgin Islands

**Future planned:** Invasive species badge (requires state-level invasive species data - see `feedback/ALPHA_FEEDBACK.md`)

**Additional updates (same session):**
- Moved badge from sidebar to Summary page (more prominent placement with full tooltip text visible)
- Changed "Drought" to "Drought Tol." in USDA reference chips for clarity
- Created `scripts/insert_test_data_introduced.R` with test data for:
  - Ailanthus altissima (Tree of Heaven) - 4 samples
  - Lonicera japonica (Japanese Honeysuckle) - 4 samples
  - Pyrus calleryana (Callery Pear) - 3 samples
  - Miscanthus sinensis (Chinese Silver Grass) - 3 samples

### 2025-01-22: User Preferences & State-Level Species Status

**Status:** IMPLEMENTED - User preferences infrastructure and state-aware species badges.

**Features:**

1. **User Preferences System**
   - New `user_preferences` table stores user's home zip code and derived location data
   - Gear icon in navbar opens preferences modal
   - Users can set their home zip code to see state-specific species information
   - Zip code auto-resolves to city, state, and coordinates via existing zipcode_db

2. **State-Specific Native Status Badges (Analysis Summary)**
   - When user has set home location: Shows "Native to [State]" or "Introduced in [State]"
   - When no preferences set: Falls back to "Native to N. America" with prompt to set location
   - State-specific data requires `ref_state_distribution` table (currently empty - data ETL needed)
   - Graceful fallback to North America-level when state data unavailable

3. **Invasive/Noxious Species Badges (Analysis Summary)**
   - Red badge: "Federal Noxious Weed" or state-level designation in user's state
   - Orange badge: "Invasive in other US states" when species is listed elsewhere
   - Requires `ref_noxious_invasive` table (currently empty - data ETL needed)
   - Tooltip shows which states the species is listed in

4. **Data Entry Location Auto-Fill**
   - "Use this location" link in Location accordion when user has saved preferences
   - One-click fills zip code, city, state, and coordinates from saved location
   - Saves time for users entering multiple samples from the same location

**Database Tables Added:**
```sql
-- User preferences (home location for native status lookups)
CREATE TABLE user_preferences (
    user_id TEXT PRIMARY KEY,
    home_zipcode VARCHAR(10),
    home_state VARCHAR(2),
    home_city TEXT,
    home_lat NUMERIC(10,6),
    home_long NUMERIC(10,6),
    created_at TIMESTAMPTZ DEFAULT now(),
    updated_at TIMESTAMPTZ DEFAULT now()
);

-- Noxious/invasive species reference
CREATE TABLE ref_noxious_invasive (
    id SERIAL PRIMARY KEY,
    taxon_id INTEGER REFERENCES ref_taxon(id),
    state_code VARCHAR(2),           -- NULL for federal
    designation TEXT NOT NULL,       -- 'Federal Noxious', 'State Noxious', 'Invasive'
    source TEXT,
    source_url TEXT,
    updated_at TIMESTAMPTZ DEFAULT now(),
    UNIQUE(taxon_id, state_code, designation)
);
```

**New Functions (R/db.R):**
- `db_get_user_prefs(user_id, pool)` - Get user's saved preferences
- `db_set_user_prefs(user_id, zipcode, city, state, lat, lon, pool)` - Save preferences (upsert)
- `db_clear_user_prefs(user_id, pool)` - Clear user's preferences

**New Functions (R/usda.R):**
- `get_invasive_status(gs_name, user_state, pool)` - Check invasive/noxious status with state fallback
- `get_native_status_for_user(gs_name, user_prefs, pool)` - State-specific native status with NA fallback

**Files Modified:**
- `app/R/db.R` - Added tables and CRUD functions
- `app/R/usda.R` - Added invasive lookup and state-specific native functions
- `app/R/mod_analysis.R` - Updated badges to use state-specific info
- `app/R/mod_data_entry.R` - Added "Use saved location" feature
- `app/app.R` - Added preferences modal and handlers, passed user_prefs to modules

**Data ETL Needed:**
To fully enable state-level features, data needs to be loaded into:
1. `ref_state_distribution` - USDA state distribution data (for state-specific native status)
2. `ref_noxious_invasive` - USDA federal/state noxious weed lists

**Data Sources:**
- Federal Noxious: https://adminplants.sc.egov.usda.gov/java/noxious?rptType=Federal
- State Noxious: https://adminplants.sc.egov.usda.gov/java/noxComposite
- Invasive: Invasive Plant Atlas (invasiveplantatlas.org) - may need scraping

**Testing Checklist:**
- [ ] Gear icon visible in navbar
- [ ] Preferences modal opens and shows current settings
- [ ] Zip code lookup shows city/state preview
- [ ] Save preferences persists to database
- [ ] Clear preferences removes saved location
- [ ] Analysis page shows state-specific badge when prefs set
- [ ] Analysis page shows "Set location" prompt when no prefs
- [ ] Data Entry shows "Use this location" when prefs set
- [ ] Clicking "Use this location" fills form fields
