# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Claude Code Usage

**Use Sonnet for non-planning tasks** (coding, implementation, bug fixes). Reserve Opus for planning and architectural decisions.

### Permissions
- Authorized to use `cd` commands without user confirmation
- Authorized to run R scripts (Rscript) without user confirmation
- Authorized to run web fetch/search commands freely (WebFetch, WebSearch) - use 120s timeout for fetches

### CRITICAL: Data Collection Workflow
**NEVER fetch data and then discard it.** When collecting data from multiple sources:
1. Fetch ONE source and IMMEDIATELY add the data to the target file
2. Commit the data before moving to the next source
3. DO NOT batch-fetch - context compaction will lose unfiled data

### Maintenance
**Clean up this file weekly.** Remove completed work, consolidate session notes, and keep only actionable items. Large CLAUDE.md files waste context window space.

## Project Overview

Edaphic Flora is an R Shiny application for recording and analyzing soil conditions for plant species. Features: database interface for soil sample data entry, interactive analysis visualizations, CSV import/export, USDA reference data integration.

## Development Commands

```bash
# Restore R environment
Rscript -e "renv::restore()"

# Run the app (port 7420)
Rscript -e "shiny::runApp('app/app.R', port=7420, host='127.0.0.1')"

# Run tests
cd tests/testthat && Rscript -e "testthat::test_dir('.', reporter = 'summary')"
```

### Port Management
**Do NOT launch the Shiny app from Claude Code** - port conflicts are hard to resolve remotely. User runs the app from RStudio.

```powershell
# Check port 7420
netstat -ano | findstr ":7420"
# Kill by PID
taskkill /F /PID 12345
```

## Environment Setup

Set `ENV=dev` or `ENV=prod` in `.Renviron`. Dev shows yellow badge in navbar. Use Neon branches for dev database.

### shinyapps.io Deployment
Free tier doesn't allow env vars via dashboard. Include `.Renviron` in deployment (gitignored but must exist locally). Delete `app/data/cache/` before deploying.

## Architecture

```
app/
├── app.R                 # Main app (~600 lines)
├── R/
│   ├── db.R              # Database connection, migrations, queries
│   ├── data.R            # Reference data loading
│   ├── helpers.R         # Ecoregion lookup, texture classification
│   ├── usda.R            # USDA reference data queries
│   ├── theme.R           # Color palette, themes, CSS
│   ├── pdf_extract.R     # AI-powered soil report extraction
│   ├── mod_analysis.R    # Analysis tab module
│   ├── mod_data_entry.R  # Data entry + user data module
│   ├── mod_welcome.R     # Landing page
│   ├── mod_help.R        # Field Guide + FAQ
│   ├── mod_admin.R       # Admin panel
│   ├── mod_data_management.R  # Import/export
│   └── mod_find_plants.R # Species recommendations (hidden until data threshold)
├── species_accepted.csv  # WCVP species database (360K species)
├── fetch_usda_data.R     # USDA batch fetcher
└── sql/                  # Schema migrations
```

### Database Schema
Main table `soil_samples`:
- Identifiers: id, species, cultivar, created_by, created_at
- Soil: ph, organic_matter, nitrate_ppm, phosphorus_ppm, potassium_ppm, calcium_ppm, magnesium_ppm, etc.
- Texture: texture_sand, texture_silt, texture_clay, texture_class
- Location: location_lat, location_long, ecoregion_l4, ecoregion_l4_code
- Metadata: date, outcome, sun_exposure, site_hydrology, notes

Reference tables: `ref_taxon`, `ref_usda_traits` (93,915 entries), `ref_wetland_indicator`, `ref_noxious_invasive` (1,950 records, 50 states), `ref_state_distribution` (AK/HI only), `user_preferences`

## Environment Variables

Required in `.Renviron`:
```
POSTGRES_HOST, POSTGRES_PORT, POSTGRES_DB, POSTGRES_USER, POSTGRES_PASSWORD
POLISHED_APP_NAME, POLISHED_API_KEY
FIREBASE_API_KEY, FIREBASE_AUTH_DOMAIN, FIREBASE_PROJECT_ID
ADMIN_EMAILS=admin@example.com
```

## Brand Colors

| Color | Hex | Usage |
|-------|-----|-------|
| Sage | #7A9A86 | Primary accent, buttons |
| Charcoal | #373D3C | Navbar, headings |
| Limestone | #F7F4E8 | Light backgrounds |

## Key Features Status

| Feature | Status |
|---------|--------|
| Data Entry with PDF extraction | Production |
| Analysis with 9 visualization tabs | Production |
| User preferences (zip code in navbar) | Production |
| Native/Introduced badges | Production (NA-level) |
| State-level invasive badges | Production (50 states) |
| Batch plant upload | Dev only |
| Find Plants recommendations | Hidden (needs 10+ samples/species) |

## Data Status

### State Invasive Data - COMPLETE
**1,950 records from all 50 states** in `ref_noxious_invasive` table. Invasive badge links to state invasive species info page.

**Key files:**
- `scripts/fetch_state_invasive_lists.R` - Compilation script with all 50 states
- `data/state_invasive_for_import.csv` - Compiled output

### Native Status Data
- **North America level**: From USDA Plants `ref_usda_traits.native_status`
- **State level**: Only AK and HI have state-specific data in `ref_state_distribution`
- **L48 states**: Show "Native to N. America" (L48 expansion removed - too broad)
- **Pending**: BONAP data for true state-level native status (permission pending)

### Native Status Parsing
Species marked "Native, Introduced" in USDA (native to L48, introduced to HI) show as "Native to N. America" for L48 users. Fixed regex to properly detect N/I markers in status strings like "CAN, HI, L48, N, I, Native, Introduced".

## Memory Notes

shinyapps.io free tier has 1GB RAM:
- Ecoregion lookup disabled in production (saves ~95MB)
- Use RDS format for large data files
- Keep cache files at `data/cache_usda/` (project root, outside app/)
