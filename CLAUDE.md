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
│   ├── mod_find_plants.R # Species recommendations (deprecated, replaced by My Garden)
│   └── mod_my_garden.R  # Wildlife dashboard with donut charts
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

Reference tables: `ref_taxon`, `ref_usda_traits` (93,915 entries), `ref_wetland_indicator`, `ref_noxious_invasive` (1,950 records, 50 states), `ref_state_distribution` (408K BONAP records, all 50 states + DC), `user_preferences`

## Environment Variables

Required in `.Renviron`:
```
POSTGRES_HOST, POSTGRES_PORT, POSTGRES_DB, POSTGRES_USER, POSTGRES_PASSWORD
POLISHED_APP_NAME, POLISHED_API_KEY
FIREBASE_API_KEY, FIREBASE_AUTH_DOMAIN, FIREBASE_PROJECT_ID
ADMIN_EMAILS=admin@example.com
# For data collection scripts (optional):
GBIF_USER, GBIF_PWD, GBIF_EMAIL  # GBIF download API
EBIRD_API_KEY                     # eBird API (https://ebird.org/api/keygen)
```

## Branding Guidelines

Reference: `branding/Branding Overview.docx`

### Brand Essence
- **Mission**: "Real soil data for real growing conditions."
- **Tagline**: "Better data from the ground up."
- **Audience**: Gardeners, researchers, restorationists, land managers, educators, and anyone who cares about what's happening underground.
- **Keywords**: Scientific, Organic, Local, Modern, Sustainable
- **The Vibe**: Intersection of "peer-reviewed data" and "running barefoot through the tall grass." Sophisticated, muddy, and unapologetically nerdy about soil.

### Color Palette

| Role | Name | Hex | Usage |
|------|------|-----|-------|
| Primary | Native Sage | #7A9A86 | Primary accent, buttons, success states |
| Accent | Horizon Gold | #D39B35 | CTAs, warning states, highlights |
| Canvas | Limestone | #F7F4E8 | Backgrounds (avoid pure white #FFFFFF) |
| Text | Charcoal | #373D3C | Body text, navbar, headings |

**Accessibility Rules:**
- Charcoal on Limestone = high contrast (good)
- White text ONLY on Charcoal backgrounds
- White text on Sage or Gold = too low contrast (avoid)

### Typography (Brand Standard)

| Role | Font | Usage |
|------|------|-------|
| Brand Voice | Baumans | Logo only, very short marketing phrases (3-5 words max) |
| Manager | Montserrat SemiBold | Headings (H1-H3), nav, buttons |
| Narrative | Rokkitt Light | Body text, captions, sub-logos |
| Data | JetBrains Mono | Species names, numeric values, code |

The app uses Montserrat (headings), Rokkitt Light (body), and JetBrains Mono (data/species names) per brand guidelines.

### Design Rules

1. **No "Radioactive" Greens**: Nature is sage, olive, moss, brown. Keep saturation muted and natural.
2. **Embrace the Brown**: Dormant grasses, dried seed heads, autumn colors are beautiful. Gardens aren't green 365 days.
3. **Macro is King**: Show texture - pollen on stamens, soil crumb structure, root systems. "Close-up of a worm" energy.
4. **Humans are Secondary**: If people appear, they're working (dirty hands, muddy boots). Plants are the main character.
5. **No Generic Stock Photos**: Avoid anything that looks like it belongs on a bag of fertilizer.

### Logo Usage
- **Safe space**: Leave padding around logo (use leaf element as spacing guide)
- **Variations**: Dark version for light backgrounds, light version for dark backgrounds
- Logo files in `branding/` folder (SVG and PNG formats)

## Key Features Status

| Feature | Status |
|---------|--------|
| Data Entry with PDF extraction | Production |
| Analysis with 9 visualization tabs | Production |
| User preferences (zip code in navbar) | Production |
| Native/Introduced badges | Production (state-level via BONAP) |
| State-level invasive badges | Production (50 states) |
| Mobile-responsive layout | Production |
| My Garden wildlife dashboard | Production (needs wildlife ETL + state presence data) |
| Batch plant upload | Dev only |
| Find Plants recommendations | Deprecated (replaced by My Garden) |

## Data Status

### State Invasive Data - COMPLETE
**1,950 records from all 50 states** in `ref_noxious_invasive` table. Invasive badge links to state invasive species info page.

**Key files:**
- `scripts/fetch_state_invasive_lists.R` - Compilation script with all 50 states
- `data/state_invasive_for_import.csv` - Compiled output

### Native Status Data - COMPLETE
- **State level**: BONAP (Biota of North America Program) provides authoritative per-state nativity for all 50 states + DC. 408K records in `ref_state_distribution` with `source='BONAP'`. Covers ~20K species at 90% match rate to `ref_taxon`.
- **NA-level fallback**: USDA Plants `ref_usda_traits.native_status` used when BONAP lacks state data for a species (~10% of flora). Shows "Native to other US states, not recorded in {state}" with USDA source.
- **Tooltips**: Include source attribution, e.g., "Native to Minnesota (BONAP)"

**Key files:**
- `scripts/scrape_bonap_nativity.R` - BONAP TDC scraper (cached TSVs in `data/cache_bonap/`)
- `app/R/etl/bonap_state_dist_etl.R` - ETL to load BONAP data into DB
- `data/bonap_state_nativity_compiled.csv` - Compiled scrape output (160K records)

### Wildlife Data
- **5 tables**: `ref_wildlife_plants` (~463), `ref_wildlife_species` (~2,220), `ref_wildlife_interactions` (~11,246), `ref_specialist_bees_by_genus` (~61), `ref_wildlife_state_presence` (GBIF+eBird state occurrence data)
- **Source**: Proprietary Excel files from consulting business (never committed to git)
- **ETL**: `app/R/etl/wildlife_etl.R` — follows bonap_state_dist_etl.R pattern
- **State presence ETL**: `app/R/etl/wildlife_state_presence_etl.R` — loads GBIF+eBird data
- **Data collection**: `scripts/fetch_gbif_wildlife_presence.R` (Lep+Bees), `scripts/fetch_ebird_wildlife_presence.R` (Birds)
- **Module**: `app/R/mod_my_garden.R` — donut charts showing wildlife coverage per family, state-filtered when presence data available

## Public Site (site/)

Static Astro site for edaphicflora.org (and .com, which redirects), deployed by Netlify from this repo with base directory `site` on the edaphicflora@gmail.com Netlify account. Details in `site/README.md`.

- The site reads ONLY the `public_site` views in `site/sql/public_views.sql`, using the read-only `site_reader` role (`DATABASE_URL` in Netlify). Privacy rules live in those views: flagged and disabled-user samples excluded, no `created_by`, no locations. Every sample shows publicly with its count (ranges at 3+, `public_site.settings`); the Shiny app keeps its own threshold in `helpers.R`. Wildlife is counts and highlight flags only.
- `npm run build` runs `scripts/export-data.mjs` then `astro build`. Without `DATABASE_URL` it uses `src/data/sample/` and shows a sample-data ribbon; a production build without it fails on purpose.
- Work on the `dev` branch (free branch deploys). Merges to `main` are production deploys (15 of 300 free credits each). Weekly refresh (photos + rebuild): `.github/workflows/site-weekly-rebuild.yml`. Scheduled runs only happen once it is on `main`.
- No em dashes in site copy.
- Photos: `site/scripts/fetch-photos.mjs` stores one openly licensed photo per species in `ref_species_photos` (`site/sql/species_photos.sql`), iNaturalist first, Wikimedia Commons fallback, in tiers (soil snapshot, any samples, wildlife plants, natives, rest). Images are hotlinked, never stored in the repo. Locked rows are hand-picked and never overwritten.
- Local preview: `cd site && npm run dev` (reads `site/.env`, see `.env.example`) serves http://localhost:4321 with live reload.
- Contributing on the site: `/contribute/` and `/account/` (Firebase sign-in, same project as the app) call Netlify functions in `site/netlify/functions/`; the `site_writer` login can only call `site_api` functions from `site/sql/contribute.sql`. Existing contributors link to their Polished uid by email via `legacy_users` (loaded by `scripts/load_site_reference.R`). Tests: `npm test` in site/, `Rscript site/test/db/contribute_functions_test.R`.
- CSV downloads: "Download my samples" on `/account/` (`site/netlify/functions/export.mjs` calls `site_api.export_my_samples` in `site/sql/export.sql`: own rows with location and notes, 10 a day, logged as `export`) and the public CC BY-NC 4.0 file `/data/edaphic-flora-community-soil.csv`, built at build time from the species records (one row per species, only what species pages show), linked from `/about/#download`. Shared builder `site/netlify/lib/csv.mjs`. Tests: `test/csv.test.mjs`, `Rscript site/test/db/export_functions_test.R`.
- Moderation: `/admin/` (not linked, noindex) flags/unflags samples, disables/re-enables contributors (optionally flagging all their samples), and lists disabled accounts and the audit log. Admin = verified Firebase email in BOTH `ADMIN_EMAILS` (Netlify, checked in `site/netlify/functions/admin.mjs` before any DB call) and the `site_admins` table (checked by every function). The function connects as the separate `site_admin` login, which can only call `admin_api` functions in `site/sql/admin.sql` (kept apart from `site_writer` so a leaked contribute string can't moderate). Every change writes `audit_log` like the app. Setup and the SQL fallback: "Moderating" in `site/README.md`. Tests: `site/test/admin.test.mjs`, `Rscript site/test/db/admin_functions_test.R`.
- Shiny app is retired at the site launch (shinyapps.io deployment stopped; `app/` code stays in the repo as the reference). Port next, keeping the app's design where it worked: My Garden wildlife dashboard. Admin tools are ported (Moderation above); on a Neon branch where they aren't set up yet, moderate with SQL (`soil_samples.flagged`, `disabled_users`).
- Not in the site yet: editing samples, reuse a previous test, CSV batch upload, Analysis charts.

## Important Conventions
- Brand name is capitalized in text: "Edaphic Flora" (logos/navbar stay lowercase per brand design)
- Admin access requires BOTH Polished admin status AND listing in `ADMIN_EMAILS` env var
- Welcome page action buttons use unnamespaced IDs (handled by app.R observers, not module)
- Don't commit/push until user confirms changes work

## Memory Notes

shinyapps.io free tier has 1GB RAM:
- Ecoregion lookup disabled in production (saves ~95MB)
- Use RDS format for large data files
- Keep cache files at `data/cache_usda/` (project root, outside app/)
