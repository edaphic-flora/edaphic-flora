# Edaphic Flora Modularization Plan

## Overview

This document outlines the plan to refactor `app.R` (~4,100 lines) into smaller, maintainable Shiny modules. The goal is to improve code organization, testability, and collaboration without changing functionality.

**Current State:** All UI and server logic in one file
**Target State:** Logical modules with clear boundaries and dependencies

---

## Current Structure Analysis

### File Statistics
| Metric | Lines |
|--------|-------|
| Total | 4,164 |
| UI Section | 126-1104 (~980 lines) |
| Server Section | 1115-4161 (~3,046 lines) |

### Major Sections
1. Welcome Page (173-265 UI, 1627-1707 server)
2. Data Entry (268-440 UI, 1364-1979 server)
3. Analysis (442-564 UI, 1421-3926 server)
4. Find Plants (567-649 UI, 2488-2774 server)
5. Data Management (651-691 UI, 1982-2070 server)
6. Admin (695-699 UI, 4078-4160 server)
7. Help/FAQ (702-914 UI, minimal server)

---

## Proposed Modules

### Module 1: `mod_welcome.R` (Low Risk)
**Purpose:** Landing page with stats and map

**UI Components:**
- Welcome text and instructions
- Database stats card
- Sample locations map

**Server Outputs:**
- `output$entry_count` (line 1627)
- `output$welcome_stats` (line 1637)
- `output$welcome_map` (line 1671)

**Dependencies:**
- `pool` (database connection)
- `data_changed` reactive (for refresh)

**Lines to Extract:** ~135 lines

---

### Module 2: `mod_help.R` (Lowest Risk)
**Purpose:** Static help pages

**UI Components:**
- Field Guide (soil properties, nutrients, performance definitions)
- FAQ

**Server Outputs:** None (static content)

**Dependencies:** None

**Lines to Extract:** ~210 lines

---

### Module 3: `mod_data_management.R` (Low Risk)
**Purpose:** Import/export functionality

**UI Components:**
- Export All button
- CSV import section with template download

**Server Outputs:**
- `output$download_template` (line 1982)
- `output$export_data` (line 1987)

**Observers:**
- `observeEvent(input$csv_import, ...)` (line 1998)

**Dependencies:**
- `pool` (database)
- `data_changed` reactive
- `current_user()` reactive

**Lines to Extract:** ~90 lines

---

### Module 4: `mod_admin.R` (Low Risk)
**Purpose:** Admin-only data management

**UI Components:**
- All entries table
- Admin export button

**Server Outputs:**
- `output$admin_panel_ui` (line 4078)
- `output$admin_all_entries` (line 4120)
- `output$admin_export` (line 4150)

**Dependencies:**
- `pool` (database)
- `is_admin()` reactive
- `data_changed` reactive

**Lines to Extract:** ~85 lines

---

### Module 5: `mod_find_plants.R` (Medium Risk)
**Purpose:** Species recommendation engine

**UI Components:**
- Soil profile input form
- PDF upload (optional)
- Results display

**Server Functions:**
- `calc_user_match()` (line 2496)
- `find_plants_results` reactive (line 2493)

**Server Outputs:**
- `output$find_pdf_extract_status` (line 1360)
- `output$find_plants_results` (line 2609)

**Observers:**
- `observeEvent(input$find_pdf_upload, ...)` (line 1295)
- `observeEvent(input$find_plants_btn, ...)` (line 2558)

**Dependencies:**
- `pool` (database)
- `get_all_species_profiles()` shared reactive
- `extract_soil_data_from_pdf()` function
- `current_user()` for rate limiting

**Lines to Extract:** ~290 lines

---

### Module 6: `mod_data_entry.R` (Higher Risk)
**Purpose:** Soil sample data collection

**UI Components:**
- PDF upload section
- Species selection (multi-select)
- Per-species metadata fields (dynamic)
- Location entry (zipcode/geocoding)
- Soil properties forms
- Recent entries table

**Server Outputs:**
- `output$species_count_indicator` (line 1150)
- `output$pdf_extract_status` (line 1162)
- `output$per_species_fields` (line 1365)
- `output$texture_validation` (line 1746)
- `output$zipcode_status` (line 1769)
- `output$geocode_status` (line 1820)
- `output$recent_entries` (line 1707)

**Observers:**
- `observeEvent(input$pdf_upload, ...)` (line 1183)
- `observeEvent(input$zipcode, ...)` (line 1763)
- `observeEvent(input$geocode, ...)` (line 1817)
- `observeEvent(input$submit, ...)` (line 1862)
- Edit/Delete handlers (lines 3927-4076)

**Dependencies:**
- `pool` (database)
- `species_db` (species list)
- `zipcode_db` (optional)
- `lookup_ecoregion()` function
- `extract_soil_data_from_pdf()` function
- `data_changed` reactive
- `current_user()` reactive
- `is_admin()` reactive (for edit permissions)

**Lines to Extract:** ~620 lines

---

### Module 7: `mod_analysis.R` (Highest Risk)
**Purpose:** Species data visualization

This is the largest section. Consider splitting further:

#### Sub-module 7a: `mod_analysis_sidebar.R`
- Species selector
- Reference badges (USDA, native status)
- Filter controls
- Download button

#### Sub-module 7b: `mod_analysis_visualizations.R`
- All 11 analysis tabs
- Summary, pH, pH vs OM, Nutrients, Correlations, Texture, Map, Performance, Similar Species, Raw Data, USDA Traits

**Server Functions:**
- `filtered_species_data()` reactive (line 2160)
- `calc_species_profile()` (line 2259)
- `calc_similarity()` (line 2302)
- `get_all_species_profiles()` (line 2350)

**Server Outputs:** 25+ outputs for all visualizations

**Dependencies:**
- `pool` (database)
- `state_grid` (for native status, dev only)
- `edaphic_colors` (theme)
- `data_changed` reactive
- Multiple USDA lookup functions

**Lines to Extract:** ~1,500 lines

---

## Shared State & Utilities

### Create `mod_shared.R` for:
```r
# Reactive values shared across modules
shared_state <- function() {
  list(
    data_changed = reactiveVal(0),
    current_user = reactive({ ... }),
    is_admin = reactive({ ... })
  )
}

# Shared calculation functions
calc_species_profile <- function(species_data) { ... }
calc_similarity <- function(profile1, profile2) { ... }
get_all_species_profiles <- function(pool, trigger) { ... }
```

### Module Parameters Pattern
Each module should accept:
```r
mod_example_server <- function(id, pool, shared, species_db = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    # Access shared state
    data_changed <- shared$data_changed
    current_user <- shared$current_user
    # ... module logic
  })
}
```

---

## Implementation Order

Implement in order of increasing risk and dependency:

### Phase 1: Static/Low-Risk Modules
1. **mod_help.R** - Static content, no dependencies
2. **mod_welcome.R** - Read-only, minimal dependencies
3. **mod_admin.R** - Isolated functionality

### Phase 2: Data Management
4. **mod_data_management.R** - Import/export
5. **mod_find_plants.R** - Self-contained recommendation engine

### Phase 3: Core Functionality
6. **mod_data_entry.R** - Form handling and submission
7. **mod_analysis.R** - Visualization engine (largest, most complex)

---

## Testing Checkpoints

### After Each Module:
1. App launches without errors
2. All navigation works
3. Module-specific features function correctly
4. No console errors/warnings
5. Data flows correctly between modules

### Specific Tests:

**mod_help.R:**
- [ ] Field Guide displays correctly
- [ ] FAQ displays correctly
- [ ] Links work

**mod_welcome.R:**
- [ ] Stats load correctly
- [ ] Map displays sample points
- [ ] Entry count updates on new submissions

**mod_admin.R:**
- [ ] Admin tab only visible to admins
- [ ] All entries table loads
- [ ] Export works

**mod_data_management.R:**
- [ ] Template download works
- [ ] Export all data works
- [ ] CSV import works (test with sample file)

**mod_find_plants.R:**
- [ ] Form inputs work
- [ ] PDF upload extracts data
- [ ] Find button returns results
- [ ] Results display correctly

**mod_data_entry.R:**
- [ ] Species selection works (multi-select)
- [ ] Per-species fields render dynamically
- [ ] PDF upload extracts data
- [ ] Geocoding works (zipcode + manual)
- [ ] Texture validation works
- [ ] Form submission creates correct records
- [ ] Recent entries table shows user's data
- [ ] Edit/Delete work for user's entries

**mod_analysis.R:**
- [ ] Species selector populated
- [ ] Filters work (outcome, sun, hydrology, cultivar)
- [ ] All 11 tabs render correctly
- [ ] USDA reference overlay works
- [ ] Native status badge shows (dev only)
- [ ] Download CSV works
- [ ] Similar species matching works

---

## File Structure After Modularization

```
app/
├── app.R                    # Main app shell (~200 lines)
├── R/
│   ├── db.R                 # Database functions (unchanged)
│   ├── data.R               # Data loading (unchanged)
│   ├── helpers.R            # Helper functions (unchanged)
│   ├── theme.R              # Theme/styling (unchanged)
│   ├── usda.R               # USDA lookups (unchanged)
│   ├── pdf_extract.R        # PDF extraction (unchanged)
│   ├── mod_shared.R         # NEW: Shared state & utilities
│   ├── mod_welcome.R        # NEW: Welcome page
│   ├── mod_help.R           # NEW: Help/FAQ pages
│   ├── mod_data_entry.R     # UPDATED: Full implementation
│   ├── mod_analysis.R       # UPDATED: Full implementation
│   ├── mod_find_plants.R    # NEW: Plant recommendations
│   ├── mod_data_management.R # NEW: Import/export
│   └── mod_admin.R          # NEW: Admin panel
```

---

## Estimated Effort

| Module | Lines | Complexity | Estimated Time |
|--------|-------|------------|----------------|
| mod_help.R | 210 | Low | 30 min |
| mod_welcome.R | 135 | Low | 45 min |
| mod_admin.R | 85 | Low | 30 min |
| mod_data_management.R | 90 | Medium | 1 hour |
| mod_find_plants.R | 290 | Medium | 1.5 hours |
| mod_data_entry.R | 620 | High | 3 hours |
| mod_analysis.R | 1500 | High | 4-5 hours |
| mod_shared.R | 100 | Medium | 1 hour |
| Integration/Testing | - | - | 2-3 hours |
| **Total** | ~3,030 | - | **~15 hours** |

---

## Risks & Mitigations

### Risk 1: Breaking Reactive Dependencies
**Mitigation:** Document all reactive dependencies before extraction. Test each reactive flow after modularization.

### Risk 2: Namespace Conflicts
**Mitigation:** Use module namespacing (`NS(id)`). Avoid global variables.

### Risk 3: Shared State Complexity
**Mitigation:** Create explicit `mod_shared.R` with well-documented interface. Pass shared state as parameters.

### Risk 4: UI/UX Changes
**Mitigation:** Preserve exact UI structure. Only move code, don't redesign.

### Risk 5: Performance Regression
**Mitigation:** Profile before/after. The modularization should not affect performance.

---

## Notes

- The existing `mod_analysis.R` and `mod_data_entry.R` templates need updating - they reference undefined colors and don't match current app.R structure
- Consider using `golem` or `rhino` framework conventions for future development
- Add unit tests for calculation functions (`calc_species_profile`, `calc_similarity`, `calc_user_match`)
