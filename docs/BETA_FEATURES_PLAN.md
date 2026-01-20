# Beta Features Implementation Plan

**Created:** 2025-01-20
**Status:** Planning (for implementation before beta release)
**Branch:** Keep in dev during alpha testing

This document outlines three features to reduce data entry friction while maintaining data quality standards.

---

## Feature 1: Duplicate Location Detection

### Problem
Users may accidentally re-enter soil data for locations where they've already submitted samples. This creates redundant data entry and potential inconsistencies.

### Solution
Before submit, check if the user has existing entries within ~50 meters of the new coordinates. Show a modal offering options to reuse existing soil data.

### User Flow

```
User clicks Submit
    ↓
Check for nearby locations (within 50m)
    ↓
If found → Show modal:
┌─────────────────────────────────────────────────────────┐
│  Existing Location Detected                             │
│                                                         │
│  You have 12 samples from this location                 │
│  (last updated March 2024)                              │
│                                                         │
│  ○ Add plants to existing soil data                     │
│    Uses your previous soil test from 3/15/2024          │
│                                                         │
│  ○ Enter new soil test                                  │
│    Submit with the soil data you just entered           │
│                                                         │
│  ○ View existing entries                                │
│    See what you've already recorded here                │
│                                                         │
│  [Cancel]                        [Continue]             │
└─────────────────────────────────────────────────────────┘
    ↓
If "Add plants to existing soil data":
    → Copy soil chemistry from most recent entry
    → Only submit species/outcome/metadata as new
    → Show confirmation with soil data being reused
    ↓
If "Enter new soil test":
    → Proceed with normal submit flow
    ↓
If "View existing entries":
    → Show table of entries at this location
    → Allow user to update outcomes on existing entries
```

### Implementation Details

#### 1. Distance Calculation Function (R/db.R)

```r
#' Find user's entries near a location
#' @param user_id Firebase UID
#' @param lat Latitude
#' @param lon Longitude
#' @param radius_m Search radius in meters (default 50)
#' @return Data frame of nearby entries
db_get_nearby_user_entries <- function(user_id, lat, lon, radius_m = 50) {
  # Haversine distance in PostgreSQL
  # 6371000 = Earth radius in meters
  query <- "
    SELECT id, species, cultivar, outcome, date, created_at,
           location_lat, location_long, ph, organic_matter,
           (6371000 * acos(
             cos(radians($2)) * cos(radians(location_lat)) *
             cos(radians(location_long) - radians($3)) +
             sin(radians($2)) * sin(radians(location_lat))
           )) AS distance_m
    FROM soil_samples
    WHERE created_by = $1
      AND location_lat IS NOT NULL
      AND location_long IS NOT NULL
    HAVING distance_m <= $4
    ORDER BY created_at DESC
  "
  # Note: PostgreSQL doesn't support HAVING without GROUP BY
  # Use subquery or WHERE with the formula instead

  dbGetQuery(pool, query, params = list(user_id, lat, lon, radius_m))
}
```

**Alternative (simpler, works in PostgreSQL):**
```r
db_get_nearby_user_entries <- function(user_id, lat, lon, radius_m = 50) {
  # Approximate: 1 degree ≈ 111km at equator
  # For 50m radius, use ~0.0005 degrees (conservative)
  degree_tolerance <- radius_m / 111000

  query <- "
    SELECT DISTINCT ON (location_lat, location_long)
           id, species, date, created_at, ph, organic_matter,
           location_lat, location_long,
           COUNT(*) OVER (PARTITION BY location_lat, location_long) as entry_count
    FROM soil_samples
    WHERE created_by = $1
      AND location_lat BETWEEN $2 - $4 AND $2 + $4
      AND location_long BETWEEN $3 - $5 AND $3 + $5
    ORDER BY location_lat, location_long, created_at DESC
  "
  dbGetQuery(pool, query, params = list(user_id, lat, degree_tolerance, lon, degree_tolerance * 1.5))
}
```

#### 2. Modal UI (R/mod_data_entry.R)

Add after validation, before insert loop (~line 693):

```r
# Check for nearby locations
nearby <- db_get_nearby_user_entries(
  user_id = current_user()$user_uid,
  lat = input$latitude,
  lon = input$longitude,
  radius_m = 50
)

if (nrow(nearby) > 0) {
  # Show modal and wait for user choice
  showModal(modalDialog(
    title = "Existing Location Detected",
    # ... modal content
    footer = tagList(
      actionButton(ns("loc_cancel"), "Cancel"),
      actionButton(ns("loc_continue"), "Continue", class = "btn-primary")
    )
  ))
  return()  # Exit handler, wait for modal response
}
```

#### 3. Reuse Soil Data Logic

When user selects "Add plants to existing soil data":
- Query most recent entry at that location
- Copy all soil chemistry fields (ph, organic_matter, nutrients, texture)
- Keep user's newly entered species, cultivar, outcome, sun_exposure, hydrology
- Submit as normal

#### 4. Files to Modify

| File | Changes |
|------|---------|
| `R/db.R` | Add `db_get_nearby_user_entries()` function |
| `R/mod_data_entry.R` | Add location check before submit, modal UI, modal handlers |

#### 5. Testing Checklist

- [ ] No nearby entries → normal submit flow
- [ ] Nearby entries found → modal appears
- [ ] "Add plants to existing" → soil data copied correctly
- [ ] "Enter new soil test" → original data submitted
- [ ] "View existing" → shows correct entries
- [ ] Edge case: exact same coordinates
- [ ] Edge case: user has no previous entries

---

## Feature 2: Opt-in Outcome Reminder Emails

### Problem
Users submit soil samples with plant species but may not return to update outcomes (Thriving/Struggling/etc.) after a growing season. This reduces the value of the longitudinal data.

### Solution
Allow users to opt-in to email reminders prompting them to update plant outcomes after 3-6 months.

### User Flow

```
Data Entry Form (after species selection)
    ↓
┌─────────────────────────────────────────────────────────┐
│  ☐ Remind me to update outcomes                         │
│    Get an email in 6 months to record how these         │
│    plants are doing                                     │
└─────────────────────────────────────────────────────────┘
    ↓
On Submit → Store reminder preference
    ↓
After 6 months → Send email:
┌─────────────────────────────────────────────────────────┐
│  Subject: How are your plants doing?                    │
│                                                         │
│  Hi [Name],                                             │
│                                                         │
│  6 months ago, you logged these plants in Edaphic Flora:│
│                                                         │
│  • Acer rubrum 'October Glory' - [Update Outcome]       │
│  • Echinacea purpurea - [Update Outcome]                │
│  • Baptisia australis - [Update Outcome]                │
│                                                         │
│  Take a minute to record how they're doing. Your data   │
│  helps other gardeners make better planting decisions.  │
│                                                         │
│  [Update My Plants →]                                   │
│                                                         │
│  ─────────────────────────────────────────────────────  │
│  Don't want reminders? [Unsubscribe]                    │
└─────────────────────────────────────────────────────────┘
```

### Implementation Details

#### 1. Database Schema

```sql
-- New table for reminder preferences
CREATE TABLE IF NOT EXISTS outcome_reminders (
  id SERIAL PRIMARY KEY,
  user_id TEXT NOT NULL,              -- Firebase UID
  user_email TEXT NOT NULL,           -- For sending reminder
  sample_ids INTEGER[] NOT NULL,      -- Array of soil_samples.id
  remind_at TIMESTAMP NOT NULL,       -- When to send reminder
  sent_at TIMESTAMP,                  -- NULL until sent
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_reminders_remind_at ON outcome_reminders(remind_at)
  WHERE sent_at IS NULL;

-- User preferences table
CREATE TABLE IF NOT EXISTS user_preferences (
  user_id TEXT PRIMARY KEY,
  email TEXT,
  reminder_opt_in BOOLEAN DEFAULT FALSE,
  reminder_months INTEGER DEFAULT 6,  -- 3, 6, or 12
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

#### 2. Email Service Options

**Option A: Brevo (formerly Sendinblue) - Recommended**
- Free tier: 300 emails/day
- R package: `{blastula}` or direct API
- Transactional email support

**Option B: Mailgun**
- Free tier: 5,000 emails/month for 3 months
- Good deliverability

**Option C: Self-hosted (Not Recommended)**
- Requires SMTP server setup
- Deliverability issues

#### 3. Reminder Processing

**Option A: Scheduled R Script (Simple)**
```r
# scripts/send_reminders.R
# Run daily via cron: 0 9 * * * Rscript scripts/send_reminders.R

source("R/db.R")
pool <- db_connect()

# Get reminders due today
due_reminders <- dbGetQuery(pool, "
  SELECT r.*, u.email
  FROM outcome_reminders r
  JOIN user_preferences u ON r.user_id = u.user_id
  WHERE r.remind_at <= NOW()
    AND r.sent_at IS NULL
")

for (reminder in split(due_reminders, seq_len(nrow(due_reminders)))) {
  # Get sample details
  samples <- dbGetQuery(pool,
    "SELECT id, species, cultivar, outcome FROM soil_samples WHERE id = ANY($1)",
    params = list(reminder$sample_ids)
  )

  # Send email via Brevo/Mailgun
  send_outcome_reminder_email(reminder$user_email, samples)

  # Mark as sent
  dbExecute(pool,
    "UPDATE outcome_reminders SET sent_at = NOW() WHERE id = $1",
    params = list(reminder$id)
  )
}
```

**Option B: GitHub Actions (No Server)**
```yaml
# .github/workflows/send-reminders.yml
name: Send Outcome Reminders
on:
  schedule:
    - cron: '0 14 * * *'  # 9 AM EST daily
jobs:
  send:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - run: Rscript scripts/send_reminders.R
```

#### 4. UI Components

**Checkbox in Data Entry Form:**
```r
# In mod_data_entry.R, after species selection
checkboxInput(
  ns("reminder_opt_in"),
  label = tagList(
    "Remind me to update outcomes",
    tags$small(class = "text-muted d-block",
      "Get an email in 6 months to record how these plants are doing"
    )
  ),
  value = FALSE
)
```

**User Preferences Page (future):**
- Reminder frequency: 3 / 6 / 12 months
- Email address confirmation
- Unsubscribe link

#### 5. Files to Create/Modify

| File | Changes |
|------|---------|
| `R/db.R` | Add migration for `outcome_reminders` and `user_preferences` tables |
| `R/mod_data_entry.R` | Add reminder checkbox, save preference on submit |
| `R/email.R` | New file: email sending functions |
| `scripts/send_reminders.R` | New file: daily reminder processor |
| `.github/workflows/send-reminders.yml` | Optional: GitHub Actions cron job |

#### 6. Testing Checklist

- [ ] Checkbox appears in data entry form
- [ ] Reminder record created on submit (when checked)
- [ ] Reminder not created (when unchecked)
- [ ] Email sends correctly (test with personal email)
- [ ] "Update My Plants" link goes to correct page
- [ ] Unsubscribe works
- [ ] Reminder marked as sent after sending

#### 7. Privacy/Legal Considerations

- Clear opt-in language (not pre-checked)
- Easy unsubscribe in every email
- Store minimal PII (just email for reminders)
- Consider adding to privacy policy

---

## Feature 3: Batch Plant Upload

### Problem
Serious gardeners often maintain spreadsheets of all plants on their property. Entering them one-by-one is tedious, especially when they share the same soil test data.

### Solution
Allow CSV/Excel upload of plant lists that pair with PDF soil report extraction for minimal-friction bulk entry.

### User Flow

```
Data Entry Form
    ↓
┌─────────────────────────────────────────────────────────┐
│  📄 Upload soil report (PDF/image)     [Choose File]    │
│  ────────────────────────────────────────────────────── │
│  🌱 Upload plant list (CSV/Excel)      [Choose File]    │
│     Download template                                   │
└─────────────────────────────────────────────────────────┘
    ↓
User uploads soil report → Auto-fills soil chemistry
User uploads plant list → Populates species table
    ↓
Review screen shows:
┌─────────────────────────────────────────────────────────┐
│  Soil Data (from PDF)                                   │
│  pH: 6.2 | OM: 4.1% | Texture: Sandy Loam              │
│                                                         │
│  Plants to Add (from CSV)                    [Edit]     │
│  ┌────────────────────────────────────────────────────┐ │
│  │ Species              │ Cultivar      │ Outcome     │ │
│  ├──────────────────────┼───────────────┼─────────────│ │
│  │ Acer rubrum          │ October Glory │ Thriving    │ │
│  │ Echinacea purpurea   │               │ Established │ │
│  │ Baptisia australis   │ Purple Smoke  │             │ │
│  │ ⚠️ Unknown species   │               │             │ │
│  └────────────────────────────────────────────────────┘ │
│                                                         │
│  ⚠️ 1 species not found in database (will be skipped)   │
│                                                         │
│  [Cancel]                           [Submit 3 Plants]   │
└─────────────────────────────────────────────────────────┘
```

### CSV Template Format

```csv
species,cultivar,outcome,sun_exposure,site_hydrology,inat_url,notes
Acer rubrum,October Glory,Thriving,Full Sun,Mesic,,Planted 2022
Echinacea purpurea,,Established,Part Shade,Dry,,
Baptisia australis,Purple Smoke,,,,,Nice blue flowers
Quercus alba,,,Full Sun,Mesic,,From local nursery
```

**Required columns:** `species`
**Optional columns:** `cultivar`, `outcome`, `sun_exposure`, `site_hydrology`, `inat_url`, `notes`

**Valid values:**
- `outcome`: Thriving, Established, Struggling, Failed/Died, (blank)
- `sun_exposure`: Full Sun, Part Sun, Part Shade, Full Shade, (blank)
- `site_hydrology`: Dry, Mesic, Wet, (blank)

### Implementation Details

#### 1. CSV Parser Function (R/helpers.R)

```r
#' Parse plant list CSV for batch upload
#' @param file_path Path to uploaded CSV file
#' @param species_db Species database for validation
#' @return List with $valid (data frame) and $invalid (character vector)
parse_plant_list <- function(file_path, species_db) {
  # Read CSV
  df <- read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

  # Normalize column names
  names(df) <- tolower(trimws(names(df)))

  # Require species column
 if (!"species" %in% names(df)) {
    return(list(valid = NULL, invalid = NULL, error = "Missing required 'species' column"))
  }

  # Clean species names
  df$species <- trimws(df$species)
  df <- df[!is.na(df$species) & df$species != "", ]

  # Validate against WCVP database
  valid_species <- df$species %in% species_db$taxon_name

  # Validate enum values
  valid_outcomes <- c("Thriving", "Established", "Struggling", "Failed/Died", NA)
  valid_sun <- c("Full Sun", "Part Sun", "Part Shade", "Full Shade", NA)
  valid_hydro <- c("Dry", "Mesic", "Wet", NA)

  if ("outcome" %in% names(df)) {
    df$outcome[!df$outcome %in% valid_outcomes] <- NA
  }
  if ("sun_exposure" %in% names(df)) {
    df$sun_exposure[!df$sun_exposure %in% valid_sun] <- NA
  }
  if ("site_hydrology" %in% names(df)) {
    df$site_hydrology[!df$site_hydrology %in% valid_hydro] <- NA
  }

  list(
    valid = df[valid_species, ],
    invalid = df$species[!valid_species],
    error = NULL
  )
}
```

#### 2. Template Download

```r
# In mod_data_entry.R
output$download_plant_template <- downloadHandler(
  filename = "plant_list_template.csv",
  content = function(file) {
    template <- data.frame(
      species = c("Acer rubrum", "Echinacea purpurea"),
      cultivar = c("October Glory", ""),
      outcome = c("Thriving", ""),
      sun_exposure = c("Full Sun", "Part Shade"),
      site_hydrology = c("Mesic", "Dry"),
      inat_url = c("", ""),
      notes = c("Example entry", "")
    )
    write.csv(template, file, row.names = FALSE)
  }
)
```

#### 3. UI Components

```r
# File upload for plant list
fileInput(
  ns("plant_list_file"),
  label = tagList(
    icon("seedling"), "Upload plant list (CSV)",
    downloadLink(ns("download_plant_template"), "Download template", class = "ms-2 small")
  ),
  accept = c(".csv", ".xlsx"),
  placeholder = "Optional: bulk add plants"
)

# Preview table (shown after upload)
conditionalPanel(
  condition = "output.has_plant_list",
  ns = ns,
  card(
    card_header("Plants to Add"),
    DTOutput(ns("plant_list_preview")),
    uiOutput(ns("plant_list_warnings"))
  )
)
```

#### 4. Integration with Existing Form

**Key consideration:** The current form has per-species metadata inputs that appear dynamically. With batch upload, this UI is replaced by the uploaded data.

```r
# Reactive to track upload state
plant_list_data <- reactiveVal(NULL)

observeEvent(input$plant_list_file, {
  req(input$plant_list_file)

  result <- parse_plant_list(input$plant_list_file$datapath, species_db)

  if (!is.null(result$error)) {
    showNotification(result$error, type = "error")
    return()
  }

  plant_list_data(result)

  # Show warnings for invalid species
  if (length(result$invalid) > 0) {
    showNotification(
      paste(length(result$invalid), "species not found and will be skipped"),
      type = "warning"
    )
  }
})

# Modify submit handler to use plant_list_data() if available
# Instead of looping through input$species, loop through plant_list_data()$valid
```

#### 5. Excel Support (Optional Enhancement)

```r
# Requires: install.packages("readxl")
parse_plant_list <- function(file_path, species_db) {
  ext <- tools::file_ext(file_path)

  if (ext == "xlsx" || ext == "xls") {
    df <- readxl::read_excel(file_path)
  } else {
    df <- read.csv(file_path, stringsAsFactors = FALSE)
  }
  # ... rest of parsing logic
}
```

#### 6. Files to Create/Modify

| File | Changes |
|------|---------|
| `R/helpers.R` | Add `parse_plant_list()` function |
| `R/mod_data_entry.R` | Add file upload UI, preview table, modify submit handler |
| `app/www/plant_list_template.csv` | Static template file (alternative to dynamic generation) |

#### 7. Testing Checklist

- [ ] Template download works
- [ ] CSV upload parses correctly
- [ ] Invalid species flagged with warning
- [ ] Valid enum values accepted
- [ ] Invalid enum values converted to NA (not rejected)
- [ ] Preview table displays correctly
- [ ] Submit creates correct number of records
- [ ] Each record has correct species-specific metadata
- [ ] Works with PDF extraction (soil data) + CSV (plants) combined
- [ ] Works with CSV only (manual soil entry)
- [ ] Excel files parse correctly (if supported)

---

## Implementation Priority

| Priority | Feature | Effort | Impact | Risk |
|----------|---------|--------|--------|------|
| 1 | Duplicate Location Detection | Medium | High | Low |
| 2 | Batch Plant Upload | Medium | High | Low |
| 3 | Outcome Reminders | High | Medium | Medium |

**Recommended order:**
1. **Duplicate Location** - Prevents data quality issues, should be in place before beta
2. **Batch Plant Upload** - High value for power users, pairs well with existing PDF extraction
3. **Outcome Reminders** - Requires email infrastructure, can be added post-beta

---

## Dev Mode Toggle

All three features should be behind a dev mode flag during alpha:

```r
# In app.R or config
BETA_FEATURES <- list(
  duplicate_location_check = Sys.getenv("ENV") == "dev",
  batch_plant_upload = Sys.getenv("ENV") == "dev",
  outcome_reminders = FALSE  # Enable when email infrastructure ready
)

# Usage in modules
if (BETA_FEATURES$duplicate_location_check) {
  # Show duplicate location modal
}
```

This allows testing in dev while keeping production stable during alpha.

---

## Questions to Resolve

1. **Duplicate detection radius:** 50m seems reasonable, but should it be configurable?
2. **Reminder timing:** Default 6 months, but should users choose 3/6/12?
3. **Email service:** Brevo vs Mailgun vs other?
4. **Batch upload limit:** Max plants per upload? (suggest 50-100)
5. **Excel support:** Worth the added dependency?
