-- Reference schema for Edaphic Flora (USDA / NWPL / CSR)
-- Run as the DB owner/admin role (not the shiny_app runtime user).

BEGIN;

-- Track sources/versions (USDA PLANTS, NWPL 2022, Pierce 2017 CSR, etc.)
CREATE TABLE IF NOT EXISTS ref_source (
  id        SERIAL PRIMARY KEY,
  name      TEXT NOT NULL,
  version   TEXT,
  url       TEXT,
  license   TEXT,
  loaded_at TIMESTAMP DEFAULT now(),
  CONSTRAINT uq_ref_source UNIQUE (name, version)
);

-- Canonical taxa to key all reference data to
CREATE TABLE IF NOT EXISTS ref_taxon (
  id              BIGSERIAL PRIMARY KEY,
  scientific_name TEXT NOT NULL,     -- e.g., 'Solidago lepida'
  author          TEXT,
  family          TEXT,
  usda_symbol     TEXT,              -- PLANTS code if available
  notes           TEXT
);
-- Enforce uniqueness by name, case-insensitive
CREATE UNIQUE INDEX IF NOT EXISTS idx_ref_taxon_name_lower
  ON ref_taxon (lower(scientific_name));

-- USDA PLANTS characteristics: edaphic + climate tolerances
CREATE TABLE IF NOT EXISTS ref_usda_characteristics (
  taxon_id              BIGINT PRIMARY KEY
                        REFERENCES ref_taxon(id) ON DELETE CASCADE,
  source_id             INTEGER REFERENCES ref_source(id),
  ph_min                NUMERIC,
  ph_max                NUMERIC,
  salinity_tolerance    TEXT,        -- None / Low / Medium / High (as given)
  shade_tolerance       TEXT,        -- Intolerant / Intermediate / Tolerant
  drought_tolerance     TEXT,        -- Low / Medium / High
  soil_texture_coarse   BOOLEAN,
  soil_texture_medium   BOOLEAN,
  soil_texture_fine     BOOLEAN,
  precipitation_min_mm  INTEGER,
  precipitation_max_mm  INTEGER,
  min_temp_c            INTEGER
);

-- NWPL: wetland indicator status (OBL/FACW/FAC/FACU/UPL), by region
CREATE TABLE IF NOT EXISTS ref_wetland_indicator (
  taxon_id  BIGINT REFERENCES ref_taxon(id) ON DELETE CASCADE,
  source_id INTEGER REFERENCES ref_source(id),
  region    TEXT NOT NULL,           -- 'National' or USACE region code
  indicator TEXT NOT NULL,           -- 'OBL','FACW','FAC','FACU','UPL'
  PRIMARY KEY (taxon_id, region)
);

-- CSR strategies (e.g., Pierce et al. 2017)
CREATE TABLE IF NOT EXISTS ref_csr_strategy (
  taxon_id   BIGINT  REFERENCES ref_taxon(id) ON DELETE CASCADE,
  source_id  INTEGER REFERENCES ref_source(id),
  c_percent  NUMERIC,                -- 0..100
  s_percent  NUMERIC,
  r_percent  NUMERIC,
  csr_class  TEXT,                   -- e.g., 'C/CS','CSR','SR', etc.
  notes      TEXT,
  PRIMARY KEY (taxon_id, source_id)
);

-- (Optional) future-proof: a synonyms table if we need name mapping later
CREATE TABLE IF NOT EXISTS ref_synonym (
  taxon_id        BIGINT REFERENCES ref_taxon(id) ON DELETE CASCADE,
  synonym_name    TEXT NOT NULL,
  PRIMARY KEY (taxon_id, synonym_name)
);

COMMIT;

-- ---- Minimal runtime privileges for the app user (read-only on refs) ----
-- Run these as the table owner (same role that created tables).
GRANT USAGE ON SCHEMA public TO shiny_app;

GRANT SELECT ON
  ref_source,
  ref_taxon,
  ref_usda_characteristics,
  ref_wetland_indicator,
  ref_csr_strategy,
  ref_synonym
TO shiny_app;

-- Ensure future tables default to readable by shiny_app (optional but handy)
ALTER DEFAULT PRIVILEGES IN SCHEMA public
  GRANT SELECT ON TABLES TO shiny_app;
