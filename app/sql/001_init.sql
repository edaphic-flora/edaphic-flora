-- soil_samples + sample_files (reference DDL)
CREATE TABLE IF NOT EXISTS soil_samples (
  id SERIAL PRIMARY KEY,
  species VARCHAR(255),
  cultivar VARCHAR(255),
  ph NUMERIC(4,2),
  organic_matter NUMERIC(5,2),
  nitrate_ppm NUMERIC,
  ammonium_ppm NUMERIC,
  phosphorus_ppm NUMERIC,
  potassium_ppm NUMERIC,
  calcium_ppm NUMERIC,
  magnesium_ppm NUMERIC,
  soluble_salts_ppm NUMERIC,
  texture_sand NUMERIC(5,2),
  texture_silt NUMERIC(5,2),
  texture_clay NUMERIC(5,2),
  texture_class VARCHAR(50),
  location_lat NUMERIC(10,6),
  location_long NUMERIC(10,6),
  date DATE,
  ecoregion_l4 VARCHAR(255),
  ecoregion_l4_code VARCHAR(50),
  notes TEXT,
  created_by VARCHAR(255),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS sample_files (
  id SERIAL PRIMARY KEY,
  sample_id INTEGER REFERENCES soil_samples(id),
  filename VARCHAR(255),
  file_type VARCHAR(50),
  file_size INTEGER,
  storage_path VARCHAR(512),
  mime_type VARCHAR(120),
  bucket VARCHAR(120),
  object_key VARCHAR(512),
  session_token VARCHAR(128),
  uploader_user_id VARCHAR(255),
  version_id VARCHAR(255),
  visibility VARCHAR(20) DEFAULT 'private',
  upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_samples_species ON soil_samples(species);
CREATE INDEX IF NOT EXISTS idx_samples_date    ON soil_samples(date);

-- =============================================================================
-- USDA Reference Data Tables
-- =============================================================================

-- Reference taxonomy (USDA PLANTS accepted names)
CREATE TABLE IF NOT EXISTS ref_taxon (
  id SERIAL PRIMARY KEY,
  usda_symbol VARCHAR(20) UNIQUE,
  scientific_name VARCHAR(255) NOT NULL,
  common_name VARCHAR(255),
  family VARCHAR(100),
  genus VARCHAR(100),
  species VARCHAR(100),
  variety VARCHAR(100),
  subspecies VARCHAR(100),
  forma VARCHAR(100),
  hybrid BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_ref_taxon_symbol ON ref_taxon(usda_symbol);
CREATE INDEX IF NOT EXISTS idx_ref_taxon_scientific ON ref_taxon(scientific_name);
CREATE INDEX IF NOT EXISTS idx_ref_taxon_genus ON ref_taxon(genus);

-- Synonyms mapping (USDA synonyms -> accepted taxon)
CREATE TABLE IF NOT EXISTS ref_synonym (
  id SERIAL PRIMARY KEY,
  taxon_id INTEGER REFERENCES ref_taxon(id) ON DELETE CASCADE,
  synonym_name VARCHAR(255) NOT NULL,
  synonym_symbol VARCHAR(20),
  source VARCHAR(50) DEFAULT 'USDA',
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_ref_synonym_taxon ON ref_synonym(taxon_id);
CREATE INDEX IF NOT EXISTS idx_ref_synonym_name ON ref_synonym(synonym_name);

-- USDA plant characteristics/traits
CREATE TABLE IF NOT EXISTS ref_usda_traits (
  id SERIAL PRIMARY KEY,
  taxon_id INTEGER UNIQUE REFERENCES ref_taxon(id) ON DELETE CASCADE,
  usda_symbol VARCHAR(20),

  -- Growth characteristics
  duration VARCHAR(50),           -- Annual, Biennial, Perennial
  growth_habit VARCHAR(100),      -- Forb/herb, Graminoid, Shrub, Tree, etc.
  native_status VARCHAR(50),      -- Native, Introduced, Both
  usda_group VARCHAR(100),        -- Dicot, Monocot, Gymnosperm, etc.

  -- Soil preferences
  soil_ph_min NUMERIC(4,2),
  soil_ph_max NUMERIC(4,2),
  soil_texture_adapted TEXT,      -- Coarse, Medium, Fine

  -- Climate preferences
  precip_min_in NUMERIC(6,2),
  precip_max_in NUMERIC(6,2),
  temp_min_f INTEGER,

  -- Tolerances (scale varies: None, Low, Medium, High)
  moisture_use VARCHAR(20),
  shade_tolerance VARCHAR(20),
  drought_tolerance VARCHAR(20),
  salinity_tolerance VARCHAR(20),
  caco3_tolerance VARCHAR(20),
  fire_tolerance VARCHAR(20),
  anaerobic_tolerance VARCHAR(20),

  -- Additional characteristics
  nitrogen_fixation VARCHAR(20),    -- None, Low, Medium, High
  root_depth_min_in NUMERIC(6,2),
  height_mature_ft NUMERIC(6,2),
  growth_rate VARCHAR(20),          -- Slow, Moderate, Rapid
  lifespan VARCHAR(50),
  leaf_retention VARCHAR(10),       -- Yes, No
  bloom_period VARCHAR(100),
  fruit_seed_period VARCHAR(100),
  foliage_color VARCHAR(50),
  flower_color VARCHAR(100),

  -- Metadata
  source_url TEXT,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_ref_usda_traits_taxon ON ref_usda_traits(taxon_id);
CREATE INDEX IF NOT EXISTS idx_ref_usda_traits_symbol ON ref_usda_traits(usda_symbol);

-- NWPL Wetland indicator status by region
CREATE TABLE IF NOT EXISTS ref_nwpl_regional (
  id SERIAL PRIMARY KEY,
  taxon_id INTEGER REFERENCES ref_taxon(id) ON DELETE CASCADE,
  region_code VARCHAR(10) NOT NULL,    -- AW, EMP, GP, etc.
  region_name VARCHAR(100),
  indicator VARCHAR(10),               -- OBL, FACW, FAC, FACU, UPL
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(taxon_id, region_code)
);

CREATE INDEX IF NOT EXISTS idx_ref_nwpl_taxon ON ref_nwpl_regional(taxon_id);
CREATE INDEX IF NOT EXISTS idx_ref_nwpl_region ON ref_nwpl_regional(region_code);

-- View: Combined taxon + national wetland indicator
-- National indicator = most common indicator across all regions
CREATE OR REPLACE VIEW vw_taxon_wetland_with_national AS
SELECT
  t.id AS taxon_id,
  t.usda_symbol,
  t.scientific_name,
  t.common_name,
  mode() WITHIN GROUP (ORDER BY n.indicator) AS national_indicator
FROM ref_taxon t
LEFT JOIN ref_nwpl_regional n ON n.taxon_id = t.id
GROUP BY t.id, t.usda_symbol, t.scientific_name, t.common_name;
