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
