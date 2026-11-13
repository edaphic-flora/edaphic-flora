-- =============================================================================
-- Edaphic Flora: contributing soil tests from edaphicflora.org
-- =============================================================================
-- Run as the database owner (the role that ran public_views.sql), on the Neon
-- dev branch first, then main. Safe to re-run. Then load the reference data
-- with scripts/load_site_reference.R.
--
-- Design:
--   * The site's Netlify functions verify the Firebase sign-in, then call the
--     SECURITY DEFINER functions in the site_api schema. The site_writer login
--     can ONLY call those functions: it has no access to any table, so if its
--     connection string ever leaked it could not read locations, emails or
--     anyone's samples, and every write is capped and audited.
--   * Identity: soil_samples.created_by holds the Shiny app's Polished user id.
--     On first sign-in on the site, a Firebase account is linked to its Polished
--     id by verified email (legacy_users), so a contributor keeps one identity
--     across the app and the site. New people use their Firebase uid.
--   * Rules mirror app/R/mod_data_entry.R and app/R/db.R (20 samples per user
--     per 24 hours, duplicate check, disabled users, audit log).
-- =============================================================================

BEGIN;

CREATE EXTENSION IF NOT EXISTS pg_trgm;

-- -----------------------------------------------------------------------------
-- Reference data (loaded by scripts/load_site_reference.R)
-- -----------------------------------------------------------------------------

-- WCVP accepted species: the same list the app validates against
-- (app/species_accepted.csv). common_name comes from USDA PLANTS.
CREATE TABLE IF NOT EXISTS ref_species_accepted (
  taxon_name  TEXT PRIMARY KEY,
  family      TEXT,
  common_name TEXT
);
-- Prefix search on the scientific name; substring search on common names only
-- (a trigram index over all 360K scientific names would cost far more space).
CREATE INDEX IF NOT EXISTS idx_species_accepted_prefix
  ON ref_species_accepted (lower(taxon_name) text_pattern_ops);
CREATE INDEX IF NOT EXISTS idx_species_accepted_common
  ON ref_species_accepted USING gin (lower(common_name) gin_trgm_ops)
  WHERE common_name IS NOT NULL;

-- Zip code centroids (app/data/zipcode_lookup.csv)
CREATE TABLE IF NOT EXISTS ref_zipcodes (
  zipcode   CHAR(5) PRIMARY KEY,
  city      TEXT,
  state     CHAR(2),
  latitude  NUMERIC(9,6),
  longitude NUMERIC(9,6)
);

-- Level 4 ecoregions on a 0.1 degree grid (app/data/ecoregion_grid.csv), the
-- same lookup the production app uses (get_ecoregion_from_grid in app/R/data.R)
CREATE TABLE IF NOT EXISTS ref_ecoregion_grid (
  lat            NUMERIC(4,1),
  lon            NUMERIC(5,1),
  ecoregion_name TEXT,
  ecoregion_code TEXT,
  PRIMARY KEY (lat, lon)
);

-- -----------------------------------------------------------------------------
-- Identity (private: never exposed in public_site views)
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS legacy_users (
  email_lower  TEXT PRIMARY KEY,
  polished_uid TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS site_users (
  firebase_uid TEXT PRIMARY KEY,
  user_id      TEXT NOT NULL,      -- written to soil_samples.created_by
  email        TEXT,
  created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_site_users_user_id ON site_users (user_id);

-- -----------------------------------------------------------------------------
-- Public lookups for the contribute form (reference data only)
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW public_site.species_names AS
SELECT taxon_name, family, common_name
FROM ref_species_accepted;

CREATE OR REPLACE VIEW public_site.zip_lookup AS
SELECT z.zipcode, z.city, z.state, z.latitude, z.longitude, g.ecoregion_name, g.ecoregion_code
FROM ref_zipcodes z
LEFT JOIN ref_ecoregion_grid g
  ON g.lat = round(z.latitude, 1) AND g.lon = round(z.longitude, 1);

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'site_reader') THEN
    GRANT SELECT ON public_site.species_names, public_site.zip_lookup TO site_reader;
  END IF;
END $$;

-- -----------------------------------------------------------------------------
-- site_api: the only things the site_writer login can do
-- -----------------------------------------------------------------------------
CREATE SCHEMA IF NOT EXISTS site_api;
REVOKE ALL ON SCHEMA site_api FROM PUBLIC;

-- Link a verified Firebase account to its contributor id. Returns the id,
-- whether the account is disabled, and how many samples it can add today.
CREATE OR REPLACE FUNCTION site_api.resolve_user(p_firebase_uid TEXT, p_email TEXT)
RETURNS TABLE (user_id TEXT, disabled BOOLEAN, remaining_today INT)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
#variable_conflict use_column
DECLARE
  v_uid TEXT;
BEGIN
  IF p_firebase_uid IS NULL OR length(p_firebase_uid) NOT BETWEEN 1 AND 128 THEN
    RAISE EXCEPTION 'invalid account id';
  END IF;

  SELECT su.user_id INTO v_uid FROM site_users su WHERE su.firebase_uid = p_firebase_uid;
  IF v_uid IS NULL THEN
    SELECT lu.polished_uid INTO v_uid
      FROM legacy_users lu
     WHERE lu.email_lower = lower(btrim(p_email));
    INSERT INTO site_users (firebase_uid, user_id, email)
    VALUES (p_firebase_uid, coalesce(v_uid, p_firebase_uid), left(btrim(p_email), 320))
    ON CONFLICT (firebase_uid) DO NOTHING;
    SELECT su.user_id INTO v_uid FROM site_users su WHERE su.firebase_uid = p_firebase_uid;
    INSERT INTO audit_log (action, table_name, record_id, user_id, details)
    VALUES ('site_link', 'site_users', NULL, v_uid, 'first sign-in on edaphicflora.org');
  END IF;

  RETURN QUERY
  SELECT v_uid,
         EXISTS (SELECT 1 FROM disabled_users d WHERE d.user_id = v_uid),
         greatest(0, 20 - (SELECT count(*)::int FROM soil_samples s
                            WHERE s.created_by = v_uid AND s.created_at > now() - interval '24 hours'));
END $$;

-- Save one soil test for 1 to 20 plants, all or nothing.
--   p_soil:     {"ph":6.5,"organic_matter":4.2,...,"texture_class":"Loam",
--                "texture_sand":40,"texture_silt":40,"texture_clay":20}
--   p_plants:   [{"species":"Asclepias tuberosa","cultivar":null,"outcome":"Thriving",
--                 "sun_exposure":"Full Sun","site_hydrology":"Dry","inat_url":null}, ...]
--   p_location: {"zip":"06511"} or {"lat":41.3,"lon":-72.9}
-- Returns {"status":"ok","ids":[...]} or {"status":"<reason>", ...}.
-- The Netlify function validates everything first; these checks are the
-- backstop if the function is ever bypassed.
CREATE OR REPLACE FUNCTION site_api.submit_samples(
  p_user_id TEXT, p_soil JSONB, p_plants JSONB, p_location JSONB,
  p_date DATE, p_notes TEXT, p_confirm_duplicates BOOLEAN)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  c_daily_per_user CONSTANT INT := 20;
  c_daily_global   CONSTANT INT := 300;
  v_n      INT;
  v_recent INT;
  v_global INT;
  v_lat    NUMERIC;
  v_lon    NUMERIC;
  v_eco_name TEXT;
  v_eco_code TEXT;
  v_bad    TEXT[];
  v_ids    INT[];
  v_num    NUMERIC;
  v_key    TEXT;
BEGIN
  IF p_user_id IS NULL OR NOT EXISTS (SELECT 1 FROM site_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'unknown_user');
  END IF;
  IF jsonb_typeof(p_plants) IS DISTINCT FROM 'array' OR jsonb_typeof(p_soil) IS DISTINCT FROM 'object'
     OR jsonb_typeof(p_location) IS DISTINCT FROM 'object' THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'malformed request');
  END IF;
  v_n := jsonb_array_length(p_plants);
  IF v_n < 1 OR v_n > 20 THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Add between 1 and 20 plants.');
  END IF;

  -- One submit at a time per person, then one at a time overall, so the
  -- daily caps can't be raced.
  PERFORM pg_advisory_xact_lock(hashtext('site_api.submit:' || p_user_id));
  PERFORM pg_advisory_xact_lock(hashtext('site_api.submit:all'));

  IF EXISTS (SELECT 1 FROM disabled_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'disabled');
  END IF;

  SELECT count(*) INTO v_recent FROM soil_samples
   WHERE created_by = p_user_id AND created_at > now() - interval '24 hours';
  IF v_recent + v_n > c_daily_per_user THEN
    RETURN jsonb_build_object('status', 'rate_limited', 'remaining', greatest(0, c_daily_per_user - v_recent));
  END IF;
  SELECT count(*) INTO v_global FROM soil_samples WHERE created_at > now() - interval '24 hours';
  IF v_global + v_n > c_daily_global THEN
    RETURN jsonb_build_object('status', 'busy');
  END IF;

  -- Backstop checks on the values
  IF p_date IS NULL OR p_date < DATE '1900-01-01' OR p_date > current_date + 1 THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Sample date is out of range.');
  END IF;
  IF length(coalesce(p_notes, '')) > 1000 THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Notes must be 1,000 characters or fewer.');
  END IF;
  FOR v_key IN SELECT k FROM jsonb_object_keys(p_soil) AS k LOOP
    IF v_key NOT IN ('ph', 'organic_matter', 'cec_meq', 'soluble_salts_ppm', 'nitrate_ppm', 'ammonium_ppm',
                     'phosphorus_ppm', 'potassium_ppm', 'calcium_ppm', 'magnesium_ppm', 'sulfur_ppm',
                     'iron_ppm', 'manganese_ppm', 'zinc_ppm', 'copper_ppm', 'boron_ppm',
                     'texture_class', 'texture_sand', 'texture_silt', 'texture_clay') THEN
      RETURN jsonb_build_object('status', 'invalid', 'error', 'Unknown field: ' || left(v_key, 40));
    END IF;
    IF v_key <> 'texture_class' AND jsonb_typeof(p_soil -> v_key) NOT IN ('number', 'null') THEN
      RETURN jsonb_build_object('status', 'invalid', 'error', 'Not a number: ' || v_key);
    END IF;
    IF v_key LIKE '%\_ppm' AND (p_soil ->> v_key)::NUMERIC NOT BETWEEN 0 AND 100000 THEN
      RETURN jsonb_build_object('status', 'invalid', 'error', v_key || ' must be between 0 and 100,000 ppm');
    END IF;
  END LOOP;
  IF (p_soil ->> 'ph') IS NULL AND (p_soil ->> 'organic_matter') IS NULL THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Please enter at least pH or organic matter percentage.');
  END IF;
  IF (p_soil ->> 'ph')::NUMERIC NOT BETWEEN 2 AND 14
     OR (p_soil ->> 'organic_matter')::NUMERIC NOT BETWEEN 0 AND 50
     OR (p_soil ->> 'cec_meq')::NUMERIC NOT BETWEEN 0 AND 200
     OR (p_soil ->> 'texture_sand')::NUMERIC NOT BETWEEN 0 AND 100
     OR (p_soil ->> 'texture_silt')::NUMERIC NOT BETWEEN 0 AND 100
     OR (p_soil ->> 'texture_clay')::NUMERIC NOT BETWEEN 0 AND 100 THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'A soil value is out of range.');
  END IF;
  IF (p_soil ->> 'texture_class') IS NOT NULL AND (p_soil ->> 'texture_class') NOT IN (
       'Heavy Clay', 'Silty Clay', 'Clay', 'Silty Clay Loam', 'Clay Loam', 'Silt', 'Silty Loam',
       'Sandy Clay', 'Loam', 'Sandy Clay Loam', 'Sandy Loam', 'Loamy Sand', 'Sand', 'Unclassified') THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Unknown texture class.');
  END IF;

  -- Plants: known species, allowed values, no repeats within one submit
  SELECT array_agg(DISTINCT left(coalesce(p.species, '(blank)'), 120)) INTO v_bad
    FROM jsonb_to_recordset(p_plants) AS p(species TEXT)
   WHERE NOT EXISTS (SELECT 1 FROM ref_species_accepted s WHERE s.taxon_name = p.species);
  IF v_bad IS NOT NULL THEN
    RETURN jsonb_build_object('status', 'invalid_species', 'species', to_jsonb(v_bad));
  END IF;
  IF (SELECT count(DISTINCT p.species) FROM jsonb_to_recordset(p_plants) AS p(species TEXT)) <> v_n THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Each plant can only be listed once.');
  END IF;
  IF EXISTS (
    SELECT 1 FROM jsonb_to_recordset(p_plants)
      AS p(cultivar TEXT, outcome TEXT, sun_exposure TEXT, site_hydrology TEXT, inat_url TEXT)
     WHERE length(coalesce(p.cultivar, '')) > 255
        OR (p.outcome IS NOT NULL AND p.outcome NOT IN ('Thriving', 'Established', 'Struggling', 'Failed/Died'))
        OR (p.sun_exposure IS NOT NULL AND p.sun_exposure NOT IN ('Full Sun', 'Part Sun', 'Part Shade', 'Full Shade'))
        OR (p.site_hydrology IS NOT NULL AND p.site_hydrology NOT IN ('Dry', 'Mesic', 'Wet'))
        OR (p.inat_url IS NOT NULL AND p.inat_url !~ '^https://(www\.)?inaturalist\.org/observations/[0-9]{1,12}$')
  ) THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'A plant detail has a value that is not allowed.');
  END IF;

  -- Same plant from the same person in the last 24 hours: ask first (as the app does)
  IF NOT coalesce(p_confirm_duplicates, FALSE) THEN
    SELECT array_agg(DISTINCT p.species) INTO v_bad
      FROM jsonb_to_recordset(p_plants) AS p(species TEXT)
     WHERE EXISTS (SELECT 1 FROM soil_samples s
                    WHERE s.species = p.species AND s.created_by = p_user_id
                      AND s.created_at > now() - interval '24 hours');
    IF v_bad IS NOT NULL THEN
      RETURN jsonb_build_object('status', 'duplicate', 'species', to_jsonb(v_bad));
    END IF;
  END IF;

  -- Location: zip centroid, or coordinates typed in. Only lat/long are stored.
  IF p_location ? 'zip' THEN
    SELECT latitude, longitude INTO v_lat, v_lon FROM ref_zipcodes WHERE zipcode = p_location ->> 'zip';
    IF NOT FOUND THEN
      RETURN jsonb_build_object('status', 'invalid', 'error', 'That zip code was not found.');
    END IF;
  ELSE
    IF jsonb_typeof(p_location -> 'lat') IS DISTINCT FROM 'number'
       OR jsonb_typeof(p_location -> 'lon') IS DISTINCT FROM 'number' THEN
      RETURN jsonb_build_object('status', 'invalid', 'error', 'Enter a zip code or latitude and longitude.');
    END IF;
    v_lat := round((p_location ->> 'lat')::NUMERIC, 6);
    v_lon := round((p_location ->> 'lon')::NUMERIC, 6);
    IF v_lat NOT BETWEEN -90 AND 90 OR v_lon NOT BETWEEN -180 AND 180 THEN
      RETURN jsonb_build_object('status', 'invalid', 'error', 'Latitude or longitude is out of range.');
    END IF;
  END IF;
  SELECT ecoregion_name, ecoregion_code INTO v_eco_name, v_eco_code
    FROM ref_ecoregion_grid WHERE lat = round(v_lat, 1) AND lon = round(v_lon, 1);

  WITH ins AS (
    INSERT INTO soil_samples (
      species, cultivar, outcome, sun_exposure, site_hydrology, inat_url,
      ph, organic_matter, cec_meq, soluble_salts_ppm, nitrate_ppm, ammonium_ppm,
      phosphorus_ppm, potassium_ppm, calcium_ppm, magnesium_ppm, sulfur_ppm,
      iron_ppm, manganese_ppm, zinc_ppm, copper_ppm, boron_ppm,
      texture_class, texture_sand, texture_silt, texture_clay,
      ecoregion_l4, ecoregion_l4_code, location_lat, location_long,
      notes, date, created_by)
    SELECT
      p.species, NULLIF(btrim(p.cultivar), ''), p.outcome, p.sun_exposure, p.site_hydrology, p.inat_url,
      (p_soil ->> 'ph')::NUMERIC, (p_soil ->> 'organic_matter')::NUMERIC, (p_soil ->> 'cec_meq')::NUMERIC,
      (p_soil ->> 'soluble_salts_ppm')::NUMERIC, (p_soil ->> 'nitrate_ppm')::NUMERIC, (p_soil ->> 'ammonium_ppm')::NUMERIC,
      (p_soil ->> 'phosphorus_ppm')::NUMERIC, (p_soil ->> 'potassium_ppm')::NUMERIC, (p_soil ->> 'calcium_ppm')::NUMERIC,
      (p_soil ->> 'magnesium_ppm')::NUMERIC, (p_soil ->> 'sulfur_ppm')::NUMERIC, (p_soil ->> 'iron_ppm')::NUMERIC,
      (p_soil ->> 'manganese_ppm')::NUMERIC, (p_soil ->> 'zinc_ppm')::NUMERIC, (p_soil ->> 'copper_ppm')::NUMERIC,
      (p_soil ->> 'boron_ppm')::NUMERIC,
      p_soil ->> 'texture_class', (p_soil ->> 'texture_sand')::NUMERIC, (p_soil ->> 'texture_silt')::NUMERIC,
      (p_soil ->> 'texture_clay')::NUMERIC,
      v_eco_name, v_eco_code, v_lat, v_lon,
      NULLIF(btrim(p_notes), ''), p_date, p_user_id
    FROM jsonb_to_recordset(p_plants)
      AS p(species TEXT, cultivar TEXT, outcome TEXT, sun_exposure TEXT, site_hydrology TEXT, inat_url TEXT)
    RETURNING id, species
  ), logged AS (
    INSERT INTO audit_log (action, table_name, record_id, user_id, details)
    SELECT 'create', 'soil_samples', ins.id, p_user_id, 'species: ' || ins.species || ' (edaphicflora.org)'
    FROM ins
    RETURNING record_id
  )
  SELECT array_agg(record_id ORDER BY record_id) INTO v_ids FROM logged;

  RETURN jsonb_build_object('status', 'ok', 'ids', to_jsonb(v_ids), 'ecoregion', v_eco_name);
END $$;

-- The signed-in person's own samples (no location, no created_by)
CREATE OR REPLACE FUNCTION site_api.my_samples(p_user_id TEXT)
RETURNS TABLE (
  id INT, species TEXT, cultivar TEXT, outcome TEXT, sun_exposure TEXT, site_hydrology TEXT,
  ph NUMERIC, organic_matter NUMERIC, texture_class TEXT, ecoregion_l4 TEXT,
  sample_date DATE, created_at TIMESTAMP, flagged BOOLEAN)
LANGUAGE sql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
  SELECT s.id, s.species::TEXT, s.cultivar::TEXT, s.outcome::TEXT, s.sun_exposure::TEXT, s.site_hydrology::TEXT,
         s.ph::NUMERIC, s.organic_matter::NUMERIC, s.texture_class::TEXT, s.ecoregion_l4::TEXT,
         s.date, s.created_at, coalesce(s.flagged, FALSE)
    FROM soil_samples s
   WHERE s.created_by = p_user_id
     AND EXISTS (SELECT 1 FROM site_users su WHERE su.user_id = p_user_id)
   ORDER BY s.created_at DESC, s.id DESC
   LIMIT 500
$$;

-- Delete one of the signed-in person's own samples (audited, like the app)
CREATE OR REPLACE FUNCTION site_api.delete_my_sample(p_user_id TEXT, p_id INT)
RETURNS BOOLEAN
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_deleted INT;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM site_users WHERE user_id = p_user_id) THEN
    RETURN FALSE;
  END IF;
  WITH d AS (
    DELETE FROM soil_samples WHERE id = p_id AND created_by = p_user_id RETURNING id
  )
  INSERT INTO audit_log (action, table_name, record_id, user_id, details)
  SELECT 'delete', 'soil_samples', d.id, p_user_id, 'entry deleted (edaphicflora.org)' FROM d
  RETURNING record_id INTO v_deleted;
  RETURN v_deleted IS NOT NULL;
END $$;

-- -----------------------------------------------------------------------------
-- Home location (same user_preferences table the earlier Shiny app used)
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION site_api.get_prefs(p_user_id TEXT)
RETURNS TABLE (home_zipcode TEXT, home_state TEXT, home_city TEXT)
LANGUAGE sql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
  SELECT p.home_zipcode::TEXT, p.home_state::TEXT, p.home_city::TEXT
    FROM user_preferences p
   WHERE p.user_id = p_user_id
     AND EXISTS (SELECT 1 FROM site_users su WHERE su.user_id = p_user_id)
$$;

-- Save a home zip (state, city and coordinates come from the zip) or just a
-- state. Empty values clear the saved location.
CREATE OR REPLACE FUNCTION site_api.save_prefs(p_user_id TEXT, p_zip TEXT, p_state TEXT)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_zip   TEXT := NULLIF(btrim(p_zip), '');
  v_state TEXT := upper(NULLIF(btrim(p_state), ''));
  v_city  TEXT;
  v_lat   NUMERIC;
  v_lon   NUMERIC;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM site_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'unknown_user');
  END IF;
  IF v_zip IS NOT NULL THEN
    SELECT z.state, z.city, z.latitude, z.longitude INTO v_state, v_city, v_lat, v_lon
      FROM ref_zipcodes z WHERE z.zipcode = v_zip;
    IF NOT FOUND THEN
      RETURN jsonb_build_object('status', 'invalid', 'error', 'That zip code was not found.');
    END IF;
  ELSIF v_state IS NOT NULL AND v_state NOT IN (
    'AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD',
    'MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD',
    'TN','TX','UT','VT','VA','WA','WV','WI','WY') THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Choose a state from the list.');
  END IF;

  INSERT INTO user_preferences (user_id, home_zipcode, home_state, home_city, home_lat, home_long, updated_at)
  VALUES (p_user_id, v_zip, v_state, v_city, v_lat, v_lon, now())
  ON CONFLICT (user_id) DO UPDATE SET
    home_zipcode = EXCLUDED.home_zipcode, home_state = EXCLUDED.home_state, home_city = EXCLUDED.home_city,
    home_lat = EXCLUDED.home_lat, home_long = EXCLUDED.home_long, updated_at = now();

  RETURN jsonb_build_object('status', 'ok', 'home_zipcode', v_zip, 'home_state', v_state, 'home_city', v_city);
END $$;

-- -----------------------------------------------------------------------------
-- Soil report extraction (Anthropic API), same limits as the Shiny app:
-- 3 per person and 20 overall per calendar day. A slot is claimed before the
-- API call and released if the call fails, so failures don't count.
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION site_api.claim_extraction(p_user_id TEXT, p_filename TEXT)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  c_per_user CONSTANT INT := 3;
  c_global   CONSTANT INT := 20;
  v_user INT;
  v_all  INT;
  v_id   INT;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM site_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'unknown_user');
  END IF;
  IF EXISTS (SELECT 1 FROM disabled_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'disabled');
  END IF;
  PERFORM pg_advisory_xact_lock(hashtext('site_api.extract:all'));
  SELECT count(*) INTO v_user FROM pdf_extractions WHERE user_id = p_user_id AND extracted_at >= CURRENT_DATE;
  IF v_user >= c_per_user THEN
    RETURN jsonb_build_object('status', 'user_limit', 'limit', c_per_user);
  END IF;
  SELECT count(*) INTO v_all FROM pdf_extractions WHERE extracted_at >= CURRENT_DATE;
  IF v_all >= c_global THEN
    RETURN jsonb_build_object('status', 'global_limit');
  END IF;
  INSERT INTO pdf_extractions (user_id, filename)
  VALUES (p_user_id, left(coalesce(p_filename, ''), 200) || ' (edaphicflora.org)')
  RETURNING id INTO v_id;
  RETURN jsonb_build_object('status', 'ok', 'id', v_id, 'remaining', c_per_user - v_user - 1);
END $$;

CREATE OR REPLACE FUNCTION site_api.finish_extraction(p_user_id TEXT, p_id INT, p_tokens INT)
RETURNS VOID
LANGUAGE sql SECURITY DEFINER SET search_path = public, pg_temp AS $$
  UPDATE pdf_extractions SET tokens_used = greatest(0, p_tokens)
   WHERE id = p_id AND user_id = p_user_id
$$;

CREATE OR REPLACE FUNCTION site_api.release_extraction(p_user_id TEXT, p_id INT)
RETURNS VOID
LANGUAGE sql SECURITY DEFINER SET search_path = public, pg_temp AS $$
  DELETE FROM pdf_extractions WHERE id = p_id AND user_id = p_user_id AND tokens_used IS NULL
$$;

CREATE OR REPLACE FUNCTION site_api.extractions_left(p_user_id TEXT)
RETURNS INT
LANGUAGE sql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
  SELECT greatest(0, 3 - (SELECT count(*)::int FROM pdf_extractions
                           WHERE user_id = p_user_id AND extracted_at >= CURRENT_DATE))
$$;

-- Functions are executable by PUBLIC by default; only site_writer may call these.
REVOKE ALL ON ALL FUNCTIONS IN SCHEMA site_api FROM PUBLIC;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'site_writer') THEN
    GRANT USAGE ON SCHEMA site_api TO site_writer;
    GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA site_api TO site_writer;
  END IF;
END $$;

COMMIT;

-- =============================================================================
-- ONE TIME: the site's write login. Create it with SQL, not in the Neon
-- Console (Console roles get neon_superuser). Use a long random password.
--
--   CREATE ROLE site_writer WITH LOGIN PASSWORD 'replace-with-a-long-random-password';
--   GRANT USAGE ON SCHEMA site_api TO site_writer;
--   GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA site_api TO site_writer;
--   REVOKE ALL ON ALL TABLES IN SCHEMA public FROM site_writer;
--
-- Check it:
--   SET ROLE site_writer;
--   SELECT count(*) FROM soil_samples;               -- permission denied
--   SELECT * FROM site_api.my_samples('nobody');     -- works, returns nothing
--   RESET ROLE;
--
-- Then add it to Netlify as SITE_WRITER_DATABASE_URL:
--   postgresql://site_writer:<password>@<neon-host>/<db>?sslmode=require
-- =============================================================================
