-- =============================================================================
-- Edaphic Flora: "Download my samples (CSV)" on edaphicflora.org/account/
-- =============================================================================
-- Run as the database owner after contribute.sql, on the Neon dev branch
-- first, then main. Safe to re-run.
--
-- The signed-in person's own rows, every column they entered (their own
-- location and notes included, as in the Shiny app's My Data export), never
-- created_by. Same rules as the app (db_check_export_rate in app/R/db.R):
-- 10 exports per person per 24 hours, counted from audit_log action
-- 'export', and disabled accounts can't export. Each export is logged.
--
-- The public community download needs nothing here: it is built at build
-- time from the public_site views (site/src/pages/data/).
-- =============================================================================

BEGIN;

CREATE OR REPLACE FUNCTION site_api.export_my_samples(p_user_id TEXT)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  c_daily_per_user CONSTANT INT := 10;
  v_used INT;
  v_rows JSONB;
  v_n    INT;
BEGIN
  IF p_user_id IS NULL OR NOT EXISTS (SELECT 1 FROM site_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'unknown_user');
  END IF;
  IF EXISTS (SELECT 1 FROM disabled_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'disabled');
  END IF;

  -- One export at a time per person, so the daily cap can't be raced
  PERFORM pg_advisory_xact_lock(hashtext('site_api.export:' || p_user_id));
  SELECT count(*) INTO v_used FROM audit_log
   WHERE user_id = p_user_id AND action = 'export' AND created_at > now() - interval '24 hours';
  IF v_used >= c_daily_per_user THEN
    RETURN jsonb_build_object('status', 'rate_limited', 'limit', c_daily_per_user);
  END IF;

  SELECT coalesce(jsonb_agg(to_jsonb(x) ORDER BY x.created_at, x.id), '[]'::jsonb), count(*)
    INTO v_rows, v_n
    FROM (
      SELECT s.id, s.date::TEXT AS date, s.species, s.cultivar, s.outcome, s.sun_exposure, s.site_hydrology,
             s.ph, s.organic_matter, s.cec_meq, s.soluble_salts_ppm, s.nitrate_ppm, s.ammonium_ppm,
             s.phosphorus_ppm, s.potassium_ppm, s.calcium_ppm, s.magnesium_ppm, s.sulfur_ppm,
             s.iron_ppm, s.manganese_ppm, s.zinc_ppm, s.copper_ppm, s.boron_ppm,
             s.texture_class, s.texture_sand, s.texture_silt, s.texture_clay,
             s.location_lat, s.location_long, s.ecoregion_l4, s.ecoregion_l4_code,
             s.inat_url, s.notes,
             to_char(s.created_at, 'YYYY-MM-DD HH24:MI:SS') AS created_at,
             coalesce(s.flagged, FALSE) AS flagged
        FROM soil_samples s
       WHERE s.created_by = p_user_id
       ORDER BY s.created_at, s.id
       LIMIT 10000
    ) x;

  INSERT INTO audit_log (action, table_name, record_id, user_id, details)
  VALUES ('export', 'soil_samples', NULL, p_user_id, format('exported own data (%s rows) (edaphicflora.org)', v_n));

  RETURN jsonb_build_object('status', 'ok', 'rows', v_rows,
                            'remaining', c_daily_per_user - v_used - 1);
END $$;

REVOKE ALL ON FUNCTION site_api.export_my_samples(TEXT) FROM PUBLIC;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'site_writer') THEN
    GRANT EXECUTE ON FUNCTION site_api.export_my_samples(TEXT) TO site_writer;
  END IF;
END $$;

COMMIT;

-- Check it (as the owner):
--   SET ROLE site_writer;
--   SELECT site_api.export_my_samples('nobody');   -- {"status": "unknown_user"}
--   RESET ROLE;
