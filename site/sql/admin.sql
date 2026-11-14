-- =============================================================================
-- Edaphic Flora: moderation tools for edaphicflora.org/admin/
-- =============================================================================
-- Run as the database owner AFTER contribute.sql, on the Neon dev branch
-- first, then main. Safe to re-run. Then do the one-time steps at the bottom
-- (the site_admin login and your row in site_admins).
--
-- Design:
--   * The admin page's Netlify function (netlify/functions/admin.mjs) verifies
--     the Firebase sign-in and checks the verified email against the
--     ADMIN_EMAILS Netlify variable. Only then does it connect, as the
--     site_admin login (SITE_ADMIN_DATABASE_URL), and call the SECURITY
--     DEFINER functions below.
--   * site_admin is a separate login from site_writer, and these functions sit
--     in their own schema (admin_api), because contribute.sql grants every
--     function in site_api to site_writer. On Netlify's free plan every build
--     can see every environment variable, so the contribute login, which every
--     signed-in request uses, gets no way to moderate even if it leaks.
--   * Two lists, like the app's "Polished admin AND ADMIN_EMAILS" rule: every
--     function here checks the caller's verified email again against
--     site_admins, so ADMIN_EMAILS alone can't make anyone an admin, and
--     neither can a mistake in the function.
--   * Moderation only: flag, unflag, disable, re-enable. Nothing here deletes
--     or edits a sample or returns emails or locations, so every action can be
--     undone, and every change writes audit_log rows the way the app did.
--   * Public pages (public_views.sql) already drop flagged samples and samples
--     from disabled_users, so changes here show on the site at the next
--     weekly rebuild. Contributors see "flagged for review" on /account/ at
--     once, and a disabled account can't add samples or read reports at once.
-- =============================================================================

BEGIN;

-- Second admin list (the first is ADMIN_EMAILS in Netlify). Emails are stored
-- lower case; the functions compare them lower case.
CREATE TABLE IF NOT EXISTS site_admins (
  email_lower TEXT PRIMARY KEY CHECK (email_lower = lower(btrim(email_lower)) AND email_lower <> ''),
  added_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
  note        TEXT
);
REVOKE ALL ON site_admins FROM PUBLIC;

CREATE SCHEMA IF NOT EXISTS admin_api;
REVOKE ALL ON SCHEMA admin_api FROM PUBLIC;

-- The caller, if they are an admin. Raises "not a site admin" (SQLSTATE 42501)
-- otherwise. Returns the id written to audit_log.user_id and
-- disabled_users.disabled_by: the admin's contributor id (their Polished id if
-- they used the app, as the app logged), or their Firebase uid if they have
-- never signed in to /account/.
CREATE OR REPLACE FUNCTION admin_api._actor(p_firebase_uid TEXT, p_email TEXT)
RETURNS TEXT
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_id TEXT;
BEGIN
  IF p_firebase_uid IS NULL OR length(p_firebase_uid) NOT BETWEEN 1 AND 128
     OR p_email IS NULL
     OR NOT EXISTS (SELECT 1 FROM site_admins a WHERE a.email_lower = lower(btrim(p_email))) THEN
    RAISE EXCEPTION 'not a site admin' USING ERRCODE = '42501';
  END IF;
  SELECT su.user_id INTO v_id FROM site_users su WHERE su.firebase_uid = p_firebase_uid;
  RETURN coalesce(v_id, p_firebase_uid);
END $$;

-- A contributor id the database knows (has samples, signed in on the site, or
-- had an app account). Stops typos from disabling nobody.
CREATE OR REPLACE FUNCTION admin_api._known_user(p_user_id TEXT)
RETURNS BOOLEAN
LANGUAGE sql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
  SELECT p_user_id IS NOT NULL AND length(p_user_id) BETWEEN 1 AND 128 AND (
         EXISTS (SELECT 1 FROM soil_samples s WHERE s.created_by = p_user_id)
      OR EXISTS (SELECT 1 FROM site_users su WHERE su.user_id = p_user_id)
      OR EXISTS (SELECT 1 FROM legacy_users lu WHERE lu.polished_uid = p_user_id)
      OR EXISTS (SELECT 1 FROM disabled_users d WHERE d.user_id = p_user_id))
$$;

-- -----------------------------------------------------------------------------
-- Reads
-- -----------------------------------------------------------------------------

-- Is the caller an admin, plus the numbers for the top of the page.
CREATE OR REPLACE FUNCTION admin_api.whoami(p_firebase_uid TEXT, p_email TEXT)
RETURNS JSONB
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_actor TEXT := admin_api._actor(p_firebase_uid, p_email);
BEGIN
  RETURN jsonb_build_object(
    'actor', v_actor,
    'samples', (SELECT count(*) FROM soil_samples),
    'flagged', (SELECT count(*) FROM soil_samples WHERE flagged),
    'samples_24h', (SELECT count(*) FROM soil_samples WHERE created_at > (now() AT TIME ZONE 'UTC') - interval '24 hours'),
    'contributors', (SELECT count(DISTINCT created_by) FROM soil_samples WHERE created_by IS NOT NULL),
    'disabled', (SELECT count(*) FROM disabled_users),
    'actions', coalesce((SELECT jsonb_agg(a ORDER BY a) FROM (SELECT DISTINCT action::TEXT AS a FROM audit_log) x), '[]'::jsonb));
END $$;

-- Samples, newest first, including flagged ones and notes (where spam shows
-- up). No location. p_search is a sample id (123 or #123) or text found in
-- the species, cultivar or notes. Page with p_before_id (the last id shown).
CREATE OR REPLACE FUNCTION admin_api.samples(
  p_firebase_uid TEXT, p_email TEXT,
  p_flagged_only BOOLEAN, p_user_id TEXT, p_search TEXT, p_before_id INT, p_limit INT)
RETURNS TABLE (
  id INT, species TEXT, cultivar TEXT, created_by TEXT, ph NUMERIC, organic_matter NUMERIC,
  texture_class TEXT, outcome TEXT, ecoregion_l4 TEXT, notes TEXT, inat_url TEXT,
  sample_date TEXT, created_at TIMESTAMPTZ, flagged BOOLEAN, flag_reason TEXT, user_disabled BOOLEAN)
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
#variable_conflict use_column
DECLARE
  v_actor   TEXT := admin_api._actor(p_firebase_uid, p_email);
  v_search  TEXT := NULLIF(btrim(p_search), '');
  v_user    TEXT := NULLIF(btrim(p_user_id), '');
  v_id      INT;
  v_like    TEXT;
BEGIN
  IF v_search ~ '^#?[0-9]{1,9}$' THEN
    v_id := ltrim(v_search, '#')::INT;
  ELSIF v_search IS NOT NULL THEN
    v_like := '%' || replace(replace(replace(v_search, '\', '\\'), '%', '\%'), '_', '\_') || '%';
  END IF;
  RETURN QUERY
  SELECT s.id, s.species::TEXT, s.cultivar::TEXT, s.created_by::TEXT, s.ph::NUMERIC, s.organic_matter::NUMERIC,
         s.texture_class::TEXT, s.outcome::TEXT, s.ecoregion_l4::TEXT, s.notes::TEXT, s.inat_url::TEXT,
         s.date::TEXT, s.created_at AT TIME ZONE 'UTC', coalesce(s.flagged, FALSE), s.flag_reason::TEXT,
         EXISTS (SELECT 1 FROM disabled_users d WHERE d.user_id = s.created_by)
    FROM soil_samples s
   WHERE (NOT coalesce(p_flagged_only, FALSE) OR s.flagged)
     AND (v_user IS NULL OR s.created_by = v_user)
     AND (v_id IS NULL OR s.id = v_id)
     AND (v_like IS NULL OR s.species ILIKE v_like OR s.cultivar ILIKE v_like OR s.notes ILIKE v_like)
     AND (p_before_id IS NULL OR s.id < p_before_id)
   ORDER BY s.id DESC
   LIMIT least(greatest(coalesce(p_limit, 50), 1), 200);
END $$;

-- Everyone who has added samples, most recently active first (the app's
-- User Moderation table).
CREATE OR REPLACE FUNCTION admin_api.contributors(p_firebase_uid TEXT, p_email TEXT, p_limit INT)
RETURNS TABLE (
  user_id TEXT, n_samples INT, n_flagged INT, first_seen TIMESTAMPTZ, last_seen TIMESTAMPTZ,
  disabled BOOLEAN, disable_reason TEXT)
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
#variable_conflict use_column
DECLARE
  v_actor TEXT := admin_api._actor(p_firebase_uid, p_email);
BEGIN
  RETURN QUERY
  SELECT s.created_by::TEXT, count(*)::INT, (count(*) FILTER (WHERE s.flagged))::INT,
         min(s.created_at) AT TIME ZONE 'UTC', max(s.created_at) AT TIME ZONE 'UTC',
         d.user_id IS NOT NULL, d.reason::TEXT
    FROM soil_samples s
    LEFT JOIN disabled_users d ON d.user_id = s.created_by
   WHERE s.created_by IS NOT NULL
   GROUP BY s.created_by, d.user_id, d.reason
   ORDER BY max(s.created_at) DESC
   LIMIT least(greatest(coalesce(p_limit, 500), 1), 2000);
END $$;

-- Disabled accounts, most recent first (db_list_disabled_users in the app)
CREATE OR REPLACE FUNCTION admin_api.disabled_accounts(p_firebase_uid TEXT, p_email TEXT)
RETURNS TABLE (
  user_id TEXT, disabled_at TIMESTAMPTZ, disabled_by TEXT, reason TEXT, n_samples INT, n_flagged INT)
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
#variable_conflict use_column
DECLARE
  v_actor TEXT := admin_api._actor(p_firebase_uid, p_email);
BEGIN
  RETURN QUERY
  SELECT d.user_id::TEXT, d.disabled_at, d.disabled_by::TEXT, d.reason::TEXT,
         (SELECT count(*)::INT FROM soil_samples s WHERE s.created_by = d.user_id),
         (SELECT count(*)::INT FROM soil_samples s WHERE s.created_by = d.user_id AND s.flagged)
    FROM disabled_users d
   ORDER BY d.disabled_at DESC NULLS LAST, d.user_id;
END $$;

-- The audit log, newest first. p_search matches the acting user id exactly,
-- the record id, or any text in the details (where the app and these
-- functions name the target user). Page with p_before_id.
CREATE OR REPLACE FUNCTION admin_api.audit_entries(
  p_firebase_uid TEXT, p_email TEXT, p_action TEXT, p_search TEXT, p_before_id INT, p_limit INT)
RETURNS TABLE (
  id INT, created_at TIMESTAMPTZ, action TEXT, table_name TEXT, record_id INT, user_id TEXT, details TEXT)
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
#variable_conflict use_column
DECLARE
  v_actor  TEXT := admin_api._actor(p_firebase_uid, p_email);
  v_action TEXT := NULLIF(btrim(p_action), '');
  v_search TEXT := NULLIF(btrim(p_search), '');
BEGIN
  RETURN QUERY
  SELECT a.id, a.created_at AT TIME ZONE 'UTC', a.action::TEXT, a.table_name::TEXT, a.record_id,
         a.user_id::TEXT, a.details::TEXT
    FROM audit_log a
   WHERE (v_action IS NULL OR a.action = v_action)
     AND (v_search IS NULL
          OR a.user_id = v_search
          OR a.record_id::TEXT = v_search
          OR a.details ILIKE '%' || replace(replace(replace(v_search, '\', '\\'), '%', '\%'), '_', '\_') || '%')
     AND (p_before_id IS NULL OR a.id < p_before_id)
   ORDER BY a.id DESC
   LIMIT least(greatest(coalesce(p_limit, 100), 1), 500);
END $$;

-- -----------------------------------------------------------------------------
-- Changes. Each returns {"status": "ok", ...} or {"status": "<reason>"} and
-- writes audit_log only when something actually changed.
-- -----------------------------------------------------------------------------

-- Flag one sample (db_flag_sample in the app). Flagging a flagged sample with
-- a new reason replaces the reason.
CREATE OR REPLACE FUNCTION admin_api.flag_sample(p_firebase_uid TEXT, p_email TEXT, p_id INT, p_reason TEXT)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_actor  TEXT := admin_api._actor(p_firebase_uid, p_email);
  v_reason TEXT := coalesce(NULLIF(btrim(p_reason), ''), 'Flagged by admin');
BEGIN
  IF length(v_reason) > 500 THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Keep the reason under 500 characters.');
  END IF;
  UPDATE soil_samples SET flagged = TRUE, flag_reason = v_reason
   WHERE id = p_id AND (flagged IS NOT TRUE OR flag_reason IS DISTINCT FROM v_reason);
  IF NOT FOUND THEN
    IF EXISTS (SELECT 1 FROM soil_samples WHERE id = p_id) THEN
      RETURN jsonb_build_object('status', 'ok', 'changed', FALSE);
    END IF;
    RETURN jsonb_build_object('status', 'not_found');
  END IF;
  INSERT INTO audit_log (action, table_name, record_id, user_id, details)
  VALUES ('flag', 'soil_samples', p_id, v_actor, 'Flagged: ' || v_reason || ' (edaphicflora.org)');
  RETURN jsonb_build_object('status', 'ok', 'changed', TRUE);
END $$;

-- Clear a flag (db_unflag_sample in the app)
CREATE OR REPLACE FUNCTION admin_api.unflag_sample(p_firebase_uid TEXT, p_email TEXT, p_id INT)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_actor TEXT := admin_api._actor(p_firebase_uid, p_email);
BEGIN
  UPDATE soil_samples SET flagged = FALSE, flag_reason = NULL
   WHERE id = p_id AND (flagged IS TRUE OR flag_reason IS NOT NULL);
  IF NOT FOUND THEN
    IF EXISTS (SELECT 1 FROM soil_samples WHERE id = p_id) THEN
      RETURN jsonb_build_object('status', 'ok', 'changed', FALSE);
    END IF;
    RETURN jsonb_build_object('status', 'not_found');
  END IF;
  INSERT INTO audit_log (action, table_name, record_id, user_id, details)
  VALUES ('unflag', 'soil_samples', p_id, v_actor, 'Unflagged (edaphicflora.org)');
  RETURN jsonb_build_object('status', 'ok', 'changed', TRUE);
END $$;

-- Flag every not-yet-flagged sample from one contributor
-- (db_flag_samples_by_user in the app). Existing reasons are kept.
CREATE OR REPLACE FUNCTION admin_api.flag_user_samples(p_firebase_uid TEXT, p_email TEXT, p_user_id TEXT, p_reason TEXT)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_actor  TEXT := admin_api._actor(p_firebase_uid, p_email);
  v_reason TEXT := coalesce(NULLIF(btrim(p_reason), ''), 'Bulk flagged by admin');
  v_n      INT;
BEGIN
  IF NOT admin_api._known_user(p_user_id) THEN
    RETURN jsonb_build_object('status', 'unknown_user');
  END IF;
  IF length(v_reason) > 500 THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Keep the reason under 500 characters.');
  END IF;
  UPDATE soil_samples SET flagged = TRUE, flag_reason = coalesce(flag_reason, v_reason)
   WHERE created_by = p_user_id AND flagged IS NOT TRUE;
  GET DIAGNOSTICS v_n = ROW_COUNT;
  IF v_n > 0 THEN
    INSERT INTO audit_log (action, table_name, record_id, user_id, details)
    VALUES ('bulk_flag', 'soil_samples', NULL, v_actor,
            format('Bulk-flagged %s samples from user %s: %s (edaphicflora.org)', v_n, p_user_id, v_reason));
  END IF;
  RETURN jsonb_build_object('status', 'ok', 'flagged', v_n);
END $$;

-- Disable a contributor (db_disable_user in the app): no new samples or
-- report reading, and their samples drop off public pages at the next
-- rebuild. Optionally flag all their samples too, so they stay hidden if the
-- account is re-enabled. Disabling a disabled account updates the reason.
CREATE OR REPLACE FUNCTION admin_api.disable_user(
  p_firebase_uid TEXT, p_email TEXT, p_user_id TEXT, p_reason TEXT, p_flag_samples BOOLEAN)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_actor  TEXT := admin_api._actor(p_firebase_uid, p_email);
  v_reason TEXT := coalesce(NULLIF(btrim(p_reason), ''), 'Disabled by admin');
  v_n      INT := 0;
BEGIN
  IF p_user_id = v_actor OR p_user_id = p_firebase_uid THEN
    RETURN jsonb_build_object('status', 'self', 'error', 'You cannot disable your own account.');
  END IF;
  IF NOT admin_api._known_user(p_user_id) THEN
    RETURN jsonb_build_object('status', 'unknown_user');
  END IF;
  IF length(v_reason) > 500 THEN
    RETURN jsonb_build_object('status', 'invalid', 'error', 'Keep the reason under 500 characters.');
  END IF;

  INSERT INTO disabled_users (user_id, disabled_by, reason) VALUES (p_user_id, v_actor, v_reason)
  ON CONFLICT (user_id) DO UPDATE SET
    disabled_by = EXCLUDED.disabled_by, reason = EXCLUDED.reason, disabled_at = now();
  INSERT INTO audit_log (action, table_name, record_id, user_id, details)
  VALUES ('disable_user', 'disabled_users', NULL, v_actor,
          format('Disabled user %s: %s (edaphicflora.org)', p_user_id, v_reason));

  IF coalesce(p_flag_samples, FALSE) THEN
    UPDATE soil_samples SET flagged = TRUE, flag_reason = coalesce(flag_reason, 'User disabled: ' || v_reason)
     WHERE created_by = p_user_id AND flagged IS NOT TRUE;
    GET DIAGNOSTICS v_n = ROW_COUNT;
    IF v_n > 0 THEN
      INSERT INTO audit_log (action, table_name, record_id, user_id, details)
      VALUES ('bulk_flag', 'soil_samples', NULL, v_actor,
              format('Bulk-flagged %s samples from user %s: User disabled: %s (edaphicflora.org)', v_n, p_user_id, v_reason));
    END IF;
  END IF;
  RETURN jsonb_build_object('status', 'ok', 'flagged', v_n);
END $$;

-- Re-enable a contributor (db_enable_user in the app). Samples flagged while
-- they were disabled stay flagged.
CREATE OR REPLACE FUNCTION admin_api.enable_user(p_firebase_uid TEXT, p_email TEXT, p_user_id TEXT)
RETURNS JSONB
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_actor TEXT := admin_api._actor(p_firebase_uid, p_email);
BEGIN
  DELETE FROM disabled_users WHERE user_id = p_user_id;
  IF NOT FOUND THEN
    RETURN jsonb_build_object('status', 'not_disabled');
  END IF;
  INSERT INTO audit_log (action, table_name, record_id, user_id, details)
  VALUES ('enable_user', 'disabled_users', NULL, v_actor,
          format('Re-enabled user %s (edaphicflora.org)', p_user_id));
  RETURN jsonb_build_object('status', 'ok');
END $$;

-- Only the site_admin login may call these, and not the two helpers (the
-- functions call those with the owner's rights). site_writer and site_reader
-- get nothing in this schema.
REVOKE ALL ON ALL FUNCTIONS IN SCHEMA admin_api FROM PUBLIC;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'site_admin') THEN
    GRANT USAGE ON SCHEMA admin_api TO site_admin;
    GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA admin_api TO site_admin;
    REVOKE EXECUTE ON FUNCTION admin_api._actor(TEXT, TEXT), admin_api._known_user(TEXT) FROM site_admin;
  END IF;
END $$;

COMMIT;

-- =============================================================================
-- ONE TIME: the admin page's database login and the database's admin list.
-- Create the login with SQL, not in the Neon Console (Console roles get
-- neon_superuser). Use a long random password, different from site_writer's.
--
--   CREATE ROLE site_admin WITH LOGIN PASSWORD 'replace-with-a-long-random-password';
--   REVOKE ALL ON ALL TABLES IN SCHEMA public FROM site_admin;
--
-- Then run this file again: it grants site_admin exactly what it needs.
--
-- Add each admin's sign-in email (the same addresses as ADMIN_EMAILS):
--
--   INSERT INTO site_admins (email_lower, note) VALUES (lower('you@example.com'), 'site owner');
--
-- Remove an admin: DELETE FROM site_admins WHERE email_lower = 'them@example.com';
-- (and take them out of ADMIN_EMAILS in Netlify).
--
-- Check it:
--   SET ROLE site_admin;
--   SELECT count(*) FROM soil_samples;                                -- permission denied
--   SELECT admin_api.whoami('check', 'you@example.com');              -- works
--   SELECT admin_api.whoami('check', 'someone-else@example.com');     -- ERROR: not a site admin
--   RESET ROLE;
--
-- Then add it to Netlify as SITE_ADMIN_DATABASE_URL:
--   postgresql://site_admin:<password>@<neon-host>/<db>?sslmode=require
-- and set ADMIN_EMAILS (comma-separated, any case), e.g. you@example.com
-- =============================================================================
