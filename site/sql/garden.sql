-- =============================================================================
-- Edaphic Flora: My Garden on edaphicflora.org (signed-in wildlife dashboard)
-- =============================================================================
-- Run as the database owner AFTER public_views.sql and contribute.sql (it uses
-- public_site.species_key, ref_species_accepted and site_users), on the Neon
-- dev branch first, then main. Safe to re-run.
--
-- What it returns is what the Shiny app's My Garden tab showed signed-in
-- people (app/R/mod_my_garden.R, app/R/db.R), and nothing more:
--   * the person's own plants (species in their soil samples), sorted into
--     native or not recorded, introduced and invasive for their home state
--   * per wildlife group (moths, butterflies, specialist bees, birds) and per
--     family: how many species their plants support out of how many there
--     are, counting only species recorded in their home state when that state
--     has enough occurrence data (50 or more species)
--   * for each family, which of their plants support it and up to 3 plant
--     genera that would add the most
--   * up to 10 recommended plant genera native to their state, per plant type
-- The wildlife tables are proprietary consulting data. Like public_views.sql,
-- this never returns wildlife species names, ids, interaction lists or
-- sources: only counts, family names and plant names.
--
-- Rules carried over from the app:
--   * invasive plants (federal noxious, or listed in the home state) are
--     credited with no wildlife
--   * native plants, and plants with no record in the state, are credited at
--     genus level (host plant records are mostly kept by genus)
--   * introduced plants are credited only with wildlife recorded on that
--     exact species, and are left out of the per-family plant lists
--
-- The site_writer login can call site_api.my_garden only. The helpers live in
-- garden_private, which it cannot use.
-- =============================================================================

BEGIN;

CREATE SCHEMA IF NOT EXISTS garden_private;
REVOKE ALL ON SCHEMA garden_private FROM PUBLIC;

-- -----------------------------------------------------------------------------
-- Native status of each garden species in a state (any state when p_state is
-- NULL). Names are matched to USDA taxa by genus and epithet, then through
-- USDA synonyms for names that match nothing directly.
--   status: invasive   federal noxious, or on the state's invasive list
--           native     native (or native and introduced) in the state
--           introduced introduced in the state, or, with no state record,
--                      introduced to North America and not native to it
--           unknown    no record either way
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION garden_private.classify(p_species TEXT[], p_state TEXT)
RETURNS TABLE (species TEXT, taxon_ids INT[], genera TEXT[], status TEXT, designation TEXT, federal BOOLEAN)
LANGUAGE sql STABLE SET search_path = public, pg_temp AS $$
  WITH g AS (
    SELECT DISTINCT s AS species, public_site.species_key(s) AS skey
      FROM unnest(p_species) AS s
     WHERE s IS NOT NULL AND btrim(s) <> ''
  ),
  -- USDA writes hybrids with the sign attached to the epithet ("Carya ×brownii")
  keys AS (
    SELECT DISTINCT g.species, k
      FROM g, LATERAL unnest(ARRAY[
        lower(split_part(btrim(g.species), ' ', 1) || ' ' || split_part(btrim(g.species), ' ', 2)),
        g.skey,
        split_part(g.skey, ' ', 1) || ' ×' || split_part(g.skey, ' ', 2)]) AS k
  ),
  direct AS (
    SELECT DISTINCT keys.species, t.id AS taxon_id
      FROM keys
      JOIN ref_taxon t
        ON lower(split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2)) = keys.k
  ),
  syn AS (
    SELECT DISTINCT keys.species, s.taxon_id
      FROM keys
      JOIN ref_synonym s
        ON lower(split_part(s.synonym_name, ' ', 1) || ' ' || split_part(s.synonym_name, ' ', 2)) = keys.k
     WHERE NOT EXISTS (SELECT 1 FROM direct d WHERE d.species = keys.species)
  ),
  taxa AS (
    SELECT g.species,
           coalesce(array_agg(DISTINCT m.taxon_id) FILTER (WHERE m.taxon_id IS NOT NULL), '{}') AS ids
      FROM g
      LEFT JOIN (SELECT * FROM direct UNION ALL SELECT * FROM syn) m ON m.species = g.species
     GROUP BY g.species
  ),
  facts AS (
    SELECT tx.species, tx.ids,
      (SELECT max(CASE sd.native_status WHEN 'Native' THEN 3 WHEN 'Both' THEN 2 WHEN 'Introduced' THEN 1 ELSE 0 END)
         FROM ref_state_distribution sd
        WHERE sd.taxon_id = ANY(tx.ids) AND (p_state IS NULL OR sd.state_code = p_state)) AS state_rank,
      -- same parsing as parseNativeStatusNA (src/lib/names.js) and the app's parse_native_status_na
      (SELECT bool_or(u.native_status ~ ', N(,|$)' AND strpos(u.native_status, 'Native') > 0)
         FROM ref_usda_traits u WHERE u.taxon_id = ANY(tx.ids)) AS na_native,
      (SELECT bool_or(u.native_status ~ ', I(,|$)' AND strpos(u.native_status, 'Introduced') > 0)
         FROM ref_usda_traits u WHERE u.taxon_id = ANY(tx.ids)) AS na_introduced,
      (SELECT min(n.designation) FROM ref_noxious_invasive n
        WHERE n.taxon_id = ANY(tx.ids) AND coalesce(NULLIF(btrim(n.state_code), ''), 'US') = 'US') AS federal_designation,
      EXISTS (SELECT 1 FROM ref_noxious_invasive n
               WHERE n.taxon_id = ANY(tx.ids) AND coalesce(NULLIF(btrim(n.state_code), ''), 'US') = 'US') AS federal,
      (SELECT min(n.designation) FROM ref_noxious_invasive n
        WHERE n.taxon_id = ANY(tx.ids) AND n.state_code = p_state) AS state_designation,
      EXISTS (SELECT 1 FROM ref_noxious_invasive n
               WHERE n.taxon_id = ANY(tx.ids) AND n.state_code = p_state) AS state_listed
    FROM taxa tx
  )
  SELECT f.species,
         f.ids,
         ARRAY(SELECT DISTINCT x FROM unnest(
           ARRAY(SELECT lower(split_part(t.scientific_name, ' ', 1)) FROM ref_taxon t WHERE t.id = ANY(f.ids))
           || lower(split_part(public_site.strip_hybrid(f.species), ' ', 1))) AS x),
         CASE
           WHEN f.federal OR f.state_listed THEN 'invasive'
           WHEN f.state_rank >= 2 THEN 'native'
           WHEN f.state_rank = 1 THEN 'introduced'
           WHEN f.state_rank IS NULL AND f.na_introduced AND NOT coalesce(f.na_native, FALSE) THEN 'introduced'
           ELSE 'unknown'
         END,
         CASE WHEN f.federal THEN coalesce(f.federal_designation, 'Federal noxious weed')
              WHEN f.state_listed THEN f.state_designation END,
         f.federal
    FROM facts f
$$;

-- The state used to filter wildlife: the home state, if it has occurrence
-- records for at least 50 wildlife species (same threshold as the app).
CREATE OR REPLACE FUNCTION garden_private.presence_state(p_state TEXT)
RETURNS TEXT
LANGUAGE sql STABLE SET search_path = public, pg_temp AS $$
  SELECT p_state
   WHERE p_state IS NOT NULL
     AND (SELECT count(*) FROM ref_wildlife_state_presence WHERE state_code = p_state) >= 50
$$;

-- A common name for each genus, from USDA (the app's get_genus_common_name):
-- the genus-level record ("Asclepias L." is milkweed), otherwise the last word
-- most of its species share ("... sedge"), otherwise the shortest species name.
-- The site adds its own overrides and plurals (src/lib/garden.js).
CREATE OR REPLACE FUNCTION garden_private.genus_common_names(p_genera TEXT[])
RETURNS TABLE (genus TEXT, common_name TEXT)
LANGUAGE sql STABLE SET search_path = public, pg_temp AS $$
  WITH want AS (
    SELECT DISTINCT lower(btrim(x)) AS genus_l FROM unnest(p_genera) AS x WHERE NULLIF(btrim(x), '') IS NOT NULL
  ),
  names AS (
    SELECT DISTINCT ON (gs) genus, w2, cn, scientific_name
      FROM (
        SELECT split_part(t.scientific_name, ' ', 1) AS genus,
               split_part(t.scientific_name, ' ', 1) || ' ' || split_part(t.scientific_name, ' ', 2) AS gs,
               split_part(t.scientific_name, ' ', 2) AS w2,
               lower(btrim(u.common_name)) AS cn,
               t.scientific_name,
               CASE WHEN t.scientific_name ~ ' (var|ssp|subsp|f)\.? ' THEN 1 ELSE 0 END AS rank_order
          FROM ref_taxon t
          JOIN ref_usda_traits u ON u.taxon_id = t.id
          JOIN want w ON w.genus_l = lower(split_part(t.scientific_name, ' ', 1))
         WHERE NULLIF(btrim(u.common_name), '') IS NOT NULL
      ) x
     ORDER BY gs, rank_order, scientific_name
  ),
  genus_level AS (
    SELECT DISTINCT ON (genus) genus, cn FROM names WHERE w2 LIKE '%.%' ORDER BY genus, scientific_name
  ),
  last_word AS (
    SELECT DISTINCT ON (genus) genus, word, n, total
      FROM (
        SELECT genus, regexp_replace(cn, '^.*\s', '') AS word, count(*) AS n,
               sum(count(*)) OVER (PARTITION BY genus) AS total
          FROM names
         GROUP BY genus, regexp_replace(cn, '^.*\s', '')
      ) w
     ORDER BY genus, n DESC, word
  ),
  shortest AS (
    SELECT DISTINCT ON (genus) genus, cn FROM names ORDER BY genus, length(cn), cn
  )
  SELECT s.genus,
         coalesce(gl.cn, CASE WHEN lw.n >= greatest(lw.total * 0.5, 1) THEN lw.word END, s.cn)
    FROM shortest s
    LEFT JOIN genus_level gl ON gl.genus = s.genus
    LEFT JOIN last_word lw ON lw.genus = s.genus
$$;

-- -----------------------------------------------------------------------------
-- site_api.my_garden: everything the My Garden page shows, as one JSON value.
--
-- {"status": "ok", "state": "CT", "city": "New Haven", "state_filtered": true,
--  "plants":  [{"species", "common_name", "status", "designation", "federal",
--               "lep", "bee", "bird"}],          (counts only for introduced)
--  "supported": 312,                           wildlife species, all groups
--  "groups":  [{"type": "Butterfly", "covered", "total",
--               "families": [{"family", "covered", "total",
--                             "plants": [species], "add": [genus]}]}],
--  "recs":    {"All": [{"genus", "total", "lep", "bee", "bird", "keystone",
--                       "life_form", "natives": [{"species", "common_name"}],
--                       "native_count"}], "Tree": [...], ...},
--  "genus_names": {"Quercus": "oak", ...}}
-- or {"status": "unknown_user"}.
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION site_api.my_garden(p_user_id TEXT)
RETURNS JSONB
LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path = public, pg_temp AS $$
DECLARE
  v_species  TEXT[];
  v_state    TEXT;
  v_city     TEXT;
  v_presence TEXT;
  v_out      JSONB;
BEGIN
  IF p_user_id IS NULL OR NOT EXISTS (SELECT 1 FROM site_users WHERE user_id = p_user_id) THEN
    RETURN jsonb_build_object('status', 'unknown_user');
  END IF;

  SELECT array_agg(DISTINCT btrim(s.species) ORDER BY btrim(s.species)) INTO v_species
    FROM soil_samples s
   WHERE s.created_by = p_user_id AND s.species IS NOT NULL AND btrim(s.species) <> '';

  SELECT upper(NULLIF(btrim(p.home_state), '')), NULLIF(btrim(p.home_city), '') INTO v_state, v_city
    FROM user_preferences p
   WHERE p.user_id = p_user_id;
  IF v_state IS NOT NULL AND v_state !~ '^[A-Z]{2}$' THEN
    v_state := NULL;
    v_city := NULL;
  END IF;
  v_presence := garden_private.presence_state(v_state);

  IF v_species IS NULL THEN
    RETURN jsonb_build_object(
      'status', 'ok', 'state', v_state, 'city', v_city, 'state_filtered', v_presence IS NOT NULL,
      'plants', '[]'::jsonb, 'supported', 0, 'groups', '[]'::jsonb, 'recs', '{}'::jsonb, 'genus_names', '{}'::jsonb);
  END IF;

  WITH cls AS MATERIALIZED (
    SELECT * FROM garden_private.classify(v_species, v_state)
  ),
  -- every wildlife species in scope: recorded in the home state when it has enough data
  pool AS MATERIALIZED (
    SELECT DISTINCT ws.wildlife_id,
           CASE WHEN ws.wildlife_type = 'Skipper' THEN 'Butterfly' ELSE ws.wildlife_type END AS wtype,
           coalesce(NULLIF(btrim(ws.family), ''), 'Unknown') AS family
      FROM ref_wildlife_species ws
     WHERE ws.wildlife_type IN ('Moth', 'Butterfly', 'Skipper', 'Bee', 'Bird')
       AND (v_presence IS NULL OR EXISTS (
             SELECT 1 FROM ref_wildlife_state_presence p
              WHERE p.wildlife_id = ws.wildlife_id AND p.state_code = v_presence))
  ),
  wplants AS MATERIALIZED (
    SELECT wp.species_code, wp.taxon_id,
           split_part(t.scientific_name, ' ', 1) AS genus,
           lower(split_part(t.scientific_name, ' ', 1)) AS genus_l,
           wp.life_form, coalesce(wp.is_keystone_genus, FALSE) AS keystone
      FROM ref_wildlife_plants wp
      JOIN ref_taxon t ON t.id = wp.taxon_id
  ),
  links AS MATERIALIZED (
    SELECT DISTINCT wi.plant_species_code AS species_code, pl.wildlife_id, pl.wtype, pl.family
      FROM ref_wildlife_interactions wi
      JOIN pool pl ON pl.wildlife_id = wi.wildlife_id
  ),
  -- garden species -> the wildlife plant records they get credit for
  matched AS MATERIALIZED (
    SELECT c.species, c.status, p.species_code
      FROM cls c
      JOIN wplants p
        ON (c.status IN ('native', 'unknown') AND p.genus_l = ANY(c.genera))
        OR (c.status = 'introduced' AND p.taxon_id = ANY(c.taxon_ids))
  ),
  cov AS MATERIALIZED (
    SELECT DISTINCT m.species, m.status, l.wildlife_id, l.wtype, l.family
      FROM matched m
      JOIN links l ON l.species_code = m.species_code
  ),
  covered_ids AS MATERIALIZED (
    SELECT DISTINCT wildlife_id FROM cov
  ),
  fam AS (
    SELECT pl.wtype, pl.family, count(*)::int AS total, count(ci.wildlife_id)::int AS covered
      FROM pool pl
      LEFT JOIN covered_ids ci ON ci.wildlife_id = pl.wildlife_id
     GROUP BY pl.wtype, pl.family
  ),
  fam_plants AS (
    SELECT wtype, family, jsonb_agg(DISTINCT species ORDER BY species) AS plants
      FROM cov
     WHERE status <> 'introduced'
     GROUP BY wtype, family
  ),
  -- plants to add: wildlife plants not in the garden (natives of the home
  -- state when it is set), and the wildlife they would add that isn't
  -- supported yet
  gap_new AS MATERIALIZED (
    SELECT p.genus, p.life_form, p.keystone, l.wildlife_id, l.wtype, l.family
      FROM wplants p
      JOIN links l ON l.species_code = p.species_code
     WHERE NOT EXISTS (SELECT 1 FROM matched m WHERE m.species_code = p.species_code)
       AND NOT EXISTS (SELECT 1 FROM covered_ids ci WHERE ci.wildlife_id = l.wildlife_id)
       AND (v_state IS NULL OR EXISTS (
             SELECT 1 FROM ref_state_distribution sd
              WHERE sd.taxon_id = p.taxon_id AND sd.state_code = v_state
                AND sd.native_status IN ('Native', 'Both')))
  ),
  fam_gaps AS (
    SELECT wtype, family, jsonb_agg(genus ORDER BY rn) AS genera
      FROM (
        SELECT wtype, family, genus,
               row_number() OVER (PARTITION BY wtype, family
                                  ORDER BY count(DISTINCT wildlife_id) DESC, genus) AS rn
          FROM gap_new
         GROUP BY wtype, family, genus
      ) x
     WHERE rn <= 3
     GROUP BY wtype, family
  ),
  life_forms (category, life_form) AS (
    VALUES ('All', NULL::TEXT), ('Tree', 'Tree'), ('Shrub', 'Shrub'), ('Shrub', 'Shrub/small tree'),
           ('Perennial', 'Perennial'), ('Perennial', 'Perennial (ephemeral)'), ('Perennial', 'Groundcover'),
           ('Grass/Sedge', 'Grass'), ('Grass/Sedge', 'Sedge'), ('Vine', 'Vine'), ('Fern', 'Fern')
  ),
  recs AS MATERIALIZED (
    SELECT x.*, row_number() OVER (PARTITION BY category ORDER BY total DESC, genus) AS rn
      FROM (
        SELECT lf.category, g.genus,
               count(DISTINCT g.wildlife_id)::int AS total,
               count(DISTINCT g.wildlife_id) FILTER (WHERE g.wtype IN ('Moth', 'Butterfly'))::int AS lep,
               count(DISTINCT g.wildlife_id) FILTER (WHERE g.wtype = 'Bee')::int AS bee,
               count(DISTINCT g.wildlife_id) FILTER (WHERE g.wtype = 'Bird')::int AS bird,
               bool_or(g.keystone) AS keystone,
               mode() WITHIN GROUP (ORDER BY g.life_form) AS life_form
          FROM gap_new g
          JOIN life_forms lf ON lf.life_form IS NULL OR lf.life_form = g.life_form
         WHERE v_state IS NOT NULL
         GROUP BY lf.category, g.genus
      ) x
  ),
  rec_genera AS (
    SELECT DISTINCT genus FROM recs WHERE rn <= 10
  ),
  -- species of each recommended genus native to the home state (BONAP), one
  -- row per genus + epithet
  natives AS (
    SELECT genus,
           jsonb_agg(jsonb_build_object('species', species_name, 'common_name', common_name)
                     ORDER BY species_name) FILTER (WHERE rn <= 60) AS list,
           count(*)::int AS n
      FROM (
        SELECT y.*, row_number() OVER (PARTITION BY genus ORDER BY species_name) AS rn
          FROM (
            SELECT DISTINCT ON (k) genus, k, species_name, common_name
              FROM (
                SELECT rg.genus,
                       public_site.species_key(t.scientific_name) AS k,
                       public_site.species_label(t.scientific_name) AS species_name,
                       NULLIF(btrim(u.common_name), '') AS common_name,
                       CASE WHEN t.scientific_name ~ ' (var|ssp|subsp|f)\.? ' THEN 1 ELSE 0 END AS rank_order,
                       t.scientific_name
                  FROM rec_genera rg
                  JOIN ref_taxon t ON lower(split_part(t.scientific_name, ' ', 1)) = lower(rg.genus)
                  JOIN ref_state_distribution sd
                    ON sd.taxon_id = t.id AND sd.state_code = v_state AND sd.native_status IN ('Native', 'Both')
                  LEFT JOIN ref_usda_traits u ON u.taxon_id = t.id
                 WHERE split_part(t.scientific_name, ' ', 2) <> ''
                   AND split_part(t.scientific_name, ' ', 2) NOT LIKE '%[%'
                   AND split_part(t.scientific_name, ' ', 2) !~ '\.$'
              ) z
             ORDER BY k, rank_order, (common_name IS NULL), scientific_name
          ) y
      ) w
     GROUP BY genus
  ),
  needed_genera AS (
    SELECT split_part(public_site.strip_hybrid(species), ' ', 1) AS genus FROM cls
    UNION SELECT jsonb_array_elements_text(genera) FROM fam_gaps
    UNION SELECT genus FROM rec_genera
  ),
  gnames AS (
    SELECT * FROM garden_private.genus_common_names(ARRAY(SELECT genus FROM needed_genera))
  ),
  intro_counts AS (
    SELECT species,
           count(DISTINCT wildlife_id) FILTER (WHERE wtype IN ('Moth', 'Butterfly'))::int AS lep,
           count(DISTINCT wildlife_id) FILTER (WHERE wtype = 'Bee')::int AS bee,
           count(DISTINCT wildlife_id) FILTER (WHERE wtype = 'Bird')::int AS bird
      FROM cov
     WHERE status = 'introduced'
     GROUP BY species
  )
  SELECT jsonb_build_object(
    'status', 'ok',
    'state', v_state,
    'city', v_city,
    'state_filtered', v_presence IS NOT NULL,
    'plants', (
      SELECT coalesce(jsonb_agg(jsonb_build_object(
               'species', c.species,
               'common_name', coalesce(NULLIF(btrim(a.common_name), ''), (
                 SELECT NULLIF(btrim(u.common_name), '') FROM ref_usda_traits u
                  WHERE u.taxon_id = ANY(c.taxon_ids) AND NULLIF(btrim(u.common_name), '') IS NOT NULL
                  ORDER BY u.taxon_id LIMIT 1)),
               'status', c.status,
               'designation', c.designation,
               'federal', c.federal,
               'lep', CASE WHEN c.status = 'introduced' THEN coalesce(ic.lep, 0) END,
               'bee', CASE WHEN c.status = 'introduced' THEN coalesce(ic.bee, 0) END,
               'bird', CASE WHEN c.status = 'introduced' THEN coalesce(ic.bird, 0) END
             ) ORDER BY c.species), '[]'::jsonb)
        FROM cls c
        LEFT JOIN ref_species_accepted a ON a.taxon_name = c.species
        LEFT JOIN intro_counts ic ON ic.species = c.species),
    'supported', (SELECT count(*)::int FROM covered_ids),
    'groups', (
      SELECT coalesce(jsonb_agg(jsonb_build_object(
               'type', g.wtype, 'covered', g.covered, 'total', g.total, 'families', g.families)
             ORDER BY array_position(ARRAY['Butterfly', 'Moth', 'Bee', 'Bird'], g.wtype)), '[]'::jsonb)
        FROM (
          SELECT f.wtype, sum(f.covered)::int AS covered, sum(f.total)::int AS total,
                 jsonb_agg(jsonb_build_object(
                   'family', f.family, 'covered', f.covered, 'total', f.total,
                   'plants', coalesce(fp.plants, '[]'::jsonb), 'add', coalesce(fg.genera, '[]'::jsonb))
                   ORDER BY f.covered DESC, f.total DESC, f.family) AS families
            FROM fam f
            LEFT JOIN fam_plants fp ON fp.wtype = f.wtype AND fp.family = f.family
            LEFT JOIN fam_gaps fg ON fg.wtype = f.wtype AND fg.family = f.family
           GROUP BY f.wtype
        ) g),
    'recs', (
      SELECT coalesce(jsonb_object_agg(category, list), '{}'::jsonb)
        FROM (
          SELECT r.category,
                 jsonb_agg(jsonb_build_object(
                   'genus', r.genus, 'total', r.total, 'lep', r.lep, 'bee', r.bee, 'bird', r.bird,
                   'keystone', r.keystone, 'life_form', r.life_form,
                   'natives', coalesce(n.list, '[]'::jsonb), 'native_count', coalesce(n.n, 0))
                   ORDER BY r.rn) AS list
            FROM recs r
            LEFT JOIN natives n ON n.genus = r.genus
           WHERE r.rn <= 10
           GROUP BY r.category
        ) x),
    'genus_names', (
      SELECT coalesce(jsonb_object_agg(genus, common_name), '{}'::jsonb)
        FROM gnames WHERE common_name IS NOT NULL)
  ) INTO v_out;

  RETURN v_out;
END $$;

-- Only site_writer may call site_api.my_garden; nobody but the owner can use
-- the helpers.
REVOKE ALL ON ALL FUNCTIONS IN SCHEMA garden_private FROM PUBLIC;
REVOKE ALL ON FUNCTION site_api.my_garden(TEXT) FROM PUBLIC;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'site_writer') THEN
    GRANT EXECUTE ON FUNCTION site_api.my_garden(TEXT) TO site_writer;
  END IF;
END $$;

COMMIT;
