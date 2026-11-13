-- =============================================================================
-- Edaphic Flora public site: read-only views
-- =============================================================================
-- The website build reads ONLY from the public_site schema created here.
-- Every privacy rule for public data lives in this file, so the site code
-- never sees raw soil samples, contributor emails, or sample locations.
--
-- Rules enforced here:
--   * created_by (contributor email) is never exposed; only distinct counts
--   * location_lat / location_long, notes, photos and files are never exposed
--   * flagged samples are excluded
--   * samples from users in disabled_users are excluded
--   * every species with a sample shows its values, always with the count;
--     a range (10th to 90th percentile) needs min_range_samples values of that
--     measurement, and below that the individual readings (lo, hi) are shown
--   * wildlife data is limited to counts and highlight flags (no interaction
--     lists, no wildlife species names, no sources table)
--
-- How to run:
--   1. Run on a Neon dev branch first, as the database owner (the role that
--      owns soil_samples), not the shiny_app runtime user.
--   2. Check the output with:  SELECT * FROM public_site.soil_species_summary LIMIT 20;
--   3. Run on the main branch.
-- Safe to re-run: everything uses CREATE OR REPLACE.
-- =============================================================================

BEGIN;

CREATE SCHEMA IF NOT EXISTS public_site;

-- -----------------------------------------------------------------------------
-- Name helpers. A species is genus + epithet. Hybrid markers ("×", attached or
-- not, or a lone "x") are left out of the key, so "Iris × germanica",
-- "Iris x germanica" and "Iris germanica" are one species, and USDA's
-- "Viola ×esculenta" keys as "viola esculenta". The display name keeps the ×.
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION public_site.strip_hybrid(name text) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
  SELECT regexp_replace(
           regexp_replace(btrim(regexp_replace(name, '\s+', ' ', 'g')), '^(×|[xX] )\s*', ''),
           '^(\S+) (×|[xX] )\s*', '\1 ')
$$;

CREATE OR REPLACE FUNCTION public_site.species_key(name text) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
  SELECT lower(split_part(public_site.strip_hybrid(name), ' ', 1) || ' ' ||
               split_part(public_site.strip_hybrid(name), ' ', 2))
$$;

CREATE OR REPLACE FUNCTION public_site.species_label(name text) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
  SELECT CASE WHEN name ~ '^\s*(×|[xX]\s)' THEN '× ' ELSE '' END
      || split_part(public_site.strip_hybrid(name), ' ', 1) || ' '
      || CASE WHEN name ~ '^\s*\S+\s+(×|[xX]\s)' THEN '× ' ELSE '' END
      || split_part(public_site.strip_hybrid(name), ' ', 2)
$$;

-- -----------------------------------------------------------------------------
-- Thresholds. The public site shows every sample (1 and 1), with counts.
-- The Shiny app keeps its own MIN_SAMPLES_FOR_PUBLIC_STATS and
-- MIN_CONTRIBUTORS_FOR_PUBLIC_STATS in app/R/helpers.R.
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW public_site.settings AS
SELECT
  1::int AS min_samples,
  1::int AS min_contributors,
  3::int AS min_range_samples;

-- -----------------------------------------------------------------------------
-- Soil: one row per species (genus + epithet), aggregates gated by thresholds
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW public_site.soil_species_summary AS
WITH clean AS (
  SELECT
    public_site.species_key(s.species)   AS species_key,
    public_site.species_label(s.species) AS species_name,
    s.created_by,
    CASE WHEN s.ph BETWEEN 3 AND 10              THEN s.ph END              AS ph,
    CASE WHEN s.organic_matter BETWEEN 0 AND 100 THEN s.organic_matter END  AS om,
    CASE WHEN s.texture_sand BETWEEN 0 AND 100
          AND s.texture_silt BETWEEN 0 AND 100
          AND s.texture_clay BETWEEN 0 AND 100
          AND (s.texture_sand + s.texture_silt + s.texture_clay) BETWEEN 90 AND 110
         THEN true ELSE false END                                           AS has_texture,
    s.texture_sand, s.texture_silt, s.texture_clay,
    NULLIF(btrim(s.texture_class), '')                                      AS texture_class,
    CASE WHEN s.phosphorus_ppm >= 0 THEN s.phosphorus_ppm END               AS p,
    CASE WHEN s.potassium_ppm  >= 0 THEN s.potassium_ppm  END               AS k,
    CASE WHEN s.calcium_ppm    >= 0 THEN s.calcium_ppm    END               AS ca,
    CASE WHEN s.magnesium_ppm  >= 0 THEN s.magnesium_ppm  END               AS mg
  FROM soil_samples s
  WHERE (s.flagged IS NULL OR s.flagged = FALSE)
    AND s.species IS NOT NULL
    AND btrim(s.species) <> ''
    AND NOT EXISTS (SELECT 1 FROM disabled_users d WHERE d.user_id = s.created_by)
),
agg AS (
  SELECT
    species_key,
    mode() WITHIN GROUP (ORDER BY species_name)                         AS species_name,
    count(*)::int                                                       AS sample_count,
    count(DISTINCT created_by)::int                                     AS contributor_count,

    count(ph)::int                                                      AS ph_n,
    percentile_cont(0.1) WITHIN GROUP (ORDER BY ph)                     AS ph_p10,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY ph)                     AS ph_p50,
    percentile_cont(0.9) WITHIN GROUP (ORDER BY ph)                     AS ph_p90,
    min(ph)                                                             AS ph_min,
    max(ph)                                                             AS ph_max,

    count(om)::int                                                      AS om_n,
    percentile_cont(0.1) WITHIN GROUP (ORDER BY om)                     AS om_p10,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY om)                     AS om_p50,
    percentile_cont(0.9) WITHIN GROUP (ORDER BY om)                     AS om_p90,
    min(om)                                                             AS om_min,
    max(om)                                                             AS om_max,

    count(*) FILTER (WHERE has_texture)::int                            AS texture_n,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY texture_sand) FILTER (WHERE has_texture) AS sand_p50,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY texture_silt) FILTER (WHERE has_texture) AS silt_p50,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY texture_clay) FILTER (WHERE has_texture) AS clay_p50,
    mode() WITHIN GROUP (ORDER BY texture_class)                        AS texture_class_mode,

    count(p)::int  AS p_n,  percentile_cont(0.5) WITHIN GROUP (ORDER BY p)  AS p_p50,
    count(k)::int  AS k_n,  percentile_cont(0.5) WITHIN GROUP (ORDER BY k)  AS k_p50,
    count(ca)::int AS ca_n, percentile_cont(0.5) WITHIN GROUP (ORDER BY ca) AS ca_p50,
    count(mg)::int AS mg_n, percentile_cont(0.5) WITHIN GROUP (ORDER BY mg) AS mg_p50
  FROM clean
  GROUP BY species_key
),
gated AS (
  SELECT
    a.*,
    (a.sample_count >= st.min_samples AND a.contributor_count >= st.min_contributors) AS meets_threshold,
    st.min_samples,
    st.min_range_samples
  FROM agg a CROSS JOIN public_site.settings st
)
SELECT
  species_key,
  species_name,
  sample_count,
  contributor_count,
  meets_threshold,

  ph_n,
  CASE WHEN meets_threshold AND ph_n >= min_range_samples THEN round(ph_p10::numeric, 1) END AS ph_p10,
  CASE WHEN meets_threshold AND ph_n >= min_samples       THEN round(ph_p50::numeric, 1) END AS ph_p50,
  CASE WHEN meets_threshold AND ph_n >= min_range_samples THEN round(ph_p90::numeric, 1) END AS ph_p90,

  om_n,
  CASE WHEN meets_threshold AND om_n >= min_range_samples THEN round(om_p10::numeric, 1) END AS om_p10,
  CASE WHEN meets_threshold AND om_n >= min_samples       THEN round(om_p50::numeric, 1) END AS om_p50,
  CASE WHEN meets_threshold AND om_n >= min_range_samples THEN round(om_p90::numeric, 1) END AS om_p90,

  texture_n,
  CASE WHEN meets_threshold AND texture_n >= min_samples THEN round(sand_p50::numeric) END AS sand_p50,
  CASE WHEN meets_threshold AND texture_n >= min_samples THEN round(silt_p50::numeric) END AS silt_p50,
  CASE WHEN meets_threshold AND texture_n >= min_samples THEN round(clay_p50::numeric) END AS clay_p50,
  CASE WHEN meets_threshold AND texture_n >= min_samples THEN texture_class_mode END       AS texture_class_mode,

  p_n,  CASE WHEN meets_threshold AND p_n  >= min_samples THEN round(p_p50::numeric)  END AS p_p50,
  k_n,  CASE WHEN meets_threshold AND k_n  >= min_samples THEN round(k_p50::numeric)  END AS k_p50,
  ca_n, CASE WHEN meets_threshold AND ca_n >= min_samples THEN round(ca_p50::numeric) END AS ca_p50,
  mg_n, CASE WHEN meets_threshold AND mg_n >= min_samples THEN round(mg_p50::numeric) END AS mg_p50,

  -- The readings themselves while there are too few for a range (1 or 2)
  CASE WHEN meets_threshold AND ph_n BETWEEN min_samples AND min_range_samples - 1 THEN round(ph_min::numeric, 1) END AS ph_lo,
  CASE WHEN meets_threshold AND ph_n BETWEEN min_samples AND min_range_samples - 1 THEN round(ph_max::numeric, 1) END AS ph_hi,
  CASE WHEN meets_threshold AND om_n BETWEEN min_samples AND min_range_samples - 1 THEN round(om_min::numeric, 1) END AS om_lo,
  CASE WHEN meets_threshold AND om_n BETWEEN min_samples AND min_range_samples - 1 THEN round(om_max::numeric, 1) END AS om_hi
FROM gated;

COMMENT ON VIEW public_site.soil_species_summary IS
  'Public soil values per species, always with sample counts. Ranges need public_site.settings.min_range_samples values. No contributor identity or location.';

-- -----------------------------------------------------------------------------
-- Reference: one row per species (genus + epithet), preferring the
-- species-level record over varieties and subspecies
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW public_site.species_reference AS
SELECT DISTINCT ON (species_key)
  species_key,
  taxon_id,
  scientific_name,
  usda_symbol,
  family,
  common_name,
  duration,
  growth_habit,
  native_status_na,
  soil_ph_min,
  soil_ph_max,
  moisture_use,
  shade_tolerance,
  drought_tolerance,
  salinity_tolerance,
  bloom_period
FROM (
  SELECT
    public_site.species_key(t.scientific_name)   AS species_key,
    t.id AS taxon_id,
    public_site.species_label(t.scientific_name) AS scientific_name,
    t.usda_symbol,
    t.family,
    NULLIF(btrim(u.common_name), '') AS common_name,
    u.duration,
    u.growth_habit,
    u.native_status                  AS native_status_na,
    u.soil_ph_min,
    u.soil_ph_max,
    u.moisture_use,
    u.shade_tolerance,
    u.drought_tolerance,
    u.salinity_tolerance,
    u.bloom_period,
    CASE WHEN t.scientific_name ~ ' (var|ssp|subsp|f)\.? ' THEN 1 ELSE 0 END AS rank_order,
    CASE WHEN u.taxon_id IS NULL THEN 1 ELSE 0 END                           AS no_traits
  FROM ref_taxon t
  LEFT JOIN ref_usda_traits u ON u.taxon_id = t.id
  WHERE split_part(public_site.strip_hybrid(t.scientific_name), ' ', 2) <> ''
    AND split_part(public_site.strip_hybrid(t.scientific_name), ' ', 2) NOT LIKE '%[%'
) x
ORDER BY species_key, rank_order, no_traits, scientific_name;

-- -----------------------------------------------------------------------------
-- State status: one row per species + state.
-- Native beats Both beats Introduced when varieties disagree.
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW public_site.species_state_status AS
SELECT
  public_site.species_key(t.scientific_name) AS species_key,
  sd.state_code,
  CASE max(CASE sd.native_status WHEN 'Native' THEN 3 WHEN 'Both' THEN 2 WHEN 'Introduced' THEN 1 ELSE 0 END)
    WHEN 3 THEN 'native' WHEN 2 THEN 'both' WHEN 1 THEN 'introduced' ELSE 'unknown'
  END AS status,
  CASE WHEN bool_or(sd.source = 'BONAP') THEN 'BONAP' ELSE max(sd.source) END AS source
FROM ref_state_distribution sd
JOIN ref_taxon t ON t.id = sd.taxon_id
WHERE split_part(public_site.strip_hybrid(t.scientific_name), ' ', 2) <> ''
GROUP BY 1, 2;

-- -----------------------------------------------------------------------------
-- Invasive / noxious listings (state lists; NULL or 'US' = federal)
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW public_site.species_invasive AS
SELECT DISTINCT
  public_site.species_key(t.scientific_name) AS species_key,
  COALESCE(NULLIF(n.state_code, ''), 'US') AS state_code,
  n.designation,
  n.source,
  n.source_url
FROM ref_noxious_invasive n
JOIN ref_taxon t ON t.id = n.taxon_id;

-- -----------------------------------------------------------------------------
-- Wildlife: counts and highlight flags only
-- -----------------------------------------------------------------------------
CREATE OR REPLACE VIEW public_site.wildlife_species_summary AS
SELECT DISTINCT ON (species_key)
  species_key,
  genus,
  lepidoptera_species_count,
  specialist_bee_species_count,
  songbird_value,
  bird_food,
  hummingbird_plant,
  is_keystone_genus
FROM (
  SELECT
    public_site.species_key(t.scientific_name) AS species_key,
    COALESCE(wp.genus, split_part(public_site.strip_hybrid(t.scientific_name), ' ', 1)) AS genus,
    wp.lepidoptera_species_count,
    wp.specialist_bee_species_count,
    NULLIF(btrim(wp.songbird_value), '') AS songbird_value,
    NULLIF(btrim(wp.bird_food), '')      AS bird_food,
    COALESCE(wp.hummingbird_plant, false) AS hummingbird_plant,
    COALESCE(wp.is_keystone_genus, false) AS is_keystone_genus
  FROM ref_wildlife_plants wp
  JOIN ref_taxon t ON t.id = wp.taxon_id
) x
ORDER BY species_key, lepidoptera_species_count DESC NULLS LAST;

-- Genus-level fallback, used for native species without their own row
-- (matches the app, which credits natives at genus level).
CREATE OR REPLACE VIEW public_site.wildlife_genus_summary AS
SELECT
  lower(genus)                                         AS genus_key,
  max(genus)                                           AS genus,
  max(lepidoptera_species_count)::int                  AS lepidoptera_species_count,
  max(specialist_bee_species_count)::int               AS specialist_bee_species_count,
  mode() WITHIN GROUP (ORDER BY songbird_value)        AS songbird_value,
  mode() WITHIN GROUP (ORDER BY bird_food)             AS bird_food,
  bool_or(hummingbird_plant)                           AS hummingbird_plant,
  bool_or(is_keystone_genus)                           AS is_keystone_genus
FROM public_site.wildlife_species_summary
GROUP BY lower(genus);

COMMIT;

-- =============================================================================
-- OPTIONAL (recommended): a read-only login for the website build
-- =============================================================================
-- Create this role with SQL, NOT in the Neon Console. Roles made in the
-- Console are added to neon_superuser, which can read every table.
-- A role made with CREATE ROLE gets nothing until you grant it.
--
-- Pick a strong password (Neon requires about 60 bits of entropy, e.g. 20+
-- random characters), then run as the database owner:
--
--   CREATE ROLE site_reader WITH LOGIN PASSWORD 'replace-with-a-long-random-password';
--   GRANT USAGE  ON SCHEMA public_site TO site_reader;
--   GRANT SELECT ON ALL TABLES IN SCHEMA public_site TO site_reader;
--   ALTER DEFAULT PRIVILEGES IN SCHEMA public_site GRANT SELECT ON TABLES TO site_reader;
--   REVOKE ALL ON ALL TABLES IN SCHEMA public FROM site_reader;
--
-- Views run with their owner's rights, so site_reader can read the views
-- above without any access to soil_samples or the wildlife tables.
-- Check it:
--   SET ROLE site_reader;
--   SELECT count(*) FROM public_site.soil_species_summary;  -- works
--   SELECT count(*) FROM soil_samples;                      -- permission denied
--   RESET ROLE;
--
-- Then put this connection string in Netlify as DATABASE_URL:
--   postgresql://site_reader:<password>@<your-neon-host>/<db>?sslmode=require
-- =============================================================================
