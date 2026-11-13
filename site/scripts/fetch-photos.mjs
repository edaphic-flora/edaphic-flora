#!/usr/bin/env node
// Find openly licensed photos for species and store them in ref_species_photos.
//
// Works through species in priority order and saves each result as soon as it
// is found, so it can be stopped and restarted at any time:
//   tier 1  species with a public soil snapshot
//   tier 2  every other species in someone's garden (any soil sample)
//   tier 3  plants in the wildlife tables
//   tier 4  species native to at least one US state (with USDA traits)
//   tier 5  everything else with a US state record
//
// Species already checked are skipped. Species where nothing was found are
// tried again after PHOTO_RECHECK_DAYS. Rows marked locked are never touched.
//
// Environment:
//   PHOTOS_DATABASE_URL  connection that can write ref_species_photos
//                        (falls back to DATABASE_URL)
//   PHOTO_LIMIT          species to look up this run (default 400, about 8 to 15 minutes)
//   PHOTO_MAX_TIER       stop after this tier (default 5)
//   PHOTO_RECHECK_DAYS   retry "nothing found" after this many days (default 90)
//   PHOTO_LICENSES       comma list (default cc0,pd,cc-by,cc-by-sa,cc-by-nc,cc-by-nc-sa)
//   PHOTO_DRY_RUN        "true" = list what would be looked up, change nothing
//   PHOTO_FIXTURES       testing only: JSON file of name -> result instead of the network

import fs from 'node:fs/promises';
import pg from 'pg';
import { makePhotoLookup, normalizeName, DEFAULT_LICENSES } from './lib/photo-lookup.mjs';

const log = (...a) => console.log('[fetch-photos]', ...a);
const env = process.env;
const LIMIT = Math.max(1, parseInt(env.PHOTO_LIMIT || '400', 10));
const MAX_TIER = Math.min(5, Math.max(1, parseInt(env.PHOTO_MAX_TIER || '5', 10)));
const RECHECK_DAYS = Math.max(1, parseInt(env.PHOTO_RECHECK_DAYS || '90', 10));
const LICENSES = env.PHOTO_LICENSES ? env.PHOTO_LICENSES.split(',').map((s) => s.trim()) : DEFAULT_LICENSES;
const DRY = env.PHOTO_DRY_RUN === 'true';
const USER_AGENT = 'EdaphicFlora/1.0 (https://edaphicflora.org; edaphicflora@gmail.com) species photo lookup';

// species_reference and species_state_status scan every taxon, so each is
// computed once (MATERIALIZED), and the state branch only runs when tiers 4
// or 5 are wanted.
const CANDIDATES_SQL = `
WITH ref AS MATERIALIZED (
  SELECT species_key, scientific_name, growth_habit FROM public_site.species_reference
),
st AS MATERIALIZED (
  SELECT species_key, bool_or(status IN ('native', 'both')) AS any_native
    FROM public_site.species_state_status
   GROUP BY species_key
),
cand AS (
  SELECT species_key, min(tier) AS tier, min(name) AS name
  FROM (
    SELECT species_key, species_name AS name, CASE WHEN meets_threshold THEN 1 ELSE 2 END AS tier
      FROM public_site.soil_species_summary
    UNION ALL
    SELECT w.species_key, r.scientific_name, 3
      FROM public_site.wildlife_species_summary w
      JOIN ref r USING (species_key)
    UNION ALL
    SELECT s.species_key, r.scientific_name,
           CASE WHEN s.any_native AND r.growth_habit IS NOT NULL THEN 4 ELSE 5 END
      FROM st s
      JOIN ref r USING (species_key)
     WHERE $2::int >= 4
  ) x
  GROUP BY species_key
)
SELECT c.species_key, c.tier, c.name
FROM cand c
LEFT JOIN ref_species_photos p USING (species_key)
WHERE c.tier <= $2
  AND (p.species_key IS NULL
       OR (p.locked = FALSE AND p.source = 'none' AND p.checked_at < now() - make_interval(days => $3)))
ORDER BY c.tier, c.name
LIMIT $1`;

const UPSERT_SQL = `
INSERT INTO ref_species_photos
  (species_key, source, photo_id, page_url, image_url, large_url, width, height, license,
   creator, attribution, inat_taxon_id, matched_name, tier, checked_at, updated_at)
VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14, now(), now())
ON CONFLICT (species_key) DO UPDATE SET
  source = EXCLUDED.source, photo_id = EXCLUDED.photo_id, page_url = EXCLUDED.page_url,
  image_url = EXCLUDED.image_url, large_url = EXCLUDED.large_url, width = EXCLUDED.width,
  height = EXCLUDED.height, license = EXCLUDED.license, creator = EXCLUDED.creator,
  attribution = EXCLUDED.attribution, inat_taxon_id = EXCLUDED.inat_taxon_id,
  matched_name = EXCLUDED.matched_name, tier = EXCLUDED.tier,
  checked_at = now(), updated_at = now()
WHERE ref_species_photos.locked = FALSE`;

async function makeLookup() {
  if (!env.PHOTO_FIXTURES) return makePhotoLookup({ userAgent: USER_AGENT, licenses: LICENSES });
  const fx = JSON.parse(await fs.readFile(env.PHOTO_FIXTURES, 'utf8'));
  log(`using fixtures from ${env.PHOTO_FIXTURES} (no network)`);
  return {
    lookup: async (name) => {
      const hit = fx[normalizeName(name)];
      if (hit === 'ERROR') throw new Error('simulated network error');
      return hit || { source: 'none', matched_name: null, inat_taxon_id: null };
    },
  };
}

async function main() {
  const url = env.PHOTOS_DATABASE_URL || env.DATABASE_URL;
  if (!url) {
    console.error('[fetch-photos] Set PHOTOS_DATABASE_URL (or DATABASE_URL) to a connection that can write ref_species_photos.');
    process.exit(1);
  }
  const local = /@(localhost|127\.0\.0\.1)[:/]/.test(url) || /sslmode=disable/.test(url);
  const client = new pg.Client({ connectionString: url, ssl: local ? false : { rejectUnauthorized: true } });
  await client.connect();

  try {
    const exists = await client.query(`SELECT to_regclass('public.ref_species_photos') AS t`);
    if (!exists.rows[0].t) throw new Error('ref_species_photos does not exist yet. Run sql/species_photos.sql first.');

    const { rows } = await client.query(CANDIDATES_SQL, [LIMIT, MAX_TIER, RECHECK_DAYS]);
    const byTier = rows.reduce((m, r) => ((m[r.tier] = (m[r.tier] || 0) + 1), m), {});
    log(`${rows.length} species to look up (limit ${LIMIT}, tiers 1 to ${MAX_TIER}):`, JSON.stringify(byTier));
    if (DRY) {
      rows.slice(0, 50).forEach((r) => log(`  tier ${r.tier}  ${r.name}`));
      return;
    }

    const lookup = await makeLookup();
    const counts = { inaturalist: 0, wikimedia: 0, none: 0, error: 0 };
    let consecutiveErrors = 0;
    const started = Date.now();

    for (const [i, row] of rows.entries()) {
      let rec;
      try {
        rec = await lookup.lookup(row.name);
        consecutiveErrors = 0;
      } catch (err) {
        counts.error++;
        consecutiveErrors++;
        log(`  error for ${row.name}: ${err.message}`);
        if (consecutiveErrors >= 10) throw new Error('10 errors in a row; stopping. Run again later.');
        continue; // not saved, so it is retried next run
      }
      await client.query(UPSERT_SQL, [
        row.species_key, rec.source, rec.photo_id || null, rec.page_url || null, rec.image_url || null,
        rec.large_url || null, rec.width || null, rec.height || null, rec.license || null,
        rec.creator || null, rec.attribution || null, rec.inat_taxon_id || null, rec.matched_name || null,
        row.tier,
      ]);
      counts[rec.source] = (counts[rec.source] || 0) + 1;
      if ((i + 1) % 25 === 0 || i === rows.length - 1) {
        const mins = ((Date.now() - started) / 60000).toFixed(1);
        log(`  ${i + 1}/${rows.length} done in ${mins} min`, JSON.stringify(counts));
      }
    }

    const { rows: totals } = await client.query(
      `SELECT source, count(*)::int AS n FROM ref_species_photos GROUP BY source ORDER BY source`
    );
    log('finished this run:', JSON.stringify(counts));
    log('photos table now:', totals.map((t) => `${t.source} ${t.n}`).join(', '));
  } finally {
    await client.end();
  }
}

main().catch((err) => {
  console.error('[fetch-photos] failed:', err.message);
  process.exit(1);
});
