#!/usr/bin/env node
// Export public data from Neon for the static site build.
//
// Reads ONLY the public_site views (see sql/public_views.sql) and writes
// src/data/generated/species.json, which the Astro pages render from.
//
// Environment:
//   DATABASE_URL       Postgres connection string (use the read-only
//                      site_reader role). Read from site/.env when present
//                      (npm scripts pass --env-file-if-exists). If unset, the site builds from
//                      src/data/sample/species.json and shows a "sample data"
//                      ribbon.
//   SITE_SCOPE         Which species get pages:
//                        curated (default) species with soil samples or
//                                          wildlife data, plus featured ones
//                        native            curated + every species with a
//                                          native US state record and USDA traits
//                        all               curated + every species with any
//                                          US state record
//   ALLOW_SAMPLE_DATA  Set to "true" to let a production build run without
//                      DATABASE_URL. Otherwise production fails loudly so
//                      sample numbers never go live by accident.
//   EXPORT_OUT         Override output path (used to regenerate the sample file).
//   EXPORT_LABEL       "database" (default) or "sample".

import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import pg from 'pg';
import { classifyTexture, normalizeTexture } from '../src/lib/texture.js';
import { slugify, parseNativeStatusNA } from '../src/lib/names.js';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '..');
const OUT = process.env.EXPORT_OUT
  ? path.resolve(process.env.EXPORT_OUT)
  : path.join(root, 'src/data/generated/species.json');
const LABEL = process.env.EXPORT_LABEL || 'database';
const SCOPE = (process.env.SITE_SCOPE || 'curated').toLowerCase();
const isProduction = process.env.CONTEXT === 'production';

const log = (...a) => console.log('[export-data]', ...a);

async function main() {
  const url = process.env.DATABASE_URL;
  if (!url) {
    if (isProduction && process.env.ALLOW_SAMPLE_DATA !== 'true') {
      console.error(
        '[export-data] DATABASE_URL is not set for a production build.\n' +
          'Add it in Netlify (Site configuration > Environment variables), or set\n' +
          'ALLOW_SAMPLE_DATA=true if you really want to publish sample data.'
      );
      process.exit(1);
    }
    log('DATABASE_URL not set, so the site will build from sample data.');
    return;
  }
  if (!['curated', 'native', 'all'].includes(SCOPE)) {
    throw new Error(`SITE_SCOPE must be curated, native or all (got "${SCOPE}")`);
  }

  const curated = JSON.parse(await fs.readFile(path.join(root, 'src/data/curated.json'), 'utf8'));
  const featuredKeys = (curated.featured || []).map((s) => s.replace(/-/g, ' '));

  const client = new pg.Client({
    connectionString: url,
    ssl: /sslmode=disable/.test(url) || /@(localhost|127\.0\.0\.1)|host=\/tmp/.test(url) ? false : { rejectUnauthorized: true },
    statement_timeout: 120000,
  });
  await client.connect();
  const q = async (sql, params = []) => (await client.query(sql, params)).rows;

  try {
    const [settings] = await q('SELECT * FROM public_site.settings');

    // ---- choose which species get pages
    const curatedKeys = await q(
      `SELECT species_key FROM public_site.soil_species_summary
       UNION SELECT species_key FROM public_site.wildlife_species_summary
       UNION SELECT unnest($1::text[])`,
      [featuredKeys]
    );
    const keys = new Set(curatedKeys.map((r) => r.species_key));
    if (SCOPE === 'native') {
      const rows = await q(
        `SELECT DISTINCT s.species_key FROM public_site.species_state_status s
         JOIN public_site.species_reference r USING (species_key)
         WHERE s.status IN ('native','both') AND r.growth_habit IS NOT NULL`
      );
      rows.forEach((r) => keys.add(r.species_key));
    } else if (SCOPE === 'all') {
      const rows = await q('SELECT DISTINCT species_key FROM public_site.species_state_status');
      rows.forEach((r) => keys.add(r.species_key));
    }
    const keyList = [...keys];
    log(`scope=${SCOPE}: ${keyList.length} species`);

    // ---- pull everything for those species (one query at a time on one connection)
    const ref = await q('SELECT * FROM public_site.species_reference WHERE species_key = ANY($1)', [keyList]);
    const states = await q('SELECT species_key, state_code, status, source FROM public_site.species_state_status WHERE species_key = ANY($1)', [keyList]);
    const invasive = await q('SELECT species_key, state_code, designation, source, source_url FROM public_site.species_invasive WHERE species_key = ANY($1)', [keyList]);
    const soil = await q('SELECT * FROM public_site.soil_species_summary WHERE species_key = ANY($1)', [keyList]);
    const wildSp = await q('SELECT * FROM public_site.wildlife_species_summary WHERE species_key = ANY($1)', [keyList]);
    const wildGenus = await q('SELECT * FROM public_site.wildlife_genus_summary');
    let photos = [];
    try {
      photos = await q('SELECT * FROM public_site.species_photos WHERE species_key = ANY($1)', [keyList]);
    } catch (err) {
      log('no photos yet (run sql/species_photos.sql, then npm run fetch-photos):', err.message.split('\n')[0]);
    }

    const by = (rows) => {
      const m = new Map();
      for (const r of rows) {
        if (!m.has(r.species_key)) m.set(r.species_key, []);
        m.get(r.species_key).push(r);
      }
      return m;
    };
    const refBy = new Map(ref.map((r) => [r.species_key, r]));
    const soilBy = new Map(soil.map((r) => [r.species_key, r]));
    const wildBy = new Map(wildSp.map((r) => [r.species_key, r]));
    const genusBy = new Map(wildGenus.map((r) => [r.genus_key, r]));
    const photoBy = new Map(photos.map((r) => [r.species_key, r]));
    const statesBy = by(states);
    const invBy = by(invasive);

    const num = (v) => (v === null || v === undefined ? null : Number(v));
    const records = [];
    const usedSlugs = new Set();

    for (const key of keyList) {
      const r = refBy.get(key);
      const s = soilBy.get(key);
      const name = r?.scientific_name || s?.species_name;
      if (!name) continue; // featured slug with no data at all

      let slug = slugify(name);
      if (usedSlugs.has(slug)) slug = `${slug}-${slugify(r?.usda_symbol || String(records.length))}`;
      usedSlugs.add(slug);

      const stateRows = statesBy.get(key) || [];
      const stateMap = Object.fromEntries(stateRows.map((x) => [x.state_code, x.status]));
      const sources = new Set(stateRows.map((x) => x.source).filter(Boolean));
      const anyNative = stateRows.some((x) => x.status === 'native' || x.status === 'both');
      const na = parseNativeStatusNA(r?.native_status_na);

      // soil
      let soilOut = null;
      if (s) {
        // p10/p90 once there are enough values for a range; lo/hi are the
        // readings themselves before that (one or two samples)
        const metric = (n, p10, p50, p90, lo, hi) =>
          num(p50) === null ? null : { n, p10: num(p10), p50: num(p50), p90: num(p90), lo: num(lo), hi: num(hi) };
        let texture = null;
        if (num(s.sand_p50) !== null) {
          const norm = normalizeTexture(num(s.sand_p50), num(s.silt_p50), num(s.clay_p50));
          texture = norm && {
            n: s.texture_n,
            ...norm,
            class: classifyTexture(norm.sand, norm.silt, norm.clay) || s.texture_class_mode || null,
          };
        }
        const nut = (n, v) => (num(v) === null ? null : { n, p50: num(v) });
        soilOut = {
          sample_count: s.sample_count,
          contributor_count: s.contributor_count,
          meets_threshold: s.meets_threshold,
          ph: metric(s.ph_n, s.ph_p10, s.ph_p50, s.ph_p90, s.ph_lo, s.ph_hi),
          organic_matter: metric(s.om_n, s.om_p10, s.om_p50, s.om_p90, s.om_lo, s.om_hi),
          texture,
          nutrients: {
            phosphorus: nut(s.p_n, s.p_p50),
            potassium: nut(s.k_n, s.k_p50),
            calcium: nut(s.ca_n, s.ca_p50),
            magnesium: nut(s.mg_n, s.mg_p50),
          },
        };
      }

      // wildlife: species row first; natives fall back to their genus
      const genus = name.split(' ')[0];
      let wildlife = null;
      const w = wildBy.get(key);
      const isNativeHere = anyNative || na?.status === 'native' || na?.status === 'both';
      const wg = !w && isNativeHere ? genusBy.get(genus.toLowerCase()) : null;
      const src = w || wg;
      if (src) {
        wildlife = {
          level: w ? 'species' : 'genus',
          genus: src.genus || genus,
          lepidoptera: num(src.lepidoptera_species_count) ?? 0,
          specialist_bees: num(src.specialist_bee_species_count) ?? 0,
          songbird_value: src.songbird_value || null,
          bird_food: src.bird_food || null,
          hummingbird: !!src.hummingbird_plant,
          keystone: !!src.is_keystone_genus,
        };
      }

      records.push({
        key,
        slug,
        scientific_name: name,
        genus,
        family: r?.family || null,
        common_name: r?.common_name || null,
        usda_symbol: r?.usda_symbol || null,
        usda: r
          ? {
              growth_habit: r.growth_habit,
              duration: r.duration,
              native_status_na: na?.label || null,
              na_status: na?.status || null,
              ph_min: num(r.soil_ph_min),
              ph_max: num(r.soil_ph_max),
              moisture_use: r.moisture_use,
              shade_tolerance: r.shade_tolerance,
              drought_tolerance: r.drought_tolerance,
              salinity_tolerance: r.salinity_tolerance,
              bloom_period: r.bloom_period,
            }
          : null,
        states: stateMap,
        state_source: sources.size === 0 ? null : sources.size === 1 ? [...sources][0] : 'mixed',
        invasive: (invBy.get(key) || []).map((x) => ({
          state: x.state_code,
          designation: x.designation,
          source: x.source,
          url: x.source_url,
        })),
        soil: soilOut,
        wildlife,
        photo: (() => {
          const p = photoBy.get(key);
          if (!p) return null;
          return {
            src: p.image_url,
            large: p.large_url || p.image_url,
            w: p.width || null,
            h: p.height || null,
            source: p.source,
            license: p.license,
            creator: p.creator,
            page: p.page_url,
          };
        })(),
      });
    }

    records.sort((a, b) => a.scientific_name.localeCompare(b.scientific_name));
    const out = {
      meta: {
        source: LABEL,
        generated_at: new Date().toISOString(),
        scope: SCOPE,
        thresholds: settings,
        species_count: records.length,
        public_sample_count: records.reduce((t, r) => t + (r.soil?.sample_count || 0), 0),
        photo_count: records.filter((r) => r.photo).length,
      },
      species: records,
    };
    await fs.mkdir(path.dirname(OUT), { recursive: true });
    await fs.writeFile(OUT, JSON.stringify(out, null, LABEL === 'sample' ? 1 : 0));
    log(`wrote ${records.length} species to ${path.relative(root, OUT)}`);
  } finally {
    await client.end();
  }
}

main().catch((err) => {
  console.error('[export-data] failed:', err.message);
  process.exit(1);
});
