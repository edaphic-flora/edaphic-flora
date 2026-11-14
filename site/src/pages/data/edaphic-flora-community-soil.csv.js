// Public community soil data, one row per species (CC BY-NC 4.0), written as a
// static file at build time so downloads cost no function calls.
//
// Source: the species records scripts/export-data.mjs builds from
// public_site.soil_species_summary, so the file holds only what species pages
// already show: no per-sample rows, contributors, locations or notes. Column
// guide and license: the About page (src/pages/about.astro).
import { species } from '../../lib/data.js';
import { SITE_URL } from '../../config.js';
import { communityCsv } from '../../../netlify/lib/csv.mjs';

export function GET() {
  const rows = species.map((s) => ({
    scientific_name: s.scientific_name,
    common_name: s.common || s.common_name || null,
    family: s.family,
    page: `${SITE_URL}${s.url}`,
    soil: s.soil,
  }));
  return new Response(communityCsv(rows), { headers: { 'Content-Type': 'text/csv; charset=utf-8' } });
}
