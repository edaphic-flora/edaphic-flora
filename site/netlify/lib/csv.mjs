// CSV building for the two downloads, shared by the Netlify function
// (netlify/functions/export.mjs, a person's own samples) and the site build
// (src/pages/data/, the public community file).
//
// RFC 4180: comma separated, CRLF line ends, fields quoted when they hold a
// comma, quote or line break. Text that a spreadsheet would run as a formula
// (starting with = + - @, tab or carriage return) gets a leading apostrophe.
// Numbers are written as numbers, so negative values are left alone.

const FORMULA_START = /^[=+\-@\t\r]/;

export function csvCell(v) {
  if (v === null || v === undefined) return '';
  if (typeof v === 'number') return Number.isFinite(v) ? String(v) : '';
  if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
  let s = String(v);
  if (FORMULA_START.test(s)) s = `'${s}`;
  return /[",\r\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

// columns: [{ key, get? }], rows: array of objects
export function toCsv(columns, rows) {
  const lines = [columns.map((c) => csvCell(c.key)).join(',')];
  for (const r of rows) lines.push(columns.map((c) => csvCell(c.get ? c.get(r) : r[c.key])).join(','));
  return lines.join('\r\n') + '\r\n';
}

// ---------------------------------------------------------------------------
// A person's own samples (rows from site_api.export_my_samples)
// ---------------------------------------------------------------------------
export const MY_SAMPLE_COLUMNS = [
  'id', 'date', 'species', 'cultivar', 'outcome', 'sun_exposure', 'site_hydrology',
  'ph', 'organic_matter', 'cec_meq', 'soluble_salts_ppm', 'nitrate_ppm', 'ammonium_ppm',
  'phosphorus_ppm', 'potassium_ppm', 'calcium_ppm', 'magnesium_ppm', 'sulfur_ppm',
  'iron_ppm', 'manganese_ppm', 'zinc_ppm', 'copper_ppm', 'boron_ppm',
  'texture_class', 'texture_sand', 'texture_silt', 'texture_clay',
  'location_lat', 'location_long', 'ecoregion_l4', 'ecoregion_l4_code',
  'inat_url', 'notes', 'created_at', 'flagged',
].map((key) => ({ key }));

export function mySamplesCsv(rows) {
  return toCsv(MY_SAMPLE_COLUMNS, rows);
}

export function mySamplesFilename(today) {
  return `edaphic-flora-my-samples-${today}.csv`;
}

// ---------------------------------------------------------------------------
// Public community file: one row per species, from the exported species
// records (src/data/generated/species.json, which scripts/export-data.mjs
// builds from public_site.soil_species_summary). Only values a species page
// already shows; never per-sample rows, locations, notes or contributors.
// ---------------------------------------------------------------------------
export const COMMUNITY_FILE = 'edaphic-flora-community-soil.csv';

// A measurement's sample count. When the record has no value: 0 if the
// species is public (nothing measured), blank if the view held it back.
const count = (m, soil) => (m ? m.n : soil.meets_threshold ? 0 : null);
const metric = (name, label = name) => [
  { key: `${label}_n`, get: (r) => count(r.soil[name], r.soil) },
  { key: `${label}_median`, get: (r) => r.soil[name]?.p50 },
  { key: `${label}_p10`, get: (r) => r.soil[name]?.p10 },
  { key: `${label}_p90`, get: (r) => r.soil[name]?.p90 },
  { key: `${label}_min`, get: (r) => r.soil[name]?.lo },
  { key: `${label}_max`, get: (r) => r.soil[name]?.hi },
];
const nutrient = (name) => [
  { key: `${name}_n`, get: (r) => count(r.soil.nutrients?.[name], r.soil) },
  { key: `${name}_ppm_median`, get: (r) => r.soil.nutrients?.[name]?.p50 },
];

export const COMMUNITY_COLUMNS = [
  { key: 'species', get: (r) => r.scientific_name },
  { key: 'common_name', get: (r) => r.common_name },
  { key: 'family', get: (r) => r.family },
  { key: 'page', get: (r) => r.page },
  { key: 'sample_count', get: (r) => r.soil.sample_count },
  { key: 'contributor_count', get: (r) => r.soil.contributor_count },
  ...metric('ph'),
  ...metric('organic_matter', 'organic_matter_pct'),
  { key: 'texture_n', get: (r) => count(r.soil.texture, r.soil) },
  { key: 'sand_pct', get: (r) => r.soil.texture?.sand },
  { key: 'silt_pct', get: (r) => r.soil.texture?.silt },
  { key: 'clay_pct', get: (r) => r.soil.texture?.clay },
  { key: 'texture_class', get: (r) => r.soil.texture?.class },
  ...nutrient('phosphorus'),
  ...nutrient('potassium'),
  ...nutrient('calcium'),
  ...nutrient('magnesium'),
];

// species: [{ scientific_name, common_name, family, page, soil }]
export function communityRows(species) {
  return species
    .filter((s) => (s.soil?.sample_count || 0) > 0)
    .sort((a, b) => a.scientific_name.localeCompare(b.scientific_name));
}

export function communityCsv(species) {
  return toCsv(COMMUNITY_COLUMNS, communityRows(species));
}
