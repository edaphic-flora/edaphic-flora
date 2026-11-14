// CSV building for the two downloads (netlify/lib/csv.mjs)
import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {
  csvCell, toCsv, mySamplesCsv, mySamplesFilename, MY_SAMPLE_COLUMNS,
  communityCsv, communityRows, COMMUNITY_COLUMNS, COMMUNITY_FILE,
} from '../netlify/lib/csv.mjs';

// Minimal RFC 4180 reader, to check what a spreadsheet would see
function parse(text) {
  const rows = [];
  let row = [];
  let cell = '';
  let quoted = false;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (quoted) {
      if (ch === '"' && text[i + 1] === '"') { cell += '"'; i++; }
      else if (ch === '"') quoted = false;
      else cell += ch;
    } else if (ch === '"') quoted = true;
    else if (ch === ',') { row.push(cell); cell = ''; }
    else if (ch === '\r' && text[i + 1] === '\n') { row.push(cell); rows.push(row); row = []; cell = ''; i++; }
    else cell += ch;
  }
  assert.equal(cell, '', 'file should end with CRLF');
  return rows;
}
const asObjects = (text) => {
  const [head, ...rows] = parse(text);
  return { head, rows: rows.map((r) => Object.fromEntries(head.map((h, i) => [h, r[i]]))) };
};

test('cells: blanks, numbers, booleans and quoting', () => {
  assert.equal(csvCell(null), '');
  assert.equal(csvCell(undefined), '');
  assert.equal(csvCell(NaN), '');
  assert.equal(csvCell(6.5), '6.5');
  assert.equal(csvCell(0), '0');
  assert.equal(csvCell(-72.91), '-72.91');
  assert.equal(csvCell(true), 'TRUE');
  assert.equal(csvCell(false), 'FALSE');
  assert.equal(csvCell('Loam'), 'Loam');
  assert.equal(csvCell('a, b'), '"a, b"');
  assert.equal(csvCell('say "hi"'), '"say ""hi"""');
  assert.equal(csvCell('line 1\nline 2'), '"line 1\nline 2"');
  assert.equal(csvCell('× Chitalpa tashkentensis'), '× Chitalpa tashkentensis');
});

test('text that a spreadsheet would run as a formula is defused', () => {
  for (const s of ['=HYPERLINK("http://x")', '+1', '-2+3', '@SUM(A1)', '\tx', '\rx']) {
    const out = parse(toCsv([{ key: 'v' }], [{ v: s }]))[1][0];
    assert.equal(out, `'${s}`, JSON.stringify(s));
  }
  assert.equal(csvCell('pH 6.5 = fine'), 'pH 6.5 = fine');
});

test('rows end in CRLF and keep column order', () => {
  const text = toCsv([{ key: 'b' }, { key: 'a', get: (r) => r.a * 2 }], [{ a: 1, b: 'x' }, { a: 2 }]);
  assert.equal(text, 'b,a\r\nx,2\r\n,4\r\n');
});

const mine = [
  {
    id: 41, date: '2026-05-11', species: 'Asclepias tuberosa', cultivar: null, outcome: 'Thriving',
    sun_exposure: 'Full Sun', site_hydrology: 'Dry', ph: 6.4, organic_matter: 5.1, calcium_ppm: 1200,
    texture_class: 'Loam', texture_sand: 40, texture_silt: 42, texture_clay: 18,
    location_lat: 41.68, location_long: -72.65, ecoregion_l4: 'Connecticut Valley', ecoregion_l4_code: '59a',
    notes: 'Raised bed, "mulched", near the oak\nsecond line', created_at: '2026-05-11 14:03:22', flagged: false,
    created_by: 'someone-uid', email: 'person@example.com',
  },
  { id: 42, date: null, species: 'Carex pensylvanica', ph: null, organic_matter: 3, notes: '-watered weekly', flagged: true },
];

test('my samples: every column the person entered, never created_by', () => {
  const { head, rows } = asObjects(mySamplesCsv(mine));
  assert.deepEqual(head, MY_SAMPLE_COLUMNS.map((c) => c.key));
  assert.ok(!head.includes('created_by') && !head.includes('email'));
  assert.ok(head.includes('location_lat') && head.includes('notes'), 'their own location and notes are theirs to keep');
  assert.equal(rows.length, 2);
  assert.equal(rows[0].id, '41');
  assert.equal(rows[0].ph, '6.4');
  assert.equal(rows[0].calcium_ppm, '1200');
  assert.equal(rows[0].location_long, '-72.65');
  assert.equal(rows[0].notes, 'Raised bed, "mulched", near the oak\nsecond line');
  assert.equal(rows[0].cultivar, '');
  assert.equal(rows[0].flagged, 'FALSE');
  assert.equal(rows[1].date, '');
  assert.equal(rows[1].notes, "'-watered weekly");
  assert.equal(rows[1].flagged, 'TRUE');
  assert.ok(!mySamplesCsv(mine).includes('someone-uid'));
});

test('my samples: an empty export is just the header', () => {
  assert.equal(parse(mySamplesCsv([])).length, 1);
});

test('my samples filename carries the date', () => {
  assert.equal(mySamplesFilename('2026-11-13'), 'edaphic-flora-my-samples-2026-11-13.csv');
});

// Records shaped like scripts/export-data.mjs output
const soil = (over = {}) => ({
  sample_count: 4, contributor_count: 2, meets_threshold: true,
  ph: { n: 4, p10: 5.9, p50: 6.3, p90: 6.8, lo: null, hi: null },
  organic_matter: { n: 2, p10: null, p50: 4.5, p90: null, lo: 3.1, hi: 5.9 },
  texture: { n: 3, sand: 41, silt: 40, clay: 19, class: 'Loam' },
  nutrients: { phosphorus: { n: 4, p50: 22 }, potassium: null, calcium: { n: 1, p50: 1450 }, magnesium: null },
  ...over,
});
const sp = (name, s, extra = {}) => ({
  scientific_name: name, common_name: 'Some plant', family: 'Fabaceae',
  page: `https://edaphicflora.org/species/${name.toLowerCase().replace(/ /g, '-')}/`, soil: s, ...extra,
});

test('community file: one row per species with samples, sorted, values as shown on species pages', () => {
  const recs = [
    sp('Zizia aurea', soil()),
    sp('Baptisia australis', soil({ sample_count: 1, contributor_count: 1 })),
    sp('Quercus alba', null),
    sp('Rhus typhina', soil({ sample_count: 0 })),
  ];
  assert.deepEqual(communityRows(recs).map((r) => r.scientific_name), ['Baptisia australis', 'Zizia aurea']);
  const { head, rows } = asObjects(communityCsv(recs));
  assert.deepEqual(head, COMMUNITY_COLUMNS.map((c) => c.key));
  const z = rows[1];
  assert.equal(z.species, 'Zizia aurea');
  assert.equal(z.sample_count, '4');
  assert.equal(z.contributor_count, '2');
  assert.equal(z.ph_n, '4');
  assert.equal(z.ph_median, '6.3');
  assert.equal(z.ph_p10, '5.9');
  assert.equal(z.ph_p90, '6.8');
  assert.equal(z.ph_min, '');
  assert.equal(z.organic_matter_pct_median, '4.5');
  assert.equal(z.organic_matter_pct_min, '3.1');
  assert.equal(z.organic_matter_pct_max, '5.9');
  assert.equal(z.organic_matter_pct_p10, '');
  assert.equal(z.texture_class, 'Loam');
  assert.equal(z.sand_pct, '41');
  assert.equal(z.phosphorus_ppm_median, '22');
  assert.equal(z.potassium_n, '0', 'public species with no potassium readings');
  assert.equal(z.potassium_ppm_median, '');
  assert.equal(z.calcium_n, '1');
});

test('community file: values the view held back stay blank, counts included', () => {
  const held = soil({ meets_threshold: false, ph: null, organic_matter: null, texture: null, nutrients: { phosphorus: null } });
  const [row] = asObjects(communityCsv([sp('Asclepias tuberosa', held)])).rows;
  assert.equal(row.sample_count, '4');
  assert.equal(row.ph_n, '');
  assert.equal(row.ph_median, '');
  assert.equal(row.texture_n, '');
  assert.equal(row.phosphorus_n, '');
});

test('community file: only whitelisted columns, whatever else a record holds', () => {
  const leaky = sp('Asclepias tuberosa', soil({ notes: 'secret', location_lat: 41.3, created_by: 'uid-1' }), {
    created_by: 'uid-1', email: 'person@example.com', location_lat: 41.3, location_long: -72.9, notes: 'my yard',
  });
  const text = communityCsv([leaky]);
  for (const s of ['uid-1', 'person@example.com', '41.3', '-72.9', 'secret', 'my yard']) assert.ok(!text.includes(s), s);
  const head = parse(text)[0];
  for (const bad of ['created_by', 'email', 'location_lat', 'location_long', 'lat', 'lon', 'notes', 'cultivar', 'date', 'id']) {
    assert.ok(!head.includes(bad), bad);
  }
});

test('community file from the sample data: every species with samples, nothing else', () => {
  const data = JSON.parse(fs.readFileSync(new URL('../src/data/sample/species.json', import.meta.url), 'utf8'));
  const recs = data.species.map((s) => ({ ...s, page: `https://edaphicflora.org/species/${s.slug}/` }));
  const { rows } = asObjects(communityCsv(recs));
  const withSoil = data.species.filter((s) => (s.soil?.sample_count || 0) > 0);
  assert.equal(rows.length, withSoil.length);
  assert.equal(rows.reduce((t, r) => t + Number(r.sample_count), 0), withSoil.reduce((t, s) => t + s.soil.sample_count, 0));
  assert.equal(COMMUNITY_FILE, 'edaphic-flora-community-soil.csv');
});
