import { test } from 'node:test';
import assert from 'node:assert/strict';
import { validateSubmission, OUTCOMES, SUN_EXPOSURES, HYDROLOGY, TEXTURE_CLASS_NAMES } from '../netlify/lib/validate.mjs';

const TODAY = '2026-11-12';
const base = () => ({
  lab_test_confirmed: true,
  soil: { ph: 6.5, organic_matter: 4.2 },
  texture: { mode: 'none' },
  plants: [{ species: 'Asclepias tuberosa' }],
  location: { zip: '06511' },
  date: '2026-06-01',
  notes: '',
});
const run = (patch) => validateSubmission(typeof patch === 'function' ? patch(base()) : { ...base(), ...patch }, { today: TODAY });
const ok = (patch) => {
  const r = run(patch);
  assert.deepEqual(r.errors, []);
  return r;
};
const bad = (patch, re) => {
  const r = run(patch);
  assert.equal(r.ok, false);
  if (re) assert.ok(r.errors.some((e) => re.test(e)), `expected ${re} in ${JSON.stringify(r.errors)}`);
  return r;
};

test('a minimal valid submission passes and is normalized', () => {
  const r = ok({});
  assert.equal(r.value.soil.ph, 6.5);
  assert.equal(r.value.soil.texture_class, null);
  assert.equal(r.value.soil.calcium_ppm, null);
  assert.deepEqual(r.value.plants[0], { species: 'Asclepias tuberosa', cultivar: null, outcome: null, sun_exposure: null, site_hydrology: null, inat_url: null });
  assert.deepEqual(r.value.location, { zip: '06511' });
  assert.equal(r.value.confirm_duplicates, false);
});

test('lab confirmation, unknown fields and non-objects are refused', () => {
  bad({ lab_test_confirmed: false }, /soil testing laboratory/);
  bad({ extra: 1 }, /Unknown field/);
  bad((b) => ({ ...b, soil: { ...b.soil, zinc: 3 } }), /Unknown soil field/);
  bad((b) => ({ ...b, plants: [{ species: 'Asclepias tuberosa', color: 'red' }] }), /Unknown plant field/);
  assert.equal(validateSubmission(null).ok, false);
  assert.equal(validateSubmission([1]).ok, false);
});

test('pH or organic matter is required, and numbers must be numbers', () => {
  bad({ soil: {} }, /at least pH or organic matter/);
  ok({ soil: { ph: 7 } });
  ok({ soil: { organic_matter: 3 } });
  bad({ soil: { ph: '6.5' } }, /pH must be a number/);
  bad({ soil: { ph: NaN } }, /pH must be a number/);
});

test('hard ranges at their boundaries', () => {
  ok({ soil: { ph: 2 } }); ok({ soil: { ph: 14 } });
  bad({ soil: { ph: 1.99 } }, /pH must be between 2\.0 and 14\.0/);
  bad({ soil: { ph: 14.01 } }, /pH must be between/);
  ok({ soil: { organic_matter: 0 } }); ok({ soil: { organic_matter: 50 } });
  bad({ soil: { organic_matter: -0.1 } }, /Organic matter must be between 0% and 50%/);
  bad({ soil: { organic_matter: 50.1 } }, /Organic matter/);
  ok({ soil: { ph: 6, cec_meq: 200 } });
  bad({ soil: { ph: 6, cec_meq: 200.1 } }, /CEC must be between 0 and 200/);
  ok({ soil: { ph: 6, ammonium_ppm: 100000, boron_ppm: 0 } });
  bad({ soil: { ph: 6, boron_ppm: 100000.1 } }, /Boron must be between 0 and 100,000 ppm/);
  bad({ soil: { ph: 6, iron_ppm: -1 } }, /Iron must be between/);
});

test('texture by percentages: 90 to 110 accepted, class computed, values kept', () => {
  const r = ok({ texture: { mode: 'pct', sand: 40, silt: 40, clay: 20 } });
  assert.equal(r.value.soil.texture_class, 'Loam');
  assert.deepEqual([r.value.soil.texture_sand, r.value.soil.texture_silt, r.value.soil.texture_clay], [40, 40, 20]);
  ok({ texture: { mode: 'pct', sand: 30, silt: 30, clay: 30 } });
  ok({ texture: { mode: 'pct', sand: 40, silt: 40, clay: 30 } });
  bad({ texture: { mode: 'pct', sand: 30, silt: 30, clay: 29.9 } }, /between 90 and 110/);
  bad({ texture: { mode: 'pct', sand: 40, silt: 40, clay: 30.1 } }, /between 90 and 110/);
  bad({ texture: { mode: 'pct', sand: 101, silt: 0, clay: 0 } }, /Sand percentage must be between 0 and 100/);
  bad({ texture: { mode: 'pct', sand: 40, silt: 40 } }, /clay percentage/);
});

test('texture by class uses the app midpoints; bad modes refused', () => {
  for (const cls of TEXTURE_CLASS_NAMES) {
    const r = ok({ texture: { mode: 'class', class: cls } });
    const s = r.value.soil;
    assert.equal(s.texture_class, cls);
    assert.ok(Math.abs(s.texture_sand + s.texture_silt + s.texture_clay - 100) < 0.05);
  }
  bad({ texture: { mode: 'class', class: 'Gumbo' } }, /texture class/);
  bad({ texture: { mode: 'guess' } }, /texture was not understood/);
});

test('plants: 1 to 20, no repeats, allowed values only', () => {
  bad({ plants: [] }, /at least one plant/);
  bad({ plants: 'Asclepias tuberosa' }, /at least one plant/);
  ok({ plants: Array.from({ length: 20 }, (_, i) => ({ species: `Species ${i}` })) });
  bad({ plants: Array.from({ length: 21 }, (_, i) => ({ species: `Species ${i}` })) }, /Up to 20 plants/);
  bad({ plants: [{ species: 'A b' }, { species: 'A b' }] }, /listed twice/);
  bad({ plants: [{ species: '  ' }] }, /choose a species/);
  for (const outcome of OUTCOMES) ok({ plants: [{ species: 'A b', outcome }] });
  for (const sun_exposure of SUN_EXPOSURES) ok({ plants: [{ species: 'A b', sun_exposure }] });
  for (const h of HYDROLOGY) ok({ plants: [{ species: 'A b', site_hydrology: h.value }] });
  bad({ plants: [{ species: 'A b', outcome: 'Dead' }] }, /outcome/);
  bad({ plants: [{ species: 'A b', site_hydrology: 'Dry/Xeric' }] }, /site moisture/);
  bad({ plants: [{ species: 'A b', cultivar: 'x'.repeat(256) }] }, /cultivar/);
  ok({ plants: [{ species: 'A b', inat_url: 'https://www.inaturalist.org/observations/123456' }] });
  ok({ plants: [{ species: 'A b', inat_url: 'https://inaturalist.org/observations/1' }] });
  bad({ plants: [{ species: 'A b', inat_url: 'http://www.inaturalist.org/observations/1' }] }, /iNaturalist link/);
  bad({ plants: [{ species: 'A b', inat_url: 'https://evil.example/observations/1' }] }, /iNaturalist link/);
});

test('location: a 5-digit zip or coordinates in range', () => {
  bad({ location: {} }, /zip code where the sample was taken/);
  bad({ location: { zip: '0651' } }, /5-digit zip/);
  bad({ location: { zip: '06511-1234' } }, /5-digit zip/);
  ok({ location: { lat: 41.3, lon: -72.9 } });
  ok({ location: { lat: -90, lon: 180 } });
  bad({ location: { lat: 90.1, lon: 0 } }, /Latitude must be between/);
  bad({ location: { lat: 0, lon: -180.1 } }, /Longitude must be between/);
  bad({ location: { lat: 41 } }, /both latitude and longitude/);
});

test('date and notes', () => {
  ok({ date: '2026-11-13' }); // today + 1 allows for time zones
  bad({ date: '2026-11-14' }, /future/);
  bad({ date: '2026-02-30' }, /date the sample was taken/);
  bad({ date: '1899-12-31' }, /too early/);
  bad({ date: undefined }, /date the sample was taken/);
  ok({ notes: 'x'.repeat(1000) });
  ok({ notes: '🌱'.repeat(1000) });
  bad({ notes: 'x'.repeat(1001) }, /1,000 characters/);
  assert.equal(ok({ notes: '  hi  ' }).value.notes, 'hi');
  assert.equal(ok({ confirm_duplicates: true }).value.confirm_duplicates, true);
  bad({ confirm_duplicates: 'yes' }, /not understood/);
});

test('soft warnings match the app and never block', () => {
  const r = ok({ soil: { ph: 3.4, organic_matter: 21, cec_meq: 61, calcium_ppm: 5001 } });
  assert.equal(r.warnings.length, 4);
  assert.ok(r.warnings.some((w) => /pH value is unusually extreme/.test(w)));
  assert.ok(r.warnings.some((w) => /peat/.test(w)));
  assert.ok(r.warnings.some((w) => /meq\/100g/.test(w)));
  assert.ok(r.warnings.some((w) => /Calcium seems very high/.test(w)));
  assert.equal(ok({ soil: { ph: 3.5, organic_matter: 20, cec_meq: 60, calcium_ppm: 5000 } }).warnings.length, 0);
});

test('no em dashes in any message', () => {
  const r = run({ lab_test_confirmed: false, soil: { ph: 1, cec_meq: 999 }, plants: [], location: {}, date: 'x' });
  const emDash = String.fromCharCode(0x2014);
  for (const m of [...r.errors, ...r.warnings]) assert.ok(!m.includes(emDash), m);
});
