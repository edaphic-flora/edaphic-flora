import { test } from 'node:test';
import assert from 'node:assert/strict';
import { cleanExtraction, EXTRACTION_SCHEMA, EXTRACTION_PROMPT } from '../netlify/lib/extract.mjs';
import { validateSubmission } from '../netlify/lib/validate.mjs';

const TODAY = '2026-11-12';
const blank = () => Object.fromEntries(EXTRACTION_SCHEMA.required.map((k) => [k, k === 'extraction_warnings' ? [] : null]));

test('schema: every property required, no extras allowed (structured outputs rules)', () => {
  assert.equal(EXTRACTION_SCHEMA.additionalProperties, false);
  assert.deepEqual([...EXTRACTION_SCHEMA.required].sort(), Object.keys(EXTRACTION_SCHEMA.properties).sort());
  const s = JSON.stringify(EXTRACTION_SCHEMA);
  for (const unsupported of ['minimum', 'maximum', 'minLength', 'maxLength']) assert.ok(!s.includes(unsupported), unsupported);
  // The API allows at most 16 nullable/union parameters; this schema uses none
  assert.ok(!s.includes('anyOf') && !s.includes('"null"') && !/"type":\[/.test(s), 'no unions');
  assert.ok(!EXTRACTION_PROMPT.includes(String.fromCharCode(0x2014)));
});

test('a typical report fills the form and passes validation', () => {
  const out = cleanExtraction(
    { ...blank(), ph: 6.4, organic_matter: 5.2, phosphorus_ppm: 31, potassium_ppm: 120, cec_meq: 14.2,
      texture_sand: 42, texture_silt: 40, texture_clay: 18, sample_date: '2026-05-01',
      lab_name: 'UConn Soil Nutrient Analysis Lab', sample_id: 'A-1234', notes: 'Modified Morgan extraction' },
    { today: TODAY }
  );
  assert.deepEqual(out.soil, { ph: 6.4, organic_matter: 5.2, phosphorus_ppm: 31, potassium_ppm: 120, cec_meq: 14.2 });
  assert.deepEqual(out.texture, { mode: 'pct', sand: 42, silt: 40, clay: 18 });
  assert.equal(out.sample_date, '2026-05-01');
  assert.equal(out.notes, 'Lab: UConn Soil Nutrient Analysis Lab. Sample ID: A-1234. Modified Morgan extraction');
  assert.equal(out.found, 6);
  const v = validateSubmission(
    { lab_test_confirmed: true, soil: out.soil, texture: out.texture, plants: [{ species: 'A b' }],
      location: { zip: '06511' }, date: out.sample_date, notes: out.notes },
    { today: TODAY }
  );
  assert.deepEqual(v.errors, []);
});

test('the list form the model returns ({measurements: [{field, value}]}) fills the form', () => {
  const out = cleanExtraction(
    {
      measurements: [{ field: 'ph', value: 5.1 }, { field: 'phosphorus_ppm', value: 19 }, { field: 'not_a_field', value: 3 }],
      texture_class: 'Sandy Loam', sample_date: '2023-07-11', lab_name: '', sample_id: '514', notes: '',
      extraction_warnings: ['Report contains 3 samples; only the first was read'],
    },
    { today: TODAY }
  );
  assert.deepEqual(out.soil, { ph: 5.1, phosphorus_ppm: 19 });
  assert.deepEqual(out.texture, { mode: 'class', class: 'Sandy Loam' });
  assert.equal(out.sample_date, '2023-07-11');
  assert.equal(out.notes, 'Sample ID: 514');
  assert.equal(out.warnings.length, 1);
  assert.equal(cleanExtraction({ measurements: [], texture_class: 'none', sample_date: '', lab_name: '', sample_id: '', notes: '', extraction_warnings: [] }).found, 0);
});

test('out-of-range and non-numeric values are dropped with a warning', () => {
  const out = cleanExtraction({ ...blank(), ph: 72, organic_matter: '5', zinc_ppm: -3, iron_ppm: Infinity }, { today: TODAY });
  assert.deepEqual(out.soil, {});
  assert.equal(out.warnings.length, 2); // pH and zinc; the string and Infinity are just ignored
  assert.ok(out.warnings.some((w) => w.startsWith('pH on the report (72)')));
});

test('texture: class only, bad class, partial percentages', () => {
  assert.deepEqual(cleanExtraction({ ...blank(), texture_class: 'Silty Loam' }).texture, { mode: 'class', class: 'Silty Loam' });
  assert.equal(cleanExtraction({ ...blank(), texture_class: 'Gumbo' }).texture, null);
  assert.deepEqual(
    cleanExtraction({ ...blank(), texture_sand: 40, texture_silt: null, texture_clay: 20, texture_class: 'Loam' }).texture,
    { mode: 'class', class: 'Loam' }
  );
});

test('dates in the future or malformed are not used', () => {
  assert.equal(cleanExtraction({ ...blank(), sample_date: '2027-01-01' }, { today: TODAY }).sample_date, null);
  assert.equal(cleanExtraction({ ...blank(), sample_date: '05/01/2026' }, { today: TODAY }).sample_date, null);
  assert.equal(cleanExtraction({ ...blank(), sample_date: '1850-01-01' }, { today: TODAY }).warnings.length, 1);
});

test('model warnings are kept, trimmed and capped', () => {
  const out = cleanExtraction({ ...blank(), extraction_warnings: ['  Report contains 3 samples  ', '', 42, ...Array(20).fill('x')] });
  assert.equal(out.warnings[0], 'Report contains 3 samples');
  assert.ok(out.warnings.length <= 10);
  assert.equal(cleanExtraction(null).found, 0);
});
