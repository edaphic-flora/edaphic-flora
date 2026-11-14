// My Garden display helpers (src/lib/garden.js) and the garden endpoint's
// request checks (netlify/functions/garden.mjs).
import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {
  GROUPS, LIFE_FORMS, FAMILY_INFO, GENUS_OVERRIDES, familyLabel, familyBlurb, familyLinks, pluralName, genusLabel,
  genusOf, meterPercent, donutDash, plantName, groupPlants, introducedSupport, recTitle,
} from '../src/lib/garden.js';
import garden from '../netlify/functions/garden.mjs';

const EM = String.fromCharCode(0x2014);

test('no em dashes or double hyphens in any display text', () => {
  const texts = [
    ...Object.values(FAMILY_INFO).flat(),
    ...Object.values(GENUS_OVERRIDES),
    ...GROUPS.flatMap((g) => [g.label, g.noun]),
    ...LIFE_FORMS.map((l) => l.label),
  ];
  for (const t of texts) assert.ok(!t.includes(EM) && !t.includes('--'), t);
  const page = fs.readFileSync(new URL('../src/pages/my-garden.astro', import.meta.url), 'utf8');
  assert.ok(!page.includes(EM), 'page has an em dash');
});

test('every family entry has a name and a blurb', () => {
  for (const [fam, info] of Object.entries(FAMILY_INFO)) {
    assert.equal(info.length, 2, fam);
    assert.ok(info[0] && info[1], fam);
  }
});

test('family labels fall back to the family name', () => {
  assert.equal(familyLabel('Nymphalidae'), 'Brushfoots');
  assert.equal(familyBlurb('Nymphalidae'), 'Monarchs, Painted Ladies, Red Admirals');
  assert.equal(familyLabel('Agonoxenidae'), 'Agonoxenidae');
  assert.equal(familyBlurb('Agonoxenidae'), null);
});

test('family links go to Wikipedia and iNaturalist, first family of a pair', () => {
  assert.deepEqual(familyLinks('Pieridae'), {
    wikipedia: 'https://en.wikipedia.org/wiki/Pieridae',
    inaturalist: 'https://www.inaturalist.org/taxa/Pieridae',
  });
  assert.equal(familyLinks('Coleophoridae - Batrachedridae').wikipedia, 'https://en.wikipedia.org/wiki/Coleophoridae');
});

test('genus names: overrides first, then the USDA name made plural', () => {
  assert.equal(genusLabel('Carya', { Carya: 'hybrid hickory' }), 'Hickories');
  assert.equal(genusLabel('Quercus', { Quercus: 'oak' }), 'Oaks');
  assert.equal(genusLabel('Solidago', { Solidago: 'goldenrod' }), 'Goldenrods');
  assert.equal(genusLabel('Fagus', { Fagus: 'beech' }), 'Beeches');
  assert.equal(genusLabel('Rubus', { Rubus: 'blackberry' }), 'Blackberries');
  assert.equal(genusLabel('Carex', { Carex: 'sedge' }), 'Sedges');
  assert.equal(genusLabel('Salix'), null);
  assert.equal(pluralName('witch hazel'), 'Witch Hazels');
  assert.equal(pluralName('grass'), 'Grass');
  assert.equal(pluralName(''), null);
});

test('genus of a species, hybrids included', () => {
  assert.equal(genusOf('Quercus alba'), 'Quercus');
  assert.equal(genusOf('Amelanchier × grandiflora'), 'Amelanchier');
  assert.equal(genusOf('× Chitalpa tashkentensis'), 'Chitalpa');
});

test('meter and ring geometry', () => {
  assert.equal(meterPercent(0, 50), 0);
  assert.equal(meterPercent(5, 0), 0);
  assert.equal(meterPercent(25, 50), 50);
  assert.equal(meterPercent(60, 50), 100);
  assert.equal(meterPercent(1, 971), 1.5); // a sliver stays visible
  const c = 2 * Math.PI * 46;
  const [fill, gap] = donutDash(25, 50, 46).split(' ').map(Number);
  assert.ok(Math.abs(fill - c / 2) < 0.01 && Math.abs(fill + gap - c) < 0.02);
  assert.equal(donutDash(0, 50, 46).split(' ')[0], '0.00');
});

test('plants sort into counted (by genus), introduced and invasive', () => {
  const plants = [
    { species: 'Quercus rubra', common_name: 'northern red oak', status: 'native' },
    { species: 'Quercus alba', common_name: 'white oak', status: 'native' },
    { species: 'Amsonia tabernaemontana', common_name: null, status: 'unknown' },
    { species: 'Acer palmatum', common_name: 'Japanese maple', status: 'introduced', lep: 0, bee: 0, bird: 0 },
    { species: 'Acer platanoides', common_name: 'Norway maple', status: 'invasive', designation: 'Invasive' },
  ];
  const g = groupPlants(plants, { Quercus: 'oak' });
  assert.deepEqual(g.genera.map((x) => [x.genus, x.label, x.plants.length]), [['Amsonia', 'Amsonia', 1], ['Quercus', 'Oaks', 2]]);
  assert.deepEqual(g.genera[1].plants.map(plantName), ['Northern Red Oak', 'White Oak']);
  assert.deepEqual(g.introduced.map((p) => p.species), ['Acer palmatum']);
  assert.deepEqual(g.invasive.map((p) => p.species), ['Acer platanoides']);
  assert.equal(plantName(plants[2]), 'Amsonia tabernaemontana');
});

test('introduced plant support text', () => {
  assert.equal(introducedSupport({ lep: 0, bee: 0, bird: 0 }), 'No wildlife recorded on this species');
  assert.equal(introducedSupport({ lep: 1, bee: 0, bird: 0 }), 'Supports 1 moth or butterfly');
  assert.equal(introducedSupport({ lep: 3, bee: 1, bird: 2 }), 'Supports 3 moths and butterflies, 1 specialist bee and 2 birds');
});

test('a genus with one native species is named after that species', () => {
  const one = { genus: 'Arctostaphylos', native_count: 1, natives: [{ species: 'Arctostaphylos uva-ursi', common_name: 'kinnikinnick' }] };
  assert.equal(recTitle(one), 'Kinnikinnick');
  const many = { genus: 'Salix', native_count: 12, natives: [{ species: 'Salix discolor', common_name: 'pussy willow' }] };
  assert.equal(recTitle(many, { Salix: 'willow' }), 'Willows');
  assert.equal(recTitle({ genus: 'Zizia', native_count: 2, natives: [] }), 'Golden Alexanders');
  assert.equal(recTitle({ genus: 'Obscura', native_count: 2, natives: [] }), 'Obscura species');
});

test('garden endpoint: GET only, foreign origins refused, sign-in required', async () => {
  process.env.PUBLIC_FIREBASE_PROJECT_ID = 'test-project';
  let r = await garden(new Request('https://edaphicflora.org/api/garden', { method: 'POST' }));
  assert.equal(r.status, 405);
  r = await garden(new Request('https://edaphicflora.org/api/garden', { headers: { origin: 'https://evil.example' } }));
  assert.equal(r.status, 403);
  r = await garden(new Request('https://edaphicflora.org/api/garden'));
  assert.equal(r.status, 401);
  assert.equal((await r.json()).code, 'signin_required');
  r = await garden(new Request('https://edaphicflora.org/api/garden', { headers: { authorization: 'Bearer not.a.jwt' } }));
  assert.equal(r.status, 401);
  assert.equal(r.headers.get('cache-control'), 'no-store');
});
