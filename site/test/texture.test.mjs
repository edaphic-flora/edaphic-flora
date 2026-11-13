// The texture port must agree with the Shiny app's R code (fixtures made by
// test/fixtures/make_texture_reference.R).
import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import { classifyTextureApp, textureClassFor, texturePercentages, TEXTURE_CLASS_NAMES } from '../netlify/lib/texture.mjs';

const csv = (name) =>
  fs.readFileSync(new URL(`./fixtures/${name}`, import.meta.url), 'utf8')
    .trim().split(/\r?\n/).slice(1)
    .map((line) => line.split(',').map((v) => v.replace(/^"|"$/g, '')));

const grid = csv('texture_reference.csv').map(([sand, silt, clay, cls]) => ({ sand: +sand, silt: +silt, clay: +clay, cls }));

test('app table port matches R classify_texture on every 1% grid point', () => {
  assert.equal(grid.length, 5151);
  const wrong = grid.filter((g) => classifyTextureApp(g.sand, g.silt, g.clay) !== g.cls);
  assert.deepEqual(wrong.slice(0, 5), []);
});

test('stored class keeps the app class wherever the app has one, and never leaves a gap', () => {
  for (const g of grid) {
    const got = textureClassFor(g.sand, g.silt, g.clay);
    if (g.cls !== 'Unclassified') assert.equal(got, g.cls, `${g.sand}/${g.silt}/${g.clay}`);
    else assert.ok(TEXTURE_CLASS_NAMES.includes(got), `${g.sand}/${g.silt}/${g.clay} -> ${got}`);
  }
});

test('lab rounding: percentages summing to 90 to 110 are scaled before classifying', () => {
  assert.equal(textureClassFor(42, 41, 18), textureClassFor(42 / 1.01, 41 / 1.01, 18 / 1.01));
  assert.equal(textureClassFor(0, 0, 0), null);
});

test('class midpoints match R get_texture_percentages', () => {
  const mids = csv('texture_midpoints.csv');
  assert.equal(mids.length, 13);
  for (const [cls, sand, silt, clay] of mids) {
    const p = texturePercentages(cls);
    assert.ok(Math.abs(p.sand - +sand) < 1e-9 && Math.abs(p.silt - +silt) < 1e-9 && Math.abs(p.clay - +clay) < 1e-9, cls);
  }
  assert.equal(texturePercentages('Not a class'), null);
});
