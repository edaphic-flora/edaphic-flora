import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import { verdict, markdownLinks, collect } from '../scripts/lib/link-check.mjs';
import { SOURCES, usdaProfile, bonapGenusMaps } from '../src/lib/sources.js';

test('only a missing page or a missing domain counts as dead', () => {
  assert.equal(verdict({ status: 404 }), 'dead');
  assert.equal(verdict({ status: 410 }), 'dead');
  assert.equal(verdict({ status: 0, error: 'ENOTFOUND' }), 'dead');
  assert.equal(verdict({ status: 200 }), 'ok');
  assert.equal(verdict({ status: 301 }), 'ok');
});

test('blocked, slow and failing servers are unsure, not dead', () => {
  for (const r of [{ status: 403 }, { status: 429 }, { status: 500 }, { status: 504 }, { status: 0, error: 'TimeoutError' }, { status: 0, error: 'UND_ERR_CONNECT_TIMEOUT' }]) {
    assert.equal(verdict(r), 'unsure');
  }
});

test('markdown links are found', () => {
  assert.deepEqual(markdownLinks('a [x](https://a.example/p) and [y](http://b.example) but not [z](/local)'), ['https://a.example/p', 'http://b.example']);
});

test('the same link in two places is tested once', () => {
  const list = collect({ states: { CT: 'https://a.example/' }, sources: { one: 'https://a.example/' }, docs: { 'X.md': '[t](https://c.example/)' } });
  assert.equal(list.length, 2);
  assert.equal(list[0].where, 'state list CT, source one');
});

test('every state has an https link', () => {
  const urls = JSON.parse(fs.readFileSync(new URL('../src/data/state-invasive-urls.json', import.meta.url), 'utf8'));
  assert.equal(Object.keys(urls).length, 50);
  for (const [code, url] of Object.entries(urls)) {
    assert.match(code, /^[A-Z]{2}$/);
    assert.match(url, /^https:\/\//, `${code} should be https`);
  }
});

test('source links are https and species links are built safely', () => {
  for (const url of Object.values(SOURCES)) assert.match(url, /^https:\/\//);
  assert.equal(usdaProfile('ACRU'), 'https://plants.usda.gov/plant-profile/ACRU');
  assert.equal(bonapGenusMaps('Acer'), 'https://bonap.net/Napa/TaxonMaps/Genus/County/Acer');
  assert.equal(bonapGenusMaps(''), null);
  assert.equal(bonapGenusMaps('Acer/../x'), null);
  assert.equal(bonapGenusMaps('× Hybrid'), null);
});
