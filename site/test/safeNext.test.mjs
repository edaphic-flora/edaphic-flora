import { test } from 'node:test';
import assert from 'node:assert/strict';
import { safeNext } from '../src/lib/safeNext.js';

const ORIGIN = 'https://edaphicflora.org';
const next = (v) => safeNext(new URLSearchParams({ next: v }), ORIGIN);

test('next: paths on this site pass through', () => {
  assert.equal(next('/contribute/'), '/contribute/');
  assert.equal(next('/contribute/?species=Acer%20rubrum#plants'), '/contribute/?species=Acer%20rubrum#plants');
  assert.equal(safeNext(new URLSearchParams(''), ORIGIN), null);
});

test('next: anything that leaves the site is refused', () => {
  const outside = ['//evil.example/x', '/\\evil.example/x', '/\t/evil.example/x', '/\n/evil.example', 'https://evil.example/', 'javascript:alert(1)', 'evil.example', ''];
  for (const v of outside) assert.equal(next(v), null, JSON.stringify(v));
});
