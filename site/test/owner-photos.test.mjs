import { test } from 'node:test';
import assert from 'node:assert/strict';
import { pickOwnerPhotos, speciesKeyOf, OWNER_LICENSE } from '../scripts/lib/owner-photos.mjs';

const obs = (id, name, quality, photos, extra = {}) => ({
  id,
  uri: `https://www.inaturalist.org/observations/${id}`,
  quality_grade: quality,
  observed_on: extra.observed_on || '2025-06-01',
  faves_count: extra.faves || 0,
  taxon: { id: 1000 + id, name, rank: extra.rank || 'species', iconic_taxon_name: extra.iconic || 'Plantae' },
  photos: photos.map((p) => ({ id: p, url: `https://static.inaturalist.org/photos/${p}/square.jpg`, license_code: extra.license ?? null, original_dimensions: { width: 1536, height: 2048 }, attribution: '(c) Owner, all rights reserved' })),
});

test('species key matches the database key (lower genus and epithet, no hybrid mark)', () => {
  assert.equal(speciesKeyOf('Phlox subulata subsp. brittonii'), 'phlox subulata');
  assert.equal(speciesKeyOf('Iris × germanica'), 'iris germanica');
  assert.equal(speciesKeyOf('Carex'), null);
});

test('one photo per species: confirmed observations first, then the newest', () => {
  const picks = pickOwnerPhotos(
    [
      obs(1, 'Monarda fistulosa', 'needs_id', [11]),
      obs(2, 'Monarda fistulosa', 'research', [21, 22], { observed_on: '2024-07-01' }),
      obs(3, 'Monarda fistulosa', 'research', [31], { observed_on: '2025-07-01' }),
      obs(4, 'Phlox stolonifera', 'casual', [41], { license: 'cc-by' }),
    ],
    { ownerName: 'Owner Name' }
  );
  assert.deepEqual([...picks.keys()].sort(), ['monarda fistulosa', 'phlox stolonifera']);
  const m = picks.get('monarda fistulosa');
  assert.equal(m.record.photo_id, '31');
  assert.deepEqual([...m.photoIds].sort(), ['11', '21', '22', '31']);
  assert.equal(m.record.image_url, 'https://static.inaturalist.org/photos/31/medium.jpg');
  assert.equal(m.record.large_url, 'https://static.inaturalist.org/photos/31/large.jpg');
  assert.equal(m.record.page_url, 'https://www.inaturalist.org/observations/3');
  assert.equal(m.record.creator, 'Owner Name');
  assert.equal(m.record.license, OWNER_LICENSE);
  assert.equal(picks.get('phlox stolonifera').record.license, 'cc-by');
});

test('genus-level, non-plant and photoless observations are ignored', () => {
  const picks = pickOwnerPhotos([
    obs(1, 'Carex', 'needs_id', [1], { rank: 'genus' }),
    obs(2, 'Bombus impatiens', 'research', [2], { iconic: 'Insecta' }),
    obs(3, 'Acer rubrum', 'research', []),
  ]);
  assert.equal(picks.size, 0);
});
