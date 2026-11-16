import { test } from 'node:test';
import assert from 'node:assert/strict';
import { runBackup, keysToPrune, backupKey, KEEP } from '../netlify/lib/backup.mjs';

const ENV = { SITE_ADMIN_DATABASE_URL: 'postgresql://test', ADMIN_EMAILS: 'first@example.com, owner@example.com' };
const DATA = { format: 'edaphic-flora-backup-1', tables: { soil_samples: [{ id: 1 }, { id: 2 }], site_users: [] } };

function fakeStore(existing = []) {
  const blobs = new Map(existing.map((k) => [k, {}]));
  return {
    blobs,
    setJSON: async (k, v) => void blobs.set(k, v),
    list: async () => ({ blobs: [...blobs.keys()].map((key) => ({ key })) }),
    delete: async (k) => void blobs.delete(k),
  };
}

test('only the newest dated backups are kept; other keys are left alone', () => {
  const dated = Array.from({ length: 10 }, (_, i) => `backup-2026-09-${String(i + 1).padStart(2, '0')}.json`);
  assert.deepEqual(keysToPrune(['notes.txt', ...dated], 8), ['backup-2026-09-01.json', 'backup-2026-09-02.json']);
  assert.deepEqual(keysToPrune(['backup-2026-01-01.json'], 8), []);
  assert.equal(backupKey(new Date('2026-11-16T09:17:00Z')), 'backup-2026-11-16.json');
});

test('saves one dated copy, prunes old ones, and reports row counts', async () => {
  const old = Array.from({ length: KEEP }, (_, i) => `backup-2026-09-${String(i + 1).padStart(2, '0')}.json`);
  const store = fakeStore(old);
  const seen = [];
  const r = await runBackup({
    env: ENV,
    store,
    now: new Date('2026-11-16T09:17:00Z'),
    connect: async (url, fn) => fn({ query: async (sql, params) => (seen.push(params), { rows: [{ r: DATA }] }) }),
  });
  assert.equal(r.status, 'saved');
  assert.equal(r.key, 'backup-2026-11-16.json');
  assert.deepEqual(r.rows, { soil_samples: 2, site_users: 0 });
  assert.deepEqual(r.pruned, ['backup-2026-09-01.json']);
  assert.equal(store.blobs.size, KEEP);
  assert.deepEqual(seen[0], ['scheduled-backup', 'first@example.com', 'weekly']);
});

test('tries the next admin email when the database does not know the first', async () => {
  const tried = [];
  const r = await runBackup({
    env: ENV,
    store: fakeStore(),
    connect: async (url, fn) =>
      fn({
        query: async (sql, params) => {
          tried.push(params[1]);
          if (params[1] === 'first@example.com') throw Object.assign(new Error('not a site admin'), { code: '42501' });
          return { rows: [{ r: DATA }] };
        },
      }),
  });
  assert.equal(r.status, 'saved');
  assert.deepEqual(tried, ['first@example.com', 'owner@example.com']);
});

test('skips when not set up, and other database errors are not swallowed', async () => {
  const none = await runBackup({ env: {}, store: fakeStore(), connect: async () => assert.fail('connected') });
  assert.equal(none.status, 'skipped');
  await assert.rejects(
    runBackup({
      env: ENV,
      store: fakeStore(),
      connect: async () => {
        throw Object.assign(new Error('down'), { code: '57P01' });
      },
    }),
    /down/
  );
});
