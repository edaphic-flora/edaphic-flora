// The /admin/ "public site" tools: new samples since the last visit, the
// rebuild button and the backup download, with stand-ins for the sign-in,
// the database and the Netlify build hook.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { createAdminHandler } from '../netlify/lib/admin.mjs';

const HOOK = 'https://api.netlify.com/build_hooks/abc123DEF456';
const ENV = { ADMIN_EMAILS: 'owner@example.com', SITE_ADMIN_DATABASE_URL: 'postgresql://test', NETLIFY_BUILD_HOOK: HOOK };

function setup({ row, env = ENV, who = { uid: 'fb-1', email: 'owner@example.com' }, hookStatus = 200 } = {}) {
  const calls = [];
  const posts = [];
  const handler = createAdminHandler({
    env,
    verify: async () => who,
    connect: async (url, fn) => fn({ query: async (sql, params) => (calls.push({ sql, params }), { rows: [{ r: row }] }) }),
    fetchImpl: async (url, init) => (posts.push({ url, init }), new Response(null, { status: hookStatus })),
  });
  return { handler, calls, posts };
}
const req = (path, method = 'GET') =>
  new Request(`https://edaphicflora.org${path}`, { method, headers: { authorization: 'Bearer x.y.z' } });

test('the new routes are for admins only', async () => {
  for (const [method, path] of [['GET', '/api/admin/new'], ['GET', '/api/admin/backup'], ['POST', '/api/admin/rebuild']]) {
    const t = setup({ who: { uid: 'fb-2', email: 'someone@example.com' } });
    const res = await t.handler(req(path, method));
    assert.equal(res.status, 403, `${method} ${path}`);
    assert.equal(t.calls.length, 0);
    assert.equal(t.posts.length, 0);
  }
});

test('new since last visit: the last seen id reaches the database', async () => {
  const t = setup({ row: { new: 3, latest_id: 512, last_rebuild: null, last_backup: null } });
  const res = await t.handler(req('/api/admin/new?after=509'));
  assert.equal(res.status, 200);
  assert.deepEqual(t.calls[0].params, ['fb-1', 'owner@example.com', 509]);
  assert.equal((await res.json()).new, 3);
  const first = setup({ row: { new: 172, latest_id: 512 } });
  await first.handler(req('/api/admin/new'));
  assert.deepEqual(first.calls[0].params, ['fb-1', 'owner@example.com', 0]);
  assert.equal((await setup().handler(req('/api/admin/new?after=abc'))).status, 400);
});

test('rebuild: logged in the database first, then the build hook is called once', async () => {
  const t = setup({ row: { status: 'ok', left: 7 } });
  const res = await t.handler(req('/api/admin/rebuild', 'POST'));
  assert.equal(res.status, 200);
  assert.deepEqual(await res.json(), { left: 7 });
  assert.match(t.calls[0].sql, /admin_api\.request_rebuild/);
  assert.equal(t.posts.length, 1);
  assert.equal(t.posts[0].url, HOOK);
  assert.equal(t.posts[0].init.method, 'POST');
});

test('rebuild: over a limit means no build', async () => {
  const soon = setup({ row: { status: 'too_soon', minutes: 42 } });
  const r1 = await soon.handler(req('/api/admin/rebuild', 'POST'));
  assert.equal(r1.status, 429);
  assert.match((await r1.json()).error, /42 minutes/);
  assert.equal(soon.posts.length, 0);

  const month = setup({ row: { status: 'monthly_limit', limit: 8 } });
  assert.equal((await month.handler(req('/api/admin/rebuild', 'POST'))).status, 429);
  assert.equal(month.posts.length, 0);
});

test('rebuild: a missing or odd hook means nothing is logged or called', async () => {
  for (const hook of [undefined, '', 'https://evil.example/build_hooks/abc', 'http://api.netlify.com/build_hooks/abc']) {
    const t = setup({ row: { status: 'ok', left: 7 }, env: { ...ENV, NETLIFY_BUILD_HOOK: hook } });
    const res = await t.handler(req('/api/admin/rebuild', 'POST'));
    assert.equal(res.status, 503, String(hook));
    assert.equal(t.calls.length, 0);
    assert.equal(t.posts.length, 0);
  }
  const down = setup({ row: { status: 'ok', left: 7 }, hookStatus: 500 });
  assert.equal((await down.handler(req('/api/admin/rebuild', 'POST'))).status, 502);
});

test('backup: a JSON file download, never cached', async () => {
  const t = setup({ row: { format: 'edaphic-flora-backup-1', tables: { soil_samples: [{ id: 1 }] } } });
  const res = await t.handler(req('/api/admin/backup'));
  assert.equal(res.status, 200);
  assert.match(res.headers.get('content-disposition'), /^attachment; filename="edaphic-flora-backup-\d{4}-\d{2}-\d{2}\.json"$/);
  assert.equal(res.headers.get('cache-control'), 'no-store');
  assert.deepEqual(t.calls[0].params, ['fb-1', 'owner@example.com', 'admin page download']);
  assert.equal((await res.json()).tables.soil_samples.length, 1);
});
