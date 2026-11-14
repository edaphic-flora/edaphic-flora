// The admin API's own checks, with a stand-in for the sign-in and the
// database: who counts as an admin, that everyone else is refused before any
// database connection, and what reaches the admin_api functions.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { isAdminEmail, adminEmails, createAdminHandler } from '../netlify/lib/admin.mjs';
import { AuthError } from '../netlify/lib/auth.mjs';

const ENV = { ADMIN_EMAILS: ' Owner@Example.com, second@example.org ,', SITE_ADMIN_DATABASE_URL: 'postgresql://test' };

test('ADMIN_EMAILS: comma-separated, any case, spaces ignored', () => {
  assert.deepEqual([...adminEmails(ENV.ADMIN_EMAILS)], ['owner@example.com', 'second@example.org']);
  assert.ok(isAdminEmail('owner@example.com', ENV.ADMIN_EMAILS));
  assert.ok(isAdminEmail('OWNER@EXAMPLE.COM', ENV.ADMIN_EMAILS));
  assert.ok(isAdminEmail(' second@example.org ', ENV.ADMIN_EMAILS));
});

test('ADMIN_EMAILS: no partial matches, empty or missing list means nobody', () => {
  assert.equal(isAdminEmail('xowner@example.com', ENV.ADMIN_EMAILS), false);
  assert.equal(isAdminEmail('owner@example.com.evil.test', ENV.ADMIN_EMAILS), false);
  assert.equal(isAdminEmail('owner', ENV.ADMIN_EMAILS), false);
  assert.equal(isAdminEmail('owner@example.com', ''), false);
  assert.equal(isAdminEmail('owner@example.com', ' , ,'), false);
  assert.equal(isAdminEmail('owner@example.com', undefined), false);
  assert.equal(isAdminEmail('', ENV.ADMIN_EMAILS), false);
  assert.equal(isAdminEmail(undefined, ENV.ADMIN_EMAILS), false);
});

// A handler whose sign-in check returns `who` (or throws) and whose database
// records every query and answers with `rows`
function setup({ who = { uid: 'fb-1', email: 'owner@example.com' }, rows = [{ r: { status: 'ok', changed: true } }], env = ENV, dbError } = {}) {
  const calls = [];
  let connects = 0;
  const handler = createAdminHandler({
    env,
    verify: async () => {
      if (who instanceof Error) throw who;
      return who;
    },
    connect: async (url, fn) => {
      connects++;
      assert.equal(url, env.SITE_ADMIN_DATABASE_URL);
      if (dbError) throw dbError;
      return fn({ query: async (sql, params) => (calls.push({ sql, params }), { rows }) });
    },
  });
  return { handler, calls, connects: () => connects };
}
const req = (path, { method = 'GET', body, headers = {} } = {}) =>
  new Request(`https://edaphicflora.org${path}`, {
    method,
    headers: { authorization: 'Bearer x.y.z', ...(body !== undefined ? { 'content-type': 'application/json' } : {}), ...headers },
    body: body === undefined ? undefined : typeof body === 'string' ? body : JSON.stringify(body),
  });

test('a signed-in non-admin is refused on every route, without touching the database', async () => {
  const routes = [
    ['GET', '/api/admin/me'], ['GET', '/api/admin/samples'], ['GET', '/api/admin/contributors'],
    ['GET', '/api/admin/disabled'], ['GET', '/api/admin/audit'],
    ['POST', '/api/admin/samples/5/flag', { reason: 'x' }], ['POST', '/api/admin/samples/5/unflag'],
    ['POST', '/api/admin/users/flag', { user_id: 'u1' }], ['POST', '/api/admin/users/disable', { user_id: 'u1' }],
    ['POST', '/api/admin/users/enable', { user_id: 'u1' }],
  ];
  for (const [method, path, body] of routes) {
    const t = setup({ who: { uid: 'fb-2', email: 'someone@example.com' } });
    const res = await t.handler(req(path, { method, body }));
    assert.equal(res.status, 403, `${method} ${path}`);
    assert.equal((await res.json()).code, 'not_admin');
    assert.equal(t.connects(), 0, `${method} ${path} connected`);
  }
});

test('signed out, expired or unverified: the sign-in error, no database', async () => {
  for (const err of [new AuthError(401, 'signin_required', 'Please sign in.'), new AuthError(403, 'unverified', 'Verify first.')]) {
    const t = setup({ who: err });
    const res = await t.handler(req('/api/admin/me'));
    assert.equal(res.status, err.status);
    assert.equal(t.connects(), 0);
  }
  const t = setup({ who: new Error('network down') });
  assert.equal((await t.handler(req('/api/admin/me'))).status, 503);
});

test('no ADMIN_EMAILS set: everyone is refused', async () => {
  const t = setup({ env: { SITE_ADMIN_DATABASE_URL: 'postgresql://test' } });
  assert.equal((await t.handler(req('/api/admin/me'))).status, 403);
  assert.equal(t.connects(), 0);
});

test('admin but no database login configured: 503', async () => {
  const t = setup({ env: { ADMIN_EMAILS: 'owner@example.com' } });
  assert.equal((await t.handler(req('/api/admin/me'))).status, 503);
});

test('the database refusing (email missing from site_admins) is a 403, not a 500', async () => {
  const t = setup({ dbError: Object.assign(new Error('not a site admin'), { code: '42501' }) });
  const res = await t.handler(req('/api/admin/me'));
  assert.equal(res.status, 403);
  assert.equal((await res.json()).code, 'not_admin_db');
  const t2 = setup({ dbError: Object.assign(new Error('permission denied for function whoami'), { code: '42501' }) });
  assert.equal((await t2.handler(req('/api/admin/me'))).status, 500);
});

test('unknown routes and other sites are refused before sign-in', async () => {
  const t = setup();
  assert.equal((await t.handler(req('/api/admin/samples/abc/flag', { method: 'POST' }))).status, 404);
  assert.equal((await t.handler(req('/api/admin/samples', { method: 'DELETE' }))).status, 404);
  assert.equal((await t.handler(req('/api/admin/me', { headers: { origin: 'https://evil.example' } }))).status, 403);
  assert.equal(t.connects(), 0);
});

test('flag passes the verified uid and email, the id and the reason', async () => {
  const t = setup();
  const res = await t.handler(req('/api/admin/samples/42/flag', { method: 'POST', body: { reason: '  spam link ' } }));
  assert.equal(res.status, 200);
  assert.deepEqual(await res.json(), { changed: true });
  assert.match(t.calls[0].sql, /admin_api\.flag_sample\(\$1, \$2, \$3, \$4\)/);
  assert.deepEqual(t.calls[0].params, ['fb-1', 'owner@example.com', 42, 'spam link']);
});

test('disable sends flag_samples as a boolean; bad input is refused before the database', async () => {
  const t = setup({ rows: [{ r: { status: 'ok', flagged: 3 } }] });
  const res = await t.handler(req('/api/admin/users/disable', { method: 'POST', body: { user_id: 'abc-123', reason: 'bot', flag_samples: true } }));
  assert.deepEqual(await res.json(), { flagged: 3 });
  assert.deepEqual(t.calls[0].params, ['fb-1', 'owner@example.com', 'abc-123', 'bot', true]);

  const bad = [
    { user_id: 'abc-123', flag_samples: 'yes' },
    { user_id: 'has space' },
    { user_id: '' },
    { user_id: 'abc', reason: 'x'.repeat(501) },
    { user_id: 'abc', reason: 5 },
  ];
  for (const body of bad) {
    const tb = setup();
    assert.equal((await tb.handler(req('/api/admin/users/disable', { method: 'POST', body }))).status, 400, JSON.stringify(body));
    assert.equal(tb.connects(), 0);
  }
  const tj = setup();
  assert.equal((await tj.handler(req('/api/admin/users/enable', { method: 'POST', body: '{not json' }))).status, 400);
  assert.equal((await tj.handler(req('/api/admin/users/enable', { method: 'POST', body: '[]' }))).status, 400);
  const big = setup();
  assert.equal((await big.handler(req('/api/admin/users/enable', { method: 'POST', body: { user_id: 'a', pad: 'x'.repeat(5000) } }))).status, 413);
});

test('database statuses become plain answers', async () => {
  const cases = [
    ['/api/admin/samples/9/unflag', undefined, { status: 'not_found' }, 404],
    ['/api/admin/users/flag', { user_id: 'nobody' }, { status: 'unknown_user' }, 404],
    ['/api/admin/users/disable', { user_id: 'me' }, { status: 'self', error: 'You cannot disable your own account.' }, 400],
    ['/api/admin/users/enable', { user_id: 'u1' }, { status: 'not_disabled' }, 404],
    ['/api/admin/users/enable', { user_id: 'u1' }, { status: 'ok' }, 200],
  ];
  for (const [path, body, r, status] of cases) {
    const t = setup({ rows: [{ r }] });
    assert.equal((await t.handler(req(path, { method: 'POST', body }))).status, status, `${path} ${r.status}`);
  }
});

test('sample list: filters reach the database, numbers come back as numbers, next page id', async () => {
  const row = { id: 7, species: 'Asclepias tuberosa', created_by: 'u1', ph: '6.40', organic_matter: null, created_at: new Date(0) };
  const t = setup({ rows: [row, { ...row, id: 6 }] });
  const res = await t.handler(req('/api/admin/samples?flagged=1&user=u1&q=%23123&before=100&limit=2'));
  const out = await res.json();
  assert.deepEqual(t.calls[0].params, ['fb-1', 'owner@example.com', true, 'u1', '#123', 100, 2]);
  assert.equal(out.samples[0].ph, 6.4);
  assert.equal(out.samples[0].organic_matter, null);
  assert.equal(out.samples[0].user_id, 'u1');
  assert.equal(out.next, 6);
  const t2 = setup({ rows: [] });
  assert.equal((await t2.handler(req('/api/admin/samples?limit=500'))).status, 400);
  assert.equal((await t2.handler(req('/api/admin/audit?action=DROP%20TABLE'))).status, 400);
  const t3 = setup({ rows: [] });
  await t3.handler(req('/api/admin/samples'));
  assert.deepEqual(t3.calls[0].params, ['fb-1', 'owner@example.com', false, null, null, null, 50]);
});
