// The /admin/ API (functions/admin.mjs): who may use it, what it accepts,
// and the admin_api database calls.
//
// An admin is a verified Firebase sign-in (lib/auth.mjs) whose email is in the
// ADMIN_EMAILS Netlify variable: comma-separated, any case, spaces ignored,
// as in the Shiny app. The database checks the email again against its own
// list (site_admins in sql/admin.sql), so this list alone is not enough.

import { verifyFirebaseToken, AuthError } from './auth.mjs';
import { withClient, json } from './db.mjs';

export function adminEmails(value) {
  return new Set(
    String(value ?? '')
      .split(',')
      .map((s) => s.trim().toLowerCase())
      .filter(Boolean)
  );
}

export function isAdminEmail(email, value) {
  if (typeof email !== 'string' || !email.trim()) return false;
  return adminEmails(value).has(email.trim().toLowerCase());
}

export const MAX_REASON = 500;
export const MAX_SEARCH = 100;
// Contributor ids are Polished ids (from the app) or Firebase uids
const USER_ID_RE = /^[\x21-\x7e]{1,128}$/;

export class InputError extends Error {}

// Optional free text: trimmed string or null
export function optionalText(v, max, label) {
  if (v === undefined || v === null) return null;
  if (typeof v !== 'string') throw new InputError(`${label} must be text.`);
  const t = v.trim();
  if (t.length > max) throw new InputError(`Keep the ${label.toLowerCase()} under ${max} characters.`);
  return t || null;
}

export function userId(v) {
  if (typeof v !== 'string' || !USER_ID_RE.test(v)) throw new InputError('That contributor id does not look right.');
  return v;
}

// Positive whole number from a query string, or null when absent
export function optionalInt(v, { max = 999_999_999 } = {}) {
  if (v === null || v === undefined || v === '') return null;
  if (!/^\d{1,9}$/.test(v) || Number(v) < 1 || Number(v) > max) throw new InputError('A number in the request is out of range.');
  return Number(v);
}

// ---- the handler (functions/admin.mjs exports it)

const MAX_BODY = 4096;
const ORIGIN_OK = /^(https:\/\/(www\.)?edaphicflora\.org|https:\/\/([a-z0-9-]+--)?edaphicflora\.netlify\.app|http:\/\/localhost:(8888|4321))$/;
const SAMPLE_ACTION = /^\/api\/admin\/samples\/(\d{1,9})\/(flag|unflag)$/;

const NOT_ADMIN = 'This page is for site administrators.';
const NOT_IN_DB =
  'Your email is in ADMIN_EMAILS but not in the database admin list. Add it to site_admins (see site/sql/admin.sql).';

function routeOf(method, path) {
  const m = SAMPLE_ACTION.exec(path);
  if (method === 'POST' && m) return { name: m[2], id: Number(m[1]) };
  const name = {
    'GET /api/admin/me': 'me',
    'GET /api/admin/samples': 'samples',
    'GET /api/admin/contributors': 'contributors',
    'GET /api/admin/disabled': 'disabled',
    'GET /api/admin/audit': 'audit',
    'POST /api/admin/users/flag': 'flag_user',
    'POST /api/admin/users/disable': 'disable',
    'POST /api/admin/users/enable': 'enable',
  }[`${method} ${path}`];
  return name ? { name } : null;
}

const num = (v) => (v == null ? null : Number(v));

/**
 * @param {object} [deps] for tests
 * @param {(req: Request) => Promise<{uid: string, email: string}>} [deps.verify]
 * @param {(url: string, fn: Function) => Promise<any>} [deps.connect]
 * @param {Record<string, string|undefined>} [deps.env]
 */
export function createAdminHandler({
  verify = (req) => verifyFirebaseToken(req, { projectId: process.env.PUBLIC_FIREBASE_PROJECT_ID }),
  connect = withClient,
  env = process.env,
} = {}) {
  return async (req) => {
    const origin = req.headers.get('origin');
    if (origin && !ORIGIN_OK.test(origin)) return json({ error: 'Forbidden' }, 403);

    const url = new URL(req.url);
    const route = routeOf(req.method, url.pathname.replace(/\/+$/, ''));
    if (!route) return json({ error: 'Not found' }, 404);

    // 1. A verified sign-in
    let who;
    try {
      who = await verify(req);
    } catch (e) {
      if (e instanceof AuthError) return json({ error: e.message, code: e.code }, e.status);
      console.error('admin auth failed:', e.message);
      return json({ error: 'Sign-in could not be checked. Please try again.' }, 503);
    }
    // 2. An admin email, checked here before anything touches the database
    if (!isAdminEmail(who.email, env.ADMIN_EMAILS)) return json({ error: NOT_ADMIN, code: 'not_admin' }, 403);
    if (!env.SITE_ADMIN_DATABASE_URL) {
      return json({ error: 'Admin tools are not set up on this copy of the site yet.', code: 'not_configured' }, 503);
    }

    // 3. The request itself
    let args;
    try {
      args = await readArgs(req, route, url.searchParams);
    } catch (e) {
      if (e instanceof InputError) return json({ error: e.message }, e.status || 400);
      throw e;
    }

    try {
      return await connect(env.SITE_ADMIN_DATABASE_URL, (c) => run(c, route.name, [who.uid, who.email], args, who));
    } catch (e) {
      if (e.code === '42501' && /not a site admin/.test(e.message || '')) {
        return json({ error: NOT_IN_DB, code: 'not_admin_db' }, 403);
      }
      console.error('admin api error:', e.code || '', e.message);
      return json({ error: 'Something went wrong on our end. Please try again in a minute.' }, 500);
    }
  };
}

async function readArgs(req, route, qs) {
  if (req.method === 'GET') {
    if (route.name === 'samples') {
      return {
        flagged: qs.get('flagged') === '1' || qs.get('flagged') === 'true',
        user: qs.get('user') ? userId(qs.get('user').trim()) : null,
        q: optionalText(qs.get('q'), MAX_SEARCH, 'Search'),
        before: optionalInt(qs.get('before')),
        limit: optionalInt(qs.get('limit'), { max: 200 }) ?? 50,
      };
    }
    if (route.name === 'audit') {
      const action = optionalText(qs.get('action'), 50, 'Action');
      if (action && !/^[a-z_]{1,50}$/.test(action)) throw new InputError('Unknown action.');
      return {
        action,
        q: optionalText(qs.get('q'), MAX_SEARCH, 'Search'),
        before: optionalInt(qs.get('before')),
        limit: optionalInt(qs.get('limit'), { max: 500 }) ?? 100,
      };
    }
    return {};
  }

  // POST: unflag needs no body; the rest send small JSON objects
  if (route.name === 'unflag') return { id: route.id };
  if (!(req.headers.get('content-type') || '').includes('application/json')) {
    throw Object.assign(new InputError('Send JSON.'), { status: 415 });
  }
  const text = await req.text();
  if (text.length > MAX_BODY) throw Object.assign(new InputError('That request is too large.'), { status: 413 });
  let body;
  try {
    body = JSON.parse(text);
  } catch {
    throw new InputError('The request was not understood.');
  }
  if (!body || typeof body !== 'object' || Array.isArray(body)) throw new InputError('The request was not understood.');

  const reason = optionalText(body.reason, MAX_REASON, 'Reason');
  if (route.name === 'flag') return { id: route.id, reason };
  const user = userId(body.user_id);
  if (route.name === 'enable') return { user };
  if (route.name === 'flag_user') return { user, reason };
  if (body.flag_samples !== undefined && typeof body.flag_samples !== 'boolean') {
    throw new InputError('flag_samples must be true or false.');
  }
  return { user, reason, flagSamples: body.flag_samples === true }; // disable
}

async function run(c, name, admin, a, who) {
  const one = async (sql, params) => (await c.query(sql, [...admin, ...params])).rows[0].r;

  switch (name) {
    case 'me': {
      const r = await one('SELECT admin_api.whoami($1, $2) AS r', []);
      return json({ email: who.email, ...r });
    }

    case 'samples': {
      const { rows } = await c.query('SELECT * FROM admin_api.samples($1, $2, $3, $4, $5, $6, $7)', [
        ...admin, a.flagged, a.user, a.q, a.before, a.limit,
      ]);
      return json({
        samples: rows.map((r) => ({
          id: r.id,
          species: r.species,
          cultivar: r.cultivar,
          user_id: r.created_by,
          ph: num(r.ph),
          organic_matter: num(r.organic_matter),
          texture_class: r.texture_class,
          outcome: r.outcome,
          ecoregion: r.ecoregion_l4,
          notes: r.notes,
          inat_url: r.inat_url,
          date: r.sample_date,
          added: r.created_at,
          flagged: r.flagged,
          flag_reason: r.flag_reason,
          user_disabled: r.user_disabled,
        })),
        next: rows.length === a.limit ? rows[rows.length - 1].id : null,
      });
    }

    case 'flag':
    case 'unflag': {
      const r = name === 'flag'
        ? await one('SELECT admin_api.flag_sample($1, $2, $3, $4) AS r', [a.id, a.reason])
        : await one('SELECT admin_api.unflag_sample($1, $2, $3) AS r', [a.id]);
      if (r.status === 'ok') return json({ changed: r.changed });
      if (r.status === 'not_found') return json({ error: 'That sample was not found.' }, 404);
      return json({ error: r.error || 'That was not allowed.' }, 400);
    }

    case 'contributors': {
      const { rows } = await c.query('SELECT * FROM admin_api.contributors($1, $2, $3)', [...admin, 2000]);
      return json({ contributors: rows });
    }

    case 'disabled': {
      const { rows } = await c.query('SELECT * FROM admin_api.disabled_accounts($1, $2)', admin);
      return json({ disabled: rows });
    }

    case 'flag_user':
    case 'disable': {
      const r = name === 'flag_user'
        ? await one('SELECT admin_api.flag_user_samples($1, $2, $3, $4) AS r', [a.user, a.reason])
        : await one('SELECT admin_api.disable_user($1, $2, $3, $4, $5) AS r', [a.user, a.reason, a.flagSamples]);
      if (r.status === 'ok') return json({ flagged: r.flagged });
      if (r.status === 'unknown_user') return json({ error: 'No contributor has that id.' }, 404);
      return json({ error: r.error || 'That was not allowed.' }, 400);
    }

    case 'enable': {
      const r = await one('SELECT admin_api.enable_user($1, $2, $3) AS r', [a.user]);
      if (r.status === 'ok') return json({});
      return json({ error: 'That account is not disabled.' }, 404);
    }

    case 'audit': {
      const { rows } = await c.query('SELECT * FROM admin_api.audit_entries($1, $2, $3, $4, $5, $6)', [
        ...admin, a.action, a.q, a.before, a.limit,
      ]);
      return json({ entries: rows, next: rows.length === a.limit ? rows[rows.length - 1].id : null });
    }

    default:
      return json({ error: 'Not found' }, 404);
  }
}

