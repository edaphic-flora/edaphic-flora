// Signed-in API for contributing soil tests.
//
//   GET    /api/me            -> { email, disabled, remaining_today, extractions_left, home: { zip, state, city } }
//   PUT    /api/me            { zip, state } -> { home: { zip, state, city } }   saves the home location
//   GET    /api/samples       -> { samples: [...] }       the person's own samples
//   POST   /api/samples       -> 201 { ids, ecoregion, warnings }
//   DELETE /api/samples/:id   -> 204
//
// Every call needs a Firebase ID token (Authorization: Bearer ...). The
// database login (SITE_WRITER_DATABASE_URL) can only call the site_api
// functions in site/sql/contribute.sql, never touch tables directly.

import { verifyFirebaseToken, AuthError } from '../lib/auth.mjs';
import { validateSubmission } from '../lib/validate.mjs';
import { withClient, json } from '../lib/db.mjs';

const MAX_BODY = 32 * 1024;
const ORIGIN_OK = /^(https:\/\/(www\.)?edaphicflora\.org|https:\/\/([a-z0-9-]+--)?edaphicflora\.netlify\.app|http:\/\/localhost:(8888|4321))$/;

const listNames = (names) =>
  names.length <= 2 ? names.join(' and ') : `${names.slice(0, -1).join(', ')} and ${names[names.length - 1]}`;

export default async (req) => {
  const origin = req.headers.get('origin');
  if (origin && !ORIGIN_OK.test(origin)) return json({ error: 'Forbidden' }, 403);

  const path = new URL(req.url).pathname.replace(/\/+$/, '');
  const sampleId = /^\/api\/samples\/(\d{1,9})$/.exec(path)?.[1];
  const route =
    path === '/api/me' && req.method === 'GET' ? 'me'
    : path === '/api/me' && req.method === 'PUT' ? 'prefs'
    : path === '/api/samples' && req.method === 'GET' ? 'list'
    : path === '/api/samples' && req.method === 'POST' ? 'create'
    : sampleId && req.method === 'DELETE' ? 'delete'
    : null;
  if (!route) return json({ error: 'Not found' }, 404);

  let who;
  try {
    who = await verifyFirebaseToken(req, { projectId: process.env.PUBLIC_FIREBASE_PROJECT_ID });
  } catch (e) {
    if (e instanceof AuthError) return json({ error: e.message, code: e.code }, e.status);
    console.error('auth failed:', e.message);
    return json({ error: 'Sign-in could not be checked. Please try again.' }, 503);
  }

  // Read and check the body before opening a database connection
  let body = null;
  if (route === 'create' || route === 'prefs') {
    if (!(req.headers.get('content-type') || '').includes('application/json')) {
      return json({ error: 'Send JSON.' }, 415);
    }
    const text = await req.text();
    if (text.length > MAX_BODY) return json({ error: 'That submission is too large.' }, 413);
    try {
      body = JSON.parse(text);
    } catch {
      return json({ error: 'The submission was not understood.' }, 400);
    }
  }

  try {
    return await withClient(process.env.SITE_WRITER_DATABASE_URL, async (c) => {
      const { rows: [u] } = await c.query('SELECT * FROM site_api.resolve_user($1, $2)', [who.uid, who.email]);

      if (route === 'me') {
        const [{ rows: prefs }, { rows: [left] }] = await Promise.all([
          c.query('SELECT * FROM site_api.get_prefs($1)', [u.user_id]),
          c.query('SELECT site_api.extractions_left($1) AS n', [u.user_id]),
        ]);
        const p = prefs[0] || {};
        return json({
          email: who.email,
          disabled: u.disabled,
          remaining_today: u.remaining_today,
          extractions_left: left.n,
          home: { zip: p.home_zipcode || null, state: p.home_state || null, city: p.home_city || null },
        });
      }

      if (route === 'prefs') {
        if (body === null || typeof body !== 'object' || Array.isArray(body)) return json({ error: 'Not understood.' }, 400);
        const zip = typeof body.zip === 'string' ? body.zip.trim() : '';
        const state = typeof body.state === 'string' ? body.state.trim() : '';
        if (zip && !/^\d{5}$/.test(zip)) return json({ error: 'Enter a 5-digit zip code.' }, 400);
        if (state && !/^[A-Za-z]{2}$/.test(state)) return json({ error: 'Choose a state from the list.' }, 400);
        const { rows: [r] } = await c.query('SELECT site_api.save_prefs($1, $2, $3) AS r', [u.user_id, zip, state]);
        if (r.r.status !== 'ok') return json({ error: r.r.error || 'Could not save that.' }, 400);
        return json({ home: { zip: r.r.home_zipcode, state: r.r.home_state, city: r.r.home_city } });
      }

      if (route === 'list') {
        const { rows } = await c.query(
          'SELECT *, sample_date::text AS sample_date_text FROM site_api.my_samples($1)',
          [u.user_id]
        );
        return json({
          samples: rows.map((r) => ({
            id: r.id,
            species: r.species,
            cultivar: r.cultivar,
            outcome: r.outcome,
            sun_exposure: r.sun_exposure,
            site_hydrology: r.site_hydrology,
            ph: r.ph == null ? null : Number(r.ph),
            organic_matter: r.organic_matter == null ? null : Number(r.organic_matter),
            texture_class: r.texture_class,
            ecoregion: r.ecoregion_l4,
            date: r.sample_date_text,
            added: r.created_at,
            flagged: r.flagged,
          })),
        });
      }

      if (route === 'delete') {
        const { rows: [r] } = await c.query('SELECT site_api.delete_my_sample($1, $2) AS ok', [u.user_id, Number(sampleId)]);
        return r.ok ? json(null, 204) : json({ error: 'That sample was not found.' }, 404);
      }

      // create
      if (u.disabled) {
        return json({ error: 'This account cannot add samples. Email us if you think that is a mistake.', code: 'disabled' }, 403);
      }
      const v = validateSubmission(body, { today: new Date().toISOString().slice(0, 10) });
      if (!v.ok) return json({ errors: v.errors, warnings: v.warnings }, 400);
      const { soil, plants, location, date, notes, confirm_duplicates } = v.value;
      const { rows: [r] } = await c.query(
        'SELECT site_api.submit_samples($1, $2::jsonb, $3::jsonb, $4::jsonb, $5::date, $6, $7) AS r',
        [u.user_id, JSON.stringify(soil), JSON.stringify(plants), JSON.stringify(location), date, notes, confirm_duplicates]
      );
      const out = r.r;
      switch (out.status) {
        case 'ok':
          return json({ ids: out.ids, ecoregion: out.ecoregion, warnings: v.warnings }, 201);
        case 'duplicate':
          return json({
            code: 'duplicate',
            species: out.species,
            error: `You already added ${listNames(out.species)} in the last 24 hours. Add it again anyway?`,
          }, 409);
        case 'rate_limited':
          return json({
            code: 'rate_limited',
            remaining: out.remaining,
            error: out.remaining > 0
              ? `You can add ${out.remaining} more ${out.remaining === 1 ? 'sample' : 'samples'} today (20 per day). Remove some plants or try again tomorrow.`
              : "You've reached the daily limit (20 samples per day). Please try again tomorrow.",
          }, 429);
        case 'busy':
          return json({ code: 'busy', error: 'We are getting a lot of submissions right now. Please try again later today.' }, 503);
        case 'disabled':
          return json({ code: 'disabled', error: 'This account cannot add samples. Email us if you think that is a mistake.' }, 403);
        case 'invalid_species':
          return json({ errors: [`Not in our species list: ${listNames(out.species)}. Choose names from the search list.`] }, 400);
        case 'invalid':
          return json({ errors: [out.error] }, 400);
        default:
          return json({ error: 'Please sign out and sign in again.' }, 403);
      }
    });
  } catch (e) {
    console.error('api error:', e.message);
    return json({ error: 'Something went wrong on our end. Please try again in a minute.' }, 500);
  }
};

export const config = {
  path: ['/api/me', '/api/samples', '/api/samples/:id'],
  rateLimit: { windowLimit: 20, windowSize: 60, aggregateBy: ['ip', 'domain'] },
};
