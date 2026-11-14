// GET /api/garden -> the signed-in person's My Garden dashboard (see
// site_api.my_garden in site/sql/garden.sql for the shape): their plants by
// native status, wildlife supported per group and family, and plant genera
// to add. Counts, family names and plant names only, as in the Shiny app.
//
// Needs a Firebase ID token (Authorization: Bearer ...). Uses the same
// site_writer login as api.mjs, which can only call site_api functions. No
// Netlify rate-limit rule here: the free plan allows two, and lookup.mjs and
// api.mjs use them.

import { verifyFirebaseToken, AuthError } from '../lib/auth.mjs';
import { withClient, json } from '../lib/db.mjs';

const ORIGIN_OK = /^(https:\/\/(www\.)?edaphicflora\.org|https:\/\/([a-z0-9-]+--)?edaphicflora\.netlify\.app|http:\/\/localhost:(8888|4321))$/;

export default async (req) => {
  const origin = req.headers.get('origin');
  if (origin && !ORIGIN_OK.test(origin)) return json({ error: 'Forbidden' }, 403);
  if (req.method !== 'GET') return json({ error: 'Method not allowed' }, 405, { Allow: 'GET' });

  let who;
  try {
    who = await verifyFirebaseToken(req, { projectId: process.env.PUBLIC_FIREBASE_PROJECT_ID });
  } catch (e) {
    if (e instanceof AuthError) return json({ error: e.message, code: e.code }, e.status);
    console.error('auth failed:', e.message);
    return json({ error: 'Sign-in could not be checked. Please try again.' }, 503);
  }

  try {
    return await withClient(process.env.SITE_WRITER_DATABASE_URL, async (c) => {
      const { rows: [u] } = await c.query('SELECT * FROM site_api.resolve_user($1, $2)', [who.uid, who.email]);
      const { rows: [r] } = await c.query('SELECT site_api.my_garden($1) AS g', [u.user_id]);
      if (r.g?.status !== 'ok') return json({ error: 'Please sign out and sign in again.' }, 403);
      return json(r.g);
    });
  } catch (e) {
    console.error('garden error:', e.message);
    return json({ error: 'Something went wrong on our end. Please try again in a minute.' }, 500);
  }
};

export const config = {
  path: '/api/garden',
};
