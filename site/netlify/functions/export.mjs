// GET /api/export/my-samples -> text/csv (the signed-in person's own samples)
//
// Needs a Firebase ID token (Authorization: Bearer ...). The database does the
// rest in site_api.export_my_samples (site/sql/export.sql): 10 exports per
// person per day, none for disabled accounts, each one written to audit_log
// as 'export', like the Shiny app. No Netlify rate-limit rule here: the free
// plan allows two, and lookup.mjs and api.mjs use them.

import { verifyFirebaseToken, AuthError } from '../lib/auth.mjs';
import { withClient, json } from '../lib/db.mjs';
import { mySamplesCsv, mySamplesFilename } from '../lib/csv.mjs';

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

  let out;
  try {
    out = await withClient(process.env.SITE_WRITER_DATABASE_URL, async (c) => {
      const { rows: [u] } = await c.query('SELECT * FROM site_api.resolve_user($1, $2)', [who.uid, who.email]);
      const { rows: [r] } = await c.query('SELECT site_api.export_my_samples($1) AS r', [u.user_id]);
      return r.r;
    });
  } catch (e) {
    console.error('export error:', e.message);
    return json({ error: 'Something went wrong on our end. Please try again in a minute.' }, 500);
  }

  switch (out.status) {
    case 'ok': {
      const today = new Date().toISOString().slice(0, 10);
      return new Response(mySamplesCsv(out.rows), {
        headers: {
          'Content-Type': 'text/csv; charset=utf-8',
          'Content-Disposition': `attachment; filename="${mySamplesFilename(today)}"`,
          'Cache-Control': 'no-store',
          'X-Content-Type-Options': 'nosniff',
          'X-Row-Count': String(out.rows.length),
          'X-Exports-Left': String(out.remaining),
        },
      });
    }
    case 'rate_limited':
      return json({
        code: 'rate_limited',
        error: `You've reached the daily download limit (${out.limit} per day). Please try again tomorrow.`,
      }, 429);
    case 'disabled':
      return json({ code: 'disabled', error: 'This account cannot download samples. Email us if you think that is a mistake.' }, 403);
    default:
      return json({ error: 'Please sign out and sign in again.' }, 403);
  }
};

export const config = { path: '/api/export/my-samples' };
