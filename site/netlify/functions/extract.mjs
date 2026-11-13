// POST /api/extract  { filename, media_type, data }  (data = base64 PDF or image)
//                 or { filename, media_type: 'text/plain', text }  (RTF/TXT/CSV, converted in the browser)
// -> { soil, texture, sample_date, notes, warnings, found, extractions_left }
//
// Reads a soil lab report (PDF or photo) with Claude so the contribute form
// can be filled in for the person to check. Same limits as the Shiny app: 3
// per person and 20 overall per day (enforced in the database), and the file
// is never stored. No Netlify rate-limit rule here: the free plan allows two,
// and lookup.mjs and api.mjs use them.

import Anthropic from '@anthropic-ai/sdk';
import { verifyFirebaseToken, AuthError } from '../lib/auth.mjs';
import { withClient, json } from '../lib/db.mjs';
import { EXTRACT_MODEL, EXTRACTION_PROMPT, EXTRACTION_SCHEMA, MAX_FILE_BYTES, MAX_TEXT_CHARS, MEDIA_TYPES, cleanExtraction } from '../lib/extract.mjs';

const ORIGIN_OK = /^(https:\/\/(www\.)?edaphicflora\.org|https:\/\/([a-z0-9-]+--)?edaphicflora\.netlify\.app|http:\/\/localhost:(8888|4321))$/;
const MAX_BODY = Math.ceil((MAX_FILE_BYTES * 4) / 3) + 4096;

let anthropic = null;
// Netlify allows 60 s per call; leave room for the database steps
const claude = () => (anthropic ||= new Anthropic({ apiKey: process.env.ANTHROPIC_API_KEY, timeout: 50_000, maxRetries: 0 }));

export default async (req) => {
  const origin = req.headers.get('origin');
  if (origin && !ORIGIN_OK.test(origin)) return json({ error: 'Forbidden' }, 403);
  if (req.method !== 'POST') return json({ error: 'Method not allowed' }, 405, { Allow: 'POST' });
  if (!process.env.ANTHROPIC_API_KEY) return json({ error: 'Reading reports is not set up yet.' }, 503);

  let who;
  try {
    who = await verifyFirebaseToken(req, { projectId: process.env.PUBLIC_FIREBASE_PROJECT_ID });
  } catch (e) {
    if (e instanceof AuthError) return json({ error: e.message, code: e.code }, e.status);
    return json({ error: 'Sign-in could not be checked. Please try again.' }, 503);
  }

  const text = await req.text();
  if (text.length > MAX_BODY) return json({ error: 'That file is too large. Use a file under 4 MB.' }, 413);
  let body;
  try {
    body = JSON.parse(text);
  } catch {
    return json({ error: 'The upload was not understood.' }, 400);
  }
  const mediaType = body?.media_type;
  const isText = mediaType === 'text/plain';
  const data = typeof body?.data === 'string' ? body.data : '';
  const reportText = typeof body?.text === 'string' ? body.text.trim() : '';
  const filename = typeof body?.filename === 'string' ? body.filename.slice(0, 120) : 'report';
  if (!MEDIA_TYPES.includes(mediaType)) {
    return json({ error: 'Upload a PDF, a photo, or an RTF, TXT or CSV file.' }, 400);
  }
  if (isText) {
    if (reportText.length < 20) return json({ error: 'That file has no readable text.' }, 400);
    if (reportText.length > MAX_TEXT_CHARS) return json({ error: 'That file is too long to read.' }, 413);
  } else {
    if (!data || !/^[A-Za-z0-9+/]+={0,2}$/.test(data)) return json({ error: 'The upload was not understood.' }, 400);
    if ((data.length * 3) / 4 > MAX_FILE_BYTES) return json({ error: 'That file is too large. Use a file under 4 MB.' }, 413);
  }

  const db = process.env.SITE_WRITER_DATABASE_URL;
  let userId;
  let claim;
  try {
    ({ userId, claim } = await withClient(db, async (c) => {
      const { rows: [u] } = await c.query('SELECT * FROM site_api.resolve_user($1, $2)', [who.uid, who.email]);
      const { rows: [r] } = await c.query('SELECT site_api.claim_extraction($1, $2) AS r', [u.user_id, filename]);
      return { userId: u.user_id, claim: r.r };
    }));
  } catch (e) {
    console.error('extract claim failed:', e.message);
    return json({ error: 'Something went wrong on our end. Please try again in a minute.' }, 500);
  }
  if (claim.status === 'user_limit') {
    return json({ code: 'limit', error: `You can read up to ${claim.limit} reports a day. Enter the values by hand, or try again tomorrow.` }, 429);
  }
  if (claim.status === 'global_limit') {
    return json({ code: 'busy', error: 'Report reading is at its daily limit for everyone. Enter the values by hand, or try again tomorrow.' }, 429);
  }
  if (claim.status !== 'ok') {
    return json({ code: claim.status, error: 'This account cannot read reports. Email us if you think that is a mistake.' }, 403);
  }

  const release = () =>
    withClient(db, (c) => c.query('SELECT site_api.release_extraction($1, $2)', [userId, claim.id])).catch(() => {});

  const fileBlock = isText
    ? { type: 'document', source: { type: 'text', media_type: 'text/plain', data: reportText }, title: filename }
    : mediaType === 'application/pdf'
      ? { type: 'document', source: { type: 'base64', media_type: 'application/pdf', data } }
      : { type: 'image', source: { type: 'base64', media_type: mediaType, data } };

  let response;
  try {
    response = await claude().beta.messages.create({
      model: EXTRACT_MODEL,
      max_tokens: 16000,
      // A simple, well-specified extraction: low effort is enough
      output_config: { effort: 'low', format: { type: 'json_schema', schema: EXTRACTION_SCHEMA } },
      // If the model declines, let the API retry on a fallback model
      betas: ['server-side-fallback-2026-07-01'],
      fallbacks: 'default',
      messages: [{ role: 'user', content: [fileBlock, { type: 'text', text: EXTRACTION_PROMPT }] }],
    });
  } catch (e) {
    await release();
    if (e instanceof Anthropic.BadRequestError) {
      return json({ error: 'That file could not be read. Try a clearer photo, or a PDF.' }, 422);
    }
    if (e instanceof Anthropic.RateLimitError || e instanceof Anthropic.APIConnectionTimeoutError) {
      return json({ error: 'Report reading is busy right now. Please try again in a minute.' }, 503);
    }
    console.error('extract failed:', e?.status || '', e?.message);
    return json({ error: 'Reading the report did not work. Please try again, or enter the values by hand.' }, 502);
  }

  const textBlock = response.content.find((b) => b.type === 'text');
  let raw = null;
  if (response.stop_reason === 'end_turn' && textBlock) {
    try {
      raw = JSON.parse(textBlock.text);
    } catch {
      raw = null;
    }
  }
  if (!raw) {
    await release();
    return json({ error: 'The report could not be read. Try a clearer photo, or enter the values by hand.' }, 422);
  }

  const tokens = (response.usage?.input_tokens || 0) + (response.usage?.output_tokens || 0);
  await withClient(db, (c) => c.query('SELECT site_api.finish_extraction($1, $2, $3)', [userId, claim.id, tokens])).catch((e) =>
    console.error('extract log failed:', e.message)
  );

  const out = cleanExtraction(raw, { today: new Date().toISOString().slice(0, 10) });
  return json({ ...out, extractions_left: claim.remaining });
};

export const config = { path: '/api/extract' };
