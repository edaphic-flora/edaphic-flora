// Weekly backup: one JSON document of everything contributed (samples,
// accounts, logs), written to a private Netlify Blobs store. The newest
// KEEP copies are kept and older ones deleted.
//
// The data comes from admin_api.backup_export (sql/admin.sql), which only an
// email in site_admins may call and which logs every call.

import { adminEmails } from './admin.mjs';

export const KEEP = 8;
export const STORE = 'backups';
const KEY_RE = /^backup-\d{4}-\d{2}-\d{2}\.json$/;

export const backupKey = (date = new Date()) => `backup-${date.toISOString().slice(0, 10)}.json`;

// Keys to delete so only the newest `keep` dated backups remain
export function keysToPrune(keys, keep = KEEP) {
  const dated = keys.filter((k) => KEY_RE.test(k)).sort();
  return dated.slice(0, Math.max(0, dated.length - keep));
}

/**
 * @param {object} deps
 * @param {(url: string, fn: Function) => Promise<any>} deps.connect  lib/db.mjs withClient
 * @param {{setJSON: Function, list: Function, delete: Function}} deps.store  a Netlify Blobs store
 * @param {Record<string, string|undefined>} deps.env
 * @param {Date} [deps.now]
 * @returns {Promise<{status: 'skipped'|'saved', key?: string, rows?: Record<string, number>, pruned?: string[], reason?: string}>}
 */
export async function runBackup({ connect, store, env, now = new Date() }) {
  const db = env.SITE_ADMIN_DATABASE_URL;
  const emails = [...adminEmails(env.ADMIN_EMAILS)];
  if (!db || !emails.length) return { status: 'skipped', reason: 'SITE_ADMIN_DATABASE_URL or ADMIN_EMAILS is not set' };

  // Any address that is in both admin lists will do
  let data = null;
  let lastError = null;
  for (const email of emails) {
    try {
      data = await connect(db, async (c) =>
        (await c.query('SELECT admin_api.backup_export($1, $2, $3) AS r', ['scheduled-backup', email, 'weekly'])).rows[0].r
      );
      break;
    } catch (e) {
      lastError = e;
      if (e.code !== '42501') throw e;
    }
  }
  if (!data) throw lastError;

  const key = backupKey(now);
  await store.setJSON(key, data);
  const { blobs } = await store.list();
  const pruned = keysToPrune(blobs.map((b) => b.key));
  for (const k of pruned) await store.delete(k);
  const rows = Object.fromEntries(Object.entries(data.tables || {}).map(([t, v]) => [t, Array.isArray(v) ? v.length : 0]));
  return { status: 'saved', key, rows, pruned };
}
