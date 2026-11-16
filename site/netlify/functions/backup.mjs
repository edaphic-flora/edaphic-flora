// Scheduled: every Monday 09:17 UTC, save a backup of the contributed data to
// the private "backups" Netlify Blobs store and keep the newest eight.
// Runs only on the published production site (Netlify does not run scheduled
// functions on branch deploys). It has no URL: nobody can call it.
//
// Get a copy back with the Netlify CLI, from site/:
//   npx netlify blobs:list backups
//   npx netlify blobs:get backups backup-2026-11-16.json > backup.json
// or download a fresh one from /admin/.

import { getStore } from '@netlify/blobs';
import { withClient } from '../lib/db.mjs';
import { runBackup, STORE } from '../lib/backup.mjs';

export default async () => {
  try {
    const r = await runBackup({ connect: withClient, store: getStore(STORE), env: process.env });
    if (r.status === 'skipped') console.log('backup skipped:', r.reason);
    else console.log('backup saved:', r.key, JSON.stringify(r.rows), r.pruned.length ? `removed ${r.pruned.length} old` : '');
  } catch (e) {
    // No details: the error text could hold connection information
    console.error('backup failed:', e.code || '', String(e.message || '').slice(0, 120));
  }
  return new Response(null, { status: 204 });
};

export const config = { schedule: '17 9 * * 1' };
