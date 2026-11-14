// Moderation API for /admin/ (admins only).
//
//   GET  /api/admin/me                      -> { email, actor, samples, flagged, samples_24h, contributors, disabled, actions }
//   GET  /api/admin/samples?flagged=1&user=&q=&before=&limit=   -> { samples, next }
//   POST /api/admin/samples/:id/flag        { reason } -> { changed }
//   POST /api/admin/samples/:id/unflag                 -> { changed }
//   GET  /api/admin/contributors            -> { contributors }
//   GET  /api/admin/disabled                -> { disabled }
//   POST /api/admin/users/flag              { user_id, reason } -> { flagged }
//   POST /api/admin/users/disable           { user_id, reason, flag_samples } -> { flagged }
//   POST /api/admin/users/enable            { user_id } -> {}
//   GET  /api/admin/audit?action=&q=&before=&limit=   -> { entries, next }
//
// Every call needs a Firebase ID token (Authorization: Bearer ...) whose
// verified email is in ADMIN_EMAILS; anyone else is refused here, before any
// database connection. The database login (SITE_ADMIN_DATABASE_URL) can only
// call the admin_api functions in site/sql/admin.sql, which check the email
// again against site_admins and write the audit log. No Netlify rate-limit
// rule: the free plan allows two, and lookup.mjs and api.mjs use them.

import { createAdminHandler } from '../lib/admin.mjs';

export default createAdminHandler();

export const config = { path: '/api/admin/*' };
