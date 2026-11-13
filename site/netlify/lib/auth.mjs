// Verifies the Firebase ID token sent by the site as "Authorization: Bearer <token>".
//
// Checks the signature against Google's published keys and the claims Firebase
// documents for ID tokens (issuer, audience = project id, RS256 only, subject,
// auth time). Accepts only Google and email-and-password sign-ins with a
// verified email, matching the Shiny app's rules.

import { createRemoteJWKSet, jwtVerify } from 'jose';

const JWKS_URL = 'https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com';
const PROVIDERS = ['google.com', 'password'];
const TOKEN_RE = /^Bearer ([A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+)$/;

let remoteKeys = null;

export class AuthError extends Error {
  constructor(status, code, message) {
    super(message);
    this.status = status;
    this.code = code;
  }
}

/**
 * @param {Request} req
 * @param {{projectId: string, keys?: Function, now?: number}} opts  keys and now are for tests
 * @returns {Promise<{uid: string, email: string, provider: string}>}
 */
export async function verifyFirebaseToken(req, { projectId, keys, now } = {}) {
  if (!projectId) throw new AuthError(500, 'not_configured', 'Sign-in is not configured.');
  const m = TOKEN_RE.exec(req.headers.get('authorization') || '');
  if (!m) throw new AuthError(401, 'signin_required', 'Please sign in.');

  const keySet = keys || (remoteKeys ||= createRemoteJWKSet(new URL(JWKS_URL)));
  let payload;
  try {
    ({ payload } = await jwtVerify(m[1], keySet, {
      issuer: `https://securetoken.google.com/${projectId}`,
      audience: projectId,
      algorithms: ['RS256'],
      clockTolerance: 5,
      ...(now ? { currentDate: new Date(now * 1000) } : {}),
    }));
  } catch {
    throw new AuthError(401, 'invalid_token', 'Your sign-in has expired. Please sign in again.');
  }

  const t = now ?? Math.floor(Date.now() / 1000);
  if (typeof payload.sub !== 'string' || payload.sub.length < 1 || payload.sub.length > 128) {
    throw new AuthError(401, 'invalid_token', 'Your sign-in was not recognized.');
  }
  if (typeof payload.iat !== 'number' || payload.iat > t + 5 ||
      typeof payload.auth_time !== 'number' || payload.auth_time > t + 5) {
    throw new AuthError(401, 'invalid_token', 'Your sign-in was not recognized.');
  }
  const provider = payload.firebase?.sign_in_provider;
  if (!PROVIDERS.includes(provider)) {
    throw new AuthError(403, 'provider', 'Please sign in with Google or with your email and password.');
  }
  if (payload.email_verified !== true || typeof payload.email !== 'string' || !payload.email) {
    throw new AuthError(403, 'unverified', 'Please verify your email address first.');
  }
  return { uid: payload.sub, email: payload.email, provider };
}
