// Token checks, using a locally generated key in place of Google's.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { SignJWT, exportJWK, generateKeyPair, createLocalJWKSet } from 'jose';
import { verifyFirebaseToken, AuthError } from '../netlify/lib/auth.mjs';

const PID = 'test-project';
const NOW = 1_800_000_000;
const { publicKey, privateKey } = await generateKeyPair('RS256');
const jwk = { ...(await exportJWK(publicKey)), kid: 'k1', alg: 'RS256', use: 'sig' };
const keys = createLocalJWKSet({ keys: [jwk] });

const claims = (over = {}) => ({
  sub: 'firebase-uid-123',
  email: 'person@example.com',
  email_verified: true,
  auth_time: NOW - 60,
  firebase: { sign_in_provider: 'google.com' },
  ...over,
});
async function token(over = {}, { iss = `https://securetoken.google.com/${PID}`, aud = PID, iat = NOW - 10, exp = NOW + 3000, key = privateKey, alg = 'RS256' } = {}) {
  return new SignJWT(claims(over)).setProtectedHeader({ alg, kid: 'k1' }).setIssuer(iss).setAudience(aud)
    .setIssuedAt(iat).setExpirationTime(exp).sign(key);
}
const req = (t) => new Request('https://edaphicflora.org/api/me', { headers: t === undefined ? {} : { authorization: t } });
const verify = (t) => verifyFirebaseToken(req(t), { projectId: PID, keys, now: NOW });
async function rejects(t, status, code) {
  await assert.rejects(verify(t), (e) => e instanceof AuthError && e.status === status && (!code || e.code === code));
}

test('a valid Google or password token passes', async () => {
  assert.deepEqual(await verify(`Bearer ${await token()}`), { uid: 'firebase-uid-123', email: 'person@example.com', provider: 'google.com' });
  const pw = await verify(`Bearer ${await token({ firebase: { sign_in_provider: 'password' } })}`);
  assert.equal(pw.provider, 'password');
});

test('missing or malformed header', async () => {
  await rejects(undefined, 401, 'signin_required');
  await rejects('Bearer', 401, 'signin_required');
  await rejects(`Basic ${await token()}`, 401, 'signin_required');
  await rejects('Bearer abc.def', 401, 'signin_required');
});

test('wrong audience, wrong issuer, expired, not yet valid', async () => {
  await rejects(`Bearer ${await token({}, { aud: 'other-project' })}`, 401, 'invalid_token');
  await rejects(`Bearer ${await token({}, { iss: 'https://securetoken.google.com/other' })}`, 401, 'invalid_token');
  await rejects(`Bearer ${await token({}, { exp: NOW - 100 })}`, 401, 'invalid_token');
  await rejects(`Bearer ${await token({}, { iat: NOW + 600 })}`, 401, 'invalid_token');
});

test('unsigned, HS256 and foreign-key tokens', async () => {
  const b64 = (o) => Buffer.from(JSON.stringify(o)).toString('base64url');
  const body = b64({ ...claims(), iss: `https://securetoken.google.com/${PID}`, aud: PID, iat: NOW, exp: NOW + 100 });
  await rejects(`Bearer ${b64({ alg: 'none' })}.${body}.`, 401);
  await rejects(`Bearer ${b64({ alg: 'none' })}.${body}.x`, 401, 'invalid_token');
  await rejects(`Bearer ${await token({}, { alg: 'HS256', key: new TextEncoder().encode('a'.repeat(32)) })}`, 401, 'invalid_token');
  const other = await generateKeyPair('RS256');
  await rejects(`Bearer ${await token({}, { key: other.privateKey })}`, 401, 'invalid_token');
});

test('unverified email, anonymous or phone sign-in, bad subject, future auth_time', async () => {
  await rejects(`Bearer ${await token({ email_verified: false })}`, 403, 'unverified');
  await rejects(`Bearer ${await token({ email: undefined })}`, 403, 'unverified');
  await rejects(`Bearer ${await token({ firebase: { sign_in_provider: 'anonymous' } })}`, 403, 'provider');
  await rejects(`Bearer ${await token({ firebase: { sign_in_provider: 'phone' } })}`, 403, 'provider');
  await rejects(`Bearer ${await token({ sub: 'x'.repeat(129) })}`, 401, 'invalid_token');
  await rejects(`Bearer ${await token({ auth_time: NOW + 600 })}`, 401, 'invalid_token');
});

test('refuses to run without a project id', async () => {
  await assert.rejects(verifyFirebaseToken(req(`Bearer ${await token()}`), { keys }), (e) => e.status === 500);
});
