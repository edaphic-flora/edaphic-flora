// Firebase sign-in, used only by /account/ and /contribute/ (no other page
// loads Firebase). Same Firebase project as the Shiny app, so existing
// accounts work. Config comes from PUBLIC_FIREBASE_* environment variables
// (public by design; set in Netlify).

import { initializeApp } from 'firebase/app';
import {
  getAuth,
  onAuthStateChanged,
  GoogleAuthProvider,
  signInWithPopup,
  signInWithEmailAndPassword,
  createUserWithEmailAndPassword,
  sendEmailVerification,
  sendPasswordResetEmail,
  signOut as fbSignOut,
} from 'firebase/auth';

const config = {
  apiKey: import.meta.env.PUBLIC_FIREBASE_API_KEY,
  authDomain: import.meta.env.PUBLIC_FIREBASE_AUTH_DOMAIN,
  projectId: import.meta.env.PUBLIC_FIREBASE_PROJECT_ID,
};

export const signInConfigured = Boolean(config.apiKey && config.authDomain && config.projectId);

let auth = null;
function getA() {
  if (!auth) auth = getAuth(initializeApp(config));
  return auth;
}

const backTo = () => ({ url: `${location.origin}/account/` });

export function watchUser(cb) {
  if (!signInConfigured) {
    cb(null);
    return () => {};
  }
  return onAuthStateChanged(getA(), cb);
}

// Email-and-password accounts must verify their email before adding data
export function needsVerification(user) {
  return !!user && !user.emailVerified && user.providerData.some((p) => p.providerId === 'password');
}

// Call directly inside a click handler, or Safari blocks the popup
export function signInWithGoogle() {
  const provider = new GoogleAuthProvider();
  provider.setCustomParameters({ prompt: 'select_account' });
  return signInWithPopup(getA(), provider);
}

export const signInWithEmail = (email, password) => signInWithEmailAndPassword(getA(), email, password);

export async function createAccount(email, password) {
  const cred = await createUserWithEmailAndPassword(getA(), email, password);
  await sendEmailVerification(cred.user, backTo());
  return cred;
}

export const resendVerification = () => sendEmailVerification(getA().currentUser, backTo());
export const resetPassword = (email) => sendPasswordResetEmail(getA(), email, backTo());
export const signOut = () => fbSignOut(getA());

// After clicking the email link in another tab: pick up the verified status
export async function refreshUser() {
  const u = getA().currentUser;
  if (!u) return null;
  await u.reload();
  await u.getIdToken(true);
  return getA().currentUser;
}

// Calls the site API with the current sign-in token
export async function api(path, { method = 'GET', body } = {}) {
  const u = getA().currentUser;
  if (!u) return { ok: false, status: 401, data: { error: 'Please sign in.' } };
  const token = await u.getIdToken();
  let res;
  try {
    res = await fetch(path, {
      method,
      headers: { Authorization: `Bearer ${token}`, ...(body ? { 'Content-Type': 'application/json' } : {}) },
      body: body ? JSON.stringify(body) : undefined,
    });
  } catch {
    return { ok: false, status: 0, data: { error: 'Could not reach the server. Check your connection and try again.' } };
  }
  const data = res.status === 204 ? null : await res.json().catch(() => null);
  return { ok: res.ok, status: res.status, data };
}

// Plain-language messages for Firebase sign-in errors (null = say nothing)
export function authMessage(e) {
  const code = e?.code || '';
  if (code === 'auth/popup-closed-by-user' || code === 'auth/cancelled-popup-request') return null;
  if (['auth/invalid-credential', 'auth/wrong-password', 'auth/user-not-found', 'auth/invalid-login-credentials'].includes(code)) {
    return "That email and password don't match. Try again, or reset your password.";
  }
  if (code === 'auth/email-already-in-use') return 'There is already an account with that email. Sign in instead.';
  if (code === 'auth/weak-password') return 'Use a longer password (at least 8 characters).';
  if (code === 'auth/invalid-email') return 'That email address does not look right.';
  if (code === 'auth/popup-blocked') return 'Your browser blocked the sign-in window. Allow pop-ups for this site and try again.';
  if (code === 'auth/too-many-requests') return 'Too many attempts. Wait a few minutes and try again.';
  if (code === 'auth/network-request-failed') return 'Could not reach the sign-in service. Check your connection.';
  if (code === 'auth/unauthorized-domain') return "Sign-in isn't set up for this web address yet.";
  if (code === 'auth/account-exists-with-different-credential') {
    return 'That email already has an account with a different sign-in method. Try the other method.';
  }
  return 'Sign-in did not work. Please try again.';
}

// ?next= only to a path on this site (one leading slash, no backslashes)
export function safeNext(params) {
  const n = params.get('next') || '';
  return /^\/(?![/\\])[^\\\r\n]*$/.test(n) ? n : null;
}
