// Where to go after sign-in: the ?next= value, only if it stays on this site.
// The URL is resolved the way the browser will resolve it and its origin
// compared, so tricks a pattern can miss (tabs, backslashes, "//host") fail.
export function safeNext(params, origin = globalThis.location?.origin) {
  const n = params.get('next') || '';
  if (!n.startsWith('/') || !origin) return null;
  let u;
  try {
    u = new URL(n, origin);
  } catch {
    return null;
  }
  return u.origin === origin ? u.pathname + u.search + u.hash : null;
}
