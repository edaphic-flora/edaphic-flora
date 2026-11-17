// Helpers for scripts/check-links.mjs, kept apart so they can be unit tested.

// What one request tells us about a link.
//   dead:    the page is gone (404, 410) or the domain no longer exists
//   ok:      it loaded
//   unsure:  blocked, slow, or the server is having a bad day; not proof of a dead link
export function verdict({ status, error }) {
  if (status === 404 || status === 410) return 'dead';
  if (error === 'ENOTFOUND') return 'dead';
  if (status >= 200 && status < 400) return 'ok';
  return 'unsure';
}

// Markdown links: [text](https://...)
export function markdownLinks(text) {
  return [...text.matchAll(/\]\((https?:\/\/[^)\s]+)\)/g)].map((m) => m[1]);
}

// The list of links to test: { url, where } with duplicates merged.
export function collect({ states = {}, sources = {}, docs = {} }) {
  const seen = new Map();
  const add = (url, where) => {
    if (!url) return;
    const at = seen.get(url);
    if (at) at.push(where);
    else seen.set(url, [where]);
  };
  for (const [code, url] of Object.entries(states)) add(url, `state list ${code}`);
  for (const [name, url] of Object.entries(sources)) add(url, `source ${name}`);
  for (const [file, text] of Object.entries(docs)) for (const url of markdownLinks(text)) add(url, file);
  return [...seen].map(([url, where]) => ({ url, where: where.join(', ') }));
}
