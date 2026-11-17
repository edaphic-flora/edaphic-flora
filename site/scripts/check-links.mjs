// Tests the outbound links the site depends on: the 50 state invasive lists,
// the named data sources, and the links in PRIVACY.md and TERMS.md.
//
//   npm run check-links
//
// Exits 1 only when a link is plainly dead (404, 410, or the domain is gone).
// Blocked, slow or erroring servers are printed as "unsure" and do not fail the
// run, since agency sites often turn scripts away. Per-species links (USDA
// profiles, photo credits) are not tested here: there are hundreds, and they
// come straight from those services' own records.
import fs from 'node:fs';
import { verdict, collect } from './lib/link-check.mjs';
import { SOURCES } from '../src/lib/sources.js';

const root = new URL('..', import.meta.url);
const read = (p) => fs.readFileSync(new URL(p, root), 'utf8');

const links = collect({
  states: JSON.parse(read('src/data/state-invasive-urls.json')),
  sources: SOURCES,
  docs: { 'PRIVACY.md': read('../PRIVACY.md'), 'TERMS.md': read('../TERMS.md') },
});

const UA = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36';

async function request(url) {
  try {
    const r = await fetch(url, {
      redirect: 'follow',
      signal: AbortSignal.timeout(30000),
      headers: { 'user-agent': UA, accept: 'text/html,application/xhtml+xml,*/*;q=0.8', 'accept-language': 'en-US,en;q=0.9' },
    });
    await r.arrayBuffer().catch(() => {});
    return { status: r.status };
  } catch (e) {
    return { status: 0, error: String(e.cause?.code || e.name) };
  }
}

async function test(link) {
  let res = await request(link.url);
  // One more try for anything that isn't a clean answer
  if (verdict(res) === 'unsure') {
    await new Promise((r) => setTimeout(r, 2000));
    res = await request(link.url);
  }
  return { ...link, ...res, verdict: verdict(res) };
}

const results = [];
let next = 0;
await Promise.all(
  Array.from({ length: 6 }, async () => {
    while (next < links.length) results.push(await test(links[next++]));
  })
);

const dead = results.filter((r) => r.verdict === 'dead');
const unsure = results.filter((r) => r.verdict === 'unsure');
const line = (r) => `  ${r.status || r.error}  ${r.where}  ${r.url}`;

console.log(`Checked ${results.length} links: ${results.length - dead.length - unsure.length} ok, ${unsure.length} unsure, ${dead.length} dead.`);
if (unsure.length) console.log(`\nUnsure (blocked, slow or server error; look by hand if it keeps happening):\n${unsure.map(line).join('\n')}`);
if (dead.length) {
  console.log(`\nDead (fix in src/data/state-invasive-urls.json, src/lib/sources.js or the document named):\n${dead.map(line).join('\n')}`);
  process.exit(1);
}
