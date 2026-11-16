// The site owner's own iNaturalist photos come first: when the owner has
// observed a species, that photo is used instead of a looked-up one.
//
// The owner's photos need no open license (they are the owner's to use), so
// photos with no Creative Commons license are stored with license 'arr'
// (all rights reserved) and credited the same way as every other photo.
//
// No dependencies; works in Node 22+.

export const OWNER_LICENSE = 'arr';

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const SPECIES_RANKS = ['species', 'hybrid', 'subspecies', 'variety', 'form'];
// Community-confirmed first, then the owner's own garden plants, then unconfirmed
const QUALITY_ORDER = { research: 0, casual: 1, needs_id: 2 };

// "Phlox subulata subsp. brittonii" -> "phlox subulata" (same as public_site.species_key)
export function speciesKeyOf(name) {
  const parts = String(name || '')
    .replace(/×/g, ' ')
    .trim()
    .split(/\s+/)
    .filter((p) => p.toLowerCase() !== 'x');
  return parts.length >= 2 ? `${parts[0]} ${parts[1]}`.toLowerCase() : null;
}

function record(obs, photo, ownerName) {
  const medium = String(photo.url).replace(/\/square\./, '/medium.');
  const dims = photo.original_dimensions || {};
  const creator = ownerName || (obs.user && (obs.user.name || obs.user.login)) || null;
  return {
    source: 'inaturalist',
    photo_id: String(photo.id),
    page_url: obs.uri || `https://www.inaturalist.org/observations/${obs.id}`,
    image_url: medium,
    large_url: medium.replace(/\/medium\./, '/large.'),
    width: dims.width || null,
    height: dims.height || null,
    license: photo.license_code || OWNER_LICENSE,
    creator,
    attribution: photo.attribution || (creator ? `(c) ${creator}` : null),
    inat_taxon_id: obs.taxon.id || null,
    matched_name: obs.taxon.name,
  };
}

/**
 * One photo per species from the owner's observations.
 * @returns {Map<string, {record: object, photoIds: Set<string>}>} species key ->
 *   the chosen photo, plus every photo id the owner has for that species (so a
 *   photo already in use is not swapped for another of the owner's)
 */
export function pickOwnerPhotos(observations, { ownerName = null } = {}) {
  const bySpecies = new Map();
  for (const obs of observations || []) {
    const t = obs && obs.taxon;
    if (!t || !SPECIES_RANKS.includes(t.rank) || t.iconic_taxon_name !== 'Plantae') continue;
    const photos = (obs.photos || []).filter((p) => p && p.url && /^https:\/\//.test(p.url) && !(p.flags && p.flags.length));
    if (!photos.length) continue;
    const key = speciesKeyOf(t.name);
    if (!key) continue;
    if (!bySpecies.has(key)) bySpecies.set(key, []);
    bySpecies.get(key).push({ obs, photos });
  }
  const out = new Map();
  for (const [key, list] of bySpecies) {
    list.sort(
      (a, b) =>
        (QUALITY_ORDER[a.obs.quality_grade] ?? 3) - (QUALITY_ORDER[b.obs.quality_grade] ?? 3) ||
        (b.obs.faves_count || 0) - (a.obs.faves_count || 0) ||
        String(b.obs.observed_on || '').localeCompare(String(a.obs.observed_on || ''))
    );
    out.set(key, {
      record: record(list[0].obs, list[0].photos[0], ownerName),
      photoIds: new Set(list.flatMap((x) => x.photos.map((p) => String(p.id)))),
    });
  }
  return out;
}

// Every plant observation with photos for one iNaturalist login (public API)
export async function fetchOwnerObservations({ login, fetchImpl = globalThis.fetch, userAgent = null, delayMs = 1100, max = 5000 }) {
  const all = [];
  let idAbove = 0;
  while (all.length < max) {
    const url =
      `https://api.inaturalist.org/v1/observations?user_login=${encodeURIComponent(login)}` +
      `&iconic_taxa=Plantae&photos=true&per_page=200&order=asc&order_by=id&id_above=${idAbove}`;
    const headers = { Accept: 'application/json' };
    if (userAgent) headers['User-Agent'] = userAgent;
    const res = await fetchImpl(url, { headers });
    if (!res.ok) throw new Error(`HTTP ${res.status} from iNaturalist for ${login}`);
    const j = await res.json();
    const results = (j && j.results) || [];
    if (!results.length) break;
    all.push(...results);
    idAbove = results[results.length - 1].id;
    if (all.length >= (j.total_results || 0)) break;
    await sleep(delayMs);
  }
  return all;
}
