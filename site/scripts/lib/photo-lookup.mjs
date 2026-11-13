// Finds one openly licensed photo for a plant species.
//
// 1. iNaturalist: exact match on the scientific name (accepted name or a
//    synonym), plants only. Uses the taxon's default photo if its license is
//    allowed, otherwise the first allowed photo in the taxon's photo set.
// 2. Wikimedia Commons: the lead image of the species' English Wikipedia
//    article, if its Commons license is allowed.
//
// Images are hotlinked from iNaturalist's open data bucket and Wikimedia, so
// they never count against Netlify bandwidth.
//
// No dependencies; works in Node 22+ and in a browser (for testing).

export const DEFAULT_LICENSES = ['cc0', 'pd', 'cc-by', 'cc-by-sa', 'cc-by-nc', 'cc-by-nc-sa'];

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const lc = (s) => String(s || '').trim().toLowerCase();

// "schizachyrium Scoparium" -> "Schizachyrium scoparium"
export function normalizeName(name) {
  const parts = String(name || '').trim().split(/\s+/);
  if (parts.length < 2) return null;
  const genus = parts[0][0].toUpperCase() + parts[0].slice(1).toLowerCase();
  return `${genus} ${parts[1].toLowerCase()}`;
}

function stripHtml(s) {
  return String(s || '')
    .replace(/<[^>]*>/g, ' ')
    .replace(/&amp;/g, '&')
    .replace(/&quot;/g, '"')
    .replace(/&#0?39;|&apos;/g, "'")
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&nbsp;/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

// Commons LicenseShortName -> our license code (null = not allowed at all)
export function commonsLicenseCode(shortName) {
  const s = String(shortName || '').trim();
  if (/^cc0\b/i.test(s)) return 'cc0';
  if (/^(public domain|pd\b|pd-)/i.test(s)) return 'pd';
  if (/^cc[ -]by[ -]sa\b/i.test(s)) return 'cc-by-sa';
  if (/^cc[ -]by\b(?![ -]n)/i.test(s)) return 'cc-by';
  return null; // GFDL-only, non-commercial or unknown: skip
}

const PLANT_RANKS = ['species', 'hybrid', 'subspecies', 'variety', 'form'];

export function makePhotoLookup({
  fetchImpl = globalThis.fetch,
  userAgent = null,
  licenses = DEFAULT_LICENSES,
  delayMs = 1100,
} = {}) {
  let last = 0;
  const allowed = new Set(licenses);

  async function getJson(url) {
    for (let attempt = 0; attempt < 4; attempt++) {
      const wait = last + delayMs - Date.now();
      if (wait > 0) await sleep(wait);
      last = Date.now();
      const headers = { Accept: 'application/json' };
      if (userAgent) headers['User-Agent'] = userAgent;
      let res;
      try {
        res = await fetchImpl(url, { headers });
      } catch (err) {
        if (attempt === 3) throw err;
        await sleep(3000 * (attempt + 1));
        continue;
      }
      if (res.status === 429 || res.status >= 500) {
        await sleep(10000 * (attempt + 1));
        continue;
      }
      if (res.status === 404) return null;
      if (!res.ok) throw new Error(`HTTP ${res.status} for ${url}`);
      return res.json();
    }
    throw new Error(`Gave up after retries: ${url}`);
  }

  const okInat = (p) => p && allowed.has(p.license_code) && !(p.flags && p.flags.length) && (p.medium_url || p.url);

  function inatRecord(taxon, p) {
    const medium = p.medium_url || String(p.url).replace(/\/square\./, '/medium.');
    const dims = p.original_dimensions || {};
    return {
      source: 'inaturalist',
      photo_id: String(p.id),
      page_url: `https://www.inaturalist.org/photos/${p.id}`,
      image_url: medium,
      large_url: medium.replace(/\/medium\./, '/large.'),
      width: dims.width || null,
      height: dims.height || null,
      license: p.license_code,
      creator: p.attribution_name || stripHtml(p.attribution).replace(/^\(c\)\s*/i, '').split(',')[0] || null,
      attribution: stripHtml(p.attribution) || null,
      inat_taxon_id: taxon.id,
      matched_name: taxon.name,
    };
  }

  async function fromINat(name) {
    const want = lc(name);
    const j = await getJson(
      `https://api.inaturalist.org/v1/taxa?q=${encodeURIComponent(name)}&is_active=true&per_page=10`
    );
    const hits = ((j && j.results) || []).filter(
      (t) =>
        t.iconic_taxon_name === 'Plantae' &&
        PLANT_RANKS.includes(t.rank) &&
        (lc(t.matched_term) === want || lc(t.name) === want)
    );
    hits.sort((a, b) => (a.rank === 'species' ? 0 : 1) - (b.rank === 'species' ? 0 : 1));
    const taxon = hits[0];
    if (!taxon) return { taxon: null, record: null };
    if (okInat(taxon.default_photo)) return { taxon, record: inatRecord(taxon, taxon.default_photo) };

    const full = await getJson(`https://api.inaturalist.org/v1/taxa/${taxon.id}`);
    const photos = ((full && full.results && full.results[0] && full.results[0].taxon_photos) || []).map((tp) => tp.photo);
    const p = photos.find(okInat);
    return { taxon, record: p ? inatRecord(taxon, p) : null };
  }

  async function fromCommons(name, inatTaxon) {
    // Prefer the Wikipedia article iNaturalist links to (handles synonyms), else the name itself.
    let title = name;
    const wu = inatTaxon && inatTaxon.wikipedia_url;
    if (wu && /wikipedia\.org\/wiki\//.test(wu)) title = decodeURIComponent(wu.split('/wiki/')[1]).replace(/_/g, ' ');

    const w = await getJson(
      'https://en.wikipedia.org/w/api.php?action=query&format=json&formatversion=2&prop=pageimages&piprop=name&redirects=1&origin=*&titles=' +
        encodeURIComponent(title)
    );
    const page = w && w.query && w.query.pages && w.query.pages[0];
    if (!page || page.missing || !page.pageimage) return null;
    // A species title that redirects to a genus or list page would show the wrong plant.
    const finalTitle = page.title || title;
    if (finalTitle.trim().split(/\s+/).length < 2 || /^list of/i.test(finalTitle)) return null;
    const file = page.pageimage;
    if (/\.(svg|gif|tiff?|pdf|djvu)$/i.test(file)) return null;
    if (/(map|range|distribution|herbarium|specimen|illustration|drawing|plate|botanical)/i.test(file)) return null;

    const c = await getJson(
      'https://commons.wikimedia.org/w/api.php?action=query&format=json&formatversion=2&prop=imageinfo&iiprop=url|extmetadata|size|mime&iiurlwidth=1024&origin=*&titles=' +
        encodeURIComponent('File:' + file)
    );
    const ii = c && c.query && c.query.pages && c.query.pages[0] && c.query.pages[0].imageinfo && c.query.pages[0].imageinfo[0];
    if (!ii || !/^image\/(jpeg|png|webp)$/.test(ii.mime || '')) return null;
    const meta = ii.extmetadata || {};
    const code = commonsLicenseCode(meta.LicenseShortName && meta.LicenseShortName.value);
    if (!code || !allowed.has(code)) return null;

    const clean = (u) => String(u || '').split('?')[0];
    const large = clean(ii.thumburl || ii.url);
    const medium = /\/\d+px-/.test(large) && ii.width > 500 ? large.replace(/\/\d+px-/, '/500px-') : large;
    const creator = stripHtml(meta.Artist && meta.Artist.value).slice(0, 120) || null;
    const licName = (meta.LicenseShortName && meta.LicenseShortName.value) || code;
    return {
      source: 'wikimedia',
      photo_id: file,
      page_url: ii.descriptionurl || `https://commons.wikimedia.org/wiki/File:${encodeURIComponent(file)}`,
      image_url: medium,
      large_url: large,
      width: ii.width || null,
      height: ii.height || null,
      license: code,
      creator,
      attribution: `${creator || 'Unknown author'}, ${licName}, via Wikimedia Commons`,
      inat_taxon_id: inatTaxon ? inatTaxon.id : null,
      matched_name: finalTitle,
    };
  }

  // Returns a record. source is 'none' when nothing usable was found.
  async function lookup(rawName) {
    const name = normalizeName(rawName);
    if (!name) return { source: 'none', matched_name: null, inat_taxon_id: null };
    const { taxon, record } = await fromINat(name);
    if (record) return record;
    const commons = await fromCommons(name, taxon);
    if (commons) return commons;
    return { source: 'none', matched_name: taxon ? taxon.name : null, inat_taxon_id: taxon ? taxon.id : null };
  }

  return { lookup, fromINat, fromCommons };
}
