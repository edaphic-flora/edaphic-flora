// Photo credit helpers. Every photo shown on the site is credited to its
// creator with its license, as Creative Commons licenses require.

const LICENSES = {
  cc0: { label: 'CC0', url: 'https://creativecommons.org/publicdomain/zero/1.0/' },
  pd: { label: 'public domain', url: null },
  'cc-by': { label: 'CC BY', url: 'https://creativecommons.org/licenses/by/4.0/' },
  'cc-by-sa': { label: 'CC BY-SA', url: 'https://creativecommons.org/licenses/by-sa/4.0/' },
  'cc-by-nc': { label: 'CC BY-NC', url: 'https://creativecommons.org/licenses/by-nc/4.0/' },
  'cc-by-nc-sa': { label: 'CC BY-NC-SA', url: 'https://creativecommons.org/licenses/by-nc-sa/4.0/' },
  // The site owner's own photos (scripts/lib/owner-photos.mjs, scripts/add-own-photo.mjs)
  arr: { label: 'all rights reserved', url: null },
};

const VIA = { inaturalist: 'iNaturalist', wikimedia: 'Wikimedia Commons', manual: null };

// Creator names arrive as free text: collapse stray whitespace and drop a
// wiki signature tail such as "Name -- User 07:58, 29 June 2007 (UTC)".
export function cleanCreator(name) {
  return String(name || '')
    .replace(/\s+/g, ' ')
    .replace(/\s+--\s+.*\(UTC\)$/, '')
    .trim();
}

export function photoCredit(photo) {
  if (!photo) return null;
  const lic = LICENSES[photo.license] || { label: photo.license || 'license unknown', url: null };
  const creator = cleanCreator(photo.creator) || 'Unknown photographer';
  const via = VIA[photo.source] ?? null;
  const text = `Photo: ${creator}, ${lic.label}${via ? `, via ${via}` : ''}`;
  return { creator, licenseLabel: lic.label, licenseUrl: lic.url, via, page: photo.page || null, text };
}

// Aspect ratio for width/height attributes (avoids layout shift while loading)
export function photoSize(photo, width) {
  const ratio = photo && photo.w && photo.h ? photo.h / photo.w : 0.75;
  return { width, height: Math.round(width * ratio) };
}
