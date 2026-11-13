// Public lookups for the contribute form. Read-only (site_reader), reference
// data only, cached at Netlify's edge so repeat lookups don't run the function.
//
//   GET /api/species?q=butterfly  -> { results: [{ name, common }] }   (up to 20)
//   GET /api/zip/06511            -> { zip, city, state, lat, lon, ecoregion }

import { withClient, json } from '../lib/db.mjs';

const CACHE = {
  'Cache-Control': 'public, max-age=3600',
  'Netlify-CDN-Cache-Control': 'public, max-age=86400, stale-while-revalidate=604800',
};

// Ranked: scientific-name prefix (with or without a hybrid ×), then common
// names containing every word, then scientific names containing every word.
const SPECIES_SQL = `
  SELECT taxon_name, common_name, min(r) AS r, min(l) AS l
  FROM (
    (SELECT taxon_name, common_name, 0 AS r, length(taxon_name) AS l
       FROM public_site.species_names
      WHERE lower(taxon_name) LIKE $1 OR lower(taxon_name) LIKE $2
      ORDER BY taxon_name LIMIT 20)
    UNION ALL
    (SELECT taxon_name, common_name, 1, length(common_name)
       FROM public_site.species_names
      WHERE common_name IS NOT NULL AND lower(common_name) LIKE ALL ($3::text[])
      ORDER BY length(common_name), common_name LIMIT 20)
    UNION ALL
    (SELECT taxon_name, common_name, 2, length(taxon_name)
       FROM public_site.species_names
      WHERE $4::boolean AND lower(taxon_name) LIKE ALL ($3::text[])
      ORDER BY length(taxon_name), taxon_name LIMIT 20)
  ) x
  GROUP BY taxon_name, common_name
  ORDER BY min(r), min(l), taxon_name
  LIMIT 20`;

const likeEscape = (s) => s.replace(/[\\%_]/g, (m) => `\\${m}`);

export default async (req) => {
  if (req.method !== 'GET') return json({ error: 'Method not allowed' }, 405, { Allow: 'GET' });
  const url = new URL(req.url);

  try {
    if (url.pathname.startsWith('/api/zip/')) {
      const zip = url.pathname.slice('/api/zip/'.length).replace(/\/$/, '');
      if (!/^\d{5}$/.test(zip)) return json({ error: 'Enter a 5-digit zip code.' }, 400);
      const { rows } = await withClient(process.env.DATABASE_URL, (c) =>
        c.query(
          'SELECT city, state, latitude, longitude, ecoregion_name FROM public_site.zip_lookup WHERE zipcode = $1',
          [zip]
        )
      );
      if (!rows.length) return json({ error: 'That zip code was not found.' }, 404, CACHE);
      const z = rows[0];
      return json(
        { zip, city: z.city, state: z.state, lat: Number(z.latitude), lon: Number(z.longitude), ecoregion: z.ecoregion_name },
        200,
        CACHE
      );
    }

    // Species search: scientific-name prefix first, then common-name matches
    const q = (url.searchParams.get('q') || '').trim().toLowerCase().replace(/\s+/g, ' ');
    const headers = { ...CACHE, 'Netlify-Vary': 'query=q' };
    if (q.length < 2 || q.length > 60) return json({ results: [] }, 200, headers);
    // "iris x germ" or "iris germ" should both find "Iris × germanica"
    const words = q.split(' ').filter((w) => !['x', '×'].includes(w));
    const plain = words.join(' ');
    if (plain.length < 2) return json({ results: [] }, 200, headers);
    const hybrid = words.length > 1 ? `${words[0]} × ${words.slice(1).join(' ')}` : `× ${plain}`;
    const { rows } = await withClient(process.env.DATABASE_URL, (c) =>
      c.query(SPECIES_SQL, [
        `${likeEscape(plain)}%`,
        `${likeEscape(hybrid)}%`,
        words.map((w) => `%${likeEscape(w)}%`),
        q.length >= 4,
      ])
    );
    return json({ results: rows.map((r) => ({ name: r.taxon_name, common: r.common_name })) }, 200, headers);
  } catch (e) {
    console.error('lookup failed:', e.message);
    return json({ error: 'Lookup is unavailable right now. Please try again in a minute.' }, 503);
  }
};

export const config = {
  path: ['/api/species', '/api/zip/:zip'],
  rateLimit: { windowLimit: 60, windowSize: 60, aggregateBy: ['ip', 'domain'] },
};
