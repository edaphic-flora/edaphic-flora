// Compact index used by the Browse page filters (fetched in the browser).
import { browseOrder } from '../lib/data.js';

export function GET() {
  const rows = browseOrder.map((s) => ({
    s: s.slug,
    c: s.common || null,
    a: s.common_name && s.common_name.toLowerCase() !== (s.common || '').toLowerCase() ? s.common_name : undefined,
    n: s.scientific_name,
    t: s.type,
    k: s.kind,
    nat: s.nativeCodes.join(' '),
    int: s.introducedCodes.join(' '),
    inv: s.invasiveCodes.join(' ') + (s.federalInvasive ? ' US' : ''),
    sc: s.sampleCount,
    ok: s.soilReady ? 1 : 0,
    sw: s.swatchCss,
    h: s.photoHint,
  }));
  return new Response(JSON.stringify(rows), { headers: { 'Content-Type': 'application/json' } });
}
