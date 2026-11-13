// Soil texture for contributed samples, matching the Shiny app so stored
// classes use the app's names ("Silty Loam", "Heavy Clay").
//
// Ports of soil_texture_classes (app/R/data.R), classify_texture and
// get_texture_percentages (app/R/helpers.R). Tested against output of the R
// code in site/test/fixtures/.
//
// The app's table leaves about a third of sand/silt/clay combinations
// "Unclassified". For those, fall back to the standard NRCS triangle the
// public pages already use (src/lib/texture.js), with the app's class names.

import { classifyTexture as nrcsTriangle } from '../../src/lib/texture.js';

export const TEXTURE_TOLERANCE = 0.1;

// name, clay min/max, silt min/max, sand min/max (percent)
export const TEXTURE_CLASSES = [
  ['Heavy Clay', 60, 100, 0, 40, 0, 45],
  ['Silty Clay', 40, 60, 40, 60, 0, 20],
  ['Clay', 40, 60, 40, 60, 0, 45],
  ['Silty Clay Loam', 27, 40, 40, 60, 0, 20],
  ['Clay Loam', 27, 40, 15, 52, 20, 45],
  ['Silt', 0, 12, 88, 100, 0, 20],
  ['Silty Loam', 0, 27, 74, 88, 0, 50],
  ['Sandy Clay', 35, 55, 0, 20, 45, 65],
  ['Loam', 7, 27, 28, 50, 23, 52],
  ['Sandy Clay Loam', 20, 35, 20, 45, 45, 80],
  ['Sandy Loam', 0, 20, 0, 50, 50, 70],
  ['Loamy Sand', 0, 15, 0, 30, 70, 86],
  ['Sand', 0, 10, 0, 14, 86, 100],
].map(([name, clayMin, clayMax, siltMin, siltMax, sandMin, sandMax]) => ({
  name, clayMin, clayMax, siltMin, siltMax, sandMin, sandMax,
}));

export const TEXTURE_CLASS_NAMES = TEXTURE_CLASSES.map((c) => c.name);

const NRCS_TO_APP = { 'Silt Loam': 'Silty Loam' };

// classify_texture: first matching row of the app's table.
export function classifyTextureApp(sand, silt, clay) {
  if (Math.abs(sand + silt + clay - 100) > TEXTURE_TOLERANCE) return null;
  const t = TEXTURE_TOLERANCE;
  for (const c of TEXTURE_CLASSES) {
    if (
      clay >= c.clayMin - t && clay <= c.clayMax + t &&
      silt >= c.siltMin - t && silt <= c.siltMax + t &&
      sand >= c.sandMin - t && sand <= c.sandMax + t
    ) return c.name;
  }
  return 'Unclassified';
}

// Class for lab percentages that may not add up to exactly 100 (the form
// accepts 90 to 110 for lab rounding): scale to 100, use the app's table,
// and fall back to the NRCS triangle where the table has a gap.
export function textureClassFor(sand, silt, clay) {
  const total = sand + silt + clay;
  if (!(total > 0)) return null;
  const k = 100 / total;
  const app = classifyTextureApp(sand * k, silt * k, clay * k);
  if (app && app !== 'Unclassified') return app;
  const n = nrcsTriangle(sand, silt, clay);
  return n ? NRCS_TO_APP[n] || n : 'Unclassified';
}

// get_texture_percentages: class midpoints scaled to sum to 100.
export function texturePercentages(name) {
  const c = TEXTURE_CLASSES.find((x) => x.name === name);
  if (!c) return null;
  const sand = (c.sandMin + c.sandMax) / 2;
  const silt = (c.siltMin + c.siltMax) / 2;
  const clay = (c.clayMin + c.clayMax) / 2;
  const k = 100 / (sand + silt + clay);
  return { sand: sand * k, silt: silt * k, clay: clay * k };
}
