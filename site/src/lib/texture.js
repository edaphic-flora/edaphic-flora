// USDA soil texture class from sand / silt / clay percentages.
// Uses the standard NRCS texture triangle boundaries. Inputs are normalized
// to sum to 100 first, because medians of three columns rarely add up exactly.

export function normalizeTexture(sand, silt, clay) {
  const vals = [sand, silt, clay].map(Number);
  if (vals.some((v) => !Number.isFinite(v) || v < 0)) return null;
  const total = vals[0] + vals[1] + vals[2];
  if (total <= 0) return null;
  const [s, si] = vals.map((v) => Math.round((v * 100) / total));
  return { sand: s, silt: si, clay: 100 - s - si };
}

export function classifyTexture(sand, silt, clay) {
  const n = normalizeTexture(sand, silt, clay);
  if (!n) return null;
  const { sand: sa, silt: si, clay: cl } = n;
  if (si + 1.5 * cl < 15) return 'Sand';
  if (si + 1.5 * cl >= 15 && si + 2 * cl < 30) return 'Loamy Sand';
  if ((cl >= 7 && cl < 20 && sa > 52 && si + 2 * cl >= 30) || (cl < 7 && si < 50 && si + 2 * cl >= 30)) return 'Sandy Loam';
  if (cl >= 7 && cl < 27 && si >= 28 && si < 50 && sa <= 52) return 'Loam';
  if ((si >= 50 && cl >= 12 && cl < 27) || (si >= 50 && si < 80 && cl < 12)) return 'Silt Loam';
  if (si >= 80 && cl < 12) return 'Silt';
  if (cl >= 20 && cl < 35 && si < 28 && sa > 45) return 'Sandy Clay Loam';
  if (cl >= 27 && cl < 40 && sa > 20 && sa <= 45) return 'Clay Loam';
  if (cl >= 27 && cl < 40 && sa <= 20) return 'Silty Clay Loam';
  if (cl >= 35 && sa > 45) return 'Sandy Clay';
  if (cl >= 40 && si >= 40) return 'Silty Clay';
  if (cl >= 40 && sa <= 45 && si < 40) return 'Clay';
  return null;
}
