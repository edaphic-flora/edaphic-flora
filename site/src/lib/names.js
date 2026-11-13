// Name helpers shared by the export script and the site.

export function slugify(name) {
  return String(name)
    .normalize('NFKD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/×/g, 'x')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
}

// plural(1, 'sample') -> "1 sample", plural(3, 'sample') -> "3 samples"
export function plural(n, word) {
  return `${n} ${word}${n === 1 ? '' : 's'}`;
}

// "little bluestem" -> "Little Bluestem"; keeps words that are already
// capitalized ("New England aster" -> "New England Aster").
export function titleCaseCommon(name) {
  if (!name) return null;
  return String(name)
    .trim()
    .split(/\s+/)
    .map((w) => (w ? w[0].toUpperCase() + w.slice(1) : w))
    .join(' ');
}

// Map a USDA growth_habit string to a short plant type label.
export function plantType(growthHabit) {
  if (!growthHabit) return 'Other';
  const first = String(growthHabit).split(',')[0].trim().toLowerCase();
  if (first.startsWith('tree')) return 'Tree';
  if (first.startsWith('shrub') || first.startsWith('subshrub')) return 'Shrub';
  if (first.startsWith('vine')) return 'Vine';
  if (first.startsWith('graminoid')) return 'Grass / Graminoid';
  if (first.startsWith('forb')) return 'Forb / Herb';
  return 'Other';
}

// Parse the USDA native_status string (e.g. "CAN, L48, N, Native") the same
// way app/R/usda.R parse_native_status_na() does.
export function parseNativeStatusNA(str) {
  if (!str) return null;
  const s = String(str);
  const hasNative = /, N(,|$)/.test(s) && s.includes('Native');
  const hasIntro = /, I(,|$)/.test(s) && s.includes('Introduced');
  if (hasNative && hasIntro) return { status: 'both', label: 'Native and introduced in North America' };
  if (hasNative) return { status: 'native', label: 'Native to North America' };
  if (hasIntro) return { status: 'introduced', label: 'Introduced to North America' };
  return null;
}
