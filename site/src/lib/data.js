// Loads the exported species data and builds the view models the pages use.
// Runs at build time only (Node), never in the browser.

import fs from 'node:fs';
import path from 'node:path';
import curated from '../data/curated.json';
import { STATES, STATE_NAME } from '../data/states.js';
import invasiveUrls from '../data/state-invasive-urls.json';
import { titleCaseCommon, plantType } from './names.js';
import { photoCredit } from './photos.js';

const GENERATED = path.join(process.cwd(), 'src/data/generated/species.json');
const SAMPLE = path.join(process.cwd(), 'src/data/sample/species.json');

function load() {
  const file = fs.existsSync(GENERATED) ? GENERATED : SAMPLE;
  return JSON.parse(fs.readFileSync(file, 'utf8'));
}

const raw = load();
export const meta = raw.meta;
export const isSample = meta.source !== 'database';
export const thresholds = { min_samples: 1, min_contributors: 1, min_range_samples: 3, ...meta.thresholds };

// Muted, earthy swatches for cards until real macro photos exist.
const SWATCHES = [
  ['#B79B6A', '#8A7B5C'], ['#A8A06A', '#8A8C5C'], ['#A88E5E', '#8A7348'], ['#8FA07A', '#73855E'],
  ['#B093A0', '#957B88'], ['#C77E63', '#A86149'], ['#D3A35A', '#C9824E'], ['#9AA58A', '#7C876C'],
];
function hash(s) {
  let h = 0;
  for (const c of s) h = (h * 31 + c.charCodeAt(0)) >>> 0;
  return h;
}

export const STATUS_STYLE = {
  native: { label: 'Native', bg: '#93AF9C', fg: '#373D3C' },
  both: { label: 'Native + introduced', bg: '#9DAF92', fg: '#373D3C' },
  introduced: { label: 'Introduced', bg: '#6B5D49', fg: '#F7F4E8' },
  invasive: { label: 'Invasive', bg: '#D39B35', fg: '#373D3C' },
};

function countLabel(codes) {
  const hasDC = codes.includes('DC');
  const n = codes.filter((c) => c !== 'DC').length;
  const states = `${n} ${n === 1 ? 'state' : 'states'}`;
  return hasDC ? (n ? `${states} and DC` : 'DC') : states;
}

function describe(sp) {
  const bits = [];
  const habit = sp.type === 'Other' ? 'plant' : sp.type.split(' / ')[0].toLowerCase();
  const dur = sp.usda?.duration ? sp.usda.duration.split(',')[0].trim().toLowerCase() : null;
  const lead = [dur, habit].filter(Boolean).join(' ');
  if (sp.nativeCodes.length) bits.push(`A ${lead} native to ${countLabel(sp.nativeCodes)} in the US.`);
  else if (sp.introducedCodes.length) bits.push(`A ${lead} introduced to ${countLabel(sp.introducedCodes)} in the US.`);
  else bits.push(`A ${lead} in the ${sp.genus} genus.`);
  if (sp.invasiveCodes.length) bits.push(`Listed as invasive or noxious in ${countLabel(sp.invasiveCodes)}.`);
  const n = sp.soil?.sample_count || 0;
  if (n) bits.push(`${n} community soil ${n === 1 ? 'sample' : 'samples'} so far.`);
  return bits.join(' ');
}

function enrich(r) {
  const c = curated.species?.[r.slug] || {};
  const states = r.states || {};
  const nativeCodes = Object.keys(states).filter((k) => states[k] === 'native' || states[k] === 'both');
  const introducedCodes = Object.keys(states).filter((k) => states[k] === 'introduced');
  const invasiveCodes = [...new Set((r.invasive || []).map((i) => i.state).filter((s) => s && s !== 'US'))];
  const federalInvasive = (r.invasive || []).some((i) => i.state === 'US');
  const type = plantType(r.usda?.growth_habit);
  const common = c.common_name || titleCaseCommon(r.common_name);
  const kind =
    invasiveCodes.length || federalInvasive ? 'invasive' : nativeCodes.length ? 'native' : introducedCodes.length ? 'introduced' : null;
  // Any public soil data at all (every sample counts; ranges come later)
  const soilReady = !!r.soil?.meets_threshold && (r.soil?.sample_count || 0) > 0;
  const swatch = c.swatch || SWATCHES[hash(r.slug) % SWATCHES.length];
  const sp = {
    ...r,
    common,
    displayName: common || r.scientific_name,
    type,
    kind,
    nativeCodes,
    introducedCodes,
    invasiveCodes,
    federalInvasive,
    soilReady,
    sampleCount: r.soil?.sample_count || 0,
    swatch,
    swatchCss: `linear-gradient(135deg, ${swatch[0]}, ${swatch[1]})`,
    photoCredit: photoCredit(r.photo),
    url: `/species/${r.slug}/`,
  };
  sp.blurb = c.blurb || null;
  sp.summary = describe(sp);
  return sp;
}

export const species = raw.species.map(enrich);
const bySlug = new Map(species.map((s) => [s.slug, s]));

export function getSpecies(slug) {
  return bySlug.get(slug);
}

export function featured(n = 4) {
  const picks = (curated.featured || []).map((s) => bySlug.get(s)).filter(Boolean);
  const rest = [...species].sort((a, b) => b.sampleCount - a.sampleCount).filter((s) => !picks.includes(s));
  return [...picks, ...rest].slice(0, n);
}

// Browse order: species with a soil snapshot first, then most samples, then name.
export const browseOrder = [...species].sort(
  (a, b) => Number(b.soilReady) - Number(a.soilReady) || b.sampleCount - a.sampleCount || a.displayName.localeCompare(b.displayName)
);

export function related(sp, n = 4) {
  const sameGenus = species.filter((s) => s !== sp && s.genus === sp.genus);
  const sameType = species
    .filter((s) => s !== sp && s.genus !== sp.genus && s.type === sp.type && s.kind === sp.kind)
    .sort((a, b) => b.sampleCount - a.sampleCount);
  return [...sameGenus, ...sameType].slice(0, n);
}

export const plantTypes = [...new Set(species.map((s) => s.type))].sort();

export const totals = {
  species: species.length,
  samples: meta.public_sample_count ?? species.reduce((t, s) => t + s.sampleCount, 0),
  photos: species.filter((s) => s.photo).length,
  soilReady: species.filter((s) => s.soilReady).length,
};

export function stateStatus(sp, code) {
  if (sp.invasiveCodes.includes(code)) return 'invasive';
  return sp.states?.[code] || null;
}

export function invasiveLink(code) {
  return invasiveUrls[code] || null;
}

export { STATES, STATE_NAME, countLabel };
