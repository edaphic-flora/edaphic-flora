// Validation for a soil test submitted from edaphicflora.org.
//
// Pure (no Node or browser APIs): the Netlify function uses it as the real
// check, and the contribute page imports the same file so people see errors
// and warnings before they submit. Rules and messages follow the Shiny app
// (app/R/mod_data_entry.R, safe_submit_body). The database functions in
// site/sql/contribute.sql repeat the key checks as a backstop.

import { TEXTURE_CLASS_NAMES, textureClassFor, texturePercentages } from './texture.mjs';

export const MAX_PLANTS = 20;
export const MAX_NOTES = 1000;

export const OUTCOMES = ['Thriving', 'Established', 'Struggling', 'Failed/Died'];
export const SUN_EXPOSURES = ['Full Sun', 'Part Sun', 'Part Shade', 'Full Shade'];
export const HYDROLOGY = [
  { value: 'Dry', label: 'Dry/Xeric' },
  { value: 'Mesic', label: 'Mesic' },
  { value: 'Wet', label: 'Wet/Hydric' },
];
export { TEXTURE_CLASS_NAMES };

// Soil measurements, in form order. "basic" ones are always shown.
export const SOIL_FIELDS = [
  { key: 'ph', label: 'pH', min: 2, max: 14, step: 0.1, basic: true, error: 'pH must be between 2.0 and 14.0' },
  { key: 'organic_matter', label: 'Organic matter', unit: '%', min: 0, max: 50, step: 0.1, basic: true, error: 'Organic matter must be between 0% and 50%' },
  { key: 'phosphorus_ppm', label: 'Phosphorus', unit: 'ppm', basic: true },
  { key: 'potassium_ppm', label: 'Potassium', unit: 'ppm', basic: true },
  { key: 'calcium_ppm', label: 'Calcium', unit: 'ppm', basic: true },
  { key: 'magnesium_ppm', label: 'Magnesium', unit: 'ppm', basic: true },
  { key: 'cec_meq', label: 'CEC', unit: 'meq/100g', min: 0, max: 200, step: 0.1, error: 'CEC must be between 0 and 200 meq/100g' },
  { key: 'soluble_salts_ppm', label: 'Soluble salts', unit: 'ppm' },
  { key: 'nitrate_ppm', label: 'Nitrate', unit: 'ppm' },
  { key: 'ammonium_ppm', label: 'Ammonium', unit: 'ppm' },
  { key: 'sulfur_ppm', label: 'Sulfur', unit: 'ppm' },
  { key: 'iron_ppm', label: 'Iron', unit: 'ppm' },
  { key: 'manganese_ppm', label: 'Manganese', unit: 'ppm' },
  { key: 'zinc_ppm', label: 'Zinc', unit: 'ppm' },
  { key: 'copper_ppm', label: 'Copper', unit: 'ppm' },
  { key: 'boron_ppm', label: 'Boron', unit: 'ppm' },
].map((f) =>
  f.unit === 'ppm'
    ? { min: 0, max: 100000, step: 'any', error: `${f.label} must be between 0 and 100,000 ppm`, ...f }
    : f
);

const TOP_KEYS = ['lab_test_confirmed', 'soil', 'texture', 'plants', 'location', 'date', 'notes', 'confirm_duplicates'];
const PLANT_KEYS = ['species', 'cultivar', 'outcome', 'sun_exposure', 'site_hydrology', 'inat_url'];
const INAT_RE = /^https:\/\/(www\.)?inaturalist\.org\/observations\/\d{1,12}$/;
const DATE_RE = /^(\d{4})-(\d{2})-(\d{2})$/;

const isObj = (v) => v !== null && typeof v === 'object' && !Array.isArray(v);
const blank = (v) => v === null || v === undefined || v === '';
const chars = (s) => [...s].length; // code points, like R's nchar
const round2 = (x) => Math.round(x * 100) / 100;

// A number, or null for empty. Returns undefined when the value isn't a number.
function num(v) {
  if (blank(v)) return null;
  return typeof v === 'number' && Number.isFinite(v) ? v : undefined;
}

// Optional short text: trimmed, null when empty.
function text(v, max) {
  if (blank(v)) return null;
  if (typeof v !== 'string') return undefined;
  const t = v.trim();
  if (!t) return null;
  return chars(t) <= max ? t : undefined;
}

function validDate(s) {
  const m = DATE_RE.exec(s || '');
  if (!m) return false;
  const d = new Date(Date.UTC(+m[1], +m[2] - 1, +m[3]));
  return d.getUTCFullYear() === +m[1] && d.getUTCMonth() === +m[2] - 1 && d.getUTCDate() === +m[3];
}

function addDays(ymd, n) {
  const [y, m, d] = ymd.split('-').map(Number);
  return new Date(Date.UTC(y, m - 1, d + n)).toISOString().slice(0, 10);
}

/**
 * @param {unknown} body  the request JSON
 * @param {{today: string}} opts  today's date as YYYY-MM-DD
 * @returns {{ok: boolean, errors: string[], warnings: string[], value: object|null}}
 */
export function validateSubmission(body, { today } = {}) {
  const errors = [];
  const warnings = [];
  if (!isObj(body)) return { ok: false, errors: ['The request was not understood.'], warnings, value: null };

  for (const k of Object.keys(body)) {
    if (!TOP_KEYS.includes(k)) errors.push(`Unknown field: ${String(k).slice(0, 40)}`);
  }

  if (body.lab_test_confirmed !== true) {
    errors.push('Please confirm this data comes from a soil testing laboratory.');
  }

  // ---- soil values
  const soilIn = body.soil ?? {};
  const soil = {};
  if (!isObj(soilIn)) {
    errors.push('Soil values were not understood.');
  } else {
    for (const k of Object.keys(soilIn)) {
      if (!SOIL_FIELDS.some((f) => f.key === k)) errors.push(`Unknown soil field: ${String(k).slice(0, 40)}`);
    }
    for (const f of SOIL_FIELDS) {
      const v = num(soilIn[f.key]);
      if (v === undefined) {
        errors.push(`${f.label} must be a number.`);
        continue;
      }
      if (v !== null && (v < f.min || v > f.max)) errors.push(f.error);
      soil[f.key] = v;
    }
    if (soil.ph == null && soil.organic_matter == null) {
      errors.push('Please enter at least pH or organic matter percentage.');
    }
  }

  // ---- texture: none, percentages, or a class
  const tx = body.texture ?? { mode: 'none' };
  soil.texture_class = null;
  soil.texture_sand = null;
  soil.texture_silt = null;
  soil.texture_clay = null;
  if (!isObj(tx) || !['none', 'pct', 'class'].includes(tx.mode)) {
    errors.push('Soil texture was not understood.');
  } else if (tx.mode === 'pct') {
    const parts = [['sand', 'Sand'], ['silt', 'Silt'], ['clay', 'Clay']].map(([k, label]) => {
      const v = num(tx[k]);
      if (v == null) errors.push(`Enter the ${k} percentage, or choose a texture class instead.`);
      else if (v < 0 || v > 100) errors.push(`${label} percentage must be between 0 and 100`);
      return v;
    });
    if (parts.every((v) => typeof v === 'number' && v >= 0 && v <= 100)) {
      const [sand, silt, clay] = parts;
      const sum = sand + silt + clay;
      if (sum < 90 || sum > 110) {
        errors.push(`Sand + silt + clay = ${sum.toFixed(1)}%. They must add up to between 90 and 110% (allowing for lab rounding).`);
      } else {
        soil.texture_class = textureClassFor(sand, silt, clay);
        soil.texture_sand = round2(sand);
        soil.texture_silt = round2(silt);
        soil.texture_clay = round2(clay);
      }
    }
  } else if (tx.mode === 'class') {
    const p = TEXTURE_CLASS_NAMES.includes(tx.class) ? texturePercentages(tx.class) : null;
    if (!p) errors.push('Choose a texture class from the list.');
    else {
      soil.texture_class = tx.class;
      soil.texture_sand = round2(p.sand);
      soil.texture_silt = round2(p.silt);
      soil.texture_clay = round2(p.clay);
    }
  }

  // ---- plants
  const plants = [];
  if (!Array.isArray(body.plants) || body.plants.length === 0) {
    errors.push('Please add at least one plant.');
  } else if (body.plants.length > MAX_PLANTS) {
    errors.push(`Up to ${MAX_PLANTS} plants per soil test.`);
  } else {
    const seen = new Set();
    body.plants.forEach((p, i) => {
      const n = i + 1;
      if (!isObj(p)) {
        errors.push(`Plant ${n} was not understood.`);
        return;
      }
      for (const k of Object.keys(p)) {
        if (!PLANT_KEYS.includes(k)) errors.push(`Unknown plant field: ${String(k).slice(0, 40)}`);
      }
      const species = text(p.species, 255);
      if (!species) {
        errors.push(`Plant ${n}: choose a species from the list.`);
        return;
      }
      if (seen.has(species)) errors.push(`${species} is listed twice.`);
      seen.add(species);
      const cultivar = text(p.cultivar, 255);
      if (cultivar === undefined) errors.push(`${species}: cultivar must be 255 characters or fewer.`);
      const pick = (v, allowed, what) => {
        if (blank(v)) return null;
        if (!allowed.includes(v)) {
          errors.push(`${species}: choose a ${what} from the list.`);
          return null;
        }
        return v;
      };
      const inat = blank(p.inat_url) ? null : typeof p.inat_url === 'string' ? p.inat_url.trim() : '';
      if (inat !== null && !INAT_RE.test(inat)) {
        errors.push(`${species}: the iNaturalist link should look like https://www.inaturalist.org/observations/12345`);
      }
      plants.push({
        species,
        cultivar: cultivar ?? null,
        outcome: pick(p.outcome, OUTCOMES, 'outcome'),
        sun_exposure: pick(p.sun_exposure, SUN_EXPOSURES, 'sun exposure'),
        site_hydrology: pick(p.site_hydrology, HYDROLOGY.map((h) => h.value), 'site moisture'),
        inat_url: inat && INAT_RE.test(inat) ? inat : null,
      });
    });
  }

  // ---- location: zip code, or coordinates
  let location = null;
  const loc = body.location;
  if (isObj(loc) && !blank(loc.zip)) {
    const zip = String(loc.zip).trim();
    if (/^\d{5}$/.test(zip)) location = { zip };
    else errors.push('Enter a 5-digit zip code.');
  } else if (isObj(loc) && (!blank(loc.lat) || !blank(loc.lon))) {
    const lat = num(loc.lat);
    const lon = num(loc.lon);
    if (lat == null || lon == null) errors.push('Enter both latitude and longitude.');
    else if (lat < -90 || lat > 90) errors.push('Latitude must be between -90 and 90 degrees');
    else if (lon < -180 || lon > 180) errors.push('Longitude must be between -180 and 180 degrees');
    else location = { lat: Math.round(lat * 1e6) / 1e6, lon: Math.round(lon * 1e6) / 1e6 };
  } else {
    errors.push('Enter the zip code where the sample was taken, or its latitude and longitude.');
  }

  // ---- date and notes
  let date = null;
  if (!validDate(body.date)) {
    errors.push('Enter the date the sample was taken.');
  } else if (body.date < '1900-01-01') {
    errors.push('The sample date looks too early.');
  } else if (today && body.date > addDays(today, 1)) {
    errors.push("The sample date can't be in the future.");
  } else {
    date = body.date;
  }

  let notes = null;
  if (!blank(body.notes)) {
    if (typeof body.notes !== 'string') errors.push('Notes were not understood.');
    else if (chars(body.notes.trim()) > MAX_NOTES) errors.push('Notes must be 1,000 characters or fewer.');
    else notes = body.notes.trim() || null;
  }

  if (body.confirm_duplicates !== undefined && typeof body.confirm_duplicates !== 'boolean') {
    errors.push('The request was not understood.');
  }

  // ---- soft warnings (as in the app: shown, never blocking)
  if (soil.ph != null && (soil.ph < 3.5 || soil.ph > 9.5)) warnings.push('This pH value is unusually extreme.');
  if (soil.organic_matter != null && soil.organic_matter > 20) {
    warnings.push('Organic matter above 20% is uncommon outside of peat soils.');
  }
  if (soil.cec_meq != null && soil.cec_meq > 60) warnings.push('CEC above 60 is uncommon. Check that the units are meq/100g.');
  for (const f of SOIL_FIELDS) {
    if (f.unit === 'ppm' && soil[f.key] != null && soil[f.key] > 5000) {
      warnings.push(`${f.label} seems very high. Check that the units are ppm.`);
    }
  }

  const ok = errors.length === 0;
  return {
    ok,
    errors,
    warnings,
    value: ok ? { soil, plants, location, date, notes, confirm_duplicates: body.confirm_duplicates === true } : null,
  };
}
