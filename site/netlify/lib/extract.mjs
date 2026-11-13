// Reading a soil lab report (PDF or photo) with Claude.
//
// The prompt rules come from the earlier Shiny app (app/R/pdf_extract.R). The
// response is constrained by a JSON schema (structured outputs), then
// cleaned here so nothing out of range or malformed reaches the form: the
// person still checks every value before saving.

import { SOIL_FIELDS, TEXTURE_CLASS_NAMES } from './validate.mjs';

export const EXTRACT_MODEL = 'claude-opus-5-5';
export const MAX_FILE_BYTES = 4 * 1024 * 1024; // Netlify's 6 MB request limit, after base64
export const MEDIA_TYPES = ['application/pdf', 'image/jpeg', 'image/png', 'image/webp', 'image/gif', 'text/plain'];
// RTF, TXT and CSV reports are turned into plain text in the browser
export const MAX_TEXT_CHARS = 200_000;

export const EXTRACTION_PROMPT = `You are extracting soil test data from a soil lab report. Fill in each field if it is present and use null when it is missing.

Field meanings:
- ph: soil pH
- organic_matter: organic matter as a percentage
- cec_meq: cation exchange capacity in meq/100g
- *_ppm fields: concentrations in parts per million (mg/kg)
- texture_sand, texture_silt, texture_clay: percentages
- texture_class: the report's texture class, using the closest name from the allowed list ("Silt Loam" is "Silty Loam")
- sample_date: the date the sample was collected (or received, if that is the only date), as YYYY-MM-DD
- lab_name, sample_id: the laboratory and its sample number or ID
- notes: any other relevant information, such as the extraction method (for example Mehlich-3 or Modified Morgan)
- extraction_warnings: data quality issues, or an empty list

Extraction rules:
- If a value is given as "Qualitative (numeric ppm)", such as "Medium High (50 ppm)", use the number (50)
- If ONLY a qualitative descriptor is given (such as "Medium") with no numeric ppm or %, use null for that field and add a warning naming which fields were descriptor-only; the person must look up the number on their lab's chart
- If only a texture class is given without percentages, fill in texture_class and leave the percentages null
- If a value is given as a range, use the midpoint
- Do not convert units. If a nutrient is reported in lb/acre or another unit that is not ppm, use null for that field and add a warning naming the field and its unit

Multi-sample reports:
- If the report contains MULTIPLE soil samples, extract ONLY THE FIRST sample's data
- Add a warning such as "Report contains 3 samples; only the first was read"

Add to extraction_warnings if:
- Multiple samples exist in the report
- Any values seem unusual or potentially misread
- Units are ambiguous
- Data appears incomplete`;

const nullable = (schema) => ({ anyOf: [schema, { type: 'null' }] });
const NUMBER_KEYS = [...SOIL_FIELDS.map((f) => f.key), 'texture_sand', 'texture_silt', 'texture_clay'];

export const EXTRACTION_SCHEMA = {
  type: 'object',
  properties: {
    ...Object.fromEntries(NUMBER_KEYS.map((k) => [k, nullable({ type: 'number' })])),
    texture_class: nullable({ type: 'string', enum: TEXTURE_CLASS_NAMES }),
    sample_date: nullable({ type: 'string', format: 'date' }),
    lab_name: nullable({ type: 'string' }),
    sample_id: nullable({ type: 'string' }),
    notes: nullable({ type: 'string' }),
    extraction_warnings: { type: 'array', items: { type: 'string' } },
  },
  required: [...NUMBER_KEYS, 'texture_class', 'sample_date', 'lab_name', 'sample_id', 'notes', 'extraction_warnings'],
  additionalProperties: false,
};

const clip = (s, n) => (typeof s === 'string' && s.trim() ? s.trim().slice(0, n) : null);

/**
 * Turns the model's JSON into values the contribute form can use. Anything out
 * of the form's allowed range is dropped with a warning instead of passed on.
 * @param {object} raw  parsed JSON from the model
 * @param {{today: string}} opts  YYYY-MM-DD
 */
export function cleanExtraction(raw, { today } = {}) {
  const warnings = Array.isArray(raw?.extraction_warnings)
    ? raw.extraction_warnings.map((w) => clip(w, 300)).filter(Boolean).slice(0, 10)
    : [];
  const soil = {};
  for (const f of SOIL_FIELDS) {
    const v = raw?.[f.key];
    if (typeof v !== 'number' || !Number.isFinite(v)) continue;
    if (v < f.min || v > f.max) {
      warnings.push(`${f.label} on the report (${v}) is outside the range the form accepts, so it was left blank.`);
      continue;
    }
    soil[f.key] = v;
  }

  let texture = null;
  const pct = ['texture_sand', 'texture_silt', 'texture_clay'].map((k) => raw?.[k]);
  if (pct.every((v) => typeof v === 'number' && Number.isFinite(v) && v >= 0 && v <= 100)) {
    texture = { mode: 'pct', sand: pct[0], silt: pct[1], clay: pct[2] };
  } else if (TEXTURE_CLASS_NAMES.includes(raw?.texture_class)) {
    texture = { mode: 'class', class: raw.texture_class };
  }

  let sampleDate = null;
  if (typeof raw?.sample_date === 'string' && /^\d{4}-\d{2}-\d{2}$/.test(raw.sample_date)) {
    if (raw.sample_date >= '1900-01-01' && (!today || raw.sample_date <= today)) sampleDate = raw.sample_date;
    else warnings.push(`The report date (${raw.sample_date}) looks wrong, so the date was left as is.`);
  }

  // The app added the lab and sample number to the notes; do the same
  const noteBits = [
    clip(raw?.lab_name, 120) && `Lab: ${clip(raw.lab_name, 120)}`,
    clip(raw?.sample_id, 60) && `Sample ID: ${clip(raw.sample_id, 60)}`,
    clip(raw?.notes, 600),
  ].filter(Boolean);

  return {
    soil,
    texture,
    sample_date: sampleDate,
    notes: noteBits.length ? noteBits.join('. ').slice(0, 900) : null,
    warnings,
    found: Object.keys(soil).length + (texture ? 1 : 0),
  };
}
