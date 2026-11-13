// RTF to plain text, for soil reports saved as .rtf (for example the
// Connecticut Agricultural Experiment Station's Morgan reports). Runs in the
// browser before upload. Keeps line breaks, tabs and table rows (\cell -> tab,
// \row -> new line) so the lab's tables stay readable; skips fonts, colors,
// pictures and other non-text parts. No dependencies.

// Destinations that hold no readable text
const SKIP = new Set([
  'fonttbl', 'colortbl', 'stylesheet', 'info', 'pict', 'object', 'objdata', 'themedata', 'colorschememapping',
  'latentstyles', 'datastore', 'xmlnstbl', 'listtable', 'listoverridetable', 'rsidtbl', 'generator', 'filetbl',
  'revtbl', 'pgdsctbl', 'fldinst', 'shppict', 'nonshppict', 'bkmkstart', 'bkmkend', 'xmlopen', 'mmathPr',
]);
const BREAK = new Set(['par', 'line', 'row', 'sect', 'page']);
const TAB = new Set(['tab', 'cell']);
const SYMBOL = { emdash: ' - ', endash: '-', bullet: '* ', lquote: "'", rquote: "'", ldblquote: '"', rdblquote: '"', emspace: ' ', enspace: ' ', qmspace: ' ' };
// Windows-1252 characters 0x80-0x9F (the rest of \'hh maps straight to Latin-1)
const CP1252 = { 0x80: '€', 0x82: '‚', 0x84: '„', 0x85: '...', 0x86: '†', 0x87: '‡', 0x89: '‰', 0x8b: '‹', 0x91: "'", 0x92: "'", 0x93: '"', 0x94: '"', 0x95: '* ', 0x96: '-', 0x97: ' - ', 0x99: '™', 0x9b: '›' };

export function isRtf(text) {
  return typeof text === 'string' && /^\s*\{\\rtf/.test(text);
}

export function rtfToText(rtf) {
  if (!isRtf(rtf)) return null;
  let out = '';
  const stack = [];
  let skip = false; // inside a group with no readable text
  let uc = 1; // characters to skip after \uN (the plain-text fallback)
  let fallback = 0;
  const emit = (s) => {
    if (skip) return;
    if (fallback > 0) {
      fallback--;
      return;
    }
    out += s;
  };

  for (let i = 0; i < rtf.length; ) {
    const ch = rtf[i];
    if (ch === '{') {
      stack.push({ skip, uc });
      i++;
      continue;
    }
    if (ch === '}') {
      const s = stack.pop();
      if (s) ({ skip, uc } = s);
      fallback = 0;
      i++;
      continue;
    }
    if (ch === '\r' || ch === '\n') {
      i++;
      continue;
    }
    if (ch !== '\\') {
      emit(ch);
      i++;
      continue;
    }

    const next = rtf[i + 1];
    if (next === '\\' || next === '{' || next === '}') {
      emit(next);
      i += 2;
      continue;
    }
    if (next === '*') {
      skip = true; // ignorable destination: skip this whole group
      i += 2;
      continue;
    }
    if (next === "'") {
      const code = parseInt(rtf.substr(i + 2, 2), 16);
      if (Number.isFinite(code)) emit(CP1252[code] ?? String.fromCharCode(code));
      i += 4;
      continue;
    }
    if (next === '~') {
      emit(' ');
      i += 2;
      continue;
    }
    if (next === '_') {
      emit('-');
      i += 2;
      continue;
    }
    const m = /^\\([a-zA-Z]{1,32})(-?\d{1,10})? ?/.exec(rtf.slice(i, i + 48));
    if (!m) {
      i += 2; // other control symbol (\-, \|, \:, line break after backslash)
      continue;
    }
    i += m[0].length;
    const word = m[1];
    const arg = m[2] === undefined ? null : parseInt(m[2], 10);
    if (word === 'bin' && arg > 0) {
      i += arg; // raw binary data
      continue;
    }
    if (SKIP.has(word)) {
      skip = true;
      continue;
    }
    if (word === 'uc' && arg !== null) {
      uc = arg;
      continue;
    }
    if (skip) continue;
    if (word === 'u' && arg !== null) {
      out += String.fromCharCode(arg < 0 ? arg + 65536 : arg);
      fallback = uc;
    } else if (BREAK.has(word)) {
      out += '\n';
    } else if (TAB.has(word)) {
      out += '\t';
    } else if (SYMBOL[word]) {
      out += SYMBOL[word];
    }
  }

  return out
    .split('\n')
    .map((line) => line.replace(/[  ]+/g, ' ').replace(/\t +| +\t/g, '\t').replace(/\s+$/, ''))
    .join('\n')
    .replace(/\n{3,}/g, '\n\n')
    .trim();
}
