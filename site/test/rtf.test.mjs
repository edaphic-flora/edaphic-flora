import { test } from 'node:test';
import assert from 'node:assert/strict';
import { rtfToText, isRtf } from '../netlify/lib/rtf.mjs';

// A made-up report in the shape of the CAES Morgan reports (tab stops, fonts,
// colors, nested groups), plus the escapes RTF writers commonly use.
const SAMPLE = String.raw`{\rtf1\ansi\ansicpg1252{\colortbl\red0\green0\blue0;}{\fonttbl\f0\fcharset0\fnil Arial;}
{\*\generator Some Writer 1.0;}{\info{\title Secret title}{\author Someone}}
\pard\plain{\plain\tab\fs28\b\f2 SAMPLE SOIL LAB{\fs34\par}}
\pard{\plain\tab\fs20 LABORATORY NUMBER:\plain\tab 1234{\fs25\par}}
\pard{\plain\tab pH\plain\tab 6.2{\fs25\par}}
\pard{\plain\tab PHOSPHORUS\plain\tab Medium High (50 ppm){\par}}
\trowd\cellx2000\cellx4000 CALCIUM\cell High (1600 ppm)\cell\row
{\pict\pngblip 89504e470d0a1a0a}
Caf\'e9 \u8211?dash \u233\'e9 and \{braces\} and \\backslash\line second line\par
{\*\bkmkstart x}kept text{\*\bkmkend x}\par
}`;

test('detects RTF', () => {
  assert.equal(isRtf(SAMPLE), true);
  assert.equal(isRtf('pH 6.2'), false);
  assert.equal(rtfToText('not rtf'), null);
});

test('keeps the text and table layout, drops fonts, colors, info and pictures', () => {
  const t = rtfToText(SAMPLE);
  const lines = t.split('\n').map((l) => l.trim());
  for (const want of ['SAMPLE SOIL LAB', 'LABORATORY NUMBER:\t1234', 'pH\t6.2', 'PHOSPHORUS\tMedium High (50 ppm)', 'CALCIUM\tHigh (1600 ppm)']) {
    assert.ok(lines.includes(want), `${JSON.stringify(want)} not in ${JSON.stringify(lines)}`);
  }
  for (const gone of ['Arial', 'Secret title', 'Someone', 'Some Writer', '89504e47', 'red0', '\\par', '\\tab', 'fcharset']) {
    assert.ok(!t.includes(gone), `should not contain ${gone}`);
  }
  assert.ok(t.includes('kept text'));
});

test('escapes: hex, unicode with fallback, braces, backslash, line breaks', () => {
  const t = rtfToText(SAMPLE);
  assert.ok(t.includes('Café'));
  assert.ok(t.includes('\u2013dash')); // \u8211 with its "?" fallback skipped
  assert.ok(t.includes('é and')); // \u233 then the \'e9 fallback skipped
  assert.ok(t.includes('{braces}'));
  assert.ok(t.includes('\\backslash'));
  assert.ok(t.includes('\nsecond line'));
});

test('handles empty and malformed input without throwing', () => {
  assert.equal(rtfToText('{\\rtf1}'), '');
  assert.equal(typeof rtfToText('{\\rtf1 unclosed {group \\par text'), 'string');
  assert.equal(typeof rtfToText('{\\rtf1 \\'), 'string');
});
