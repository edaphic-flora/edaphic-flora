#!/usr/bin/env node
// Use one of your own photo files for a species.
//
//   node scripts/add-own-photo.mjs "Viola sororia" "C:/path/to/photo.jpg"
//
// Writes two web-sized copies into public/photos/own/ (640 px for cards,
// 1600 px for banners; location and camera data are stripped) and saves a
// locked row in ref_species_photos, so the weekly photo lookup never replaces
// it. These photos are served from the site itself; every other species photo
// is hotlinked.
//
// For a photo that is not of one species (a garden view for the home page):
//
//   node scripts/add-own-photo.mjs --scene woodland-garden "C:/path/to/photo.jpg"
//
// writes the two files only; list it in src/lib/sitePhotos.js to use it.
//
// Environment:
//   PHOTOS_DATABASE_URL  connection that can write ref_species_photos
//                        (falls back to DATABASE_URL). Without either, the
//                        files are written and the row is skipped.
//   PHOTO_CREATOR        credit name (default Todd Testerman)

import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import pg from 'pg';
import sharp from 'sharp'; // ships with Astro
import { normalizeName } from './lib/photo-lookup.mjs';
import { OWNER_LICENSE } from './lib/owner-photos.mjs';

const argv = process.argv.slice(2);
const scene = argv[0] === '--scene';
const [rawName, file] = scene ? argv.slice(1) : argv;
const name = scene ? null : normalizeName(rawName);
if ((!scene && !name) || (scene && !/^[a-z0-9]+(-[a-z0-9]+)*$/.test(rawName || '')) || !file) {
  console.error('usage: node scripts/add-own-photo.mjs "Genus species" <photo file>');
  console.error('   or: node scripts/add-own-photo.mjs --scene short-name <photo file>');
  process.exit(1);
}
const creator = process.env.PHOTO_CREATOR || 'Todd Testerman';
const key = scene ? null : name.toLowerCase();
const slug = scene ? rawName : key.replace(/[^a-z0-9]+/g, '-');
const outDir = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'public', 'photos', 'own');
await fs.mkdir(outDir, { recursive: true });

// .rotate() applies the camera's orientation; metadata is not carried over
const src = sharp(file).rotate();
const large = await src.clone().resize({ width: 1600, withoutEnlargement: true }).jpeg({ quality: 82, mozjpeg: true }).toFile(path.join(outDir, `${slug}-1600.jpg`));
await src.clone().resize({ width: 640, withoutEnlargement: true }).jpeg({ quality: 80, mozjpeg: true }).toFile(path.join(outDir, `${slug}-640.jpg`));
console.log(`[add-own-photo] wrote public/photos/own/${slug}-640.jpg and -1600.jpg (${large.width}x${large.height})`);

if (scene) process.exit(0);

const url = process.env.PHOTOS_DATABASE_URL || process.env.DATABASE_URL;
if (!url) {
  console.log('[add-own-photo] no database connection set, so the photo row was not saved.');
  process.exit(0);
}
const local = /@(localhost|127\.0\.0\.1)[:/]/.test(url) || /sslmode=disable/.test(url);
const client = new pg.Client({ connectionString: url, ssl: local ? false : { rejectUnauthorized: true } });
await client.connect();
try {
  await client.query(
    `INSERT INTO ref_species_photos
       (species_key, source, photo_id, page_url, image_url, large_url, width, height, license,
        creator, attribution, matched_name, tier, locked, checked_at, updated_at)
     VALUES ($1, 'manual', $2, NULL, $3, $4, $5, $6, $7, $8, $9, $10, 0, TRUE, now(), now())
     ON CONFLICT (species_key) DO UPDATE SET
       source = EXCLUDED.source, photo_id = EXCLUDED.photo_id, page_url = NULL,
       image_url = EXCLUDED.image_url, large_url = EXCLUDED.large_url, width = EXCLUDED.width,
       height = EXCLUDED.height, license = EXCLUDED.license, creator = EXCLUDED.creator,
       attribution = EXCLUDED.attribution, matched_name = EXCLUDED.matched_name, tier = 0,
       locked = TRUE, checked_at = now(), updated_at = now()`,
    [key, `${slug}-1600.jpg`, `/photos/own/${slug}-640.jpg`, `/photos/own/${slug}-1600.jpg`, large.width, large.height,
     OWNER_LICENSE, creator, `(c) ${creator}, all rights reserved`, name]
  );
  console.log(`[add-own-photo] saved a locked photo row for ${name}`);
} finally {
  await client.end();
}
