// Photos used outside species pages (home mosaic, feature cards, call-to-action
// band, About, 404), credited wherever they appear.
//
// Most are species photos from ref_species_photos, picked by slug; each list
// falls back to the next slug, then to any featured species with a photo, if
// a species drops out. The home mosaic also takes "scenes": the site owner's
// own garden photos in public/photos/own/ (scripts/add-own-photo.mjs --scene),
// which belong to no single species page.
import { getSpecies, featured } from './data.js';
import { photoCredit } from './photos.js';

const OWNER = 'Todd Testerman';

// A photo of the owner's that is not one species: no link, just a caption
const scene = (file, caption, alt) => {
  const photo = { src: `/photos/own/${file}-640.jpg`, large: `/photos/own/${file}-1600.jpg`, source: 'manual', license: 'arr', creator: OWNER };
  return { scene: true, photo, photoCredit: photoCredit(photo), displayName: caption, alt, url: null };
};

// Home mosaic, in tile order: tall, wide, small, small, wide (bottom right)
const MOSAIC = [
  scene('woodland-garden', 'Woodland garden', 'A woodland garden in bloom: wild geranium, foamflower and golden flowers'),
  'symphyotrichum-novae-angliae',
  'adiantum-pedatum',
  'schizachyrium-scoparium',
  'rudbeckia-subtomentosa',
];
// Stand-ins when a species above has no photo (sample data, a removed species)
const MOSAIC_SPARE = ['onoclea-sensibilis', 'echinacea-purpurea', 'eragrostis-spectabilis', 'sporobolus-heterolepis'];

const SLUGS = {
  native: ['delphinium-exaltatum', 'baptisia-australis'],
  wildlife: ['asclepias-syriaca', 'eutrochium-maculatum', 'asclepias-tuberosa'],
  band: ['andropogon-virginicus', 'schizachyrium-scoparium'],
  about: ['baptisia-australis', 'rudbeckia-subtomentosa'],
  notFound: ['carex-stricta', 'sporobolus-heterolepis'],
};

const withPhoto = (slugs) => slugs.map(getSpecies).filter((s) => s && s.photo);
const fallback = () => featured(8).filter((s) => s.photo);
const first = (slugs) => withPhoto(slugs)[0] || fallback()[0] || null;

function mosaic() {
  const spare = [...withPhoto(MOSAIC_SPARE), ...fallback()];
  const used = new Set();
  const tiles = [];
  for (const item of MOSAIC) {
    let tile = typeof item === 'string' ? withPhoto([item])[0] : item;
    while (!tile || used.has(tile)) {
      tile = spare.shift();
      if (!tile) break;
    }
    if (!tile) continue;
    used.add(tile);
    tiles.push(tile);
  }
  return tiles;
}

export const sitePhotos = {
  mosaic: mosaic(),
  native: first(SLUGS.native),
  wildlife: first(SLUGS.wildlife),
  band: first(SLUGS.band),
  about: first(SLUGS.about),
  notFound: first(SLUGS.notFound),
};
