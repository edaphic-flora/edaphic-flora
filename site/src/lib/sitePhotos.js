// Photos used outside species pages (home mosaic, feature cards, call-to-action
// band, About, 404). Each is a species photo from ref_species_photos, so it is
// openly licensed and credited wherever it appears. Each list falls back to the
// next slug, then to any featured species with a photo, if a species drops out.
import { getSpecies, featured } from './data.js';

const SLUGS = {
  mosaic: ['schizachyrium-scoparium', 'echinacea-purpurea', 'eragrostis-spectabilis', 'sporobolus-heterolepis', 'rudbeckia-subtomentosa'],
  native: ['delphinium-exaltatum', 'baptisia-australis'],
  wildlife: ['eutrochium-maculatum', 'asclepias-tuberosa'],
  band: ['andropogon-virginicus', 'schizachyrium-scoparium'],
  about: ['baptisia-australis', 'rudbeckia-subtomentosa'],
  notFound: ['carex-stricta', 'sporobolus-heterolepis'],
};

const withPhoto = (slugs) => slugs.map(getSpecies).filter((s) => s && s.photo);
const fallback = () => featured(8).filter((s) => s.photo);
const first = (slugs) => withPhoto(slugs)[0] || fallback()[0] || null;
const mosaic = withPhoto(SLUGS.mosaic);

export const sitePhotos = {
  mosaic: mosaic.length >= 5 ? mosaic : [...mosaic, ...fallback().filter((s) => !mosaic.includes(s))].slice(0, 5),
  native: first(SLUGS.native),
  wildlife: first(SLUGS.wildlife),
  band: first(SLUGS.band),
  about: first(SLUGS.about),
  notFound: first(SLUGS.notFound),
};
