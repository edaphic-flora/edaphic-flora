// Outbound links to the data sources named on the site, kept in one place.
// scripts/check-links.mjs tests these (and the state invasive lists) every week.
import bonapMissing from '../data/bonap-genus-missing.json' with { type: 'json' };

export const SOURCES = {
  bonap: 'https://bonap.net/tdc',
  usda: 'https://plants.usda.gov/',
  nwf: 'https://nativeplantfinder.nwf.org/',
  fowler: 'https://jarrodfowler.com/specialist_bees.html',
  audubon: 'https://www.audubon.org/native-plants',
};

// A species' own page on USDA PLANTS
export const usdaProfile = (symbol) => `https://plants.usda.gov/plant-profile/${encodeURIComponent(symbol)}`;

// BONAP's county maps for a genus. Null for the few genera BONAP has no page for.
export function bonapGenusMaps(genus) {
  if (!genus || !/^[A-Za-z-]+$/.test(genus) || bonapMissing.includes(genus)) return null;
  return `https://bonap.net/Napa/TaxonMaps/Genus/County/${genus}`;
}
