// Helpers for the My Garden page (/my-garden/). The data comes from
// GET /api/garden (site_api.my_garden in sql/garden.sql); this file only adds
// display names and does the arithmetic for the charts. Family names, blurbs
// and genus names are carried over from the Shiny app (app/R/mod_my_garden.R).

import { titleCaseCommon } from './names.js';

// Wildlife groups, in the order the page shows them
export const GROUPS = [
  { type: 'Butterfly', label: 'Butterflies', noun: 'butterfly', id: 'butterflies' },
  { type: 'Moth', label: 'Moths', noun: 'moth', id: 'moths' },
  { type: 'Bee', label: 'Specialist bees', noun: 'specialist bee', id: 'bees' },
  { type: 'Bird', label: 'Birds', noun: 'bird', id: 'birds' },
];

// Plant types for the recommendations filter (keys match sql/garden.sql)
export const LIFE_FORMS = [
  { key: 'All', label: 'All plant types' },
  { key: 'Tree', label: 'Trees' },
  { key: 'Shrub', label: 'Shrubs' },
  { key: 'Perennial', label: 'Perennials' },
  { key: 'Grass/Sedge', label: 'Grasses and sedges' },
  { key: 'Vine', label: 'Vines' },
  { key: 'Fern', label: 'Ferns' },
];

// Friendly family names, with a few familiar members
export const FAMILY_INFO = {
  // butterflies and moths
  Nymphalidae: ['Brushfoots', 'Monarchs, Painted Ladies, Red Admirals'],
  Papilionidae: ['Swallowtails', 'Tiger Swallowtails, Black Swallowtails, Spicebush Swallowtails'],
  Pieridae: ['Whites & Sulphurs', 'Cabbage Whites, Clouded Sulphurs, Orange Sulphurs'],
  Lycaenidae: ['Blues & Hairstreaks', 'Spring Azures, Gray Hairstreaks, Eastern Tailed-Blues'],
  Hesperiidae: ['Skippers', 'Silver-Spotted Skippers, Fiery Skippers, Cloudywings'],
  Riodinidae: ['Metalmarks', 'Northern Metalmarks, small jewel-like butterflies'],
  Libytheidae: ['Snout Butterflies', 'American Snouts, whose long palps look like a snout'],
  Satyridae: ['Satyrs & Wood-Nymphs', 'Common Wood-Nymphs, Little Wood-Satyrs'],
  Danaidae: ['Milkweed Butterflies', 'Monarchs and Queens, milkweed specialists'],
  Noctuidae: ['Owlet Moths', 'Underwings, daggers and cutworms: the largest moth family'],
  Geometridae: ['Geometer Moths', 'Inchworms, peppered moths, emerald moths'],
  Erebidae: ['Tiger Moths & Kin', 'Isabella Tiger Moths (woolly bears), lichen moths'],
  Arctiidae: ['Tiger Moths', 'Woolly bears, Isabella moths, garden tiger moths'],
  Sphingidae: ['Sphinx Moths', 'Hummingbird moths, tomato hornworms, hawk moths'],
  Saturniidae: ['Giant Silk Moths', 'Luna Moths, Cecropia Moths, Polyphemus Moths'],
  Tortricidae: ['Leafroller Moths', 'Spruce budworms, codling moths, oak leafrollers'],
  Notodontidae: ['Prominent Moths', 'Variable oakleaf moths, walnut caterpillar moths'],
  Lasiocampidae: ['Tent Caterpillar Moths', 'Eastern tent caterpillars, forest tent caterpillars'],
  Pyralidae: ['Snout Moths', 'Meal moths, wax moths, bee moths'],
  Gracillariidae: ['Leaf Miner Moths', 'Tiny moths whose larvae mine inside leaves'],
  Gelechiidae: ['Twirler Moths', 'Leaf-tying and stem-boring micro-moths'],
  Oecophoridae: ['Concealer Moths', 'Small moths often found on dead leaves and bark'],
  Sesiidae: ['Clearwing Moths', 'Wasp-mimicking moths with transparent wings'],
  Coleophoridae: ['Casebearer Moths', 'Larvae build portable cases from leaf fragments'],
  Nepticulidae: ['Pygmy Moths', 'Among the smallest moths; larvae mine in leaves'],
  Bucculatrigidae: ['Ribbed Cocoon Makers', 'Micro-moths with distinctive ribbed cocoons'],
  Tisheriidae: ['Trumpet Leaf Miner Moths', 'Trumpet-shaped leaf mines on oaks and other trees'],
  Cosmopterigidae: ['Cosmet Moths', 'Colorful micro-moths, some of them stem borers'],
  Lymantriidae: ['Tussock Moths', 'White-marked tussock moths, spongy moths'],
  Yponomeutidae: ['Ermine Moths', 'Small white moths with black spots'],
  Pterophoridae: ['Plume Moths', 'T-shaped moths with feathery wings'],
  Psychidae: ['Bagworm Moths', 'Larvae build camouflaged bags from plant material'],
  Heliozelidae: ['Shield Bearer Moths', 'Metallic micro-moths; larvae mine in leaves'],
  Drepanidae: ['Hooktip Moths', 'Arched hooktip, rose hooktip'],
  Limacodidae: ['Slug Caterpillar Moths', 'Saddleback caterpillars, hag moths'],
  Crambidae: ['Grass Moths', 'Sod webworms, grape leaffolder moths'],
  Depressariidae: ['Flat-Body Moths', 'Agonopterix and kin; larvae feed in flower heads and rolled leaves'],
  Choreutidae: ['Metalmark Moths', 'Day-flying moths with metallic wing patterns'],
  Cossidae: ['Carpenter Moths', 'Large moths whose larvae bore in wood'],
  Megalopygidae: ['Flannel Moths', "Fluffy 'puss caterpillars' with hidden stinging spines"],
  Zygaenidae: ['Smoky Moths', 'Day-flying moths, some with bright warning colors'],
  Hepialidae: ['Ghost Moths', 'Swift moths with hovering courtship flights at dusk'],
  Blastobasidae: ['Scavenger Moths', 'Small moths whose larvae feed on seeds and detritus'],
  Thyatiridae: ['Lutestring Moths', 'Subtly patterned moths, often on birch and willow'],
  Tineidae: ['Fungus & Clothes Moths', 'Larvae feed on fungi, lichens and organic debris'],
  Elachistidae: ['Grass-Miner Moths', 'Tiny moths whose larvae mine in grass blades'],
  Lyonetiidae: ['Liner Moths', 'Minute moths with narrow fringed wings'],
  Acrolophidae: ['Tube Moths', 'Larvae build silk tubes in soil and leaf litter'],
  Scythrididae: ['Flower Moths', 'Small day-flying moths often found on flowers'],
  Eriocraniidae: ['Sparkling Archaic Moths', 'Primitive metallic moths, early spring fliers'],
  Mimallonidae: ['Sack-Bearer Moths', 'Caterpillars build leaf cases, like bagworms'],
  Incurvariidae: ['Leafcutter Moths', 'Females cut leaf discs to protect their eggs'],
  Epiplemidae: ['Tropical Geometers', 'Geometer relatives with scalloped wing edges'],
  Carposinidae: ['Fruitworm Moths', 'Larvae bore into fruits and seeds'],
  Ypsolophidae: ['Ypsolophid Moths', 'Small moths whose larvae feed on trees and shrubs'],
  Momphidae: ['Mompha Moths', 'Tiny moths whose larvae mine in evening primrose stems'],
  Apatelodidae: ['American Silkworm Moths', 'Fuzzy moths related to silk moths'],
  Schreckensteiniidae: ['Bristle-Legged Moths', 'Distinctive spiny-legged micro-moths'],
  Symmocidae: ['Symmocid Moths', 'Little-known micro-moths, mostly bark and litter feeders'],
  Douglasiidae: ['Douglas Moths', 'Tiny leaf-mining moths on mints and composites'],
  Epermeniidae: ['Fringe-Tufted Moths', 'Small moths with raised scale tufts on the hindwings'],
  Copromorphidae: ['Tropical Fruitworm Moths', 'A rare family; larvae in fruits and seeds'],
  Plutellidae: ['Diamondback Moths', 'Includes the widespread diamondback moth'],
  Urodidae: ['False Burnet Moths', 'Rare moths related to burnets'],
  Adelidae: ['Fairy Moths', 'Tiny metallic moths with very long antennae'],
  Thyrididae: ['Window-Winged Moths', 'Translucent wing patches like tiny stained glass'],
  Prodoxidae: ['Yucca Moths', 'Famous pollinators of yucca plants'],
  Galacticidae: ['Galactic Moths', 'Rare micro-moths with little-known biology'],
  // bees
  Andrenidae: ['Mining Bees', 'Solitary ground-nesters, important spring pollinators'],
  Halictidae: ['Sweat Bees', 'Metallic green bees, tiny but prolific pollinators'],
  Megachilidae: ['Leafcutter & Mason Bees', 'Orchard bees and leafcutters that nest in stems'],
  Colletidae: ['Plasterer Bees', 'Cellophane bees that line their nests with a waterproof secretion'],
  Apidae: ['Long-Tongued Bees', 'Bumble bees, carpenter bees, cuckoo bees, digger bees'],
  Melittidae: ['Melittid Bees', 'Oil-collecting bees, often specialists on a few flowers'],
  // birds
  Parulidae: ['Warblers', 'Yellow Warblers, Black-and-White Warblers'],
  Fringillidae: ['Finches', 'Goldfinches, Purple Finches, House Finches'],
  Icteridae: ['Blackbirds & Orioles', 'Baltimore Orioles, Red-winged Blackbirds'],
  Turdidae: ['Thrushes', 'American Robins, Wood Thrushes, bluebirds'],
  Corvidae: ['Crows & Jays', 'Blue Jays, American Crows'],
  Picidae: ['Woodpeckers', 'Downy Woodpeckers, Red-bellied Woodpeckers, flickers'],
  Trochilidae: ['Hummingbirds', 'Ruby-throated Hummingbirds'],
  Paridae: ['Chickadees & Titmice', 'Black-capped Chickadees, Tufted Titmice'],
  Sittidae: ['Nuthatches', 'White-breasted Nuthatches, Red-breasted Nuthatches'],
  Vireonidae: ['Vireos', 'Red-eyed Vireos, Warbling Vireos'],
  Bombycillidae: ['Waxwings', 'Cedar Waxwings, voracious fruit eaters'],
  Mimidae: ['Mockingbirds & Thrashers', 'Northern Mockingbirds, Gray Catbirds, Brown Thrashers'],
  Passerellidae: ['New World Sparrows', 'Song Sparrows, White-throated Sparrows, towhees'],
  Cardinalidae: ['Cardinals & Grosbeaks', 'Northern Cardinals, Rose-breasted Grosbeaks, Indigo Buntings'],
  Phasianidae: ['Grouse & Quail', 'Ruffed Grouse, Wild Turkeys, bobwhites'],
  Columbidae: ['Doves', 'Mourning Doves'],
  Tyrannidae: ['Flycatchers', 'Eastern Phoebes, Great Crested Flycatchers'],
  Anatidae: ['Ducks & Geese', 'Wood Ducks and Mallards, which eat seeds and acorns'],
  Sturnidae: ['Starlings', 'European Starlings, introduced and abundant fruit eaters'],
};

export const familyLabel = (family) => FAMILY_INFO[family]?.[0] || family;
export const familyBlurb = (family) => FAMILY_INFO[family]?.[1] || null;
// A few records name two families ("Coleophoridae - Batrachedridae"): link the first
export const familyLinks = (family) => {
  const f = encodeURIComponent(String(family).split(/\s+-\s+/)[0]);
  return { wikipedia: `https://en.wikipedia.org/wiki/${f}`, inaturalist: `https://www.inaturalist.org/taxa/${f}` };
};

// Genera whose USDA genus-level names are obscure or misleading
export const GENUS_OVERRIDES = {
  Vaccinium: 'Blueberries', Carya: 'Hickories', Populus: 'Poplars', Prunus: 'Cherries & Plums',
  Celtis: 'Hackberries', Nyssa: 'Tupelos', Amelanchier: 'Serviceberries', Viburnum: 'Viburnums',
  Rhus: 'Sumacs', Ilex: 'Hollies', Liriodendron: 'Tulip Trees', Lindera: 'Spicebushes', Cercis: 'Redbuds',
  Cephalanthus: 'Buttonbushes', Asclepias: 'Milkweeds', Monarda: 'Bee Balms', Pycnanthemum: 'Mountain Mints',
  Eutrochium: 'Joe-Pye Weeds', Vernonia: 'Ironweeds', Zizia: 'Golden Alexanders', Helianthus: 'Sunflowers',
  Echinacea: 'Coneflowers', Liatris: 'Blazing Stars', Agastache: 'Hyssops', Baptisia: 'Wild Indigos',
  Lobelia: 'Lobelias', Chelone: 'Turtleheads', Penstemon: 'Beardtongues', Schizachyrium: 'Little Bluestems',
  Panicum: 'Switchgrasses', Andropogon: 'Bluestems', Sporobolus: 'Dropseed Grasses', Campsis: 'Trumpet Creepers',
  Clematis: 'Clematis', Rhododendron: 'Rhododendrons & Azaleas', Wisteria: 'Wisterias', Celastrus: 'Bittersweets',
  Parthenocissus: 'Virginia Creepers', Lonicera: 'Honeysuckles', Tsuga: 'Hemlocks', Ulmus: 'Elms',
  Diospyros: 'Persimmons',
};

// "oak" -> "Oaks", "blueberry" -> "Blueberries", "beech" -> "Beeches"
export function pluralName(name) {
  const t = titleCaseCommon(name);
  if (!t) return null;
  if (/s$/i.test(t)) return t;
  if (/[^aeiou]y$/i.test(t)) return t.slice(0, -1) + 'ies';
  if (/(sh|ch|x|z)$/i.test(t)) return t + 'es';
  return t + 's';
}

// Display name for a genus: the override, else the USDA name made plural, else null
export function genusLabel(genus, names = {}) {
  if (!genus) return null;
  return GENUS_OVERRIDES[genus] || pluralName(names[genus]) || null;
}

// "Carya × brownii" -> "Carya"
export const genusOf = (species) => String(species || '').replace(/^\s*×\s*/, '').split(/\s+/)[0];

// Width of a meter fill, in percent. A small nonzero share still shows a sliver.
export function meterPercent(covered, total) {
  if (!(total > 0) || !(covered > 0)) return 0;
  return Math.min(100, Math.max(1.5, (100 * covered) / total));
}

// stroke-dasharray for a donut ring of radius r showing covered / total
export function donutDash(covered, total, r) {
  const c = 2 * Math.PI * r;
  const fill = (meterPercent(covered, total) / 100) * c;
  return `${fill.toFixed(2)} ${(c - fill).toFixed(2)}`;
}

export const plantName = (p) => titleCaseCommon(p.common_name) || p.species;
const sortName = (g) => (g.plants.length === 1 ? plantName(g.plants[0]) : g.label);

// Sort the garden into the plants that count (native or not recorded, grouped
// by genus), introduced plants and invasive plants.
export function groupPlants(plants, genusNames = {}) {
  const sorted = [...plants].sort((a, b) => plantName(a).localeCompare(plantName(b)));
  const counted = sorted.filter((p) => p.status === 'native' || p.status === 'unknown');
  const byGenus = new Map();
  for (const p of counted) {
    const g = genusOf(p.species);
    if (!byGenus.has(g)) byGenus.set(g, []);
    byGenus.get(g).push(p);
  }
  // sorted by what the list shows: the plant's name, or the genus name for a group
  const genera = [...byGenus.entries()]
    .map(([genus, list]) => ({ genus, label: genusLabel(genus, genusNames) || genus, plants: list }))
    .sort((a, b) => sortName(a).localeCompare(sortName(b)));
  return {
    genera,
    introduced: sorted.filter((p) => p.status === 'introduced'),
    invasive: sorted.filter((p) => p.status === 'invasive'),
  };
}

// "supports 3 moths and butterflies and 1 bird" for an introduced plant
export function introducedSupport(p) {
  const parts = [];
  if (p.lep) parts.push(`${p.lep} ${p.lep === 1 ? 'moth or butterfly' : 'moths and butterflies'}`);
  if (p.bee) parts.push(`${p.bee} specialist ${p.bee === 1 ? 'bee' : 'bees'}`);
  if (p.bird) parts.push(`${p.bird} ${p.bird === 1 ? 'bird' : 'birds'}`);
  if (!parts.length) return 'No wildlife recorded on this species';
  return `Supports ${parts.length === 1 ? parts[0] : `${parts.slice(0, -1).join(', ')} and ${parts.at(-1)}`}`;
}

// Title for a recommended genus. With only one native species in the state,
// name that species ("Bearberry", not "Manzanitas"), as the app did.
export function recTitle(rec, genusNames = {}) {
  if (rec.natives?.length === 1 && rec.native_count === 1 && rec.natives[0].common_name) {
    return titleCaseCommon(rec.natives[0].common_name);
  }
  return genusLabel(rec.genus, genusNames) || `${rec.genus} species`;
}
