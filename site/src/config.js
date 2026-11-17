// Site-wide settings. Change these in one place.

export const SITE_URL = 'https://edaphicflora.org';
export const SITE_NAME = 'Edaphic Flora';

// "Add a soil test" buttons go to the site's own contribute page.
export const CONTRIBUTE_URL = '/contribute/';

export const CONTACT_EMAIL = 'edaphicflora@gmail.com';
export const GITHUB_URL = 'https://github.com/edaphic-flora/edaphic-flora';

// Netlify sets CONTEXT to production / deploy-preview / branch-deploy.
// Local builds have no CONTEXT and count as non-production.
export const IS_PRODUCTION = process.env.CONTEXT === 'production';

// pH axis used by the soil snapshot bars
export const PH_SCALE = { min: 4.0, max: 8.5 };

// Organic matter axis on the soil snapshot, in percent. Most mineral soils fall
// well inside it; 20% and up is where labs start calling a soil organic.
export const OM_SCALE = { min: 0, max: 20 };
