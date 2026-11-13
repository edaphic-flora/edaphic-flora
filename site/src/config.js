// Site-wide settings. Change these in one place.

export const SITE_URL = 'https://edaphicflora.org';
export const SITE_NAME = 'Edaphic Flora';

// "Add a soil test" buttons go to the site's own contribute page.
export const CONTRIBUTE_URL = '/contribute/';

// Which home page hero ships to production: 'a' (editorial, dark) or 'b'
// (data-forward, light). Non-production builds show both with a switcher.
export const HERO = 'b';

export const CONTACT_EMAIL = 'edaphicflora@gmail.com';
export const GITHUB_URL = 'https://github.com/edaphic-flora/edaphic-flora';

// Netlify sets CONTEXT to production / deploy-preview / branch-deploy.
// Local builds have no CONTEXT and count as non-production.
export const IS_PRODUCTION = process.env.CONTEXT === 'production';

// pH axis used by the soil snapshot bars
export const PH_SCALE = { min: 4.0, max: 8.5 };
