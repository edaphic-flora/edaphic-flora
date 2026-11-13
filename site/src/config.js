// Site-wide settings. Change these in one place.

export const SITE_URL = 'https://edaphicflora.org';
export const SITE_NAME = 'Edaphic Flora';

// Where "contribute" and "open the app" buttons go. When the new
// contribution flow replaces Shiny, change this one line.
export const APP_URL = 'https://edaphicflora.shinyapps.io/edaphic-flora/';

// Which home page hero ships to production: 'a' (editorial, dark) or 'b'
// (data-forward, light). Non-production builds show both with a switcher.
export const HERO = 'a';

export const CONTACT_EMAIL = 'edaphicflora@gmail.com';
export const GITHUB_URL = 'https://github.com/edaphic-flora/edaphic-flora';

// Netlify sets CONTEXT to production / deploy-preview / branch-deploy.
// Local builds have no CONTEXT and count as non-production.
export const IS_PRODUCTION = process.env.CONTEXT === 'production';

// pH axis used by the soil snapshot bars
export const PH_SCALE = { min: 4.0, max: 8.5 };
