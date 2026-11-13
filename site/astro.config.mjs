import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';

export default defineConfig({
  site: 'https://edaphicflora.org',
  trailingSlash: 'always',
  build: { format: 'directory' },
  // Shorter scoping markup than the default data attributes (smaller pages at 20K species)
  scopedStyleStrategy: 'class',
  // Never inline scripts: the sign-in pages' content security policy only
  // allows scripts loaded from the site itself.
  vite: { build: { assetsInlineLimit: 0 } },
  integrations: [
    sitemap({
      filter: (page) => !page.includes('/404') && !page.includes('/account/'),
    }),
  ],
});
