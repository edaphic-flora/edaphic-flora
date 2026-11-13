import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';

export default defineConfig({
  site: 'https://edaphicflora.org',
  trailingSlash: 'always',
  build: { format: 'directory' },
  // Shorter scoping markup than the default data attributes (smaller pages at 20K species)
  scopedStyleStrategy: 'class',
  integrations: [
    sitemap({
      filter: (page) => !page.includes('/404'),
    }),
  ],
});
