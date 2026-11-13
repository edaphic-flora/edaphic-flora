# edaphicflora.org (public site)

A fast, no-login static site built with [Astro](https://astro.build). Every species gets its own indexable page
(`/species/schizachyrium-scoparium/`) with native status by state, USDA traits, wildlife counts, and a community soil
snapshot once enough samples exist. Hosted on Netlify's free plan; data comes from the same Neon database as the app.

```
site/
├── astro.config.mjs        site URL, sitemap
├── netlify.toml            build, headers, .com → .org redirect
├── sql/public_views.sql    the ONLY data the site can read (privacy rules live here)
├── sql/species_photos.sql  photo table + view (one openly licensed photo per species)
├── scripts/export-data.mjs reads the views, writes src/data/generated/species.json
├── scripts/fetch-photos.mjs finds photos on iNaturalist / Wikimedia, saves them to Neon
├── public/                 logos, favicon, og image, robots.txt
└── src/
    ├── config.js           APP_URL, which hero ships, site URL
    ├── data/curated.json   hand-written blurbs, featured species, display names
    ├── data/sample/        illustrative sample data (used when there's no database)
    ├── lib/                data loading, texture classification, name helpers
    ├── components/         nav, footer, cards, soil snapshot, state map, etc.
    └── pages/              home, /species/, /species/[slug]/, /about/, 404
```

## How the data flows

1. `sql/public_views.sql` creates a `public_site` schema of read-only views. They strip contributor emails and
   locations and drop flagged samples and suspended users. Every remaining sample counts: soil values always come
   with their sample count, and pH and organic matter get a range (10th to 90th percentile) at 3 or more values
   (`public_site.settings`). Wildlife is counts and highlight flags only.
2. On every build, `scripts/export-data.mjs` reads those views (as the read-only `site_reader` role) and writes
   `src/data/generated/species.json`. That file is gitignored.
3. Astro renders one page per species from that file.

## Photos

`scripts/fetch-photos.mjs` finds one photo per species and stores the URL and credit in `ref_species_photos`
(`sql/species_photos.sql`). It tries iNaturalist first (exact plant name match, including synonyms), then the lead
image of the species' Wikipedia article on Wikimedia Commons. Only CC0, public domain, CC BY, CC BY-SA, CC BY-NC and
CC BY-NC-SA photos are used. Images are loaded straight from iNaturalist and Wikimedia, so they don't use Netlify
bandwidth. Each species page shows the photographer and license.

It works in priority order and saves each result as it goes, so it can be stopped and restarted:

1. species with a public soil snapshot
2. every other species in someone's garden (any soil sample)
3. plants in the wildlife tables
4. species native to at least one US state
5. everything else with a US state record

Run it locally (`npm run fetch-photos`, 400 species per run by default, about 1.2 to 2 seconds each) or let the
weekly GitHub workflow do 2,500 at a time. `PHOTO_LIMIT=1000 npm run fetch-photos` changes the batch size;
`PHOTO_DRY_RUN=true` lists what it would look up. Species with no usable photo are retried after 90 days.

To use your own photo for a species, insert it with `locked = TRUE` (example at the bottom of
`sql/species_photos.sql`); the script never replaces locked rows.

With no `DATABASE_URL`, the build uses `src/data/sample/species.json` and shows a gold "sample data" ribbon. A
production build without `DATABASE_URL` fails on purpose so sample numbers never go live.

`SITE_SCOPE` (set in `netlify.toml`) decides which species get pages:

| Scope     | Pages for                                                                  |
|-----------|----------------------------------------------------------------------------|
| `curated` | species with soil samples or wildlife data, plus featured ones (default)   |
| `native`  | curated + every species native to at least one US state with USDA traits   |
| `all`     | curated + every species with any US state record                           |

## One-time setup

**Neon**

1. Open the SQL editor on a dev branch and run `sql/public_views.sql` as the database owner. Spot check:
   `SELECT * FROM public_site.soil_species_summary LIMIT 20;`
2. Run it on the main branch.
3. Create the read-only role with SQL (instructions at the bottom of the file). Don't create it in the Neon
   Console: Console roles get `neon_superuser`, which can read everything.

**Netlify** (edaphicflora@gmail.com account)

1. Add new project > Import an existing project > GitHub > `edaphic-flora/edaphic-flora`.
2. Branch to deploy: `main`. **Base directory: `site`**. Build command and publish directory come from `netlify.toml`.
3. Site configuration > Environment variables: add `DATABASE_URL` =
   `postgresql://site_reader:<password>@<neon-host>/<db>?sslmode=require`.
4. Build & deploy > Branches and deploy contexts: add `dev` as a branch deploy.
5. Domain management: add `edaphicflora.org` as the primary domain and `edaphicflora.com` as an alias. Using
   Netlify DNS (point both domains' nameservers at Netlify) is the simplest; HTTPS is automatic.
6. Weekly refresh (`.github/workflows/site-weekly-rebuild.yml` at the repo root): create a Netlify build hook
   for `main` and add its URL to GitHub as the `NETLIFY_BUILD_HOOK` secret. For photos, run
   `sql/species_photos.sql`, create the `photo_loader` role (bottom of that file) and add its connection string as
   the `PHOTOS_DATABASE_URL` secret. Every Monday the workflow finds photos for 2,500 more species, then rebuilds.

## Everyday workflow

- Work on the `dev` branch and push. Netlify builds `dev--edaphicflora.netlify.app`. Branch deploys are free.
- Merge to `main` when it looks right. That's a production deploy.
- Commits that only touch the R app don't trigger a build (Netlify skips builds when `site/` is unchanged).

**Free plan budget:** 300 credits a month. A production deploy costs 15, bandwidth costs 20 per GB. The weekly
refresh is about 65 a month, which leaves room for roughly a dozen merges to `main`. If credits run out, every site
on the account pauses until the next month, which is why this site lives on its own Netlify account.

## Previewing on your computer

This is the fastest way to work on the site: it reloads in the browser a moment after any file changes.

One time (needs Node 22.23 or newer; check with `node -v`):

```bash
cd site
npm install
copy .env.example .env      # Windows (cp on Mac/Linux), then paste the real site_reader string into .env
```

Every time:

```bash
cd site
npm run dev                 # pulls fresh data from Neon, then serves http://localhost:4321
```

Leave it running while you work. Without `.env` it uses the sample data. `npm run fetch-photos` uses the same `.env`
(add `PHOTOS_DATABASE_URL`, or put the database owner's connection string in `DATABASE_URL` for that one run).

`node_modules/` is about 100 MB of small files and OneDrive will sync all of it. That works, just slowly. Keeping the
repo outside OneDrive (for example `C:\dev\edaphic-flora`) avoids it; GitHub is already the backup.

## Editing content

- **Blurbs, featured species, display names:** `src/data/curated.json`.
- **Home page hero:** `HERO` in `src/config.js` (`'a'` or `'b'`). Non-production builds show both with a switcher.
- **Where app buttons go:** `APP_URL` in `src/config.js`.
- **About page, credits:** `src/pages/about.astro`.
- **Thresholds:** `public_site.settings` in `sql/public_views.sql` (re-run the file after changing).

To regenerate the sample file from a test database: `EXPORT_LABEL=sample EXPORT_OUT=src/data/sample/species.json npm run export-data`.
