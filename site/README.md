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
├── sql/contribute.sql      sign-in + submit: reference tables, site_api functions, site_writer role
├── sql/export.sql          "Download my samples (CSV)": site_api.export_my_samples
├── sql/admin.sql           moderation: admin_api functions, site_admin role, site_admins list
├── sql/garden.sql          My Garden: site_api.my_garden (wildlife counts for a signed-in person's plants)
├── netlify/functions/      lookup.mjs (species search, zip), api.mjs (signed-in submit), export.mjs (my samples CSV),
│                           admin.mjs (/admin/ moderation, admins only), garden.mjs (My Garden)
├── netlify/lib/            token check, validation, texture (shared with the contribute page)
├── test/                   `npm test` unit tests; test/db/ checks the database functions
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

## Contributing soil tests (sign-in and submit)

`/contribute/` and `/account/` let people add soil tests without the Shiny app. They sign in with Firebase (the
same Firebase project as the app, so app accounts work), and two Netlify functions do the rest:

- `netlify/functions/lookup.mjs`: public species search and zip lookup, read-only (`DATABASE_URL`), cached at the edge.
- `netlify/functions/api.mjs`: verifies the Firebase ID token, validates with `netlify/lib/validate.mjs` (the same
  rules as the app), then calls the `site_api` database functions with `SITE_WRITER_DATABASE_URL`.

The `site_writer` login can only call `site_api` functions (`sql/contribute.sql`), never read or write tables
directly, because Netlify's free plan exposes every environment variable to builds too. The functions cap samples
at 20 per person per day (300 overall), check for duplicates, skip disabled users and write the audit log, like the
app. Existing app contributors are linked to their Polished id by verified email on first sign-in (`legacy_users`).
New samples show in the person's account right away and on species pages after the weekly rebuild. Firebase loads
only on these two pages, which get a strict content security policy (`netlify.toml`).

Reading a lab report: `netlify/functions/extract.mjs` (`POST /api/extract`) sends the PDF or photo to Claude
(`claude-opus-5-5` at low effort, structured JSON output, server-side refusal fallback) and returns values for the
person to check; the file is never stored. Limits match the earlier app (3 per person and 20 overall per day,
enforced in the database so failed calls don't count). Needs `ANTHROPIC_API_KEY` in Netlify. The account page also
saves a home zip or state (`user_preferences`), which fills in the contribute form.

## Downloads (CSV)

Both replace the Shiny app's CSV exports, which are gone with the app.

- **Download my samples** on `/account/`: `netlify/functions/export.mjs` (`GET /api/export/my-samples`) calls
  `site_api.export_my_samples` (`sql/export.sql`). It returns every column the person entered, their own location and
  notes included, and never `created_by`. Same rules as the app: 10 downloads per person per 24 hours, none for
  disabled accounts, each one logged to `audit_log` as `export`.
- **Community soil data** on `/about/#download`: `src/pages/data/edaphic-flora-community-soil.csv.js` writes
  `/data/edaphic-flora-community-soil.csv` at build time, so downloads cost no function calls. One row per species with
  samples, built from the same exported records as the species pages (so from `public_site.soil_species_summary`):
  sample and contributor counts, pH and organic matter medians with ranges or readings, texture, and P, K, Ca and Mg
  medians. No per-sample rows, contributors, locations or notes. Licensed CC BY-NC 4.0 (About page, plus a
  `Link: rel="license"` header in `netlify.toml`). It refreshes with the weekly rebuild.

Both use `netlify/lib/csv.mjs` (column lists, quoting, spreadsheet formula guard), tested in `test/csv.test.mjs`.

Tests: `npm test` (validation, texture against the app's R code, token checks, report cleanup, CSV building) and,
from the repo root, `Rscript site/test/db/contribute_functions_test.R` and `Rscript site/test/db/export_functions_test.R`
(database functions, run in a transaction that is rolled back; the export test applies `sql/export.sql` inside it).
Locally, `npx netlify dev` from `site/` serves the site and the functions on http://localhost:8888.

## Moderating (/admin/)

`/admin/` is the site's version of the app's Admin tab. It isn't linked from any page and isn't indexed. Sign in on
`/account/` with an admin account, then open `edaphicflora.org/admin/` (or `dev--edaphicflora.netlify.app/admin/`).

- **Samples**: newest first, with notes and flag state. Search by plant, notes or `#id`, or show one contributor or
  only flagged samples. **Flag** (optional reason) or **Unflag**.
- **Contributors**: everyone with samples, with counts. **Flag all** flags every unflagged sample from one person
  (existing reasons are kept). **Disable** stops new samples and report reading; tick the box to also flag their
  samples, so they stay hidden if the account is re-enabled. You can't disable yourself.
- **Disabled accounts**: who, when, by whom and why. **Re-enable** lets them contribute again; flags stay.
- **Audit log**: every change, newest first. Filter by action; search by user id, record id or text.

Flagged samples and every sample from a disabled account drop out of the `public_site` views, so they leave species
pages at the next weekly rebuild (run the build hook to do it sooner). The contributor sees "flagged for review" on
`/account/` straight away. Nothing deletes or edits a sample, and every change writes an `audit_log` row as the app
did (`flag`, `unflag`, `bulk_flag`, `disable_user`, `enable_user`, with the admin's contributor id).

**Who is an admin:** a verified sign-in whose email is in both lists, like the app's "Polished admin and
`ADMIN_EMAILS`" rule:

1. `ADMIN_EMAILS` in Netlify (comma-separated, any case). `netlify/functions/admin.mjs` checks it before it opens a
   database connection; everyone else gets a 403.
2. The `site_admins` table, checked again by every `admin_api` function in `sql/admin.sql`.

The function connects as `site_admin`, its own login that can only call `admin_api` functions (no table access, no
emails or locations returned). It is kept apart from `site_writer` on purpose: every signed-in request uses the
`site_writer` string, and Netlify's free plan shows every variable to builds, so that string leaking must never allow
moderation. To add or remove an admin, change both lists.

**Setup** (once per Neon branch, dev first):

1. Run `sql/admin.sql` as the database owner, after `contribute.sql`.
2. Create the `site_admin` login and add your email to `site_admins` (SQL at the bottom of `admin.sql`), then run
   `admin.sql` once more so it grants `site_admin` its rights.
3. In Netlify, add `SITE_ADMIN_DATABASE_URL` (the `site_admin` connection string; the `dev` branch deploy can point at
   the Neon dev branch) and `ADMIN_EMAILS`.

Before that, or without the page, moderate with SQL in Neon as the owner and add the `audit_log` row yourself:
`UPDATE soil_samples SET flagged = TRUE, flag_reason = '...' WHERE id = ...;` to flag,
`INSERT INTO disabled_users (user_id, disabled_by, reason) VALUES (...);` to disable, and
`DELETE FROM disabled_users WHERE user_id = '...';` to re-enable.

Tests: `npm test` includes `test/admin.test.mjs` (the `ADMIN_EMAILS` check, refusal before the database, request
checks). From the repo root, `Rscript site/test/db/admin_functions_test.R` loads `admin.sql` inside a transaction
that is rolled back and checks every function as a role with `site_admin`'s rights, so it works on a branch where
`admin.sql` hasn't been run yet.

## My Garden (signed-in wildlife dashboard)

`/my-garden/` (linked from `/account/`) is the Shiny app's My Garden tab on the site: which moths, butterflies,
specialist bees and birds the plants in a person's soil tests can support. It shows a ring per group, a meter per
family (with the person's plants that support it and up to 3 plant genera to add), their plants sorted into native,
introduced and invasive for their home state, and up to 10 recommended plant genera native to that state per plant type.

- `netlify/functions/garden.mjs` (`GET /api/garden`) checks the Firebase token, then calls `site_api.my_garden`
  (`sql/garden.sql`) as `site_writer`. Its helpers live in a `garden_private` schema that `site_writer` can't use.
- Same exposure as the app gave signed-in people: counts per group and family, family names, the person's own plant
  names and plant genera. Never wildlife species names, ids, interaction lists or sources (the wildlife tables are
  proprietary). The database test checks the output against every wildlife name and id for the state.
- Same rules as the app: invasive plants (federal, or listed in the home state) count for nothing; native plants and
  plants with no record in the state count at genus level; introduced plants count only wildlife recorded on that
  exact species and aren't named under families. Totals count only wildlife recorded in the home state when it has
  occurrence records for 50 or more species (GBIF, eBird); otherwise every species in the data.
- Small changes from the app: "both" (native and introduced in the state) counts as native, as on species pages;
  "+N wildlife species" on a recommendation counts only species the garden doesn't support yet; the genera to add
  for each family are natives of the home state, like the recommendations.
- Setup: run `sql/garden.sql` as the database owner after `sql/contribute.sql`, on the Neon dev branch, then main
  (before merging the page to `main`). Safe to re-run.
- Tests: `test/garden.test.mjs` (in `npm test`) and, from the repo root, `Rscript site/test/db/garden_functions_test.R`.
- Local preview without signing in: in `npm run dev` only, the page has `window.__gardenPreview(data)`, which draws
  a saved `site_api.my_garden` result (keep such files in the gitignored `src/data/generated/`).

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
7. Contributing: run `sql/contribute.sql` and `sql/export.sql`, then `Rscript scripts/load_site_reference.R` from the repo root (species
   list, zip codes, ecoregion grid, app accounts). Create the `site_writer` role (bottom of `contribute.sql`) and add
   `SITE_WRITER_DATABASE_URL` in Netlify (branch deploys can point at a Neon dev branch). Add
   `PUBLIC_FIREBASE_API_KEY`, `PUBLIC_FIREBASE_AUTH_DOMAIN` and `PUBLIC_FIREBASE_PROJECT_ID` (the app's
   `FIREBASE_*` values; public by design). In Firebase > Authentication > Settings > Authorized domains, add
   `edaphicflora.org`, `www.edaphicflora.org` and `dev--edaphicflora.netlify.app`.
8. Moderation: run `sql/admin.sql`, create the `site_admin` login, add yourself to `site_admins`, and add
   `SITE_ADMIN_DATABASE_URL` and `ADMIN_EMAILS` in Netlify (details under "Moderating" above).

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
- **Home page and banner photos:** species slugs in `src/lib/sitePhotos.js` (home mosaic, feature cards, call-to-action band, About banner, 404). Each list falls back to the next slug if a species drops out.
- **Where app buttons go:** `APP_URL` in `src/config.js`.
- **About page, credits:** `src/pages/about.astro`.
- **Thresholds:** `public_site.settings` in `sql/public_views.sql` (re-run the file after changing).

To regenerate the sample file from a test database: `EXPORT_LABEL=sample EXPORT_OUT=src/data/sample/species.json npm run export-data`.
