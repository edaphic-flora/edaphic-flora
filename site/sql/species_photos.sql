-- =============================================================================
-- Edaphic Flora: species photos
-- =============================================================================
-- One openly licensed photo per species, found by scripts/fetch-photos.mjs
-- (iNaturalist first, Wikimedia Commons as a fallback). Images are hotlinked
-- from iNaturalist and Wikimedia, so only URLs and credits are stored here.
--
-- Run once on the dev branch, then on main, as the database owner (the same
-- role that ran public_views.sql). Safe to re-run.
-- =============================================================================

BEGIN;

CREATE TABLE IF NOT EXISTS ref_species_photos (
  species_key   TEXT PRIMARY KEY,           -- lower('Genus epithet'), same key as public_site views
  source        TEXT NOT NULL CHECK (source IN ('inaturalist', 'wikimedia', 'manual', 'none')),
  photo_id      TEXT,
  page_url      TEXT,                        -- photo page (credit link)
  image_url     TEXT,                        -- about 500 px, for cards
  large_url     TEXT,                        -- about 1000 px, for species page headers
  width         INTEGER,                     -- original size, used for aspect ratio
  height        INTEGER,
  license       TEXT,                        -- cc0, pd, cc-by, cc-by-sa, cc-by-nc, cc-by-nc-sa
  creator       TEXT,
  attribution   TEXT,
  inat_taxon_id INTEGER,
  matched_name  TEXT,                        -- name the source matched (accepted name if ours is a synonym)
  tier          SMALLINT,                    -- priority tier when fetched (1 = has a soil snapshot)
  locked        BOOLEAN NOT NULL DEFAULT FALSE, -- TRUE = hand-picked, the fetch script never overwrites it
  checked_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_species_photos_source ON ref_species_photos (source);

-- What the site build reads (rows with no photo are left out)
CREATE OR REPLACE VIEW public_site.species_photos AS
SELECT species_key, source, page_url, image_url, large_url, width, height, license, creator, attribution
FROM ref_species_photos
WHERE source <> 'none' AND image_url IS NOT NULL;

-- Let the site's read-only login see the new view, if that role exists
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'site_reader') THEN
    GRANT SELECT ON public_site.species_photos TO site_reader;
  END IF;
END $$;

COMMIT;

-- =============================================================================
-- OPTIONAL (needed for the weekly GitHub Action): a login that can only
-- update photos. Create it with SQL, not in the Neon Console.
--
--   CREATE ROLE photo_loader WITH LOGIN PASSWORD 'replace-with-a-long-random-password';
--   GRANT USAGE  ON SCHEMA public_site TO photo_loader;
--   GRANT SELECT ON ALL TABLES IN SCHEMA public_site TO photo_loader;
--   GRANT SELECT, INSERT, UPDATE ON ref_species_photos TO photo_loader;
--
-- Then add its connection string to GitHub as the PHOTOS_DATABASE_URL secret:
--   postgresql://photo_loader:<password>@<your-neon-host>/<db>?sslmode=require
--
-- To hand-pick a photo (for example one of your own) and keep the script
-- from replacing it:
--   INSERT INTO ref_species_photos
--     (species_key, source, image_url, large_url, page_url, width, height,
--      creator, license, attribution, locked)
--   VALUES ('asclepias tuberosa', 'manual', '<500px url>', '<1000px url>', '<page url>',
--      1600, 1200, 'Todd Testerman', 'cc-by', 'Todd Testerman, CC BY', TRUE)
--   ON CONFLICT (species_key) DO UPDATE SET
--     source = EXCLUDED.source, image_url = EXCLUDED.image_url, large_url = EXCLUDED.large_url,
--     page_url = EXCLUDED.page_url, width = EXCLUDED.width, height = EXCLUDED.height,
--     creator = EXCLUDED.creator, license = EXCLUDED.license, attribution = EXCLUDED.attribution,
--     locked = TRUE, updated_at = now();
-- =============================================================================
