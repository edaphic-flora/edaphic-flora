# Load the reference data the public site's contribute form needs, and the
# Polished account list used to link existing contributors on first sign-in.
#
# Run from the repo root after site/sql/contribute.sql:
#   Rscript scripts/load_site_reference.R
#
# Uses the database owner login and POLISHED_API_KEY from .Renviron. Safe to
# re-run: reference tables are replaced, legacy_users is upserted. Prints
# counts only, never emails.

readRenviron(".Renviron")
stopifnot(file.exists("app/species_accepted.csv"),
          file.exists("app/data/zipcode_lookup.csv"),
          file.exists("app/data/ecoregion_grid.csv"))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"), port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
  dbname = Sys.getenv("POSTGRES_DB"), user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"), sslmode = "require"
)
on.exit(DBI::dbDisconnect(con), add = TRUE)
cat("Database host:", strsplit(Sys.getenv("POSTGRES_HOST"), ".", fixed = TRUE)[[1]][1], "\n")

need <- c("ref_species_accepted", "ref_zipcodes", "ref_ecoregion_grid", "legacy_users")
have <- DBI::dbGetQuery(con, "SELECT tablename FROM pg_tables WHERE schemaname = 'public'")$tablename
if (length(setdiff(need, have))) stop("Run site/sql/contribute.sql first. Missing: ", paste(setdiff(need, have), collapse = ", "))

# ---- read the app's lookup files ---------------------------------------------
species <- read.csv("app/species_accepted.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
species <- unique(data.frame(taxon_name = trimws(species$taxon_name), family = species$family,
                             stringsAsFactors = FALSE))
species <- species[nzchar(species$taxon_name) & !duplicated(species$taxon_name), ]
species$common_name <- NA_character_

zips <- read.csv("app/data/zipcode_lookup.csv", stringsAsFactors = FALSE, colClasses = c(zipcode = "character"))
zips <- zips[grepl("^[0-9]{5}$", zips$zipcode) & !duplicated(zips$zipcode),
             c("zipcode", "city", "state", "latitude", "longitude")]

grid <- read.csv("app/data/ecoregion_grid.csv", stringsAsFactors = FALSE)
grid$lat <- round(grid$lat, 1)
grid$lon <- round(grid$lon, 1)
grid <- grid[!duplicated(grid[, c("lat", "lon")]), c("lat", "lon", "ecoregion_name", "ecoregion_code")]

# ---- replace reference tables in one transaction -------------------------------
DBI::dbWithTransaction(con, {
  DBI::dbExecute(con, "TRUNCATE ref_species_accepted, ref_zipcodes, ref_ecoregion_grid")
  DBI::dbWriteTable(con, "ref_species_accepted", species, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "ref_zipcodes", zips, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "ref_ecoregion_grid", grid, append = TRUE, row.names = FALSE)

  # Common names from USDA PLANTS, matched on genus + epithet the same way the
  # app builds its "Common Name (Scientific)" labels (load_common_name_index in
  # app/R/data.R), using the site's hybrid-aware species key.
  DBI::dbExecute(con, "
    WITH cn AS (
      SELECT DISTINCT ON (k) k, common_name
      FROM (
        SELECT public_site.species_key(t.scientific_name) AS k, btrim(r.common_name) AS common_name,
               CASE WHEN t.scientific_name !~ ' (var|ssp|subsp|f)\\.? ' THEN 0 ELSE 1 END AS rank_order,
               t.scientific_name
        FROM ref_taxon t
        JOIN ref_usda_traits r ON r.taxon_id = t.id
        WHERE r.common_name IS NOT NULL AND btrim(r.common_name) <> ''
      ) s
      ORDER BY k, rank_order, scientific_name
    )
    UPDATE ref_species_accepted a
       SET common_name = cn.common_name
      FROM cn
     WHERE cn.k = public_site.species_key(a.taxon_name)")
})

counts <- DBI::dbGetQuery(con, "
  SELECT (SELECT count(*) FROM ref_species_accepted)::int AS species,
         (SELECT count(*) FROM ref_species_accepted WHERE common_name IS NOT NULL)::int AS with_common_name,
         (SELECT count(*) FROM ref_zipcodes)::int AS zipcodes,
         (SELECT count(*) FROM ref_ecoregion_grid)::int AS grid_cells,
         (SELECT count(*) FROM soil_samples s WHERE NOT EXISTS
            (SELECT 1 FROM ref_species_accepted a WHERE a.taxon_name = s.species))::int AS samples_with_unlisted_species")
print(counts)

# ---- Polished accounts -> legacy_users -----------------------------------------
key <- Sys.getenv("POLISHED_API_KEY")
if (!nzchar(key)) {
  cat("POLISHED_API_KEY not set; skipping legacy_users.\n")
} else {
  res <- polished::get_users(api_key = key)
  users <- res$content
  if (is.null(users) || !nrow(users)) stop("Polished returned no users.")
  stopifnot(all(c("uid", "email") %in% names(users)))
  legacy <- data.frame(email_lower = tolower(trimws(users$email)), polished_uid = users$uid,
                       stringsAsFactors = FALSE)
  legacy <- legacy[!is.na(legacy$email_lower) & nzchar(legacy$email_lower) & !duplicated(legacy$email_lower), ]
  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "tmp_legacy_users", legacy, temporary = TRUE, overwrite = TRUE, row.names = FALSE)
    DBI::dbExecute(con, "
      INSERT INTO legacy_users (email_lower, polished_uid)
      SELECT email_lower, polished_uid FROM tmp_legacy_users
      ON CONFLICT (email_lower) DO UPDATE SET polished_uid = EXCLUDED.polished_uid")
  })
  linked <- DBI::dbGetQuery(con, "
    SELECT (SELECT count(*) FROM legacy_users)::int AS legacy_users,
           (SELECT count(DISTINCT s.created_by) FROM soil_samples s
             WHERE s.created_by IN (SELECT polished_uid FROM legacy_users))::int AS contributors_matched,
           (SELECT count(DISTINCT created_by) FROM soil_samples)::int AS contributors_total")
  print(linked)
}

cat("Database size:", DBI::dbGetQuery(con, "SELECT pg_size_pretty(pg_database_size(current_database())) AS s")$s, "\n")
