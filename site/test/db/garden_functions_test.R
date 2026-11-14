# Exercises site_api.my_garden (site/sql/garden.sql) as a throwaway role with
# exactly site_writer's rights, inside one transaction that is ALWAYS rolled
# back: nothing is kept, on any branch.
#
#   Rscript site/test/db/garden_functions_test.R
#
# Uses the owner login from .Renviron. Run garden.sql (and contribute.sql)
# on that database first. Prints pass/fail lines only (no emails).

readRenviron(".Renviron")
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"), port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
  dbname = Sys.getenv("POSTGRES_DB"), user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"), sslmode = "require"
)
q <- function(sql, ...) {
  p <- list(...)
  if (length(p)) DBI::dbGetQuery(con, sql, params = p) else DBI::dbGetQuery(con, sql)
}
x <- function(sql) invisible(DBI::dbExecute(con, sql))
fails <- 0
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS" else { fails <<- fails + 1; "FAIL" }, label, "\n")
}
denied <- function(sql) {
  x("SAVEPOINT d")
  out <- tryCatch({ q(sql); FALSE }, error = function(e) grepl("permission denied", conditionMessage(e)))
  x("ROLLBACK TO SAVEPOINT d")
  out
}
garden <- function(uid) {
  jsonlite::fromJSON(q("SELECT site_api.my_garden($1)::text AS g", uid)$g, simplifyVector = FALSE)
}
garden_text <- function(uid) q("SELECT site_api.my_garden($1)::text AS g", uid)$g
submit <- function(uid, plants) {
  r <- q("SELECT site_api.submit_samples($1, $2::jsonb, $3::jsonb, $4::jsonb, $5::date, $6, $7)::text AS r",
         uid, '{"ph": 6.4, "organic_matter": 5.1}',
         as.character(jsonlite::toJSON(lapply(plants, function(s) list(species = s, outcome = "Thriving")), auto_unbox = TRUE)),
         '{"zip": "06511"}', "2026-06-01", "test", TRUE)
  jsonlite::fromJSON(r$r)$status
}
prefs <- function(uid, zip, state) {
  jsonlite::fromJSON(q("SELECT site_api.save_prefs($1, $2, $3)::text AS r", uid, zip, state)$r)$status
}
plant <- function(g, sp) Filter(function(p) p$species == sp, g$plants)[[1]]
group <- function(g, type) Filter(function(x) x$type == type, g$groups)[[1]]
family_plants <- function(g) unique(unlist(lapply(g$groups, function(x) lapply(x$families, function(f) unlist(f$plants)))))

# Native (genus credit), introduced with congeners in the wildlife tables
# (species credit only, so none), invasive with congeners (no credit at all)
NATIVE <- c("Quercus alba", "Asclepias tuberosa", "Prunus serotina")
INTRODUCED <- "Acer palmatum"
INVASIVE <- "Acer platanoides"

DBI::dbBegin(con)
tryCatch({
  x("CREATE ROLE tmp_garden_writer_test NOLOGIN")
  x("GRANT USAGE ON SCHEMA site_api TO tmp_garden_writer_test")
  x("GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA site_api TO tmp_garden_writer_test")
  x("GRANT tmp_garden_writer_test TO CURRENT_USER")
  x("SET ROLE tmp_garden_writer_test")

  check("writer cannot use the garden helpers", denied("SELECT * FROM garden_private.classify(ARRAY['Quercus alba'], 'CT')"))
  check("writer cannot read wildlife species", denied("SELECT count(*) FROM ref_wildlife_species"))
  check("writer cannot read wildlife interactions", denied("SELECT count(*) FROM ref_wildlife_interactions"))
  check("unknown user gets nothing", garden("not-a-site-user")$status == "unknown_user")

  u1 <- q("SELECT user_id FROM site_api.resolve_user('test-garden-1', 'nobody-g1@example.invalid')")$user_id
  u2 <- q("SELECT user_id FROM site_api.resolve_user('test-garden-2', 'nobody-g2@example.invalid')")$user_id
  u3 <- q("SELECT user_id FROM site_api.resolve_user('test-garden-3', 'nobody-g3@example.invalid')")$user_id

  g2 <- garden(u2)
  check("no samples: ok, empty garden", g2$status == "ok" && length(g2$plants) == 0 && length(g2$groups) == 0 && g2$supported == 0)

  check("test plants saved", submit(u1, c(NATIVE, INTRODUCED, INVASIVE)) == "ok" && submit(u3, c("Solidago rugosa", "Acer rubrum")) == "ok")
  check("home zip saved (CT)", prefs(u1, "06511", NA_character_) == "ok")

  t0 <- Sys.time()
  g1 <- garden(u1)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  check(sprintf("garden loads (%.1f s)", secs), g1$status == "ok" && secs < 10)
  check("home state is CT, wildlife filtered to CT", g1$state == "CT" && isTRUE(g1$state_filtered) && g1$city == "New Haven")
  check("all five plants listed, own plants only",
        setequal(vapply(g1$plants, `[[`, "", "species"), c(NATIVE, INTRODUCED, INVASIVE)))
  check("natives are native", all(vapply(NATIVE, function(s) plant(g1, s)$status == "native", TRUE)))
  check("Japanese maple is introduced", plant(g1, INTRODUCED)$status == "introduced")
  check("Norway maple is invasive in CT, with its designation",
        plant(g1, INVASIVE)$status == "invasive" && nzchar(plant(g1, INVASIVE)$designation))
  check("introduced plants get species-level counts, natives none",
        is.numeric(plant(g1, INTRODUCED)$lep) && is.null(plant(g1, "Quercus alba")$lep))
  check("common names come along", identical(tolower(plant(g1, "Quercus alba")$common_name), "white oak"))

  x("RESET ROLE")
  # Totals: every wildlife species recorded in CT, per group
  totals <- q("SELECT ws.wildlife_type AS t, count(DISTINCT ws.wildlife_id)::int AS n
                 FROM ref_wildlife_species ws
                WHERE EXISTS (SELECT 1 FROM ref_wildlife_state_presence p WHERE p.wildlife_id = ws.wildlife_id AND p.state_code = 'CT')
                GROUP BY 1")
  # Covered, worked out independently: natives by genus, nothing else (no
  # wildlife plant record is Japanese maple, and Norway maple is invasive)
  covered <- q("SELECT ws.wildlife_type AS t, count(DISTINCT ws.wildlife_id)::int AS n
                  FROM ref_wildlife_plants wp
                  JOIN ref_taxon t ON t.id = wp.taxon_id
                  JOIN ref_wildlife_interactions wi ON wi.plant_species_code = wp.species_code
                  JOIN ref_wildlife_species ws ON ws.wildlife_id = wi.wildlife_id
                 WHERE lower(split_part(t.scientific_name, ' ', 1)) IN ('quercus', 'asclepias', 'prunus')
                   AND EXISTS (SELECT 1 FROM ref_wildlife_state_presence p WHERE p.wildlife_id = ws.wildlife_id AND p.state_code = 'CT')
                 GROUP BY 1")
  acer_any <- q("SELECT count(*)::int AS n FROM ref_wildlife_plants wp JOIN ref_taxon t ON t.id = wp.taxon_id
                  WHERE split_part(t.scientific_name, ' ', 1) = 'Acer'")$n
  maple_names <- q("SELECT DISTINCT ws.common_name, ws.scientific_name, ws.wildlife_id
                      FROM ref_wildlife_species ws
                      JOIN ref_wildlife_state_presence p ON p.wildlife_id = ws.wildlife_id AND p.state_code = 'CT'")
  x("SET ROLE tmp_garden_writer_test")

  for (t in c("Butterfly", "Moth", "Bee", "Bird")) {
    gr <- group(g1, t)
    want_total <- totals$n[totals$t == t]
    want_cov <- if (any(covered$t == t)) covered$n[covered$t == t] else 0L
    check(sprintf("%s: %d of %d supported, matches a direct count", t, gr$covered, gr$total),
          gr$total == want_total && gr$covered == want_cov)
    check(sprintf("%s: families add up", t),
          sum(vapply(gr$families, `[[`, 0, "covered")) == gr$covered &&
            sum(vapply(gr$families, `[[`, 0, "total")) == gr$total)
  }
  check("supported = all groups together", g1$supported == sum(vapply(g1$groups, `[[`, 0, "covered")))
  check("maples have wildlife records, so the test means something", acer_any > 0)
  fp <- family_plants(g1)
  check("family plant lists name natives only", all(fp %in% NATIVE) && length(fp) > 0)
  check("invasive and introduced plants are not named under families", !any(c(INTRODUCED, INVASIVE) %in% fp))

  fam1 <- group(g1, "Moth")$families[[1]]
  check("each family carries only counts, plants and genera",
        setequal(names(fam1), c("family", "covered", "total", "plants", "add")))
  check("top level has only the documented keys",
        setequal(names(g1), c("status", "state", "city", "state_filtered", "plants", "supported", "groups", "recs", "genus_names")))

  strings <- q("SELECT DISTINCT v #>> '{}' AS s FROM jsonb_path_query(site_api.my_garden($1), 'strict $.**') v
                 WHERE jsonb_typeof(v) = 'string'", u1)$s
  leaked <- intersect(strings, c(maple_names$common_name, maple_names$scientific_name, maple_names$wildlife_id))
  check(sprintf("no wildlife species names or ids in the output (%d strings checked)", length(strings)), length(leaked) == 0)

  recs <- g1$recs
  check("recommendations by plant type, always including All",
        "All" %in% names(recs) && all(names(recs) %in% c("All", "Tree", "Shrub", "Perennial", "Grass/Sedge", "Vine", "Fern")))
  all_recs <- recs$All
  rec_genera <- vapply(all_recs, `[[`, "", "genus")
  check("at most 10, sorted by wildlife added", length(all_recs) <= 10 && length(all_recs) > 0 &&
          !is.unsorted(rev(vapply(all_recs, `[[`, 0, "total"))))
  check("never recommends a genus already in the garden", !any(c("Quercus", "Asclepias", "Prunus") %in% rec_genera))
  check("every recommended genus lists species native to CT", all(vapply(all_recs, function(r) r$native_count > 0 && length(r$natives) > 0, TRUE)))
  check("tree recommendations are trees", all(vapply(recs$Tree, function(r) r$life_form == "Tree", TRUE)))
  check("group counts add up on recommendations", all(vapply(all_recs, function(r) r$lep + r$bee + r$bird == r$total, TRUE)))
  check("family gap genera are not in the garden either",
        !any(c("Quercus", "Asclepias", "Prunus") %in% unlist(lapply(g1$groups, function(x) lapply(x$families, function(f) unlist(f$add))))))
  check("genus names: oak for Quercus", identical(g1$genus_names$Quercus, "oak"))

  # Someone with no home state: national totals, no recommendations
  g3 <- garden(u3)
  x("RESET ROLE")
  national <- q("SELECT count(DISTINCT wildlife_id)::int AS n FROM ref_wildlife_species WHERE wildlife_type = 'Moth'")$n
  hi_rows <- q("SELECT count(*)::int AS n FROM ref_wildlife_state_presence WHERE state_code = 'HI'")$n
  x("SET ROLE tmp_garden_writer_test")
  check("no home state: not filtered, national totals", is.null(g3$state) && !isTRUE(g3$state_filtered) && group(g3, "Moth")$total == national)
  check("no home state: no recommendations", length(g3$recs) == 0)
  check("other people's plants stay out", !any(c("Solidago rugosa", "Acer rubrum") %in% vapply(g1$plants, `[[`, "", "species")))
  check("red maple is native and supports wildlife", plant(g3, "Acer rubrum")$status == "native" &&
          "Acer rubrum" %in% family_plants(g3))
  if (hi_rows < 50) {
    prefs(u3, NA_character_, "HI")
    g3h <- garden(u3)
    check("a state with little occurrence data is not filtered", g3h$state == "HI" && !isTRUE(g3h$state_filtered))
  }

  x("RESET ROLE")
  check("site_reader cannot call my_garden", !q("SELECT has_function_privilege('site_reader', 'site_api.my_garden(text)', 'EXECUTE') AS ok")$ok)
  check("PUBLIC cannot call the helpers", !q("SELECT has_schema_privilege('public', 'garden_private', 'USAGE') AS ok")$ok)
}, error = function(e) {
  fails <<- fails + 1
  cat("ERROR", conditionMessage(e), "\n")
}, finally = {
  DBI::dbRollback(con)
  DBI::dbDisconnect(con)
  cat("rolled back;", if (fails == 0) "all checks passed\n" else paste(fails, "failed\n"))
})
