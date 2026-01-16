# SPDX-License-Identifier: MIT
# R/etl/usda_traits_etl.R — Phase 1: USDA Traits (Duration / Growth Habit / Native / Invasive)

suppressPackageStartupMessages({
  library(DBI); library(RPostgres)
  library(readr); library(readxl)
  library(dplyr); library(stringr)
})

# ---- Configuration -----------------------------------------------------------
usda_traits_default <- Sys.getenv("USDA_TRAITS_LOCAL", "data/raw/usda/usda_traits.csv")

# ---- Helpers ----------------------------------------------------------------
normalize_names <- function(v) tolower(gsub("[^a-z0-9]+","_", trimws(v)))

# Return the ORIGINAL header name (not the normalized token)
pick_orig <- function(cands, nms_norm, names_orig) {
  cn <- normalize_names(cands)
  # exact normalized match
  hit <- which(nms_norm %in% cn)
  if (length(hit)) return(names_orig[hit[1]])
  # partial normalized grep
  g <- grep(paste(cn, collapse = "|"), nms_norm, fixed = TRUE)
  if (length(g)) return(names_orig[g[1]])
  NA_character_
}

read_traits_table <- function(path) {
  stopifnot(is.character(path), length(path) == 1)
  if (!nzchar(path) || !file.exists(path)) stop("Traits file not found: ", path)

  # Read CSV/XLSX with simple delimiter detection for delimited text files
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx","xls")) {
    x <- readxl::read_excel(path)
  } else {
    first  <- readLines(path, n = 1, warn = FALSE)
    delims <- c("," = ",", "\t" = "\t", ";" = ";", "|" = "|")
    best   <- names(which.max(sapply(delims, function(d) length(strsplit(first, d, fixed = TRUE)[[1]]))))
    x <- readr::read_delim(path, delim = delims[[best]],
                           col_types = readr::cols(.default = readr::col_character()))
  }

  # Clean headers (trim + remove BOM if present)
  names_orig <- trimws(names(x))
  names_orig <- sub("^\uFEFF", "", names_orig, useBytes = TRUE)
  nms_norm   <- normalize_names(names_orig)

  n          <- nrow(x)
  col_or_na  <- function(nm) if (!is.na(nm)) x[[nm]] else rep(NA_character_, n)

  cols <- list(
    symbol        = pick_orig(c("Symbol","PLANTS Symbol","Plant Symbol"), nms_norm, names_orig),
    scientific    = pick_orig(c("Scientific Name with Author","Scientific Name","Sci Name"), nms_norm, names_orig),
    duration      = pick_orig(c("Duration"), nms_norm, names_orig),
    growth_habit  = pick_orig(c("Growth Habit","Habit","Growth Form"), nms_norm, names_orig),
    native_status = pick_orig(c("Native Status","Native/Introduced","Nativity","Introduced Status"), nms_norm, names_orig),
    invasive_flag = pick_orig(c("Invasive","Noxious","Federal Noxious Weed","State Noxious Weed"), nms_norm, names_orig)
  )

  if (is.na(cols$symbol) && is.na(cols$scientific))
    stop("Could not find a Symbol or Scientific Name column in: ", path)

  df <- tibble::tibble(
    usda_symbol     = col_or_na(cols$symbol),
    scientific_name = col_or_na(cols$scientific),
    duration        = col_or_na(cols$duration),
    growth_habit    = col_or_na(cols$growth_habit),
    native_status   = col_or_na(cols$native_status),
    invasive_status = col_or_na(cols$invasive_flag)
  ) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), NA_character_, trimws(as.character(.x)))))

  # normalize invasive_status to Yes/No/NA
  yes_tokens <- c("yes","y","1","true","present","listed")
  no_tokens  <- c("no","n","0","false","not","absent")
  df <- df |>
    mutate(invasive_status = case_when(
      is.na(invasive_status) ~ NA_character_,
      str_to_lower(invasive_status) %in% yes_tokens ~ "Yes",
      str_to_lower(invasive_status) %in% no_tokens  ~ "No",
      TRUE ~ invasive_status
    ))

  # normalize native_status to a small set if obvious
  native_yes   <- c("native","n","y","yes")
  native_no    <- c("introduced","exotic","non-native","i","no")
  native_both  <- c("both","native and introduced","mixed")
  df <- df |>
    mutate(native_status = case_when(
      is.na(native_status) ~ NA_character_,
      str_to_lower(native_status) %in% native_yes  ~ "Native",
      str_to_lower(native_status) %in% native_no   ~ "Introduced",
      str_to_lower(native_status) %in% native_both ~ "Both",
      TRUE ~ native_status
    ))

  # canonicalize scientific name to "Genus species" for fallback matching (row-wise)
  df <- df |>
    mutate(
      scientific_gs = {
        s <- str_squish(scientific_name)
        toks <- strsplit(ifelse(is.na(s), "", s), "\\s+")
        vapply(
          toks,
          function(tt) {
            if (length(tt) >= 2) paste(tt[1], tt[2])
            else if (length(tt) == 1) tt[1]
            else NA_character_
          },
          FUN.VALUE = character(1)
        )
      }
    )

  # drop rows where ALL traits are empty (so a blank template doesn't load anything)
  nz <- function(x) !is.na(x) & nzchar(x)
  df <- df |>
    filter(nz(duration) | nz(growth_habit) | nz(native_status) | nz(invasive_status)) |>
    filter(nz(usda_symbol) | nz(scientific_gs))

  df
}

# Upsert/ensure a single source row WITHOUT requiring a unique constraint on name
upsert_source <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_source (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      version TEXT,
      url TEXT,
      license TEXT
    );
  ")

  # Try to find an existing row by name
  existing <- DBI::dbGetQuery(con, "
    SELECT id FROM ref_source WHERE name = $1 ORDER BY id LIMIT 1;
  ", params = list("USDA PLANTS — Characteristics (curated)"))

  if (nrow(existing)) {
    sid <- existing$id[1]
    # keep terms up to date
    DBI::dbExecute(con, "
      UPDATE ref_source
         SET version = $2, url = $3, license = $4
       WHERE id = $1;
    ", params = list(
      sid,
      format(Sys.Date(), "%Y-%m-%d"),
      "https://plants.usda.gov/",
      "See USDA PLANTS site terms"
    ))
    return(as.integer(sid))
  } else {
    sid <- DBI::dbGetQuery(con, "
      INSERT INTO ref_source (name, version, url, license)
      VALUES ($1,$2,$3,$4)
      RETURNING id;
    ", params = list(
      "USDA PLANTS — Characteristics (curated)",
      format(Sys.Date(), "%Y-%m-%d"),
      "https://plants.usda.gov/",
      "See USDA PLANTS site terms"
    ))$id
    return(as.integer(sid[1]))
  }
}

ensure_tables <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_usda_traits (
      taxon_id INT PRIMARY KEY REFERENCES ref_taxon(id) ON DELETE CASCADE,
      usda_symbol TEXT,
      duration TEXT,
      growth_habit TEXT,
      native_status TEXT,
      invasive_status TEXT,
      source_id INT REFERENCES ref_source(id),
      updated_at TIMESTAMPTZ DEFAULT now()
    );
  ")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS ix_traits_symbol ON ref_usda_traits (usda_symbol);")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS ix_traits_habit ON ref_usda_traits (growth_habit);")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS ix_traits_native ON ref_usda_traits (native_status);")
}

# ---- Main ETL ---------------------------------------------------------------
usda_traits_etl_run <- function(traits_path = usda_traits_default) {
  message("== Edaphic Flora | USDA Traits ETL ==")
  message("Reading: ", traits_path)
  df <- read_traits_table(traits_path)
  message("Rows in input (non-empty traits): ", nrow(df))

  if (nrow(df) == 0) {
    message("No non-empty trait rows found. Nothing to load.")
    return(invisible(list(traits_rows_in = 0, traits_rows_loaded = NA_integer_, unmatched_estimate = NA_integer_)))
  }

  # Connect with admin role for DDL/UPSERT
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("POSTGRES_HOST"),
    port = as.integer(Sys.getenv("POSTGRES_PORT")),
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_ADMIN_USER"),
    password = Sys.getenv("POSTGRES_ADMIN_PASSWORD"),
    sslmode = Sys.getenv("POSTGRES_SSLMODE","require")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ensure_tables(con)
  source_id <- upsert_source(con)

  # Stage rows in a TEMP table
  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_usda_traits;")
  DBI::dbExecute(con, "
    CREATE TEMP TABLE tmp_usda_traits (
      usda_symbol TEXT,
      scientific_gs TEXT,
      duration TEXT,
      growth_habit TEXT,
      native_status TEXT,
      invasive_status TEXT
    ) ON COMMIT DROP;
  ")
  DBI::dbWriteTable(
    con, "tmp_usda_traits",
    df %>% select(usda_symbol, scientific_gs, duration, growth_habit, native_status, invasive_status),
    append = TRUE, temporary = TRUE, row.names = FALSE
  )

  # Match to ref_taxon: prefer exact USDA symbol; fallback to Genus+species text
  # Upsert into ref_usda_traits keyed by taxon_id
  DBI::dbExecute(con, "
    WITH matched AS (
      SELECT
        COALESCE(t1.id, t2.id) AS taxon_id,
        u.usda_symbol,
        u.duration,
        u.growth_habit,
        u.native_status,
        u.invasive_status
      FROM tmp_usda_traits u
      LEFT JOIN ref_taxon t1
        ON t1.usda_symbol = u.usda_symbol
        AND u.usda_symbol IS NOT NULL AND u.usda_symbol <> ''
      LEFT JOIN ref_taxon t2
        ON t1.id IS NULL
       AND lower(split_part(t2.scientific_name,' ',1) || ' ' || split_part(t2.scientific_name,' ',2))
           = lower(u.scientific_gs)
    ),
    dedup AS (
      SELECT DISTINCT ON (taxon_id)
        taxon_id, usda_symbol, duration, growth_habit, native_status, invasive_status
      FROM matched
      WHERE taxon_id IS NOT NULL
      ORDER BY taxon_id, COALESCE(NULLIF(usda_symbol,''),'~') DESC
    )
    INSERT INTO ref_usda_traits AS r
      (taxon_id, usda_symbol, duration, growth_habit, native_status, invasive_status, source_id)
    SELECT
      taxon_id, usda_symbol, duration, growth_habit, native_status, invasive_status, $1
    FROM dedup
    ON CONFLICT (taxon_id) DO UPDATE SET
      usda_symbol     = EXCLUDED.usda_symbol,
      duration        = EXCLUDED.duration,
      growth_habit    = EXCLUDED.growth_habit,
      native_status   = EXCLUDED.native_status,
      invasive_status = EXCLUDED.invasive_status,
      source_id       = EXCLUDED.source_id,
      updated_at      = now();
  ", params = list(source_id))

  # Report
  traits_rows_loaded <- DBI::dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM ref_usda_traits;")$n[1]
  unmatched_count <- DBI::dbGetQuery(con, "
    WITH matched AS (
      SELECT COALESCE(t1.id, t2.id) AS taxon_id
      FROM tmp_usda_traits u
      LEFT JOIN ref_taxon t1
        ON t1.usda_symbol = u.usda_symbol
        AND u.usda_symbol IS NOT NULL AND u.usda_symbol <> ''
      LEFT JOIN ref_taxon t2
        ON t1.id IS NULL
       AND lower(split_part(t2.scientific_name,' ',1) || ' ' || split_part(t2.scientific_name,' ',2))
           = lower(u.scientific_gs)
    )
    SELECT COUNT(*)::int AS n FROM matched WHERE taxon_id IS NULL;
  ")$n[1]

  sample_unmatched <- DBI::dbGetQuery(con, "
    WITH matched AS (
      SELECT
        u.usda_symbol, u.scientific_gs,
        COALESCE(t1.id, t2.id) AS taxon_id
      FROM tmp_usda_traits u
      LEFT JOIN ref_taxon t1
        ON t1.usda_symbol = u.usda_symbol
        AND u.usda_symbol IS NOT NULL AND u.usda_symbol <> ''
      LEFT JOIN ref_taxon t2
        ON t1.id IS NULL
       AND lower(split_part(t2.scientific_name,' ',1) || ' ' || split_part(t2.scientific_name,' ',2))
           = lower(u.scientific_gs)
    )
    SELECT usda_symbol, scientific_gs
    FROM matched
    WHERE taxon_id IS NULL
    LIMIT 10;
  ")

  message(sprintf("Loaded traits rows (total in ref_usda_traits): %d", traits_rows_loaded))
  message(sprintf("Unmatched rows this run: %d", unmatched_count))
  if (nrow(sample_unmatched)) {
    message("Sample unmatched (up to 10):")
    apply(sample_unmatched, 1, function(row)
      message(sprintf("  symbol=%s | sci=%s", row[[1]] %||% "", row[[2]] %||% ""))
    )
  }

  invisible(list(
    traits_rows_in      = nrow(df),
    traits_rows_loaded  = traits_rows_loaded,
    unmatched_estimate  = unmatched_count
  ))
}

# small infix helper (safe default)
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || (is.character(a) && !nzchar(a))) b else a
