# R/etl/usda_ref_etl.R
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# ---------- helpers ----------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
msg <- function(...) cat(sprintf(...), "\n")

ensure_dirs <- function(paths) {
  for (p in paths) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

pg_connect_admin <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("POSTGRES_HOST"),
    port     = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
    dbname   = Sys.getenv("POSTGRES_DB"),
    user     = Sys.getenv("POSTGRES_ADMIN_USER"),
    password = Sys.getenv("POSTGRES_ADMIN_PASSWORD"),
    sslmode  = Sys.getenv("POSTGRES_SSLMODE", "require")
  )
}

http_try_download <- function(urls, dest) {
  stopifnot(length(urls) >= 1)
  ensure_dirs(dirname(dest))
  for (u in urls) {
    ok <- FALSE
    try({
      resp <- httr::GET(u, httr::write_disk(dest, overwrite = TRUE), httr::timeout(60))
      if (!httr::http_error(resp)) {
        ct <- tolower(httr::headers(resp)[["content-type"]] %||% "")
        if (grepl("officedocument|excel|spreadsheet|application/vnd.openxmlformats-officedocument", ct)) ok <- TRUE
        if (!ok && grepl("\\.xlsx$", u, ignore.case = TRUE)) {
          ok <- file.exists(dest) && (file.size(dest) > 5 * 1024)
        }
      }
    }, silent = TRUE)
    if (ok) return(invisible(TRUE))
  }
  stop("All download sources failed for: ", paste(urls, collapse = " | "))
}

# Canonicalize scientific names (strip authors; keep genus, species, and infraspecific ranks)
canonicalize_scientific <- function(x) {
  if (length(x) == 0) return(character())
  x <- tolower(trimws(x))
  x <- gsub("[,;]", " ", x)         # soft cleanup
  x <- gsub("\\s+", " ", x)
  ranks <- c("subsp", "ssp", "var", "subvar", "f", "forma")
  vapply(strsplit(x, "\\s+"), function(tok) {
    keep <- character(0)
    for (i in seq_along(tok)) {
      t <- tok[[i]]
      if (i == 1L) { keep <- c(keep, t); next }                 # genus (lowercased)
      if (t %in% c("×","x")) { keep <- c(keep, "x"); next }     # hybrid marker
      if (t %in% ranks) { keep <- c(keep, t); next }            # infraspecific rank keyword
      # stop at author tokens (capitalized/initials/parentheses, etc.)
      if (grepl("^[a-z][a-z\\-]*$", t)) { keep <- c(keep, t); next }
      break
    }
    paste(keep, collapse = " ")
  }, character(1))
}

read_usda_file <- function(path) {
  # Canonical headers expected downstream:
  # "Symbol", "Synonym Symbol", "Scientific Name with Authors", "National Common Name", "Family"
  if (!file.exists(path)) stop("USDA file not found at: ", path)
  first <- readLines(path, n = 1, warn = FALSE)
  if (grepl("^\\s*<!doctype html>|^\\s*<html", tolower(first))) {
    stop("USDA file parsed but required columns not found.\n",
         "Have: doctype_html\n",
         "Need at least: Symbol and Scientific Name (with authors).\n\n",
         "If you fetched HTML by mistake, delete ", path, " and place the real 'plantlst.bin'\n",
         "at data/raw/usda/plantlst.bin, then re-run.\n\n",
         "See USDA PLANTS Downloads page for the checklist description.")
  }

  # Detect delimiter
  delims <- c("," = ",", "\t" = "\t", ";" = ";", "|" = "|")
  split_counts <- sapply(delims, function(d) length(strsplit(first, d, fixed = TRUE)[[1]]))
  best_delim <- names(which.max(split_counts))

  x <- suppressMessages(readr::read_delim(
    path, delim = delims[[best_delim]],
    col_types = cols(.default = col_character()),
    trim_ws = TRUE, quote = "\""
  ))

  # Header normalize (common variants)
  hdr <- names(x)
  hdr <- sub("^Scientific Name with Author$",  "Scientific Name with Authors", hdr)
  hdr <- sub("^Common Name$",                 "National Common Name",         hdr)
  names(x) <- hdr

  have <- tolower(gsub("\\s+", " ", names(x)))
  if (!("symbol" %in% have && "scientific name with authors" %in% have)) {
    # Tolerant mapping if weird headers
    nrm <- function(v) tolower(gsub("[^a-z0-9]+", "_", v))
    nms <- nrm(names(x))
    map <- list(
      symbol = which(nms %in% c("symbol", "plant_symbol", "plants_symbol"))[1] %||% NA_integer_,
      synsym = which(nms %in% c("synonym_symbol", "synonymsymbol", "synonyms", "accepted_symbol"))[1] %||% NA_integer_,
      sci    = which(nms %in% c("scientific_name_with_authors", "scientific_name_with_author", "scientific_name"))[1] %||% NA_integer_,
      common = which(nms %in% c("national_common_name", "common_name"))[1] %||% NA_integer_,
      family = which(nms %in% c("family", "family_name"))[1] %||% NA_integer_
    )
    if (is.na(map$symbol) || is.na(map$sci)) {
      stop("USDA file parsed but required columns not found.\n",
           "Have: ", paste(nms, collapse = ", "), "\n",
           "Need at least: Symbol and Scientific Name (with authors).")
    }
    x <- tibble::tibble(
      Symbol = x[[ map$symbol ]],
      `Synonym Symbol` = if (!is.na(map$synsym)) x[[ map$synsym ]] else NA_character_,
      `Scientific Name with Authors` = x[[ map$sci ]],
      `National Common Name` = if (!is.na(map$common)) x[[ map$common ]] else NA_character_,
      Family = if (!is.na(map$family)) x[[ map$family ]] else NA_character_
    )
  } else {
    x <- x %>%
      rename(
        Symbol = any_of("Symbol"),
        `Synonym Symbol` = any_of("Synonym Symbol"),
        `Scientific Name with Authors` = any_of("Scientific Name with Authors"),
        `National Common Name` = any_of("National Common Name"),
        Family = any_of("Family")
      ) %>%
      select(Symbol, `Synonym Symbol`, `Scientific Name with Authors`, `National Common Name`, Family)
  }

  x
}

upsert_source <- function(con, name, version, url, license) {
  existing <- dbGetQuery(con, "SELECT id FROM ref_source WHERE name = $1 AND version = $2 LIMIT 1",
                         params = list(name, version))
  if (nrow(existing)) return(existing$id[[1]])
  dbGetQuery(con,
             "INSERT INTO ref_source (name, version, url, license)
              VALUES ($1, $2, $3, $4)
              ON CONFLICT (name, version) DO UPDATE SET url = EXCLUDED.url, license = EXCLUDED.license
              RETURNING id",
             params = list(name, version, url, license))$id[[1]]
}

to_blank_na <- function(x) ifelse(is.na(x), "", x)

# ---------- NWPL parsing ----------

valid_indicators <- c("OBL","FACW+","FACW","FACW-","FAC+","FAC","FAC-",
                      "FACU+","FACU","FACU-","UPL","UPL?","NI","NI*","NA","NO","–","-","")

parse_nwpl_file <- function(path, region = "National") {
  stopifnot(file.exists(path))
  sheets <- readxl::excel_sheets(path)
  known_regions  <- c("N","National","AK","AGCP","AW","CB","EMP","GP","MW","NCNE","WMVC")
  known_regionsU <- toupper(known_regions)
  norm <- function(v) tolower(gsub("[^a-z0-9]+","_", v))

  is_indicator_values <- function(vec) {
    v <- toupper(trimws(as.character(vec))); v <- v[v != ""]
    if (!length(v)) return(FALSE)
    mean(v %in% valid_indicators) >= 0.25 && length(v) >= 30
  }

  results <- list()

  for (s in sheets) {
    df <- suppressMessages(readxl::read_excel(path, sheet = s))
    if (!nrow(df)) next

    use_df <- function(df_in) {
      if (!nrow(df_in)) return(tibble::tibble())
      nms_raw <- names(df_in)
      nms     <- norm(nms_raw)

      # Long format: has Region + Indicator columns
      reg_col <- which(toupper(nms_raw) %in% c("REGION","NWPL REGION","REGION CODE","REGION_CODE"))[1]
      ind_col <- which(grepl("indicator", nms, ignore.case = TRUE) | grepl("indicator", nms_raw, ignore.case = TRUE))[1]
      if (!is.na(reg_col) && !is.na(ind_col)) {
        sym_col <- which(nms %in% c("symbol","plant_symbol","plants_symbol"))[1]
        sci_col <- which(nms %in% c("scientific_name_with_authors","scientific_name","scientificname"))[1]
        out <- df_in %>%
          transmute(
            symbol = if (!is.na(sym_col)) .data[[ sym_col ]] else NA_character_,
            scientific_name = if (!is.na(sci_col)) .data[[ sci_col ]] else NA_character_,
            region = toupper(trimws(as.character(.data[[ reg_col ]]))),
            indicator = toupper(trimws(as.character(.data[[ ind_col ]]))))
        out <- out %>% filter(indicator %in% valid_indicators, indicator != "")
        if (nrow(out)) return(out)
      }

      # Wide format: indicator columns by name or values
      name_hits <- which(toupper(nms_raw) %in% known_regionsU |
                         grepl("^\\s*National\\b", nms_raw, ignore.case = TRUE))
      value_hits <- which(vapply(df_in, is_indicator_values, logical(1)))
      ind_idx <- sort(unique(c(name_hits, value_hits)))
      if (!length(ind_idx)) return(tibble::tibble())

      sym_col <- which(nms %in% c("symbol","plant_symbol","plants_symbol"))[1]
      if (is.na(sym_col)) sym_col <- which(toupper(nms_raw) == "SYMBOL")[1]
      sci_col <- which(nms %in% c("scientific_name_with_authors","scientific_name","scientificname"))[1]
      if (is.na(sci_col)) sci_col <- which(grepl("^\\s*Scientific", nms_raw, ignore.case = TRUE))[1]

      keep <- c(if (!is.na(sym_col)) sym_col, if (!is.na(sci_col)) sci_col, ind_idx)
      df2  <- df_in[, keep, drop = FALSE]

      out <- df2
      if (!is.na(sym_col)) names(out)[names(out) == names(df2)[1]] <- ".symbol"
      if (!is.na(sci_col)) {
        nm_sci <- if (is.na(sym_col)) names(df2)[1] else names(df2)[2]
        names(out)[names(out) == nm_sci] <- ".sci"
      }

      ind_cols <- setdiff(names(out), c(".symbol",".sci"))
      long <- tidyr::pivot_longer(out, cols = all_of(ind_cols),
                                  names_to = "region_raw", values_to = "indicator")

      reg_norm <- function(x) {
        xr <- trimws(as.character(x))
        xrU <- toupper(xr)
        xrU[xrU %in% c("N","NATIONAL","NAT","NATIONAL INDICATOR","NATIONAL STATUS")] <- "NATIONAL"
        xrU
      }

      long <- long %>%
        mutate(
          region = reg_norm(region_raw),
          indicator = toupper(trimws(as.character(indicator)))
        ) %>%
        filter(indicator %in% valid_indicators, indicator != "") %>%
        transmute(
          symbol = if (".symbol" %in% names(long)) .data[[".symbol"]] else NA_character_,
          scientific_name = if (".sci" %in% names(long)) .data[[".sci"]] else NA_character_,
          region = ifelse(region == "NATIONAL", "National", region),
          indicator = indicator
        )

      long
    }

    res1 <- use_df(df)
    if (nrow(res1)) { results[[s]] <- res1; next }

    df0 <- suppressMessages(readxl::read_excel(path, sheet = s, col_names = FALSE))
    top <- min(30, nrow(df0))
    header_row <- NA_integer_
    for (i in seq_len(top)) {
      cells <- as.character(unlist(df0[i, ], use.names = FALSE))
      cells <- cells[!is.na(cells)]
      if (!length(cells)) next
      if (any(grepl("^\\s*Symbol\\s*$", cells, ignore.case = TRUE)) ||
          any(toupper(cells) %in% known_regionsU) ||
          any(grepl("Indicator", cells, ignore.case = TRUE))) { header_row <- i; break }
    }
    if (!is.na(header_row)) {
      df2 <- suppressMessages(readxl::read_excel(path, sheet = s, skip = header_row - 1))
      res2 <- use_df(df2)
      if (nrow(res2)) results[[s]] <- res2
    }
  }

  total <- if (length(results)) dplyr::bind_rows(results) else tibble::tibble()
  total
}

# ---------- main run ----------

usda_ref_etl_run <- function() {
  msg("== Edaphic Flora | USDA + NWPL ETL (robust downloader) ==")
  ensure_dirs(c("data/raw/usda", "data/raw/nwpl", "data/processed"))

  # 1) Locate USDA file
  usda_local <- Sys.getenv("USDA_PLANTS_LOCAL", "")
  usda_path  <- if (nzchar(usda_local) && file.exists(usda_local)) {
    usda_local
  } else if (file.exists("data/raw/usda/plantlst.bin")) {
    "data/raw/usda/plantlst.bin"
  } else {
    dest <- "data/raw/usda/usda_plants_download.bin"
    msg("Downloading: %s -> %s", "https://plants.usda.gov/csvdownload?plantLst=plantCompleteList", dest)
    http_try_download(c("https://plants.usda.gov/csvdownload?plantLst=plantCompleteList"), dest)
    dest
  }

  msg("Using local USDA file: %s", usda_path)
  usda <- read_usda_file(usda_path)

  # 2) Split into taxa (accepted rows) and synonyms
  usda_clean <- usda %>%
    mutate(
      Symbol = trimws(Symbol),
      `Synonym Symbol` = to_blank_na(trimws(`Synonym Symbol`)),
      `Scientific Name with Authors` = trimws(`Scientific Name with Authors`),
      Family = to_blank_na(trimws(Family))
    )

  taxa_tbl <- usda_clean %>%
    group_by(Symbol) %>%
    arrange(Symbol, `Synonym Symbol` != "") %>%   # prefer the accepted record
    slice(1L) %>%
    ungroup() %>%
    transmute(
      usda_symbol     = na_if(Symbol, ""),
      scientific_name = `Scientific Name with Authors`,
      family          = na_if(Family, "")
    ) %>%
    filter(!is.na(usda_symbol), !is.na(scientific_name), scientific_name != "")

  syn_tbl <- usda_clean %>%
    filter(`Synonym Symbol` != "") %>%
    transmute(
      usda_symbol    = Symbol,
      synonym_symbol = `Synonym Symbol`,
      synonym_name   = `Scientific Name with Authors`
    ) %>%
    filter(!is.na(usda_symbol), !is.na(synonym_name), synonym_name != "")

  msg("USDA taxa rows: %d", nrow(taxa_tbl))
  msg("USDA synonyms: %d", nrow(syn_tbl))

  # 3) Connect to DB
  con <- pg_connect_admin()
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  DBI::dbWithTransaction(con, {

    # 4) Upsert sources
    usda_source_id <- upsert_source(
      con,
      name    = "USDA PLANTS",
      version = "Complete Checklist",
      url     = "https://plants.usda.gov/",
      license = "USDA Public Domain"
    )

    nwpl_source_id <- upsert_source(
      con,
      name    = "USACE NWPL",
      version = "2022",
      url     = "https://nwpl.sec.usace.army.mil/",
      license = "USACE Terms"
    )

    # 5) Stage tables (TEMP)
    DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_taxa")
    DBI::dbExecute(con, "CREATE TEMP TABLE tmp_taxa (usda_symbol text, scientific_name text, family text)")
    DBI::dbWriteTable(con, "tmp_taxa", taxa_tbl, append = TRUE)

    DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_synonyms")
    DBI::dbExecute(con, "CREATE TEMP TABLE tmp_synonyms (usda_symbol text, synonym_symbol text, synonym_name text)")
    DBI::dbWriteTable(con, "tmp_synonyms", syn_tbl, append = TRUE)

    # 6) Insert taxa (dedup by lower(scientific_name)); prefer rows with a symbol
    DBI::dbExecute(con, "
      INSERT INTO ref_taxon (scientific_name, family, usda_symbol)
      SELECT DISTINCT ON (lower(u.scientific_name))
             u.scientific_name, NULLIF(u.family,''), NULLIF(u.usda_symbol,'')
      FROM tmp_taxa u
      LEFT JOIN ref_taxon t1 ON t1.usda_symbol = u.usda_symbol
      LEFT JOIN ref_taxon t2 ON lower(t2.scientific_name) = lower(u.scientific_name)
      WHERE t1.id IS NULL AND t2.id IS NULL
      ORDER BY lower(u.scientific_name), (u.usda_symbol IS NULL), u.usda_symbol
      ON CONFLICT ((lower(scientific_name))) DO NOTHING
    ")

    # 7) Insert synonyms (dedup-safe)
    DBI::dbExecute(con, "
      INSERT INTO ref_synonym (taxon_id, synonym_name)
      SELECT t.id, s.synonym_name
      FROM tmp_synonyms s
      JOIN ref_taxon t ON t.usda_symbol = s.usda_symbol
      WHERE s.synonym_name IS NOT NULL
        AND s.synonym_name <> ''
        AND NOT EXISTS (
          SELECT 1
          FROM ref_synonym rs
          WHERE rs.taxon_id = t.id
            AND lower(rs.synonym_name) = lower(s.synonym_name)
        )
    ")

    # 8) Fetch / parse NWPL (National + Regions)
    ensure_dirs("data/raw/nwpl")
    nwpl_nat <- "data/raw/nwpl/2022_NWPL_National.xlsx"

    # Local override?
    nwpl_local <- Sys.getenv("NWPL_NATIONAL_LOCAL", "")
    if (nzchar(nwpl_local) && file.exists(nwpl_local)) {
      file.copy(nwpl_local, nwpl_nat, overwrite = TRUE)
    } else if (!file.exists(nwpl_nat)) {
      msg("Downloading: %s -> %s", "https://nwpl.sec.usace.army.mil/static/reports/2022_NWPL_National.xlsx", nwpl_nat)
      http_try_download(c(
        "https://nwpl.sec.usace.army.mil/static/reports/2022_NWPL_National.xlsx",
        "https://wetland-plants.usace.army.mil/static/reports/2022_NWPL_National.xlsx"
      ), nwpl_nat)
    }

    # Parse national + regions
    nwpl_nat_tbl <- parse_nwpl_file(nwpl_nat, region = "National")

    regions <- c("AK","AGCP","AW","CB","EMP","GP","MW","NCNE","WMVC")
    nwpl_regions <- list()
    for (r in regions) {
      fp <- file.path("data/raw/nwpl", sprintf("2022_NWPL_%s.xlsx", r))
      if (!file.exists(fp)) {
        try(http_try_download(c(
          sprintf("https://nwpl.sec.usace.army.mil/static/reports/2022_NWPL_%s.xlsx", r),
          sprintf("https://wetland-plants.usace.army.mil/static/reports/2022_NWPL_%s.xlsx", r)
        ), fp), silent = TRUE)
      }
      if (file.exists(fp)) {
        t <- try(parse_nwpl_file(fp, region = r), silent = TRUE)
        if (!inherits(t, "try-error")) nwpl_regions[[r]] <- t
      }
    }

    nwpl_all <- bind_rows(nwpl_nat_tbl, !!!nwpl_regions)
    msg("NWPL parsed rows: %d", nrow(nwpl_all))
    msg("NWPL unique taxa refs: %d",
        length(unique(dplyr::coalesce(nwpl_all$symbol, tolower(nwpl_all$scientific_name)))))

    # 9) Stage indicators for upsert (add canonical sci names for matching)
    if (nrow(nwpl_all)) {
      ind_stage <- nwpl_all %>%
        mutate(
          sci_canon = canonicalize_scientific(scientific_name),
          usda_symbol = na_if(trimws(symbol), ""),
          scientific_name = na_if(trimws(scientific_name), ""),
          region = toupper(trimws(region)),
          indicator = toupper(trimws(indicator))
        ) %>%
        select(usda_symbol, scientific_name, sci_canon, region, indicator)

      DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_wetland")
      DBI::dbExecute(con, "CREATE TEMP TABLE tmp_wetland (usda_symbol text, scientific_name text, sci_canon text, region text, indicator text)")
      DBI::dbWriteTable(con, "tmp_wetland", ind_stage, append = TRUE)

      # Build a temp mapping of canonical names -> taxon_id from ref_taxon
      tax_map <- DBI::dbGetQuery(con, "SELECT id, scientific_name, usda_symbol FROM ref_taxon")
      tax_map$sci_canon <- canonicalize_scientific(tax_map$scientific_name)
      DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_taxon_name")
      DBI::dbExecute(con, "CREATE TEMP TABLE tmp_taxon_name (taxon_id bigint, usda_symbol text, sci_canon text)")
      DBI::dbWriteTable(con, "tmp_taxon_name",
                        tax_map[, c("id","usda_symbol","sci_canon")] %>%
                          rename(taxon_id = id), append = TRUE)

      # Insert indicators, prefer symbol-match; fallback to canonical-name match (case-insensitive)
      DBI::dbExecute(con, "
        WITH targets AS (
          SELECT
            COALESCE(t_by_sym.taxon_id, t_by_name.taxon_id) AS taxon_id,
            w.region,
            w.indicator
          FROM tmp_wetland w
          LEFT JOIN tmp_taxon_name t_by_sym
            ON w.usda_symbol IS NOT NULL AND w.usda_symbol <> '' AND t_by_sym.usda_symbol = w.usda_symbol
          LEFT JOIN tmp_taxon_name t_by_name
            ON (w.usda_symbol IS NULL OR w.usda_symbol = '')
           AND w.sci_canon IS NOT NULL AND w.sci_canon <> ''
           AND t_by_name.sci_canon = w.sci_canon
        )
        INSERT INTO ref_wetland_indicator (taxon_id, region, indicator, source_id)
        SELECT DISTINCT ON (taxon_id, region)
               taxon_id, region, indicator, $1::bigint
        FROM targets
        WHERE taxon_id IS NOT NULL
        ON CONFLICT (taxon_id, region) DO UPDATE
        SET indicator = EXCLUDED.indicator, source_id = EXCLUDED.source_id
      ", params = list(nwpl_source_id))
    }
  }) # end transaction

  # 10) Done — show quick counts
  out <- DBI::dbGetQuery(con, "
    SELECT
      (SELECT count(*) FROM ref_taxon)             AS ref_taxon_n,
      (SELECT count(*) FROM ref_synonym)           AS ref_synonym_n,
      (SELECT count(*) FROM ref_wetland_indicator) AS ref_wetland_indicator_n
  ")
  print(out)

  invisible(TRUE)
}
