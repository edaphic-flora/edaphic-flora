# SPDX-License-Identifier: MIT
# Edaphic Flora — USDA Characteristics API scraper (robust, USDA-shape aware)

suppressPackageStartupMessages({
  library(DBI); library(RPostgres)
  library(jsonlite); library(httr2)
  library(dplyr); library(stringr); library(purrr); library(tibble)
})

# ---------- Connections ----------
con_admin <- function() DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_ADMIN_USER"),
  password = Sys.getenv("POSTGRES_ADMIN_PASSWORD"),
  sslmode = Sys.getenv("POSTGRES_SSLMODE","require")
)

con_runtime <- function() DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT")),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode = Sys.getenv("POSTGRES_SSLMODE","require")
)

# ---------- One-time DDL + grants (admin) ----------
ensure_schema_with_admin <- function() {
  admin <- con_admin(); on.exit(DBI::dbDisconnect(admin), add = TRUE)

  DBI::dbExecute(admin, "
    CREATE TABLE IF NOT EXISTS ref_usda_traits (
      taxon_id                INTEGER PRIMARY KEY,
      usda_symbol             VARCHAR(32) UNIQUE,
      duration                TEXT,
      growth_habit            TEXT,
      native_status           TEXT,
      usda_group              TEXT,
      soil_ph_min             NUMERIC,
      soil_ph_max             NUMERIC,
      precip_min_in           NUMERIC,
      precip_max_in           NUMERIC,
      temp_min_f              NUMERIC,
      moisture_use            TEXT,
      shade_tolerance         TEXT,
      drought_tolerance       TEXT,
      salinity_tolerance      TEXT,
      caco3_tolerance         TEXT,
      mature_height_ft        NUMERIC,
      mature_spread_ft        NUMERIC,
      leaf_retention          TEXT,
      bloom_period            TEXT,
      planting_density_min_per_acre NUMERIC,
      planting_density_max_per_acre NUMERIC,
      -- optional extras present in some envs; adds are idempotent below
      updated_at              TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
  ")
  DBI::dbExecute(admin, "CREATE UNIQUE INDEX IF NOT EXISTS ux_traits_symbol ON ref_usda_traits (usda_symbol);")

  # Add commonly-seen extra columns if missing (harmless if they already exist)
  extra_sql <- c(
    "ALTER TABLE ref_usda_traits ADD COLUMN IF NOT EXISTS growth_rate TEXT;",
    "ALTER TABLE ref_usda_traits ADD COLUMN IF NOT EXISTS soil_texture TEXT;",
    "ALTER TABLE ref_usda_traits ADD COLUMN IF NOT EXISTS anaerobic_tolerance TEXT;",
    "ALTER TABLE ref_usda_traits ADD COLUMN IF NOT EXISTS invasive_status TEXT;"
  )
  invisible(lapply(extra_sql, function(q) try(DBI::dbExecute(admin, q), silent = TRUE)))

  DBI::dbExecute(admin, "
    CREATE TABLE IF NOT EXISTS etl_usda_char_log (
      symbol         TEXT,
      fetched_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      http_status    INTEGER,
      used_cache     BOOLEAN,
      parse_pairs    INTEGER,
      traits_updated INTEGER,
      message        TEXT
    );
  ")

  runtime_role <- Sys.getenv("POSTGRES_USER")
  DBI::dbExecute(admin, sprintf("GRANT USAGE ON SCHEMA public TO %s;", dbQuoteIdentifier(admin, runtime_role)))
  DBI::dbExecute(admin, sprintf("GRANT SELECT ON ref_taxon TO %s;",      dbQuoteIdentifier(admin, runtime_role)))
  DBI::dbExecute(admin, sprintf("GRANT SELECT, INSERT, UPDATE ON ref_usda_traits TO %s;", dbQuoteIdentifier(admin, runtime_role)))
  DBI::dbExecute(admin, sprintf("GRANT SELECT, INSERT ON etl_usda_char_log TO %s;",       dbQuoteIdentifier(admin, runtime_role)))

  # (optional) make future tables readable by runtime
  # DBI::dbExecute(admin, sprintf(
  #   "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO %s;",
  #   dbQuoteIdentifier(admin, runtime_role)
  # ))
}

# ---------- Safe coalescer ----------
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a)) a <- as.list(a[1, , drop = TRUE])
  if (is.list(a)) a <- unlist(a, use.names = FALSE)
  if (!length(a) || all(is.na(a))) return(b)
  if (is.character(a)) {
    a <- a[!is.na(a) & nzchar(a)]
    if (!length(a)) return(b)
    return(paste(unique(a), collapse = ", "))
  }
  if (is.numeric(a)) return(a[which(!is.na(a))[1]])
  a[1]
}

# ---------- HTTP helpers ----------
ua <- "EdaphicFlora/1.0 (research; contact: ttesterman.13@gmail.com)"

get_json <- function(url, cache_key = NULL, cache_dir = "data/cache/usda_char") {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- if (!is.null(cache_key)) file.path(cache_dir, paste0(cache_key, ".json")) else NULL

  if (!is.null(cache_file) && file.exists(cache_file)) {
    txt <- readLines(cache_file, warn = FALSE)
    return(list(json = jsonlite::fromJSON(txt, simplifyVector = TRUE), status = 200L, used_cache = TRUE))
  }
  resp <- tryCatch(
    request(url) |> req_user_agent(ua) |> req_timeout(30) |> req_perform(),
    error = identity
  )
  if (inherits(resp, "error")) return(list(json = NULL, status = NA_integer_, used_cache = FALSE))
  status <- resp_status(resp)
  if (status >= 200 && status < 300) {
    body <- resp_body_string(resp)
    if (!is.null(cache_file)) writeLines(body, cache_file)
    list(json = jsonlite::fromJSON(body, simplifyVector = TRUE), status = status, used_cache = FALSE)
  } else list(json = NULL, status = status, used_cache = FALSE)
}

# ---------- JSON helpers ----------
profile_row <- function(pj) {
  if (is.null(pj)) return(NULL)
  if (is.data.frame(pj)) return(as.list(pj[1, , drop = TRUE]))
  if (is.list(pj) && !is.null(pj$Id)) return(pj)
  if (is.list(pj) && length(pj) >= 1 && is.list(pj[[1]])) return(pj[[1]])
  pj
}

pluck_any <- function(x, candidates) {
  if (is.null(x)) return(NULL)
  nms <- names(x); if (is.null(nms)) return(NULL)
  ln <- tolower(nms)
  for (cand in candidates) {
    c2 <- tolower(cand)
    hit <- which(ln == c2 | ln == gsub("\\s+", "", c2))
    if (length(hit)) return(x[[ hit[1] ]])
  }
  NULL
}

# --- USDA-specific characteristics flattener ---
flatten_usda_characteristics <- function(node) {
  if (is.data.frame(node) &&
      all(c("PlantCharacteristicName","PlantCharacteristicValue") %in% names(node))) {
    tibble(
      label = as.character(node$PlantCharacteristicName),
      value = as.character(node$PlantCharacteristicValue)
    ) |>
      filter(!is.na(value) & nzchar(value))
  } else {
    tibble(label = character(), value = character())
  }
}

# Generic (fallback) flattener: finds (name/label/title/key) → (value/val/text)
flatten_pairs_generic <- function(x) {
  out <- list()
  walk_it <- function(node) {
    if (is.null(node)) return()
    if (is.data.frame(node)) {
      cols <- tolower(names(node))
      has_name  <- any(cols %in% c("name","label","title","key"))
      has_value <- any(cols %in% c("value","val","text"))
      if (has_name && has_value) {
        ncol <- names(node)[which(cols %in% c("name","label","title","key"))[1]]
        vcol <- names(node)[which(cols %in% c("value","val","text"))[1]]
        for (i in seq_len(nrow(node))) out[[ as.character(node[[ncol]][i]) ]] <<- as.character(node[[vcol]][i])
      }
    }
    if (is.list(node) && !is.data.frame(node)) {
      nms <- names(node)
      if (length(nms)) {
        ln <- tolower(nms)
        if (any(ln %in% c("name","label","title","key")) && any(ln %in% c("value","val","text"))) {
          nm <- node[[ which(ln %in% c("name","label","title","key"))[1] ]]
          val<- node[[ which(ln %in% c("value","val","text"))[1] ]]
          if (!is.null(nm)) out[[as.character(nm)]] <<- as.character(val)
        }
        for (child in node) walk_it(child)
      } else {
        for (child in node) walk_it(child)
      }
    }
  }
  walk_it(x)
  if (!length(out)) return(tibble(label=character(), value=character()))
  tibble(label = names(out), value = unname(unlist(out)))
}

# Canonical key map (accepts many USDA label variants)
norm_key <- function(s) {
  s <- tolower(trimws(as.character(s)))
  s <- gsub("[\\u00B0\\(\\)\\.,]", " ", s)   # strip degree, (), punctuation
  s <- gsub("\\s+", " ", s)
  key <- gsub("[^a-z0-9]+"," ", s)
  key <- trimws(key)

  # direct forms
  map <- list(
    "duration" = "duration",
    "growth habit" = "growth_habit",
    "group" = "usda_group",
    "native status" = "native_status",
    "ph minimum" = "soil_ph_min",
    "ph maximum" = "soil_ph_max",
    "precipitation minimum in" = "precip_min_in",
    "precipitation maximum in" = "precip_max_in",
    "temperature minimum f" = "temp_min_f",
    "moisture use" = "moisture_use",
    "shade tolerance" = "shade_tolerance",
    "drought tolerance" = "drought_tolerance",
    "salinity tolerance" = "salinity_tolerance",
    "caco3 tolerance" = "caco3_tolerance",
    "mature height ft" = "mature_height_ft",
    "mature spread ft" = "mature_spread_ft",
    "leaf retention" = "leaf_retention",
    "bloom period" = "bloom_period",
    "planting density min acre" = "planting_density_min_per_acre",
    "planting density max acre" = "planting_density_max_per_acre",
    # extras present in some envs
    "growth rate" = "growth_rate",
    "soil texture" = "soil_texture",
    "anaerobic tolerance" = "anaerobic_tolerance",
    "invasive status" = "invasive_status"
  )
  if (key %in% names(map)) return(map[[key]])

  # variants / regex
  if (grepl("^ph( |$).*min", s)) return("soil_ph_min")
  if (grepl("^ph( |$).*max", s)) return("soil_ph_max")
  if (grepl("^precip.*min", s)) return("precip_min_in")
  if (grepl("^precip.*max", s)) return("precip_max_in")
  if (grepl("^(min|minimum) temperature", s)) return("temp_min_f")
  if (grepl("^temperature.*min", s)) return("temp_min_f")
  if (grepl("^mature height", s)) return("mature_height_ft")
  if (grepl("^mature spread", s)) return("mature_spread_ft")
  if (grepl("^planting density.*min", s)) return("planting_density_min_per_acre")
  if (grepl("^planting density.*max", s)) return("planting_density_max_per_acre")
  NA_character_
}

parse_numeric <- function(v) {
  if (is.null(v) || is.na(v) || !nzchar(v)) return(NA_real_)
  nums <- as.numeric(unlist(regmatches(v, gregexpr("[-+]?[0-9]*\\.?[0-9]+", v))))
  if (!length(nums)) return(NA_real_)
  nums[1]
}

# ---------- Main runner ----------
usda_characteristics_scrape_run <- function(
  symbols        = NULL,
  only_missing   = TRUE,
  limit          = Inf,
  sleep_sec      = 0.5,
  overwrite_existing = TRUE,
  use_cache_dir  = "data/cache/usda_char",
  skip_admin_ddl = FALSE
) {
  message("== Edaphic Flora | USDA Characteristics Scraper (API) ==")

  if (!isTRUE(skip_admin_ddl)) {
    tryCatch(ensure_schema_with_admin(), error = function(e) {
      message("Admin DDL/grants step failed (continuing): ", e$message)
    })
  }

  con <- con_runtime(); on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Pull actual table columns so we only update what's present
  traits_cols <- tryCatch(DBI::dbListFields(con, "ref_usda_traits"), error = function(e) character())
  has_col <- function(nm) nm %in% traits_cols

  # Choose symbols
  if (is.null(symbols)) {
    sql <- "SELECT DISTINCT usda_symbol, id FROM ref_taxon WHERE usda_symbol IS NOT NULL AND usda_symbol <> ''"
    if (isTRUE(only_missing)) {
      sql <- paste0(sql, "
        AND id IN (
          SELECT t.id FROM ref_taxon t
          LEFT JOIN ref_usda_traits r ON r.taxon_id = t.id
          WHERE r.taxon_id IS NULL
             OR r.soil_ph_min IS NULL OR r.soil_ph_max IS NULL
             OR r.precip_min_in IS NULL OR r.precip_max_in IS NULL
             OR r.temp_min_f IS NULL
        )")
    }
    rows <- DBI::dbGetQuery(con, sql)
    symbols <- rows$usda_symbol
  }
  symbols <- unique(na.omit(symbols))
  if (!length(symbols)) { message("No symbols to process."); return(invisible(NULL)) }
  if (is.finite(limit) && length(symbols) > limit) symbols <- symbols[seq_len(limit)]

  total_pairs <- 0L; total_updates <- 0L

  for (sym in symbols) {
    # taxon id
    tax <- DBI::dbGetQuery(con, "SELECT id FROM ref_taxon WHERE usda_symbol = $1 LIMIT 1", params = list(sym))
    if (!nrow(tax)) {
      DBI::dbExecute(con, "INSERT INTO etl_usda_char_log(symbol,http_status,used_cache,parse_pairs,traits_updated,message)
                           VALUES($1,$2,$3,$4,$5,$6)",
                     params = list(sym, 404L, FALSE, 0L, 0L, "symbol_not_in_ref_taxon"))
      next
    }
    taxon_id <- tax$id[1]

    # 1) PlantProfile
    prof <- get_json(sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantProfile?symbol=%s",
                             URLencode(sym, reserved = TRUE)),
                     cache_key = paste0("profile_", sym), cache_dir = use_cache_dir)
    if (is.na(prof$status) || prof$status >= 400 || is.null(prof$json)) {
      DBI::dbExecute(con, "INSERT INTO etl_usda_char_log(symbol,http_status,used_cache,parse_pairs,traits_updated,message)
                           VALUES($1,$2,$3,$4,$5,$6)",
                     params = list(sym, prof$status %||% NA_integer_, FALSE, 0L, 0L, "profile_fetch_failed"))
      next
    }

    pj <- profile_row(prof$json)
    duration      <- pluck_any(pj, c("Duration","Durations")) %||% NA_character_
    growth_habit  <- pluck_any(pj, c("GrowthHabits","Growth_Habits","GrowthHabitsText")) %||% NA_character_
    usda_group    <- pluck_any(pj, c("Group","PlantGroup")) %||% NA_character_
    native_status <- pluck_any(pj, c("NativeStatus","NativeStatusText")) %||% NA_character_
    if (is.list(duration))      duration      <- paste(unlist(duration), collapse=", ")
    if (is.list(growth_habit))  growth_habit  <- paste(unlist(growth_habit), collapse=", ")

    plant_id <- pluck_any(pj, c("Id","PlantId","id")) %||% NA
    plant_id <- suppressWarnings(as.integer(plant_id))
    if (is.na(plant_id)) {
      DBI::dbExecute(con, "INSERT INTO etl_usda_char_log(symbol,http_status,used_cache,parse_pairs,traits_updated,message)
                           VALUES($1,$2,$3,$4,$5,$6)",
                     params = list(sym, 200L, prof$used_cache, 0L, 0L, "no_profile_id"))
      next
    }

    # 2) Characteristics (USDA table shape)
    Sys.sleep(sleep_sec)
    ch <- get_json(sprintf("https://plantsservices.sc.egov.usda.gov/api/PlantCharacteristics/%s", plant_id),
                   cache_key = paste0("char_", plant_id), cache_dir = use_cache_dir)
    if (is.na(ch$status) || ch$status >= 400 || is.null(ch$json)) {
      DBI::dbExecute(con, "INSERT INTO etl_usda_char_log(symbol,http_status,used_cache,parse_pairs,traits_updated,message)
                           VALUES($1,$2,$3,$4,$5,$6)",
                     params = list(sym, ch$status %||% NA_integer_, ch$used_cache, 0L, 0L, "char_fetch_failed"))
      next
    }

    pairs_usda <- flatten_usda_characteristics(ch$json)
    pairs_fallback <- if (nrow(pairs_usda)) tibble(label=character(),value=character()) else flatten_pairs_generic(ch$json)
    pairs <- bind_rows(pairs_usda, pairs_fallback) |> distinct()
    n_pairs <- nrow(pairs); total_pairs <- total_pairs + n_pairs

    kv <- list()
    if (n_pairs > 0) {
      pairs$label_std <- vapply(pairs$label, norm_key, character(1))
      pairs <- pairs[!is.na(pairs$label_std), , drop = FALSE]
      for (i in seq_len(nrow(pairs))) {
        key <- pairs$label_std[i]; val <- pairs$value[i]
        if (key %in% c("soil_ph_min","soil_ph_max","precip_min_in","precip_max_in","temp_min_f",
                       "mature_height_ft","mature_spread_ft",
                       "planting_density_min_per_acre","planting_density_max_per_acre")) {
          kv[[key]] <- parse_numeric(val)
        } else if (!is.na(key)) {
          kv[[key]] <- if (is.null(val) || !nzchar(val)) NA_character_ else as.character(val)
        }
      }
    }

    # Merge in profile bits (prefer fresh unless overwrite_existing=FALSE and value exists)
    for (nm in c("duration","growth_habit","usda_group","native_status")) {
      if (overwrite_existing || is.null(kv[[nm]])) kv[[nm]] <- get(nm)
    }

    # Upsert stub
    DBI::dbExecute(con, "
      INSERT INTO ref_usda_traits (taxon_id, usda_symbol)
      VALUES ($1, $2)
      ON CONFLICT (taxon_id) DO UPDATE SET usda_symbol = EXCLUDED.usda_symbol
    ", params = list(taxon_id, sym))

    # Limit to columns that actually exist in this DB
    if (length(kv)) kv <- kv[names(kv) %in% traits_cols]

    updated <- 0L
    if (length(kv)) {
      if (!overwrite_existing) {
        sel_cols <- paste(names(kv), collapse = ", ")
        exist <- DBI::dbGetQuery(con, paste0(
          "SELECT ", sel_cols, " FROM ref_usda_traits WHERE taxon_id = $1"
        ), params = list(taxon_id))
        keep <- vapply(names(kv), function(k) {
          val <- exist[[k]][1]; is.null(val) || is.na(val) || (is.character(val) && !nzchar(val))
        }, logical(1))
        kv <- kv[keep]
      }
      if (length(kv)) {
        set_clause <- paste0(names(kv), " = $", seq_along(kv), collapse = ", ")
        sql <- paste0("UPDATE ref_usda_traits SET ", set_clause,
                      ", updated_at = CURRENT_TIMESTAMP WHERE taxon_id = $", length(kv) + 1)
        DBI::dbExecute(con, sql, params = c(unname(kv), list(taxon_id)))
        updated <- length(kv)
      }
    }
    total_updates <- total_updates + updated

    DBI::dbExecute(con, "
      INSERT INTO etl_usda_char_log(symbol, fetched_at, http_status, used_cache, parse_pairs, traits_updated, message)
      VALUES ($1, CURRENT_TIMESTAMP, $2, $3, $4, $5, $6)
    ", params = list(sym, 200L, (prof$used_cache || ch$used_cache), n_pairs, updated,
                     if (n_pairs == 0) "no_pairs" else if (updated == 0) "nothing_to_update" else "ok"))
  }

  message("Done.")
  invisible(list(processed = length(symbols), pairs = total_pairs, updated = total_updates))
}
