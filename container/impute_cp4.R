#!/usr/bin/env Rscript
# impute_cp4.R — containerised CP2021 level-4 imputation, Postgres in / staging out
#
# Reads OJA postings from Postgres, fills the missing CP2021 level-4 codes with
# skillviz::predict_cp4_knn(), and writes a table carrying the vendor code and
# the imputed code in one column plus a flag saying which is which.
#
# Run modes, decided from which months already exist in the target table:
#   target absent, or every month missing, or more than
#   IMPUTE_MAX_INCREMENTAL missing  -> FULL        rebuild everything
#   1..IMPUTE_MAX_INCREMENTAL missing -> INCREMENTAL impute only those months
#   nothing missing                   -> NO-OP      exit 0
#
# Exit codes follow the house contract (itaposts/docker/README.md):
#   0 ok · 1 error · 2 skillviz not installed · 3 another run holds the lock
#
# Secrets come from the environment and are never logged.

# 1. Logging and small helpers -----

.ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

.log <- function(level, ...) {
  msg <- sprintf("%s %-5s %s", .ts(), level, paste0(..., collapse = ""))
  if (level %in% c("ERROR", "WARN")) {
    message(msg)
  } else {
    cat(msg, "\n", sep = "")
  }
  invisible(NULL)
}

.info <- function(...) .log("INFO", ...)
.warn <- function(...) .log("WARN", ...)
.err <- function(...) .log("ERROR", ...)

.die <- function(status, ...) {
  .err(...)
  quit(status = status, save = "no")
}

getenv_default <- function(name, default = NA_character_) {
  v <- Sys.getenv(name, unset = NA_character_)
  if (is.na(v) || !nzchar(v)) default else v
}

getenv_int <- function(name, default) {
  v <- getenv_default(name)
  if (is.na(v)) {
    return(default)
  }
  n <- suppressWarnings(as.integer(v))
  if (is.na(n)) {
    .die(1L, name, " must be an integer, got: ", v)
  }
  n
}

getenv_flag <- function(name, default) {
  v <- getenv_default(name)
  if (is.na(v)) {
    return(default)
  }
  tolower(v) %in% c("1", "true", "yes", "y", "on")
}

# 2. Dependencies -----

for (pkg in c("DBI", "RPostgres", "data.table")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .die(2L, "required package not installed in the image: ", pkg)
  }
}
if (!requireNamespace("skillviz", quietly = TRUE)) {
  .die(2L, "skillviz is not installed in the image")
}
suppressPackageStartupMessages({
  library(DBI)
  library(data.table)
})

# 3. Configuration -----
# PG_* is the canonical naming used by the rest of the ecosystem
# (CCNL/R/db.R, data_pipeline/R/db_functions.R); POSTGRES_* is accepted as an
# alias because that is what container/.Renviron and Docker conventions use.

PG_ALIASES <- list(
  PG_HOST = "POSTGRES_HOST",
  PG_PORT = "POSTGRES_PORT",
  PG_DBNAME = "POSTGRES_DB",
  PG_USER = "POSTGRES_USER",
  PG_PASSWORD = "POSTGRES_PASSWORD"
)

pg_params <- function() {
  out <- list()
  missing <- character(0)
  for (nm in names(PG_ALIASES)) {
    v <- getenv_default(nm)
    if (is.na(v)) {
      v <- getenv_default(PG_ALIASES[[nm]])
    }
    if (is.na(v)) {
      missing <- c(missing, sprintf("%s (or %s)", nm, PG_ALIASES[[nm]]))
    }
    out[[nm]] <- v
  }
  if (length(missing)) {
    .die(
      1L,
      "missing required environment variables: ",
      paste(missing, collapse = ", ")
    )
  }
  port <- suppressWarnings(as.integer(out$PG_PORT))
  if (is.na(port) || port <= 0L) {
    .die(1L, "PG_PORT is not a positive integer")
  }
  out$PG_PORT <- port
  out
}

cfg <- list(
  schema = getenv_default("IMPUTE_TARGET_SCHEMA", "staging"),
  table = getenv_default("IMPUTE_TARGET_TABLE", "gm_cp4_imputed"),
  window_months = getenv_int("IMPUTE_WINDOW_MONTHS", 24L),
  max_incremental = getenv_int("IMPUTE_MAX_INCREMENTAL", 3L),
  k = getenv_int("IMPUTE_K", 7L),
  sector_boost = as.numeric(getenv_default("IMPUTE_SECTOR_BOOST", "5")),
  rescue = getenv_flag("IMPUTE_RESCUE", TRUE),
  rescue_k = getenv_int("IMPUTE_RESCUE_K", 10L),
  rescue_max_train = getenv_int("IMPUTE_RESCUE_MAX_TRAIN", 400000L),
  dry_run = getenv_flag("IMPUTE_DRY_RUN", FALSE),
  # Per-ESCO-group training cap. The subsample is a deterministic stride, so
  # exceeding it costs training data but not reproducibility.
  max_train = getenv_int("IMPUTE_MAX_TRAIN", 50000L),
  lock_dir = getenv_default("IMPUTE_LOCK_DIR", tempdir())
)

# 4. Lock: one run at a time -----

lock_file <- file.path(cfg$lock_dir, "impute_cp4.lock")
if (!dir.exists(cfg$lock_dir)) {
  dir.create(cfg$lock_dir, recursive = TRUE, showWarnings = FALSE)
}
if (file.exists(lock_file)) {
  prev <- suppressWarnings(readLines(lock_file, warn = FALSE)[1])
  alive <- !is.na(prev) &&
    nzchar(prev) &&
    length(suppressWarnings(
      system2("ps", c("-p", prev), stdout = TRUE, stderr = FALSE)
    )) >=
      2
  if (isTRUE(alive)) {
    .die(3L, "another run holds the lock (pid ", prev, "): ", lock_file)
  }
  .warn("removing stale lock from pid ", prev)
  file.remove(lock_file)
}
writeLines(as.character(Sys.getpid()), lock_file)
on.exit(try(file.remove(lock_file), silent = TRUE), add = TRUE)

# 5. Connection -----

pg_connect <- function(p) {
  delays <- c(0, 5, 15)
  for (i in seq_along(delays)) {
    if (delays[i] > 0) {
      .warn("connection attempt ", i, " in ", delays[i], "s")
      Sys.sleep(delays[i])
    }
    con <- tryCatch(
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = p$PG_HOST,
        port = p$PG_PORT,
        dbname = p$PG_DBNAME,
        user = p$PG_USER,
        password = p$PG_PASSWORD,
        sslmode = getenv_default("PGSSLMODE", "require"),
        connect_timeout = 30L
      ),
      error = function(e) {
        .warn("connect failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(con)) {
      return(con)
    }
  }
  NULL
}

# 6. Run-mode decision (pure, unit-testable) -----

#' Decide the run mode from the months present in source and target.
#'
#' @param months_src Integer vector of yyyymm present in the source window.
#' @param months_done Integer vector of yyyymm already written to the target.
#' @param target_exists Logical: does the target table exist?
#' @param max_incremental Integer threshold above which a full rebuild wins.
#' @return list(mode = "full"|"incremental"|"noop", months = <to impute>)
decide_mode <- function(
  months_src,
  months_done,
  target_exists,
  max_incremental
) {
  months_src <- sort(unique(as.integer(months_src)))
  months_done <- sort(unique(as.integer(months_done)))
  missing <- setdiff(months_src, months_done)

  if (!isTRUE(target_exists) || length(missing) == length(months_src)) {
    return(list(mode = "full", months = months_src))
  }
  if (length(missing) == 0L) {
    return(list(mode = "noop", months = integer(0)))
  }
  if (length(missing) > max_incremental) {
    return(list(mode = "full", months = months_src))
  }
  list(mode = "incremental", months = sort(missing))
}

# 7. Reads -----

SQL_POSTINGS <- "
WITH d AS (
  SELECT DISTINCT ON (a.general_id)
         a.general_id,
         a.year_grab_date,
         a.month_grab_date,
         a.cp2021_id_level_5,
         a.idesco_level_5,
         a.idsector
  FROM public.lightcast_annunci_e12 a
  WHERE a.year_grab_date * 100 + a.month_grab_date BETWEEN $1 AND $2
  ORDER BY a.general_id,
           a.year_grab_date DESC, a.month_grab_date DESC, a.day_grab_date DESC
)
SELECT d.general_id,
       d.year_grab_date,
       d.month_grab_date,
       d.idsector,
       c.cp4              AS cp2021_id_level_4,
       e.idlevel_4        AS idesco_level_4
FROM d
LEFT JOIN staging.dim_cp2021_5        c ON d.cp2021_id_level_5 = c.cp5
LEFT JOIN public.lightcast_dim_occ_esco e ON d.idesco_level_5   = e.idlevel_5
"

SQL_SKILLS <- "
SELECT DISTINCT s.general_id, s.idescoskill_level_3 AS escoskill_level_3
FROM public.lightcast_skills_e12 s
WHERE s.year_grab_date * 100 + s.month_grab_date BETWEEN $1 AND $2
  AND s.idescoskill_level_3 IS NOT NULL
"

read_postings <- function(con, ym_from, ym_to) {
  dt <- setDT(DBI::dbGetQuery(con, SQL_POSTINGS, params = list(ym_from, ym_to)))
  dt[, general_id := as.character(general_id)]
  dt[, ym := year_grab_date * 100L + month_grab_date]
  dt[]
}

read_skills <- function(con, ym_from, ym_to, keep_ids) {
  dt <- setDT(DBI::dbGetQuery(con, SQL_SKILLS, params = list(ym_from, ym_to)))
  dt[, general_id := as.character(general_id)]
  dt[general_id %chin% keep_ids]
}

# 8. Output assembly -----
# vendor_cp4 is the code the vendor supplied, resolved through dim_cp2021_5.
# predict_cp4_knn() only ever returns rows for the unlabeled subset, so a row
# either keeps its vendor code (flag 0) or takes the imputed one (flag 1).
# Rows that stay unresolved keep NULL in both columns.
assemble <- function(postings, pred) {
  out <- postings[, list(
    general_id,
    year_grab_date,
    month_grab_date,
    vendor_cp4 = cp2021_id_level_4
  )]
  if (!is.null(pred) && nrow(pred) > 0L) {
    p <- unique(
      pred[
        !is.na(cp2021_id_level_4),
        list(general_id, imputed_cp4 = cp2021_id_level_4)
      ],
      by = "general_id"
    )
    out <- p[out, on = "general_id"]
  } else {
    out[, imputed_cp4 := NA_character_]
  }
  out[,
    cp2021_id_level_4 := fifelse(!is.na(vendor_cp4), vendor_cp4, imputed_cp4)
  ]
  out[,
    cp4_imputed := fifelse(
      is.na(cp2021_id_level_4),
      NA_integer_,
      fifelse(!is.na(vendor_cp4), 0L, 1L)
    )
  ]
  out[, list(
    general_id = as.numeric(general_id),
    year_grab_date,
    month_grab_date,
    cp2021_id_level_4,
    cp4_imputed
  )]
}

# 9. Reconciliation, asserted before anything is committed -----

reconcile <- function(out, postings) {
  stopifnot(
    "row count differs from deduped source" = nrow(out) == nrow(postings),
    "duplicate general_id in output" = !anyDuplicated(out$general_id),
    "flag set where no code exists" = all(
      is.na(out$cp4_imputed) == is.na(out$cp2021_id_level_4)
    ),
    "flag outside {0,1}" = all(
      out$cp4_imputed %in% c(0L, 1L) | is.na(out$cp4_imputed)
    )
  )
  # A vendor code must never be overwritten by an imputed one.
  chk <- merge(
    out[, list(
      general_id = as.character(general_id),
      cp2021_id_level_4,
      cp4_imputed
    )],
    postings[, list(general_id, vendor_cp4 = cp2021_id_level_4)],
    by = "general_id"
  )
  bad <- chk[
    !is.na(vendor_cp4) & (cp4_imputed != 0L | cp2021_id_level_4 != vendor_cp4)
  ]
  if (nrow(bad) > 0L) {
    .die(1L, "imputation overwrote ", nrow(bad), " vendor-supplied codes")
  }
  invisible(TRUE)
}

# 10. Writes -----

qi <- function(con, x) DBI::dbQuoteIdentifier(con, x)

target_id <- function(schema, table) DBI::Id(schema = schema, table = table)

table_exists <- function(con, schema, table) {
  DBI::dbExistsTable(con, target_id(schema, table))
}

add_constraints <- function(con, schema, table) {
  s <- qi(con, schema)
  t <- qi(con, table)
  DBI::dbExecute(
    con,
    sprintf(
      "ALTER TABLE %s.%s ADD PRIMARY KEY (general_id)",
      s,
      t
    )
  )
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE INDEX %s ON %s.%s (year_grab_date, month_grab_date)",
      qi(con, paste0("idx_", table, "_ym")),
      s,
      t
    )
  )
}

write_full <- function(con, out, schema, table) {
  staging_tbl <- paste0(table, "_new")
  if (table_exists(con, schema, staging_tbl)) {
    DBI::dbRemoveTable(con, target_id(schema, staging_tbl))
  }
  .info("writing ", nrow(out), " rows to ", schema, ".", staging_tbl)
  DBI::dbWriteTable(con, target_id(schema, staging_tbl), out, overwrite = TRUE)
  add_constraints(con, schema, staging_tbl)

  s <- qi(con, schema)
  DBI::dbBegin(con)
  ok <- tryCatch(
    {
      if (table_exists(con, schema, table)) {
        DBI::dbExecute(con, sprintf("DROP TABLE %s.%s", s, qi(con, table)))
      }
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s.%s RENAME TO %s",
          s,
          qi(con, staging_tbl),
          qi(con, table)
        )
      )
      DBI::dbCommit(con)
      TRUE
    },
    error = function(e) {
      DBI::dbRollback(con)
      .err("swap failed, rolled back: ", conditionMessage(e))
      FALSE
    }
  )
  if (!ok) {
    .die(1L, "full write aborted")
  }
  .info("swapped ", schema, ".", staging_tbl, " into ", schema, ".", table)
}

write_incremental <- function(con, out, schema, table, months) {
  s <- qi(con, schema)
  t <- qi(con, table)
  DBI::dbBegin(con)
  ok <- tryCatch(
    {
      n_del <- DBI::dbExecute(
        con,
        sprintf(
          "DELETE FROM %s.%s WHERE year_grab_date * 100 + month_grab_date = ANY($1)",
          s,
          t
        ),
        params = list(list(as.integer(months)))
      )
      .info("deleted ", n_del, " existing rows for the target months")
      DBI::dbAppendTable(con, target_id(schema, table), out)
      DBI::dbCommit(con)
      TRUE
    },
    error = function(e) {
      DBI::dbRollback(con)
      .err("incremental write failed, rolled back: ", conditionMessage(e))
      FALSE
    }
  )
  if (!ok) {
    .die(1L, "incremental write aborted")
  }
  .info("appended ", nrow(out), " rows for ", length(months), " month(s)")
}

# 11. Main -----

main <- function() {
  p <- pg_params()
  .info(
    "config: host=",
    p$PG_HOST,
    " db=",
    p$PG_DBNAME,
    " user=",
    p$PG_USER,
    " target=",
    cfg$schema,
    ".",
    cfg$table,
    " window=",
    cfg$window_months,
    "m k=",
    cfg$k,
    " boost=",
    cfg$sector_boost,
    " rescue=",
    cfg$rescue,
    " dry_run=",
    cfg$dry_run
  )

  con <- pg_connect(p)
  if (is.null(con)) {
    .die(1L, "could not connect to Postgres after 3 attempts")
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  # -- scope: the last N months present in the source --
  rng <- setDT(DBI::dbGetQuery(
    con,
    "
    SELECT DISTINCT year_grab_date * 100 + month_grab_date AS ym
    FROM public.lightcast_annunci_e12 ORDER BY 1"
  ))
  if (nrow(rng) == 0L) {
    .die(1L, "source table is empty")
  }
  months_src <- tail(rng$ym, cfg$window_months)
  ym_from <- min(months_src)
  ym_to <- max(months_src)
  .info("scope: ", length(months_src), " months, ", ym_from, "-", ym_to)

  exists_target <- table_exists(con, cfg$schema, cfg$table)
  months_done <- integer(0)
  if (exists_target) {
    months_done <- setDT(DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT year_grab_date * 100 + month_grab_date AS ym FROM %s.%s",
        qi(con, cfg$schema),
        qi(con, cfg$table)
      )
    ))$ym
  }

  d <- decide_mode(months_src, months_done, exists_target, cfg$max_incremental)
  .info("mode=", d$mode, " months to impute: ", length(d$months))
  if (d$mode == "noop") {
    .info("target is up to date, nothing to do")
    return(invisible(0L))
  }

  # -- read --
  .info("reading postings ...")
  postings <- read_postings(con, ym_from, ym_to)
  .info("postings: ", nrow(postings), " deduped rows")

  # In incremental mode keep every labelled row as training material but only
  # the target months' unlabelled rows as prediction targets: predict_cp4_knn()
  # splits on cp2021_id_level_4 being NA, so this is all the restriction needed.
  if (d$mode == "incremental") {
    postings <- postings[!is.na(cp2021_id_level_4) | ym %in% d$months]
    .info("incremental scope: ", nrow(postings), " rows after restriction")
  }

  .info("reading skills ...")
  skills <- read_skills(con, ym_from, ym_to, postings$general_id)
  .info("skills: ", nrow(skills), " assignments")

  # -- training-cap visibility --
  # predict_cp4_knn() subsamples groups above max_train by a deterministic
  # stride, so exceeding the cap costs training data but not reproducibility.
  # Report it instead of aborting and let the operator decide.
  grp <- postings[
    !is.na(cp2021_id_level_4) & !is.na(idesco_level_4),
    .N,
    by = idesco_level_4
  ]
  if (nrow(grp) && max(grp$N) > cfg$max_train) {
    over <- grp[N > cfg$max_train]
    .warn(
      nrow(over),
      " ESCO group(s) exceed IMPUTE_MAX_TRAIN=",
      cfg$max_train,
      " (largest ",
      max(over$N),
      "); they are subsampled by a deterministic stride. Raise ",
      "IMPUTE_MAX_TRAIN to use them whole, at proportionally higher memory."
    )
  }

  # -- impute --
  n_missing <- postings[is.na(cp2021_id_level_4), .N]
  .info("imputing ", n_missing, " rows without a vendor code ...")
  t0 <- proc.time()
  pred <- skillviz::predict_cp4_knn(
    postings[, list(general_id, idesco_level_4, cp2021_id_level_4, idsector)],
    skills,
    k = cfg$k,
    sector_boost = cfg$sector_boost,
    max_train = cfg$max_train,
    rescue_no_match = cfg$rescue,
    rescue_k = cfg$rescue_k,
    rescue_max_train = cfg$rescue_max_train,
    verbose = TRUE
  )
  .info("imputation done in ", round((proc.time() - t0)[["elapsed"]], 1), "s")
  if (nrow(pred)) {
    mix <- pred[, .N, by = method][order(-N)]
    .info(
      "method mix: ",
      paste(sprintf("%s=%d", mix$method, mix$N), collapse = ", ")
    )
  }

  # -- assemble and check --
  out <- assemble(postings, pred)
  if (d$mode == "incremental") {
    out <- out[year_grab_date * 100L + month_grab_date %in% d$months]
    reconcile(out, postings[ym %in% d$months])
  } else {
    reconcile(out, postings)
  }
  flag <- out[, .N, by = cp4_imputed][order(cp4_imputed)]
  .info(
    "output: ",
    nrow(out),
    " rows | ",
    paste(sprintf("flag %s=%d", flag$cp4_imputed, flag$N), collapse = ", ")
  )

  if (cfg$dry_run) {
    .info("IMPUTE_DRY_RUN set — nothing written")
    return(invisible(0L))
  }

  if (d$mode == "full") {
    write_full(con, out, cfg$schema, cfg$table)
  } else {
    write_incremental(con, out, cfg$schema, cfg$table, d$months)
  }
  .info("done")
  invisible(0L)
}

if (!interactive()) {
  tryCatch(
    {
      main()
      quit(status = 0L, save = "no")
    },
    error = function(e) {
      .err(conditionMessage(e))
      quit(status = 1L, save = "no")
    }
  )
}
