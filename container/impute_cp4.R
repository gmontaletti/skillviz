#!/usr/bin/env Rscript
# impute_cp4.R — containerised CP2021 level-4 imputation, Postgres in / staging out
#
# Reads OJA postings from Postgres, fills the missing CP2021 codes, and writes a
# table carrying vendor and imputed codes at levels 4 and 5, each with a flag
# saying which is which.
#
# One k-NN pass does the work: skillviz::predict_cp5_knn() votes at level 5 with
# the candidate space restricted on idesco_level_5, and the level-4 code is read
# off by truncation. Announcements with no ESCO code at any level cannot be
# restricted, so they go to a one-hot + xgboost model instead.
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

# on.exit() does NOT run under Rscript -- not on quit(), not even at the end of
# a script -- so the lock has to be released explicitly on every exit path.
# LOCK_HELD arms it only once this run actually owns the lock, so the "another
# run holds it" path cannot delete someone else's.
LOCK_HELD <- FALSE

.release_lock <- function() {
  if (isTRUE(LOCK_HELD) && exists("lock_file") && file.exists(lock_file)) {
    try(file.remove(lock_file), silent = TRUE)
  }
  invisible(NULL)
}

.die <- function(status, ...) {
  .err(...)
  .release_lock()
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
  # The compute takes hours, so its result is cached before the write is
  # attempted; IMPUTE_RESUME=1 writes that cache without recomputing.
  cache_file = getenv_default(
    "IMPUTE_CACHE_FILE",
    file.path(getenv_default("IMPUTE_LOCK_DIR", tempdir()), "cp4_output.rds")
  ),
  resume = getenv_flag("IMPUTE_RESUME", FALSE),
  # The mode decision compares MONTHS, so it cannot see that the model changed
  # -- only that data did. Re-imputing after a routing or model change therefore
  # needs an explicit force.
  force_full = getenv_flag("IMPUTE_FORCE_FULL", FALSE),
  # Per-ESCO-group training cap. The subsample is a deterministic stride, so
  # exceeding it costs training data but not reproducibility.
  max_train = getenv_int("IMPUTE_MAX_TRAIN", 50000L),
  # Elements in the dense test x train block. Each batch holds three such
  # matrices, so peak memory is ~24 bytes per element: the package default of
  # 2e8 costs ~4.8 GB and OOM-killed (exit 137) a 7.75 GB container on the
  # 24-month window. 2.5e7 costs ~600 MB. Only chunking changes, not results.
  dense_budget = as.numeric(getenv_default("IMPUTE_DENSE_BUDGET", "2.5e7")),
  # ESCO-less rows go to a one-hot + xgboost model instead of the k-NN rescue.
  # The k-NN wins where an ESCO code exists, because its exact candidate-set
  # restriction beats anything the covariates can do; where none exists that
  # restriction is unavailable and the covariates win instead (66.8% vs 77.5%
  # CP3 when this was measured). So each segment gets the model that wins on it.
  xgb = getenv_flag("IMPUTE_XGB", TRUE),
  xgb_rare_min = getenv_int("IMPUTE_XGB_RARE_MIN", 30L),
  xgb_rounds = getenv_int("IMPUTE_XGB_ROUNDS", 400L),
  # Level 5 is no longer optional: it IS the model. The single k-NN pass votes
  # at level 5 restricted on idesco_level_5, and the level-4 column is read off
  # by truncation. There is no flag to turn that off, because there is no
  # cheaper level-4 path left to fall back to -- the level-5 pass is itself
  # ~30% faster than the level-4 one it replaced (541s against 772s on the
  # 24-month window) and 0.8 GB lighter.
  seed = getenv_int("IMPUTE_SEED", 42L),
  lock_dir = getenv_default("IMPUTE_LOCK_DIR", tempdir())
)

# 4. Lock: one run at a time -----

lock_file <- file.path(cfg$lock_dir, "impute_cp4.lock")
if (!dir.exists(cfg$lock_dir)) {
  dir.create(cfg$lock_dir, recursive = TRUE, showWarnings = FALSE)
}
# Staleness is decided by AGE, not by whether the recorded PID is alive. Inside
# a container the entrypoint is always PID 1, so `ps -p 1` succeeds in every
# run and a leftover lock would look alive for ever -- blocking the job
# permanently after any crash. Concurrency is really guarded by `docker run
# --name`, which fails immediately on a second start; this file is the
# belt-and-braces, and a TTL is the only staleness signal that means anything
# here.
lock_ttl_s <- getenv_int("IMPUTE_LOCK_TTL_HOURS", 6L) * 3600L
if (file.exists(lock_file)) {
  age_s <- as.numeric(
    difftime(Sys.time(), file.mtime(lock_file), units = "secs")
  )
  if (age_s < lock_ttl_s) {
    .die(
      3L,
      "another run holds the lock (age ",
      round(age_s / 60),
      " min, TTL ",
      round(lock_ttl_s / 3600),
      "h): ",
      lock_file
    )
  }
  .warn("removing stale lock, age ", round(age_s / 3600, 1), "h")
  file.remove(lock_file)
}
writeLines(
  paste0(Sys.getpid(), " ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  lock_file
)
LOCK_HELD <- TRUE

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
         a.idsector,
         a.idcity, a.idprovince, a.idmacro_sector, a.idcategory_sector,
         a.idcontract, a.ideducational_level, a.idexperience,
         a.idworking_hours, a.idsalary, a.salaryvalue,
         a.source, a.source_relevance
  FROM public.lightcast_annunci_e12 a
  WHERE a.year_grab_date * 100 + a.month_grab_date BETWEEN $1 AND $2
  ORDER BY a.general_id,
           a.year_grab_date DESC, a.month_grab_date DESC, a.day_grab_date DESC
)
SELECT d.general_id,
       d.year_grab_date,
       d.month_grab_date,
       d.idsector,
       d.idcity, d.idprovince, d.idmacro_sector, d.idcategory_sector,
       d.idcontract, d.ideducational_level, d.idexperience,
       d.idworking_hours, d.idsalary, d.salaryvalue,
       d.source, d.source_relevance,
       c.cp4              AS cp2021_id_level_4,
       c.cp3              AS cp2021_id_level_3,
       -- c.cp5, never d.cp2021_id_level_5: the join to the dimension is what
       -- filters malformed vendor codes today, and taking the raw column would
       -- quietly let one through at level 5 while level 4 kept rejecting it.
       c.cp5              AS cp2021_id_level_5,
       e.idlevel_4        AS idesco_level_4,
       -- The level-5 occupation key is the restrictor the coder now uses. It
       -- was already in the CTE as a join key; this projects it.
       d.idesco_level_5   AS idesco_level_5
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
  # staging.dim_cp2021_5 is an external table with no DDL in this repo, so its
  # shape is asserted rather than trusted. Both held on the whole 24-month
  # window at the time of writing: 813 CP5 codes, all well formed, all with
  # left(cp5, 7) = cp4. A violation would silently corrupt the level-5 column
  # and break the hierarchy check in reconcile().
  bad_shape <- dt[
    !is.na(cp2021_id_level_5) &
      !grepl("^[0-9]\\.[0-9]\\.[0-9]\\.[0-9]\\.[0-9]$", cp2021_id_level_5),
    .N
  ]
  if (bad_shape > 0L) {
    .die(1L, bad_shape, " rows carry a malformed cp2021_id_level_5")
  }
  bad_hier <- dt[
    !is.na(cp2021_id_level_5) &
      substring(cp2021_id_level_5, 1L, 7L) != cp2021_id_level_4,
    .N
  ]
  if (bad_hier > 0L) {
    .die(1L, bad_hier, " rows where left(cp5, 7) does not equal cp4")
  }

  # The level-5 occupation key marks "no ESCO code" with the literal string
  # "Unclassifiable", never NULL. Every downstream predicate here tests
  # is.na(), so the sentinel is normalised once, at the boundary, rather than
  # in five places. The package does the same defensively; both layers have to
  # be correct on their own because the routing below is container-local.
  n_unc <- dt[idesco_level_5 == "Unclassifiable", .N]
  dt[idesco_level_5 == "Unclassifiable", idesco_level_5 := NA_character_]
  .info("normalised ", n_unc, " Unclassifiable level-5 codes to NA")

  # Both ESCO columns derive from the same join, so their NA-ness must coincide.
  # A divergence means lightcast_dim_occ_esco changed shape and the routing
  # below would silently send rows to the wrong model.
  n_div <- dt[is.na(idesco_level_5) != is.na(idesco_level_4), .N]
  if (n_div > 0L) {
    .die(
      1L,
      n_div,
      " rows where idesco_level_4 and idesco_level_5 disagree on being NA; ",
      "the occupation dimension has changed shape"
    )
  }
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
assemble <- function(postings, preds) {
  out <- postings[, list(
    general_id,
    year_grab_date,
    month_grab_date,
    vendor_cp4 = cp2021_id_level_4,
    vendor_cp5 = cp2021_id_level_5
  )]

  # Every model now supplies BOTH levels, and in both of them the level-4 code
  # is the truncation of the level-5 one -- the k-NN votes at level 5, xgboost
  # predicts at level 5, and vendor rows take both from the same
  # staging.dim_cp2021_5 row. So there is no longer any padding, and no
  # "one column, two precisions" convention: a level-4 code in this table is
  # always a genuine level-4 code.
  preds <- Filter(function(x) !is.null(x) && nrow(x) > 0L, preds)
  if (length(preds)) {
    pp <- unique(
      rbindlist(lapply(preds, function(x) {
        x[
          !is.na(cp2021_id_level_5),
          list(
            general_id,
            imputed_cp5 = cp2021_id_level_5,
            imputed_cp4 = cp2021_id_level_4
          )
        ]
      })),
      by = "general_id"
    )
    out <- pp[out, on = "general_id"]
  } else {
    out[, `:=`(imputed_cp5 = NA_character_, imputed_cp4 = NA_character_)]
  }

  # A model prediction must be self-consistent across the two levels; anything
  # else is an assembly bug, not a modelling disagreement, so it aborts rather
  # than being silently repaired.
  n_bad <- out[
    !is.na(imputed_cp5) &
      substring(imputed_cp5, 1L, 7L) != imputed_cp4,
    .N
  ]
  if (n_bad > 0L) {
    .die(
      1L,
      n_bad,
      " imputed level-4 codes are not the truncation of their level-5 code"
    )
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
  out[,
    cp2021_id_level_5 := fifelse(!is.na(vendor_cp5), vendor_cp5, imputed_cp5)
  ]
  out[,
    cp5_imputed := fifelse(
      is.na(cp2021_id_level_5),
      NA_integer_,
      fifelse(!is.na(vendor_cp5), 0L, 1L)
    )
  ]

  out[, list(
    general_id = as.numeric(general_id),
    year_grab_date,
    month_grab_date,
    cp2021_id_level_4,
    cp4_imputed,
    cp2021_id_level_5,
    cp5_imputed
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
    ),
    # No level-4 code may end in ".0". Of the 510 real CP4 codes none does, so
    # such a value could only be a level-3 code padded into level-4 shape --
    # the convention this job used before every model predicted at level 5.
    # The check now covers imputed rows too, not just vendor ones.
    # NOTE it is valid at level 4 ONLY: 340 of 813 real CP5 codes end in ".0",
    # so the same test at level 5 would reject genuine codes.
    "a level-4 code is padded" = !any(
      grepl("\\.0$", out$cp2021_id_level_4),
      na.rm = TRUE
    )
  )

  # -- level 5, only when the columns are present --
  if ("cp2021_id_level_5" %in% names(out)) {
    stopifnot(
      "cp5 flag set where no cp5 exists" = all(
        is.na(out$cp5_imputed) == is.na(out$cp2021_id_level_5)
      ),
      "cp5 flag outside {0,1}" = all(
        out$cp5_imputed %in% c(0L, 1L) | is.na(out$cp5_imputed)
      ),
      # A level-5 code without its level-4 parent would be unreadable.
      "cp5 present without cp4" = !any(
        !is.na(out$cp2021_id_level_5) & is.na(out$cp2021_id_level_4)
      ),
      # The whole point of the separate column: the two levels must agree.
      # assemble() drops any imputed level-5 code that would break this.
      "cp5 does not sit under its cp4" = all(
        is.na(out$cp2021_id_level_5) |
          substring(out$cp2021_id_level_5, 1L, 7L) == out$cp2021_id_level_4
      ),
      "cp5 is malformed" = all(
        is.na(out$cp2021_id_level_5) |
          grepl(
            "^[0-9]\\.[0-9]\\.[0-9]\\.[0-9]\\.[0-9]$",
            out$cp2021_id_level_5
          )
      )
    )
    # A vendor-supplied level-5 code must survive untouched, exactly as at
    # level 4.
    chk5 <- merge(
      out[, list(
        general_id = as.character(general_id),
        cp2021_id_level_5,
        cp5_imputed
      )],
      postings[, list(general_id, vendor_cp5 = cp2021_id_level_5)],
      by = "general_id"
    )
    bad5 <- chk5[
      !is.na(vendor_cp5) &
        (cp5_imputed != 0L | cp2021_id_level_5 != vendor_cp5)
    ]
    if (nrow(bad5) > 0L) {
      .die(
        1L,
        "imputation overwrote ",
        nrow(bad5),
        " vendor-supplied cp5 codes"
      )
    }
  }
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

# 9b. ESCO-less rows: one-hot + xgboost, predicting CP5 -----
#
# These postings carry idesco_level_5 = "Unclassifiable", so there is no ESCO
# code at any level and the k-NN has no candidate set to restrict to. The k-NN
# rescue reaches only ~67% CP3 on them; this model reaches ~81%, and it is the
# only one that can classify a posting with NO skills at all (81.5% vs 69.7%),
# because it uses city, sector, source, contract, education and salary.
#
# It predicts CP5 directly, and the level-4 code is its truncation -- so these
# rows now carry a genuine level-4 code rather than a level-3 one padded into
# level-4 shape. Two findings replaced the old design, measured on the 42,530
# labelled ESCO-less rows over a 3-month temporal holdout
# (skillviz_workflow/run_cp5_xgboost_no_esco.R):
#
#   vtreat was dropped. Impact coding is both SLOWER and LESS accurate than a
#   plain one-hot: at CP3, 736s and 80.09% against 21s and 81.34%; at CP5,
#   2953s and 75.44% against 35s and 76.36%. It compresses every categorical
#   into per-class scores fitted on the training rows, and it builds a DENSE
#   frame whose width grows with the class count -- which is the whole reason
#   level 5 was previously believed impossible here. One-hot stays sparse, so
#   its cost does not depend on the number of classes at all: the same 1,531
#   columns serve 56 classes and 102.
#
#   Level 5 is affordable. The old comment claimed CP4 needed a ~33 GB dense
#   frame; that was a property of vtreat, not of the level. Only 102 classes
#   survive the rare-class threshold here, and the one-hot fit takes 35s
#   against 21s for level 3.
#
# The accuracy differences are ~1-2 standard errors on a 4,496-row test set and
# should not be over-read; the 35-84x speed difference is what settles it.

XGB_COVARS <- c(
  "idcity",
  "idprovince",
  "idsector",
  "idmacro_sector",
  "idcategory_sector",
  "idcontract",
  "ideducational_level",
  "idexperience",
  "idworking_hours",
  "idsalary",
  "salaryvalue",
  "source",
  "source_relevance",
  "month_grab_date",
  "n_skills"
)

# Covariates that are genuinely numeric; everything else is a code and becomes
# an indicator, however integer-looking it is.
XGB_NUMERIC <- c("salaryvalue", "n_skills", "month_grab_date")

xgb_impute_no_esco <- function(postings, skills) {
  for (pkg in c("xgboost", "Matrix")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      .die(2L, "IMPUTE_XGB is on but package not in the image: ", pkg)
    }
  }
  # Selected on the level-5 key, which is the primary one: level 4 is derived
  # from it through the occupation dimension, so its NA-ness was only ever
  # accidental -- a dimension that gained an 'Unclassifiable' row would have
  # broken this predicate silently. read_postings() already asserts the two
  # agree, so this is a no-op today and stays correct if that ever changes.
  d <- postings[is.na(idesco_level_5)]
  if (nrow(d) == 0L) {
    return(NULL)
  }
  n_sk <- skills[, list(n_skills = .N), by = general_id]
  d <- n_sk[d, on = "general_id"]
  d[is.na(n_skills), n_skills := 0L]

  lab_i <- which(!is.na(d$cp2021_id_level_5))
  unl_i <- which(is.na(d$cp2021_id_level_5))
  if (length(lab_i) < 500L || length(unl_i) == 0L) {
    .warn(
      "too few labelled ESCO-less rows (",
      length(lab_i),
      ") to fit xgboost"
    )
    return(NULL)
  }

  # Classes too rare to learn are folded into an "other" bucket. Rows predicted
  # into it are dropped rather than guessed into a common class -- an honest
  # NULL beats a confident wrong code.
  cls <- d[lab_i, .N, by = cp2021_id_level_5]
  rare <- cls[N < cfg$xgb_rare_min, cp2021_id_level_5]
  y_lab <- fifelse(
    d$cp2021_id_level_5[lab_i] %chin% rare,
    "other",
    d$cp2021_id_level_5[lab_i]
  )
  lev <- sort(unique(y_lab))
  .info(
    "xgb: ",
    length(lab_i),
    " labelled / ",
    length(unl_i),
    " to impute | ",
    length(lev),
    " classes (",
    length(rare),
    " rare folded into 'other')"
  )

  # One-hot over labelled and unlabelled together, so the two design matrices
  # share a column space by construction rather than by a prepare() step.
  t0 <- proc.time()
  cv <- copy(d[, XGB_COVARS, with = FALSE])
  for (v in setdiff(XGB_COVARS, XGB_NUMERIC)) {
    x <- as.character(cv[[v]])
    set(cv, j = v, value = factor(fifelse(is.na(x), "NA", x)))
  }
  for (v in XGB_NUMERIC) {
    x <- as.numeric(cv[[v]])
    set(cv, j = v, value = fifelse(is.na(x), -1, x))
  }
  X_cov <- Matrix::sparse.model.matrix(~ . - 1, data = cv)

  skill_levels <- sort(unique(skills$escoskill_level_3))
  sr <- skills[general_id %chin% d$general_id]
  X_sk <- Matrix::sparseMatrix(
    i = match(sr$general_id, d$general_id),
    j = match(sr$escoskill_level_3, skill_levels),
    x = 1,
    dims = c(nrow(d), length(skill_levels))
  )
  X <- cbind(X_cov, X_sk)
  .info(sprintf(
    "xgb: one-hot design %d x %d in %.0fs",
    nrow(X),
    ncol(X),
    (proc.time() - t0)[["elapsed"]]
  ))

  t0 <- proc.time()
  set.seed(cfg$seed)
  bst <- xgboost::xgb.train(
    params = list(
      objective = "multi:softprob",
      num_class = length(lev),
      eval_metric = "mlogloss",
      tree_method = "hist",
      max_depth = 8,
      eta = 0.2,
      subsample = 0.8,
      colsample_bytree = 0.5,
      min_child_weight = 5,
      nthread = max(1L, parallel::detectCores() - 2L)
    ),
    data = xgboost::xgb.DMatrix(
      X[lab_i, , drop = FALSE],
      label = match(y_lab, lev) - 1L
    ),
    nrounds = cfg$xgb_rounds,
    verbose = 0
  )
  .info(sprintf("xgb: trained in %.0fs", (proc.time() - t0)[["elapsed"]]))

  # xgboost >= 2.0 returns multi:softprob already shaped n x num_class;
  # reshaping it again scrambles every row.
  pm <- predict(bst, xgboost::xgb.DMatrix(X[unl_i, , drop = FALSE]))
  if (is.null(dim(pm))) {
    pm <- matrix(pm, ncol = length(lev), byrow = TRUE)
  }
  stopifnot(nrow(pm) == length(unl_i), ncol(pm) == length(lev))
  pred <- lev[max.col(pm, ties.method = "first")]

  out <- data.table(
    general_id = d$general_id[unl_i],
    cp2021_id_level_5 = pred
  )
  out <- out[cp2021_id_level_5 != "other"]
  .info(
    "xgb: predicted ",
    nrow(out),
    " of ",
    length(unl_i),
    " ESCO-less rows"
  )
  out[, list(
    general_id,
    cp2021_id_level_5,
    cp2021_id_level_4 = substring(cp2021_id_level_5, 1L, 7L),
    method = "xgb_cp5"
  )]
}

# 10. Writes -----

qi <- function(con, x) DBI::dbQuoteIdentifier(con, x)

target_id <- function(schema, table) DBI::Id(schema = schema, table = table)

table_exists <- function(con, schema, table) {
  DBI::dbExistsTable(con, target_id(schema, table))
}

# Index and constraint names are global to the schema, and ALTER TABLE ...
# RENAME does not rename them. So the names carry a per-run suffix and KEEP it:
# they are never renamed to a canonical form.
#
# An earlier version did rename them, on the reasoning that dropping the live
# table frees its names. That reasoning has a hole, and production found it: the
# live table had been renamed to gm_cp4_imputed_old as a manual backup before a
# model change, so it was never dropped, and it still owned the index named
# gm_cp4_imputed_pkey. The swap wrote 1.66M rows, renamed the staging table into
# place, then failed on "relation gm_cp4_imputed_pkey already exists" and rolled
# the whole thing back -- leaving NO live table at all. Any relation in the
# schema holding the canonical name reproduces this, and a backup made with
# RENAME always holds it.
#
# Nothing references these names, so keeping the suffix costs nothing. The
# primary key works whatever it is called.
#
# The suffix must be genuinely unique. Sys.getpid() is not: inside a container
# the entrypoint is PID 1, so every run produced "..._pkey_1". The lock file has
# so far prevented two runs from colliding on it, but a leftover staging table
# would collide at add_constraints() time, before the transaction.
build_names <- function(table, suffix) {
  list(
    pkey = paste0(table, "_pkey_", suffix),
    ym = paste0("idx_", table, "_ym_", suffix)
  )
}

add_constraints <- function(con, schema, table, nm) {
  s <- qi(con, schema)
  t <- qi(con, table)
  DBI::dbExecute(
    con,
    sprintf(
      "ALTER TABLE %s.%s ADD CONSTRAINT %s PRIMARY KEY (general_id)",
      s,
      t,
      qi(con, nm$pkey)
    )
  )
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE INDEX %s ON %s.%s (year_grab_date, month_grab_date)",
      qi(con, nm$ym),
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
  nm <- build_names(table, format(Sys.time(), "%Y%m%d%H%M%S"))
  .info("writing ", nrow(out), " rows to ", schema, ".", staging_tbl)
  DBI::dbWriteTable(con, target_id(schema, staging_tbl), out, overwrite = TRUE)
  add_constraints(con, schema, staging_tbl, nm)

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
      # No rename to canonical names: see build_names(). The suffixed names
      # stay, and the swap is now two statements that cannot collide with
      # anything else in the schema.
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

  # -- resume: write a cached compute without redoing it --
  if (cfg$resume) {
    if (!nzchar(cfg$cache_file) || !file.exists(cfg$cache_file)) {
      .die(1L, "IMPUTE_RESUME set but no cache at ", cfg$cache_file)
    }
    cached <- readRDS(cfg$cache_file)
    .info(
      "resuming from ",
      cfg$cache_file,
      ": ",
      nrow(cached$out),
      " rows, mode=",
      cached$mode
    )
    if (cfg$dry_run) {
      .info("IMPUTE_DRY_RUN set — nothing written")
      return(invisible(0L))
    }
    write_out(p, cached$out, list(mode = cached$mode, months = cached$months))
    return(invisible(0L))
  }

  con <- pg_connect(p)
  if (is.null(con)) {
    .die(1L, "could not connect to Postgres after 3 attempts")
  }
  on.exit(
    try(if (!is.null(con)) DBI::dbDisconnect(con), silent = TRUE),
    add = TRUE
  )

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
  if (cfg$force_full && d$mode != "full") {
    .info(
      "IMPUTE_FORCE_FULL set: overriding mode=",
      d$mode,
      " with a full rebuild"
    )
    d <- list(mode = "full", months = months_src)
  }

  # Schema drift. decide_mode() compares months, so it cannot see that the
  # output gained columns. An incremental run appends into the live table, and
  # dbAppendTable fails outright when the target predates the level-5 columns.
  # Force a full rebuild instead of dying on the first run after the change.
  if (exists_target && d$mode == "incremental") {
    have <- DBI::dbGetQuery(
      con,
      "SELECT column_name FROM information_schema.columns
        WHERE table_schema = $1 AND table_name = $2",
      params = list(cfg$schema, cfg$table)
    )$column_name
    want <- c("cp2021_id_level_5", "cp5_imputed")
    missing <- setdiff(want, have)
    if (length(missing)) {
      .info(
        "target lacks ",
        paste(missing, collapse = ", "),
        "; forcing a full rebuild so the new columns land atomically"
      )
      d <- list(mode = "full", months = months_src)
    }
  }
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

  # Everything needed is in memory now, and the imputation takes hours. Holding
  # the connection open across it does not survive contact with a managed
  # Postgres: Azure dropped an idle 5.5-hour session and the write failed with
  # "SSL SYSCALL error: Operation timed out" AFTER the whole imputation had
  # succeeded. Close here and reconnect when there is something to write.
  DBI::dbDisconnect(con)
  con <- NULL
  .info("disconnected for the compute phase")

  # -- training-cap visibility --
  # The k-NN subsamples groups above max_train by a deterministic stride, so
  # exceeding the cap costs training data but not reproducibility. Report it
  # instead of aborting and let the operator decide. Grouped on the restrictor
  # actually in use: under level 5 the largest group is ~6.8x smaller, so this
  # will normally stay silent -- that is information, not an absence of it.
  grp <- postings[
    !is.na(cp2021_id_level_5) & !is.na(idesco_level_5),
    .N,
    by = idesco_level_5
  ]
  if (nrow(grp) && max(grp$N) > cfg$max_train) {
    over <- grp[N > cfg$max_train]
    .warn(
      nrow(over),
      " ESCO level-5 group(s) exceed IMPUTE_MAX_TRAIN=",
      cfg$max_train,
      " (largest ",
      max(over$N),
      "); they are subsampled by a deterministic stride. Raise ",
      "IMPUTE_MAX_TRAIN to use them whole, at proportionally higher memory."
    )
  } else if (nrow(grp)) {
    .info(
      "largest ESCO level-5 training group ",
      max(grp$N),
      ", under IMPUTE_MAX_TRAIN=",
      cfg$max_train,
      "; no subsampling"
    )
  }

  # -- impute: ONE k-NN pass, at level 5, restricted on ESCO level 5 --
  # The level-4 code is then read off by truncation rather than voted
  # separately. Two measurements license this:
  #
  #   Truncating the level-5 argmax reproduces the level-4 argmax to within
  #   +0.004 pp contemporaneously and +0.02 pp walk-forward, changing 0.07% of
  #   predictions -- two orders of magnitude under the 0.244 pp noise floor.
  #
  #   The neighbour search is ~98.6% of the runtime and the vote 1.4%, so a
  #   second pass would nearly double the cost to re-derive the same answer.
  #
  # It also removes a failure mode rather than merely saving time: voting the
  # two levels independently let them disagree, and a disagreeing level-5 code
  # had to be discarded. Under truncation the hierarchy holds by construction.
  n_missing <- postings[is.na(cp2021_id_level_5), .N]
  .info("imputing ", n_missing, " rows without a vendor code ...")
  t0 <- proc.time()
  pred5 <- skillviz::predict_cp5_knn(
    postings[, list(
      general_id,
      idesco_level_5,
      cp2021_id_level_5,
      idsector
    )],
    skills,
    restrictor = "idesco_level_5",
    k = cfg$k,
    sector_boost = cfg$sector_boost,
    max_train = cfg$max_train,
    dense_budget = cfg$dense_budget,
    # With IMPUTE_XGB on, the ESCO-less rows belong to xgboost, so the k-NN's
    # own rescue path is switched off to avoid two models claiming the same
    # rows. Measured at CP3: k-NN 66.8% vs xgboost 77.5% there.
    rescue_no_match = cfg$rescue && !cfg$xgb,
    rescue_k = cfg$rescue_k,
    rescue_max_train = cfg$rescue_max_train,
    verbose = TRUE
  )
  .info("imputation done in ", round((proc.time() - t0)[["elapsed"]], 1), "s")
  if (nrow(pred5)) {
    mix <- pred5[, .N, by = method][order(-N)]
    .info(
      "method mix: ",
      paste(sprintf("%s=%d", mix$method, mix$N), collapse = ", ")
    )
  }

  # The level-4 prediction, by truncation.
  pred_knn <- pred5[
    !is.na(cp2021_id_level_5),
    list(
      general_id,
      cp2021_id_level_5,
      cp2021_id_level_4 = substring(cp2021_id_level_5, 1L, 7L)
    )
  ]

  # -- ESCO-less rows: xgboost, CP3 padded into the same column --
  pred_xgb <- NULL
  if (cfg$xgb) {
    t0 <- proc.time()
    pred_xgb <- xgb_impute_no_esco(postings, skills)
    .info("xgb path done in ", round((proc.time() - t0)[["elapsed"]], 1), "s")
  }

  # -- assemble and check --
  out <- assemble(postings, list(pred_knn, pred_xgb))
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

  # Persist before attempting the write. The compute costs hours; a transient
  # database failure must not throw it away. IMPUTE_RESUME=1 reloads this and
  # skips straight to the write.
  if (nzchar(cfg$cache_file)) {
    saveRDS(list(out = out, mode = d$mode, months = d$months), cfg$cache_file)
    .info("cached output to ", cfg$cache_file)
  }

  if (cfg$dry_run) {
    .info("IMPUTE_DRY_RUN set — nothing written")
    return(invisible(0L))
  }

  write_out(p, out, d)
  invisible(0L)
}

# 12. Write phase, on its own connection -----
# Separated so it can run either after a fresh compute or from the cache.
write_out <- function(p, out, d) {
  con <- pg_connect(p)
  if (is.null(con)) {
    .die(1L, "could not reconnect to Postgres to write")
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
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
      .release_lock()
      quit(status = 0L, save = "no")
    },
    error = function(e) {
      .err(conditionMessage(e))
      .release_lock()
      quit(status = 1L, save = "no")
    }
  )
}
