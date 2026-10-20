# Offline smoke test for the container's assembly and reconciliation.
# Loads impute_cp4.R's definitions without running main(), then drives
# assemble() and reconcile() over synthetic postings covering every row class.
#
# Run from either the package root or container/:
#   Rscript container/test_impute_cp4.R

library(data.table)

Sys.setenv(IMPUTE_LOCK_DIR = tempdir())
SCRIPT <- if (file.exists("impute_cp4.R")) {
  "impute_cp4.R"
} else {
  "container/impute_cp4.R"
}

exprs <- parse(SCRIPT)
# Drop the trailing `if (!interactive()) { main() ... }` guard.
last <- exprs[[length(exprs)]]
stopifnot(identical(as.character(last[[1]]), "if"))
env <- new.env(parent = globalenv())
for (e in exprs[-length(exprs)]) {
  eval(e, envir = env)
}

assemble <- get("assemble", envir = env)
reconcile <- get("reconcile", envir = env)

ok <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    cat("  ok   ", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("  FAIL ", label, "\n")
  }
}

# Row classes, in order:
#  1 vendor-coded, both levels from the dimension
#  2 vendor-coded on a CP4 whose only CP5 child ends in .0
#  3 unlabelled with ESCO    -> k-NN at level 5, level 4 by truncation
#  4 unlabelled with ESCO    -> same, landing on a .0-terminated CP5
#  5 unlabelled without ESCO -> xgboost at level 5, level 4 by truncation
#  6 unlabelled, unresolved  -> nothing anywhere
postings <- data.table(
  general_id = as.character(1:6),
  year_grab_date = 2025L,
  month_grab_date = 6L,
  cp2021_id_level_4 = c("1.1.1.1", "2.2.2.1", NA, NA, NA, NA),
  cp2021_id_level_5 = c("1.1.1.1.2", "2.2.2.1.0", NA, NA, NA, NA),
  ym = 202506L
)

# main() derives every level-4 code by truncating the level-5 one, so the
# fixture does the same: the two can no longer disagree by construction.
.trunc <- function(dt) {
  dt[, list(
    general_id,
    cp2021_id_level_5,
    cp2021_id_level_4 = substring(cp2021_id_level_5, 1L, 7L)
  )]
}
pred_knn <- .trunc(data.table(
  general_id = c("3", "4"),
  cp2021_id_level_5 = c("3.3.3.3.1", "4.4.4.4.0")
))
# The ESCO-less path now predicts level 5 too, so its rows get a genuine
# level-4 code rather than a level-3 one padded into level-4 shape.
pred_xgb <- .trunc(data.table(
  general_id = "5",
  cp2021_id_level_5 = "5.5.5.5.0"
))

out <- assemble(postings, list(pred_knn, pred_xgb))
setkey(out, general_id)
g <- function(id, col) out[general_id == as.numeric(id)][[col]]

cat("== shape ==\n")
check(
  "seven columns, both levels flagged",
  identical(
    names(out),
    c(
      "general_id",
      "year_grab_date",
      "month_grab_date",
      "cp2021_id_level_4",
      "cp4_imputed",
      "cp2021_id_level_5",
      "cp5_imputed"
    )
  )
)

cat("\n== row classes ==\n")
check(
  "vendor codes kept at both levels, flags 0",
  g(1, "cp2021_id_level_5") == "1.1.1.1.2" &&
    g(1, "cp2021_id_level_4") == "1.1.1.1" &&
    g(1, "cp5_imputed") == 0L &&
    g(1, "cp4_imputed") == 0L
)
check(
  "a vendor CP5 ending .0 is a real code, not padding",
  g(2, "cp2021_id_level_5") == "2.2.2.1.0" && g(2, "cp5_imputed") == 0L
)
check(
  "k-NN row: level 4 is the truncation of level 5, flags 1",
  g(3, "cp2021_id_level_5") == "3.3.3.3.1" &&
    g(3, "cp2021_id_level_4") == "3.3.3.3" &&
    g(3, "cp5_imputed") == 1L &&
    g(3, "cp4_imputed") == 1L
)
check(
  "k-NN row landing on a .0 CP5 still yields a real CP4",
  g(4, "cp2021_id_level_5") == "4.4.4.4.0" &&
    g(4, "cp2021_id_level_4") == "4.4.4.4"
)
check(
  "xgboost row now carries BOTH levels, not a padded CP3",
  g(5, "cp2021_id_level_5") == "5.5.5.5.0" &&
    g(5, "cp2021_id_level_4") == "5.5.5.5" &&
    g(5, "cp5_imputed") == 1L
)
check(
  "unresolved row is NULL everywhere",
  is.na(g(6, "cp2021_id_level_4")) && is.na(g(6, "cp2021_id_level_5"))
)

cat("\n== invariants ==\n")
check(
  "hierarchy holds wherever both codes exist",
  out[
    !is.na(cp2021_id_level_5) & !is.na(cp2021_id_level_4),
    all(substring(cp2021_id_level_5, 1L, 7L) == cp2021_id_level_4)
  ]
)
check(
  "no level-4 code is padded",
  !any(grepl("\\.0$", out$cp2021_id_level_4), na.rm = TRUE)
)
check("reconcile passes", isTRUE(reconcile(out, postings)))

cat("\n== reconcile catches a hierarchy break ==\n")
broken <- copy(out)
broken[general_id == 3, cp2021_id_level_5 := "7.7.7.7.1"]
check(
  "hierarchy violation is fatal",
  tryCatch(
    {
      reconcile(broken, postings)
      FALSE
    },
    error = function(e) grepl("does not sit under", conditionMessage(e))
  )
)

cat("\n== reconcile catches a malformed cp5 ==\n")
bad <- copy(out)
bad[general_id == 1, cp2021_id_level_5 := "1.1.1.1"]
check(
  "malformed cp5 is fatal",
  tryCatch(
    {
      reconcile(bad, postings)
      FALSE
    },
    error = function(e) {
      grepl("malformed|does not sit under", conditionMessage(e))
    }
  )
)

cat("\n== write_full keeps the live table and its dependants ==\n")
# DuckDB stands in for Postgres: TRUNCATE, INSERT ... SELECT and a view on the
# live table behave alike. ALTER TABLE ... ADD CONSTRAINT does not exist in
# DuckDB, so add_constraints() is stubbed; .die() raises instead of quitting.
if (requireNamespace("duckdb", quietly = TRUE)) {
  assign("add_constraints", function(...) invisible(NULL), envir = env)
  assign(
    ".die",
    function(status, ...) stop("die: ", paste0(..., collapse = "")),
    envir = env
  )
  write_full <- get("write_full", envir = env)
  dcon <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(dcon, "CREATE SCHEMA staging")
  rows <- function(n, cp4) {
    data.table(
      general_id = as.numeric(seq_len(n)),
      year_grab_date = 2026L,
      month_grab_date = 6L,
      cp2021_id_level_4 = cp4,
      cp4_imputed = 0L
    )
  }

  write_full(dcon, rows(3L, "1.1.1.1"), "staging", "gm_test")
  check(
    "first run renames _new into place",
    DBI::dbGetQuery(dcon, "SELECT count(*) n FROM staging.gm_test")$n == 3L
  )

  DBI::dbExecute(
    dcon,
    "CREATE VIEW staging.v_test AS SELECT * FROM staging.gm_test"
  )
  write_full(dcon, rows(5L, "2.2.2.2"), "staging", "gm_test")
  v <- DBI::dbGetQuery(dcon, "SELECT * FROM staging.v_test")
  check(
    "second run replaces the rows, seen through the view",
    nrow(v) == 5L && all(v$cp2021_id_level_4 == "2.2.2.2")
  )
  check(
    "staging table is removed after the copy",
    !DBI::dbExistsTable(
      dcon,
      DBI::Id(schema = "staging", table = "gm_test_new")
    )
  )

  extra <- rows(2L, "3.3.3.3")[, nuova := 1L]
  failed <- tryCatch(
    {
      write_full(dcon, extra, "staging", "gm_test")
      FALSE
    },
    error = function(e) grepl("^die: full write aborted", conditionMessage(e))
  )
  v <- DBI::dbGetQuery(dcon, "SELECT * FROM staging.v_test")
  check(
    "a column the live table lacks aborts and keeps the old rows",
    failed && nrow(v) == 5L && all(v$cp2021_id_level_4 == "2.2.2.2")
  )
  DBI::dbDisconnect(dcon, shutdown = TRUE)
} else {
  cat("  skip  duckdb not installed\n")
}

cat(sprintf("\n%d ok, %d failed\n", ok, fail))
if (fail > 0L) {
  quit(status = 1L, save = "no")
}
