# Source composition -----
#
# Decomposition of the change of an aggregate share into a within-source
# component and the components due to the composition of the sources
# (between, entry, exit), and share series at fixed source weights.

# 0. internal helpers -----

#' Prepare a key x source x time count table and the source totals
#'
#' @return A list with `dt` (key_cols, .s, .t, .x) and `totals`
#'   (.s, .t, .ns), with every row of `dt` matched by a total.
#' @keywords internal
#' @noRd
.sc_prepare <- function(
  counts,
  key_cols,
  source_col,
  time_col,
  x_col,
  n_col,
  source_totals,
  caller
) {
  .si_check_df(counts, "counts", caller)
  .si_check_keys(key_cols, caller)
  .si_check_name(source_col, "source_col", caller)
  .si_check_name(time_col, "time_col", caller)
  .si_check_name(x_col, "x_col", caller)
  .si_check_name(n_col, "n_col", caller)
  if (any(c(source_col, time_col) %in% key_cols)) {
    stop(
      caller,
      ": `source_col` and `time_col` cannot be key columns",
      call. = FALSE
    )
  }
  use_totals <- !is.null(source_totals)
  cols <- c(key_cols, source_col, time_col, x_col, if (!use_totals) n_col)
  check_columns(counts, cols, caller = caller)
  dt <- data.table::as.data.table(counts)[, cols, with = FALSE]
  data.table::setnames(
    dt,
    c(source_col, time_col, x_col, if (!use_totals) n_col),
    c(".s", ".t", ".x", if (!use_totals) ".n")
  )
  if (!is.numeric(dt$.x) || anyNA(dt$.x) || any(dt$.x < 0)) {
    stop(
      caller,
      ": `",
      x_col,
      "` must be a non-negative count without NA",
      call. = FALSE
    )
  }
  if (anyDuplicated(dt, by = c(key_cols, ".s", ".t")) > 0L) {
    stop(
      caller,
      ": duplicated rows for the same key, source and period",
      call. = FALSE
    )
  }

  if (use_totals) {
    .si_check_df(source_totals, "source_totals", caller)
    check_columns(
      source_totals,
      c(source_col, time_col, n_col),
      caller = caller
    )
    tot <- data.table::as.data.table(source_totals)[,
      c(source_col, time_col, n_col),
      with = FALSE
    ]
    data.table::setnames(tot, c(".s", ".t", ".ns"))
    if (anyDuplicated(tot, by = c(".s", ".t")) > 0L) {
      stop(
        caller,
        ": duplicated source and period in `source_totals`",
        call. = FALSE
      )
    }
  } else {
    if (!is.numeric(dt$.n) || anyNA(dt$.n) || any(dt$.n < 0)) {
      stop(
        caller,
        ": `",
        n_col,
        "` must be a non-negative number without NA",
        call. = FALSE
      )
    }
    tot <- unique(dt[, list(.s, .t, .ns = .n)])
    if (anyDuplicated(tot, by = c(".s", ".t")) > 0L) {
      stop(
        caller,
        ": `",
        n_col,
        "` varies across keys within a source and period; ",
        "pass the denominators in `source_totals`",
        call. = FALSE
      )
    }
    dt[, .n := NULL]
  }
  if (!is.numeric(tot$.ns) || anyNA(tot$.ns) || any(tot$.ns < 0)) {
    stop(
      caller,
      ": source denominators must be non-negative without NA",
      call. = FALSE
    )
  }
  dt[tot, .ns := .ns, on = c(".s", ".t")]
  if (anyNA(dt$.ns)) {
    stop(
      caller,
      ": source and period pairs in `counts` missing from `source_totals`",
      call. = FALSE
    )
  }
  if (any(dt$.x > dt$.ns)) {
    stop(
      caller,
      ": counts must not exceed the source denominators",
      call. = FALSE
    )
  }
  dt[, .ns := NULL]
  list(dt = dt, totals = tot)
}

# 1. decompose_share_change -----

#' Decompose the change of a share into within-source and composition
#'
#' Applies the dynamic Olley-Pakes decomposition of Melitz and Polanec
#' (2015) to the aggregate share of each key between two periods (or two
#' windows of periods), with sources in the role of firms.
#'
#' @param counts data.frame or data.table with one row per key, source
#'   and period, holding the count `x_col` and, when `source_totals` is
#'   `NULL`, the source denominator `n_col`.
#' @param key_cols Character vector of key columns.
#' @param source_col Name of the source column (default `"source"`).
#' @param time_col Name of the period column.
#' @param t0,t1 Values of `time_col` of the base and the comparison
#'   period; vectors of several values define windows, over which counts
#'   and denominators are summed by source. They must not overlap.
#' @param x_col,n_col Names of the count and denominator columns
#'   (defaults `"x"`, `"n"`).
#' @param source_totals Optional data.frame with one row per source and
#'   period and columns `source_col`, `time_col`, `n_col` (postings of
#'   the source in the period, also where the key has no posting).
#'   Without it the denominators are read from `counts` and must be the
#'   same for all keys of a source and period.
#'
#' @return A data.table with one row per key of `counts` and columns
#'   `key_cols`, `p0`, `p1`, `delta`, `entro`, `fra`, `ingressi`,
#'   `uscite`, `composizione`. The attribute `fonti` lists the sources
#'   with their denominators `n0`, `n1` and class `classe` (`"C"`
#'   continuing, `"E"` entering, `"X"` exiting).
#'
#' @details
#' The aggregate share is `P = sum_s w_s p_s` with source weights
#' `w_s = n_s / sum(n)` and source shares `p_s = x_s / n_s`. Sources are
#' continuing (C: `n > 0` in both periods), entering (E: only in `t1`) or
#' exiting (X: only in `t0`). With `W_G` the weight of group `G` and
#' `P_G` its aggregate share, Melitz and Polanec (2015) write
#'
#' `P_1 - P_0 = (P_C1 - P_C0) + W_E1 (P_E1 - P_C1) + W_X0 (P_C0 - P_X0)`
#'
#' and split the change among continuing sources as in Olley and Pakes
#' (1996), with weights renormalised within C
#' (`omega_s = n_s / sum_C n`) and averages between the two periods
#' (bars):
#'
#' * `entro = sum_C mean(omega_s) (p_s1 - p_s0)`: change of the shares
#'   within sources;
#' * `fra = sum_C mean(p_s) (omega_s1 - omega_s0)`: reallocation of
#'   postings among continuing sources;
#' * `ingressi = W_E1 (P_E1 - P_C1)`, `uscite = W_X0 (P_C0 - P_X0)`.
#'
#' The four components sum to `delta = p1 - p0` up to rounding error;
#' `composizione = fra + ingressi + uscite`. Groups without sources
#' contribute 0; when there is no continuing source `P_C` is set to 0,
#' so that `ingressi = p1` and `uscite = -p0`.
#'
#' @references
#' Melitz, M. J. and Polanec, S. (2015). Dynamic Olley-Pakes productivity
#' decomposition with entry and exit. *RAND Journal of Economics*, 46(2),
#' 362-375.
#'
#' Olley, G. S. and Pakes, A. (1996). The dynamics of productivity in the
#' telecommunications equipment industry. *Econometrica*, 64(6),
#' 1263-1297.
#'
#' @examples
#' counts <- data.frame(
#'   code = "a",
#'   source = c("s1", "s2", "s1", "s2", "s3"),
#'   mese = c(1, 1, 2, 2, 2),
#'   x = c(10, 5, 12, 20, 30)
#' )
#' totals <- data.frame(
#'   source = c("s1", "s2", "s1", "s2", "s3"),
#'   mese = c(1, 1, 2, 2, 2),
#'   n = c(100, 100, 100, 200, 100)
#' )
#' decompose_share_change(counts, "code", time_col = "mese", t0 = 1,
#'                        t1 = 2, source_totals = totals)
#'
#' @export
decompose_share_change <- function(
  counts,
  key_cols,
  source_col = "source",
  time_col,
  t0,
  t1,
  x_col = "x",
  n_col = "n",
  source_totals = NULL
) {
  caller <- "decompose_share_change"
  if (length(t0) == 0L || length(t1) == 0L || anyNA(t0) || anyNA(t1)) {
    stop(caller, ": `t0` and `t1` must be non-empty without NA", call. = FALSE)
  }
  if (length(intersect(t0, t1)) > 0L) {
    stop(caller, ": `t0` and `t1` must not overlap", call. = FALSE)
  }
  prep <- .sc_prepare(
    counts,
    key_cols,
    source_col,
    time_col,
    x_col,
    n_col,
    source_totals,
    caller
  )
  dt <- prep$dt
  tot <- prep$totals

  # 1a. sources and their classes
  src <- tot[
    .t %in% c(t0, t1),
    list(n0 = sum(.ns[.t %in% t0]), n1 = sum(.ns[.t %in% t1])),
    by = .s
  ]
  n0_all <- sum(src$n0)
  n1_all <- sum(src$n1)
  if (n0_all <= 0 || n1_all <= 0) {
    stop(
      caller,
      ": the denominators of `t0` and `t1` must be positive",
      call. = FALSE
    )
  }
  src[,
    classe := data.table::fcase(
      n0 > 0 & n1 > 0  , "C" ,
      n0 == 0 & n1 > 0 , "E" ,
      n0 > 0 & n1 == 0 , "X" ,
      default = "-"
    )
  ]
  nc0 <- src[classe == "C", sum(n0)]
  nc1 <- src[classe == "C", sum(n1)]
  ne1 <- src[classe == "E", sum(n1)]
  nx0 <- src[classe == "X", sum(n0)]
  src[, `:=`(om0 = 0, om1 = 0)]
  if (nc0 > 0) {
    src[classe == "C", `:=`(om0 = n0 / nc0, om1 = n1 / nc1)]
  }

  # 1b. key x source counts on the two windows
  xs <- dt[
    .t %in% c(t0, t1),
    list(x0 = sum(.x[.t %in% t0]), x1 = sum(.x[.t %in% t1])),
    by = c(key_cols, ".s")
  ]
  xs[
    src,
    on = ".s",
    `:=`(
      n0 = i.n0,
      n1 = i.n1,
      classe = i.classe,
      om0 = i.om0,
      om1 = i.om1
    )
  ]
  xs[, `:=`(
    p0s = data.table::fifelse(classe == "C", x0 / n0, 0),
    p1s = data.table::fifelse(classe == "C", x1 / n1, 0)
  )]
  agg <- xs[,
    list(
      .x0 = sum(x0),
      .x1 = sum(x1),
      entro = sum(((om0 + om1) / 2 * (p1s - p0s))[classe == "C"]),
      fra = sum(((p0s + p1s) / 2 * (om1 - om0))[classe == "C"]),
      .xc0 = sum(x0[classe == "C"]),
      .xc1 = sum(x1[classe == "C"]),
      .xe1 = sum(x1[classe == "E"]),
      .xx0 = sum(x0[classe == "X"])
    ),
    by = key_cols
  ]

  # 1c. all keys of counts, zeros where absent from the windows
  out <- unique(dt[, key_cols, with = FALSE])
  out <- agg[out, on = key_cols]
  num <- c("entro", "fra", ".x0", ".x1", ".xc0", ".xc1", ".xe1", ".xx0")
  for (v in num) {
    data.table::set(out, which(is.na(out[[v]])), v, 0)
  }
  pc0 <- if (nc0 > 0) out$.xc0 / nc0 else 0
  pc1 <- if (nc1 > 0) out$.xc1 / nc1 else 0
  out[, `:=`(p0 = .x0 / n0_all, p1 = .x1 / n1_all)]
  out[, delta := p1 - p0]
  out[, ingressi := if (ne1 > 0) (ne1 / n1_all) * (.xe1 / ne1 - pc1) else 0]
  out[, uscite := if (nx0 > 0) (nx0 / n0_all) * (pc0 - .xx0 / nx0) else 0]
  out[, composizione := fra + ingressi + uscite]
  out <- out[,
    c(
      key_cols,
      "p0",
      "p1",
      "delta",
      "entro",
      "fra",
      "ingressi",
      "uscite",
      "composizione"
    ),
    with = FALSE
  ]
  fonti <- src[, list(.s, n0, n1, classe)]
  data.table::setnames(fonti, ".s", source_col)
  data.table::setattr(out, "fonti", fonti)
  out[]
}

# 2. fixed_weight_share -----

#' Share series at fixed source weights
#'
#' Computes for every key and period the average of the source shares
#' with weights fixed over the whole period, renormalised over the
#' sources present in each period, next to the observed share.
#'
#' @inheritParams decompose_share_change
#' @param weights Optional named numeric vector of non-negative source
#'   weights (names = sources). By default the weight of a source is its
#'   share of the postings over all the periods of `source_totals` (or of
#'   `counts`).
#'
#' @return A data.table with one row per key and period (all keys of
#'   `counts` crossed with all periods with at least one source) and
#'   columns `key_cols`, `time_col`, `quota_fissa`, `quota_osservata`,
#'   `n_fonti` (sources with a positive denominator in the period).
#'
#' @details
#' With source shares `p_kst = x_kst / n_st` and fixed weights `w_s`,
#'
#' `quota_fissa_kt = sum_{s in S_t} w_s p_kst / sum_{s in S_t} w_s`
#'
#' where `S_t` is the set of sources with `n_st > 0`. The observed share
#' is `sum_s x_kst / sum_s n_st`. With fixed weights the reallocation of
#' postings among continuing sources does not move the series, so the
#' difference between the two series measures the composition effect
#' (see [decompose_share_change()]). Entry and exit of sources still
#' change `S_t`. When all the sources of a period have weight 0,
#' `quota_fissa` is `NA`.
#'
#' @examples
#' counts <- data.frame(
#'   code = "a",
#'   source = c("s1", "s2", "s1", "s2"),
#'   mese = c(1, 1, 2, 2),
#'   x = c(10, 5, 10, 20)
#' )
#' totals <- data.frame(
#'   source = c("s1", "s2", "s1", "s2"),
#'   mese = c(1, 1, 2, 2),
#'   n = c(100, 100, 100, 400)
#' )
#' fixed_weight_share(counts, "code", time_col = "mese",
#'                    source_totals = totals)
#'
#' @export
fixed_weight_share <- function(
  counts,
  key_cols,
  source_col = "source",
  time_col,
  weights = NULL,
  x_col = "x",
  n_col = "n",
  source_totals = NULL
) {
  caller <- "fixed_weight_share"
  prep <- .sc_prepare(
    counts,
    key_cols,
    source_col,
    time_col,
    x_col,
    n_col,
    source_totals,
    caller
  )
  dt <- prep$dt
  tot <- prep$totals[.ns > 0]
  if (nrow(tot) == 0L) {
    stop(caller, ": no source with a positive denominator", call. = FALSE)
  }

  if (is.null(weights)) {
    wt <- tot[, list(.w = sum(.ns)), by = .s]
    wt[, .w := .w / sum(.w)]
  } else {
    if (
      !is.numeric(weights) ||
        is.null(names(weights)) ||
        anyNA(weights) ||
        any(weights < 0)
    ) {
      stop(
        caller,
        ": `weights` must be a named numeric vector of non-negative values",
        call. = FALSE
      )
    }
    miss <- setdiff(unique(tot$.s), names(weights))
    if (length(miss) > 0L) {
      stop(
        caller,
        ": `weights` missing for sources: ",
        paste(utils::head(miss, 5L), collapse = ", "),
        call. = FALSE
      )
    }
    wt <- data.table::data.table(.s = names(weights), .w = unname(weights))
  }
  tot[wt, .w := .w, on = ".s"]
  per <- tot[, list(.wt = sum(.w), .nt = sum(.ns), n_fonti = .N), by = .t]

  dt <- dt[.x > 0]
  dt[tot, `:=`(.ns = i..ns, .w = i..w), on = c(".s", ".t")]
  dt <- dt[!is.na(.ns)]
  agg <- dt[,
    list(.num = sum(.w * .x / .ns), .xo = sum(.x)),
    by = c(key_cols, ".t")
  ]

  keys <- unique(prep$dt[, key_cols, with = FALSE])
  nk <- nrow(keys)
  times <- sort(unique(per$.t))
  grid <- keys[rep(seq_len(nk), each = length(times))]
  grid[, .t := rep(times, nk)]
  grid <- agg[grid, on = c(key_cols, ".t")]
  grid[is.na(.num), .num := 0]
  grid[is.na(.xo), .xo := 0]
  grid[per, `:=`(.wt = i..wt, .nt = i..nt, n_fonti = i.n_fonti), on = ".t"]
  grid[, quota_fissa := data.table::fifelse(.wt > 0, .num / .wt, NA_real_)]
  grid[, quota_osservata := .xo / .nt]
  grid[, n_fonti := as.integer(n_fonti)]
  out <- grid[,
    c(key_cols, ".t", "quota_fissa", "quota_osservata", "n_fonti"),
    with = FALSE
  ]
  data.table::setnames(out, ".t", time_col)
  data.table::setorderv(out, c(key_cols, time_col))
  out[]
}
