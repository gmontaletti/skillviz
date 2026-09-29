# Emerging skills -----
#
# Indicators of skill emergence built on monthly share panels: share
# panels, drift-net logit trends, year-on-year ratios, acceleration,
# onset, revealed comparative advantage, diffusion over professions,
# taxonomy drift, support calibration, composite scoring and a
# pseudo-prospective backtest of the score.

# 0. internal helpers -----

#' Validate and standardise a share panel
#'
#' Copies the needed columns and renames time, count and denominator to
#' the internal names `.t`, `.x`, `.n`.
#'
#' @param panel data.frame or data.table with one row per key and period.
#' @param key_cols Character vector of key columns.
#' @param time_col,x_col,n_col Names of the time index, count and
#'   denominator columns.
#' @param caller Name of the calling function for error messages.
#' @param need_n Logical, whether the denominator column is required.
#' @return A data.table with columns `key_cols`, `.t`, `.x` and, when
#'   `need_n = TRUE`, `.n`.
#' @keywords internal
#' @noRd
.em_prepare <- function(
  panel,
  key_cols,
  time_col,
  x_col,
  n_col,
  caller,
  need_n = TRUE
) {
  if (!is.data.frame(panel)) {
    stop(caller, ": `panel` must be a data.frame or data.table", call. = FALSE)
  }
  if (!is.character(key_cols) || length(key_cols) == 0L) {
    stop(
      caller,
      ": `key_cols` must be a non-empty character vector",
      call. = FALSE
    )
  }
  cols <- c(key_cols, time_col, x_col, if (need_n) n_col)
  check_columns(panel, cols, caller = caller)
  if (any(key_cols %in% c(".t", ".x", ".n"))) {
    stop(
      caller,
      ": `key_cols` cannot use the reserved names .t, .x, .n",
      call. = FALSE
    )
  }

  dt <- data.table::as.data.table(panel)[, cols, with = FALSE]
  new_names <- c(".t", ".x", if (need_n) ".n")
  data.table::setnames(dt, c(time_col, x_col, if (need_n) n_col), new_names)

  if (!is.numeric(dt$.t)) {
    stop(
      caller,
      ": `",
      time_col,
      "` must be a numeric month index",
      call. = FALSE
    )
  }
  if (!is.numeric(dt$.x) || any(dt$.x < 0, na.rm = TRUE)) {
    stop(caller, ": `", x_col, "` must be a non-negative count", call. = FALSE)
  }
  if (need_n) {
    if (!is.numeric(dt$.n) || any(dt$.x > dt$.n, na.rm = TRUE)) {
      stop(
        caller,
        ": `",
        x_col,
        "` must not exceed `",
        n_col,
        "`",
        call. = FALSE
      )
    }
  }
  if (anyDuplicated(dt, by = c(key_cols, ".t")) > 0L) {
    stop(caller, ": duplicated rows for the same key and period", call. = FALSE)
  }
  dt
}

#' Check a time window argument
#'
#' @param w Numeric vector of length 2 (first and last period).
#' @param name Argument name for messages.
#' @param caller Calling function.
#' @return `w` as numeric, invisibly checked.
#' @keywords internal
#' @noRd
.em_check_window <- function(w, name, caller) {
  if (!is.numeric(w) || length(w) != 2L || anyNA(w) || w[1L] > w[2L]) {
    stop(
      caller,
      ": `",
      name,
      "` must be a numeric vector c(from, to) ",
      "with from <= to",
      call. = FALSE
    )
  }
  as.numeric(w)
}

#' Robust z-score (median / MAD)
#'
#' Falls back to the standard deviation when the MAD is zero, and to
#' zero when both are zero.
#'
#' @param v Numeric vector.
#' @return Numeric vector of robust z-scores.
#' @keywords internal
#' @noRd
.robust_z <- function(v) {
  med <- stats::median(v, na.rm = TRUE)
  s <- stats::mad(v, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    s <- stats::sd(v, na.rm = TRUE)
  }
  if (is.na(s) || s == 0) {
    return(ifelse(is.na(v), NA_real_, 0))
  }
  (v - med) / s
}

#' Weighted median
#'
#' Lower weighted median: the smallest value whose cumulative weight
#' reaches half of the total weight.
#'
#' @param v Numeric vector.
#' @param w Non-negative weights.
#' @return A single numeric value.
#' @keywords internal
#' @noRd
.weighted_median <- function(v, w) {
  ok <- !is.na(v) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  v <- v[ok]
  w <- w[ok]
  o <- order(v)
  cw <- cumsum(w[o])
  v[o][which(cw >= 0.5 * cw[length(cw)])[1L]]
}

#' Evaluate an expression with a fixed seed, restoring the RNG state
#'
#' @param seed Integer seed.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.with_seed <- function(seed, expr) {
  env <- globalenv()
  had_seed <- exists(".Random.seed", envir = env, inherits = FALSE)
  if (had_seed) {
    old <- get(".Random.seed", envir = env, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old, envir = env)
    } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
      rm(".Random.seed", envir = env)
    }
  })
  set.seed(seed)
  expr
}

#' Count panel from posting-level incidence
#'
#' @param inc data.table with posting id, time, group and skill columns,
#'   one row per posting and skill.
#' @inheritParams compute_share_panel
#' @return A data.table with `group_cols`, `skill_col`, `time_col`, `x`,
#'   `n`, `quota`.
#' @keywords internal
#' @noRd
.panel_from_incidence <- function(
  inc,
  id_col,
  time_col,
  skill_col,
  group_cols,
  fill_zero
) {
  inc <- unique(inc[, c(id_col, time_col, group_cols, skill_col), with = FALSE])
  ids <- unique(inc[, c(id_col, time_col, group_cols), with = FALSE])
  if (anyDuplicated(ids, by = id_col) > 0L) {
    stop(
      "compute_share_panel: each posting must belong to a single ",
      "period and group",
      call. = FALSE
    )
  }

  den <- ids[, list(n = .N), by = c(group_cols, time_col)]
  num <- inc[, list(x = .N), by = c(group_cols, skill_col, time_col)]

  if (fill_zero) {
    pairs <- unique(num[, c(group_cols, skill_col), with = FALSE])
    if (is.null(group_cols)) {
      pairs[, .cj := 1L]
      den2 <- data.table::copy(den)[, .cj := 1L]
      grid <- merge(pairs, den2, by = ".cj", allow.cartesian = TRUE)
      grid[, .cj := NULL]
    } else {
      grid <- merge(pairs, den, by = group_cols, allow.cartesian = TRUE)
    }
    out <- merge(
      grid,
      num,
      by = c(group_cols, skill_col, time_col),
      all.x = TRUE
    )
    out[is.na(x), x := 0L]
  } else {
    out <- merge(num, den, by = c(group_cols, time_col))
  }

  out[, quota := x / n]
  data.table::setcolorder(
    out,
    c(group_cols, skill_col, time_col, "x", "n", "quota")
  )
  data.table::setkeyv(out, c(group_cols, skill_col, time_col))
  out[]
}

#' Closed-form drift-net WLS trend of the logit share
#'
#' @param dt Standardised panel from `.em_prepare()`.
#' @param key_cols Key columns.
#' @param t_from,t_to Window bounds (inclusive).
#' @param break_times Numeric vector of break periods or `NULL`.
#' @param drift data.table with `.t` and `drift_livello`, or `NULL`.
#' @param weights `"pooled"` or `"observed"` (see [compute_share_trend()]).
#' @return A data.table with one row per key.
#' @keywords internal
#' @noRd
.em_trend_core <- function(
  dt,
  key_cols,
  t_from,
  t_to,
  break_times,
  drift,
  weights = "pooled"
) {
  d <- dt[.t >= t_from & .t <= t_to & .n > 0]
  brk <- sort(unique(break_times))

  seg_of <- function(t) {
    if (length(brk) == 0L) integer(length(t)) else findInterval(t, brk)
  }

  d[, `:=`(
    .t = as.numeric(.t),
    .x = as.numeric(.x),
    .n = as.numeric(.n)
  )]
  d[, `:=`(
    .y = stats::qlogis((.x + 0.5) / (.n + 1)),
    .seg = seg_of(.t)
  )]
  if (weights == "observed") {
    d[, .w := 1 / (1 / (.x + 0.5) + 1 / (.n - .x + 0.5))]
  } else {
    # binomial variance at the pooled share of the series in the window
    d[, .p := sum(.x) / sum(.n), by = key_cols]
    d[, .w := 1 / (1 / (.n * .p + 0.5) + 1 / (.n * (1 - .p) + 0.5))]
  }
  # Within-segment weighted centring = partialling out intercept and
  # step dummies (Frisch-Waugh-Lovell).
  d[,
    `:=`(
      .tc = .t - sum(.w * .t) / sum(.w),
      .yc = .y - sum(.w * .y) / sum(.w)
    ),
    by = c(key_cols, ".seg")
  ]

  res <- d[,
    {
      sxx <- sum(.w * .tc^2)
      b <- if (sxx > 0) sum(.w * .tc * .yc) / sxx else NA_real_
      dfree <- .N - 1L - data.table::uniqueN(.seg)
      chi2 <- if (is.na(b)) NA_real_ else sum(.w * (.yc - b * .tc)^2)
      phi <- if (dfree > 0L && !is.na(chi2)) max(1, chi2 / dfree) else NA_real_
      list(
        n_mesi = .N,
        pendenza_lorda = b,
        se = if (is.na(phi)) NA_real_ else sqrt(phi / sxx),
        phi = phi
      )
    },
    by = key_cols
  ]

  # drift slope over the same window and segments, unit weights
  b_drift <- 0
  if (!is.null(drift)) {
    dd <- drift[.t >= t_from & .t <= t_to & !is.na(drift_livello)]
    if (nrow(dd) >= 2L) {
      dd[, .seg := seg_of(.t)]
      dd[,
        `:=`(
          .tc = .t - mean(.t),
          .yc = drift_livello - mean(drift_livello)
        ),
        by = .seg
      ]
      sxx_d <- sum(dd$.tc^2)
      b_drift <- if (sxx_d > 0) sum(dd$.tc * dd$.yc) / sxx_d else 0
    }
  }

  res[, `:=`(
    pendenza_drift = b_drift,
    pendenza = pendenza_lorda - b_drift
  )]
  res[, z := pendenza / se]
  res[, p_value := 2 * stats::pnorm(-abs(z))]
  res[, p_adj := stats::p.adjust(p_value, method = "BH")]
  data.table::setcolorder(
    res,
    c(
      key_cols,
      "n_mesi",
      "pendenza",
      "se",
      "z",
      "p_value",
      "p_adj",
      "phi",
      "pendenza_lorda",
      "pendenza_drift"
    )
  )
  res[]
}

#' Standardise a drift table for `.em_trend_core()`
#'
#' @param drift Output of [compute_drift_index()] or `NULL`.
#' @param time_col Time column name.
#' @param caller Calling function.
#' @return A data.table with `.t` and `drift_livello`, or `NULL`.
#' @keywords internal
#' @noRd
.em_prepare_drift <- function(drift, time_col, caller) {
  if (is.null(drift)) {
    return(NULL)
  }
  if (!is.data.frame(drift)) {
    stop(caller, ": `drift` must be a data.frame or NULL", call. = FALSE)
  }
  check_columns(drift, c(time_col, "drift_livello"), caller = caller)
  dd <- data.table::as.data.table(drift)[,
    c(time_col, "drift_livello"),
    with = FALSE
  ]
  data.table::setnames(dd, time_col, ".t")
  dd
}

#' Match the `weights` argument of the trend functions
#'
#' @param weights Character, `"pooled"` or `"observed"`.
#' @param caller Calling function.
#' @return The matched value.
#' @keywords internal
#' @noRd
.em_match_weights <- function(weights, caller) {
  choices <- c("pooled", "observed")
  if (identical(weights, choices)) {
    return("pooled")
  }
  if (!is.character(weights) || length(weights) != 1L ||
    !weights %in% choices) {
    stop(caller, ": `weights` must be \"pooled\" or \"observed\"",
      call. = FALSE)
  }
  weights
}

# 1. compute_share_panel -----

#' Monthly skill share panel from posting-level incidence
#'
#' Builds the share panel \eqn{s_{k,t} = x_{k,t} / n_t}, where
#' \eqn{x_{k,t}} is the number of postings of period \eqn{t} that
#' mention skill \eqn{k} and \eqn{n_t} is the number of postings of
#' period \eqn{t} with at least one skill. When `group_cols` are given
#' (e.g. a profession code) counts and denominators are computed within
#' each group.
#'
#' @param incidence A data.frame/data.table with one row per posting and
#'   skill (duplicates are removed). Must contain `id_col`, `time_col`,
#'   `skill_col` and any `group_cols`. Each posting must belong to a
#'   single period and group.
#' @param id_col Posting identifier column. Default `"general_id"`.
#' @param time_col Period column (typically an integer month index).
#'   Default `"mese_idx"`.
#' @param skill_col Skill identifier column. Default `"skill_id"`.
#' @param group_cols Optional character vector of grouping columns (e.g.
#'   `"cp4"`). Default `NULL` (global panel).
#' @param fill_zero Logical. When `TRUE` (default) every group-skill pair
#'   observed at least once gets a row for every period in which its
#'   group has postings, with `x = 0` where the skill is absent.
#' @return A data.table with columns `group_cols`, `skill_col`,
#'   `time_col`, `x` (postings with the skill), `n` (postings with at
#'   least one skill) and `quota` (`x / n`), keyed by the first three.
#' @examples
#' inc <- data.table::data.table(
#'   general_id = c(1, 1, 2, 3, 3, 4),
#'   mese_idx = c(1, 1, 1, 2, 2, 2),
#'   skill_id = c("a", "b", "a", "a", "c", "b")
#' )
#' compute_share_panel(inc)
#' @export
compute_share_panel <- function(
  incidence,
  id_col = "general_id",
  time_col = "mese_idx",
  skill_col = "skill_id",
  group_cols = NULL,
  fill_zero = TRUE
) {
  if (!is.data.frame(incidence)) {
    stop("compute_share_panel: `incidence` must be a data.frame", call. = FALSE)
  }
  check_columns(
    incidence,
    c(id_col, time_col, skill_col, group_cols),
    caller = "compute_share_panel"
  )
  if (!is.logical(fill_zero) || length(fill_zero) != 1L || is.na(fill_zero)) {
    stop(
      "compute_share_panel: `fill_zero` must be TRUE or FALSE",
      call. = FALSE
    )
  }
  inc <- data.table::as.data.table(incidence)
  .panel_from_incidence(inc, id_col, time_col, skill_col, group_cols, fill_zero)
}

# 2. compute_share_trend -----

#' Drift-net logit trend of skill shares
#'
#' Estimates, for every series, the slope of the empirical logit share on
#' the time index by weighted least squares, optionally with step dummies
#' at break periods and net of a common drift slope. The computation is
#' closed-form and vectorised over series.
#'
#' @details
#' For each series the response is
#' \eqn{y_t = \mathrm{logit}((x_t + 0.5)/(n_t + 1))}, weighted by the
#' inverse of the approximate sampling variance of the empirical logit.
#' With `weights = "pooled"` (default) the variance is evaluated at the
#' pooled share of the series over the window,
#' \eqn{\bar p = \sum_t x_t / \sum_t n_t}:
#' \eqn{w_t = 1 / (1/(n_t \bar p + 0.5) + 1/(n_t (1 - \bar p) + 0.5))},
#' where the 0.5 terms keep the weights finite when \eqn{\bar p} is 0 or 1.
#' With `weights = "observed"` it is evaluated at the observed count,
#' \eqn{w_t = 1 / (1/(x_t + 0.5) + 1/(n_t - x_t + 0.5))}. Observed weights
#' correlate with the response: a month with \eqn{x_t = 0} weighs about
#' 0.5 whatever \eqn{n_t}, while a month with a large count weighs about
#' \eqn{x_t}, so a series that spikes and then falls to zero keeps a large
#' positive slope driven by the spike. Pooled weights depend only on
#' \eqn{n_t} within a series and do not have this bias; on a series with
#' constant share the two options coincide up to sampling noise. Break
#' periods `break_times` add a step dummy \eqn{1\{t \ge b\}}; together
#' with the intercept they define segment-specific intercepts, so the
#' slope is obtained after weighted centring of \eqn{t} and \eqn{y}
#' within each segment (Frisch-Waugh-Lovell).
#'
#' Overdispersion is handled by the quasi-likelihood factor
#' \eqn{\phi = \max(1, X^2 / df)}, where \eqn{X^2} is the weighted
#' residual sum of squares (Pearson statistic) and
#' \eqn{df = T - 1 - S} with \eqn{S} segments; the standard error is
#' \eqn{\sqrt{\phi / \sum_t w_t \tilde t_t^2}}.
#'
#' When `drift` is supplied the slope \eqn{\beta_D} of the cumulative
#' drift level `drift_livello` (see [compute_drift_index()]) is estimated
#' by OLS on the same window and segments and subtracted:
#' `pendenza` \eqn{= \beta_k - \beta_D}. The uncertainty of
#' \eqn{\beta_D} is ignored, since it is common to all series and
#' estimated from all of them. Two-sided p-values use the normal
#' approximation; `p_adj` applies the Benjamini-Hochberg correction to
#' all series of the call, which therefore defines the testing family.
#'
#' @param panel A share panel with one row per key and period, e.g. from
#'   [compute_share_panel()]. Periods with `n = 0` are ignored.
#' @param key_cols Character vector of columns identifying a series
#'   (e.g. `"skill_id"` or `c("cp4", "skill_id")`).
#' @param time_col Numeric period index column. Default `"mese_idx"`.
#' @param x_col Count column. Default `"x"`.
#' @param n_col Denominator column. Default `"n"`.
#' @param window Number of trailing periods ending at `end` used for the
#'   fit (e.g. `12` or `24`). `NULL` (default) uses all periods up to
#'   `end`.
#' @param end Last period of the window. Default: the maximum period in
#'   `panel`.
#' @param break_times Optional numeric vector of break periods; each adds
#'   a step dummy. Default `NULL`.
#' @param drift Optional data.table with `time_col` and `drift_livello`,
#'   as returned by [compute_drift_index()]. Default `NULL` (no drift
#'   correction).
#' @param weights Weighting of the least squares: `"pooled"` (default,
#'   binomial variance at the pooled share of the series in the window) or
#'   `"observed"` (variance at the observed count of each period, the
#'   behaviour of version 0.6.0). See Details.
#' @return A data.table with one row per series: `key_cols`, `n_mesi`
#'   (periods used), `pendenza` (drift-net slope per period on the logit
#'   scale), `se`, `z`, `p_value`, `p_adj` (BH), `phi` (overdispersion
#'   factor), `pendenza_lorda` (slope before drift correction) and
#'   `pendenza_drift` (drift slope subtracted).
#' @examples
#' set.seed(1)
#' panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = 1:24)
#' panel[, n := 2000L]
#' panel[, x := rbinom(.N, n, plogis(-3 + ifelse(skill_id == "a", 0.05, 0) *
#'   mese_idx))]
#' compute_share_trend(panel, key_cols = "skill_id", window = 24)
#' @export
compute_share_trend <- function(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  window = NULL,
  end = NULL,
  break_times = NULL,
  drift = NULL,
  weights = c("pooled", "observed")
) {
  caller <- "compute_share_trend"
  weights <- .em_match_weights(weights, caller)
  dt <- .em_prepare(panel, key_cols, time_col, x_col, n_col, caller)
  if (
    !is.null(window) &&
      (!is.numeric(window) || length(window) != 1L || window < 3)
  ) {
    stop(
      caller,
      ": `window` must be a single number >= 3 or NULL",
      call. = FALSE
    )
  }
  if (!is.null(break_times) && !is.numeric(break_times)) {
    stop(caller, ": `break_times` must be numeric or NULL", call. = FALSE)
  }
  dd <- .em_prepare_drift(drift, time_col, caller)

  if (is.null(end)) {
    end <- max(dt$.t)
  }
  t_from <- if (is.null(window)) min(dt$.t) else end - window + 1
  .em_trend_core(dt, key_cols, t_from, end, break_times, dd, weights)
}

# 3. compute_yoy_ratio -----

#' Year-on-year ratio of pooled shares with Katz confidence interval
#'
#' Compares the pooled share of the last `months` periods ending at
#' `end` with the pooled share of the same periods `lag` periods
#' earlier.
#'
#' @details
#' With \eqn{x_1, n_1} the counts and denominators summed over the
#' current block and \eqn{x_0, n_0} over the reference block, the ratio
#' is \eqn{R = (x_1/n_1)/(x_0/n_0)}. The Katz interval uses
#' \eqn{SE(\log R) = \sqrt{1/x_1 - 1/n_1 + 1/x_0 - 1/n_0}}. When either
#' count is zero, 0.5 is added to both counts and both denominators of
#' that series.
#'
#' @inheritParams compute_share_trend
#' @param months Number of periods pooled in each block. Default `3`.
#' @param lag Distance in periods between the two blocks. Default `12`.
#' @param conf Confidence level of the interval. Default `0.95`.
#' @return A data.table with `key_cols`, `x_attuale`, `n_attuale`,
#'   `x_precedente`, `n_precedente`, `yoy` (ratio), `yoy_lo`, `yoy_hi`,
#'   `log_yoy`, `se_log` and `p_value` (two-sided test of `yoy = 1`).
#'   Series with a zero denominator in either block get `NA`.
#' @examples
#' panel <- data.table::CJ(skill_id = "a", mese_idx = 1:15)
#' panel[, `:=`(n = 1000L, x = ifelse(mese_idx > 12, 60L, 30L))]
#' compute_yoy_ratio(panel, key_cols = "skill_id")
#' @export
compute_yoy_ratio <- function(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  months = 3L,
  lag = 12L,
  end = NULL,
  conf = 0.95
) {
  caller <- "compute_yoy_ratio"
  dt <- .em_prepare(panel, key_cols, time_col, x_col, n_col, caller)
  if (!is.numeric(months) || length(months) != 1L || months < 1) {
    stop(caller, ": `months` must be a positive number", call. = FALSE)
  }
  if (!is.numeric(lag) || length(lag) != 1L || lag < months) {
    stop(caller, ": `lag` must be a number >= `months`", call. = FALSE)
  }
  if (!is.numeric(conf) || length(conf) != 1L || conf <= 0 || conf >= 1) {
    stop(caller, ": `conf` must be in (0, 1)", call. = FALSE)
  }
  if (is.null(end)) {
    end <- max(dt$.t)
  }
  cur <- seq(end - months + 1, end)
  prev <- cur - lag

  res <- dt[
    .t %in% c(cur, prev),
    list(
      x_attuale = sum(.x[.t %in% cur]),
      n_attuale = sum(.n[.t %in% cur]),
      x_precedente = sum(.x[.t %in% prev]),
      n_precedente = sum(.n[.t %in% prev])
    ),
    by = key_cols
  ]

  zq <- stats::qnorm(1 - (1 - conf) / 2)
  res[, `:=`(
    .x1 = as.numeric(x_attuale),
    .n1 = as.numeric(n_attuale),
    .x0 = as.numeric(x_precedente),
    .n0 = as.numeric(n_precedente)
  )]
  res[
    .x1 == 0 | .x0 == 0,
    `:=`(.x1 = .x1 + 0.5, .n1 = .n1 + 0.5, .x0 = .x0 + 0.5, .n0 = .n0 + 0.5)
  ]
  res[, `:=`(log_yoy = NA_real_, se_log = NA_real_)]
  res[
    n_attuale > 0 & n_precedente > 0,
    `:=`(
      log_yoy = log((.x1 / .n1) / (.x0 / .n0)),
      se_log = sqrt(1 / .x1 - 1 / .n1 + 1 / .x0 - 1 / .n0)
    )
  ]
  res[, `:=`(
    yoy = exp(log_yoy),
    yoy_lo = exp(log_yoy - zq * se_log),
    yoy_hi = exp(log_yoy + zq * se_log),
    p_value = 2 * stats::pnorm(-abs(log_yoy / se_log))
  )]
  res[, c(".x1", ".n1", ".x0", ".n0") := NULL]
  data.table::setcolorder(
    res,
    c(
      key_cols,
      "x_attuale",
      "n_attuale",
      "x_precedente",
      "n_precedente",
      "yoy",
      "yoy_lo",
      "yoy_hi",
      "log_yoy",
      "se_log",
      "p_value"
    )
  )
  res[]
}

# 4. compute_share_acceleration -----

#' Acceleration of the logit share trend
#'
#' Difference between the drift-net slope of the last `window` periods
#' and the slope of the `window` periods before them.
#'
#' @details
#' Both slopes are estimated as in [compute_share_trend()], with the same
#' `weights` option; pooled weights use the pooled share of each window. The
#' acceleration is \eqn{a = \beta_{recent} - \beta_{previous}} with
#' \eqn{SE(a) = \sqrt{SE_{recent}^2 + SE_{previous}^2}} (the two windows
#' are disjoint), \eqn{z = a / SE(a)}, a two-sided normal p-value and a
#' BH-adjusted p-value over the series of the call.
#'
#' @inheritParams compute_share_trend
#' @param window Length of each of the two windows. Default `12`.
#' @return A data.table with `key_cols`, `pendenza_recente`,
#'   `pendenza_precedente`, `accelerazione`, `se`, `z`, `p_value`,
#'   `p_adj`.
#' @examples
#' set.seed(2)
#' panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = 1:24)
#' panel[, n := 5000L]
#' panel[, x := rbinom(.N, n, plogis(-3 + 0.08 * pmax(0, mese_idx - 12)))]
#' compute_share_acceleration(panel, key_cols = "skill_id")
#' @export
compute_share_acceleration <- function(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  window = 12L,
  end = NULL,
  break_times = NULL,
  drift = NULL,
  weights = c("pooled", "observed")
) {
  caller <- "compute_share_acceleration"
  weights <- .em_match_weights(weights, caller)
  dt <- .em_prepare(panel, key_cols, time_col, x_col, n_col, caller)
  if (!is.numeric(window) || length(window) != 1L || window < 3) {
    stop(caller, ": `window` must be a single number >= 3", call. = FALSE)
  }
  dd <- .em_prepare_drift(drift, time_col, caller)
  if (is.null(end)) {
    end <- max(dt$.t)
  }
  rec <- .em_trend_core(
    dt,
    key_cols,
    end - window + 1,
    end,
    break_times,
    dd,
    weights
  )
  prv <- .em_trend_core(
    dt,
    key_cols,
    end - 2 * window + 1,
    end - window,
    break_times,
    dd,
    weights
  )

  res <- merge(
    rec[, c(key_cols, "pendenza", "se"), with = FALSE],
    prv[, c(key_cols, "pendenza", "se"), with = FALSE],
    by = key_cols,
    suffixes = c("_recente", "_precedente")
  )
  res[, `:=`(
    accelerazione = pendenza_recente - pendenza_precedente,
    se = sqrt(se_recente^2 + se_precedente^2)
  )]
  res[, z := accelerazione / se]
  res[, p_value := 2 * stats::pnorm(-abs(z))]
  res[, p_adj := stats::p.adjust(p_value, method = "BH")]
  res[, c("se_recente", "se_precedente") := NULL]
  data.table::setcolorder(
    res,
    c(
      key_cols,
      "pendenza_recente",
      "pendenza_precedente",
      "accelerazione",
      "se",
      "z",
      "p_value",
      "p_adj"
    )
  )
  res[]
}

# 5. detect_skill_onset -----

#' First month of consolidated presence of each series
#'
#' The onset is the first period in which the rolling sum of counts over
#' `roll` consecutive periods reaches `min_count`.
#'
#' @details
#' Periods missing from the panel count as zero. Before the first period
#' of the panel nothing is observed, so the rolling sum of the first
#' `roll - 1` periods covers fewer months. An onset within the first
#' `censor_months` periods of the panel is left-censored
#' (`censura_sx = TRUE`): the skill may have been present before the
#' observation window. `eta_mesi = end - prima_comparsa`.
#'
#' @inheritParams compute_share_trend
#' @param min_count Minimum rolling count defining the onset. Default
#'   `10`.
#' @param roll Rolling window length in periods. Default `3`.
#' @param censor_months Number of initial periods in which an onset is
#'   flagged as left-censored. Default `3`.
#' @return A data.table with `key_cols`, `prima_osservazione` (first
#'   period with `x > 0`), `prima_comparsa` (onset period, `NA` if never
#'   reached), `eta_mesi` and `censura_sx`.
#' @examples
#' panel <- data.table::data.table(
#'   skill_id = "a", mese_idx = 1:8, x = c(0, 0, 1, 2, 5, 6, 8, 9)
#' )
#' detect_skill_onset(panel, key_cols = "skill_id", min_count = 10)
#' @export
detect_skill_onset <- function(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  min_count = 10,
  roll = 3L,
  censor_months = 3L,
  end = NULL
) {
  caller <- "detect_skill_onset"
  dt <- .em_prepare(
    panel,
    key_cols,
    time_col,
    x_col,
    NULL,
    caller,
    need_n = FALSE
  )
  if (!is.numeric(min_count) || length(min_count) != 1L || min_count <= 0) {
    stop(caller, ": `min_count` must be a positive number", call. = FALSE)
  }
  if (!is.numeric(roll) || length(roll) != 1L || roll < 1) {
    stop(caller, ": `roll` must be a positive integer", call. = FALSE)
  }
  t_min <- min(dt$.t)
  if (is.null(end)) {
    end <- max(dt$.t)
  }
  dt <- dt[.t <= end]
  roll <- as.integer(roll)

  dt[, .t := as.numeric(.t)]
  data.table::setorderv(dt, c(key_cols, ".t"))
  first_obs <- unique(dt[.x > 0], by = key_cols)[,
    c(key_cols, ".t"),
    with = FALSE
  ]
  data.table::setnames(first_obs, ".t", "prima_osservazione")

  # complete each series from its first positive period to `end`
  grid <- first_obs[,
    list(.t = seq(prima_osservazione, end)),
    by = key_cols
  ]
  grid <- merge(grid, dt, by = c(key_cols, ".t"), all.x = TRUE)
  grid[is.na(.x), .x := 0]
  data.table::setorderv(grid, c(key_cols, ".t"))
  grid[,
    .r := Reduce(`+`, data.table::shift(.x, 0:(roll - 1L), fill = 0)),
    by = key_cols
  ]
  onset <- unique(grid[.r >= min_count], by = key_cols)[,
    c(key_cols, ".t"),
    with = FALSE
  ]
  data.table::setnames(onset, ".t", "prima_comparsa")

  res <- merge(first_obs, onset, by = key_cols, all.x = TRUE)
  all_keys <- unique(dt[, key_cols, with = FALSE])
  res <- merge(all_keys, res, by = key_cols, all.x = TRUE)
  res[, `:=`(
    eta_mesi = end - prima_comparsa,
    censura_sx = !is.na(prima_comparsa) &
      prima_comparsa < t_min + censor_months
  )]
  res[]
}

# 6. compute_rca_panel -----

#' Revealed comparative advantage of skills by profession and window
#'
#' Computes, for each window, the posting-based revealed comparative
#' advantage of skill \eqn{k} in profession \eqn{j}.
#'
#' @details
#' Within window \eqn{W}, \eqn{s_{kj} = \sum_{t \in W} x_{kjt} /
#' \sum_{t \in W} n_{jt}} is the share of the profession's postings that
#' mention the skill and \eqn{s_k = \sum_j \sum_{t} x_{kjt} / \sum_j
#' \sum_t n_{jt}} the share over all professions. The index is
#' \eqn{RCA_{kj} = s_{kj} / s_k}; values of at least 1 indicate
#' specialisation. The denominator of a profession-period must be the
#' same on all its rows; professions are assumed to partition the
#' postings.
#'
#' @param panel Profession-level share panel (e.g. from
#'   [compute_share_panel()] with `group_cols = prof_col`).
#' @param skill_col Skill column. Default `"skill_id"`.
#' @param prof_col Profession column.
#' @param time_col,x_col,n_col See [compute_share_trend()].
#' @param windows Named list of numeric vectors `c(from, to)`, e.g.
#'   `list(W1 = c(1, 12), W3 = c(25, 36))`.
#' @return A data.table with `finestra`, `skill_col`, `prof_col`, `x`,
#'   `n`, `quota` (\eqn{s_{kj}}), `quota_skill` (\eqn{s_k}) and `rca`.
#' @examples
#' panel <- data.table::data.table(
#'   cp4 = rep(c("p1", "p2"), each = 2), skill_id = rep(c("a", "b"), 2),
#'   mese_idx = 1, x = c(30, 5, 10, 20), n = rep(c(100, 100), each = 2)
#' )
#' compute_rca_panel(panel, prof_col = "cp4", windows = list(W1 = c(1, 1)))
#' @export
compute_rca_panel <- function(
  panel,
  skill_col = "skill_id",
  prof_col,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  windows
) {
  caller <- "compute_rca_panel"
  dt <- .em_prepare(
    panel,
    c(prof_col, skill_col),
    time_col,
    x_col,
    n_col,
    caller
  )
  if (
    !is.list(windows) ||
      length(windows) == 0L ||
      is.null(names(windows)) ||
      any(names(windows) == "")
  ) {
    stop(
      caller,
      ": `windows` must be a named list of c(from, to)",
      call. = FALSE
    )
  }
  lapply(names(windows), function(w) .em_check_window(windows[[w]], w, caller))

  den <- unique(dt[, c(prof_col, ".t", ".n"), with = FALSE])
  if (anyDuplicated(den, by = c(prof_col, ".t")) > 0L) {
    stop(
      caller,
      ": `",
      n_col,
      "` must be constant within profession ",
      "and period",
      call. = FALSE
    )
  }

  res <- data.table::rbindlist(lapply(names(windows), function(w) {
    b <- windows[[w]]
    nj <- den[.t >= b[1L] & .t <= b[2L], list(n = sum(.n)), by = prof_col]
    xk <- dt[
      .t >= b[1L] & .t <= b[2L],
      list(x = sum(.x)),
      by = c(skill_col, prof_col)
    ]
    out <- merge(xk, nj, by = prof_col)
    n_tot <- sum(nj$n)
    out[, quota_skill := sum(x) / n_tot, by = skill_col]
    out[, `:=`(finestra = w, quota = x / n)]
    out[, rca := ifelse(quota_skill > 0, quota / quota_skill, NA_real_)]
    out
  }))
  data.table::setcolorder(
    res,
    c("finestra", skill_col, prof_col, "x", "n", "quota", "quota_skill", "rca")
  )
  res[]
}

# 7. compute_diffusion_panel -----

#' Diffusion of skills across professions by window
#'
#' Counts the professions in which a skill has a revealed comparative
#' advantage and measures how evenly the skill is spread over
#' professions.
#'
#' @details
#' For skill \eqn{k} in window \eqn{W}, `n_prof_rca` is the number of
#' professions with \eqn{RCA_{kj} \ge} `threshold`. With
#' \eqn{p_{kj} = s_{kj} / \sum_j s_{kj}}, the normalised entropy is
#' \eqn{H_k = -\sum_j p_{kj} \log p_{kj} / \log J}, where \eqn{J} is the
#' number of professions present in the window. \eqn{H_k = 1} when the
#' skill has the same share in all professions and 0 when it appears in
#' one profession only.
#'
#' @param rca Output of [compute_rca_panel()].
#' @param skill_col Skill column. Default `"skill_id"`.
#' @param prof_col Profession column.
#' @param threshold RCA threshold. Default `1`.
#' @param compare Optional character vector of two window names, e.g.
#'   `c("W1", "W3")`. When given, the result has one row per skill with
#'   values of both windows and their differences.
#' @return When `compare = NULL`, a data.table with `skill_col`,
#'   `finestra`, `n_prof` (professions with `x > 0`), `n_prof_rca` and
#'   `entropia`. Otherwise a data.table with `skill_col`,
#'   `n_prof_rca_base`, `n_prof_rca_target`, `delta_n_prof_rca`,
#'   `entropia_base`, `entropia_target`, `delta_entropia`, where `base`
#'   and `target` are the first and second element of `compare`; a skill
#'   absent from a window counts zero professions and zero entropy.
#' @examples
#' panel <- data.table::data.table(
#'   cp4 = rep(c("p1", "p2"), each = 2), skill_id = rep(c("a", "b"), 2),
#'   mese_idx = 1, x = c(30, 5, 10, 20), n = rep(c(100, 100), each = 2)
#' )
#' rca <- compute_rca_panel(panel, prof_col = "cp4",
#'   windows = list(W1 = c(1, 1)))
#' compute_diffusion_panel(rca, prof_col = "cp4")
#' @export
compute_diffusion_panel <- function(
  rca,
  skill_col = "skill_id",
  prof_col,
  threshold = 1,
  compare = NULL
) {
  caller <- "compute_diffusion_panel"
  if (!is.data.frame(rca)) {
    stop(caller, ": `rca` must be a data.frame", call. = FALSE)
  }
  check_columns(
    rca,
    c("finestra", skill_col, prof_col, "x", "quota", "rca"),
    caller = caller
  )
  if (!is.numeric(threshold) || length(threshold) != 1L) {
    stop(caller, ": `threshold` must be a single number", call. = FALSE)
  }
  dt <- data.table::as.data.table(rca)
  if (
    !is.null(compare) &&
      (length(compare) != 2L || !all(compare %in% dt$finestra))
  ) {
    stop(
      caller,
      ": `compare` must name two windows present in `rca`",
      call. = FALSE
    )
  }

  j_win <- dt[, list(.J = data.table::uniqueN(get(prof_col))), by = finestra]
  dt <- merge(dt, j_win, by = "finestra")
  res <- dt[,
    {
      tot <- sum(quota)
      p <- if (tot > 0) quota[quota > 0] / tot else numeric(0)
      h <- if (.J[1L] > 1L && length(p) > 0L) {
        -sum(p * log(p)) / log(.J[1L])
      } else {
        0
      }
      list(
        n_prof = sum(x > 0),
        n_prof_rca = sum(rca >= threshold, na.rm = TRUE),
        entropia = h
      )
    },
    by = c(skill_col, "finestra")
  ]

  if (is.null(compare)) {
    data.table::setcolorder(res, c(skill_col, "finestra"))
    return(res[])
  }

  skills <- unique(res[, skill_col, with = FALSE])
  pick <- function(w, suffix) {
    out <- merge(skills, res[finestra == w], by = skill_col, all.x = TRUE)
    out[is.na(n_prof_rca), `:=`(n_prof_rca = 0L, entropia = 0)]
    out <- out[, c(skill_col, "n_prof_rca", "entropia"), with = FALSE]
    data.table::setnames(
      out,
      c("n_prof_rca", "entropia"),
      paste0(c("n_prof_rca_", "entropia_"), suffix)
    )
    out
  }
  wide <- merge(
    pick(compare[1L], "base"),
    pick(compare[2L], "target"),
    by = skill_col
  )
  wide[, `:=`(
    delta_n_prof_rca = n_prof_rca_target - n_prof_rca_base,
    delta_entropia = entropia_target - entropia_base
  )]
  data.table::setcolorder(
    wide,
    c(
      skill_col,
      "n_prof_rca_base",
      "n_prof_rca_target",
      "delta_n_prof_rca",
      "entropia_base",
      "entropia_target",
      "delta_entropia"
    )
  )
  wide[]
}

# 8. compute_drift_index -----

#' Common drift index of logit shares
#'
#' Estimates the month-to-month movement shared by all skills, which
#' captures changes in extraction or taxonomy rather than in demand.
#'
#' @details
#' For each series with observations in consecutive periods
#' \eqn{t-1, t}, \eqn{\Delta y_{k,t}} is the first difference of the
#' empirical logit share \eqn{\mathrm{logit}((x + 0.5)/(n + 1))}, with
#' weight \eqn{1/(v_{k,t} + v_{k,t-1})} and
#' \eqn{v = 1/(x + 0.5) + 1/(n - x + 0.5)}. The drift index
#' \eqn{D_t} is the weighted median of \eqn{\Delta y_{k,t}} across
#' series, which is robust to the minority of skills with genuine
#' change. `drift_livello` is the cumulative sum of \eqn{D_t} starting
#' from 0 at the first period; periods without differences add 0.
#'
#' @inheritParams compute_share_trend
#' @param min_count Series whose total count over the panel is below
#'   this value are excluded. Default `0`.
#' @return A data.table with `time_col`, `drift` (\eqn{D_t}, `NA` in the
#'   first period), `drift_livello` and `n_serie` (series contributing).
#' @examples
#' set.seed(3)
#' panel <- data.table::CJ(skill_id = letters[1:10], mese_idx = 1:12)
#' panel[, n := 5000L]
#' panel[, x := rbinom(.N, n, plogis(-3 + 0.03 * mese_idx))]
#' compute_drift_index(panel, key_cols = "skill_id")
#' @export
compute_drift_index <- function(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  min_count = 0
) {
  caller <- "compute_drift_index"
  dt <- .em_prepare(panel, key_cols, time_col, x_col, n_col, caller)
  if (!is.numeric(min_count) || length(min_count) != 1L) {
    stop(caller, ": `min_count` must be a single number", call. = FALSE)
  }
  dt <- dt[.n > 0]
  dt[, .tot := sum(.x), by = key_cols]
  dt <- dt[.tot >= min_count]
  data.table::setorderv(dt, c(key_cols, ".t"))
  dt[, `:=`(
    .y = stats::qlogis((.x + 0.5) / (.n + 1)),
    .v = 1 / (.x + 0.5) + 1 / (.n - .x + 0.5)
  )]
  dt[,
    `:=`(
      .dy = .y - data.table::shift(.y),
      .dw = 1 / (.v + data.table::shift(.v)),
      .gap = .t - data.table::shift(.t)
    ),
    by = key_cols
  ]
  dd <- dt[
    !is.na(.dy) & .gap == 1,
    list(drift = .weighted_median(.dy, .dw), n_serie = .N),
    by = .t
  ]
  all_t <- data.table::data.table(.t = sort(unique(dt$.t)))
  res <- merge(all_t, dd, by = ".t", all.x = TRUE)
  res[is.na(n_serie), n_serie := 0L]
  res[, drift_livello := cumsum(ifelse(is.na(drift), 0, drift))]
  data.table::setnames(res, ".t", time_col)
  data.table::setcolorder(res, c(time_col, "drift", "drift_livello", "n_serie"))
  res[]
}

# 9. detect_taxonomy_drift -----

#' Detect extraction and taxonomy breaks
#'
#' Flags months with anomalous jumps in data-quality metrics and skills
#' whose first appearance looks like a taxonomy artefact.
#'
#' @details
#' **Months.** For each metric the first difference \eqn{\Delta m_t} is
#' converted to a robust z-score (median and MAD of the differences).
#' The threshold is the empirical `prob` quantile of the absolute
#' z-scores pooled over all metrics and months, unless `threshold` is
#' given. A month is a break (`rottura = TRUE`) when any metric exceeds
#' it.
#'
#' **Skills.** When `panel_prof` is given, each skill gets its first
#' period with a positive count and the number of professions with a
#' positive count in that period (`n_prof_prima`). Skills first seen in
#' the first period of the panel are left-censored and never flagged.
#' The reference distribution is `n_prof_prima` of skills first seen in
#' non-break periods; a skill is a taxonomy artefact
#' (`flag_tassonomia = TRUE`) when it is first seen in a break month and
#' `n_prof_prima` exceeds the `skill_prob` quantile of the reference.
#'
#' @param quality A data.frame with one row per period: `time_col` and
#'   the metric columns (e.g. number of postings, sources, skills per
#'   posting, new skills, drift index).
#' @param metrics Character vector of metric columns in `quality`.
#' @param time_col Numeric period column. Default `"mese_idx"`.
#' @param prob Quantile of the pooled absolute robust z-scores used as
#'   threshold. Default `0.995`.
#' @param threshold Optional fixed threshold on the absolute robust
#'   z-score, overriding `prob`. Default `NULL`.
#' @param panel_prof Optional profession-level panel with `skill_col`,
#'   `prof_col`, `time_col` and `x_col`.
#' @param skill_col,prof_col,x_col Columns of `panel_prof`.
#' @param skill_prob Quantile of the reference distribution of
#'   `n_prof_prima`. Default `0.95`.
#' @return A list with:
#'   - `mesi`: data.table with `time_col`, one `z_<metric>` column per
#'     metric, `z_max` (largest absolute z) and `rottura`;
#'   - `soglia`: the threshold used;
#'   - `skill`: `NULL`, or a data.table with `skill_col`,
#'     `prima_comparsa`, `n_prof_prima`, `flag_tassonomia`;
#'   - `soglia_prof`: the `n_prof_prima` threshold (or `NA`).
#' @examples
#' q <- data.table::data.table(mese_idx = 1:24,
#'   skill_per_annuncio = c(rep(10, 12), rep(12, 12)) + sin(1:24) / 10)
#' detect_taxonomy_drift(q, metrics = "skill_per_annuncio")$mesi
#' @export
detect_taxonomy_drift <- function(
  quality,
  metrics,
  time_col = "mese_idx",
  prob = 0.995,
  threshold = NULL,
  panel_prof = NULL,
  skill_col = "skill_id",
  prof_col = NULL,
  x_col = "x",
  skill_prob = 0.95
) {
  caller <- "detect_taxonomy_drift"
  if (!is.data.frame(quality)) {
    stop(caller, ": `quality` must be a data.frame", call. = FALSE)
  }
  check_columns(quality, c(time_col, metrics), caller = caller)
  if (!is.numeric(prob) || length(prob) != 1L || prob <= 0 || prob >= 1) {
    stop(caller, ": `prob` must be in (0, 1)", call. = FALSE)
  }
  q <- data.table::as.data.table(quality)[, c(time_col, metrics), with = FALSE]
  data.table::setorderv(q, time_col)

  zcols <- paste0("z_", metrics)
  for (i in seq_along(metrics)) {
    d <- c(NA_real_, diff(as.numeric(q[[metrics[i]]])))
    data.table::set(q, j = zcols[i], value = .robust_z(d))
  }
  zmat <- abs(as.matrix(q[, zcols, with = FALSE]))
  soglia <- if (is.null(threshold)) {
    as.numeric(stats::quantile(zmat, prob, na.rm = TRUE))
  } else {
    threshold
  }
  z_max <- apply(zmat, 1L, function(r) {
    if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE)
  })
  data.table::set(q, j = "z_max", value = z_max)
  q[, rottura := !is.na(z_max) & z_max > soglia]
  mesi <- q[, c(time_col, zcols, "z_max", "rottura"), with = FALSE]

  skill_res <- NULL
  soglia_prof <- NA_real_
  if (!is.null(panel_prof)) {
    if (is.null(prof_col)) {
      stop(caller, ": `prof_col` is required with `panel_prof`", call. = FALSE)
    }
    pp <- .em_prepare(
      panel_prof,
      c(skill_col, prof_col),
      time_col,
      x_col,
      NULL,
      caller,
      need_n = FALSE
    )
    t0 <- min(pp$.t)
    pos <- pp[.x > 0]
    first <- pos[, list(prima_comparsa = min(.t)), by = skill_col]
    pos <- merge(pos, first, by = skill_col)
    skill_res <- pos[
      .t == prima_comparsa,
      list(n_prof_prima = data.table::uniqueN(get(prof_col))),
      by = c(skill_col, "prima_comparsa")
    ]
    break_t <- mesi[rottura == TRUE][[time_col]]
    ref <- skill_res[
      prima_comparsa > t0 & !prima_comparsa %in% break_t,
      n_prof_prima
    ]
    if (length(ref) > 0L) {
      soglia_prof <- as.numeric(stats::quantile(ref, skill_prob))
      skill_res[,
        flag_tassonomia := prima_comparsa > t0 &
          prima_comparsa %in% break_t &
          n_prof_prima > soglia_prof
      ]
    } else {
      warning(
        caller,
        ": no skill first seen in a non-break period; ",
        "no taxonomy flag computed",
        call. = FALSE
      )
      skill_res[, flag_tassonomia := FALSE]
    }
  }

  list(
    mesi = mesi[],
    soglia = soglia,
    skill = skill_res,
    soglia_prof = soglia_prof
  )
}

# 10. calibrate_min_support -----

#' Calibrate the minimum support by split-half reliability
#'
#' Chooses the smallest support threshold at which the trend slope is
#' reliable, measured by the agreement of slopes estimated on two random
#' halves of the postings.
#'
#' @details
#' Postings are split at random into two halves (by posting id, with a
#' fixed `seed`; the global RNG state is restored). For each half the
#' share panel is built and the slope of the last `window` periods is
#' estimated as in [compute_share_trend()] (no breaks, no drift, the
#' chosen `weights`). The
#' support of a series is its average count per 3 periods in the full
#' sample over the window, \eqn{3 \sum_t x_t / window}, the same unit
#' as the rolling count of [detect_skill_onset()]. For each value of
#' `grid`, the Spearman correlation between the half-sample slopes of
#' series with support at least that value is computed; the chosen
#' threshold is the smallest one with correlation at least `target` and
#' at least `min_series` series.
#'
#' @inheritParams compute_share_panel
#' @param window Number of trailing periods for the slope. Default `24`.
#' @param grid Numeric vector of candidate thresholds. Default
#'   `c(5, 10, 20, 30, 50, 100)`.
#' @param target Minimum split-half Spearman correlation. Default `0.7`.
#' @param min_series Minimum number of series for a valid correlation.
#'   Default `10`.
#' @param seed Integer seed for the split. Default `1`.
#' @param weights Trend weighting, `"pooled"` (default) or `"observed"`;
#'   see [compute_share_trend()].
#' @return A list with `tabella` (data.table with `n_min`, `n_serie`,
#'   `rho`) and `n_min` (chosen threshold, `NA` with a warning when no
#'   value reaches `target`).
#' @examples
#' set.seed(4)
#' inc <- data.table::rbindlist(lapply(1:12, function(m) {
#'   ids <- (m * 1000):(m * 1000 + 299)
#'   data.table::rbindlist(lapply(letters[1:12], function(s) {
#'     p <- plogis(-2 + (match(s, letters) - 6) * 0.02 * m)
#'     data.table::data.table(general_id = ids[runif(300) < p],
#'       skill_id = s)
#'   }))[, mese_idx := m]
#' }))
#' calibrate_min_support(inc, window = 12, grid = c(5, 20), min_series = 5)
#' @export
calibrate_min_support <- function(
  incidence,
  id_col = "general_id",
  time_col = "mese_idx",
  skill_col = "skill_id",
  group_cols = NULL,
  window = 24L,
  grid = c(5, 10, 20, 30, 50, 100),
  target = 0.7,
  min_series = 10L,
  seed = 1L,
  weights = c("pooled", "observed")
) {
  caller <- "calibrate_min_support"
  weights <- .em_match_weights(weights, caller)
  if (!is.data.frame(incidence)) {
    stop(caller, ": `incidence` must be a data.frame", call. = FALSE)
  }
  check_columns(
    incidence,
    c(id_col, time_col, skill_col, group_cols),
    caller = caller
  )
  if (!is.numeric(grid) || length(grid) == 0L || anyNA(grid)) {
    stop(caller, ": `grid` must be a numeric vector", call. = FALSE)
  }
  if (
    !is.numeric(target) || length(target) != 1L || target <= -1 || target > 1
  ) {
    stop(caller, ": `target` must be in (-1, 1]", call. = FALSE)
  }
  if (!is.numeric(window) || length(window) != 1L || window < 3) {
    stop(caller, ": `window` must be a single number >= 3", call. = FALSE)
  }
  keys <- c(group_cols, skill_col)
  inc <- data.table::as.data.table(incidence)[,
    c(id_col, time_col, group_cols, skill_col),
    with = FALSE
  ]
  t_end <- max(inc[[time_col]])
  t_from <- t_end - window + 1
  inc <- inc[get(time_col) >= t_from]

  ids <- unique(inc[[id_col]])
  half_a <- .with_seed(seed, sample(ids, floor(length(ids) / 2)))
  in_a <- inc[[id_col]] %in% half_a

  slope_of <- function(sub) {
    p <- .panel_from_incidence(
      sub,
      id_col,
      time_col,
      skill_col,
      group_cols,
      TRUE
    )
    p <- .em_prepare(p, keys, time_col, "x", "n", caller)
    .em_trend_core(p, keys, t_from, t_end, NULL, NULL, weights)[,
      c(keys, "pendenza"),
      with = FALSE
    ]
  }
  sa <- slope_of(inc[in_a])
  sb <- slope_of(inc[!in_a])
  full <- .panel_from_incidence(
    inc,
    id_col,
    time_col,
    skill_col,
    group_cols,
    FALSE
  )
  supp <- full[, list(supporto = 3 * sum(x) / window), by = keys]

  both <- merge(sa, sb, by = keys, suffixes = c("_a", "_b"))
  both <- merge(both, supp, by = keys)
  both <- both[is.finite(pendenza_a) & is.finite(pendenza_b)]

  tab <- data.table::rbindlist(lapply(sort(grid), function(g) {
    sub <- both[supporto >= g]
    rho <- if (nrow(sub) >= max(3L, min_series)) {
      suppressWarnings(stats::cor(
        sub$pendenza_a,
        sub$pendenza_b,
        method = "spearman"
      ))
    } else {
      NA_real_
    }
    data.table::data.table(n_min = g, n_serie = nrow(sub), rho = rho)
  }))

  ok <- tab[!is.na(rho) & rho >= target]
  n_min <- if (nrow(ok) > 0L) min(ok$n_min) else NA_real_
  if (is.na(n_min)) {
    warning(
      caller,
      ": no grid value reaches the target reliability",
      call. = FALSE
    )
  }
  list(tabella = tab[], n_min = n_min)
}

# 11. score_emergence -----

#' Composite emergence score and skill state
#'
#' Combines several emergence indicators into a robust composite score,
#' ranks the series and assigns a state label.
#'
#' @details
#' Each component \eqn{c} is multiplied by its direction (+1 or -1) and
#' converted to a robust z-score \eqn{(v - \mathrm{median})/\mathrm{MAD}}.
#' Missing z-scores count as 0 (the median). The score is
#' \eqn{\sum_c w_c z_c} with weights:
#' - `"equal"`: \eqn{w_c = 1/C};
#' - `"pc1"`: loadings of the first principal component of the z-scores
#'   (complete rows), oriented so that the slope loading is positive and
#'   scaled to unit absolute sum;
#' - `"slope"`: weight 1 on `slope_col` and 0 elsewhere;
#' - a numeric vector with one weight per component.
#'
#' States are assigned in order of precedence:
#' 1. `"Artefatto di tassonomia"`: `flag_col` is `TRUE`;
#' 2. `"Nuova non consolidata"`: `age_col` below `min_age` (recent
#'    onset, too short a history);
#' 3. `"Emergente"`: significant positive slope (`p_col < alpha`) and
#'    score at least the `q_star` quantile of the scores;
#' 4. `"In crescita"`: significant positive slope;
#' 5. `"In calo"`: significant negative slope;
#' 6. `"Stabile"`: otherwise.
#'
#' @param indicators A data.frame with one row per series and the
#'   component columns.
#' @param key_cols Series key columns.
#' @param components Character vector of component columns. Default
#'   `"pendenza"`.
#' @param directions Numeric vector of +1/-1, one per component (e.g. -1
#'   for an age, so that younger skills score higher). Default all +1.
#' @param weights `"equal"`, `"pc1"`, `"slope"` or a numeric vector.
#' @param slope_col Slope column used for significance and the `"slope"`
#'   weights. Default `"pendenza"`.
#' @param p_col Adjusted p-value column. Default `"p_adj"`.
#' @param alpha Significance level. Default `0.05`.
#' @param q_star Score quantile for the `"Emergente"` state. Default
#'   `0.9`.
#' @param age_col Optional column with the age of the series in periods
#'   (e.g. `eta_mesi` from [detect_skill_onset()]). Default `NULL`.
#' @param min_age Age below which a series is `"Nuova non consolidata"`.
#'   Default `12`.
#' @param flag_col Optional logical column marking taxonomy artefacts.
#'   Default `NULL`.
#' @return A copy of `indicators` with `z_<component>` columns,
#'   `punteggio`, `rango` (1 = highest score) and `stato`; the weights
#'   used are stored in the attribute `"pesi"`.
#' @examples
#' ind <- data.table::data.table(
#'   skill_id = letters[1:6],
#'   pendenza = c(0.10, 0.05, 0.00, -0.04, 0.02, 0.08),
#'   p_adj = c(0.001, 0.01, 0.9, 0.01, 0.5, 0.001),
#'   eta_mesi = c(30, 30, 30, 30, 30, 5)
#' )
#' score_emergence(ind, "skill_id", age_col = "eta_mesi", q_star = 0.8)
#' @export
score_emergence <- function(
  indicators,
  key_cols,
  components = "pendenza",
  directions = NULL,
  weights = "equal",
  slope_col = "pendenza",
  p_col = "p_adj",
  alpha = 0.05,
  q_star = 0.9,
  age_col = NULL,
  min_age = 12,
  flag_col = NULL
) {
  caller <- "score_emergence"
  if (!is.data.frame(indicators)) {
    stop(caller, ": `indicators` must be a data.frame", call. = FALSE)
  }
  check_columns(
    indicators,
    unique(c(key_cols, components, slope_col, p_col, age_col, flag_col)),
    caller = caller
  )
  if (is.null(directions)) {
    directions <- rep(1, length(components))
  }
  if (
    length(directions) != length(components) ||
      !all(directions %in% c(-1, 1))
  ) {
    stop(
      caller,
      ": `directions` must be +1/-1, one per component",
      call. = FALSE
    )
  }
  if (!is.numeric(q_star) || length(q_star) != 1L || q_star < 0 || q_star > 1) {
    stop(caller, ": `q_star` must be in [0, 1]", call. = FALSE)
  }
  dt <- data.table::copy(data.table::as.data.table(indicators))

  zcols <- paste0("z_", components)
  for (i in seq_along(components)) {
    data.table::set(
      dt,
      j = zcols[i],
      value = .robust_z(directions[i] * as.numeric(dt[[components[i]]]))
    )
  }
  zmat <- as.matrix(dt[, zcols, with = FALSE])

  w <- .em_weights(weights, zmat, components, slope_col, caller)
  zmat0 <- zmat
  zmat0[is.na(zmat0)] <- 0
  dt[, punteggio := as.numeric(zmat0 %*% w)]
  dt[, rango := data.table::frank(-punteggio, ties.method = "min")]

  cut <- as.numeric(stats::quantile(dt$punteggio, q_star, na.rm = TRUE))
  sig <- !is.na(dt[[p_col]]) & dt[[p_col]] < alpha
  up <- sig & !is.na(dt[[slope_col]]) & dt[[slope_col]] > 0
  down <- sig & !is.na(dt[[slope_col]]) & dt[[slope_col]] < 0
  young <- if (is.null(age_col)) {
    rep(FALSE, nrow(dt))
  } else {
    !is.na(dt[[age_col]]) & dt[[age_col]] < min_age
  }
  artef <- if (is.null(flag_col)) {
    rep(FALSE, nrow(dt))
  } else {
    !is.na(dt[[flag_col]]) & as.logical(dt[[flag_col]])
  }
  stato <- data.table::fcase(
    artef                    , "Artefatto di tassonomia" ,
    young                    , "Nuova non consolidata"   ,
    up & dt$punteggio >= cut , "Emergente"               ,
    up                       , "In crescita"             ,
    down                     , "In calo"                 ,
    default = "Stabile"
  )
  data.table::set(dt, j = "stato", value = stato)
  data.table::setattr(dt, "pesi", stats::setNames(w, components))
  dt[]
}

#' Resolve the weights of the emergence score
#'
#' @param weights Weight specification (see [score_emergence()]).
#' @param zmat Matrix of robust z-scores.
#' @param components Component names.
#' @param slope_col Slope column.
#' @param caller Calling function.
#' @return Numeric vector of weights.
#' @keywords internal
#' @noRd
.em_weights <- function(weights, zmat, components, slope_col, caller) {
  k <- length(components)
  if (is.numeric(weights)) {
    if (length(weights) != k) {
      stop(
        caller,
        ": numeric `weights` must have one value per component",
        call. = FALSE
      )
    }
    return(as.numeric(weights))
  }
  if (
    !is.character(weights) ||
      length(weights) != 1L ||
      !weights %in% c("equal", "pc1", "slope")
  ) {
    stop(
      caller,
      ": `weights` must be 'equal', 'pc1', 'slope' or numeric",
      call. = FALSE
    )
  }
  i_slope <- match(slope_col, components)
  if (is.na(i_slope)) {
    i_slope <- 1L
  }
  switch(
    weights,
    equal = rep(1 / k, k),
    slope = replace(numeric(k), i_slope, 1),
    pc1 = {
      cc <- zmat[stats::complete.cases(zmat), , drop = FALSE]
      if (k == 1L || nrow(cc) < k + 1L) {
        rep(1 / k, k)
      } else {
        v <- stats::prcomp(cc, center = TRUE, scale. = FALSE)$rotation[, 1L]
        if (v[i_slope] < 0) {
          v <- -v
        }
        as.numeric(v / sum(abs(v)))
      }
    }
  )
}

# 12. backtest_emergence -----

#' Pseudo-prospective backtest of the emergence score
#'
#' Evaluates the ability of the emergence score to anticipate significant
#' share growth, computing indicators with the data available at each
#' origin and comparing the predicted `"Emergente"` series with realised
#' growth over the following `horizon` periods.
#'
#' @details
#' For each origin \eqn{o}, only periods \eqn{\le o} are used. The
#' default indicators (when `indicators_fun = NULL`) are, over the
#' `window` periods ending at \eqn{o}: the drift-net slope `pendenza`
#' and its `p_adj` ([compute_share_trend()]), the acceleration
#' `accelerazione` over two halves of the window
#' ([compute_share_acceleration()]), and `novita = -eta_mesi` from
#' [detect_skill_onset()] with `min_count = min_support`. With
#' `net_drift = TRUE` the drift index is recomputed from the data up to
#' the origin, avoiding look-ahead. Only series whose support
#' \eqn{3 \sum x / window} in the window is at least `min_support` are
#' evaluated.
#'
#' The outcome is significant realised growth: the ratio between the
#' pooled share of the 3 periods ending at \eqn{o + horizon} and of the
#' 3 periods ending at \eqn{o} ([compute_yoy_ratio()] with
#' `lag = horizon`) has a Katz lower bound above 1.
#'
#' For each combination of `weights_grid` and `q_grid`,
#' [score_emergence()] labels the series; `"Emergente"` is the positive
#' prediction. Counts are pooled over origins for precision, recall and
#' F1; precision@k is the share of the `k` top-scored series with
#' positive outcome, averaged over origins.
#'
#' @inheritParams compute_share_trend
#' @param origins Numeric vector of origin periods.
#' @param horizon Outcome horizon in periods. Default `12`.
#' @param window Indicator window in periods. Default `24`.
#' @param min_support Minimum support (average count per 3 periods).
#'   Default `10`.
#' @param components,directions Passed to [score_emergence()]. Defaults
#'   match the default indicators.
#' @param weights_grid Character vector of weight schemes. Default
#'   `c("equal", "pc1", "slope")`.
#' @param q_grid Numeric vector of `q_star` values. Default
#'   `c(0.8, 0.9, 0.95)`.
#' @param alpha Significance level for trend and outcome. Default `0.05`.
#' @param k Size of the top list for precision@k. Default `50`.
#' @param net_drift Logical; recompute and net out the drift index at
#'   each origin. Default `FALSE`.
#' @param trend_weights Weighting of the default trend and acceleration
#'   indicators, `"pooled"` (default) or `"observed"`; see
#'   [compute_share_trend()]. Distinct from `weights_grid`, which weights
#'   the score components.
#' @param indicators_fun Optional function `f(panel, origin)` receiving
#'   the standardised panel truncated at the origin (columns
#'   `key_cols`, `time_col`, `x_col`, `n_col`) and returning a
#'   data.table with `key_cols`, the `components`, `pendenza` and
#'   `p_adj`. Default `NULL` (built-in indicators).
#' @return A list with:
#'   - `griglia`: one row per `pesi` x `q_star` with `tp`, `fp`, `fn`,
#'     `precision`, `recall`, `f1`, `precision_at_k`;
#'   - `dettaglio`: the same metrics by origin;
#'   - `migliore`: the row of `griglia` with the highest F1.
#' @examples
#' set.seed(5)
#' panel <- data.table::CJ(skill_id = sprintf("s%02d", 1:30),
#'   mese_idx = 1:30)
#' panel[, beta := ifelse(as.integer(substr(skill_id, 2, 3)) <= 8, 0.06, 0)]
#' panel[, n := 3000L]
#' panel[, x := rbinom(.N, n, plogis(-3.5 + beta * mese_idx))]
#' bt <- backtest_emergence(panel, "skill_id", origins = 18, horizon = 12,
#'   window = 18, k = 5, q_grid = 0.7)
#' bt$griglia
#' @export
backtest_emergence <- function(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  origins,
  horizon = 12L,
  window = 24L,
  min_support = 10,
  components = c("pendenza", "accelerazione", "novita"),
  directions = NULL,
  weights_grid = c("equal", "pc1", "slope"),
  q_grid = c(0.8, 0.9, 0.95),
  alpha = 0.05,
  k = 50L,
  net_drift = FALSE,
  break_times = NULL,
  indicators_fun = NULL,
  trend_weights = c("pooled", "observed")
) {
  caller <- "backtest_emergence"
  trend_weights <- .em_match_weights(trend_weights, caller)
  dt <- .em_prepare(panel, key_cols, time_col, x_col, n_col, caller)
  if (!is.numeric(origins) || length(origins) == 0L) {
    stop(caller, ": `origins` must be a numeric vector", call. = FALSE)
  }
  t_min <- min(dt$.t)
  t_max <- max(dt$.t)
  bad <- origins + horizon > t_max | origins - window + 1 < t_min
  if (any(bad)) {
    stop(
      caller,
      ": origins ",
      paste(origins[bad], collapse = ", "),
      " leave no room for `window` before or `horizon` after them",
      call. = FALSE
    )
  }
  if (!is.null(indicators_fun) && !is.function(indicators_fun)) {
    stop(caller, ": `indicators_fun` must be a function or NULL", call. = FALSE)
  }

  # rename back to user names for the public helpers
  to_user <- function(d) {
    d <- data.table::copy(d)
    data.table::setnames(d, c(".t", ".x", ".n"), c(time_col, x_col, n_col))
    d
  }

  per_origin <- lapply(origins, function(o) {
    past <- to_user(dt[.t <= o])
    ind <- if (is.null(indicators_fun)) {
      .em_default_indicators(
        past,
        key_cols,
        time_col,
        x_col,
        n_col,
        o,
        window,
        min_support,
        break_times,
        net_drift,
        trend_weights
      )
    } else {
      data.table::as.data.table(indicators_fun(past, o))
    }
    check_columns(
      ind,
      unique(c(key_cols, components, "pendenza", "p_adj")),
      caller = caller
    )

    supp <- dt[
      .t > o - window & .t <= o,
      list(.supp = 3 * sum(.x) / window),
      by = key_cols
    ]
    ind <- merge(
      ind,
      supp[.supp >= min_support, key_cols, with = FALSE],
      by = key_cols
    )

    out <- compute_yoy_ratio(
      to_user(dt[.t <= o + horizon]),
      key_cols,
      time_col,
      x_col,
      n_col,
      months = 3L,
      lag = horizon,
      end = o + horizon,
      conf = 1 - alpha
    )
    out[, esito := !is.na(yoy_lo) & yoy_lo > 1]
    ind <- merge(ind, out[, c(key_cols, "esito"), with = FALSE], by = key_cols)

    grid <- data.table::CJ(pesi = weights_grid, q_star = q_grid, sorted = FALSE)
    data.table::rbindlist(lapply(seq_len(nrow(grid)), function(g) {
      sc <- score_emergence(
        ind,
        key_cols,
        components,
        directions,
        weights = grid$pesi[g],
        alpha = alpha,
        q_star = grid$q_star[g]
      )
      pred <- sc$stato == "Emergente"
      top <- sc[order(-punteggio)][seq_len(min(k, .N))]
      data.table::data.table(
        origine = o,
        pesi = grid$pesi[g],
        q_star = grid$q_star[g],
        n_serie = nrow(sc),
        tp = sum(pred & sc$esito),
        fp = sum(pred & !sc$esito),
        fn = sum(!pred & sc$esito),
        precision_at_k = if (nrow(top) > 0L) mean(top$esito) else NA_real_
      )
    }))
  })
  det <- data.table::rbindlist(per_origin)

  add_metrics <- function(d) {
    d[, `:=`(
      precision = ifelse(tp + fp > 0, tp / (tp + fp), NA_real_),
      recall = ifelse(tp + fn > 0, tp / (tp + fn), NA_real_)
    )]
    d[, f1 := ifelse(tp > 0, 2 * precision * recall / (precision + recall), 0)]
    d
  }
  det <- add_metrics(det)
  gr <- det[,
    list(
      n_origini = .N,
      tp = sum(tp),
      fp = sum(fp),
      fn = sum(fn),
      precision_at_k = mean(precision_at_k, na.rm = TRUE)
    ),
    by = c("pesi", "q_star")
  ]
  gr <- add_metrics(gr)
  data.table::setcolorder(
    gr,
    c(
      "pesi",
      "q_star",
      "n_origini",
      "tp",
      "fp",
      "fn",
      "precision",
      "recall",
      "f1",
      "precision_at_k"
    )
  )
  best <- gr[order(-f1, -precision_at_k)][1L]
  list(griglia = gr[], dettaglio = det[], migliore = best)
}

#' Default indicators for the emergence backtest
#'
#' @param past Panel truncated at the origin, with user column names.
#' @param origin Origin period.
#' @inheritParams backtest_emergence
#' @return A data.table with `key_cols`, `pendenza`, `p_adj`,
#'   `accelerazione`, `eta_mesi`, `novita`.
#' @keywords internal
#' @noRd
.em_default_indicators <- function(
  past,
  key_cols,
  time_col,
  x_col,
  n_col,
  origin,
  window,
  min_support,
  break_times,
  net_drift,
  trend_weights = "pooled"
) {
  drift <- if (net_drift) {
    compute_drift_index(past, key_cols, time_col, x_col, n_col)
  } else {
    NULL
  }
  tr <- compute_share_trend(
    past,
    key_cols,
    time_col,
    x_col,
    n_col,
    window = window,
    end = origin,
    break_times = break_times,
    drift = drift,
    weights = trend_weights
  )
  acc <- compute_share_acceleration(
    past,
    key_cols,
    time_col,
    x_col,
    n_col,
    window = max(3L, floor(window / 2)),
    end = origin,
    break_times = break_times,
    drift = drift,
    weights = trend_weights
  )
  ons <- detect_skill_onset(
    past,
    key_cols,
    time_col,
    x_col,
    min_count = min_support,
    end = origin
  )
  res <- merge(
    tr[, c(key_cols, "pendenza", "p_adj"), with = FALSE],
    acc[, c(key_cols, "accelerazione"), with = FALSE],
    by = key_cols,
    all.x = TRUE
  )
  res <- merge(
    res,
    ons[, c(key_cols, "eta_mesi"), with = FALSE],
    by = key_cols,
    all.x = TRUE
  )
  res[, novita := -eta_mesi]
  res[]
}
