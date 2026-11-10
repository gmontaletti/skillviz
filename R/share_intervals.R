# Share intervals -----
#
# Interval estimation for shares computed on postings clustered by source:
# design effects from a linearised ratio estimator, smoothing of the design
# effect along a code hierarchy, empirical Bayes estimates on trees,
# cluster bootstrap, hierarchical false discovery rate control and the
# finest reliable level of a hierarchy.

# 0. internal helpers -----

#' Check that an argument is a data.frame
#' @keywords internal
#' @noRd
.si_check_df <- function(x, name, caller) {
  if (!is.data.frame(x)) {
    stop(
      caller,
      ": `",
      name,
      "` must be a data.frame or data.table",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Check a single column name argument
#' @keywords internal
#' @noRd
.si_check_name <- function(x, name, caller) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(caller, ": `", name, "` must be a single column name", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check key columns against reserved internal names
#' @keywords internal
#' @noRd
.si_check_keys <- function(key_cols, caller) {
  if (!is.character(key_cols) || length(key_cols) == 0L || anyNA(key_cols)) {
    stop(
      caller,
      ": `key_cols` must be a non-empty character vector",
      call. = FALSE
    )
  }
  if (any(startsWith(key_cols, "."))) {
    stop(
      caller,
      ": `key_cols` cannot start with '.' (reserved for internal columns)",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Check a confidence level
#' @keywords internal
#' @noRd
.si_check_conf <- function(conf, caller) {
  if (
    !is.numeric(conf) ||
      length(conf) != 1L ||
      is.na(conf) ||
      conf <= 0 ||
      conf >= 1
  ) {
    stop(caller, ": `conf` must be a single number in (0, 1)", call. = FALSE)
  }
  invisible(TRUE)
}

#' Prepare a key x cluster count table
#'
#' Copies the needed columns of `counts` and renames cluster, count and
#' denominator to `.s`, `.x`, `.n`. When `cluster_totals` is given, the
#' denominator is taken from it.
#'
#' @return A list with `dt` (key_cols, .s, .x, .n) and `totals` (.s, .ns)
#'   or `NULL`.
#' @keywords internal
#' @noRd
.si_prepare_counts <- function(
  counts,
  key_cols,
  cluster_col,
  x_col,
  n_col,
  cluster_totals,
  caller
) {
  .si_check_df(counts, "counts", caller)
  .si_check_keys(key_cols, caller)
  .si_check_name(cluster_col, "cluster_col", caller)
  .si_check_name(x_col, "x_col", caller)
  .si_check_name(n_col, "n_col", caller)
  if (cluster_col %in% key_cols) {
    stop(caller, ": `cluster_col` cannot be a key column", call. = FALSE)
  }

  use_totals <- !is.null(cluster_totals)
  cols <- c(key_cols, cluster_col, x_col, if (!use_totals) n_col)
  check_columns(counts, cols, caller = caller)
  dt <- data.table::as.data.table(counts)[, cols, with = FALSE]
  data.table::setnames(
    dt,
    c(cluster_col, x_col, if (!use_totals) n_col),
    c(".s", ".x", if (!use_totals) ".n")
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
  if (anyDuplicated(dt, by = c(key_cols, ".s")) > 0L) {
    stop(
      caller,
      ": duplicated rows for the same key and cluster",
      call. = FALSE
    )
  }

  totals <- NULL
  if (use_totals) {
    .si_check_df(cluster_totals, "cluster_totals", caller)
    check_columns(cluster_totals, c(cluster_col, n_col), caller = caller)
    totals <- data.table::as.data.table(cluster_totals)[,
      c(cluster_col, n_col),
      with = FALSE
    ]
    data.table::setnames(totals, c(".s", ".ns"))
    if (!is.numeric(totals$.ns) || anyNA(totals$.ns) || any(totals$.ns < 0)) {
      stop(
        caller,
        ": `cluster_totals` must have a non-negative `",
        n_col,
        "` without NA",
        call. = FALSE
      )
    }
    if (anyDuplicated(totals, by = ".s") > 0L) {
      stop(caller, ": duplicated clusters in `cluster_totals`", call. = FALSE)
    }
    dt[totals, .n := .ns, on = ".s"]
    if (anyNA(dt$.n)) {
      stop(
        caller,
        ": clusters in `counts` missing from `cluster_totals`",
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
  }
  if (any(dt$.x > dt$.n)) {
    stop(caller, ": `", x_col, "` must not exceed `", n_col, "`", call. = FALSE)
  }
  list(dt = dt, totals = totals)
}

#' Identify root nodes of a tree
#'
#' A node is a root when its parent is `NA`, empty, or not among the keys.
#' @keywords internal
#' @noRd
.si_is_root <- function(parent, keys) {
  is.na(parent) | parent == "" | !(parent %in% keys)
}

#' Prepare a tree table (key, parent, level)
#'
#' @return A data.table copy of `est` with internal columns `.key`,
#'   `.par` (character, NA for roots), `.lev`, `.pi` (row of the parent).
#' @keywords internal
#' @noRd
.si_prepare_tree <- function(
  est,
  key_col,
  parent_col,
  level_col,
  caller,
  strict_parent = FALSE
) {
  .si_check_df(est, "est", caller)
  .si_check_name(key_col, "key_col", caller)
  .si_check_name(parent_col, "parent_col", caller)
  if (!is.null(level_col)) {
    .si_check_name(level_col, "level_col", caller)
  }
  check_columns(est, c(key_col, parent_col, level_col), caller = caller)
  dt <- data.table::copy(data.table::as.data.table(est))
  key <- as.character(dt[[key_col]])
  if (anyNA(key) || anyDuplicated(key) > 0L) {
    stop(caller, ": `", key_col, "` must be unique and not NA", call. = FALSE)
  }
  par <- as.character(dt[[parent_col]])
  missing_par <- !is.na(par) & par != "" & !(par %in% key)
  if (strict_parent && any(missing_par)) {
    stop(
      caller,
      ": parents not found among the keys: ",
      paste(utils::head(unique(par[missing_par]), 5L), collapse = ", "),
      call. = FALSE
    )
  }
  root <- .si_is_root(par, key)
  par[root] <- NA_character_
  pi <- match(par, key)
  if (!is.null(level_col)) {
    lev <- dt[[level_col]]
    if (anyNA(lev)) {
      stop(caller, ": `", level_col, "` must not contain NA", call. = FALSE)
    }
    nr <- !root
    if (any(nr) && any(!(lev[pi[nr]] < lev[nr]))) {
      stop(
        caller,
        ": every parent must have a lower `",
        level_col,
        "` than its children",
        call. = FALSE
      )
    }
  } else {
    lev <- .si_depth(pi)
  }
  list(dt = dt, key = key, par = par, pi = pi, lev = lev, root = root)
}

#' Depth of each node from parent row indices
#' @keywords internal
#' @noRd
.si_depth <- function(pi) {
  depth <- integer(length(pi))
  cur <- pi
  steps <- 0L
  while (any(!is.na(cur))) {
    steps <- steps + 1L
    if (steps > length(pi)) {
      stop("cycle detected in the parent links", call. = FALSE)
    }
    has <- !is.na(cur)
    depth[has] <- depth[has] + 1L
    cur[has] <- pi[cur[has]]
  }
  depth
}

#' Concentration of a beta-binomial with known means
#'
#' Maximises over `log(kappa)` the beta-binomial log-likelihood of `x` out
#' of `n` with means `m`.
#' @return List with `kappa` (Inf at the upper bound), `loglik`.
#' @keywords internal
#' @noRd
.si_kappa_bb <- function(x, n, m, lower = 1e-6, upper = 1e8) {
  m <- pmin(pmax(m, 1e-10), 1 - 1e-10)
  ll <- function(lk) {
    k <- exp(lk)
    sum(lbeta(x + k * m, n - x + k * (1 - m)) - lbeta(k * m, k * (1 - m)))
  }
  .si_kappa_optimize(ll, lower, upper)
}

#' Concentration of a Dirichlet-multinomial with known means
#' @keywords internal
#' @noRd
.si_kappa_dm <- function(x, m, lower = 1e-6, upper = 1e8) {
  m <- pmax(m, 1e-10)
  m <- m / sum(m)
  tot <- sum(x)
  ll <- function(lk) {
    k <- exp(lk)
    lgamma(k) - lgamma(tot + k) + sum(lgamma(x + k * m) - lgamma(k * m))
  }
  .si_kappa_optimize(ll, lower, upper)
}

#' One-dimensional maximisation on log(kappa) with boundary detection
#' @keywords internal
#' @noRd
.si_kappa_optimize <- function(ll, lower, upper) {
  lo <- log(lower)
  hi <- log(upper)
  opt <- stats::optimize(ll, c(lo, hi), maximum = TRUE)
  ll_hi <- ll(hi)
  ll_lo <- ll(lo)
  if (ll_hi >= opt$objective - 1e-9 && ll_hi >= ll_lo) {
    return(list(kappa = Inf, loglik = ll_hi))
  }
  if (ll_lo > opt$objective) {
    return(list(kappa = lower, loglik = ll_lo))
  }
  list(kappa = exp(opt$maximum), loglik = opt$objective)
}

# 1. share_interval -----

#' Confidence interval for a share with a design effect
#'
#' Computes Wilson, Jeffreys or logit intervals for the share `x / n`
#' using the effective sample size `n / deff`, so that the extra variance
#' due to clustering (for example of postings by source) widens the
#' interval.
#'
#' @param x Numeric vector of counts (postings with the code).
#' @param n Numeric vector of denominators, same length as `x` or length 1.
#' @param method Interval method: `"wilson"` (default), `"jeffreys"` or
#'   `"logit"`.
#' @param deff Design effect, length 1 or `length(x)`. Must be positive;
#'   `NA` gives `NA` bounds.
#' @param df Degrees of freedom of the t quantile, length 1 or
#'   `length(x)`. `Inf` (default) uses the normal quantile. Must be
#'   positive; `NA` gives `NA` bounds. Not used by `"jeffreys"`.
#' @param conf Confidence level, default 0.95.
#'
#' @return A data.table with one row per element of `x` and columns
#'   `quota` (`x / n`), `lo`, `hi`, `n_eff` (`n / deff`) and `metodo`.
#'
#' @details
#' With effective counts `n_eff = n / deff`, `x_eff = x / deff`, share
#' `p = x / n` and quantile `z = qt(1 - (1 - conf) / 2, df)` (equal to
#' the normal quantile when `df = Inf`):
#'
#' * **Wilson**: `(p + z^2 / (2 n_eff)) / (1 + z^2 / n_eff) +/-
#'   z / (1 + z^2 / n_eff) * sqrt(p (1 - p) / n_eff + z^2 / (4 n_eff^2))`.
#' * **Jeffreys**: quantiles `(1 - conf) / 2` and `1 - (1 - conf) / 2`
#'   of Beta(`x_eff + 0.5`, `n_eff - x_eff + 0.5`); the lower bound is 0
#'   when `x = 0` and the upper bound is 1 when `x = n` (Brown, Cai and
#'   DasGupta, 2001).
#' * **Logit**: `plogis(logit(p) +/- z * sqrt(deff / (n p (1 - p))))`;
#'   when `x = 0` or `x = n` the Haldane correction replaces `p` with
#'   `(x + 0.5) / (n + 1)` and `n` with `n + 1`.
#'
#' The design effect is the ratio between the variance of the estimator
#' under the actual design and its binomial variance (Kish, 1965); see
#' [cluster_deff()] for its estimation with sources as clusters and
#' [smooth_deff()] for its stabilisation along a code hierarchy.
#'
#' @references
#' Kish, L. (1965). *Survey Sampling*. Wiley.
#'
#' Brown, L. D., Cai, T. T. and DasGupta, A. (2001). Interval estimation
#' for a binomial proportion. *Statistical Science*, 16(2), 101-133.
#'
#' @examples
#' share_interval(x = c(0, 12, 300), n = 1000)
#' share_interval(x = c(0, 12, 300), n = 1000, deff = 4, df = 20)
#' share_interval(x = 12, n = 1000, method = "logit", deff = 2)
#'
#' @export
share_interval <- function(
  x,
  n,
  method = c("wilson", "jeffreys", "logit"),
  deff = 1,
  df = Inf,
  conf = 0.95
) {
  caller <- "share_interval"
  method <- match.arg(method)
  .si_check_conf(conf, caller)
  for (a in list(
    list(x, "x"),
    list(n, "n"),
    list(deff, "deff"),
    list(df, "df")
  )) {
    if (!is.numeric(a[[1L]]) && !all(is.na(a[[1L]]))) {
      stop(caller, ": `", a[[2L]], "` must be numeric", call. = FALSE)
    }
  }
  len <- length(x)
  if (len == 0L) {
    return(data.table::data.table(
      quota = numeric(0),
      lo = numeric(0),
      hi = numeric(0),
      n_eff = numeric(0),
      metodo = character(0)
    ))
  }
  recycle <- function(v, name) {
    if (length(v) == 1L) {
      return(rep(as.numeric(v), len))
    }
    if (length(v) != len) {
      stop(
        caller,
        ": `",
        name,
        "` must have length 1 or the length of `x`",
        call. = FALSE
      )
    }
    as.numeric(v)
  }
  x <- as.numeric(x)
  n <- recycle(n, "n")
  deff <- recycle(deff, "deff")
  df <- recycle(df, "df")
  if (any(n <= 0, na.rm = TRUE)) {
    stop(caller, ": `n` must be positive", call. = FALSE)
  }
  if (any(x < 0, na.rm = TRUE)) {
    stop(caller, ": `x` must be non-negative", call. = FALSE)
  }
  if (any(x > n, na.rm = TRUE)) {
    stop(caller, ": `x` must not exceed `n`", call. = FALSE)
  }
  if (any(deff <= 0, na.rm = TRUE)) {
    stop(caller, ": `deff` must be positive", call. = FALSE)
  }
  if (any(df <= 0, na.rm = TRUE)) {
    stop(caller, ": `df` must be positive", call. = FALSE)
  }

  alpha <- 1 - conf
  z <- stats::qt(1 - alpha / 2, df)
  p <- x / n
  n_eff <- n / deff
  x_eff <- x / deff

  if (method == "wilson") {
    z2 <- z^2
    den <- 1 + z2 / n_eff
    centre <- (p + z2 / (2 * n_eff)) / den
    half <- z / den * sqrt(p * (1 - p) / n_eff + z2 / (4 * n_eff^2))
    lo <- centre - half
    hi <- centre + half
  } else if (method == "jeffreys") {
    a <- x_eff + 0.5
    b <- n_eff - x_eff + 0.5
    lo <- data.table::fifelse(x == 0, 0, stats::qbeta(alpha / 2, a, b))
    hi <- data.table::fifelse(x == n, 1, stats::qbeta(1 - alpha / 2, a, b))
    lo[is.na(n_eff)] <- NA_real_
    hi[is.na(n_eff)] <- NA_real_
  } else {
    haldane <- x == 0 | x == n
    pt <- data.table::fifelse(haldane, (x + 0.5) / (n + 1), p)
    nt <- data.table::fifelse(haldane, n + 1, n)
    se <- sqrt(deff / (nt * pt * (1 - pt)))
    eta <- stats::qlogis(pt)
    lo <- stats::plogis(eta - z * se)
    hi <- stats::plogis(eta + z * se)
  }
  data.table::data.table(
    quota = p,
    lo = pmin(pmax(lo, 0), 1),
    hi = pmin(pmax(hi, 0), 1),
    n_eff = n_eff,
    metodo = method
  )
}

# 2. cluster_deff -----

#' Design effect of a share with sources as clusters
#'
#' Treats the share of a code as a ratio estimator `x / n` summed over
#' clusters (sources) and computes its linearised between-cluster
#' variance, the design effect with respect to the binomial variance,
#' the intra-cluster correlation and the effective number of clusters.
#'
#' @param counts data.frame or data.table with one row per key and
#'   cluster, holding the count `x_col` and (when `cluster_totals` is
#'   `NULL`) the cluster denominator `n_col`.
#' @param key_cols Character vector of key columns (e.g. the code).
#' @param cluster_col Name of the cluster column (default `"source"`).
#' @param x_col Name of the count column (default `"x"`).
#' @param n_col Name of the denominator column (default `"n"`), in
#'   `counts` or in `cluster_totals`.
#' @param cluster_totals Optional data.frame with one row per cluster and
#'   columns `cluster_col` and `n_col`. When given, the denominator of
#'   every cluster is taken from it and every cluster with a positive
#'   denominator enters the variance of every key, also when the key has
#'   no row (zero count) for that cluster. Recommended.
#'
#' @return A data.table with one row per key and columns `key_cols`, `x`,
#'   `n`, `quota`, `v_bin`, `v_cluster`, `deff`, `rho`, `n_cluster`,
#'   `n_cluster_eff`, `df`.
#'
#' @details
#' For a key with cluster counts `x_s` and denominators `n_s`
#' (`s = 1, ..., S`, clusters with `n_s > 0`), `x = sum(x_s)`,
#' `n = sum(n_s)` and `p = x / n`. The linearised (Taylor) variance of
#' the ratio estimator with clusters as primary units (Wolter, 2007) is
#'
#' `v_cluster = S / (S - 1) * sum_s (x_s - p n_s)^2 / n^2`
#'
#' and the binomial variance is `v_bin = p (1 - p) / n`. The design
#' effect (Kish, 1965) is `deff = v_cluster / v_bin` (`NA` when `p` is 0
#' or 1, or when `S < 2`). With the weighted mean cluster size
#' `m = sum(n_s^2) / n`, the intra-cluster correlation implied by
#' `deff = 1 + (m - 1) rho` is `rho = (deff - 1) / (m - 1)`. The
#' effective number of clusters is `n_cluster_eff = n^2 / sum(n_s^2)`
#' and `df = n_cluster_eff - 1` is the suggested number of degrees of
#' freedom for [share_interval()].
#'
#' Clusters with a zero count for the key contribute `(p n_s)^2` to the
#' sum. Without `cluster_totals` only the rows present in `counts` enter
#' the computation, so `counts` must then be complete (one row per key
#' and cluster, zeros included).
#'
#' @references
#' Kish, L. (1965). *Survey Sampling*. Wiley.
#'
#' Wolter, K. M. (2007). *Introduction to Variance Estimation* (2nd ed.).
#' Springer.
#'
#' @examples
#' counts <- data.frame(
#'   code = rep(c("a", "b"), each = 4),
#'   source = rep(c("s1", "s2", "s3", "s4"), 2),
#'   x = c(10, 40, 5, 25, 3, 2, 4, 1)
#' )
#' totals <- data.frame(source = c("s1", "s2", "s3", "s4"),
#'                      n = c(200, 400, 100, 300))
#' cluster_deff(counts, key_cols = "code", cluster_totals = totals)
#'
#' @export
cluster_deff <- function(
  counts,
  key_cols,
  cluster_col = "source",
  x_col = "x",
  n_col = "n",
  cluster_totals = NULL
) {
  caller <- "cluster_deff"
  prep <- .si_prepare_counts(
    counts,
    key_cols,
    cluster_col,
    x_col,
    n_col,
    cluster_totals,
    caller
  )
  dt <- prep$dt[.n > 0]
  tot <- prep$totals

  agg <- dt[,
    list(.xk = sum(.x), .nk = sum(.n), .nn_p = sum(.n^2), .sk = .N),
    by = key_cols
  ]
  if (!is.null(tot)) {
    tot <- tot[.ns > 0]
    agg[, `:=`(
      .nk = sum(tot$.ns),
      .nn = sum(tot$.ns^2),
      .sk = nrow(tot)
    )]
  } else {
    agg[, .nn := .nn_p]
  }
  agg[, .p := .xk / .nk]
  dt[agg, .p := i..p, on = key_cols]
  ss <- dt[, list(.ss = sum((.x - .p * .n)^2)), by = key_cols]
  agg[ss, .ss := i..ss, on = key_cols]
  # clusters without a row for the key (x_s = 0) add (p n_s)^2
  agg[, .ss := .ss + .p^2 * (.nn - .nn_p)]

  agg[, `:=`(
    v_bin = .p * (1 - .p) / .nk,
    v_cluster = data.table::fifelse(
      .sk >= 2L,
      .sk / (.sk - 1) * .ss / .nk^2,
      NA_real_
    ),
    .mbar = .nn / .nk
  )]
  agg[,
    deff := data.table::fifelse(.p > 0 & .p < 1, v_cluster / v_bin, NA_real_)
  ]
  agg[,
    rho := data.table::fifelse(.mbar > 1, (deff - 1) / (.mbar - 1), NA_real_)
  ]
  agg[, n_cluster_eff := .nk^2 / .nn]
  out <- agg[,
    c(
      key_cols,
      ".xk",
      ".nk",
      ".p",
      "v_bin",
      "v_cluster",
      "deff",
      "rho",
      ".sk",
      "n_cluster_eff"
    ),
    with = FALSE
  ]
  data.table::setnames(
    out,
    c(".xk", ".nk", ".p", ".sk"),
    c("x", "n", "quota", "n_cluster")
  )
  out[, n_cluster := as.integer(n_cluster)]
  out[, df := n_cluster_eff - 1]
  out[]
}

# 3. smooth_deff -----

#' Smooth design effects along a code hierarchy
#'
#' Stabilises the noisy design effects of small codes with a generalised
#' variance function (GVF) by level and with a normal-normal shrinkage of
#' `log(deff)` towards the parent node.
#'
#' @param est data.frame or data.table with one row per node, e.g. the
#'   output of [cluster_deff()] with the hierarchy columns added.
#' @param key_col Name of the node identifier column.
#' @param parent_col Name of the parent identifier column (`NA` or `""`
#'   for roots; parents absent from `est` are treated as roots).
#' @param level_col Name of the level column; parents must have a lower
#'   level than their children.
#' @param method `"gvf_parent"` (default), `"gvf"` or `"parent"`.
#' @param deff_col,quota_col,df_col Names of the design effect, share and
#'   degrees of freedom columns (defaults `"deff"`, `"quota"`, `"df"`).
#'
#' @return A copy of `est` with columns `deff_liscio` (smoothed design
#'   effect, truncated at 1) and `peso_contrazione` (weight of the prior
#'   mean, from 0 = direct estimate to 1 = prior mean). The attributes
#'   `gvf` (intercept, slope and number of fitted nodes by level) and
#'   `tau2` (prior variance by level) document the fit.
#'
#' @details
#' **GVF.** For every level the weighted least squares regression
#' `log(deff) = a + b log(quota)`, weights `df`, is fitted on the nodes
#' with finite positive `deff`, `quota` in (0, 1) and `df > 0`; the
#' prediction `g` is the GVF value (quotas outside the fitted range are
#' clamped to it). With fewer than three nodes or no variation in
#' `quota` the slope is set to 0, and with no usable node `g = 0`.
#'
#' **Shrinkage.** Levels are processed from the top. For node `k`, the
#' direct value `y_k = log(deff_k)` has sampling variance approximately
#' `s_k^2 = 2 / df_k` (variance of the log of a scaled chi-square).
#' The prior mean `m_k` is
#' * `"parent"`: the smoothed `log(deff)` of the parent;
#' * `"gvf_parent"`: `g_k` plus the smoothed residual of the parent,
#'   `log(deff_liscio_parent) - g_parent`, so that the GVF carries the
#'   dependence on the share and the parent the code-specific clustering;
#' * roots: `g_k` for both.
#'
#' The prior variance `tau^2` of each level is the moment estimator of
#' DerSimonian and Laird (1986) with known means,
#' `tau^2 = max(0, (sum w (y - m)^2 - K) / sum w)`, `w = 1 / s^2`. The
#' posterior mean is `B m + (1 - B) y` with `B = s^2 / (s^2 + tau^2)`,
#' reported as `peso_contrazione`. Nodes without a usable direct value
#' receive the prior mean (`B = 1`). With `"gvf"` the smoothed value is
#' `g` itself (`B = 1`). The truncation at 1 is applied to the output
#' only; untruncated values are passed down the tree.
#'
#' @references
#' Wolter, K. M. (2007). *Introduction to Variance Estimation* (2nd ed.).
#' Springer, chapter 7 (generalized variance functions).
#'
#' DerSimonian, R. and Laird, N. (1986). Meta-analysis in clinical
#' trials. *Controlled Clinical Trials*, 7(3), 177-188.
#'
#' @examples
#' est <- data.frame(
#'   code = c("A", "A1", "A2", "B", "B1"),
#'   parent = c(NA, "A", "A", NA, "B"),
#'   level = c(1, 2, 2, 1, 2),
#'   quota = c(0.3, 0.2, 0.1, 0.05, 0.05),
#'   deff = c(8, 6, 1.5, 3, 12),
#'   df = c(20, 15, 4, 10, 2)
#' )
#' smooth_deff(est, "code", "parent", "level")
#'
#' @export
smooth_deff <- function(
  est,
  key_col,
  parent_col,
  level_col,
  method = c("gvf_parent", "gvf", "parent"),
  deff_col = "deff",
  quota_col = "quota",
  df_col = "df"
) {
  caller <- "smooth_deff"
  method <- match.arg(method)
  tr <- .si_prepare_tree(est, key_col, parent_col, level_col, caller)
  check_columns(tr$dt, c(deff_col, quota_col, df_col), caller = caller)
  dt <- tr$dt
  d <- as.numeric(dt[[deff_col]])
  q <- as.numeric(dt[[quota_col]])
  dfv <- as.numeric(dt[[df_col]])
  lev <- tr$lev
  pi <- tr$pi

  valid <- is.finite(d) & d > 0 & is.finite(dfv) & dfv > 0
  y <- data.table::fifelse(valid, log(pmax(d, .Machine$double.xmin)), NA_real_)
  s2 <- data.table::fifelse(valid, 2 / dfv, NA_real_)

  # 3a. GVF by level
  levels_u <- sort(unique(lev))
  g <- numeric(length(d))
  gvf_tab <- vector("list", length(levels_u))
  for (i in seq_along(levels_u)) {
    idx <- which(lev == levels_u[i])
    fit_idx <- idx[valid[idx] & is.finite(q[idx]) & q[idx] > 0 & q[idx] < 1]
    a <- 0
    b <- 0
    if (length(fit_idx) > 0L) {
      lq <- log(q[fit_idx])
      w <- dfv[fit_idx]
      if (length(fit_idx) >= 3L && stats::var(lq) > 0) {
        cf <- stats::lm.wfit(cbind(1, lq), y[fit_idx], w = w)$coefficients
        a <- unname(cf[1L])
        b <- unname(cf[2L])
      } else {
        a <- sum(w * y[fit_idx]) / sum(w)
      }
      rng <- range(lq)
      lq_all <- log(q[idx])
      lq_all[!is.finite(lq_all) | is.na(lq_all)] <- rng[1L]
      lq_all <- pmin(pmax(lq_all, rng[1L]), rng[2L])
      g[idx] <- a + b * lq_all
    } else {
      g[idx] <- a
    }
    gvf_tab[[i]] <- data.table::data.table(
      livello = levels_u[i],
      intercetta = a,
      pendenza = b,
      n_nodi = length(fit_idx)
    )
  }

  # 3b. shrinkage, top-down
  ls <- g
  B <- rep(1, length(d))
  tau_tab <- vector("list", length(levels_u))
  if (method != "gvf") {
    for (i in seq_along(levels_u)) {
      idx <- which(lev == levels_u[i])
      has_par <- !is.na(pi[idx])
      m <- g[idx]
      if (method == "parent") {
        m[has_par] <- ls[pi[idx][has_par]]
      } else {
        m[has_par] <- g[idx][has_par] +
          (ls[pi[idx][has_par]] - g[pi[idx][has_par]])
      }
      ok <- valid[idx]
      tau2 <- 0
      if (any(ok)) {
        w <- 1 / s2[idx][ok]
        qstat <- sum(w * (y[idx][ok] - m[ok])^2)
        tau2 <- max(0, (qstat - sum(ok)) / sum(w))
      }
      b_k <- rep(1, length(idx))
      b_k[ok] <- s2[idx][ok] / (s2[idx][ok] + tau2)
      val <- m
      val[ok] <- b_k[ok] * m[ok] + (1 - b_k[ok]) * y[idx][ok]
      ls[idx] <- val
      B[idx] <- b_k
      tau_tab[[i]] <- data.table::data.table(
        livello = levels_u[i],
        tau2 = tau2,
        n_nodi = sum(ok)
      )
    }
  }

  dt[, deff_liscio := pmax(1, exp(ls))]
  dt[, peso_contrazione := B]
  data.table::setattr(dt, "gvf", data.table::rbindlist(gvf_tab))
  data.table::setattr(
    dt,
    "tau2",
    if (method == "gvf") NULL else data.table::rbindlist(tau_tab)
  )
  dt[]
}

# 4. fit_betabinom -----

#' Maximum likelihood fit of a beta-binomial distribution
#'
#' Estimates the mean `mu` and the concentration `kappa` of a
#' beta-binomial distribution from counts `x` out of `n`, with
#' parametrisation `alpha = mu kappa`, `beta = (1 - mu) kappa`.
#'
#' @param x Numeric vector of counts (non-integer values, e.g. counts
#'   divided by a design effect, are allowed).
#' @param n Numeric vector of trials, same length as `x` or length 1.
#'   Rows with `n = 0` are dropped.
#' @param mean `NULL` (default) to estimate the mean, or a known mean:
#'   a single value or one value per element of `x`, in (0, 1). With a
#'   known mean only `kappa` is estimated.
#'
#' @return A list with `mean` (estimated or given mean), `kappa`
#'   (concentration; `Inf` when the data show no overdispersion), `rho`
#'   (intra-class correlation `1 / (1 + kappa)`), `loglik` (maximised
#'   log-likelihood, binomial coefficients included) and `convergence`
#'   (0 on success, as in [stats::optim()]).
#'
#' @details
#' The log-likelihood is
#' `sum(lchoose(n, x) + lbeta(x + alpha, n - x + beta) - lbeta(alpha, beta))`,
#' with the binomial coefficient computed through `lgamma()`. With an
#' unknown mean the maximisation runs on `(logit(mu), log(kappa))` with
#' [stats::optim()] (method `"L-BFGS-B"`, `kappa` in `[1e-6, 1e8]`); with
#' a known mean it runs on `log(kappa)` with [stats::optimize()].
#'
#' When the maximum is reached at the upper bound of `kappa` the data are
#' compatible with a binomial distribution: the function then returns
#' `kappa = Inf`, `rho = 0`, the binomial log-likelihood and a warning.
#' A warning and `kappa = Inf` are also returned when all counts are 0
#' or all equal `n` and the mean is unknown.
#'
#' @examples
#' set.seed(1)
#' p <- stats::rbeta(40, 2, 18)
#' x <- stats::rbinom(40, 500, p)
#' fit_betabinom(x, 500)
#' fit_betabinom(x, 500, mean = 0.1)
#'
#' @export
fit_betabinom <- function(x, n, mean = NULL) {
  caller <- "fit_betabinom"
  if (!is.numeric(x) || !is.numeric(n) || length(x) == 0L) {
    stop(
      caller,
      ": `x` and `n` must be non-empty numeric vectors",
      call. = FALSE
    )
  }
  if (length(n) == 1L) {
    n <- rep(n, length(x))
  }
  if (length(n) != length(x)) {
    stop(caller, ": `n` must have length 1 or the length of `x`", call. = FALSE)
  }
  if (anyNA(x) || anyNA(n) || any(x < 0) || any(n < 0) || any(x > n)) {
    stop(
      caller,
      ": `x` and `n` must be non-negative without NA, with x <= n",
      call. = FALSE
    )
  }
  if (!is.null(mean)) {
    if (
      !is.numeric(mean) ||
        anyNA(mean) ||
        any(mean <= 0 | mean >= 1) ||
        !(length(mean) %in% c(1L, length(x)))
    ) {
      stop(
        caller,
        ": `mean` must be NULL or values in (0, 1) of length 1 or length(x)",
        call. = FALSE
      )
    }
    mean <- rep_len(mean, length(x))
  }
  keep <- n > 0
  if (!any(keep)) {
    stop(
      caller,
      ": at least one element of `n` must be positive",
      call. = FALSE
    )
  }
  x <- x[keep]
  n <- n[keep]
  if (!is.null(mean)) {
    mean <- mean[keep]
  }
  lc <- sum(lgamma(n + 1) - lgamma(x + 1) - lgamma(n - x + 1))
  binom_ll <- function(mu) {
    mu <- pmin(pmax(mu, 1e-300), 1 - 1e-16)
    lc + sum(x * log(mu) + (n - x) * log1p(-mu))
  }
  no_overdispersion <- function(mu, conv) {
    warning(
      caller,
      ": no overdispersion detected, kappa set to Inf",
      call. = FALSE
    )
    list(
      mean = mu,
      kappa = Inf,
      rho = 0,
      loglik = binom_ll(mu),
      convergence = conv
    )
  }

  if (!is.null(mean)) {
    k <- .si_kappa_bb(x, n, mean)
    mu_out <- if (length(unique(mean)) == 1L) mean[1L] else mean
    if (is.infinite(k$kappa)) {
      return(no_overdispersion(mu_out, 0L))
    }
    return(list(
      mean = mu_out,
      kappa = k$kappa,
      rho = 1 / (1 + k$kappa),
      loglik = lc + k$loglik,
      convergence = 0L
    ))
  }

  mu0 <- sum(x) / sum(n)
  if (mu0 == 0 || mu0 == 1) {
    return(no_overdispersion(mu0, 0L))
  }
  lower <- c(-30, log(1e-6))
  upper <- c(30, log(1e8))
  nll <- function(th) {
    mu <- stats::plogis(th[1L])
    k <- exp(th[2L])
    -sum(lbeta(x + mu * k, n - x + (1 - mu) * k) - lbeta(mu * k, (1 - mu) * k))
  }
  opt <- stats::optim(
    c(stats::qlogis(mu0), log(10)),
    nll,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper
  )
  # flat likelihood towards kappa = Inf: compare with the upper bound
  at_bound <- -nll(c(opt$par[1L], upper[2L])) >= -opt$value - 1e-8
  if (opt$par[2L] >= upper[2L] - 0.01 || at_bound) {
    return(no_overdispersion(mu0, opt$convergence))
  }
  k <- exp(opt$par[2L])
  list(
    mean = stats::plogis(opt$par[1L]),
    kappa = k,
    rho = 1 / (1 + k),
    loglik = lc - opt$value,
    convergence = opt$convergence
  )
}

# 5. eb_share_tree -----

#' Empirical Bayes shares on a code hierarchy
#'
#' Shrinks the shares of the codes of a hierarchy towards a prior mean
#' with a concentration estimated from the data, and returns posterior
#' means and credible intervals.
#'
#' @param est data.frame or data.table with one row per node and columns
#'   `key_col`, `parent_col`, `level_col`, `x_col`, `n_col`.
#' @param key_col Name of the node identifier column.
#' @param parent_col Name of the parent column (`NA` or `""` for roots).
#' @param level_col Name of the level column; parents must have a lower
#'   level than their children.
#' @param type `"partition"` (children partition the parent, e.g.
#'   professions with an "unknown" category) or `"overlap"` (each node
#'   has its own count with no sum constraint, e.g. postings with at
#'   least one skill of a group).
#' @param prior_mean Optional name of a column with the prior mean. For
#'   `"partition"` it is the conditional share child | parent (renormalised
#'   within each family); for `"overlap"` it is the share of the node.
#'   See Details for the default.
#' @param deff_col Optional name of a design effect column; counts are
#'   divided by it before the fit.
#' @param conf Credibility level, default 0.95.
#' @param draws Number of posterior draws for `"partition"` (default
#'   2000). Set the seed with [set.seed()] before the call for
#'   reproducible intervals; the function does not set it.
#' @param x_col,n_col Names of the count and denominator columns
#'   (defaults `"x"`, `"n"`).
#' @param tol Tolerance of the coherence checks (default `1e-8`).
#'
#' @return A copy of `est` with columns `quota_eb` (posterior mean),
#'   `lo_eb`, `hi_eb` (credible interval), `kappa_famiglia` (estimated
#'   concentration of the family the node belongs to; `NA` when not
#'   estimable, `Inf` when the data show no extra dispersion around the
#'   prior mean) and `vincoli_ok` (coherence check on parent nodes, `NA`
#'   on leaves).
#'
#' @details
#' **Partition.** A family is the set of children of one parent (the
#' roots form one family with total `n`). Within a family the children
#' counts `x_c` must sum to the parent count (to `n` for the roots).
#' Counts are divided by the family design effect, the `x`-weighted mean
#' of the children `deff`. The family vector follows a
#' Dirichlet-multinomial with mean `m` (the prior mean, or the uniform
#' share `1 / K` when `prior_mean` is `NULL`) and concentration `kappa`,
#' estimated by maximising the Dirichlet-multinomial likelihood of the
#' family. The joint likelihood is used instead of separate beta-binomial
#' margins because it is the exact law of the family vector: the margins
#' share the same `kappa` and are dependent, so their product would count
#' the same information more than once. The posterior of the conditional
#' shares is Dirichlet(`kappa m + x_eff`); families are independent, so
#' the posterior mean of a marginal share is the product of the
#' conditional posterior means along the path to the root, and the
#' posterior means of the children sum to that of the parent exactly.
#' Intervals are quantiles of `draws` posterior draws (normalised Gamma
#' variables) multiplied along the path.
#'
#' **Overlap.** For each family `kappa` is estimated from the beta-binomial
#' likelihood of the children with their own prior means (see
#' [fit_betabinom()]); the posterior of each node is
#' Beta(`kappa m + x_eff`, `kappa (1 - m) + n_eff - x_eff`) and the
#' interval uses its quantiles. Without `prior_mean` no shrinkage of the
#' mean is applied: `quota_eb = x / n` and the interval is the Jeffreys
#' interval on the effective counts; the hierarchy then acts only through
#' the design effect. `vincoli_ok` checks, on parent nodes,
#' `max(children) <= parent <= min(1, sum(children))` up to `tol`.
#'
#' A concentration at the upper bound (`1e8`) is reported as `Inf` and
#' the posterior collapses on the prior mean; the uncertainty about
#' `kappa` is not propagated (empirical Bayes).
#'
#' @references
#' Mosimann, J. E. (1962). On the compound multinomial distribution, the
#' multivariate beta-distribution, and correlations among proportions.
#' *Biometrika*, 49(1/2), 65-82.
#'
#' Efron, B. and Morris, C. (1975). Data analysis using Stein's estimator
#' and its generalizations. *Journal of the American Statistical
#' Association*, 70(350), 311-319.
#'
#' @examples
#' est <- data.frame(
#'   code = c("A", "B", "A1", "A2", "B1", "B2"),
#'   parent = c(NA, NA, "A", "A", "B", "B"),
#'   level = c(1, 1, 2, 2, 2, 2),
#'   x = c(600, 400, 590, 10, 200, 200),
#'   n = 1000
#' )
#' set.seed(1)
#' eb_share_tree(est, "code", "parent", "level", draws = 500)
#'
#' @export
eb_share_tree <- function(
  est,
  key_col,
  parent_col,
  level_col,
  type = c("partition", "overlap"),
  prior_mean = NULL,
  deff_col = NULL,
  conf = 0.95,
  draws = 2000L,
  x_col = "x",
  n_col = "n",
  tol = 1e-8
) {
  caller <- "eb_share_tree"
  type <- match.arg(type)
  .si_check_conf(conf, caller)
  if (!is.numeric(draws) || length(draws) != 1L || is.na(draws) || draws < 10) {
    stop(caller, ": `draws` must be a number >= 10", call. = FALSE)
  }
  draws <- as.integer(draws)
  tr <- .si_prepare_tree(
    est,
    key_col,
    parent_col,
    level_col,
    caller,
    strict_parent = type == "partition"
  )
  check_columns(
    tr$dt,
    c(x_col, n_col, prior_mean, deff_col),
    caller = caller
  )
  dt <- tr$dt
  x <- as.numeric(dt[[x_col]])
  n <- as.numeric(dt[[n_col]])
  if (anyNA(x) || anyNA(n) || any(x < 0) || any(n <= 0) || any(x > n)) {
    stop(
      caller,
      ": counts must be non-negative, denominators positive, x <= n",
      call. = FALSE
    )
  }
  d <- if (is.null(deff_col)) rep(1, length(x)) else as.numeric(dt[[deff_col]])
  d[is.na(d)] <- 1
  if (any(d <= 0)) {
    stop(caller, ": design effects must be positive", call. = FALSE)
  }
  m0 <- if (is.null(prior_mean)) NULL else as.numeric(dt[[prior_mean]])
  fam <- data.table::fifelse(tr$root, "\001root", tr$par)
  fam_idx <- split(seq_along(x), fam)
  alpha <- 1 - conf
  kappa <- rep(NA_real_, length(x))

  if (type == "partition") {
    res <- .si_eb_partition(
      x,
      n,
      d,
      m0,
      tr,
      fam_idx,
      draws,
      alpha,
      tol,
      caller
    )
  } else {
    res <- .si_eb_overlap(x, n, d, m0, fam_idx, alpha)
  }

  # coherence checks on parent nodes
  ch_idx <- split(which(!tr$root), tr$pi[!tr$root])
  ok <- rep(NA, length(x))
  qeb <- res$quota_eb
  for (p in names(ch_idx)) {
    pr <- as.integer(p)
    qc <- qeb[ch_idx[[p]]]
    ok[pr] <- if (type == "partition") {
      abs(sum(qc) - qeb[pr]) <= tol
    } else {
      max(qc) <= qeb[pr] + tol && qeb[pr] <= min(1, sum(qc)) + tol
    }
  }

  dt[, quota_eb := res$quota_eb]
  dt[, lo_eb := res$lo_eb]
  dt[, hi_eb := res$hi_eb]
  dt[, kappa_famiglia := res$kappa]
  dt[, vincoli_ok := ok]
  dt[]
}

#' Partition branch of eb_share_tree()
#' @keywords internal
#' @noRd
.si_eb_partition <- function(
  x,
  n,
  d,
  m0,
  tr,
  fam_idx,
  draws,
  alpha,
  tol,
  caller
) {
  nn <- length(x)
  cond_mean <- numeric(nn)
  cond_draw <- matrix(0, nn, draws)
  kappa <- rep(NA_real_, nn)
  for (f in names(fam_idx)) {
    ch <- fam_idx[[f]]
    if (f == "\001root") {
      tot <- unique(n[ch])
      if (length(tot) != 1L) {
        stop(caller, ": root nodes must share the same `n`", call. = FALSE)
      }
    } else {
      tot <- x[match(f, tr$key)]
    }
    if (abs(sum(x[ch]) - tot) > tol * max(1, tot)) {
      stop(
        caller,
        ": children counts do not sum to the parent count (family ",
        if (f == "\001root") "of the roots" else f,
        ")",
        call. = FALSE
      )
    }
    K <- length(ch)
    m <- if (is.null(m0)) rep(1 / K, K) else m0[ch]
    m[is.na(m) | m < 0] <- 0
    m <- pmax(m, 1e-10)
    m <- m / sum(m)
    if (K == 1L) {
      cond_mean[ch] <- 1
      cond_draw[ch, ] <- 1
      next
    }
    if (tot == 0) {
      cond_mean[ch] <- m
      cond_draw[ch, ] <- m
      next
    }
    df_ <- if (sum(x[ch]) > 0) {
      sum(d[ch] * x[ch]) / sum(x[ch])
    } else {
      mean(d[ch])
    }
    xe <- x[ch] / df_
    ne <- tot / df_
    k <- .si_kappa_dm(xe, m)
    kappa[ch] <- k$kappa
    k_num <- if (is.infinite(k$kappa)) 1e8 else k$kappa
    a <- k_num * m + xe
    cond_mean[ch] <- a / (k_num + ne)
    gm <- matrix(stats::rgamma(K * draws, shape = a), K, draws)
    cs <- colSums(gm)
    bad <- !is.finite(cs) | cs <= 0
    gm <- sweep(gm, 2L, cs, "/")
    if (any(bad)) {
      gm[, bad] <- cond_mean[ch]
    }
    cond_draw[ch, ] <- gm
  }

  # marginal shares along the path, top-down
  marg_mean <- cond_mean
  marg_draw <- cond_draw
  for (lv in sort(unique(tr$lev))) {
    idx <- which(tr$lev == lv & !tr$root)
    if (length(idx) == 0L) {
      next
    }
    pp <- tr$pi[idx]
    marg_mean[idx] <- marg_mean[pp] * cond_mean[idx]
    marg_draw[idx, ] <- marg_draw[pp, , drop = FALSE] *
      cond_draw[idx, , drop = FALSE]
  }
  qs <- apply(
    marg_draw,
    1L,
    stats::quantile,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE
  )
  list(
    quota_eb = marg_mean,
    lo_eb = pmin(qs[1L, ], marg_mean),
    hi_eb = pmax(qs[2L, ], marg_mean),
    kappa = kappa
  )
}

#' Overlap branch of eb_share_tree()
#' @keywords internal
#' @noRd
.si_eb_overlap <- function(x, n, d, m0, fam_idx, alpha) {
  xe <- x / d
  ne <- n / d
  kappa <- rep(NA_real_, length(x))
  if (is.null(m0)) {
    a <- xe + 0.5
    b <- ne - xe + 0.5
    lo <- data.table::fifelse(x == 0, 0, stats::qbeta(alpha / 2, a, b))
    hi <- data.table::fifelse(x == n, 1, stats::qbeta(1 - alpha / 2, a, b))
    return(list(quota_eb = x / n, lo_eb = lo, hi_eb = hi, kappa = kappa))
  }
  m <- pmin(pmax(m0, 1e-10), 1 - 1e-10)
  m[is.na(m)] <- x[is.na(m)] / n[is.na(m)]
  m <- pmin(pmax(m, 1e-10), 1 - 1e-10)
  for (f in names(fam_idx)) {
    ch <- fam_idx[[f]]
    kappa[ch] <- .si_kappa_bb(xe[ch], ne[ch], m[ch])$kappa
  }
  k_num <- data.table::fifelse(is.infinite(kappa), 1e8, kappa)
  a <- k_num * m + xe
  b <- k_num * (1 - m) + ne - xe
  list(
    quota_eb = a / (a + b),
    lo_eb = stats::qbeta(alpha / 2, a, b),
    hi_eb = stats::qbeta(1 - alpha / 2, a, b),
    kappa = kappa
  )
}

# 6. cluster_boot_share -----

#' Cluster bootstrap interval for shares
#'
#' Resamples clusters (sources) with replacement and returns percentile
#' intervals and bootstrap standard errors of the share of every key.
#'
#' @inheritParams cluster_deff
#' @param B Number of bootstrap replicates (default 1000).
#' @param conf Confidence level, default 0.95.
#'
#' @return A data.table with one row per key and columns `key_cols`,
#'   `quota`, `lo_boot`, `hi_boot`, `se_boot`.
#'
#' @details
#' Let `X` be the sparse key x cluster count matrix and `n_s` the
#' cluster denominators. The multiplicities of the `S` clusters in `B`
#' replicates form the matrix `M` (`S x B`, columns drawn from a
#' multinomial with `S` trials and equal probabilities); the replicate
#' counts are `X M` and the replicate denominators `n_s M`, so all
#' replicates are computed with two matrix products (processed in blocks
#' of keys to bound memory). The interval is the percentile interval of
#' the replicate shares; replicates with a zero denominator are dropped.
#' Set the seed with [set.seed()] before the call; the function does not
#' set it. The bootstrap of the clusters is the resampling counterpart of
#' the linearised variance of [cluster_deff()] and shares its
#' assumptions: clusters are treated as a sample of sources.
#'
#' @references
#' Davison, A. C. and Hinkley, D. V. (1997). *Bootstrap Methods and their
#' Application*. Cambridge University Press, section 3.8.
#'
#' @examples
#' counts <- data.frame(
#'   code = rep(c("a", "b"), each = 4),
#'   source = rep(c("s1", "s2", "s3", "s4"), 2),
#'   x = c(10, 40, 5, 25, 3, 2, 4, 1)
#' )
#' totals <- data.frame(source = c("s1", "s2", "s3", "s4"),
#'                      n = c(200, 400, 100, 300))
#' set.seed(1)
#' cluster_boot_share(counts, "code", B = 200, cluster_totals = totals)
#'
#' @export
cluster_boot_share <- function(
  counts,
  key_cols,
  cluster_col = "source",
  B = 1000L,
  conf = 0.95,
  cluster_totals = NULL,
  x_col = "x",
  n_col = "n"
) {
  caller <- "cluster_boot_share"
  .si_check_conf(conf, caller)
  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 2) {
    stop(caller, ": `B` must be a number >= 2", call. = FALSE)
  }
  B <- as.integer(B)
  prep <- .si_prepare_counts(
    counts,
    key_cols,
    cluster_col,
    x_col,
    n_col,
    cluster_totals,
    caller
  )
  dt <- prep$dt[.n > 0]
  tot <- prep$totals
  clusters <- if (is.null(tot)) {
    sort(unique(dt$.s))
  } else {
    sort(unique(tot[.ns > 0]$.s))
  }
  S <- length(clusters)
  if (S < 2L) {
    stop(caller, ": at least two clusters are needed", call. = FALSE)
  }
  keys <- unique(prep$dt[, key_cols, with = FALSE])
  keys[, .kid := .I]
  dt[keys, .kid := i..kid, on = key_cols]
  dt[, .sid := match(.s, clusters)]

  X <- Matrix::sparseMatrix(
    i = dt$.kid,
    j = dt$.sid,
    x = dt$.x,
    dims = c(nrow(keys), S)
  )
  if (is.null(tot)) {
    Nm <- Matrix::sparseMatrix(
      i = dt$.kid,
      j = dt$.sid,
      x = dt$.n,
      dims = c(nrow(keys), S)
    )
    n_k <- Matrix::rowSums(Nm)
  } else {
    ns <- tot$.ns[match(clusters, tot$.s)]
    n_k <- rep(sum(ns), nrow(keys))
  }
  x_k <- Matrix::rowSums(X)

  M <- stats::rmultinom(B, S, rep(1 / S, S))
  if (!is.null(tot)) {
    nb_all <- as.numeric(crossprod(ns, M))
  }
  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
  K <- nrow(keys)
  lo <- hi <- se <- numeric(K)
  block <- max(1L, floor(5e6 / B))
  starts <- seq.int(1L, K, by = block)
  for (st in starts) {
    rows <- st:min(K, st + block - 1L)
    xb <- as.matrix(X[rows, , drop = FALSE] %*% M)
    nb <- if (is.null(tot)) {
      as.matrix(Nm[rows, , drop = FALSE] %*% M)
    } else {
      matrix(nb_all, length(rows), B, byrow = TRUE)
    }
    r <- xb / nb
    r[!is.finite(r)] <- NA_real_
    qs <- apply(
      r,
      1L,
      stats::quantile,
      probs = probs,
      na.rm = TRUE,
      names = FALSE
    )
    lo[rows] <- qs[1L, ]
    hi[rows] <- qs[2L, ]
    cnt <- rowSums(!is.na(r))
    mu <- rowMeans(r, na.rm = TRUE)
    se[rows] <- sqrt(rowSums((r - mu)^2, na.rm = TRUE) / pmax(cnt - 1, 1))
  }
  keys[, .kid := NULL]
  keys[, `:=`(
    quota = x_k / n_k,
    lo_boot = lo,
    hi_boot = hi,
    se_boot = se
  )]
  keys[]
}

# 7. tree_fdr -----

#' Hierarchical false discovery rate control on a tree
#'
#' Tests the hypotheses of a tree from the top: the Benjamini-Hochberg
#' procedure is applied within each family of siblings, and the children
#' of a node are tested only when the node is rejected.
#'
#' @param p Numeric vector of p-values, one per node (`NA` = not testable;
#'   the node and its descendants are not tested).
#' @param node Character vector of node identifiers (unique).
#' @param parent Character vector of parent identifiers; `NA`, `""` or an
#'   identifier not in `node` marks a node of the first family.
#' @param q Target level within each family (default 0.05).
#'
#' @return A data.table in the order of `node` with columns `node`,
#'   `parent`, `p`, `q_famiglia` (BH-adjusted p-value within the family,
#'   `NA` when not tested), `tested`, `rejected`.
#'
#' @details
#' The procedure follows Yekutieli (2008): the family of the nodes
#' without a parent is tested first with Benjamini-Hochberg at level `q`;
#' for each rejected node, the family of its children is tested in the
#' same way, and so on down the tree. Yekutieli shows that, with
#' independent p-values, the expected proportion of false discoveries
#' over the whole tree is bounded by `q` times a factor that depends on
#' the tree (about 2 times the number of levels in the worst case);
#' Benjamini and Bogomolov (2014) give the selective FDR interpretation
#' of the family-wise adjusted values.
#'
#' @references
#' Yekutieli, D. (2008). Hierarchical false discovery rate-controlling
#' methodology. *Journal of the American Statistical Association*,
#' 103(481), 309-316.
#'
#' Benjamini, Y. and Bogomolov, M. (2014). Selective inference on
#' multiple families of hypotheses. *Journal of the Royal Statistical
#' Society: Series B*, 76(1), 297-318.
#'
#' @examples
#' tree_fdr(
#'   p = c(0.001, 0.4, 0.01, 0.03, 0.2),
#'   node = c("A", "B", "A1", "A2", "B1"),
#'   parent = c(NA, NA, "A", "A", "B")
#' )
#'
#' @export
tree_fdr <- function(p, node, parent, q = 0.05) {
  caller <- "tree_fdr"
  if (!is.numeric(p) && !all(is.na(p))) {
    stop(caller, ": `p` must be numeric", call. = FALSE)
  }
  p <- as.numeric(p)
  node <- as.character(node)
  parent <- as.character(parent)
  if (length(node) != length(p) || length(parent) != length(p)) {
    stop(
      caller,
      ": `p`, `node` and `parent` must have the same length",
      call. = FALSE
    )
  }
  if (anyNA(node) || anyDuplicated(node) > 0L) {
    stop(caller, ": `node` must be unique and not NA", call. = FALSE)
  }
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop(caller, ": `p` must be in [0, 1]", call. = FALSE)
  }
  if (!is.numeric(q) || length(q) != 1L || is.na(q) || q <= 0 || q >= 1) {
    stop(caller, ": `q` must be a single number in (0, 1)", call. = FALSE)
  }
  root <- .si_is_root(parent, node)
  pi <- match(parent, node)
  pi[root] <- NA_integer_
  children <- split(which(!root), pi[!root])

  qf <- rep(NA_real_, length(p))
  tested <- rep(FALSE, length(p))
  rejected <- rep(FALSE, length(p))
  test_family <- function(idx) {
    idx <- idx[!is.na(p[idx])]
    if (length(idx) == 0L) {
      return(integer(0))
    }
    adj <- stats::p.adjust(p[idx], method = "BH")
    qf[idx] <<- adj
    tested[idx] <<- TRUE
    rejected[idx] <<- adj <= q
    idx[adj <= q]
  }
  frontier <- test_family(which(root))
  while (length(frontier) > 0L) {
    nxt <- integer(0)
    for (r in frontier) {
      ch <- children[[as.character(r)]]
      if (!is.null(ch)) {
        nxt <- c(nxt, test_family(ch))
      }
    }
    frontier <- nxt
  }
  data.table::data.table(
    node = node,
    parent = parent,
    p = p,
    q_famiglia = qf,
    tested = tested,
    rejected = rejected
  )
}

# 8. finest_reliable_level -----

#' Finest reliable level of a code hierarchy
#'
#' For every node, returns the node itself when its interval is narrow
#' enough, otherwise the closest ancestor whose interval is.
#'
#' @param est data.frame or data.table with one row per node.
#' @param key_col Name of the node identifier column.
#' @param parent_col Name of the parent column (`NA` or `""` for roots).
#' @param rel_width_col Name of the relative width column (e.g.
#'   `(hi - lo) / quota`); `NA` counts as not reliable.
#' @param max_rel_width Maximum relative width of a reliable node.
#' @param level_col Optional name of a level column; when given, the
#'   level of the reliable node is returned as well.
#'
#' @return A copy of `est` with column `codice_affidabile` (the node or
#'   its closest reliable ancestor, `NA` when none is reliable) and, when
#'   `level_col` is given, `livello_affidabile`.
#'
#' @details
#' The search walks up the parent links one level at a time, vectorised
#' over all nodes, and stops at the first node with
#' `rel_width <= max_rel_width`.
#'
#' @examples
#' est <- data.frame(
#'   code = c("A", "A1", "A11", "B", "B1"),
#'   parent = c(NA, "A", "A1", NA, "B"),
#'   level = c(1, 2, 3, 1, 2),
#'   ampiezza_rel = c(0.1, 0.4, 1.2, 0.9, 2)
#' )
#' finest_reliable_level(est, "code", "parent", "ampiezza_rel", 0.5,
#'                       level_col = "level")
#'
#' @export
finest_reliable_level <- function(
  est,
  key_col,
  parent_col,
  rel_width_col,
  max_rel_width,
  level_col = NULL
) {
  caller <- "finest_reliable_level"
  if (
    !is.numeric(max_rel_width) ||
      length(max_rel_width) != 1L ||
      is.na(max_rel_width) ||
      max_rel_width <= 0
  ) {
    stop(caller, ": `max_rel_width` must be a positive number", call. = FALSE)
  }
  tr <- .si_prepare_tree(est, key_col, parent_col, NULL, caller)
  check_columns(tr$dt, c(rel_width_col, level_col), caller = caller)
  dt <- tr$dt
  rw <- as.numeric(dt[[rel_width_col]])
  reliable <- !is.na(rw) & rw <= max_rel_width
  cur <- seq_len(nrow(dt))
  todo <- !reliable[cur]
  while (any(todo)) {
    cur[todo] <- tr$pi[cur[todo]]
    todo <- !is.na(cur) & !reliable[cur]
  }
  dt[, codice_affidabile := tr$key[cur]]
  if (!is.null(level_col)) {
    dt[, livello_affidabile := dt[[level_col]][cur]]
  }
  dt[]
}
