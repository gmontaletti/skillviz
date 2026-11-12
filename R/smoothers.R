# Smoothers, post-smoothing bands and robust re-smoothing -----
#
# Henderson moving averages with Musgrave end weights, the Whittaker-Henderson
# smoother, both with the smoothing parameter chosen by generalised
# cross-validation (GCV), and a robust two-pass trend extraction that applies
# bands after smoothing, sets the weeks outside the band to missing and
# smooths again.

# 1. internal helpers: Henderson -----

#' Check a vector of odd Henderson lengths (>= 3)
#' @keywords internal
#' @noRd
.hd_check_lengths <- function(x, name, caller) {
  ok <- is.numeric(x) &&
    length(x) >= 1L &&
    !anyNA(x) &&
    all(is.finite(x)) &&
    all(x == round(x)) &&
    all(x >= 3) &&
    all(x %% 2 == 1)
  if (!ok) {
    stop(
      caller,
      ": `",
      name,
      "` must contain odd integers >= 3",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Musgrave asymmetric weights for one end point
#'
#' `w` are the symmetric weights of length N = 2p + 1 (positions 1..N), `q`
#' the number of future observations available (0..p-1), so that only the
#' first M = p + 1 + q positions are observed. Returns the M weights applied
#' to the observed positions (Doherty 2001, eq. for the surrogate filters),
#' with D = 4 / (pi * R^2) written as 1 / D = pi * R^2 / 4 so that R = 0
#' (D infinite) and R = Inf (D = 0) are handled.
#' @keywords internal
#' @noRd
.hd_musgrave <- function(w, q, ic_ratio) {
  n_w <- length(w)
  p <- (n_w - 1L) %/% 2L
  m <- p + 1L + q
  cm <- (m + 1) / 2
  miss <- (m + 1L):n_w
  s0 <- sum(w[miss])
  s1 <- sum((miss - cm) * w[miss])
  inv_d <- pi * ic_ratio^2 / 4
  cc <- m * (m^2 - 1) / 12
  b <- if (is.infinite(inv_d)) 0 else s1 / (inv_d + cc)
  ia <- seq_len(m)
  w[ia] + s0 / m + (ia - cm) * b
}

#' Henderson filter of length H with Musgrave ends: trend and trace
#' @keywords internal
#' @noRd
.hd_apply <- function(x, H, ic_ratio) {
  n <- length(x)
  w <- henderson_weights(H)
  p <- (H - 1L) %/% 2L
  tr <- as.numeric(stats::filter(x, w, sides = 2L))
  dg <- rep(w[p + 1L], n)
  for (q in seq_len(p) - 1L) {
    u <- .hd_musgrave(w, q, ic_ratio)
    te <- n - q
    tr[te] <- sum(u * x[(te - p):n])
    dg[te] <- u[p + 1L]
    ts <- q + 1L
    tr[ts] <- sum(rev(u) * x[seq_len(ts + p)])
    dg[ts] <- u[p + 1L]
  }
  list(trend = tr, trace = sum(dg))
}

#' I/C ratio from a preliminary Henderson pass (13 terms, R = 3.5)
#' @keywords internal
#' @noRd
.hd_ic_ratio <- function(x) {
  n <- length(x)
  H <- if (n >= 13L) {
    13L
  } else if (n %% 2L == 1L) {
    n
  } else {
    n - 1L
  }
  cc <- .hd_apply(x, H, 3.5)$trend
  dc <- mean(abs(diff(cc)))
  di <- mean(abs(diff(x - cc)))
  if (dc > 0) {
    di / dc
  } else if (di > 0) {
    Inf
  } else {
    0
  }
}

# 2. henderson_weights -----

#' Symmetric Henderson weights
#'
#' Weights of the symmetric Henderson moving average of a given odd length,
#' from the closed formula.
#'
#' @details
#' With `length = 2p + 1` and `m = p + 2`, the weight at offset
#' \eqn{j = -p, \dots, p} is
#' \deqn{w_j = \frac{315\,[(m-1)^2 - j^2][m^2 - j^2][(m+1)^2 - j^2]
#'   [3m^2 - 16 - 11j^2]}{8m(m^2-1)(4m^2-1)(4m^2-9)(4m^2-25)}.}
#' The weights sum to one, are symmetric, reproduce polynomials of degree up
#' to three and minimise the sum of squares of the third differences of the
#' weights (Henderson 1916; Kenny and Durbin 1982). For `length = 3` the
#' filter is the identity `(0, 1, 0)`.
#'
#' @param length Odd integer >= 3, number of terms.
#'
#' @return A numeric vector of `length` weights, for offsets `-p..p`.
#'
#' @examples
#' w <- henderson_weights(13)
#' round(w, 5)
#' sum(w)
#'
#' @export
henderson_weights <- function(length) {
  caller <- "henderson_weights"
  .hd_check_lengths(length, "length", caller)
  if (base::length(length) != 1L) {
    stop(caller, ": `length` must be a single odd integer", call. = FALSE)
  }
  p <- (length - 1) / 2
  m <- p + 2
  j <- -p:p
  num <- 315 *
    ((m - 1)^2 - j^2) *
    (m^2 - j^2) *
    ((m + 1)^2 - j^2) *
    (3 * m^2 - 16 - 11 * j^2)
  den <- 8 * m * (m^2 - 1) * (4 * m^2 - 1) * (4 * m^2 - 9) * (4 * m^2 - 25)
  num / den
}

# 3. henderson_filter -----

#' Henderson trend filter with Musgrave end weights
#'
#' Applies a Henderson moving average to a complete series, with the
#' symmetric weights in the interior and the Musgrave asymmetric weights at
#' the first and last `p` points. The length can be chosen among candidates
#' by generalised cross-validation.
#'
#' @details
#' **Interior and ends.** For a length `H = 2p + 1`, points `p + 1` to
#' `n - p` get the symmetric weights of [henderson_weights()]. At the end
#' point with `q < p` future observations, the Musgrave weights on the
#' `M = p + 1 + q` available observations are (Doherty 2001)
#' \deqn{u_i = w_i + \frac{1}{M}\sum_{j=M+1}^{N} w_j +
#'   \frac{(i - \frac{M+1}{2})\,D}{1 + \frac{M(M-1)(M+1)}{12}D}
#'   \sum_{j=M+1}^{N}\left(j - \frac{M+1}{2}\right) w_j,}
#' with \eqn{D = 4 / (\pi R^2)} and `R` the I/C ratio (mean absolute weekly
#' change of the irregular over that of the trend). The weights sum to one.
#' They minimise the expected revision under a locally linear trend with
#' noise, so they reproduce a straight line exactly only in the limit
#' `R -> 0`; for `R > 0` the slope enters as a penalty, not a constraint.
#' The first `p` points use the mirrored weights.
#'
#' **I/C ratio.** When `ic_ratio = NULL`, `R` is estimated from a
#' preliminary 13-term Henderson pass with `R = 3.5` (the X-11 value for that
#' length): `R = mean(|diff(x - C)|) / mean(|diff(C)|)`, `C` the preliminary
#' trend. Series shorter than 13 use the longest odd length that fits. The
#' same `R` is used for every candidate length.
#'
#' **Length.** When `length = NULL`, each candidate of `lengths` not longer
#' than the series is applied and the one with the smallest
#' \eqn{GCV = n\,RSS / (n - \mathrm{tr}\,S)^2} is kept, where `S` is the
#' linear smoother matrix (symmetric rows in the interior, Musgrave rows at
#' the ends) and \eqn{\mathrm{tr}\,S} its trace (the equivalent degrees of
#' freedom). GCV assumes independent errors; with positively autocorrelated
#' noise it tends to choose short filters.
#'
#' **Missing values.** The filter needs a complete series: `x` with `NA`
#' raises an error. Fill missing values first (e.g. linear interpolation, or
#' the fitted values of a previous pass as in X-11).
#'
#' @param x Numeric vector, complete (no `NA`) and finite.
#' @param length `NULL` (choose by GCV among `lengths`) or a single odd
#'   integer >= 3 not longer than `x`.
#' @param lengths Candidate odd lengths when `length = NULL`.
#' @param ic_ratio `NULL` (estimate) or a non-negative I/C ratio for the
#'   Musgrave end weights.
#'
#' @return A list with `trend` (numeric, same length as `x`), `length`
#'   (chosen length), `ic_ratio` (used), `edf` (trace of the smoother
#'   matrix), `gcv` (GCV of the chosen length) and `path` (data.table with
#'   `length`, `edf` and `gcv` for every candidate evaluated).
#'
#' @references
#' Doherty, M. (2001). The surrogate Henderson filters in X-11. *Australian
#' and New Zealand Journal of Statistics*, 43(4), 385-392.
#'
#' Musgrave, J. C. (1964). A set of end weights to end all end weights.
#' Working paper, U.S. Bureau of the Census.
#'
#' @examples
#' set.seed(1)
#' x <- sin(seq(0, 4 * pi, length.out = 120)) + rnorm(120, 0, 0.2)
#' h <- henderson_filter(x)
#' h$length
#' h$path
#' h13 <- henderson_filter(x, length = 13L, ic_ratio = 3.5)
#' head(h13$trend)
#'
#' @export
henderson_filter <- function(
  x,
  length = NULL,
  lengths = c(13L, 23L, 39L),
  ic_ratio = NULL
) {
  caller <- "henderson_filter"

  # 3.1 validation -----
  if (!is.numeric(x)) {
    stop(caller, ": `x` must be numeric", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(
      caller,
      ": `x` must be finite without NA; fill missing values first ",
      "(e.g. linear interpolation)",
      call. = FALSE
    )
  }
  x <- as.numeric(x)
  n <- base::length(x)
  if (is.null(length)) {
    .hd_check_lengths(lengths, "lengths", caller)
    cand <- sort(unique(as.integer(lengths)))
  } else {
    .hd_check_lengths(length, "length", caller)
    if (base::length(length) != 1L) {
      stop(caller, ": `length` must be a single odd integer", call. = FALSE)
    }
    cand <- as.integer(length)
  }
  if (!is.null(ic_ratio)) {
    .ti_check_num(ic_ratio, "ic_ratio", caller, 0, Inf)
  }
  cand <- cand[cand <= n]
  if (base::length(cand) == 0L) {
    stop(
      caller,
      ": the series (",
      n,
      " values) is shorter than every candidate length",
      call. = FALSE
    )
  }

  # 3.2 I/C ratio and GCV over lengths -----
  R <- if (is.null(ic_ratio)) .hd_ic_ratio(x) else ic_ratio
  fits <- lapply(cand, function(H) .hd_apply(x, H, R))
  edf <- vapply(fits, function(f) f$trace, numeric(1))
  rss <- vapply(fits, function(f) sum((x - f$trend)^2), numeric(1))
  gcv <- n * rss / (n - edf)^2
  pick <- which.min(gcv)
  if (base::length(pick) == 0L) {
    pick <- 1L
  }
  list(
    trend = fits[[pick]]$trend,
    length = cand[pick],
    ic_ratio = R,
    edf = edf[pick],
    gcv = gcv[pick],
    path = data.table::data.table(length = cand, edf = edf, gcv = gcv)
  )
}

# 4. internal helpers: Whittaker -----

#' Sparse difference matrix of a given order ((n - order) x n)
#' @keywords internal
#' @noRd
.wh_diff_matrix <- function(n, order) {
  k <- 0:order
  cf <- (-1)^(order - k) * choose(order, k)
  Matrix::bandSparse(
    n - order,
    n,
    k = k,
    diagonals = lapply(cf, function(v) rep(v, n - order))
  )
}

# 5. whittaker_smooth -----

#' Whittaker-Henderson smoother with GCV
#'
#' Penalised least-squares smoother: solves
#' \eqn{(W + \lambda D'D) z = W y}, with `D` the difference matrix of a given
#' order, and chooses \eqn{\lambda} by generalised cross-validation.
#'
#' @details
#' **Missing values and weights.** `NA` values of `y` get weight zero: the
#' smoother interpolates them (a polynomial spline of degree
#' `2 * order - 1` between observed points). A weight of zero and an `NA`
#' give the same result.
#'
#' **Limits.** As \eqn{\lambda \to 0} the solution tends to `y` (at points
#' with positive weight); as \eqn{\lambda \to \infty} it tends to the
#' weighted least-squares polynomial of degree `order - 1` (a straight line
#' for `order = 2`, which is the Hodrick-Prescott filter). For weekly series
#' the Ravn-Uhlig rule gives \eqn{\lambda = 1600 \cdot 13^4}, about
#' \eqn{10^{7.66}}, for comparison with the GCV choice.
#'
#' **GCV.** For each \eqn{\lambda = 10^g}, `g` in `log10_grid`,
#' \eqn{GCV = m\,RSS_w / (m - \mathrm{edf})^2}, with `m` the number of
#' positive weights, \eqn{RSS_w = \sum w_i (y_i - z_i)^2} and
#' \eqn{\mathrm{edf} = \mathrm{tr}[(W + \lambda D'D)^{-1} W]}. The system
#' is solved with sparse matrices (`Matrix`); the trace is exact, from the
#' dense inverse of the Cholesky factor, which costs \eqn{O(n^3)} per grid
#' value: fine for a few hundred weeks, slow for many thousands of points.
#' GCV assumes independent errors and tends to undersmooth under positive
#' autocorrelation.
#'
#' @param y Numeric vector; `NA` allowed (weight zero).
#' @param lambda `NULL` (choose by GCV) or a single positive smoothing
#'   parameter.
#' @param weights `NULL` (all ones) or non-negative weights of the same
#'   length as `y`.
#' @param order Order of the differences in the penalty (positive integer,
#'   default 2).
#' @param log10_grid Grid of \eqn{\log_{10}\lambda} values searched when
#'   `lambda = NULL`.
#'
#' @return A list with `trend` (numeric, same length as `y`), `lambda`
#'   (used), `edf` (equivalent degrees of freedom), `gcv` (GCV at `lambda`)
#'   and `path` (data.table with `lambda`, `edf` and `gcv` over the grid;
#'   one row when `lambda` is given).
#'
#' @references
#' Eilers, P. H. C. (2003). A perfect smoother. *Analytical Chemistry*,
#' 75(14), 3631-3636.
#'
#' @examples
#' set.seed(1)
#' y <- cumsum(rnorm(150, 0, 0.1)) + rnorm(150, 0, 0.3)
#' y[c(20, 21, 90)] <- NA
#' wh <- whittaker_smooth(y)
#' log10(wh$lambda)
#' wh$edf
#' # Ravn-Uhlig value for weekly data
#' whittaker_smooth(y, lambda = 1600 * 13^4)$edf
#'
#' @export
whittaker_smooth <- function(
  y,
  lambda = NULL,
  weights = NULL,
  order = 2L,
  log10_grid = seq(0, 9, 0.1)
) {
  caller <- "whittaker_smooth"

  # 5.1 validation -----
  if (!is.numeric(y)) {
    stop(caller, ": `y` must be numeric", call. = FALSE)
  }
  y <- as.numeric(y)
  if (any(!is.finite(y[!is.na(y)]))) {
    stop(caller, ": `y` must be finite or NA", call. = FALSE)
  }
  n <- length(y)
  .ti_check_num(order, "order", caller, 1, Inf, integer = TRUE)
  order <- as.integer(order)
  if (is.null(weights)) {
    w <- rep(1, n)
  } else {
    if (
      !is.numeric(weights) ||
        length(weights) != n ||
        anyNA(weights) ||
        any(!is.finite(weights)) ||
        any(weights < 0)
    ) {
      stop(
        caller,
        ": `weights` must be non-negative finite numbers, one per value",
        call. = FALSE
      )
    }
    w <- as.numeric(weights)
  }
  if (!is.null(lambda)) {
    .ti_check_num(lambda, "lambda", caller, 0, Inf, open = TRUE)
  } else if (
    !is.numeric(log10_grid) ||
      length(log10_grid) == 0L ||
      anyNA(log10_grid) ||
      any(!is.finite(log10_grid))
  ) {
    stop(caller, ": `log10_grid` must be finite numbers", call. = FALSE)
  }
  w[is.na(y)] <- 0
  y0 <- y
  y0[is.na(y0)] <- 0
  m <- sum(w > 0)
  if (n <= order || m <= order) {
    stop(
      caller,
      ": more than `order` = ",
      order,
      " values with positive weight are needed",
      call. = FALSE
    )
  }

  # 5.2 solve and GCV -----
  P <- Matrix::crossprod(.wh_diff_matrix(n, order))
  Wd <- Matrix::Diagonal(x = w)
  wy <- w * y0
  one <- function(lam) {
    A <- Wd + lam * P
    z <- as.numeric(Matrix::solve(A, wy))
    R <- tryCatch(chol(as.matrix(A)), error = function(e) NULL)
    edf <- if (is.null(R)) NA_real_ else sum(diag(chol2inv(R)) * w)
    rss <- sum(w * (y0 - z)^2)
    list(trend = z, edf = edf, gcv = m * rss / (m - edf)^2)
  }
  lams <- if (is.null(lambda)) 10^log10_grid else lambda
  res <- lapply(lams, one)
  edf <- vapply(res, function(r) r$edf, numeric(1))
  gcv <- vapply(res, function(r) r$gcv, numeric(1))
  ok <- is.finite(gcv)
  if (!is.null(lambda)) {
    pick <- 1L
  } else if (!any(ok)) {
    stop(caller, ": GCV is not finite for any lambda", call. = FALSE)
  } else {
    pick <- which(ok)[which.min(gcv[ok])]
  }
  list(
    trend = res[[pick]]$trend,
    lambda = lams[pick],
    edf = edf[pick],
    gcv = gcv[pick],
    path = data.table::data.table(lambda = lams, edf = edf, gcv = gcv)
  )
}

# 6. internal helpers: trend fits for robust_smooth -----

#' Names of the columns added by robust_smooth()
#' @keywords internal
#' @noRd
.rs_out_cols <- c(
  "observed",
  "seasonal",
  "trend",
  "trend_se",
  "trend_lo",
  "trend_hi",
  "remainder",
  "trend_pass1",
  "band_lo",
  "band_hi",
  "excluded",
  "flag_1",
  "flag_2",
  "flag",
  "flag_type",
  "pass",
  "method",
  "weight",
  "flag_weight"
)

#' Empty description of a trend fit (filled by each method)
#' @keywords internal
#' @noRd
.rs_fit_info <- function(...) {
  base <- list(
    trend_se = NULL,
    kalman = FALSE,
    h_hat = NA_real_,
    h_t = NULL,
    parameter = NA_character_,
    value = NA_real_,
    criterion = NA_character_,
    criterion_value = NA_real_,
    edf = NA_real_,
    rho = NA_real_,
    ic_ratio = NA_real_,
    var_level = NA_real_,
    var_slope = NA_real_,
    var_obs = NA_real_,
    iterations = NA_integer_,
    weights = NULL,
    irls_converged = NA,
    q_select = NA_character_,
    phi = NA_real_,
    loglik_profile = NA_real_,
    q_at_edge = NA,
    phi_at_edge = NA
  )
  utils::modifyList(base, list(...))
}

#' Transform to the modelling scale (adds log1p to .ti_transform)
#' @keywords internal
#' @noRd
.rs_transform <- function(y, transform, caller) {
  if (transform != "log1p") {
    return(.ti_transform(y, transform, caller))
  }
  ok <- y[!is.na(y)]
  if (any(!is.finite(ok))) {
    stop(caller, ": values must be finite or NA", call. = FALSE)
  }
  if (any(ok < 0)) {
    stop(
      caller,
      ": transform = 'log1p' requires non-negative values",
      call. = FALSE
    )
  }
  log1p(y)
}

#' Backfitting of a trend fit and Fourier seasonality (general trend fit)
#'
#' As .ti_backfit() but with any trend function `fit_fun(x)` returning a
#' list with `trend`, and a mask `ok` of the weeks used in the seasonal
#' regression.
#' @keywords internal
#' @noRd
.rs_backfit <- function(y, X, fit_fun, n_backfit, ok) {
  n <- length(y)
  if (is.null(X)) {
    return(list(seasonal = rep(0, n), fit = fit_fun(y)))
  }
  tt <- seq_len(n)
  tt <- (tt - mean(tt)) / stats::sd(tt)
  Z <- cbind(1, tt, tt^2, X)
  b <- stats::lm.fit(Z[ok, , drop = FALSE], y[ok])$coefficients
  b[is.na(b)] <- 0
  seasonal <- drop(X %*% b[-(1:3)])
  X1 <- cbind(1, X)
  for (i in seq_len(n_backfit)) {
    fit <- fit_fun(y - seasonal)
    r <- y - fit$trend
    b <- stats::lm.fit(X1[ok, , drop = FALSE], r[ok])$coefficients
    b[is.na(b)] <- 0
    seasonal <- drop(X %*% b[-1L])
  }
  list(seasonal = seasonal, fit = fit_fun(y - seasonal))
}

#' Henderson trend (GCV over lengths) on a series with NA interpolated
#' @keywords internal
#' @noRd
.rs_henderson <- function(x, lengths) {
  xi <- .ti_interp(x)
  h <- henderson_filter(xi, lengths = lengths[lengths <= length(xi)])
  .rs_fit_info(
    trend = h$trend,
    parameter = "length",
    value = h$length,
    criterion = "gcv",
    criterion_value = h$gcv,
    edf = h$edf,
    ic_ratio = h$ic_ratio
  )
}

#' Robust loess trend, span by GCV of the gaussian fit
#' @keywords internal
#' @noRd
.rs_loess <- function(x, spans) {
  n <- length(x)
  tt <- seq_len(n)
  ok <- !is.na(x)
  m <- sum(ok)
  df <- data.frame(tt = tt[ok], x = x[ok])
  ctrl <- stats::loess.control(surface = "direct")
  g <- vapply(
    spans,
    function(s) {
      f <- tryCatch(
        suppressWarnings(stats::loess(
          x ~ tt,
          data = df,
          span = s,
          degree = 1L,
          family = "gaussian",
          control = ctrl
        )),
        error = function(e) NULL
      )
      if (is.null(f)) {
        return(c(NA_real_, NA_real_))
      }
      tr <- f$trace.hat
      c(m * sum(stats::residuals(f)^2) / (m - tr)^2, tr)
    },
    numeric(2)
  )
  okg <- is.finite(g[1L, ])
  if (!any(okg)) {
    stop("loess: no span could be fitted", call. = FALSE)
  }
  pick <- which(okg)[which.min(g[1L, okg])]
  f <- suppressWarnings(stats::loess(
    x ~ tt,
    data = df,
    span = spans[pick],
    degree = 1L,
    family = "symmetric",
    control = ctrl
  ))
  .rs_fit_info(
    trend = as.numeric(stats::predict(f, newdata = data.frame(tt = tt))),
    parameter = "span",
    value = spans[pick],
    criterion = "gcv",
    criterion_value = g[1L, pick],
    edf = g[2L, pick]
  )
}

#' Whittaker trend (lambda by GCV)
#' @keywords internal
#' @noRd
.rs_whittaker <- function(x) {
  wh <- whittaker_smooth(x)
  .rs_fit_info(
    trend = wh$trend,
    parameter = "lambda",
    value = wh$lambda,
    criterion = "gcv",
    criterion_value = wh$gcv,
    edf = wh$edf
  )
}

#' Kalman local level by StructTS + KalmanSmooth
#'
#' Leading NA weeks are dropped for the estimation and recovered by the
#' smoother on the full series, as in .ti_trend_llt(); the series is
#' rescaled to unit standard deviation of its first differences for the
#' optimiser and the variances are back-transformed.
#' @keywords internal
#' @noRd
.rs_level <- function(x) {
  n <- length(x)
  first <- which(!is.na(x))[1L]
  xf <- x[first:n]
  sc <- sqrt(.ti_var(diff(xf)))
  if (!is.finite(sc) || sc <= 0) {
    sc <- 1
  }
  fit <- tryCatch(
    suppressWarnings(stats::StructTS(
      stats::ts((xf - xf[1L]) / sc),
      type = "level"
    )),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    stop(conditionMessage(fit), call. = FALSE)
  }
  coef <- unname(fit$coef) * sc^2
  mod <- fit$model0
  mod$a <- xf[1L]
  mod$P[] <- 1e4 * stats::var(xf, na.rm = TRUE)
  mod$Pn[] <- 0
  mod$V[] <- coef[1L]
  mod$h <- coef[2L]
  sm <- stats::KalmanSmooth(x, mod, nit = -1L)
  .rs_fit_info(
    trend = as.numeric(sm$smooth[, 1L]),
    trend_se = sqrt(pmax(as.numeric(sm$var[, 1L, 1L]), 0)),
    kalman = TRUE,
    h_hat = coef[2L],
    parameter = "q",
    value = if (coef[2L] > 0) coef[1L] / coef[2L] else NA_real_,
    criterion = "loglik",
    criterion_value = fit$loglik - sum(!is.na(xf)) * log(sc),
    var_level = coef[1L],
    var_obs = coef[2L]
  )
}

#' Kalman filter and fixed-interval smoother for a (damped) smooth trend
#' with level variance 0 and time-varying observation variance
#'
#' State (level, slope), T = ((1, 1), (0, phi)) by rows, Z = (1, 0),
#' Q = diag(0, var_slope), observation variance `h` (vector). With
#' `phi = 1` (default) this is the smooth trend model; `phi < 1` damps the
#' slope, slope_{t+1} = phi * slope_t + eta_t. NA weeks are skipped in the
#' update. The initial state is the one of .ti_trend_llt() (StructTS model0
#' with a = (first observed value, 0) and every entry of P equal to
#' 1e4 * var(x), at time 0, predicted to time 1 with T as KalmanSmooth()
#' does), so that with constant `h` and `phi = 1` the result equals the
#' KalmanSmooth() output of the `smooth` method; the same diffuse start is
#' used for every `phi`. The backward pass is the de Jong / Durbin-Koopman
#' form of the fixed-interval smoother, which gives the same smoothed
#' moments as the Rauch-Tung-Striebel recursion without inverting the
#' predicted covariance (singular under that initial state). With
#' `phi = 1` every multiplication by `phi` is exact, so the output is the
#' one of the undamped recursion.
#' Returns the smoothed level and its variance, and the one-step prediction
#' errors `v` and their variances `F` (NA on missing weeks).
#' @keywords internal
#' @noRd
.rs_kf_smooth <- function(x, h, var_slope, phi = 1) {
  n <- length(x)
  kap <- 1e4 * max(stats::var(x, na.rm = TRUE), .Machine$double.eps)
  # predicted states and covariances, innovations
  ap1 <- ap2 <- pp11 <- pp12 <- pp22 <- numeric(n)
  af1 <- pf11 <- pf12 <- numeric(n)
  vv <- ff <- rep(NA_real_, n)
  a1 <- x[which(!is.na(x))[1L]]
  a2 <- 0
  # StructTS convention: (a, P) refer to time 0 and are predicted to t = 1
  p11 <- 4 * kap
  p12 <- 2 * phi * kap
  p22 <- phi^2 * kap + var_slope
  for (t in seq_len(n)) {
    ap1[t] <- a1
    ap2[t] <- a2
    pp11[t] <- p11
    pp12[t] <- p12
    pp22[t] <- p22
    if (!is.na(x[t])) {
      v <- x[t] - a1
      f <- p11 + h[t]
      vv[t] <- v
      ff[t] <- f
      a1 <- a1 + p11 / f * v
      a2 <- a2 + p12 / f * v
      n22 <- p22 - p12 * p12 / f
      n12 <- p12 * h[t] / f
      p11 <- p11 * h[t] / f
      p12 <- n12
      p22 <- n22
    }
    af1[t] <- a1
    pf11[t] <- p11
    pf12[t] <- p12
    # predict t + 1: a <- T a, P <- T P T' + Q
    a1 <- a1 + a2
    a2 <- phi * a2
    q11 <- p11 + 2 * p12 + p22
    q12 <- phi * (p12 + p22)
    p11 <- q11
    p12 <- q12
    p22 <- phi^2 * p22 + var_slope
  }
  # backward smoother: r, N
  r1 <- r2 <- 0
  n11 <- n12 <- n22 <- 0
  lev <- vlev <- numeric(n)
  for (t in rev(seq_len(n))) {
    # smoothed level from the filtered moments and r_t, N_t:
    # a_t|n = a_t|t + (P_t|t T')' r_t, V_t|n = P_t|t - c' N_t c with c the
    # first row of P_t|t T' (no cancellation with a diffuse start)
    c1 <- pf11[t] + pf12[t]
    c2 <- phi * pf12[t]
    lev[t] <- af1[t] + c1 * r1 + c2 * r2
    vlev[t] <- pf11[t] - (c1^2 * n11 + 2 * c1 * c2 * n12 + c2^2 * n22)
    if (!is.na(vv[t])) {
      f <- ff[t]
      k1 <- (pp11[t] + pp12[t]) / f
      k2 <- phi * pp12[t] / f
      # L = T - K Z = [[1 - k1, 1], [-k2, phi]]
      l11 <- 1 - k1
      l21 <- -k2
      nr1 <- vv[t] / f + l11 * r1 + l21 * r2
      nr2 <- r1 + phi * r2
      # L' N L
      a11 <- l11 * n11 + l21 * n12
      a12 <- l11 * n12 + l21 * n22
      a21 <- n11 + phi * n12
      a22 <- n12 + phi * n22
      m11 <- a11 * l11 + a12 * l21 + 1 / f
      m12 <- a11 + a12 * phi
      m22 <- a21 + a22 * phi
    } else {
      # L = T
      nr1 <- r1
      nr2 <- r1 + phi * r2
      m11 <- n11
      m12 <- n11 + phi * n12
      m22 <- n11 + 2 * phi * n12 + phi^2 * n22
    }
    r1 <- nr1
    r2 <- nr2
    n11 <- m11
    n12 <- m12
    n22 <- m22
  }
  list(level = lev, var_level = pmax(vlev, 0), v = vv, F = ff)
}

#' Diffuse Gaussian log-likelihood by prediction-error decomposition
#'
#' From the prediction errors `v` and variances `F` of .rs_kf_smooth(); the
#' first `n_diffuse` observed errors (the diffuse elements of the initial
#' state) are left out, so that values are comparable across models with
#' the same number of diffuse states.
#' @keywords internal
#' @noRd
.rs_kf_loglik <- function(kf, n_diffuse = 2L) {
  i <- which(!is.na(kf$v))
  i <- i[-seq_len(min(n_diffuse, length(i)))]
  -0.5 * sum(log(2 * pi * kf$F[i]) + kf$v[i]^2 / kf$F[i])
}

#' IRLS of the robust (Student-t) smooth trend for a fixed seasonal
#' @keywords internal
#' @noRd
.rs_irls <- function(x, w, h, q, nu, max_irls = 20L, tol = 1e-4, phi = 1) {
  ok <- !is.na(x)
  n_ok <- sum(ok)
  it <- 0L
  repeat {
    it <- it + 1L
    ks <- .rs_kf_smooth(x, h / w, q * h, phi)
    e <- x - ks$level
    m2 <- e^2 + ks$var_level
    w_new <- rep(1, length(x))
    w_new[ok] <- (nu + 1) / (nu + m2[ok] / h)
    h <- sum(w_new[ok] * m2[ok]) / n_ok
    done <- max(abs(w_new - w)) < tol
    w <- w_new
    if (done || it >= max_irls) {
      break
    }
  }
  list(w = w, h = h, iterations = it, converged = done)
}

#' Backfitting of the robust smooth trend and the Fourier seasonal
#'
#' Starting from the seasonal `s` and observation variance `h`, with `q`
#' and `phi` fixed: `n_backfit` times (only when `X` is not NULL) the IRLS
#' on y - s, then the seasonal by weighted least squares on y - trend;
#' finally the IRLS and the smoother on y - s.
#' @keywords internal
#' @noRd
.rs_t_backfit <- function(y, X, s, h, q, phi, nu, n_backfit) {
  n <- length(y)
  ok <- !is.na(y)
  w <- rep(1, n)
  for (i in seq_len(if (is.null(X)) 0L else n_backfit)) {
    ir <- .rs_irls(y - s, w, h, q, nu, phi = phi)
    w <- ir$w
    h <- ir$h
    tr <- .rs_kf_smooth(y - s, h / w, q * h, phi)$level
    b <- stats::lm.wfit(
      cbind(1, X)[ok, , drop = FALSE],
      (y - tr)[ok],
      w[ok]
    )$coefficients
    b[is.na(b)] <- 0
    s <- drop(X %*% b[-1L])
  }
  ir <- .rs_irls(y - s, w, h, q, nu, phi = phi)
  list(
    seasonal = s,
    ir = ir,
    kf = .rs_kf_smooth(y - s, ir$h / ir$w, q * ir$h, phi)
  )
}

#' Profile objective of the robust smooth trend over a (q, phi) grid
#'
#' For each pair, the IRLS (EM) on `x` (seasonally adjusted, seasonal
#' fixed) from w = 1 and the Gaussian `h0`, then
#' L(q, phi) = diffuse Gaussian log-likelihood of `x` under h_t = h / w_t
#' and slope variance q * h (first 2 prediction errors left out) +
#' sum over observed weeks of log dgamma(w_t, nu / 2, nu / 2).
#' Returns the grid with the objective and the argmax.
#' @keywords internal
#' @noRd
.rs_t_profile <- function(x, h0, nu, qs, phis) {
  ok <- !is.na(x)
  n <- length(x)
  grid <- expand.grid(q = qs, phi = phis, KEEP.OUT.ATTRS = FALSE)
  obj <- vapply(
    seq_len(nrow(grid)),
    function(i) {
      q <- grid$q[i]
      phi <- grid$phi[i]
      ir <- .rs_irls(x, rep(1, n), h0, q, nu, phi = phi)
      kf <- .rs_kf_smooth(x, ir$h / ir$w, q * ir$h, phi)
      .rs_kf_loglik(kf, 2L) +
        sum(stats::dgamma(ir$w[ok], nu / 2, rate = nu / 2, log = TRUE))
    },
    numeric(1)
  )
  fin <- is.finite(obj)
  if (!any(fin)) {
    stop("smooth_t: the profile objective is not finite on the grid",
      call. = FALSE)
  }
  pick <- which(fin)[which.max(obj[fin])]
  list(
    q = grid$q[pick],
    phi = grid$phi[pick],
    objective = obj[pick],
    path = data.table::data.table(q = grid$q, phi = grid$phi, objective = obj)
  )
}

#' Robust smooth trend (Student-t observation errors) by IRLS
#'
#' Starts from the ML fit of the smooth trend model `ks` (output of
#' .ti_extract_one(method = "smooth")): its seasonal component, observation
#' variance h and signal-to-noise ratio q = var_slope / var_obs. Each IRLS
#' step runs the Kalman filter and RTS smoother with h_t = h / w_t, then
#' sets w_t = (nu + 1) / (nu + s_t^2) with s_t^2 = (e_t^2 + P_t|n) / h
#' (EM weights of the Student-t model: e_t auxiliary residual, P_t|n
#' smoothed level variance) and re-estimates h as the weighted mean of
#' e_t^2 + P_t|n. Seasonality by backfitting with weighted least squares.
#'
#' With o$q_select = "gaussian" q is the one of the ML fit and phi = 1.
#' With "profile_t", (q, phi) maximise the profile objective of
#' .rs_t_profile() on the grid (phi = 1 unless o$damping = "profile"). To
#' keep the cost at one IRLS per grid point, the seasonal is estimated once
#' by the backfitting at the Gaussian q (phi = 1) and kept fixed during the
#' grid search; the full backfitting is then run at the selected (q, phi),
#' from the seasonal of the ML fit as in the "gaussian" case.
#' @keywords internal
#' @noRd
.rs_smooth_t <- function(y, X, ks, o) {
  nu <- o$nu
  h <- ks$var_obs
  if (!is.finite(h) || h <= 0) {
    h <- max(.ti_var(diff(y)) / 2, .Machine$double.eps, na.rm = TRUE)
  }
  q <- max(ks$var_slope, 0) / h
  phi <- 1
  prof <- NULL
  if (o$q_select == "profile_t") {
    s_a <- .rs_t_backfit(y, X, ks$seasonal, h, q, 1, nu, o$n_backfit)$seasonal
    phis <- if (o$damping == "profile") o$phi_grid else 1
    qs <- 10^o$log10_q_grid
    prof <- .rs_t_profile(y - s_a, h, nu, qs, phis)
    q <- prof$q
    phi <- prof$phi
  }
  bf <- .rs_t_backfit(y, X, ks$seasonal, h, q, phi, nu, o$n_backfit)
  ir <- bf$ir
  w <- ir$w
  h <- ir$h
  ks2 <- bf$kf
  list(
    seasonal = bf$seasonal,
    fit = .rs_fit_info(
      trend = ks2$level,
      trend_se = sqrt(ks2$var_level),
      kalman = TRUE,
      h_hat = h,
      h_t = h / w,
      weights = w,
      parameter = "q",
      value = q,
      criterion = "loglik",
      criterion_value = NA_real_,
      var_level = 0,
      var_slope = q * h,
      var_obs = h,
      iterations = ir$iterations,
      irls_converged = ir$converged,
      q_select = o$q_select,
      phi = phi,
      loglik_profile = if (is.null(prof)) NA_real_ else prof$objective,
      q_at_edge = if (is.null(prof)) {
        NA
      } else {
        q == min(qs) || q == max(qs)
      },
      phi_at_edge = if (o$damping == "profile") {
        phi == min(o$phi_grid) && phi < 1
      } else {
        NA
      }
    )
  )
}

#' GAM trend with AR(1) errors and Fourier terms in the formula
#'
#' `mgcv::bam(y ~ s(t, bs = "cr", k) + Fourier, rho, AR.start,
#' method = "fREML")` for rho on a grid, kept at the minimum fREML score.
#' Missing weeks are dropped and AR.start marks the first week after each
#' gap. Returns seasonal (Fourier part) and trend (intercept + s(t)) on all
#' weeks, with the standard error of the trend from the Bayesian covariance.
#' @keywords internal
#' @noRd
.rs_gam <- function(y, X, rhos, k_max = 40L) {
  n <- length(y)
  ok <- !is.na(y)
  K2 <- if (is.null(X)) 0L else ncol(X)
  fn <- if (K2 > 0L) paste0("f", seq_len(K2)) else character(0)
  nd <- data.frame(y = y, t = seq_len(n))
  if (K2 > 0L) {
    nd <- cbind(nd, stats::setNames(as.data.frame(X), fn))
  }
  df <- nd[ok, , drop = FALSE]
  ars <- c(TRUE, diff(which(ok)) > 1L)
  kk <- as.integer(min(k_max, sum(ok) - K2 - 2L))
  if (kk < 4L) {
    stop("gam_ar1: too few observations for the spline basis", call. = FALSE)
  }
  fml <- stats::as.formula(paste0(
    "y ~ s(t, bs = 'cr', k = ",
    kk,
    ")",
    if (K2 > 0L) paste0(" + ", paste(fn, collapse = " + ")) else ""
  ))
  fits <- lapply(rhos, function(r) {
    tryCatch(
      suppressWarnings(mgcv::bam(
        fml,
        data = df,
        rho = r,
        AR.start = ars,
        method = "fREML"
      )),
      error = function(e) NULL
    )
  })
  score <- vapply(
    fits,
    function(f) if (is.null(f)) NA_real_ else as.numeric(f$gcv.ubre),
    numeric(1)
  )
  if (!any(is.finite(score))) {
    stop("gam_ar1: no fit converged", call. = FALSE)
  }
  pick <- which(is.finite(score))[which.min(score[is.finite(score)])]
  f <- fits[[pick]]
  cf <- stats::coef(f)
  Xp <- stats::predict(f, newdata = nd, type = "lpmatrix")
  ct <- c(1L, grep("^s\\(t\\)", colnames(Xp)))
  trend <- drop(Xp[, ct, drop = FALSE] %*% cf[ct])
  Vt <- f$Vp[ct, ct, drop = FALSE]
  se <- sqrt(pmax(
    rowSums((Xp[, ct, drop = FALSE] %*% Vt) * Xp[, ct, drop = FALSE]),
    0
  ))
  seasonal <- if (K2 > 0L) drop(X %*% cf[fn]) else rep(0, n)
  list(
    seasonal = seasonal,
    fit = .rs_fit_info(
      trend = trend,
      trend_se = se,
      parameter = "rho",
      value = rhos[pick],
      criterion = "fREML",
      criterion_value = score[pick],
      edf = sum(f$edf[grep("^s\\(t\\)", names(f$edf))]),
      rho = rhos[pick],
      var_obs = f$sig2
    )
  )
}

# 7. internal helpers: one pass, bands and flags -----

#' One smoothing pass on y with the weeks `excl` set to missing
#'
#' K is chosen by the AICc of the smooth Kalman model on y with `excl` as
#' NA, then the method is fitted with that K. Henderson replaces the
#' excluded weeks with the fitted values (trend + seasonal) of the previous
#' pass `prev` instead of NA.
#' @keywords internal
#' @noRd
.rs_fit_pass <- function(y, excl, tw, method, o, prev) {
  yk <- y
  yk[excl] <- NA
  used <- !is.na(yk)
  ks <- .ti_extract_one(
    yk,
    tw,
    "smooth",
    o$period,
    o$harmonics,
    o$max_harmonics,
    o$n_backfit
  )
  K <- ks$K
  X <- if (K > 0L) .ti_fourier(tw, K, o$period) else NULL
  res <- switch(
    method,
    smooth = list(
      seasonal = ks$seasonal,
      fit = .rs_fit_info(
        trend = ks$trend,
        trend_se = ks$trend_se,
        kalman = TRUE,
        h_hat = ks$var_obs,
        parameter = "q",
        value = if (ks$var_obs > 0) ks$var_slope / ks$var_obs else NA_real_,
        criterion = "aicc",
        criterion_value = ks$aicc,
        var_level = ks$var_level,
        var_slope = ks$var_slope,
        var_obs = ks$var_obs
      )
    ),
    llt = {
      bf <- .ti_backfit(yk, X, "llt", o$n_backfit)
      f <- bf$fit
      list(
        seasonal = bf$seasonal,
        fit = .rs_fit_info(
          trend = f$trend,
          trend_se = f$trend_se,
          kalman = TRUE,
          h_hat = f$var_obs,
          parameter = "q_slope",
          value = if (f$var_obs > 0) f$var_slope / f$var_obs else NA_real_,
          criterion = "loglik",
          criterion_value = f$loglik,
          var_level = f$var_level,
          var_slope = f$var_slope,
          var_obs = f$var_obs
        )
      )
    },
    level = .rs_backfit(yk, X, .rs_level, o$n_backfit, used),
    henderson = {
      yh <- yk
      if (any(excl)) {
        yh[excl] <- prev$trend[excl] + prev$seasonal[excl]
      }
      .rs_backfit(
        yh,
        X,
        function(x) .rs_henderson(x, o$lengths),
        o$n_backfit,
        used
      )
    },
    loess = .rs_backfit(
      yk,
      X,
      function(x) .rs_loess(x, o$spans),
      o$n_backfit,
      used
    ),
    whittaker = .rs_backfit(yk, X, .rs_whittaker, o$n_backfit, used),
    gam_ar1 = .rs_gam(yk, X, o$rhos),
    smooth_t = .rs_smooth_t(yk, X, ks, o)
  )
  f <- res$fit
  n <- length(y)
  seasonal <- res$seasonal
  trend <- f$trend
  fitted <- seasonal + trend
  trend_se <- if (is.null(f$trend_se)) rep(NA_real_, n) else f$trend_se

  # band half-width
  model_band <- f$kalman && o$band == "model"
  band_defined <- TRUE
  if (model_band) {
    # with h -> 0 the model interpolates the data: k_t = P_t|n / h_t is 0/0
    # where P_t|n = 0 and the auxiliary residual has no variance, so the
    # band is undefined (half-width NA) rather than NaN or zero
    h_tol <- 1e-8 * max(.ti_var(yk), .Machine$double.eps, na.rm = TRUE)
    band_defined <- is.finite(f$h_hat) && f$h_hat > h_tol
    if (band_defined) {
      P <- pmax(trend_se^2, 0)
      ht <- if (is.null(f$h_t)) rep(f$h_hat, n) else f$h_t
      kk <- ifelse(used & ht > 0, pmin(pmax(P / ht, 0), 1), 0)
      ve <- (1 - kk)^2 * f$h_hat + P * (1 - kk)
      # negative values can only come from rounding
      half <- o$z * sqrt(pmax(ve, 0))
    } else {
      half <- rep(NA_real_, n)
    }
  } else {
    r <- (y - fitted)[used]
    half <- rep(o$k * stats::mad(r, constant = 1.4826), n)
  }
  list(
    seasonal = seasonal,
    trend = trend,
    trend_se = trend_se,
    half = half,
    used = used,
    K = K,
    aicc = ks$aicc,
    band = if (model_band) "model" else "mad",
    band_defined = band_defined,
    weight = if (is.null(f$weights)) {
      rep(NA_real_, n)
    } else {
      ifelse(used, f$weights, NA_real_)
    },
    f = f
  )
}

#' Flags against a pass: observed weeks outside the band
#'
#' All NA when the band of the pass is undefined.
#' @keywords internal
#' @noRd
.rs_flags <- function(y, fp) {
  if (!fp$band_defined) {
    return(rep(NA, length(y)))
  }
  d <- abs(y - fp$seasonal - fp$trend)
  !is.na(y) & !is.na(d) & d > fp$half
}

#' Runs of consecutive flags and their type
#'
#' Returns a data.table with one row per run (`start`, `end` positions,
#' `n_weeks`, `flag_type`, `shift`, `half_width`) and the per-week type.
#' @keywords internal
#' @noRd
.rs_flag_runs <- function(fl, sa, half, run_min, n_end = 4L, n_side = 8L) {
  n <- length(fl)
  type <- rep(NA_character_, n)
  r <- rle(fl)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  sel <- which(r$values)
  runs <- data.table::data.table(
    start = starts[sel],
    end = ends[sel],
    n_weeks = r$lengths[sel],
    flag_type = NA_character_,
    shift = NA_real_,
    half_width = NA_real_
  )
  if (nrow(runs) == 0L) {
    return(list(runs = runs, type = type))
  }
  clean <- which(!fl & !is.na(sa))
  for (i in seq_len(nrow(runs))) {
    s <- runs$start[i]
    e <- runs$end[i]
    hw <- stats::median(half[s:e])
    bef <- utils::tail(clean[clean < s], n_side)
    aft <- utils::head(clean[clean > e], n_side)
    sh <- if (length(bef) && length(aft)) {
      stats::median(sa[aft]) - stats::median(sa[bef])
    } else {
      NA_real_
    }
    tp <- if (e > n - n_end) {
      "final"
    } else if (runs$n_weeks[i] < run_min) {
      "isolated"
    } else if (!is.na(sh) && abs(sh) >= hw) {
      "level_shift"
    } else {
      "interruption"
    }
    data.table::set(runs, i, "flag_type", tp)
    data.table::set(runs, i, "shift", sh)
    data.table::set(runs, i, "half_width", hw)
    type[s:e] <- tp
  }
  list(runs = runs, type = type)
}

#' Robust smoothing of one series: passes, flags and types
#' @keywords internal
#' @noRd
.rs_one <- function(y, tw, method, o) {
  n <- length(y)
  if (sum(!is.na(y)) < 8L) {
    stop("fewer than 8 non-missing observations", call. = FALSE)
  }
  excl <- rep(FALSE, n)
  fp <- .rs_fit_pass(y, excl, tw, method, o, NULL)
  fl <- .rs_flags(y, fp)
  na_of <- function(fl, fp) {
    if (!fp$band_defined) {
      # no band, no flags: keep the current exclusions and stop
      return(excl)
    }
    if (o$flag_na == "all") {
      return(fl)
    }
    tp <- .rs_flag_runs(fl, y - fp$seasonal, fp$half, o$run_min)$type
    fl & !is.na(tp) & tp == "isolated"
  }
  rec <- list(.rs_pass_row(1L, fp, excl, fl))
  trend1 <- fp$trend
  flag_1 <- fl
  flag_2 <- rep(NA, n)
  p <- 1L
  converged <- o$max_iter == 0L
  while (p <= o$max_iter) {
    new_excl <- na_of(fl, fp)
    if (identical(new_excl, excl)) {
      # the next pass would repeat this one
      converged <- TRUE
      if (p == 1L) {
        flag_2 <- fl
      }
      break
    }
    excl <- new_excl
    p <- p + 1L
    fp <- .rs_fit_pass(y, excl, tw, method, o, fp)
    fl <- .rs_flags(y, fp)
    if (p == 2L) {
      flag_2 <- fl
    }
    rec[[p]] <- .rs_pass_row(p, fp, excl, fl)
  }
  if (!converged && p > o$max_iter) {
    converged <- identical(na_of(fl, fp), excl)
  }
  if (!fp$band_defined) {
    converged <- NA
  }
  fr <- .rs_flag_runs(
    if (fp$band_defined) fl else rep(FALSE, n),
    y - fp$seasonal,
    fp$half,
    o$run_min
  )
  fits <- data.table::rbindlist(rec)
  data.table::set(fits, j = "final", value = fits$pass == p)
  data.table::set(fits, j = "converged", value = converged)
  list(
    seasonal = fp$seasonal,
    trend = fp$trend,
    trend_se = fp$trend_se,
    half = fp$half,
    excluded = excl,
    trend_pass1 = trend1,
    flag_1 = flag_1,
    flag_2 = flag_2,
    flag = fl,
    flag_type = fr$type,
    pass = p,
    weight = fp$weight,
    fits = fits,
    runs = fr$runs
  )
}

#' One row of the fits table for a pass
#' @keywords internal
#' @noRd
.rs_pass_row <- function(p, fp, excl, fl) {
  f <- fp$f
  data.table::data.table(
    pass = as.integer(p),
    K = as.integer(fp$K),
    aicc_K = if (is.finite(fp$aicc)) fp$aicc else NA_real_,
    parameter = f$parameter,
    value = as.numeric(f$value),
    criterion = f$criterion,
    criterion_value = as.numeric(f$criterion_value),
    edf = as.numeric(f$edf),
    rho = as.numeric(f$rho),
    ic_ratio = as.numeric(f$ic_ratio),
    var_level = as.numeric(f$var_level),
    var_slope = as.numeric(f$var_slope),
    var_obs = as.numeric(f$var_obs),
    band = fp$band,
    band_defined = fp$band_defined,
    n_excluded = sum(excl),
    n_flag = sum(fl),
    iterations = as.integer(f$iterations),
    irls_converged = as.logical(f$irls_converged),
    q_select = as.character(f$q_select),
    phi = as.numeric(f$phi),
    loglik_profile = as.numeric(f$loglik_profile),
    q_at_edge = as.logical(f$q_at_edge),
    phi_at_edge = as.logical(f$phi_at_edge)
  )
}

# 8. robust_smooth -----

#' Robust trend smoothing with post-smoothing bands and re-smoothing
#'
#' Extracts a trend from each weekly series (one per key) with one of eight
#' smoothers, places a band around the fitted values (trend + seasonal),
#' flags the observed weeks outside the band, sets them to missing and
#' smooths again. No cleaning is applied before the first smoothing.
#'
#' @details
#' **Scale.** The series is modelled on the scale of `transform`. `log1p`
#' keeps zero weeks; in the output attributes it is recorded as
#' `transform = "log"` (and `observed_transform = "log1p"`), so that
#' [trend_index()] with `transform = "log"` and [trend_diagnostics()] treat
#' the log1p trend as a log trend: the index is `exp` of differences of
#' `log(1 + n)`, close to differences of `log(n)` when counts are large.
#'
#' **Seasonality.** The seasonal component is a Fourier regression with
#' period `period` and `t` the number of weeks since 1970-01-01 (as in
#' [extract_trend()]). The number of harmonics `K` is chosen once per key
#' and pass by the AICc of the `smooth` Kalman model ([extract_trend()] with
#' `method = "smooth"`, on the series with the excluded weeks set to `NA`),
#' or fixed by `harmonics`, and is shared by every method, so that methods
#' differ only in the trend filter. The seasonal coefficients are estimated
#' by backfitting (`n_backfit` iterations, as in [extract_trend()]) with the
#' trend filter of the method, except `gam_ar1`, which has the harmonics in
#' its formula.
#'
#' **Methods** (each smoothing parameter is chosen from the data):
#' * `henderson`: [henderson_filter()] with the length chosen by GCV among
#'   13, 23 and 39 (lengths longer than the series are skipped) and the
#'   Musgrave end weights with the I/C ratio of a preliminary 13-term pass.
#'   Missing weeks of the input are linearly interpolated; excluded weeks
#'   are replaced by the fitted values (trend + seasonal) of the previous
#'   pass (X-11 style).
#' * `loess`: `stats::loess(degree = 1, family = "symmetric")` on time,
#'   with the span chosen on the grid `0.06, 0.08, ..., 0.50` by the GCV of
#'   the gaussian fit, `n RSS / (n - trace.hat)^2`; `surface = "direct"` so
#'   that the trend extends to missing weeks at the ends.
#' * `gam_ar1`: `mgcv::bam(y ~ s(t, bs = "cr", k = 40) + Fourier,
#'   rho, AR.start, method = "fREML")` (`k` reduced on short series), with
#'   `rho` on the grid `0, 0.05, ..., 0.9` chosen at the minimum fREML
#'   score; `AR.start` marks the first week after each missing stretch. The
#'   trend is the intercept plus the spline, `trend_se` its standard error
#'   from the Bayesian covariance matrix. Requires mgcv.
#' * `whittaker`: [whittaker_smooth()] of order 2 with \eqn{\lambda} by
#'   GCV; missing weeks get weight zero.
#' * `level`: Kalman local level, `stats::StructTS(type = "level")` and
#'   `stats::KalmanSmooth()`, by maximum likelihood.
#' * `llt`, `smooth`: the local linear trend and the smooth trend model
#'   (level variance 0) of [extract_trend()], by maximum likelihood.
#' * `smooth_t`: robust Kalman smoother. It starts from the `smooth` ML fit
#'   (seasonal component, observation variance \eqn{\hat h}, signal-to-noise
#'   ratio `q = var_slope / var_obs`), then iterates (at most 20 times) a
#'   Kalman filter and Rauch-Tung-Striebel smoother of the same model with
#'   time-varying observation variance \eqn{h_t = \hat h / w_t}, with
#'   Student-t weights \eqn{w_t = (\nu + 1) / (\nu + s_t^2)},
#'   \eqn{s_t^2 = (e_t^2 + P_{t|n}) / \hat h} (EM weights; \eqn{e_t} the
#'   auxiliary residual, \eqn{P_{t|n}} the smoothed level variance), and
#'   \eqn{\hat h} re-estimated as the weighted mean of
#'   \eqn{e_t^2 + P_{t|n}}. With `q_select = "gaussian"` (default) `q` is
#'   kept fixed, so the smoothness of the ML fit is kept and only the
#'   weights change. The seasonal coefficients are re-estimated by weighted
#'   least squares in the backfitting. The final weights are returned in
#'   `weight`.
#'
#' **`smooth_t`: q under the t model and damped slope.** With
#' `q_select = "profile_t"`, `q` is chosen on the grid
#' \eqn{10^g}, `g` in `log10_q_grid`, by maximising
#' \deqn{L(q, \phi) = \ell_G(q, \phi; h_t = \hat h / w_t) +
#'   \sum_t \log \mathrm{Gamma}(w_t; \nu/2, \nu/2),}
#' where, for each grid point, the IRLS is run to convergence from
#' \eqn{w_t = 1} and the \eqn{\hat h} of the Gaussian fit,
#' \eqn{\ell_G} is the Gaussian log-likelihood of the seasonally adjusted
#' series by prediction-error decomposition under observation variance
#' \eqn{h_t} and slope variance \eqn{q \hat h}, with the first two
#' prediction errors (the diffuse elements of the initial state) left out
#' for every grid point, and the sum runs over the observed weeks
#' (Gamma density with shape and rate \eqn{\nu/2}). To keep one IRLS per
#' grid point, the seasonal component is estimated once by the backfitting
#' at the Gaussian `q` and held fixed during the grid search; the full
#' backfitting is then run at the selected point. With
#' `damping = "profile"` (requires `q_select = "profile_t"`) the slope is
#' damped, \eqn{\beta_{t+1} = \phi \beta_t + \eta_t},
#' \eqn{\eta_t \sim N(0, q \hat h)}, level variance 0, with both states
#' diffuse at the start for every \eqn{\phi}, and \eqn{(q, \phi)} is
#' profiled jointly on `log10_q_grid` x `phi_grid`. `fits` reports
#' `q_select`, the selected `q` (`value`), `phi`, `loglik_profile` (the
#' objective at the selected point, with the seasonal of the grid search),
#' `q_at_edge` (selected `q` at the minimum or maximum of the grid),
#' `phi_at_edge` (selected \eqn{\phi < 1} at the minimum of `phi_grid`;
#' \eqn{\phi = 1} is not an edge), and the IRLS `iterations` and
#' `irls_converged` of the final fit. The grid search multiplies the cost
#' of a `smooth_t` pass by about the number of grid points.
#'
#' **Bands.** With `band = "model"`, the Kalman methods (`level`, `llt`,
#' `smooth`, `smooth_t`) use the auxiliary-residual band
#' `trend + seasonal +/- z * sqrt(v_t)`. For an observed week used in the
#' fit, \eqn{v_t = \hat h - P_{t|n}} (variance of the auxiliary residual,
#' \eqn{P_{t|n}} the smoothed trend variance). In general
#' \eqn{v_t = (1 - k_t)^2 \hat h + (1 - k_t) P_{t|n}} with
#' \eqn{k_t = P_{t|n} / h_t} the weight of the week in its own smoothed
#' trend: it equals \eqn{\hat h - P_{t|n}} when \eqn{h_t = \hat h}, and
#' \eqn{\hat h + P_{t|n}} (prediction variance) for weeks excluded from the
#' fit (\eqn{k_t = 0}). For `smooth_t`, \eqn{h_t = \hat h / w_t}: a
#' down-weighted week is judged against the scale \eqn{\hat h}, not against
#' its inflated \eqn{h_t}. The seasonal coefficients are treated as known.
#' Negative values of \eqn{v_t} from rounding are set to 0. When
#' \eqn{\hat h} is not finite or not above `1e-8` times the variance of
#' the series of the pass (the ML fit interpolates the data, as happens for
#' `level` and `llt` when the observation variance is estimated at 0), the
#' band is undefined: `band_lo`, `band_hi` and the flags of the key are
#' `NA`, no further pass is run, `band_defined` is `FALSE` in `fits`,
#' `converged` is `NA`, and a single warning lists the keys concerned.
#' Non-Kalman methods with `band = "model"`, and every method with
#' `band = "mad"`, use `trend + seasonal +/- k * 1.4826 * MAD(remainder)`,
#' the MAD computed on the weeks used in the fit of that pass.
#'
#' **Weights.** For `smooth_t`, `weight` holds the final Student-t weight
#' \eqn{w_t} of the weeks used in the final fit and `flag_weight` is
#' \eqn{w_t < (\nu + 1) / (\nu + z^2)}, i.e. \eqn{s_t > z}: the week is
#' down-weighted as much as a standardised residual beyond the band
#' multiplier `z`. Both are `NA` for other methods and for weeks not used
#' in the final fit.
#'
#' **Passes.** Pass 1 smooths the series as observed and flags the observed
#' weeks outside the band (`flag_1`). Pass 2 sets the flagged weeks to `NA`
#' (with `flag_na = "isolated"`, only the weeks of `isolated` runs) and
#' smooths again, re-choosing `K` and the smoothing parameters; the flags
#' are recomputed on all observed weeks against the pass-2 fit (`flag_2`),
#' so a week can be unflagged. If the set of weeks to exclude changes, a
#' further pass is run with the new set, up to `max_iter` re-smoothings
#' (pass `max_iter + 1`). When the set of flags of pass 1 is empty, pass 1
#' is final. `flag` holds the flags against the final fit (consistent with
#' `band_lo`, `band_hi`) and `excluded` the weeks set to missing in the
#' final fit; they coincide when the procedure converged (column
#' `converged` of `fits`).
#'
#' **Flag types** (per run of consecutive flagged weeks of `flag`):
#' `final` when the run touches the last 4 weeks of the series; otherwise
#' `isolated` when it is shorter than `run_min` weeks; otherwise
#' `level_shift` when the median of the 8 unflagged observed weeks after
#' the run minus the median of the 8 before it (on the seasonally adjusted
#' scale, `observed - seasonal`) is at least the median band half-width
#' over the run in absolute value, and `interruption` when it is smaller or
#' cannot be computed (run at the start of the series).
#'
#' **Cost.** Each pass chooses `K` with `(max_harmonics + 1) *
#' (n_backfit + 1)` fits of the smooth Kalman model, then fits the method
#' (`n_backfit + 1` trend fits; `gam_ar1`: one `bam` per value of `rho`).
#'
#' @param dt A data.frame or data.table with one row per key x week.
#' @param key_cols Character vector of key columns identifying each series.
#' @param time_col Name of the time column (class Date or IDate, weekly).
#' @param value_col Name of the value column (counts for `log1p`, positive
#'   for `log`); `NA` allowed.
#' @param method Smoother: `"henderson"`, `"loess"`, `"gam_ar1"`,
#'   `"whittaker"`, `"level"`, `"llt"`, `"smooth"` or `"smooth_t"`.
#' @param transform Scale of the model: `"log1p"` (default), `"log"` or
#'   `"none"`.
#' @param period Seasonal period in weeks (default 365.25 / 7).
#' @param harmonics Fixed number of Fourier harmonics, or `NULL` to choose
#'   it by AICc of the smooth model.
#' @param max_harmonics Largest number of harmonics considered when
#'   `harmonics = NULL`.
#' @param n_backfit Number of backfitting iterations.
#' @param band `"model"` (auxiliary-residual band for Kalman methods, MAD
#'   band otherwise) or `"mad"` (MAD band for every method).
#' @param z Half-width multiplier of the model band (in standard
#'   deviations).
#' @param k Half-width multiplier of the MAD band (in robust standard
#'   deviations, `1.4826 * MAD`).
#' @param max_iter Maximum number of re-smoothings after pass 1
#'   (non-negative integer; 0 = pass 1 only).
#' @param run_min Minimum length in weeks of a run of flags that is not
#'   `isolated`.
#' @param nu Degrees of freedom of the Student-t weights (`smooth_t`).
#' @param level Confidence level of `trend_lo`, `trend_hi`.
#' @param flag_na `"all"` (every flagged week is set to missing in the next
#'   pass) or `"isolated"` (only the weeks of `isolated` runs).
#' @param q_select Choice of `q` for `smooth_t`: `"gaussian"` (default; the
#'   ML value of the Gaussian smooth trend model) or `"profile_t"` (maximum
#'   of the profile objective under the Student-t model, see Details).
#'   Other methods accept only the default.
#' @param log10_q_grid Grid of \eqn{\log_{10} q} values searched when
#'   `q_select = "profile_t"`.
#' @param damping `"none"` (default, \eqn{\phi = 1}) or `"profile"`
#'   (damped slope, \eqn{\phi} profiled jointly with `q` on `phi_grid`;
#'   requires `q_select = "profile_t"`, method `smooth_t` only).
#' @param phi_grid Grid of damping factors in (0, 1] searched when
#'   `damping = "profile"`.
#'
#' @return A data.table ordered by key and time with columns `key_cols`,
#'   `time_col` (IDate), `observed` (transformed scale, `NA` for missing
#'   weeks), `seasonal`, `trend`, `trend_se` (`NA` for `henderson`, `loess`,
#'   `whittaker`), `trend_lo`, `trend_hi`, `remainder`
#'   (`observed - seasonal - trend`), `trend_pass1`, `band_lo`, `band_hi`,
#'   `excluded`, `flag_1`, `flag_2` (`NA` when pass 2 was not run), `flag`,
#'   `flag_type` (`NA` for unflagged weeks), `pass` (final pass of the key),
#'   `method`, `weight` (final Student-t weight, `smooth_t` only) and
#'   `flag_weight` (`weight < (nu + 1) / (nu + z^2)`). Attributes: `fits` (one row per key x pass with `pass`,
#'   `K`, `aicc_K`, `parameter`, `value`, `criterion`, `criterion_value`,
#'   `edf`, `rho`, `ic_ratio`, `var_level`, `var_slope`, `var_obs`, `band`,
#'   `band_defined`, `n_excluded`, `n_flag`, `iterations` (IRLS, `smooth_t`),
#'   `irls_converged`, `q_select`, `phi`, `loglik_profile`, `q_at_edge`,
#'   `phi_at_edge` (`smooth_t`, see Details), `final`, `converged`,
#'   `error`), `flags` (one row per run of `flag` with `start`,
#'   `end`, `n_weeks`, `flag_type`, `shift`, `half_width`), `transform`,
#'   `observed_transform`, `key_cols`, `time_col`, `period`, `method` and
#'   `band`.
#'
#' @examples
#' set.seed(1)
#' weeks <- data.table::as.IDate("2023-01-02") + 7L * (0:119)
#' tw <- as.numeric(weeks) / 7
#' mu <- 6 + 0.002 * seq_along(weeks) + 0.2 * sin(2 * pi * tw / (365.25 / 7))
#' n <- stats::rpois(120, exp(mu))
#' n[c(40, 80)] <- n[c(40, 80)] * 15
#' d <- data.table::data.table(source = "a", week = weeks, n = n)
#' rs <- robust_smooth(d, "source", "week", "n", method = "whittaker",
#'   harmonics = 1L)
#' rs[flag == TRUE, list(week, flag_type)]
#' attr(rs, "fits")
#' idx <- trend_index(rs, "source", "week",
#'   base_period = c("2023-01-02", "2023-12-31"), transform = "log")
#'
#' @export
robust_smooth <- function(
  dt,
  key_cols,
  time_col,
  value_col,
  method = c(
    "henderson",
    "loess",
    "gam_ar1",
    "whittaker",
    "level",
    "llt",
    "smooth",
    "smooth_t"
  ),
  transform = c("log1p", "log", "none"),
  period = 365.25 / 7,
  harmonics = NULL,
  max_harmonics = 6L,
  n_backfit = 3L,
  band = c("model", "mad"),
  z = 3.5,
  k = 3.5,
  max_iter = 2L,
  run_min = 4L,
  nu = 4,
  level = 0.95,
  flag_na = c("all", "isolated"),
  q_select = c("gaussian", "profile_t"),
  log10_q_grid = seq(-6, 1, by = 0.25),
  damping = c("none", "profile"),
  phi_grid = c(seq(0.80, 0.98, by = 0.02), 0.99, 1)
) {
  caller <- "robust_smooth"

  # 8.1 validation -----
  method <- match.arg(method)
  transform <- match.arg(transform)
  band <- match.arg(band)
  flag_na <- match.arg(flag_na)
  q_select <- match.arg(q_select)
  damping <- match.arg(damping)
  .si_check_df(dt, "dt", caller)
  .ti_check_keys(key_cols, caller)
  .si_check_name(time_col, "time_col", caller)
  .si_check_name(value_col, "value_col", caller)
  check_columns(dt, c(key_cols, time_col, value_col), caller)
  if (any(c(time_col, value_col) %in% key_cols) || time_col == value_col) {
    stop(
      caller,
      ": `key_cols`, `time_col` and `value_col` must be distinct",
      call. = FALSE
    )
  }
  if (any(c(key_cols, time_col) %in% .rs_out_cols)) {
    stop(
      caller,
      ": `key_cols` and `time_col` cannot use the output names ",
      paste(.rs_out_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(dt[[value_col]])) {
    stop(caller, ": `value_col` must be numeric", call. = FALSE)
  }
  .ti_check_num(period, "period", caller, lower = 2, upper = Inf, open = TRUE)
  k_max <- floor(period / 2)
  if (!is.null(harmonics)) {
    .ti_check_num(harmonics, "harmonics", caller, 0, k_max, integer = TRUE)
    harmonics <- as.integer(harmonics)
  }
  .ti_check_num(max_harmonics, "max_harmonics", caller, 0, k_max, TRUE)
  .ti_check_num(n_backfit, "n_backfit", caller, 0, 100, integer = TRUE)
  .ti_check_num(z, "z", caller, 0, Inf, open = TRUE)
  .ti_check_num(k, "k", caller, 0, Inf, open = TRUE)
  .ti_check_num(max_iter, "max_iter", caller, 0, 20, integer = TRUE)
  .ti_check_num(run_min, "run_min", caller, 1, Inf, integer = TRUE)
  .ti_check_num(nu, "nu", caller, 0, Inf, open = TRUE)
  .ti_check_num(level, "level", caller, 0, 1, open = TRUE)
  if (method == "gam_ar1") {
    check_suggests("mgcv", "for method = 'gam_ar1'")
  }
  if (method != "smooth_t" && (q_select != "gaussian" || damping != "none")) {
    stop(
      caller,
      ": `q_select` and `damping` apply only to method = 'smooth_t'",
      call. = FALSE
    )
  }
  if (damping == "profile" && q_select != "profile_t") {
    stop(
      caller,
      ": damping = 'profile' requires q_select = 'profile_t' ",
      "((q, phi) are profiled jointly)",
      call. = FALSE
    )
  }
  if (
    !is.numeric(log10_q_grid) ||
      length(log10_q_grid) == 0L ||
      anyNA(log10_q_grid) ||
      any(!is.finite(log10_q_grid))
  ) {
    stop(caller, ": `log10_q_grid` must be finite numbers", call. = FALSE)
  }
  if (
    !is.numeric(phi_grid) ||
      length(phi_grid) == 0L ||
      anyNA(phi_grid) ||
      any(phi_grid <= 0 | phi_grid > 1)
  ) {
    stop(caller, ": `phi_grid` must contain values in (0, 1]", call. = FALSE)
  }
  o <- list(
    period = period,
    harmonics = harmonics,
    max_harmonics = as.integer(max_harmonics),
    n_backfit = as.integer(n_backfit),
    band = band,
    z = z,
    k = k,
    max_iter = as.integer(max_iter),
    run_min = as.integer(run_min),
    nu = nu,
    flag_na = flag_na,
    lengths = c(13L, 23L, 39L),
    spans = seq(0.06, 0.5, by = 0.02),
    rhos = seq(0, 0.9, by = 0.05),
    q_select = q_select,
    log10_q_grid = sort(unique(as.numeric(log10_q_grid))),
    damping = damping,
    phi_grid = sort(unique(as.numeric(phi_grid)))
  )

  # 8.2 weekly grid and transform -----
  d <- data.table::as.data.table(dt)[,
    c(key_cols, time_col, value_col),
    with = FALSE
  ]
  data.table::setnames(d, c(time_col, value_col), c(".time", ".y"))
  data.table::set(d, j = ".time", value = .ti_time(d$.time, time_col, caller))
  if (nrow(d) == 0L) {
    stop(caller, ": `dt` has no rows", call. = FALSE)
  }
  if (anyDuplicated(d, by = c(key_cols, ".time"))) {
    stop(caller, ": duplicated key x time rows in `dt`", call. = FALSE)
  }
  d <- .ti_complete_weeks(d, key_cols, caller)
  data.table::setorderv(d, c(key_cols, ".time"))
  data.table::set(
    d,
    j = "observed",
    value = .rs_transform(as.numeric(d$.y), transform, caller)
  )
  d[, .y := NULL]

  # 8.3 per-key fits -----
  d[, .gid := .GRP, by = key_cols]
  rng <- d[, list(.first = .I[1L], .last = .I[.N]), by = .gid]
  n_row <- nrow(d)
  num_cols <- c(
    "seasonal",
    "trend",
    "trend_se",
    "trend_pass1",
    ".half",
    "weight"
  )
  lgl_cols <- c("excluded", "flag_1", "flag_2", "flag")
  res <- c(
    stats::setNames(
      lapply(num_cols, function(i) rep(NA_real_, n_row)),
      num_cols
    ),
    stats::setNames(lapply(lgl_cols, function(i) rep(NA, n_row)), lgl_cols),
    list(
      flag_type = rep(NA_character_, n_row),
      pass = rep(NA_integer_, n_row)
    )
  )
  keys <- d[rng$.first, key_cols, with = FALSE]
  tw_all <- as.numeric(d$.time) / 7
  obs_all <- d$observed
  fit_list <- vector("list", nrow(rng))
  run_list <- vector("list", nrow(rng))
  n_fail <- 0L
  no_band <- character(0)
  for (g in seq_len(nrow(rng))) {
    idx <- rng$.first[g]:rng$.last[g]
    out <- tryCatch(
      .rs_one(obs_all[idx], tw_all[idx], method, o),
      error = function(e) e
    )
    kg <- keys[g]
    if (inherits(out, "error")) {
      n_fail <- n_fail + 1L
      fit_list[[g]] <- cbind(
        kg,
        data.table::data.table(
          pass = NA_integer_,
          error = conditionMessage(out)
        )
      )
      next
    }
    for (cc in c(setdiff(num_cols, ".half"), lgl_cols, "flag_type")) {
      res[[cc]][idx] <- out[[cc]]
    }
    res$.half[idx] <- out$half
    res$pass[idx] <- out$pass
    fits_g <- out$fits
    if (!all(fits_g$band_defined)) {
      no_band <- c(
        no_band,
        paste(vapply(kg, as.character, character(1)), collapse = "/")
      )
    }
    data.table::set(fits_g, j = "error", value = NA_character_)
    fit_list[[g]] <- cbind(kg[rep(1L, nrow(fits_g))], fits_g)
    if (nrow(out$runs) > 0L) {
      rr <- out$runs
      t_g <- d$.time[idx]
      run_list[[g]] <- cbind(
        kg[rep(1L, nrow(rr))],
        data.table::data.table(
          start = t_g[rr$start],
          end = t_g[rr$end],
          n_weeks = rr$n_weeks,
          flag_type = rr$flag_type,
          shift = rr$shift,
          half_width = rr$half_width
        )
      )
    }
  }

  # 8.4 output -----
  for (cc in names(res)) {
    data.table::set(d, j = cc, value = res[[cc]])
  }
  zq <- stats::qnorm(1 - (1 - level) / 2)
  data.table::set(d, j = "trend_lo", value = d$trend - zq * d$trend_se)
  data.table::set(d, j = "trend_hi", value = d$trend + zq * d$trend_se)
  data.table::set(d, j = "remainder", value = d$observed - d$seasonal - d$trend)
  data.table::set(d, j = "band_lo", value = d$seasonal + d$trend - d$.half)
  data.table::set(d, j = "band_hi", value = d$seasonal + d$trend + d$.half)
  data.table::set(d, j = "method", value = method)
  data.table::set(
    d,
    j = "flag_weight",
    value = d$weight < (nu + 1) / (nu + z^2)
  )
  d[, c(".gid", ".half") := NULL]
  data.table::setnames(d, ".time", time_col)
  data.table::setcolorder(d, c(key_cols, time_col, .rs_out_cols))
  fits <- data.table::rbindlist(fit_list, fill = TRUE)
  data.table::set(fits, j = "method", value = rep(method, nrow(fits)))
  data.table::setcolorder(fits, c(key_cols, "method"))
  flags <- data.table::rbindlist(run_list)
  if (nrow(flags) == 0L) {
    flags <- cbind(
      keys[0L],
      data.table::data.table(
        start = d[[time_col]][0L],
        end = d[[time_col]][0L],
        n_weeks = integer(0),
        flag_type = character(0),
        shift = numeric(0),
        half_width = numeric(0)
      )
    )
  }
  if (n_fail > 0L) {
    warning(
      caller,
      ": the fit failed for ",
      n_fail,
      " key(s); their components are NA (see attr(, 'fits')$error)",
      call. = FALSE
    )
  }
  if (length(no_band) > 0L) {
    warning(
      caller,
      ": method '",
      method,
      "': the model band is undefined (observation variance ~ 0) for key(s) ",
      paste(no_band, collapse = ", "),
      "; their flags are NA (see attr(, 'fits')$band_defined)",
      call. = FALSE
    )
  }
  data.table::setattr(d, "fits", fits)
  data.table::setattr(d, "flags", flags)
  data.table::setattr(
    d,
    "transform",
    if (transform == "none") "none" else "log"
  )
  data.table::setattr(d, "observed_transform", transform)
  data.table::setattr(d, "key_cols", key_cols)
  data.table::setattr(d, "time_col", time_col)
  data.table::setattr(d, "period", period)
  data.table::setattr(d, "method", method)
  data.table::setattr(d, "band", band)
  d[]
}
