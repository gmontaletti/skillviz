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
