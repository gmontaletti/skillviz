# Trend extraction, diagnostics, revisions and index numbers -----

# 1. internal helpers -----

#' Names of the columns added by extract_trend()
#' @keywords internal
#' @noRd
.ti_out_cols <- c(
  "observed",
  "seasonal",
  "trend",
  "trend_filtered",
  "trend_se",
  "trend_lo",
  "trend_hi",
  "remainder",
  "imputed",
  "method"
)

#' Check a single number within bounds
#' @keywords internal
#' @noRd
.ti_check_num <- function(
  x,
  name,
  caller,
  lower = -Inf,
  upper = Inf,
  integer = FALSE,
  open = FALSE
) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (ok) {
    ok <- if (open) x > lower && x < upper else x >= lower && x <= upper
  }
  if (ok && integer) {
    ok <- x == round(x)
  }
  if (!ok) {
    what <- if (integer) "a single integer" else "a single number"
    br <- if (open) c("(", ")") else c("[", "]")
    stop(
      caller,
      ": `",
      name,
      "` must be ",
      what,
      " in ",
      br[1L],
      lower,
      ", ",
      upper,
      br[2L],
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Check optional key columns (NULL allowed when allow_null = TRUE)
#' @keywords internal
#' @noRd
.ti_check_keys <- function(key_cols, caller, allow_null = FALSE) {
  if (is.null(key_cols) && allow_null) {
    return(invisible(TRUE))
  }
  .si_check_keys(key_cols, caller)
  if (anyDuplicated(key_cols)) {
    stop(caller, ": `key_cols` contains duplicated names", call. = FALSE)
  }
  invisible(TRUE)
}

#' Coerce a time column to IDate (no NA)
#' @keywords internal
#' @noRd
.ti_time <- function(x, name, caller) {
  .st_as_idate(x, name, caller)
}

#' Complete each key on its weekly grid between first and last week
#'
#' `d` has columns `key_cols`, `.time` (IDate) and others; missing weeks are
#' added with NA in the other columns.
#' @keywords internal
#' @noRd
.ti_complete_weeks <- function(d, key_cols, caller) {
  rng <- d[,
    list(
      .t0 = min(.time),
      .t1 = max(.time),
      .bad = any((as.integer(.time) - as.integer(min(.time))) %% 7L != 0L)
    ),
    by = key_cols
  ]
  if (any(rng$.bad)) {
    stop(
      caller,
      ": times must lie on a weekly grid (multiples of 7 days apart ",
      "within each key)",
      call. = FALSE
    )
  }
  grid <- rng[,
    list(
      .time = data.table::as.IDate(
        seq.int(as.integer(.t0), as.integer(.t1), by = 7L)
      )
    ),
    by = key_cols
  ]
  d[grid, on = c(key_cols, ".time")]
}

#' Transform a series to the modelling scale
#' @keywords internal
#' @noRd
.ti_transform <- function(y, transform, caller) {
  ok <- y[!is.na(y)]
  if (any(!is.finite(ok))) {
    stop(caller, ": values must be finite or NA", call. = FALSE)
  }
  switch(
    transform,
    log = {
      if (any(ok <= 0)) {
        stop(
          caller,
          ": transform = 'log' requires strictly positive values ",
          "(set zero weeks to NA or add an offset)",
          call. = FALSE
        )
      }
      log(y)
    },
    logit = {
      if (any(ok <= 0 | ok >= 1)) {
        stop(
          caller,
          ": transform = 'logit' requires shares strictly between 0 and 1",
          call. = FALSE
        )
      }
      stats::qlogis(y)
    },
    none = y
  )
}

#' Fourier regressors (sin, cos pairs) for K harmonics
#' @keywords internal
#' @noRd
.ti_fourier <- function(tw, K, period) {
  X <- matrix(0, nrow = length(tw), ncol = 2L * K)
  for (k in seq_len(K)) {
    a <- 2 * pi * k * tw / period
    X[, 2L * k - 1L] <- sin(a)
    X[, 2L * k] <- cos(a)
  }
  X
}

#' Linear interpolation of NA values (constant at the edges)
#' @keywords internal
#' @noRd
.ti_interp <- function(x) {
  ok <- !is.na(x)
  if (all(ok)) {
    return(x)
  }
  if (sum(ok) < 2L) {
    stop("fewer than two non-missing values", call. = FALSE)
  }
  stats::approx(which(ok), x[ok], xout = seq_along(x), rule = 2)$y
}

#' Small-sample corrected AIC
#' @keywords internal
#' @noRd
.ti_aicc <- function(loglik, p, n) {
  if (!is.finite(loglik) || n - p - 1 <= 0) {
    return(Inf)
  }
  -2 * loglik + 2 * p + 2 * p * (p + 1) / (n - p - 1)
}

#' Variance with fewer than two finite values returning NA
#' @keywords internal
#' @noRd
.ti_var <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) NA_real_ else stats::var(x)
}

#' Difference at lag h, NA-preserving
#' @keywords internal
#' @noRd
.ti_lagdiff <- function(x, h) {
  n <- length(x)
  if (n <= h) {
    return(numeric(0))
  }
  x[(h + 1L):n] - x[seq_len(n - h)]
}

#' One StructTS local linear trend fit, errors returned as conditions
#'
#' `x` must not start with `NA`. `init` are initial variances on the scale
#' of `x` (`NULL` = StructTS default). Optimiser warnings are muffled: the
#' code is returned in `$code`.
#' @keywords internal
#' @noRd
.ti_structts <- function(x, init = NULL, optim.control = NULL, fixed = NULL) {
  tryCatch(
    suppressWarnings(
      stats::StructTS(
        stats::ts(x),
        type = "trend",
        init = init,
        fixed = fixed,
        optim.control = optim.control
      )
    ),
    error = function(e) e
  )
}

#' Whether a StructTS fit is usable and converged
#' @keywords internal
#' @noRd
.ti_llt_ok <- function(fit) {
  !inherits(fit, "error") && is.finite(fit$loglik) && fit$code == 0L
}

#' Local linear trend by StructTS, smoothed by KalmanSmooth
#'
#' Two starts of the optimiser: the StructTS default, and the series
#' rescaled to unit standard deviation of its first differences with initial
#' variances (level 0.5, slope 0.01, observation 0.5) and an optimiser
#' parameter scale 100 times smaller for the slope variance. The default
#' start often stops at a spurious zero slope variance with a lower
#' likelihood. Among the converged starts the one with the highest
#' log-likelihood is kept; without a converged start, the best finite one
#' is kept and `degenerate` is `TRUE`, unless refitting it with its zero
#' variances fixed at zero converges without loss of likelihood (start
#' "boundary"). `retried` is `TRUE` when the default start did not
#' converge. Leading `NA` weeks are dropped for the
#' estimation (StructTS requires a first observed value) and recovered by
#' running the Kalman filter and smoother on the full series with the
#' estimated model.
#'
#' With `smooth = TRUE` the level variance is fixed at zero in every start
#' (`fixed = c(0, NA, NA)`, StructTS order level, slope, epsilon): the trend
#' is an integrated random walk and only the slope and observation
#' variances are estimated. The fixed level variance is reported in
#' `fixed_variance`, not in `zero_variance`, and does not make the fit a
#' boundary estimate.
#' @keywords internal
#' @noRd
.ti_trend_llt <- function(x, smooth = FALSE) {
  n <- length(x)
  first <- which(!is.na(x))[1L]
  xf <- x[first:n]
  n_f <- sum(!is.na(xf))
  sc <- sqrt(.ti_var(diff(xf)))
  if (!is.finite(sc) || sc <= 0) {
    sc <- 1
  }
  # free variances (StructTS order: level, slope, epsilon)
  free <- if (smooth) c(FALSE, TRUE, TRUE) else c(TRUE, TRUE, TRUE)
  fx0 <- if (smooth) c(0, NA_real_, NA_real_) else NULL
  starts <- list(
    default = list(fit = .ti_structts(xf, fixed = fx0), sc = 1),
    rescaled = list(
      fit = .ti_structts(
        (xf - xf[1L]) / sc,
        init = c(level = if (smooth) 0 else 0.5, slope = 0.01, epsilon = 0.5),
        optim.control = list(parscale = c(1, 0.01, 1)[free]),
        fixed = fx0
      ),
      sc = sc
    )
  )
  ll <- vapply(
    starts,
    function(s) {
      if (inherits(s$fit, "error")) NA_real_ else s$fit$loglik - n_f * log(s$sc)
    },
    numeric(1)
  )
  ok <- vapply(starts, function(s) .ti_llt_ok(s$fit), logical(1))
  cand <- if (any(ok)) which(ok) else which(is.finite(ll))
  if (length(cand) == 0L) {
    err <- Filter(function(s) inherits(s$fit, "error"), starts)
    stop(
      if (length(err)) conditionMessage(err[[1L]]$fit) else
        "non-finite log-likelihood",
      call. = FALSE
    )
  }
  pick <- cand[which.max(ll[cand])]
  tol <- 1e-8 * max(.ti_var(diff(x)), .Machine$double.eps, na.rm = TRUE)
  if (!any(ok)) {
    # no converged start: if the best one has variances at zero, refit
    # with them fixed at zero (boundary profile); accepted when it
    # converges without losing likelihood
    b <- starts[[pick]]
    zb <- b$fit$coef * b$sc^2 <= tol | !free
    if (any(zb & free) && !all(zb)) {
      xs <- if (b$sc == 1) xf else (xf - xf[1L]) / b$sc
      fx <- ifelse(zb, 0, NA_real_)
      f3 <- .ti_structts(xs, init = pmax(b$fit$coef, 1e-12), fixed = fx)
      ll3 <- if (inherits(f3, "error")) NA_real_ else
        f3$loglik - n_f * log(b$sc)
      if (.ti_llt_ok(f3) && ll3 >= ll[pick] - 1e-8 * abs(ll[pick])) {
        starts$boundary <- list(fit = f3, sc = b$sc)
        ll <- c(ll, boundary = ll3)
        ok <- c(ok, boundary = TRUE)
        pick <- length(starts)
      }
    }
  }
  fit <- starts[[pick]]$fit
  s2 <- starts[[pick]]$sc^2
  coef <- fit$coef * s2
  # model on the original scale with the StructTS initial state, run on
  # the full series (leading NA too)
  mod <- fit$model0
  mod$a <- c(xf[1L], 0)
  # diffuse prior of scale .ti_kappa instead of the StructTS default (1e4):
  # see .ti_kappa for the loss of precision in the smoothed variance
  mod$P[] <- .ti_kappa * stats::var(xf, na.rm = TRUE)
  mod$Pn[] <- 0
  mod$V <- diag(unname(coef[1:2]))
  mod$h <- unname(coef[3L])
  sm <- stats::KalmanSmooth(x, mod, nit = -1L)
  fl <- stats::KalmanRun(x, mod, nit = -1L)
  loglik <- unname(ll[pick])
  zero <- c("level", "slope", "obs")[c(coef[1:3] <= tol) & free]
  list(
    trend = as.numeric(sm$smooth[, 1L]),
    trend_se = sqrt(pmax(as.numeric(sm$var[, 1L, 1L]), 0)),
    trend_filtered = as.numeric(fl$states[, 1L]),
    loglik = loglik,
    n_par = sum(free),
    n_lik = sum(!is.na(x)),
    var_level = unname(coef[1L]),
    var_slope = unname(coef[2L]),
    var_obs = unname(coef[3L]),
    convergence = as.integer(fit$code),
    zero_variance = if (length(zero)) paste(zero, collapse = ",") else "",
    fixed_variance = if (smooth) "level" else "",
    boundary = length(zero) > 0L,
    retried = !ok[["default"]],
    start = names(starts)[pick],
    degenerate = !ok[[pick]]
  )
}

#' Scale of the diffuse initial state covariance of the Kalman smoothers
#'
#' The initial covariance is `.ti_kappa * var(x)`. The StructTS default
#' (1e4) makes the smoothed variance, a difference of terms of order
#' kappa, lose all its significant digits when the slope variance is near
#' zero: it can come out negative and be clamped to a zero standard error.
#' 1 keeps the smoothed variance accurate (it no longer changes with kappa
#' to 3 digits) and moves the smoothed state by less than 1e-6.
#' @keywords internal
#' @noRd
.ti_kappa <- 1

#' Additive ETS trend (A or Ad) by fable on the interpolated series
#' @keywords internal
#' @noRd
.ti_trend_ets <- function(x) {
  xi <- .ti_interp(x)
  tsb <- tsibble::tsibble(tt = seq_along(xi), yy = xi, index = tt)
  m <- suppressWarnings(fabletools::model(
    tsb,
    ETS = fable::ETS(yy ~ error("A") + trend(c("A", "Ad")) + season("N"))
  ))
  if (fabletools::is_null_model(m$ETS[[1L]])) {
    stop("ETS estimation failed", call. = FALSE)
  }
  cc <- fabletools::components(m)
  level <- cc$level[match(seq_along(xi), cc$tt)]
  gl <- fabletools::glance(m)
  td <- fabletools::tidy(m)
  sm <- stats::setNames(td$estimate, td$term)
  at_bound <- c("alpha", "beta")[
    c(sm["alpha"], sm["beta"]) <= 1e-4 * (1 + 1e-6)
  ]
  at_bound <- at_bound[!is.na(at_bound)]
  list(
    trend = as.numeric(level),
    trend_se = rep(NA_real_, length(xi)),
    trend_filtered = as.numeric(level),
    loglik = gl$log_lik,
    n_par = nrow(td) + 1L,
    n_lik = length(xi),
    var_level = NA_real_,
    var_slope = NA_real_,
    var_obs = gl$sigma2,
    convergence = NA_integer_,
    zero_variance = if (length(at_bound)) {
      paste(at_bound, collapse = ",")
    } else {
      ""
    },
    fixed_variance = "",
    boundary = length(at_bound) > 0L,
    retried = FALSE,
    start = NA_character_,
    degenerate = !is.finite(gl$log_lik)
  )
}

#' Trend fit dispatcher
#' @keywords internal
#' @noRd
.ti_fit_trend <- function(x, method) {
  switch(
    method,
    llt = .ti_trend_llt(x),
    smooth = .ti_trend_llt(x, smooth = TRUE),
    ets = .ti_trend_ets(x)
  )
}

#' Joint trend + Fourier seasonal fit by backfitting
#' @keywords internal
#' @noRd
.ti_backfit <- function(y, X, method, n_backfit) {
  n <- length(y)
  if (is.null(X)) {
    return(list(seasonal = rep(0, n), fit = .ti_fit_trend(y, method)))
  }
  ok <- !is.na(y)
  tt <- seq_len(n)
  tt <- (tt - mean(tt)) / stats::sd(tt)
  Z <- cbind(1, tt, tt^2, X)
  b <- stats::lm.fit(Z[ok, , drop = FALSE], y[ok])$coefficients
  b[is.na(b)] <- 0
  seasonal <- drop(X %*% b[-(1:3)])
  X1 <- cbind(1, X)
  for (i in seq_len(n_backfit)) {
    fit <- .ti_fit_trend(y - seasonal, method)
    r <- y - fit$trend
    b <- stats::lm.fit(X1[ok, , drop = FALSE], r[ok])$coefficients
    b[is.na(b)] <- 0
    seasonal <- drop(X %*% b[-1L])
  }
  list(seasonal = seasonal, fit = .ti_fit_trend(y - seasonal, method))
}

#' Trend extraction for one series
#' @keywords internal
#' @noRd
.ti_extract_one <- function(
  y,
  tw,
  method,
  period,
  harmonics,
  max_harmonics,
  n_backfit
) {
  n <- length(y)
  n_obs <- sum(!is.na(y))
  if (n_obs < 8L) {
    stop("fewer than 8 non-missing observations", call. = FALSE)
  }
  if (method == "stl") {
    xi <- .ti_interp(y)
    s <- stats::stl(
      stats::ts(xi, frequency = 52),
      s.window = "periodic",
      robust = TRUE
    )
    trend <- as.numeric(s$time.series[, "trend"])
    return(list(
      seasonal = as.numeric(s$time.series[, "seasonal"]),
      trend = trend,
      trend_se = rep(NA_real_, n),
      trend_filtered = rep(NA_real_, n),
      K = NA_integer_,
      aicc = NA_real_,
      var_level = NA_real_,
      var_slope = NA_real_,
      var_obs = NA_real_,
      convergence = NA_integer_,
      zero_variance = "",
      fixed_variance = "",
      boundary = FALSE,
      retried = FALSE,
      start = NA_character_,
      degenerate = FALSE
    ))
  }
  Ks <- if (is.null(harmonics)) 0:max_harmonics else harmonics
  best <- NULL
  for (K in Ks) {
    X <- if (K > 0L) .ti_fourier(tw, K, period) else NULL
    bf <- .ti_backfit(y, X, method, n_backfit)
    aicc <- .ti_aicc(bf$fit$loglik, bf$fit$n_par + 2L * K, bf$fit$n_lik)
    if (is.null(best) || aicc < best$aicc) {
      best <- list(K = as.integer(K), aicc = aicc, bf = bf)
    }
  }
  f <- best$bf$fit
  list(
    seasonal = best$bf$seasonal,
    trend = f$trend,
    trend_se = f$trend_se,
    trend_filtered = f$trend_filtered,
    K = best$K,
    aicc = if (is.finite(best$aicc)) best$aicc else NA_real_,
    var_level = f$var_level,
    var_slope = f$var_slope,
    var_obs = f$var_obs,
    convergence = f$convergence,
    zero_variance = f$zero_variance,
    fixed_variance = f$fixed_variance,
    boundary = f$boundary,
    retried = f$retried,
    start = f$start,
    degenerate = f$degenerate
  )
}

#' Base-period flag from two dates or a logical column
#' @keywords internal
#' @noRd
.ti_base_flag <- function(dt, time_col, base_period, caller) {
  if (is.character(base_period) && length(base_period) == 1L) {
    if (!base_period %in% names(dt)) {
      stop(
        caller,
        ": `base_period` names a column not found in the data: ",
        base_period,
        call. = FALSE
      )
    }
    f <- dt[[base_period]]
    if (!is.logical(f)) {
      stop(
        caller,
        ": the `base_period` column must be logical",
        call. = FALSE
      )
    }
    return(!is.na(f) & f)
  }
  b <- if (
    length(base_period) == 2L &&
      (inherits(base_period, "Date") || is.character(base_period))
  ) {
    tryCatch(data.table::as.IDate(base_period), error = function(e) NA)
  } else {
    NA
  }
  if (length(b) != 2L || anyNA(b) || b[1L] > b[2L]) {
    stop(
      caller,
      ": `base_period` must be two dates (start <= end, inclusive) or the ",
      "name of a logical column",
      call. = FALSE
    )
  }
  t <- dt[[time_col]]
  t >= b[1L] & t <= b[2L]
}

#' Prepare a weight table: columns .src and .w
#' @keywords internal
#' @noRd
.ti_prep_weights <- function(weights, source_col, caller) {
  .si_check_df(weights, "weights", caller)
  check_columns(weights, c(source_col, "w"), paste0(caller, " (weights)"))
  wt <- data.table::data.table(
    .src = weights[[source_col]],
    .w = weights[["w"]]
  )
  if (!is.numeric(wt$.w) || anyNA(wt$.w) || any(!is.finite(wt$.w))) {
    stop(
      caller,
      ": `weights$w` must be finite numeric without NA",
      call. = FALSE
    )
  }
  if (any(wt$.w < 0) || sum(wt$.w) <= 0) {
    stop(
      caller,
      ": `weights$w` must be non-negative with a positive sum",
      call. = FALSE
    )
  }
  if (anyNA(wt$.src) || anyDuplicated(wt$.src)) {
    stop(
      caller,
      ": `weights` must have one row per source, without NA sources",
      call. = FALSE
    )
  }
  wt
}

#' Cap weights by water-filling
#'
#' Weights above `cap` are set to `cap` and the excess is redistributed
#' proportionally over the weights below the cap, until no weight exceeds it.
#' When `length(w) * cap < 1` the cap is infeasible and equal weights are
#' returned.
#' @keywords internal
#' @noRd
.ti_waterfill <- function(w, cap) {
  n <- length(w)
  if (n == 0L) {
    return(w)
  }
  w <- w / sum(w)
  if (n * cap <= 1) {
    return(rep(1 / n, n))
  }
  eps <- 1e-12
  repeat {
    over <- w > cap + eps
    if (!any(over)) {
      break
    }
    excess <- sum(w[over] - cap)
    w[over] <- cap
    free <- w < cap - eps
    if (!any(free) || sum(w[free]) <= 0) {
      return(rep(1 / n, n))
    }
    w[free] <- w[free] + excess * w[free] / sum(w[free])
  }
  w
}

#' Weighted aggregation of source indices (internal names)
#'
#' `d` has columns `key_cols`, `.time`, `.src`, `.idx`; `wt` has `.src`,
#' `.w`. Returns `key_cols`, `.time`, `.val`, `n_sources`, `covered_weight`,
#' `n_eff` for every key x time present in `d`.
#' @keywords internal
#' @noRd
.ti_aggregate <- function(d, key_cols, wt, type, cap, caller) {
  by <- c(key_cols, ".time")
  groups <- unique(d[, by, with = FALSE])
  x <- d[!is.na(.idx)]
  x[wt, .w := i..w, on = ".src"]
  x <- x[!is.na(.w) & .w > 0]
  geo <- type == "geometric"
  if (geo && any(x$.idx <= 0)) {
    stop(
      caller,
      ": geometric aggregation requires strictly positive indices",
      call. = FALSE
    )
  }
  w_tot <- sum(wt$.w)
  x[, .wn := .w / sum(.w), by = by]
  if (!is.null(cap)) {
    x[, .wn := .ti_waterfill(.wn, cap), by = by]
  }
  agg <- x[,
    list(
      .val = if (geo) exp(sum(.wn * log(.idx))) else sum(.wn * .idx),
      n_sources = .N,
      covered_weight = sum(.w) / w_tot,
      n_eff = 1 / sum(.wn^2)
    ),
    by = by
  ]
  out <- agg[groups, on = by]
  out[is.na(n_sources), `:=`(n_sources = 0L, covered_weight = 0)]
  data.table::setorderv(out, by)
  out[]
}

#' Run an expression with a given seed, restoring the RNG state
#' @keywords internal
#' @noRd
.ti_with_seed <- function(seed, expr) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit(
    if (had) {
      assign(".Random.seed", old, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  )
  set.seed(seed)
  expr
}

#' Check the shared (source/key, time) arguments of an index table
#' @keywords internal
#' @noRd
.ti_check_index_input <- function(x, name, cols, caller) {
  .si_check_df(x, name, caller)
  check_columns(x, cols, caller)
  invisible(TRUE)
}

# 2. extract_trend -----

#' Trend extraction with Fourier seasonality for weekly series
#'
#' Splits each weekly series (one per key) into a deterministic annual
#' seasonal component, a trend and a remainder, on a transformed scale (log
#' for totals, logit for shares). The seasonal component is a Fourier
#' regression whose number of harmonics is chosen by AICc and estimated
#' jointly with the trend by backfitting.
#'
#' @details
#' **Grid and missing weeks.** Within each key, times must be on a weekly
#' grid; missing weeks (absent rows or `NA` values, e.g. outlier weeks set
#' to `NA` upstream) are kept as `NA` in `observed` and flagged in
#' `imputed`. The trend at those weeks is the model prediction (`llt`,
#' `smooth`) or
#' comes from a linear interpolation of the input used for fitting (`ets`,
#' `stl`); the per-key flag `interpolated` in the `fits` attribute marks the
#' latter.
#'
#' **Seasonality.** For `llt`, `smooth` and `ets`, the seasonal component is
#' \eqn{s_t = \sum_{k=1}^K a_k \sin(2\pi k t/P) + b_k \cos(2\pi k t/P)}, with
#' `t` the number of weeks since 1970-01-01 (so the phase is common to all
#' keys) and `P = period`. When `harmonics = NULL`, `K` ranges over
#' `0..max_harmonics` and is chosen by
#' \eqn{AICc = -2\ell + 2p + 2p(p+1)/(n-p-1)}, where \eqn{\ell} is the
#' log-likelihood of the trend model on the seasonally adjusted series and
#' `p` counts the trend parameters plus `2K` seasonal coefficients. The
#' backfitting starts from an OLS fit of a quadratic time trend plus the
#' harmonics, then `n_backfit` times fits the trend on `y - s` and
#' regresses `y - trend` on the harmonics; a final trend fit on the last
#' adjusted series gives the output.
#'
#' **Methods.**
#' * `llt` (reference): local linear trend, `stats::StructTS(type = "trend")`
#'   on the adjusted series. `trend` is the smoothed (two-sided) level from
#'   `stats::KalmanSmooth()`, `trend_se` its smoothed standard deviation,
#'   `trend_filtered` the filtered (one-sided) level. `NA` weeks are handled
#'   by the Kalman filter.
#' * `smooth`: smooth trend model, the local linear trend of `llt` with the
#'   level disturbance variance fixed at zero
#'   (`stats::StructTS(type = "trend", fixed = c(0, NA, NA))`, parameters in
#'   the order level, slope, epsilon). The level then has no shocks of its
#'   own and the trend is an integrated random walk: only the slope changes
#'   from week to week. Its smoothed level is a cubic-spline-type smoother
#'   whose smoothness is set by the signal-to-noise ratio
#'   `q = var_slope / var_obs`, estimated by maximum likelihood (no
#'   smoothing parameter is chosen by hand). Smoothing, `trend_se`,
#'   optimiser starts and treatment of `NA` weeks are those of `llt`;
#'   `var_level` is reported as `0` and `fixed_variance` as `"level"`.
#' * `ets`: `fable::ETS(error("A") + trend(c("A", "Ad")) + season("N"))` on
#'   the adjusted series, with `NA` weeks linearly interpolated (ETS does not
#'   accept missing values). `trend` and `trend_filtered` are both the
#'   filtered level; `trend_se` is `NA`. Requires tsibble, fable and
#'   fabletools.
#' * `stl`: `stats::stl(s.window = "periodic", robust = TRUE)` with frequency
#'   52 on the interpolated series. It has its own periodic seasonal
#'   component (no Fourier terms, `K = NA`) and a 52-week period instead of
#'   52.18: a descriptive benchmark only.
#'
#' **Boundary estimates and failed fits.** A variance estimated at zero is a
#' legitimate boundary maximum-likelihood estimate, not a failure: a zero
#' slope variance means a deterministic drift, a zero level variance an
#' integrated random walk (smooth) trend, a zero observation variance a
#' series without measurement noise (e.g. a moving sum). For `llt` and
#' `smooth`, `zero_variance` lists the estimated variances at zero (below
#' `1e-8` times the variance of the first differences) and `boundary` is
#' `TRUE` when there is at least one; the level variance of `smooth` is
#' fixed by the model, not estimated, so it is listed in `fixed_variance`
#' and never in `zero_variance` or `boundary`; for `ets`, `zero_variance` lists the smoothing parameters
#' (`alpha`, `beta`) at their lower bound `1e-4` and `boundary` flags them.
#' The `llt` (and `smooth`) likelihood is maximised from two starts: the StructTS default
#' and the series rescaled to unit standard deviation of its first
#' differences, with initial variances (level 0.5, slope 0.01, observation
#' 0.5) on that scale and an optimiser `parscale` of 0.01 for the slope
#' variance; the variances are back-transformed. The default start alone
#' often stops at a spurious zero slope variance with a lower likelihood.
#' Among the converged starts (`convergence == 0`, finite log-likelihood)
#' the one with the highest log-likelihood is kept (`start`); `retried` is
#' `TRUE` when the default start did not converge. When neither start
#' converges and the best one has variances at zero, the model is refitted
#' with those variances fixed at zero (`start = "boundary"`); the refit is
#' kept when it converges without loss of likelihood, which confirms a
#' boundary maximum. `degenerate` marks failed fits only: for `llt`, when
#' no start converges (the best finite one is then returned). Leading missing weeks are excluded from the
#' estimation (StructTS needs an observed first value) and filled by the
#' Kalman smoother. For `ets`, `degenerate` marks a non-finite
#' log-likelihood. No fallback method is used: when a fit errors (e.g. on a
#' deterministic series), the key keeps `NA` components, `degenerate` is
#' `TRUE`, the message is stored in `error` and a warning is raised.
#'
#' **Uncertainty.** `trend_se` (and `trend_lo`, `trend_hi`) is the
#' model-based standard error of the smoothed level, conditional on the
#' estimated variances and seasonal coefficients. It is valid only when the
#' observed series is not itself a moving sum: for stock-type series
#' (moving sums of inflow, such as the stock of open postings), the
#' innovations are moving averages by construction and `trend_se`
#' understates the uncertainty. Use it on inflow-type series only.
#'
#' **Cost.** With `harmonics = NULL`, each key requires
#' `(max_harmonics + 1) * (n_backfit + 1)` trend fits (each with two
#' optimiser starts for `llt` and `smooth`). The AICc counts 3 trend
#' parameters for `llt` and 2 for `smooth`.
#'
#' @param dt A data.frame or data.table with one row per key x week.
#' @param key_cols Character vector of key columns identifying each series.
#' @param time_col Name of the time column (class Date or IDate, weekly).
#' @param value_col Name of the value column (positive for `log`, in (0, 1)
#'   for `logit`); `NA` allowed.
#' @param transform Scale of the model: `"log"`, `"logit"` or `"none"`.
#' @param method Trend method: `"llt"` (default), `"smooth"` (local linear
#'   trend with level variance fixed at zero), `"ets"` or `"stl"`.
#' @param period Seasonal period in weeks (default 365.25 / 7).
#' @param harmonics Fixed number of Fourier harmonics, or `NULL` to choose
#'   it by AICc. Ignored by `stl`.
#' @param max_harmonics Largest number of harmonics considered when
#'   `harmonics = NULL`.
#' @param n_backfit Number of backfitting iterations (non-negative integer).
#' @param level Confidence level of `trend_lo` and `trend_hi` (`llt` and
#'   `smooth` only).
#'
#' @return A data.table with columns `key_cols`, `time_col` (IDate),
#'   `observed` (transformed scale, `NA` for missing weeks), `seasonal`,
#'   `trend` (smoothed for `llt`), `trend_filtered`, `trend_se`, `trend_lo`,
#'   `trend_hi`, `remainder` (`observed - seasonal - trend`), `imputed`
#'   (logical, `TRUE` where the input was missing) and `method`, ordered by
#'   key and time. Attribute `fits`: one row per key with `method`, `K`,
#'   `aicc`, `var_level`, `var_slope`, `var_obs`, `convergence`,
#'   `zero_variance`, `fixed_variance`, `boundary`, `retried`, `start`,
#'   `degenerate`,
#'   `interpolated`, `n_obs`, `n_na` and `error`. Attributes `transform`, `key_cols`, `time_col` and `period`
#'   record the call.
#'
#' @examples
#' set.seed(1)
#' weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:155)
#' tw <- as.numeric(weeks) / 7
#' d <- data.table::data.table(
#'   source = "a",
#'   week = weeks,
#'   n = exp(5 + 0.003 * seq_along(weeks) +
#'     0.2 * sin(2 * pi * tw / (365.25 / 7)) + rnorm(156, 0, 0.03))
#' )
#' tr <- extract_trend(d, "source", "week", "n", max_harmonics = 2L)
#' head(tr)
#' attr(tr, "fits")
#' # smooth trend model: level variance fixed at 0
#' ts_ <- extract_trend(d, "source", "week", "n", method = "smooth",
#'   harmonics = 1L)
#' attr(ts_, "fits")[, c("var_level", "var_slope", "var_obs", "fixed_variance")]
#'
#' @export
extract_trend <- function(
  dt,
  key_cols,
  time_col,
  value_col,
  transform = c("log", "logit", "none"),
  method = c("llt", "smooth", "ets", "stl"),
  period = 365.25 / 7,
  harmonics = NULL,
  max_harmonics = 6L,
  n_backfit = 3L,
  level = 0.95
) {
  caller <- "extract_trend"

  # 2.1 validation -----
  transform <- match.arg(transform)
  method <- match.arg(method)
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
  if (any(c(key_cols, time_col) %in% .ti_out_cols)) {
    stop(
      caller,
      ": `key_cols` and `time_col` cannot use the output names ",
      paste(.ti_out_cols, collapse = ", "),
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
  .ti_check_num(level, "level", caller, 0, 1, open = TRUE)
  if (method == "ets") {
    check_suggests("tsibble", "for method = 'ets'")
    check_suggests("fabletools", "for method = 'ets'")
    check_suggests("fable", "for method = 'ets'")
  }

  # 2.2 weekly grid and transform -----
  d <- data.table::as.data.table(dt)[,
    c(key_cols, time_col, value_col),
    with = FALSE
  ]
  data.table::setnames(d, c(time_col, value_col), c(".time", ".y"))
  d[, .time := .ti_time(.time, time_col, caller)]
  if (anyDuplicated(d, by = c(key_cols, ".time"))) {
    stop(
      caller,
      ": duplicated key x time rows in `dt`",
      call. = FALSE
    )
  }
  if (nrow(d) == 0L) {
    stop(caller, ": `dt` has no rows", call. = FALSE)
  }
  d <- .ti_complete_weeks(d, key_cols, caller)
  data.table::setorderv(d, c(key_cols, ".time"))
  d[, observed := .ti_transform(as.numeric(.y), transform, caller)]
  d[, .y := NULL]
  d[, imputed := is.na(observed)]

  # 2.3 per-key fits -----
  d[, .gid := .GRP, by = key_cols]
  rng <- d[, list(.first = .I[1L], .last = .I[.N]), by = .gid]
  n_row <- nrow(d)
  cols_num <- c("seasonal", "trend", "trend_filtered", "trend_se")
  res <- stats::setNames(
    lapply(cols_num, function(i) rep(NA_real_, n_row)),
    cols_num
  )
  n_key <- nrow(rng)
  fits <- data.table::data.table(
    method = rep(method, n_key),
    K = NA_integer_,
    aicc = NA_real_,
    var_level = NA_real_,
    var_slope = NA_real_,
    var_obs = NA_real_,
    convergence = NA_integer_,
    zero_variance = NA_character_,
    fixed_variance = NA_character_,
    boundary = NA,
    retried = NA,
    start = NA_character_,
    degenerate = NA,
    interpolated = FALSE,
    n_obs = 0L,
    n_na = 0L,
    error = NA_character_
  )
  tw_all <- as.numeric(d$.time) / 7
  obs_all <- d$observed
  for (g in seq_len(n_key)) {
    idx <- rng$.first[g]:rng$.last[g]
    y <- obs_all[idx]
    n_na <- sum(is.na(y))
    data.table::set(fits, g, "n_obs", length(y) - n_na)
    data.table::set(fits, g, "n_na", n_na)
    data.table::set(
      fits,
      g,
      "interpolated",
      n_na > 0L && !method %in% c("llt", "smooth")
    )
    out <- tryCatch(
      .ti_extract_one(
        y,
        tw_all[idx],
        method,
        period,
        harmonics,
        max_harmonics,
        n_backfit
      ),
      error = function(e) e
    )
    if (inherits(out, "error")) {
      data.table::set(fits, g, "error", conditionMessage(out))
      data.table::set(fits, g, "degenerate", TRUE)
      next
    }
    for (cc in cols_num) {
      res[[cc]][idx] <- out[[cc]]
    }
    for (cc in c(
      "K",
      "aicc",
      "var_level",
      "var_slope",
      "var_obs",
      "convergence",
      "zero_variance",
      "fixed_variance",
      "boundary",
      "retried",
      "start",
      "degenerate"
    )) {
      data.table::set(fits, g, cc, out[[cc]])
    }
  }

  # 2.4 output -----
  for (cc in cols_num) {
    data.table::set(d, j = cc, value = res[[cc]])
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  d[, `:=`(
    trend_lo = trend - z * trend_se,
    trend_hi = trend + z * trend_se,
    remainder = observed - seasonal - trend,
    method = method
  )]
  keys <- d[rng$.first, key_cols, with = FALSE]
  fits <- cbind(keys, fits)
  d[, .gid := NULL]
  data.table::setnames(d, ".time", time_col)
  data.table::setcolorder(d, c(key_cols, time_col, .ti_out_cols))
  n_fail <- sum(!is.na(fits$error))
  if (n_fail > 0L) {
    warning(
      caller,
      ": the fit failed for ",
      n_fail,
      " key(s); their components are NA (see attr(, 'fits')$error)",
      call. = FALSE
    )
  }
  data.table::setattr(d, "fits", fits)
  data.table::setattr(d, "transform", transform)
  data.table::setattr(d, "key_cols", key_cols)
  data.table::setattr(d, "time_col", time_col)
  data.table::setattr(d, "period", period)
  d[]
}

# 3. trend_diagnostics -----

#' Diagnostics of extracted trends
#'
#' Summarises, per key, how much of the movement of the seasonally adjusted
#' series the trend absorbs, how smooth it is and whether the remainder
#' still has autocorrelation or exceeds the sampling-noise floor.
#'
#' @details
#' On each key, ordered by time:
#' * `ratio_trend_remainder`: `var(diff(trend)) / var(diff(remainder))`.
#' * `var_removed_d1`, `var_removed_d13`: share of the variance of the
#'   1-week and 13-week differences of the seasonally adjusted series
#'   (`observed - seasonal`) removed by the trend,
#'   `1 - var(diff_h(remainder)) / var(diff_h(observed - seasonal))`. The
#'   seasonally adjusted series is used because the seasonal component, not
#'   the trend, accounts for the 13-week movement of the raw series.
#' * `lb_p_value`: Ljung-Box p-value of the remainder at lag 13.
#' * `roughness`: variance of the second differences of the trend.
#' * `remainder_var`: variance of the remainder.
#' * When `n_col` is given: `noise_floor` and `ratio_floor =
#'   remainder_var / noise_floor`. For `transform = "log"` the floor is the
#'   Poisson variance of a log count, `1 / mean(n)`; for `"logit"` it is the
#'   binomial variance of a logit share, `1 / mean(n p (1 - p))` with `p` the
#'   observed share and `n` the denominator; for `"none"` it is `NA`. A
#'   ratio near 1 means the remainder is close to pure sampling noise.
#'
#' @param tr Output of [extract_trend()] (possibly with an added column of
#'   counts).
#' @param key_cols Character vector of key columns.
#' @param n_col Name of a column of counts in `tr` (number of postings for
#'   totals, denominator for shares), or `NULL`.
#' @param time_col Name of the time column; by default taken from the
#'   `time_col` attribute of `tr`.
#'
#' @return A data.table with one row per key (and `method`, when present)
#'   and columns `n_obs`, `ratio_trend_remainder`, `var_removed_d1`,
#'   `var_removed_d13`, `lb_p_value`, `roughness`, `remainder_var` and, when
#'   `n_col` is given, `noise_floor` and `ratio_floor`.
#'
#' @examples
#' set.seed(1)
#' weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:155)
#' d <- data.table::data.table(
#'   source = "a",
#'   week = weeks,
#'   n = stats::rpois(156, exp(5 + 0.003 * seq_along(weeks)))
#' )
#' tr <- extract_trend(d, "source", "week", "n", harmonics = 1L)
#' tr[d, n := i.n, on = c("source", "week")]
#' trend_diagnostics(tr, "source", n_col = "n")
#'
#' @export
trend_diagnostics <- function(tr, key_cols, n_col = NULL, time_col = NULL) {
  caller <- "trend_diagnostics"

  # 3.1 validation -----
  .si_check_df(tr, "tr", caller)
  .ti_check_keys(key_cols, caller)
  if (is.null(time_col)) {
    time_col <- attr(tr, "time_col")
    if (is.null(time_col)) {
      stop(
        caller,
        ": `time_col` is missing and `tr` has no 'time_col' attribute",
        call. = FALSE
      )
    }
  }
  .si_check_name(time_col, "time_col", caller)
  need <- c(key_cols, time_col, "observed", "seasonal", "trend", "remainder")
  if (!is.null(n_col)) {
    .si_check_name(n_col, "n_col", caller)
    need <- c(need, n_col)
  }
  check_columns(tr, need, caller)
  if (!is.null(n_col)) {
    .st_check_count(tr[[n_col]], n_col, caller, allow_na = TRUE)
  }
  transform <- attr(tr, "transform")
  if (is.null(transform)) {
    transform <- "log"
  }

  # 3.2 per-key statistics -----
  by <- key_cols
  if ("method" %in% names(tr) && !"method" %in% key_cols) {
    by <- c(key_cols, "method")
  }
  cols <- c(by, time_col, "observed", "seasonal", "trend", "remainder")
  d <- data.table::as.data.table(tr)[, cols, with = FALSE]
  d[, .n := if (is.null(n_col)) NA_real_ else as.numeric(tr[[n_col]])]
  data.table::setnames(d, time_col, ".time")
  data.table::setorderv(d, c(by, ".time"))
  diag_one <- function(observed, seasonal, trend, remainder, n) {
    sa <- observed - seasonal
    removed <- function(h) {
      dr <- .ti_lagdiff(remainder, h)
      ds <- .ti_lagdiff(sa, h)
      ok <- is.finite(dr) & is.finite(ds)
      1 - .ti_var(dr[ok]) / .ti_var(ds[ok])
    }
    rem <- remainder
    lb <- if (sum(is.finite(rem)) > 14L) {
      stats::Box.test(rem, lag = 13L, type = "Ljung-Box")$p.value
    } else {
      NA_real_
    }
    rv <- .ti_var(rem)
    floor_v <- NA_real_
    if (!all(is.na(n))) {
      floor_v <- switch(
        transform,
        log = 1 / mean(n, na.rm = TRUE),
        logit = {
          p <- stats::plogis(observed)
          1 / mean(n * p * (1 - p), na.rm = TRUE)
        },
        NA_real_
      )
    }
    list(
      n_obs = sum(!is.na(observed)),
      ratio_trend_remainder = .ti_var(diff(trend)) / .ti_var(diff(rem)),
      var_removed_d1 = removed(1L),
      var_removed_d13 = removed(13L),
      lb_p_value = lb,
      roughness = .ti_var(diff(trend, differences = 2L)),
      remainder_var = rv,
      noise_floor = floor_v,
      ratio_floor = rv / floor_v
    )
  }
  out <- d[,
    diag_one(observed, seasonal, trend, remainder, .n),
    by = by
  ]
  if (is.null(n_col)) {
    out[, c("noise_floor", "ratio_floor") := NULL]
  }
  out[]
}

# 4. trend_revisions -----

#' Revisions of the trend as new data arrive
#'
#' Re-runs [extract_trend()] on the data truncated at each origin and
#' compares the trend estimated with data up to the origin with the
#' full-sample trend, at lags `0..max_lag` weeks before the origin.
#'
#' @details
#' For origin `o` and lag `h`, the revision is
#' `trend_full(o - 7h) - trend_vintage(o - 7h)` on the transformed scale.
#' The size of the revision as a function of `h` tells how many final weeks
#' of the trend are provisional. With `harmonics = NULL` the number of
#' harmonics is re-selected at each origin, so revisions also include
#' changes of `K`. When `scale` is given, `revision_index` expresses the
#' revision in index points: `scale * (exp(rev) - 1)` for `log`,
#' `scale * (p_full / p_vintage - 1)` for `logit` (shares) and
#' `scale * (full / vintage - 1)` for `none`.
#'
#' @param dt,key_cols,time_col,value_col As in [extract_trend()].
#' @param origins Vector of origins (Date, IDate or 'YYYY-MM-DD'), each equal
#'   to a time present in `dt`.
#' @param max_lag Largest lag in weeks (non-negative integer).
#' @param scale `NULL`, or a positive number (e.g. 100) to add revisions in
#'   index points.
#' @param ... Further arguments passed to [extract_trend()] (e.g.
#'   `transform`, `method`, `harmonics`).
#'
#' @return A data.table with columns `key_cols`, `origin`, `lag`,
#'   `time_col`, `trend_full`, `trend_vintage`, `revision` and, when `scale`
#'   is given, `revision_index`. Attribute `summary`: one row per lag with
#'   `n`, `mean_revision`, `mean_abs_revision`, `rmse_revision` and, when
#'   `scale` is given, `mean_abs_revision_index`.
#'
#' @examples
#' set.seed(1)
#' weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:119)
#' d <- data.table::data.table(
#'   source = "a",
#'   week = weeks,
#'   n = exp(5 + 0.003 * seq_along(weeks) + rnorm(120, 0, 0.03))
#' )
#' rv <- trend_revisions(d, "source", "week", "n",
#'   origins = weeks[c(100, 110)], max_lag = 4L, harmonics = 0L
#' )
#' attr(rv, "summary")
#'
#' @export
trend_revisions <- function(
  dt,
  key_cols,
  time_col,
  value_col,
  origins,
  max_lag = 26L,
  scale = NULL,
  ...
) {
  caller <- "trend_revisions"

  # 4.1 validation -----
  .si_check_df(dt, "dt", caller)
  .ti_check_keys(key_cols, caller)
  .si_check_name(time_col, "time_col", caller)
  .si_check_name(value_col, "value_col", caller)
  check_columns(dt, c(key_cols, time_col), caller)
  .ti_check_num(max_lag, "max_lag", caller, 0, Inf, integer = TRUE)
  if (!is.null(scale)) {
    .ti_check_num(scale, "scale", caller, 0, Inf, open = TRUE)
  }
  if (any(c("origin", "lag") %in% key_cols)) {
    stop(caller, ": `key_cols` cannot contain 'origin' or 'lag'", call. = FALSE)
  }
  orig <- if (
    length(origins) > 0L &&
      (inherits(origins, "Date") || is.character(origins))
  ) {
    tryCatch(data.table::as.IDate(origins), error = function(e) NA)
  } else {
    NA
  }
  times <- .ti_time(dt[[time_col]], time_col, caller)
  if (anyNA(orig) || !all(orig %in% times)) {
    stop(
      caller,
      ": `origins` must be dates present in the time column of `dt`",
      call. = FALSE
    )
  }
  orig <- sort(unique(orig))

  # 4.2 full sample and vintages -----
  full <- extract_trend(dt, key_cols, time_col, value_col, ...)
  transform <- attr(full, "transform")
  f <- full[, c(key_cols, time_col, "trend"), with = FALSE]
  data.table::setnames(f, c(time_col, "trend"), c(".time", "trend_full"))
  dd <- data.table::as.data.table(dt)[,
    c(key_cols, time_col, value_col),
    with = FALSE
  ]
  t_int <- as.integer(times)
  res <- lapply(orig, function(o) {
    sub <- dd[which(t_int <= as.integer(o))]
    v <- extract_trend(sub, key_cols, time_col, value_col, ...)
    v <- v[, c(key_cols, time_col, "trend"), with = FALSE]
    data.table::setnames(v, c(time_col, "trend"), c(".time", "trend_vintage"))
    v[, lag := (as.integer(o) - as.integer(.time)) %/% 7L]
    v <- v[lag >= 0L & lag <= max_lag]
    v[, origin := o]
    v
  })
  out <- data.table::rbindlist(res)
  out <- f[out, on = c(key_cols, ".time")]
  out[, revision := trend_full - trend_vintage]
  if (!is.null(scale)) {
    out[,
      revision_index := scale *
        switch(
          transform,
          log = exp(revision) - 1,
          logit = stats::plogis(trend_full) / stats::plogis(trend_vintage) - 1,
          trend_full / trend_vintage - 1
        )
    ]
  }
  data.table::setnames(out, ".time", time_col)
  first <- c(key_cols, "origin", "lag", time_col)
  data.table::setcolorder(out, c(first, setdiff(names(out), first)))
  data.table::setorderv(out, c(key_cols, "origin", "lag"))

  # 4.3 summary by lag -----
  smry <- out[
    !is.na(revision),
    list(
      n = .N,
      mean_revision = mean(revision),
      mean_abs_revision = mean(abs(revision)),
      rmse_revision = sqrt(mean(revision^2))
    ),
    by = lag
  ]
  data.table::setorderv(smry, "lag")
  if (!is.null(scale)) {
    smi <- out[
      !is.na(revision_index),
      list(mean_abs_revision_index = mean(abs(revision_index))),
      by = lag
    ]
    smry <- smi[smry, on = "lag"]
    data.table::setcolorder(
      smry,
      c(
        setdiff(names(smry), "mean_abs_revision_index"),
        "mean_abs_revision_index"
      )
    )
  }
  data.table::setattr(out, "summary", smry)
  out[]
}

# 5. trend_index -----

#' Index numbers from extracted trends
#'
#' Turns a trend on the transformed scale into an index equal to `scale` on
#' average over a base period, separately for each key.
#'
#' @details
#' * `log`: `indice = scale * exp(trend - mean(trend in base))`. The base is
#'   geometric: the mean of `log(indice)` over the base period is exactly
#'   `log(scale)`, so the geometric mean of the index in the base is
#'   `scale`. A geometric base keeps indices multiplicative, which makes the
#'   decomposition total x share in [compose_skill_index()] exact.
#' * `logit`: the trend is back-transformed to a share `p = plogis(trend)`
#'   and `indice = scale * p / mean(p in base)`: the arithmetic mean of the
#'   index in the base is `scale`.
#' * `none`: `indice = scale * trend / mean(trend in base)`; keys whose base
#'   mean is not positive get `NA`.
#'
#' Keys without non-missing trend values in the base period get `NA` and a
#' warning.
#'
#' @param tr A data.frame or data.table of trends (e.g. from
#'   [extract_trend()]).
#' @param key_cols Character vector of key columns.
#' @param time_col Name of the time column (class Date or IDate).
#' @param base_period Either two dates (start and end, inclusive) or the name
#'   of a logical column of `tr` marking the base rows.
#' @param value_col Name of the trend column (default `"trend"`).
#' @param transform Scale of `value_col`: `"log"`, `"logit"` or `"none"`.
#' @param scale Value of the index in the base period (default 100).
#'
#' @return A data.table with columns `key_cols`, `time_col`, `value_col` and
#'   `indice`, ordered by key and time.
#'
#' @examples
#' weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:103)
#' tr <- data.table::data.table(
#'   source = "a", week = weeks, trend = 5 + 0.002 * seq_along(weeks)
#' )
#' idx <- trend_index(tr, "source", "week",
#'   base_period = c("2024-01-01", "2024-12-31")
#' )
#' in_base <- idx$week <= as.Date("2024-12-31")
#' exp(mean(log(idx$indice[in_base])))
#'
#' @export
trend_index <- function(
  tr,
  key_cols,
  time_col,
  base_period,
  value_col = "trend",
  transform = c("log", "logit", "none"),
  scale = 100
) {
  caller <- "trend_index"

  # 5.1 validation -----
  transform <- match.arg(transform)
  .si_check_df(tr, "tr", caller)
  .ti_check_keys(key_cols, caller)
  .si_check_name(time_col, "time_col", caller)
  .si_check_name(value_col, "value_col", caller)
  check_columns(tr, c(key_cols, time_col, value_col), caller)
  if ("indice" %in% c(key_cols, time_col, value_col)) {
    stop(caller, ": 'indice' is reserved for the output", call. = FALSE)
  }
  if (!is.numeric(tr[[value_col]])) {
    stop(caller, ": `value_col` must be numeric", call. = FALSE)
  }
  .ti_check_num(scale, "scale", caller, 0, Inf, open = TRUE)

  # 5.2 base means and index -----
  base <- .ti_base_flag(tr, time_col, base_period, caller)
  d <- data.table::as.data.table(tr)[,
    c(key_cols, time_col, value_col),
    with = FALSE
  ]
  d[, .time := .ti_time(d[[time_col]], time_col, caller)]
  d[, .v := as.numeric(tr[[value_col]])]
  d[, .b := base & !is.na(.v)]
  if (transform == "logit") {
    d[, .v := stats::plogis(.v)]
  }
  d[, .m := if (any(.b)) mean(.v[.b]) else NA_real_, by = key_cols]
  d[,
    indice := switch(
      transform,
      log = scale * exp(.v - .m),
      scale * .v / data.table::fifelse(.m > 0, .m, NA_real_)
    )
  ]
  n_nobase <- data.table::uniqueN(d[is.na(.m), key_cols, with = FALSE])
  if (n_nobase > 0L) {
    warning(
      caller,
      ": ",
      n_nobase,
      " key(s) without trend values in the base period; index set to NA",
      call. = FALSE
    )
  }
  if (transform == "none" && any(!is.na(d$.m) & d$.m <= 0)) {
    warning(
      caller,
      ": non-positive base mean for some key(s); index set to NA",
      call. = FALSE
    )
  }
  data.table::setorderv(d, c(key_cols, ".time"))
  d[, c(".time", ".v", ".b", ".m") := NULL]
  d[]
}

# 6. aggregate_index -----

#' Weighted aggregation of source indices
#'
#' Combines per-source indices into one index per key and time with fixed
#' source weights, renormalised over the sources present in each cell, with
#' an optional cap on the weight of any source.
#'
#' @details
#' In each key x time cell, the sources with a non-missing index and a
#' positive weight are used, their weights `w` renormalised to sum to one.
#' With `cap`, weights above `cap` are set to `cap` and the excess is
#' redistributed proportionally over the other sources, repeating until no
#' weight exceeds the cap (water-filling); when fewer than `1 / cap` sources
#' are present the cap is infeasible and equal weights are used.
#' `type = "geometric"` returns `exp(sum(w * log(I)))`, `"arithmetic"`
#' returns `sum(w * I)`. `covered_weight` is the share of the total weight
#' of `weights` held by the sources present, before renormalisation, and
#' `n_eff = 1 / sum(w^2)` is the effective number of sources (Kish) after
#' renormalisation and cap. Sources of `idx` missing from `weights` are
#' ignored with a warning.
#'
#' @param idx A data.frame or data.table of per-source indices.
#' @param source_col Name of the source column.
#' @param key_cols Character vector of further key columns (e.g. skill), or
#'   `NULL`.
#' @param time_col Name of the time column.
#' @param index_col Name of the index column (default `"indice"`).
#' @param weights A data.frame with columns `source_col` and `w`
#'   (non-negative weights, e.g. base-period volumes).
#' @param type `"geometric"` (default) or `"arithmetic"`.
#' @param cap `NULL` or a single number in (0, 1]: maximum weight of a
#'   source.
#'
#' @return A data.table with columns `key_cols`, `time_col`, `index_col`,
#'   `n_sources`, `covered_weight` and `n_eff`, one row per key x time of
#'   `idx`.
#'
#' @examples
#' weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:3)
#' idx <- data.table::data.table(
#'   source = rep(c("a", "b"), each = 4), week = rep(weeks, 2),
#'   indice = c(100, 101, 102, 103, 100, 99, 98, 97)
#' )
#' w <- data.table::data.table(source = c("a", "b"), w = c(3, 1))
#' aggregate_index(idx, "source", time_col = "week", weights = w)
#'
#' @export
aggregate_index <- function(
  idx,
  source_col,
  key_cols = NULL,
  time_col,
  index_col = "indice",
  weights,
  type = c("geometric", "arithmetic"),
  cap = NULL
) {
  caller <- "aggregate_index"

  # 6.1 validation -----
  type <- match.arg(type)
  .si_check_df(idx, "idx", caller)
  .si_check_name(source_col, "source_col", caller)
  .ti_check_keys(key_cols, caller, allow_null = TRUE)
  .si_check_name(time_col, "time_col", caller)
  .si_check_name(index_col, "index_col", caller)
  all_cols <- c(source_col, key_cols, time_col, index_col)
  if (anyDuplicated(all_cols)) {
    stop(
      caller,
      ": `source_col`, `key_cols`, `time_col` and `index_col` must be distinct",
      call. = FALSE
    )
  }
  if (
    any(
      c(key_cols, time_col, index_col) %in%
        c("n_sources", "covered_weight", "n_eff")
    )
  ) {
    stop(
      caller,
      ": 'n_sources', 'covered_weight' and 'n_eff' are reserved for the output",
      call. = FALSE
    )
  }
  .ti_check_index_input(idx, "idx", all_cols, caller)
  if (!is.numeric(idx[[index_col]])) {
    stop(caller, ": `index_col` must be numeric", call. = FALSE)
  }
  wt <- .ti_prep_weights(weights, source_col, caller)
  if (!is.null(cap)) {
    .ti_check_num(cap, "cap", caller, 0, 1)
    if (cap <= 0) {
      stop(caller, ": `cap` must be in (0, 1]", call. = FALSE)
    }
  }

  # 6.2 aggregation -----
  d <- data.table::as.data.table(idx)[, all_cols, with = FALSE]
  data.table::setnames(
    d,
    c(source_col, time_col, index_col),
    c(".src", ".time", ".idx")
  )
  if (anyDuplicated(d, by = c(key_cols, ".time", ".src"))) {
    stop(
      caller,
      ": duplicated source x key x time rows in `idx`",
      call. = FALSE
    )
  }
  miss <- setdiff(unique(d$.src), wt$.src)
  if (length(miss) > 0L) {
    warning(
      caller,
      ": source(s) without weight ignored: ",
      paste(utils::head(miss, 5L), collapse = ", "),
      if (length(miss) > 5L) ", ...",
      call. = FALSE
    )
  }
  out <- .ti_aggregate(d, key_cols, wt, type, cap, caller)
  data.table::setnames(out, c(".time", ".val"), c(time_col, index_col))
  out[]
}

# 7. compose_skill_index -----

#' Skill index from total and share trends
#'
#' Builds, for each skill, the index of the skill stock as the product of
#' the trend of the total stock and the trend of the skill share, source by
#' source, then aggregates over sources with fixed weights.
#'
#' @details
#' For source `s`, skill `k` and week `t`, the log trend of the skill stock
#' is \eqn{L_{s,k,t} = \tau_{s,t} + \log \mathrm{plogis}(\eta_{s,k,t})}, with
#' \eqn{\tau} the log trend of the total and \eqn{\eta} the logit trend of
#' the share. The per-source index is
#' `scale * exp(L - mean(L in base))` (geometric base, as in
#' [trend_index()] with `transform = "log"`), so it factorises exactly as
#' `I_total * I_share / scale`, where `I_total` is the geometric-base index
#' of \eqn{\tau} and `I_share` the geometric-base index of
#' `log(plogis(eta))`. Note that [trend_index()] with `transform = "logit"`
#' uses an arithmetic base on shares, which differs from the geometric one
#' by a constant factor per cell.
#'
#' A cell (source, skill) is modelled when it has a non-missing index. The
#' coverage of a skill is the share of the total weight held by its
#' modelled sources; skills with coverage below `min_covered` are dropped
#' and listed in the attribute `dropped`. The remaining skills are
#' aggregated over sources with [aggregate_index()] rules: weights
#' renormalised over the modelled sources in each week, optional cap.
#' With `type = "geometric"` (default) the skill index is
#' \eqn{\exp(\sum_s \tilde w_s \log I_{s,k,t})}; with
#' `type = "arithmetic"` it is \eqn{\sum_s \tilde w_s I_{s,k,t}}, with
#' \eqn{\tilde w_s} the weights renormalised over the sources whose cell
#' (source, skill) is modelled in week `t`. The per-source indices
#' (attribute `by_source`) do not depend on `type`. The exact factorisation
#' total x share holds source by source; after aggregation it holds only
#' for the geometric mean.
#'
#' @param total_tr Trends of the total by source: columns `source_col`,
#'   `time_col` and `value_col` (log scale), e.g. from [extract_trend()]
#'   with `transform = "log"`.
#' @param share_tr Trends of skill shares by source: columns `source_col`,
#'   `skill_col`, `time_col` and `value_col` (logit scale), e.g. from
#'   [extract_trend()] with `transform = "logit"`.
#' @param source_col Name of the source column.
#' @param skill_col Name of the skill column.
#' @param time_col Name of the time column.
#' @param weights A data.frame with columns `source_col` and `w`.
#' @param base_period Two dates (start, end, inclusive) or the name of a
#'   logical column of `share_tr`.
#' @param min_covered Minimum covered weight for a skill to be kept (in
#'   \[0, 1\]).
#' @param scale Value of the index in the base period (default 100).
#' @param value_col Name of the trend column in both tables (default
#'   `"trend"`).
#' @param cap `NULL` or the maximum weight of a source (see
#'   [aggregate_index()]).
#' @param type Aggregation across sources: `"geometric"` (default) or
#'   `"arithmetic"` (see [aggregate_index()]).
#'
#' @return A data.table with columns `skill_col`, `time_col`, `indice`,
#'   `n_sources`, `covered_weight` and `n_eff`. Attributes: `by_source`
#'   (columns `source_col`, `skill_col`, `time_col`, `log_trend`, `indice`:
#'   per-source skill indices, input of [index_jackknife()]) and `dropped`
#'   (skills below `min_covered`, with `covered_weight` and `n_sources`).
#'
#' @examples
#' weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:103)
#' tot <- data.table::data.table(
#'   source = rep(c("a", "b"), each = 104), week = rep(weeks, 2),
#'   trend = c(5 + 0.002 * (1:104), 4 + 0.001 * (1:104))
#' )
#' sh <- data.table::data.table(
#'   source = rep(c("a", "b"), each = 104), skill = "k1",
#'   week = rep(weeks, 2), trend = stats::qlogis(0.1 + 0.0005 * (1:208))
#' )
#' w <- data.table::data.table(source = c("a", "b"), w = c(2, 1))
#' compose_skill_index(tot, sh, "source", "skill", "week", w,
#'   base_period = c("2024-01-01", "2024-12-31")
#' )
#' compose_skill_index(tot, sh, "source", "skill", "week", w,
#'   base_period = c("2024-01-01", "2024-12-31"), type = "arithmetic"
#' )
#'
#' @export
compose_skill_index <- function(
  total_tr,
  share_tr,
  source_col,
  skill_col,
  time_col,
  weights,
  base_period,
  min_covered = 0.5,
  scale = 100,
  value_col = "trend",
  cap = NULL,
  type = c("geometric", "arithmetic")
) {
  caller <- "compose_skill_index"

  # 7.1 validation -----
  type <- match.arg(type)
  .si_check_df(total_tr, "total_tr", caller)
  .si_check_df(share_tr, "share_tr", caller)
  for (a in c("source_col", "skill_col", "time_col", "value_col")) {
    .si_check_name(get(a), a, caller)
  }
  if (anyDuplicated(c(source_col, skill_col, time_col, value_col))) {
    stop(
      caller,
      ": `source_col`, `skill_col`, `time_col` and `value_col` must be distinct",
      call. = FALSE
    )
  }
  if (startsWith(skill_col, ".")) {
    stop(caller, ": `skill_col` cannot start with '.'", call. = FALSE)
  }
  check_columns(total_tr, c(source_col, time_col, value_col), caller)
  check_columns(share_tr, c(source_col, skill_col, time_col, value_col), caller)
  wt <- .ti_prep_weights(weights, source_col, caller)
  .ti_check_num(min_covered, "min_covered", caller, 0, 1)
  .ti_check_num(scale, "scale", caller, 0, Inf, open = TRUE)
  if (!is.null(cap)) {
    .ti_check_num(cap, "cap", caller, 0, 1)
    if (cap <= 0) {
      stop(caller, ": `cap` must be in (0, 1]", call. = FALSE)
    }
  }

  # 7.2 per-source skill log trend -----
  tot <- data.table::data.table(
    .src = total_tr[[source_col]],
    .time = .ti_time(total_tr[[time_col]], time_col, caller),
    .tau = as.numeric(total_tr[[value_col]])
  )
  if (anyDuplicated(tot, by = c(".src", ".time"))) {
    stop(caller, ": duplicated source x time rows in `total_tr`", call. = FALSE)
  }
  sh <- data.table::data.table(
    .src = share_tr[[source_col]],
    .sk = share_tr[[skill_col]],
    .time = .ti_time(share_tr[[time_col]], time_col, caller),
    .eta = as.numeric(share_tr[[value_col]]),
    .base = .ti_base_flag(share_tr, time_col, base_period, caller)
  )
  if (anyDuplicated(sh, by = c(".src", ".sk", ".time"))) {
    stop(
      caller,
      ": duplicated source x skill x time rows in `share_tr`",
      call. = FALSE
    )
  }
  sh[tot, .tau := i..tau, on = c(".src", ".time")]
  sh[, log_trend := .tau + stats::plogis(.eta, log.p = TRUE)]
  sh[, .b := .base & !is.na(log_trend)]
  sh[,
    .m := if (any(.b)) mean(log_trend[.b]) else NA_real_,
    by = c(".src", ".sk")
  ]
  sh[, indice := scale * exp(log_trend - .m)]

  # 7.3 coverage and aggregation -----
  modelled <- unique(sh[!is.na(indice), c(".src", ".sk"), with = FALSE])
  modelled[wt, .w := i..w, on = ".src"]
  w_tot <- sum(wt$.w)
  cov <- modelled[,
    list(covered_weight = sum(.w, na.rm = TRUE) / w_tot, n_sources = .N),
    by = ".sk"
  ]
  all_sk <- data.table::data.table(.sk = unique(sh$.sk))
  cov <- cov[all_sk, on = ".sk"]
  cov[is.na(covered_weight), `:=`(covered_weight = 0, n_sources = 0L)]
  dropped <- cov[covered_weight < min_covered]
  keep <- cov[covered_weight >= min_covered, .sk]
  d <- sh[.sk %in% keep, list(.src, .sk, .time, .idx = indice)]
  if (nrow(d) == 0L) {
    out <- data.table::data.table(
      .sk = sh$.sk[0L],
      .time = sh$.time[0L],
      .val = numeric(0),
      n_sources = integer(0),
      covered_weight = numeric(0),
      n_eff = numeric(0)
    )
  } else {
    out <- .ti_aggregate(d, ".sk", wt, type, cap, caller)
  }
  data.table::setnames(
    out,
    c(".sk", ".time", ".val"),
    c(skill_col, time_col, "indice")
  )
  by_source <- sh[, list(.src, .sk, .time, log_trend, indice)]
  data.table::setorderv(by_source, c(".sk", ".src", ".time"))
  data.table::setnames(
    by_source,
    c(".src", ".sk", ".time"),
    c(source_col, skill_col, time_col)
  )
  data.table::setnames(dropped, ".sk", skill_col)
  data.table::setattr(out, "by_source", by_source)
  data.table::setattr(out, "dropped", dropped[])
  out[]
}

# 8. index_jackknife -----

#' Leave-one-source-out range of an aggregated index
#'
#' Recomputes the aggregated index of [aggregate_index()] leaving out one
#' source at a time and reports the range of the results and the source
#' whose removal moves the index most.
#'
#' @details
#' Without cap, the full weighted mean (of logs for `type = "geometric"`,
#' of indices for `"arithmetic"`) is a convex combination of the
#' leave-one-out means, so the full index always lies in
#' `[jk_min, jk_max]`. In cells where a single source is present, removing
#' it leaves no index; such leave-one-out values are ignored.
#'
#' @param idx,source_col,key_cols,time_col,weights As in [aggregate_index()].
#' @param ... Further arguments passed to [aggregate_index()] (`index_col`,
#'   `type`, `cap`), used both for the full index and for every
#'   leave-one-out index: with `type = "arithmetic"` the range refers to
#'   arithmetic aggregates.
#'
#' @return A data.table with columns `key_cols`, `time_col`, the full index
#'   (`index_col`), `n_sources`, `jk_min`, `jk_max` (range of the
#'   leave-one-out indices), `jk_source` (source whose removal moves the
#'   index most) and `jk_shift` (leave-one-out index minus full index for
#'   that source).
#'
#' @examples
#' weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:3)
#' idx <- data.table::data.table(
#'   source = rep(c("a", "b", "c"), each = 4), week = rep(weeks, 3),
#'   indice = c(100, 101, 102, 103, 100, 99, 98, 97, 100, 100, 101, 101)
#' )
#' w <- data.table::data.table(source = c("a", "b", "c"), w = c(3, 1, 1))
#' index_jackknife(idx, "source", NULL, "week", w)
#'
#' @export
index_jackknife <- function(idx, source_col, key_cols, time_col, weights, ...) {
  caller <- "index_jackknife"
  args <- list(...)
  index_col <- if (is.null(args$index_col)) "indice" else args$index_col
  full <- aggregate_index(
    idx,
    source_col = source_col,
    key_cols = key_cols,
    time_col = time_col,
    weights = weights,
    ...
  )
  if (any(c("jk_min", "jk_max", "jk_source", "jk_shift") %in% key_cols)) {
    stop(caller, ": 'jk_*' names are reserved for the output", call. = FALSE)
  }
  srcs <- unique(idx[[source_col]])
  if (length(srcs) < 2L) {
    stop(caller, ": at least two sources are needed", call. = FALSE)
  }
  by <- c(key_cols, time_col)
  idx <- data.table::as.data.table(idx)
  loo <- data.table::rbindlist(lapply(srcs, function(s) {
    r <- suppressWarnings(aggregate_index(
      idx[idx[[source_col]] != s],
      source_col = source_col,
      key_cols = key_cols,
      time_col = time_col,
      weights = weights,
      ...
    ))
    r <- r[, c(by, index_col), with = FALSE]
    data.table::setnames(r, index_col, ".loo")
    r[, .drop := s]
    r
  }))
  f <- full[, c(by, index_col), with = FALSE]
  data.table::setnames(f, index_col, ".full")
  loo <- f[loo, on = by]
  loo <- loo[!is.na(.loo) & !is.na(.full)]
  loo[, .dev := abs(.loo - .full)]
  jk <- loo[,
    list(
      jk_min = min(.loo),
      jk_max = max(.loo),
      jk_source = .drop[which.max(.dev)],
      jk_shift = (.loo - .full)[which.max(.dev)]
    ),
    by = by
  ]
  out <- jk[full, on = by]
  data.table::setcolorder(
    out,
    c(by, index_col, "n_sources", "jk_min", "jk_max", "jk_source", "jk_shift")
  )
  out[, c("covered_weight", "n_eff") := NULL]
  data.table::setorderv(out, by)
  out[]
}

# 9. trend_concordance -----

#' Concordance of trends across sources
#'
#' Measures whether sources move together: the weighted share of sources
#' whose trend change over a lag has the sign of the weighted majority, and
#' the share of variance of the first principal component of source trend
#' growth rates, each compared with a placebo distribution obtained by
#' circular shifts of the source series.
#'
#' @details
#' For lag `h` (weeks), the growth of source `s` is
#' `g_{s,t} = trend_{s,t} - trend_{s,t-h}`. In week `t`, with `W+` and `W-`
#' the weights of sources with positive and negative growth and `W` the
#' weight of sources with non-missing growth, the concordance is
#' `max(W+, W-) / W` (1 when all sources agree, 0.5 when split evenly);
#' the statistic `concordance` is its mean over weeks with at least two
#' sources. `pc1_share` is the share of variance of the first principal
#' component of the standardised growth matrix (complete weeks only,
#' unweighted). The placebo shifts the growth series of each source
#' circularly by an independent random offset, which keeps the
#' autocorrelation of each source and destroys the alignment across
#' sources; `p_value = (1 + #(placebo >= observed)) / (n_perm + 1)`.
#'
#' @param tr A data.frame or data.table with one row per source x week
#'   (e.g. totals from [extract_trend()]).
#' @param source_col Name of the source column.
#' @param time_col Name of the time column (weekly Date or IDate).
#' @param lags Integer vector of lags in weeks.
#' @param weights `NULL` (equal weights) or a data.frame with columns
#'   `source_col` and `w`.
#' @param n_perm Number of placebo shifts (non-negative integer).
#' @param seed Seed of the placebo shifts; the global RNG state is restored.
#' @param value_col Name of the trend column (default `"trend"`).
#'
#' @return A data.table with one row per lag x statistic (`concordance`,
#'   `pc1_share`) and columns `lag`, `statistic`, `observed`,
#'   `placebo_mean`, `placebo_q95`, `p_value` and `n_perm`. Attribute
#'   `by_time`: columns `time_col`, `lag`, `concordance` and `n_sources`.
#'
#' @examples
#' weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:79)
#' set.seed(2)
#' tr <- data.table::data.table(
#'   source = rep(c("a", "b", "c"), each = 80), week = rep(weeks, 3),
#'   trend = c(cumsum(rnorm(80)), cumsum(rnorm(80)), cumsum(rnorm(80)))
#' )
#' trend_concordance(tr, "source", "week", lags = 4L, n_perm = 50L)
#'
#' @export
trend_concordance <- function(
  tr,
  source_col,
  time_col,
  lags = c(13L, 52L),
  weights = NULL,
  n_perm = 200L,
  seed = 1L,
  value_col = "trend"
) {
  caller <- "trend_concordance"

  # 9.1 validation -----
  .si_check_df(tr, "tr", caller)
  .si_check_name(source_col, "source_col", caller)
  .si_check_name(time_col, "time_col", caller)
  .si_check_name(value_col, "value_col", caller)
  check_columns(tr, c(source_col, time_col, value_col), caller)
  if (
    !is.numeric(lags) ||
      length(lags) == 0L ||
      anyNA(lags) ||
      any(lags < 1) ||
      any(lags != round(lags))
  ) {
    stop(caller, ": `lags` must be positive integers", call. = FALSE)
  }
  lags <- as.integer(sort(unique(lags)))
  .ti_check_num(n_perm, "n_perm", caller, 0, Inf, integer = TRUE)
  .ti_check_num(seed, "seed", caller, -Inf, Inf, integer = TRUE)

  # 9.2 wide matrix of trends -----
  d <- data.table::data.table(
    .src = tr[[source_col]],
    .time = .ti_time(tr[[time_col]], time_col, caller),
    .v = as.numeric(tr[[value_col]])
  )
  if (anyDuplicated(d, by = c(".src", ".time"))) {
    stop(
      caller,
      ": `tr` must have one row per source x time (filter other keys first)",
      call. = FALSE
    )
  }
  srcs <- sort(unique(d$.src))
  if (length(srcs) < 2L) {
    stop(caller, ": at least two sources are needed", call. = FALSE)
  }
  t0 <- min(d$.time)
  if (any((as.integer(d$.time) - as.integer(t0)) %% 7L != 0L)) {
    stop(caller, ": times must lie on a weekly grid", call. = FALSE)
  }
  grid <- data.table::as.IDate(seq.int(
    as.integer(t0),
    as.integer(max(d$.time)),
    by = 7L
  ))
  n_t <- length(grid)
  Y <- matrix(NA_real_, n_t, length(srcs))
  Y[cbind(match(d$.time, grid), match(d$.src, srcs))] <- d$.v
  w <- if (is.null(weights)) {
    rep(1, length(srcs))
  } else {
    wt <- .ti_prep_weights(weights, source_col, caller)
    ww <- wt$.w[match(srcs, wt$.src)]
    if (anyNA(ww)) {
      stop(caller, ": every source of `tr` needs a weight", call. = FALSE)
    }
    ww
  }
  if (any(lags >= n_t - 1L)) {
    stop(caller, ": `lags` must be shorter than the series", call. = FALSE)
  }

  # 9.3 statistics -----
  conc_t <- function(G) {
    ok <- !is.na(G)
    pos <- (ok & G > 0) %*% w
    neg <- (ok & G < 0) %*% w
    tot <- ok %*% w
    ns <- rowSums(ok)
    v <- ifelse(tot > 0, pmax(pos, neg) / tot, NA_real_)
    v[ns < 2L] <- NA_real_
    list(conc = as.numeric(v), ns = as.integer(ns))
  }
  pc1 <- function(G) {
    G <- G[stats::complete.cases(G), , drop = FALSE]
    if (nrow(G) < 3L) {
      return(NA_real_)
    }
    s <- apply(G, 2L, stats::sd)
    G <- G[, s > 0, drop = FALSE]
    if (ncol(G) < 2L) {
      return(NA_real_)
    }
    ev <- stats::prcomp(G, center = TRUE, scale. = TRUE)$sdev^2
    ev[1L] / sum(ev)
  }
  stat_obs <- function(G) {
    c(concordance = mean(conc_t(G)$conc, na.rm = TRUE), pc1_share = pc1(G))
  }
  by_time <- list()
  smry <- list()
  for (h in lags) {
    G <- Y[(h + 1L):n_t, , drop = FALSE] - Y[seq_len(n_t - h), , drop = FALSE]
    ct <- conc_t(G)
    by_time[[length(by_time) + 1L]] <- data.table::data.table(
      .time = grid[(h + 1L):n_t],
      lag = h,
      concordance = ct$conc,
      n_sources = ct$ns
    )
    obs <- stat_obs(G)
    nr <- nrow(G)
    perm <- if (n_perm > 0L) {
      .ti_with_seed(seed + h, {
        vapply(
          seq_len(n_perm),
          function(i) {
            sh <- sample.int(nr, ncol(G), replace = TRUE) - 1L
            Gp <- G
            for (j in seq_len(ncol(G))) {
              if (sh[j] > 0L) {
                Gp[, j] <- G[c((sh[j] + 1L):nr, seq_len(sh[j])), j]
              }
            }
            stat_obs(Gp)
          },
          numeric(2)
        )
      })
    } else {
      NULL
    }
    for (k in 1:2) {
      pk <- if (is.null(perm)) numeric(0) else perm[k, ]
      pk <- pk[!is.na(pk)]
      has <- length(pk) > 0L && !is.na(obs[k])
      smry[[length(smry) + 1L]] <- data.table::data.table(
        lag = h,
        statistic = names(obs)[k],
        observed = unname(obs[k]),
        placebo_mean = if (has) mean(pk) else NA_real_,
        placebo_q95 = if (has) unname(stats::quantile(pk, 0.95)) else NA_real_,
        p_value = if (has) {
          (1 + sum(pk >= obs[k])) / (length(pk) + 1)
        } else {
          NA_real_
        },
        n_perm = as.integer(length(pk))
      )
    }
  }
  out <- data.table::rbindlist(smry)
  bt <- data.table::rbindlist(by_time)
  data.table::setnames(bt, ".time", time_col)
  data.table::setattr(out, "by_time", bt)
  out[]
}
