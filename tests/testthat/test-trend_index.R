# Tests for trend_index.R -----

# 0. synthetic data helpers -----

ti_period <- 365.25 / 7

# Weekly log series: known trend (linear + curvature) + annual Fourier
# seasonality + Gaussian noise, returned on the original (exp) scale.
sim_weekly <- function(
  n = 180L,
  seed = 1L,
  source = "a",
  amp = 0.3,
  slope = 0.004,
  curve = 0.5,
  sd = 0.03,
  level = 5
) {
  set.seed(seed)
  weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:(n - 1L))
  tw <- as.numeric(weeks) / 7
  t <- seq_len(n)
  true <- level + slope * t + curve * ((t - n / 2) / n)^2
  seas <- amp *
    sin(2 * pi * tw / ti_period) +
    0.5 * amp * cos(4 * pi * tw / ti_period)
  data.table::data.table(
    source = source,
    week = weeks,
    n = exp(true + seas + stats::rnorm(n, 0, sd)),
    true = true,
    seas = seas
  )
}

# Deterministic trends on the log scale for several sources.
lin_trends <- function(
  n = 104L,
  levels = c(a = 5, b = 4, c = 3),
  slope = 0.003
) {
  weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:(n - 1L))
  data.table::rbindlist(lapply(names(levels), function(s) {
    data.table::data.table(
      source = s,
      week = weeks,
      trend = levels[[s]] + slope * seq_len(n) * match(s, names(levels))
    )
  }))
}

base_2024 <- c("2024-01-01", "2024-12-31")

# 1. extract_trend -----

test_that("llt recovers a known trend and seasonal component", {
  d <- sim_weekly()
  tr <- extract_trend(d, "source", "week", "n")
  expect_s3_class(tr, "data.table")
  expect_identical(
    names(tr),
    c(
      "source",
      "week",
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
  )
  expect_identical(nrow(tr), 180L)
  tr[d, `:=`(true = i.true, seas = i.seas), on = c("source", "week")]
  expect_lt(mean(abs(tr$trend - tr$true)), 0.02)
  expect_lt(max(abs(tr$trend - tr$true)), 0.06)
  expect_gt(stats::cor(tr$seasonal, tr$seas), 0.95)
  expect_equal(
    tr$remainder,
    tr$observed - tr$seasonal - tr$trend,
    tolerance = 1e-12
  )
  expect_true(all(tr$trend_se > 0))
  expect_true(all(tr$trend_lo < tr$trend & tr$trend < tr$trend_hi))
  fits <- attr(tr, "fits")
  expect_identical(nrow(fits), 1L)
  expect_true(fits$K > 0L)
  expect_identical(attr(tr, "transform"), "log")
})

test_that("llt smoothed variance stays positive near the slope boundary", {
  # With the StructTS default diffuse prior (1e4 * var) the smoothed
  # variance lost its significant digits at a (near) zero slope variance
  # and came out negative, i.e. a zero standard error after clamping. The
  # model is rebuilt here with fixed variances, independent of the ML fit.
  for (seed in 1:6) {
    d <- sim_weekly(seed = seed)
    y <- log(d$n)
    for (vs in c(1e-7, 1e-9, 1e-11, 0)) {
      fit <- suppressWarnings(stats::StructTS(stats::ts(y), "trend"))
      mod <- fit$model0
      mod$a <- c(y[1L], 0)
      mod$P[] <- skillviz:::.ti_kappa * stats::var(y)
      mod$Pn[] <- 0
      mod$V <- diag(c(0, vs))
      mod$h <- 7e-4
      v <- stats::KalmanSmooth(y, mod, nit = -1L)$var[, 1L, 1L]
      expect_true(all(v > 0), info = paste(seed, vs))
    }
  }
})

test_that("K selection picks K = 0 without seasonality", {
  d <- sim_weekly(amp = 0, seed = 3L)
  fits <- attr(extract_trend(d, "source", "week", "n"), "fits")
  expect_identical(fits$K, 0L)
  d <- sim_weekly(amp = 0.3, seed = 3L)
  fits <- attr(extract_trend(d, "source", "week", "n"), "fits")
  expect_true(fits$K > 0L)
})

test_that("fixed harmonics are used as given", {
  d <- sim_weekly(n = 120L)
  fits <- attr(extract_trend(d, "source", "week", "n", harmonics = 1L), "fits")
  expect_identical(fits$K, 1L)
})

test_that("missing weeks are completed, smoothed and flagged", {
  d <- sim_weekly(n = 150L)
  d[c(40L, 41L, 100L), n := NA]
  d <- d[-60L]
  tr <- extract_trend(d, "source", "week", "n", harmonics = 2L)
  expect_identical(nrow(tr), 150L)
  expect_identical(which(tr$imputed), c(40L, 41L, 60L, 100L))
  expect_true(all(is.na(tr$observed[tr$imputed])))
  expect_false(anyNA(tr$trend))
  expect_identical(attr(tr, "fits")$n_na, 4L)
  expect_false(attr(tr, "fits")$interpolated)
})

test_that("failed fits are degenerate, boundary variances are not", {
  weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:119)
  # a constant series: the likelihood is not computable, the fit errors
  cst <- data.table::data.table(source = "a", week = weeks, n = 5)
  expect_warning(
    tr <- extract_trend(cst, "source", "week", "n", harmonics = 0L),
    "fit failed"
  )
  fits <- attr(tr, "fits")
  expect_true(fits$degenerate)
  expect_false(is.na(fits$error))
  expect_true(all(is.na(tr$trend)))
  # an exact line: the optimiser does not converge even after the retry
  line <- data.table::data.table(
    source = "a",
    week = weeks,
    n = exp(1 + 0.01 * seq_along(weeks))
  )
  fits <- attr(
    extract_trend(line, "source", "week", "n", harmonics = 0L),
    "fits"
  )
  expect_true(fits$retried)
  expect_identical(fits$degenerate, fits$convergence != 0L)
  # a near-deterministic line: a zero variance is a boundary estimate
  set.seed(1)
  line[, n := n * exp(stats::rnorm(.N, 0, 1e-3))]
  tr <- extract_trend(line, "source", "week", "n", harmonics = 0L)
  fits <- attr(tr, "fits")
  expect_false(fits$degenerate)
  expect_true(fits$boundary)
  expect_true(nzchar(fits$zero_variance))
  expect_identical(fits$convergence, 0L)
  expect_false(anyNA(tr$trend))
})

test_that("a moving sum gets a zero observation variance, not a failure", {
  set.seed(4)
  n_w <- 160L
  weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:(n_w - 1L))
  inflow <- exp(5 + cumsum(stats::rnorm(n_w + 16L, 0, 0.05)))
  stock <- as.numeric(stats::filter(inflow, rep(1, 17L), sides = 1L))
  d <- data.table::data.table(
    source = "a",
    week = weeks,
    s = stock[17L:(n_w + 16L)]
  )
  fits <- attr(extract_trend(d, "source", "week", "s", harmonics = 0L), "fits")
  expect_false(fits$degenerate)
  expect_true(is.na(fits$error))
  expect_identical(fits$boundary, grepl("obs", fits$zero_variance))
})

test_that("leading missing weeks are smoothed, not a failure", {
  d <- sim_weekly(n = 140L)
  d[1:3, n := NA]
  tr <- extract_trend(d, "source", "week", "n")
  fits <- attr(tr, "fits")
  expect_true(is.na(fits$error))
  expect_false(fits$degenerate)
  expect_identical(fits$n_na, 3L)
  expect_false(anyNA(tr$trend))
  tr[d, true := i.true, on = c("source", "week")]
  expect_lt(mean(abs(tr$trend - tr$true)), 0.02)
  expect_lt(max(abs(tr$trend[1:3] - tr$true[1:3])), 0.1)
})

test_that("the second start never lowers the likelihood of the default", {
  real <- .ti_structts
  d <- sim_weekly()
  two <- attr(extract_trend(d, "source", "week", "n", harmonics = 2L), "fits")
  testthat::local_mocked_bindings(.ti_structts = function(x, init = NULL, ...) {
    if (is.null(init)) real(x) else simpleError("off")
  })
  one <- attr(extract_trend(d, "source", "week", "n", harmonics = 2L), "fits")
  expect_lte(two$aicc, one$aicc + 1e-8)
  expect_identical(one$start, "default")
})

test_that("non-convergence triggers one retry; persistent failure is degenerate", {
  real <- .ti_structts
  d <- sim_weekly(n = 120L)
  # first call fails to converge, the retry is the real fit
  calls <- 0L
  testthat::local_mocked_bindings(.ti_structts = function(x, init = NULL, ...) {
    calls <<- calls + 1L
    f <- real(x, init, ...)
    if (is.null(init)) {
      f$code <- 52L
    }
    f
  })
  tr <- extract_trend(d, "source", "week", "n", harmonics = 1L)
  fits <- attr(tr, "fits")
  expect_true(fits$retried)
  expect_identical(fits$start, "rescaled")
  expect_false(fits$degenerate)
  expect_identical(fits$convergence, 0L)
  expect_false(anyNA(tr$trend))
  # the retry on the rescaled series gives the same trend as the direct fit
  testthat::local_mocked_bindings(.ti_structts = real)
  tr0 <- extract_trend(d, "source", "week", "n", harmonics = 1L)
  expect_false(attr(tr0, "fits")$retried)
  expect_equal(tr$trend, tr0$trend, tolerance = 1e-3)
  # both attempts fail to converge: degenerate, components still returned
  testthat::local_mocked_bindings(.ti_structts = function(x, init = NULL, ...) {
    f <- real(x, init, ...)
    f$code <- 52L
    f
  })
  tr <- extract_trend(d, "source", "week", "n", harmonics = 1L)
  fits <- attr(tr, "fits")
  expect_true(fits$retried)
  expect_true(fits$degenerate)
  expect_identical(fits$convergence, 52L)
  expect_true(is.na(fits$error))
  # both attempts error: the key fails
  testthat::local_mocked_bindings(.ti_structts = function(x, init = NULL, ...) {
    simpleError("boom")
  })
  expect_warning(
    tr <- extract_trend(d, "source", "week", "n", harmonics = 1L),
    "fit failed"
  )
  expect_true(attr(tr, "fits")$degenerate)
})

test_that("several keys and logit transform", {
  d <- rbind(
    sim_weekly(n = 110L, source = "a"),
    sim_weekly(n = 110L, seed = 2L, source = "b")
  )
  d[, p := stats::plogis(log(n) - 6)]
  tr <- extract_trend(
    d,
    "source",
    "week",
    "p",
    transform = "logit",
    harmonics = 1L
  )
  expect_identical(nrow(attr(tr, "fits")), 2L)
  expect_equal(tr$observed, stats::qlogis(d$p), tolerance = 1e-12)
})

test_that("ets and stl run", {
  skip_if_not_installed("fable")
  skip_if_not_installed("fabletools")
  skip_if_not_installed("tsibble")
  d <- sim_weekly(n = 150L)
  d[c(30L, 31L), n := NA]
  te <- extract_trend(
    d,
    "source",
    "week",
    "n",
    method = "ets",
    max_harmonics = 2L,
    n_backfit = 1L
  )
  expect_identical(unique(te$method), "ets")
  expect_false(anyNA(te$trend))
  expect_true(all(is.na(te$trend_se)))
  expect_identical(te$trend, te$trend_filtered)
  expect_true(attr(te, "fits")$interpolated)
  fe <- attr(te, "fits")
  expect_false(fe$degenerate)
  expect_false(fe$retried)
  expect_identical(fe$boundary, nzchar(fe$zero_variance))
  tr <- data.table::copy(te)
  tr[d, true := i.true, on = c("source", "week")]
  expect_lt(mean(abs(tr$trend - tr$true)), 0.05)
})

test_that("stl runs and is labelled", {
  d <- sim_weekly(n = 150L)
  d[30L, n := NA]
  ts_ <- extract_trend(d, "source", "week", "n", method = "stl")
  fits <- attr(ts_, "fits")
  expect_identical(fits$K, NA_integer_)
  expect_true(fits$interpolated)
  expect_false(anyNA(ts_$trend))
  expect_true(all(is.na(ts_$trend_filtered)))
})

test_that("extract_trend validates its input", {
  d <- sim_weekly(n = 60L)
  expect_error(extract_trend(as.list(d), "source", "week", "n"), "data.frame")
  expect_error(extract_trend(d, "source", "week", "zz"), "missing required")
  expect_error(extract_trend(d, "source", "week", "n", method = "x"))
  d0 <- data.table::copy(d)[1L, n := 0]
  expect_error(extract_trend(d0, "source", "week", "n"), "strictly positive")
  expect_error(
    extract_trend(d, "source", "week", "n", transform = "logit"),
    "between 0 and 1"
  )
  dd <- rbind(d, d[1L])
  expect_error(extract_trend(dd, "source", "week", "n"), "duplicated")
  d2 <- data.table::copy(d)[2L, week := week + 1L]
  expect_error(extract_trend(d2, "source", "week", "n"), "weekly grid")
  expect_error(
    extract_trend(d, "source", "week", "n", harmonics = 1.5),
    "harmonics"
  )
  expect_error(extract_trend(d, "source", "week", "n", level = 1), "level")
  expect_error(
    extract_trend(d, "source", "week", "n", n_backfit = -1),
    "n_backfit"
  )
  d3 <- data.table::copy(d)[, week := as.character(week)]
  expect_error(extract_trend(d3, "source", "week", "n"), "Date")
  d4 <- data.table::copy(d)[, trend := source]
  expect_error(extract_trend(d4, "trend", "week", "n"), "output names")
})

# 2. trend_diagnostics -----

test_that("trend_diagnostics returns one row per key with the floor ratio", {
  d <- rbind(
    sim_weekly(n = 120L),
    sim_weekly(n = 120L, seed = 5L, source = "b")
  )
  tr <- extract_trend(d, "source", "week", "n", harmonics = 2L)
  tr[d, cnt := i.n, on = c("source", "week")]
  dg <- trend_diagnostics(tr, "source", n_col = "cnt")
  expect_identical(nrow(dg), 2L)
  expect_true(all(
    c(
      "ratio_trend_remainder",
      "var_removed_d1",
      "var_removed_d13",
      "lb_p_value",
      "roughness",
      "remainder_var",
      "noise_floor",
      "ratio_floor"
    ) %in%
      names(dg)
  ))
  floor_a <- 1 / mean(d[source == "a", n])
  expect_equal(dg[source == "a", noise_floor], floor_a, tolerance = 1e-12)
  expect_true(all(dg$lb_p_value >= 0 & dg$lb_p_value <= 1))
  expect_true(all(dg$var_removed_d13 <= 1))
  dg2 <- trend_diagnostics(tr, "source")
  expect_false("noise_floor" %in% names(dg2))
})

test_that("trend_diagnostics validates its input", {
  tr <- extract_trend(
    sim_weekly(n = 60L),
    "source",
    "week",
    "n",
    harmonics = 0L
  )
  expect_error(
    trend_diagnostics(tr, "source", n_col = "zz"),
    "missing required"
  )
  tr2 <- data.table::as.data.table(as.data.frame(tr))
  data.table::setattr(tr2, "time_col", NULL)
  expect_error(trend_diagnostics(tr2, "source"), "time_col")
  expect_error(trend_diagnostics(1, "source"), "data.frame")
})

# 3. trend_revisions -----

test_that("trend_revisions: shape, zero revision at the full-sample origin", {
  d <- sim_weekly(n = 110L, amp = 0)
  o <- d$week[c(100L, 110L)]
  rv <- trend_revisions(
    d,
    "source",
    "week",
    "n",
    origins = o,
    max_lag = 5L,
    scale = 100,
    harmonics = 0L
  )
  expect_identical(nrow(rv), 12L)
  expect_identical(sort(unique(rv$lag)), 0:5)
  expect_identical(rv[origin == o[2L], max(abs(revision))], 0)
  expect_equal(
    rv$revision_index,
    100 * (exp(rv$revision) - 1),
    tolerance = 1e-12
  )
  sm <- attr(rv, "summary")
  expect_identical(sm$lag, 0:5)
  expect_identical(sm$n, rep(2L, 6L))
  expect_true("mean_abs_revision_index" %in% names(sm))
})

test_that("trend_revisions validates its input", {
  d <- sim_weekly(n = 60L)
  expect_error(
    trend_revisions(d, "source", "week", "n", origins = as.IDate("1999-01-04")),
    "origins"
  )
  expect_error(
    trend_revisions(
      d,
      "source",
      "week",
      "n",
      origins = d$week[50L],
      max_lag = -1
    ),
    "max_lag"
  )
  expect_error(
    trend_revisions(d, "source", "week", "n", origins = d$week[50L], scale = 0),
    "scale"
  )
})

# 4. trend_index -----

test_that("trend_index: geometric base mean is exactly scale for log", {
  tr <- lin_trends()
  idx <- trend_index(tr, "source", "week", base_period = base_2024)
  b <- idx[week <= as.IDate("2024-12-31")]
  gm <- b[, list(m = mean(log(indice))), by = source]
  expect_equal(gm$m, rep(log(100), 3L), tolerance = 1e-12)
  expect_identical(names(idx), c("source", "week", "trend", "indice"))
})

test_that("trend_index: logit base mean of shares is exactly scale", {
  tr <- lin_trends()
  tr[, trend := stats::qlogis(0.05 + 0.001 * seq_len(.N)), by = source]
  idx <- trend_index(
    tr,
    "source",
    "week",
    base_period = base_2024,
    transform = "logit"
  )
  b <- idx[week <= as.IDate("2024-12-31")]
  expect_equal(
    b[, mean(indice), by = source]$V1,
    rep(100, 3L),
    tolerance = 1e-12
  )
  p <- stats::plogis(tr$trend)
  expect_equal(
    idx[source == "a", indice],
    100 * p[tr$source == "a"] / mean(p[tr$source == "a"][1:53]),
    tolerance = 1e-12
  )
})

test_that("trend_index accepts a logical base column and warns without base", {
  tr <- lin_trends()
  tr[, base := week < as.IDate("2024-04-01")]
  idx <- trend_index(tr, "source", "week", base_period = "base", scale = 1)
  expect_equal(
    idx[tr$base, mean(log(indice)), by = source]$V1,
    rep(0, 3L),
    tolerance = 1e-12
  )
  expect_warning(
    trend_index(
      tr,
      "source",
      "week",
      base_period = c("2030-01-01", "2030-12-31")
    ),
    "base period"
  )
})

test_that("trend_index validates its input", {
  tr <- lin_trends()
  expect_error(
    trend_index(tr, "source", "week", base_period = "nope"),
    "not found"
  )
  expect_error(
    trend_index(
      tr,
      "source",
      "week",
      base_period = c("2024-12-31", "2024-01-01")
    ),
    "two dates"
  )
  expect_error(
    trend_index(tr, "source", "week", base_2024, scale = -1),
    "scale"
  )
  expect_error(
    trend_index(tr, "source", "week", base_2024, value_col = "x"),
    "missing"
  )
})

# 5. aggregate_index -----

test_that("aggregate_index of identical sources equals the source index", {
  tr <- lin_trends(levels = c(a = 5, b = 5))
  tr[, trend := 5 + 0.003 * seq_len(.N), by = source]
  idx <- trend_index(tr, "source", "week", base_2024)
  w <- data.table::data.table(source = c("a", "b"), w = c(3, 1))
  for (tp in c("geometric", "arithmetic")) {
    ag <- aggregate_index(
      idx,
      "source",
      time_col = "week",
      weights = w,
      type = tp
    )
    expect_equal(ag$indice, idx[source == "a", indice], tolerance = 1e-12)
  }
  expect_equal(ag$covered_weight, rep(1, nrow(ag)), tolerance = 1e-12)
  expect_equal(
    ag$n_eff,
    rep(1 / (0.75^2 + 0.25^2), nrow(ag)),
    tolerance = 1e-12
  )
})

test_that("aggregate_index renormalises weights over present sources", {
  idx <- data.table::data.table(
    source = c("a", "b", "c", "a", "b", "c"),
    week = data.table::as.IDate(rep(c("2024-01-01", "2024-01-08"), each = 3L)),
    indice = c(100, 110, 120, 100, NA, 120)
  )
  w <- data.table::data.table(source = c("a", "b", "c"), w = c(2, 1, 1))
  ag <- aggregate_index(
    idx,
    "source",
    time_col = "week",
    weights = w,
    type = "arithmetic"
  )
  expect_equal(
    ag$indice[1L],
    0.5 * 100 + 0.25 * 110 + 0.25 * 120,
    tolerance = 1e-12
  )
  expect_equal(ag$indice[2L], (2 / 3) * 100 + (1 / 3) * 120, tolerance = 1e-12)
  expect_identical(ag$n_sources, c(3L, 2L))
  expect_equal(ag$covered_weight, c(1, 0.75), tolerance = 1e-12)
  ag_g <- aggregate_index(idx, "source", time_col = "week", weights = w)
  expect_equal(
    ag_g$indice[2L],
    exp((2 / 3) * log(100) + (1 / 3) * log(120)),
    tolerance = 1e-12
  )
})

test_that("cap water-filling keeps weights below the cap and summing to 1", {
  wf <- skillviz:::.ti_waterfill(c(0.9, 0.05, 0.03, 0.02), 0.3)
  expect_equal(sum(wf), 1, tolerance = 1e-12)
  expect_true(all(wf <= 0.3 + 1e-12))
  expect_equal(wf, c(0.3, 0.3, 0.24, 0.16), tolerance = 1e-12)
  expect_equal(skillviz:::.ti_waterfill(c(0.6, 0.4), 0.3), c(0.5, 0.5))
  idx <- data.table::data.table(
    source = c("a", "b", "c", "d"),
    week = data.table::as.IDate("2024-01-01"),
    indice = c(100, 200, 300, 400)
  )
  w <- data.table::data.table(
    source = c("a", "b", "c", "d"),
    w = c(90, 5, 3, 2)
  )
  ag <- aggregate_index(
    idx,
    "source",
    time_col = "week",
    weights = w,
    type = "arithmetic",
    cap = 0.3
  )
  expect_equal(ag$indice, sum(wf * c(100, 200, 300, 400)), tolerance = 1e-12)
  expect_equal(ag$n_eff, 1 / sum(wf^2), tolerance = 1e-12)
})

test_that("aggregate_index validates its input", {
  idx <- data.table::data.table(
    source = "a",
    week = as.IDate("2024-01-01"),
    indice = 100
  )
  w <- data.table::data.table(source = "a", w = 1)
  expect_error(
    aggregate_index(idx, "source", time_col = "week", weights = w, cap = 0),
    "cap"
  )
  expect_error(
    aggregate_index(
      idx,
      "source",
      time_col = "week",
      weights = data.table::data.table(source = "a", w = -1)
    ),
    "non-negative"
  )
  expect_error(
    aggregate_index(
      idx[, indice := -1],
      "source",
      time_col = "week",
      weights = w
    ),
    "strictly positive"
  )
  expect_warning(
    aggregate_index(
      data.table::data.table(
        source = "z",
        week = as.IDate("2024-01-01"),
        indice = 100
      ),
      "source",
      time_col = "week",
      weights = w
    ),
    "without weight"
  )
  expect_error(
    aggregate_index(
      idx,
      "source",
      time_col = "week",
      weights = w[, list(source)]
    ),
    "missing"
  )
})

# 6. compose_skill_index -----

test_that("compose_skill_index factorises exactly with one source", {
  tot <- lin_trends(levels = c(a = 5))
  sh <- data.table::data.table(
    source = "a",
    skill = "k1",
    week = tot$week,
    trend = stats::qlogis(0.1 + 0.0007 * seq_len(nrow(tot)))
  )
  w <- data.table::data.table(source = "a", w = 1)
  cs <- compose_skill_index(tot, sh, "source", "skill", "week", w, base_2024)
  i_tot <- trend_index(tot, "source", "week", base_2024)
  sh_log <- data.table::copy(sh)[, trend := stats::plogis(trend, log.p = TRUE)]
  i_sh <- trend_index(sh_log, "source", "week", base_2024)
  expect_equal(cs$indice, i_tot$indice * i_sh$indice / 100, tolerance = 1e-10)
  bs <- attr(cs, "by_source")
  expect_equal(bs$indice, cs$indice, tolerance = 1e-12)
  expect_identical(nrow(attr(cs, "dropped")), 0L)
})

test_that("compose_skill_index drops poorly covered skills", {
  tot <- lin_trends()
  sh <- tot[, list(source, skill = "k1", week, trend = stats::qlogis(0.2))]
  sh2 <- sh[source == "c"][, skill := "k2"]
  w <- data.table::data.table(source = c("a", "b", "c"), w = c(5, 3, 2))
  cs <- compose_skill_index(
    tot,
    rbind(sh, sh2),
    "source",
    "skill",
    "week",
    w,
    base_2024
  )
  expect_identical(unique(cs$skill), "k1")
  dr <- attr(cs, "dropped")
  expect_identical(dr$skill, "k2")
  expect_equal(dr$covered_weight, 0.2, tolerance = 1e-12)
  # constant shares: skill index equals the aggregated total index
  ag <- aggregate_index(
    trend_index(tot, "source", "week", base_2024),
    "source",
    time_col = "week",
    weights = w
  )
  expect_equal(cs$indice, ag$indice, tolerance = 1e-10)
})

test_that("compose_skill_index validates its input", {
  tot <- lin_trends()
  sh <- tot[, list(source, skill = "k1", week, trend = 0)]
  w <- data.table::data.table(source = c("a", "b", "c"), w = 1)
  expect_error(
    compose_skill_index(
      tot,
      sh,
      "source",
      "skill",
      "week",
      w,
      base_2024,
      min_covered = 2
    ),
    "min_covered"
  )
  expect_error(
    compose_skill_index(
      tot,
      sh[, !"skill"],
      "source",
      "skill",
      "week",
      w,
      base_2024
    ),
    "missing"
  )
  expect_error(
    compose_skill_index(
      rbind(tot, tot),
      sh,
      "source",
      "skill",
      "week",
      w,
      base_2024
    ),
    "duplicated"
  )
})

# 7. index_jackknife -----

test_that("jackknife range contains the full index", {
  tr <- lin_trends()
  set.seed(4)
  tr[, trend := trend + stats::rnorm(.N, 0, 0.02)]
  idx <- trend_index(tr, "source", "week", base_2024)
  w <- data.table::data.table(source = c("a", "b", "c"), w = c(5, 3, 2))
  jk <- index_jackknife(idx, "source", NULL, "week", w)
  expect_true(all(jk$jk_min <= jk$indice + 1e-9))
  expect_true(all(jk$jk_max >= jk$indice - 1e-9))
  expect_true(all(jk$jk_source %in% c("a", "b", "c")))
  ag <- aggregate_index(idx, "source", time_col = "week", weights = w)
  expect_equal(jk$indice, ag$indice, tolerance = 1e-12)
  loo_c <- aggregate_index(
    idx[source != "c"],
    "source",
    time_col = "week",
    weights = w
  )
  r <- jk$jk_source == "c"
  expect_equal(jk$jk_shift[r], (loo_c$indice - ag$indice)[r], tolerance = 1e-12)
})

test_that("index_jackknife validates its input", {
  idx <- data.table::data.table(
    source = "a",
    week = as.IDate("2024-01-01"),
    indice = 100
  )
  w <- data.table::data.table(source = "a", w = 1)
  expect_error(index_jackknife(idx, "source", NULL, "week", w), "two sources")
})

# 8. trend_concordance -----

test_that("concordance is 1 when all sources share the trend", {
  weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:99)
  common <- sin(seq_len(100) / 8)
  tr <- data.table::data.table(
    source = rep(c("a", "b", "c"), each = 100L),
    week = rep(weeks, 3L),
    trend = c(common + 1, common + 2, 2 * common)
  )
  cc <- trend_concordance(tr, "source", "week", lags = c(4L, 13L), n_perm = 30L)
  expect_identical(nrow(cc), 4L)
  expect_equal(
    cc[statistic == "concordance", observed],
    c(1, 1),
    tolerance = 1e-12
  )
  expect_equal(
    cc[statistic == "pc1_share", observed],
    c(1, 1),
    tolerance = 1e-12
  )
  expect_true(all(cc[statistic == "concordance", placebo_mean] < 1))
  bt <- attr(cc, "by_time")
  expect_identical(names(bt), c("week", "lag", "concordance", "n_sources"))
  expect_identical(nrow(bt), (100L - 4L) + (100L - 13L))
  cc2 <- trend_concordance(tr, "source", "week", lags = 4L, n_perm = 30L)
  expect_identical(
    cc2$placebo_mean,
    trend_concordance(
      tr,
      "source",
      "week",
      lags = 4L,
      n_perm = 30L
    )$placebo_mean
  )
})

test_that("weighted concordance follows the weighted majority", {
  weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:19)
  tr <- data.table::data.table(
    source = rep(c("a", "b", "c"), each = 20L),
    week = rep(weeks, 3L),
    trend = c(1:20, 1:20, 20:1)
  )
  w <- data.table::data.table(source = c("a", "b", "c"), w = c(1, 1, 6))
  cc <- trend_concordance(
    tr,
    "source",
    "week",
    lags = 1L,
    weights = w,
    n_perm = 0L
  )
  expect_equal(
    cc[statistic == "concordance", observed],
    0.75,
    tolerance = 1e-12
  )
  expect_identical(cc$n_perm, c(0L, 0L))
})

test_that("trend_concordance validates its input", {
  weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:19)
  tr <- data.table::data.table(source = "a", week = weeks, trend = 1:20)
  expect_error(
    trend_concordance(tr, "source", "week", lags = 1L),
    "two sources"
  )
  tr2 <- rbind(tr, data.table::copy(tr)[, source := "b"])
  expect_error(trend_concordance(tr2, "source", "week", lags = 0L), "lags")
  expect_error(trend_concordance(tr2, "source", "week", lags = 30L), "shorter")
  expect_error(
    trend_concordance(rbind(tr2, tr), "source", "week", lags = 1L),
    "one row"
  )
})

# 9. smooth trend model and arithmetic skill composition -----

# Smooth known trend plus heavy Gaussian noise on the log scale.
sim_smooth <- function(seed, n = 174L, sd = 0.25) {
  set.seed(seed)
  weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:(n - 1L))
  t <- seq_len(n)
  true <- 5 + 0.002 * t + 0.4 * sin(2 * pi * t / 120)
  data.table::data.table(
    source = "a",
    week = weeks,
    n = exp(true + stats::rnorm(n, 0, sd)),
    true = true
  )
}

test_that("smooth recovers a smooth trend better than llt on noisy series", {
  rmse <- vapply(
    1:10,
    function(seed) {
      d <- sim_smooth(seed)
      tl <- extract_trend(d, "source", "week", "n", harmonics = 0L)
      ts_ <- extract_trend(
        d,
        "source",
        "week",
        "n",
        method = "smooth",
        harmonics = 0L
      )
      c(
        llt = sqrt(mean((tl$trend - d$true)^2)),
        smooth = sqrt(mean((ts_$trend - d$true)^2))
      )
    },
    numeric(2)
  )
  expect_true(mean(rmse["smooth", ]) < mean(rmse["llt", ]))
  # the smooth trend is never rougher than the llt one
  d <- sim_smooth(3L)
  tl <- extract_trend(d, "source", "week", "n", harmonics = 0L)
  ts_ <- extract_trend(d, "source", "week", "n", method = "smooth", harmonics = 0L)
  expect_true(
    stats::var(diff(ts_$trend, differences = 2L)) <=
      stats::var(diff(tl$trend, differences = 2L))
  )
})

test_that("smooth reports the level variance as fixed, not as boundary", {
  d <- sim_smooth(1L)
  ts_ <- extract_trend(d, "source", "week", "n", method = "smooth", harmonics = 0L)
  f <- attr(ts_, "fits")
  expect_identical(f$method, "smooth")
  expect_identical(f$var_level, 0)
  expect_identical(f$fixed_variance, "level")
  expect_false(grepl("level", f$zero_variance))
  expect_true(f$var_slope > 0)
  expect_true(f$var_obs > 0)
  expect_false(f$boundary)
  expect_false(f$degenerate)
  expect_true(all(ts_$trend_se > 0))
  expect_true(all(ts_$trend_lo < ts_$trend & ts_$trend < ts_$trend_hi))
  # llt keeps an empty fixed_variance
  fl <- attr(extract_trend(d, "source", "week", "n", harmonics = 0L), "fits")
  expect_identical(fl$fixed_variance, "")
})

test_that("smooth handles missing weeks and seasonality like llt", {
  d <- sim_weekly(seed = 4L, sd = 0.05)
  d[c(1L, 2L, 60L, 61L), n := NA_real_]
  ts_ <- extract_trend(d, "source", "week", "n", method = "smooth", max_harmonics = 3L)
  f <- attr(ts_, "fits")
  expect_true(is.na(f$error))
  expect_false(f$interpolated)
  expect_identical(f$n_na, 4L)
  expect_true(f$K > 0L)
  expect_false(anyNA(ts_$trend))
  expect_true(sqrt(mean((ts_$trend - d$true)^2)) < 0.05)
})

test_that("trend_revisions passes method = 'smooth' through", {
  d <- sim_smooth(2L, n = 120L)
  rv <- trend_revisions(
    d[, list(source, week, n)],
    "source",
    "week",
    "n",
    origins = d$week[c(110L, 120L)],
    max_lag = 3L,
    method = "smooth",
    harmonics = 0L
  )
  expect_equal(rv[origin == max(origin), revision], rep(0, 4L), tolerance = 1e-8)
})

test_that("compose_skill_index arithmetic matches a hand computation", {
  tot <- lin_trends()
  n <- nrow(tot) / 3L
  sh <- data.table::data.table(
    source = tot$source,
    skill = "k1",
    week = tot$week,
    trend = stats::qlogis(
      rep(c(0.10, 0.20, 0.05), each = n) + 0.0005 * rep(seq_len(n), 3L)
    )
  )
  # cell (c, k1) not modelled in the last 10 weeks
  last <- sort(unique(sh$week), decreasing = TRUE)[1:10]
  sh[source == "c" & week %in% last, trend := NA_real_]
  w <- data.table::data.table(source = c("a", "b", "c"), w = c(5, 3, 2))
  ca <- compose_skill_index(
    tot,
    sh,
    "source",
    "skill",
    "week",
    w,
    base_2024,
    type = "arithmetic"
  )
  cg <- compose_skill_index(tot, sh, "source", "skill", "week", w, base_2024)
  bs <- attr(ca, "by_source")
  # per-source indices do not depend on the aggregation
  expect_equal(bs$indice, attr(cg, "by_source")$indice, tolerance = 1e-12)
  # hand computation: weights renormalised over the modelled cells
  h <- bs[!is.na(indice)]
  h[w, wt := i.w, on = "source"]
  h <- h[, list(hand = sum(wt * indice) / sum(wt), n = .N), keyby = week]
  expect_equal(ca$indice, h$hand, tolerance = 1e-12)
  expect_identical(ca$n_sources, h$n)
  expect_equal(ca[week %in% last, unique(covered_weight)], 0.8)
  # arithmetic mean >= geometric mean in every week
  expect_true(all(ca$indice >= cg$indice - 1e-9))
  expect_error(
    compose_skill_index(tot, sh, "source", "skill", "week", w, base_2024, type = "x"),
    "should be one of"
  )
})

test_that("index_jackknife passes type through to every aggregate", {
  weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:3)
  idx <- data.table::data.table(
    source = rep(c("a", "b", "c"), each = 4),
    week = rep(weeks, 3),
    indice = c(100, 120, 140, 160, 100, 90, 80, 70, 100, 100, 101, 101)
  )
  w <- data.table::data.table(source = c("a", "b", "c"), w = c(3, 1, 1))
  ja <- index_jackknife(idx, "source", NULL, "week", w, type = "arithmetic")
  ag <- aggregate_index(idx, "source", time_col = "week", weights = w, type = "arithmetic")
  expect_equal(ja$indice, ag$indice, tolerance = 1e-12)
  # leave out "a": arithmetic mean of b and c with equal weights
  sub <- idx[source != "a"]
  manual <- sub[, list(v = mean(indice)), keyby = week]$v
  expect_true(all(ja$jk_min <= manual + 1e-12 & manual <= ja$jk_max + 1e-12))
  expect_true(all(ja$jk_min <= ja$indice & ja$indice <= ja$jk_max))
  jg <- index_jackknife(idx, "source", NULL, "week", w)
  expect_false(isTRUE(all.equal(ja$jk_max, jg$jk_max)))
})
