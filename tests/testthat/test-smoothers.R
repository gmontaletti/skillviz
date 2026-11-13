# Tests for smoothers.R -----

# 1. henderson_weights -----

test_that("Henderson weights sum to one, are symmetric and match X-11", {
  for (H in c(5L, 13L, 23L, 39L)) {
    w <- henderson_weights(H)
    expect_identical(length(w), H)
    expect_equal(sum(w), 1, tolerance = 1e-12)
    expect_equal(w, rev(w), tolerance = 1e-14)
  }
  # published 13-term weights
  expect_equal(
    henderson_weights(13L)[1:7],
    c(-0.01935, -0.02786, 0, 0.06549, 0.14736, 0.21434, 0.24006),
    tolerance = 1e-4
  )
  expect_equal(henderson_weights(3L), c(0, 1, 0), tolerance = 1e-14)
})

test_that("Henderson weights reproduce a cubic in the interior", {
  x <- seq_len(80)
  cubic <- 2 + 0.3 * x - 0.02 * x^2 + 0.0004 * x^3
  h <- henderson_filter(cubic, length = 23L, ic_ratio = 1)
  inner <- 12:69
  expect_equal(h$trend[inner], cubic[inner], tolerance = 1e-10)
})

test_that("henderson_weights rejects even or short lengths", {
  expect_error(henderson_weights(12), "odd integers")
  expect_error(henderson_weights(1), "odd integers")
  expect_error(henderson_weights(c(13, 23)), "single odd")
})

# 2. Musgrave end weights -----

test_that("Musgrave end weights sum to one and match X-11", {
  w <- henderson_weights(13L)
  for (q in 0:5) {
    for (R in c(0.5, 3.5, 10)) {
      expect_equal(sum(skillviz:::.hd_musgrave(w, q, R)), 1, tolerance = 1e-12)
    }
  }
  # X-11 last-point weights of the 13-term filter, R = 3.5
  expect_equal(
    skillviz:::.hd_musgrave(w, 0L, 3.5),
    c(-0.09186, -0.05811, 0.01202, 0.11977, 0.24390, 0.35315, 0.42113),
    tolerance = 1e-4
  )
})

test_that("Musgrave end weights reproduce a straight line as R -> 0", {
  x <- 3 + 0.5 * seq_len(60)
  h <- henderson_filter(x, length = 13L, ic_ratio = 1e-8)
  expect_equal(h$trend, x, tolerance = 1e-8)
  # with R > 0 the slope is a penalty: ends are biased towards the mean
  h35 <- henderson_filter(x, length = 13L, ic_ratio = 3.5)
  expect_gt(abs(h35$trend[60] - x[60]), 1e-3)
  expect_equal(h35$trend[7:54], x[7:54], tolerance = 1e-10)
})

# 3. henderson_filter -----

test_that("henderson_filter chooses a length by GCV and reports the path", {
  set.seed(1)
  x <- sin(seq(0, 4 * pi, length.out = 120)) + stats::rnorm(120, 0, 0.2)
  h <- henderson_filter(x)
  expect_true(h$length %in% c(13L, 23L, 39L))
  expect_identical(h$path$length, c(13L, 23L, 39L))
  expect_equal(h$gcv, min(h$path$gcv), tolerance = 1e-12)
  expect_length(h$trend, 120L)
  expect_gt(h$ic_ratio, 0)
  # longer filters have fewer degrees of freedom
  expect_true(all(diff(h$path$edf) < 0))
  # lengths longer than the series are skipped
  h2 <- henderson_filter(x[1:30])
  expect_identical(h2$path$length, c(13L, 23L))
})

test_that("henderson_filter validates its input", {
  expect_error(henderson_filter(c(1, NA, 3:20)), "fill missing")
  expect_error(henderson_filter("a"), "numeric")
  expect_error(henderson_filter(1:10, length = 13L), "shorter")
  expect_error(henderson_filter(1:30, length = 14L), "odd")
  expect_error(henderson_filter(1:30, ic_ratio = -1), "ic_ratio")
})

# 4. whittaker_smooth -----

test_that("Whittaker tends to y as lambda -> 0 and to a line as lambda -> Inf", {
  set.seed(2)
  y <- cumsum(stats::rnorm(100, 0, 0.1)) + stats::rnorm(100, 0, 0.3)
  z0 <- whittaker_smooth(y, lambda = 1e-8)
  expect_equal(z0$trend, y, tolerance = 1e-6)
  zi <- whittaker_smooth(y, lambda = 1e12)
  tt <- seq_along(y)
  line <- stats::fitted(stats::lm(y ~ tt))
  expect_equal(zi$trend, unname(line), tolerance = 1e-4)
  expect_equal(zi$edf, 2, tolerance = 1e-3)
})

test_that("Whittaker: zero weight equals NA, GCV picks from the grid", {
  set.seed(3)
  y <- cumsum(stats::rnorm(120, 0, 0.1)) + stats::rnorm(120, 0, 0.3)
  ya <- y
  ya[c(10, 50, 51)] <- NA
  yb <- y
  yb[c(10, 50, 51)] <- 1e3
  wb <- rep(1, 120)
  wb[c(10, 50, 51)] <- 0
  a <- whittaker_smooth(ya, lambda = 100)
  b <- whittaker_smooth(yb, lambda = 100, weights = wb)
  expect_equal(a$trend, b$trend, tolerance = 1e-10)
  expect_equal(a$edf, b$edf, tolerance = 1e-10)
  g <- whittaker_smooth(ya)
  expect_identical(nrow(g$path), 91L)
  expect_equal(g$gcv, min(g$path$gcv), tolerance = 1e-12)
  expect_true(g$edf > 2 && g$edf < 120)
})

test_that("whittaker_smooth validates its input", {
  expect_error(whittaker_smooth("a"), "numeric")
  expect_error(whittaker_smooth(c(1, NA, NA)), "positive weight")
  expect_error(whittaker_smooth(1:10, lambda = -1), "lambda")
  expect_error(whittaker_smooth(1:10, weights = rep(-1, 10)), "weights")
  expect_error(whittaker_smooth(1:10, order = 0), "order")
})

# 5. robust_smooth: synthetic data -----

rs_period <- 365.25 / 7

# Weekly counts: smooth true trend (log scale) + annual seasonality + noise,
# with spikes of +3 log units at weeks 30, 75, 140 and a 6-week trough of
# -2 log units at weeks 100-105.
sim_rs <- function(n = 191L, seed = 1L, sd = 0.05, spikes = c(30, 75, 140),
                   trough = 100:105, depth = 2, source = "a") {
  set.seed(seed)
  weeks <- data.table::as.IDate("2023-01-02") + 7L * (0:(n - 1L))
  tw <- as.numeric(weeks) / 7
  t <- seq_len(n)
  true <- 7 + 0.002 * t + 0.25 * sin(2 * pi * t / 160)
  seas <- 0.2 * sin(2 * pi * tw / rs_period) + 0.1 * cos(4 * pi * tw / rs_period)
  y <- true + seas + stats::rnorm(n, 0, sd)
  spikes <- spikes[spikes <= n]
  y[spikes] <- y[spikes] + 3
  y[trough] <- y[trough] - depth
  data.table::data.table(
    source = source,
    week = weeks,
    n = round(expm1(y)),
    true = log1p(round(expm1(true)))
  )
}

rs_rmse <- function(a, b) sqrt(mean((a - b)^2))

test_that("spikes and a trough are flagged, typed, and re-smoothing helps", {
  d <- sim_rs()
  cases <- list(
    whittaker = list(method = "whittaker", band = "model"),
    smooth_mad = list(method = "smooth", band = "mad"),
    smooth_t = list(method = "smooth_t", band = "model")
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    rs <- robust_smooth(d, "source", "week", "n", method = cs$method,
      band = cs$band)
    info <- paste("case", nm)
    expect_true(all(rs$flag[c(30, 75, 140)]), info = info)
    expect_identical(rs$flag_type[c(30, 75, 140)], rep("isolated", 3L),
      info = info)
    expect_true(all(rs$flag[100:105]), info = info)
    expect_identical(unique(rs$flag_type[100:105]), "interruption",
      info = info)
    expect_true(all(rs$excluded[c(30, 75, 140, 100:105)]), info = info)
    if (nm != "smooth_t") {
      # smooth_t is already robust in pass 1: no improvement required
      expect_lt(rs_rmse(rs$trend, d$true), rs_rmse(rs$trend_pass1, d$true))
    }
    expect_lt(rs_rmse(rs$trend, d$true), 0.05)
    fl <- attr(rs, "flags")
    expect_identical(sum(fl$flag_type == "interruption"), 1L, info = info)
    expect_identical(fl[flag_type == "interruption", n_weeks], 6L)
  }
})

test_that("flag types follow the run rules", {
  n <- 60L
  sa <- rep(0, n)
  sa[31:n] <- 2 # level shift after week 30
  half <- rep(0.5, n)
  fl <- rep(FALSE, n)
  fl[5] <- TRUE # isolated
  fl[10:15] <- TRUE # interruption: same level before and after
  fl[27:32] <- TRUE # level shift across the run
  fl[58:59] <- TRUE # touches the last 4 weeks
  fr <- skillviz:::.rs_flag_runs(fl, sa, half, run_min = 4L)
  expect_identical(
    fr$runs$flag_type,
    c("isolated", "interruption", "level_shift", "final")
  )
  expect_identical(fr$runs$n_weeks, c(1L, 6L, 6L, 2L))
  expect_equal(fr$runs$shift[3], 2, tolerance = 1e-12)
  expect_identical(fr$type[10], "interruption")
  expect_true(is.na(fr$type[1]))
  # shorter than run_min but longer than 3 is isolated when run_min = 8
  fr8 <- skillviz:::.rs_flag_runs(fl, sa, half, run_min = 8L)
  expect_identical(fr8$runs$flag_type[2], "isolated")
})

test_that("the base-R Kalman smoother reproduces KalmanSmooth of 'smooth'", {
  set.seed(4)
  x <- 5 + cumsum(cumsum(stats::rnorm(150, 0, 0.002))) +
    stats::rnorm(150, 0, 0.05)
  x[c(20, 60:65)] <- NA
  f <- skillviz:::.ti_trend_llt(x, smooth = TRUE)
  k <- skillviz:::.rs_kf_smooth(x, rep(f$var_obs, 150), f$var_slope)
  expect_equal(k$level, f$trend, tolerance = 1e-8)
  # week 1 differs: KalmanSmooth loses precision there under the diffuse
  # start; the base-R smoother uses the filtered (cancellation-free) form
  expect_equal(sqrt(k$var_level)[-1], f$trend_se[-1], tolerance = 1e-6)
})

test_that("every method runs and returns the documented columns", {
  d <- sim_rs(n = 130L, spikes = 40, trough = integer(0))
  methods <- c("henderson", "loess", "gam_ar1", "whittaker", "level",
    "llt", "smooth", "smooth_t")
  for (m in methods) {
    if (m == "gam_ar1") {
      skip_if_not_installed("mgcv")
    }
    rs <- robust_smooth(d, "source", "week", "n", method = m,
      harmonics = 2L, n_backfit = 2L)
    expect_identical(
      names(rs),
      c("source", "week", skillviz:::.rs_out_cols),
      info = m
    )
    expect_identical(nrow(rs), 130L, info = m)
    expect_false(anyNA(rs$trend), info = m)
    expect_false(anyNA(rs$band_lo), info = m)
    expect_true(all(rs$band_lo < rs$band_hi), info = m)
    expect_identical(unique(rs$method), m, info = m)
    expect_true(rs$flag[40], info = m)
    fits <- attr(rs, "fits")
    expect_true(all(fits$K == 2L), info = m)
    expect_identical(sum(fits$final), 1L, info = m)
    expect_true(all(is.na(fits$error)), info = m)
    kalman <- m %in% c("level", "llt", "smooth", "smooth_t", "gam_ar1")
    expect_identical(!anyNA(rs$trend_se), kalman, info = m)
    band <- if (m %in% c("level", "llt", "smooth", "smooth_t")) "model" else
      "mad"
    expect_identical(unique(fits$band), band, info = m)
  }
})

test_that("log1p handles zero weeks and is recorded as a log trend", {
  d <- sim_rs(n = 120L, spikes = integer(0), trough = integer(0))
  d[c(10, 50, 51), n := 0]
  rs <- robust_smooth(d, "source", "week", "n", method = "smooth",
    harmonics = 1L)
  expect_identical(rs$observed[c(10, 50, 51)], c(0, 0, 0))
  expect_true(all(rs$flag[c(10, 50, 51)]))
  expect_identical(attr(rs, "transform"), "log")
  expect_identical(attr(rs, "observed_transform"), "log1p")
  expect_error(
    robust_smooth(d, "source", "week", "n", transform = "log"),
    "strictly positive"
  )
})

test_that("max_iter and flag_na control the passes", {
  d <- sim_rs(n = 150L, trough = 90:95)
  r0 <- robust_smooth(d, "source", "week", "n", method = "smooth_t",
    harmonics = 2L, max_iter = 0L)
  expect_identical(unique(r0$pass), 1L)
  expect_true(all(is.na(r0$flag_2)))
  expect_identical(r0$flag, r0$flag_1)
  expect_false(any(r0$excluded))
  expect_equal(r0$trend, r0$trend_pass1, tolerance = 1e-12)
  ri <- robust_smooth(d, "source", "week", "n", method = "smooth_t",
    harmonics = 2L, flag_na = "isolated")
  expect_true(all(ri$excluded[c(30, 75, 140)]))
  expect_false(any(ri$excluded[90:95]))
  expect_true(all(ri$flag_1[90:95]))
  fits <- attr(ri, "fits")
  expect_identical(fits$pass, seq_len(nrow(fits)))
  expect_true(all(fits$iterations >= 1L & fits$iterations <= 20L))
})

test_that("output feeds trend_index, aggregate_index and trend_diagnostics", {
  d <- rbind(
    sim_rs(n = 120L, source = "a", trough = integer(0)),
    sim_rs(n = 120L, seed = 2L, source = "b", spikes = 60,
      trough = integer(0))
  )
  rs <- robust_smooth(d, "source", "week", "n", method = "whittaker",
    harmonics = 2L)
  fits <- attr(rs, "fits")
  expect_identical(sort(unique(fits$source)), c("a", "b"))
  idx <- trend_index(rs, "source", "week",
    base_period = c("2023-01-02", "2023-12-31"), transform = "log")
  base <- idx$week <= as.IDate("2023-12-31")
  gm <- idx[base, exp(mean(log(indice))), by = source]$V1
  expect_equal(gm, c(100, 100), tolerance = 1e-10)
  w <- data.table::data.table(source = c("a", "b"), w = c(2, 1))
  agg <- aggregate_index(idx, "source", time_col = "week", weights = w,
    type = "arithmetic")
  expect_identical(nrow(agg), 120L)
  rs[d, n := i.n, on = c("source", "week")]
  dg <- trend_diagnostics(rs, "source", n_col = "n")
  expect_identical(dg$source, c("a", "b"))
  expect_identical(dg$method, c("whittaker", "whittaker"))
  expect_true(all(is.finite(dg$ratio_floor)))
})

test_that("robust_smooth validates its input and isolates failed keys", {
  d <- sim_rs(n = 60L, spikes = integer(0), trough = integer(0))
  expect_error(robust_smooth(d, "source", "week", "n", method = "x"),
    "should be one of")
  expect_error(robust_smooth(d, "source", "week", "zz"), "missing required")
  d2 <- data.table::copy(d)[, n := as.character(n)]
  expect_error(robust_smooth(d2, "source", "week", "n"), "numeric")
  d3 <- data.table::copy(d)[1, n := -1]
  expect_error(robust_smooth(d3, "source", "week", "n"), "non-negative")
  expect_error(robust_smooth(rbind(d, d[1]), "source", "week", "n"),
    "duplicated")
  expect_error(robust_smooth(d, "source", "week", "n", z = 0), "`z`")
  expect_error(robust_smooth(d, "source", "week", "n", max_iter = -1),
    "max_iter")
  d4 <- data.table::copy(d)[, trend := source]
  expect_error(robust_smooth(d4, "trend", "week", "n"), "output names")
  short <- rbind(d, sim_rs(n = 6L, source = "b", spikes = integer(0),
    trough = integer(0)))
  expect_warning(
    rs <- robust_smooth(short, "source", "week", "n", method = "smooth",
      harmonics = 1L),
    "failed for 1 key"
  )
  expect_true(all(is.na(rs[source == "b", trend])))
  expect_false(anyNA(rs[source == "a", trend]))
  expect_match(attr(rs, "fits")[source == "b", error], "fewer than 8")
})

test_that("model band is undefined, not NaN, when the observation variance is 0", {
  # a noiseless random walk: the ML local level has var_obs = 0
  set.seed(5)
  n <- 120L
  d <- data.table::data.table(
    source = "rw",
    week = data.table::as.IDate("2023-01-02") + 7L * (0:(n - 1L)),
    n = 6 + cumsum(stats::rnorm(n, 0, 0.1))
  )
  for (m in c("level", "llt")) {
    expect_warning(
      rs <- robust_smooth(d, "source", "week", "n", method = m,
        harmonics = 0L, transform = "none"),
      "band is undefined.*rw",
      info = m
    )
    fits <- attr(rs, "fits")
    expect_equal(fits$var_obs, 0, tolerance = 1e-10, info = m)
    expect_identical(fits$band_defined, FALSE, info = m)
    expect_identical(fits$converged, NA, info = m)
    expect_false(any(is.nan(rs$band_lo)), info = m)
    expect_true(all(is.na(rs$band_lo) & is.na(rs$band_hi)), info = m)
    expect_true(all(is.na(rs$flag)), info = m)
    expect_identical(nrow(attr(rs, "flags")), 0L, info = m)
    expect_false(anyNA(rs$trend), info = m)
  }
  # a defined band is reported as such and gives no warning
  d2 <- sim_rs(n = 120L, spikes = integer(0), trough = integer(0))
  expect_no_warning(
    rs2 <- robust_smooth(d2, "source", "week", "n", method = "level",
      harmonics = 1L)
  )
  expect_true(all(attr(rs2, "fits")$band_defined))
})

# 6. robust_smooth: smooth_t with q profile and damping -----

test_that("defaults reproduce the output computed before q profile and damping", {
  # reference values computed at commit 2d84a50 (before the new arguments).
  # smooth_t and smooth come from StructTS maximum likelihood fits whose
  # optimum is flat: changing the optimiser start moves the trend by 5e-6,
  # the band sum by 2e-4 and the q profile by 6e-3 (relative), and the
  # BLAS of another platform does the same. Their tolerances are set above
  # that sensitivity; level and whittaker are insensitive to it (0 change)
  # and keep tight tolerances. A change of the default path is of larger
  # order than these tolerances.
  d <- sim_rs(n = 130L, spikes = 40, trough = integer(0))
  ref <- list(
    smooth_t = list(
      trend = c(7.019638916, 7.346602535, 7.035510792, 6.998417629),
      band = 30.74028923,
      value = c(5.047427919e-05, 0.0008444905177, 0.0009596145955),
      n_flag = 2L
    ),
    smooth = list(
      trend = c(7.018955208, 7.352060846, 7.040295125, 6.99498457),
      band = 38.40078664,
      value = 5.047427919e-05,
      n_flag = 1L
    ),
    level = list(
      trend = c(7.041480574, 7.360615707, 7.016459549, 6.988713929),
      band = 32.07486047,
      value = 0.0128734106,
      n_flag = 1L
    ),
    whittaker = list(
      trend = c(7.035196807, 7.349241377, 7.041860371, 6.986543109),
      band = 39.15347232,
      value = 25118.86432,
      n_flag = 1L
    )
  )
  for (m in names(ref)) {
    rs <- robust_smooth(d, "source", "week", "n", method = m,
      harmonics = 2L, n_backfit = 2L)
    fits <- attr(rs, "fits")
    ml <- m %in% c("smooth_t", "smooth")
    expect_equal(rs$trend[c(1, 50, 100, 130)], ref[[m]]$trend,
      tolerance = if (ml) 1e-4 else 1e-6, info = m)
    expect_equal(sum(rs$band_hi - rs$band_lo), ref[[m]]$band,
      tolerance = if (ml) 2e-3 else 1e-6, info = m)
    expect_equal(fits$value[seq_along(ref[[m]]$value)], ref[[m]]$value,
      tolerance = if (ml) 3e-2 else 1e-5, info = m)
    expect_identical(sum(rs$flag), ref[[m]]$n_flag, info = m)
  }
})

# reference copy of the undamped smoother before `phi` was added
rs_kf_smooth_v0 <- function(x, h, var_slope) {
  n <- length(x)
  kap <- skillviz:::.ti_kappa * max(stats::var(x, na.rm = TRUE), .Machine$double.eps)
  pp11 <- pp12 <- numeric(n)
  af1 <- pf11 <- pf12 <- numeric(n)
  vv <- ff <- rep(NA_real_, n)
  a1 <- x[which(!is.na(x))[1L]]
  a2 <- 0
  p11 <- 4 * kap
  p12 <- 2 * kap
  p22 <- kap + var_slope
  for (t in seq_len(n)) {
    pp11[t] <- p11
    pp12[t] <- p12
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
    a1 <- a1 + a2
    q11 <- p11 + 2 * p12 + p22
    q12 <- p12 + p22
    p11 <- q11
    p12 <- q12
    p22 <- p22 + var_slope
  }
  r1 <- r2 <- 0
  n11 <- n12 <- n22 <- 0
  lev <- vlev <- numeric(n)
  for (t in rev(seq_len(n))) {
    c1 <- pf11[t] + pf12[t]
    c2 <- pf12[t]
    lev[t] <- af1[t] + c1 * r1 + c2 * r2
    vlev[t] <- pf11[t] - (c1^2 * n11 + 2 * c1 * c2 * n12 + c2^2 * n22)
    if (!is.na(vv[t])) {
      f <- ff[t]
      l11 <- 1 - (pp11[t] + pp12[t]) / f
      l21 <- -pp12[t] / f
      nr1 <- vv[t] / f + l11 * r1 + l21 * r2
      nr2 <- r1 + r2
      a11 <- l11 * n11 + l21 * n12
      a12 <- l11 * n12 + l21 * n22
      m11 <- a11 * l11 + a12 * l21 + 1 / f
      m12 <- a11 + a12
      m22 <- n11 + n12 + n12 + n22
    } else {
      nr1 <- r1
      nr2 <- r1 + r2
      m11 <- n11
      m12 <- n11 + n12
      m22 <- n11 + 2 * n12 + n22
    }
    r1 <- nr1
    r2 <- nr2
    n11 <- m11
    n12 <- m12
    n22 <- m22
  }
  list(level = lev, var_level = pmax(vlev, 0))
}

test_that("the damped smoother equals the old one at phi = 1 and KalmanSmooth", {
  set.seed(4)
  x <- 5 + cumsum(cumsum(stats::rnorm(150, 0, 0.002))) +
    stats::rnorm(150, 0, 0.05)
  x[c(20, 60:65)] <- NA
  h <- 0.05^2 * stats::runif(150, 0.5, 2)
  old <- rs_kf_smooth_v0(x, h, 1e-5)
  new <- skillviz:::.rs_kf_smooth(x, h, 1e-5, phi = 1)
  expect_equal(new$level, old$level, tolerance = 1e-10)
  expect_equal(new$var_level, old$var_level, tolerance = 1e-10)
  expect_identical(
    skillviz:::.rs_kf_smooth(x, h, 1e-5)$level,
    new$level
  )
  # damped slope: same model and initial state in stats::KalmanSmooth()
  kap <- skillviz:::.ti_kappa * stats::var(x, na.rm = TRUE)
  for (phi in c(0.9, 0.5)) {
    mod <- list(
      T = matrix(c(1, 0, 1, phi), 2L), Z = c(1, 0), h = 0.0025,
      V = diag(c(0, 1e-5)), a = c(x[1L], 0), P = matrix(kap, 2L, 2L),
      Pn = matrix(0, 2L, 2L)
    )
    sm <- stats::KalmanSmooth(x, mod, nit = -1L)
    k <- skillviz:::.rs_kf_smooth(x, rep(0.0025, 150), 1e-5, phi = phi)
    expect_equal(k$level, sm$smooth[, 1L], tolerance = 1e-8, info = phi)
    # concentrated variance of KalmanRun() from the prediction errors
    kr <- stats::KalmanRun(x, mod, nit = -1L)
    i <- which(!is.na(k$v))
    expect_equal(mean(k$v[i]^2 / k$F[i]), unname(kr$values[2L]),
      tolerance = 1e-8, info = phi)
  }
})

# Smooth trend (optionally damped slope) + 3 harmonics + scaled t(4) noise
# of variance h; the t-model scale is h * (nu - 2) / nu, so the q of the
# t model is q * nu / (nu - 2).
sim_t <- function(n = 191L, q = 1e-3, h = 0.01, phi = 1, seed = 1L,
                  nu = 4, trough = NULL) {
  set.seed(seed)
  weeks <- data.table::as.IDate("2023-01-02") + 7L * (0:(n - 1L))
  tw <- as.numeric(weeks) / 7
  sl <- lev <- numeric(n)
  sl[1L] <- 0.01
  lev[1L] <- 5
  for (t in 2:n) {
    sl[t] <- phi * sl[t - 1L] + stats::rnorm(1L, 0, sqrt(q * h))
    lev[t] <- lev[t - 1L] + sl[t - 1L]
  }
  X <- skillviz:::.ti_fourier(tw, 3L, rs_period)
  y <- lev + drop(X %*% stats::rnorm(ncol(X), 0, 0.08)) +
    sqrt(h) * stats::rt(n, nu) * sqrt((nu - 2) / nu)
  if (!is.null(trough)) {
    y[trough] <- y[trough] - 1.5
  }
  data.table::data.table(source = "s", week = weeks, n = y, lev = lev)
}

rs_t <- function(d, ...) {
  robust_smooth(d, "source", "week", "n", method = "smooth_t",
    transform = "none", harmonics = 3L, max_iter = 0L, ...)
}

test_that("profile_t recovers q and is less affected by a trough", {
  d <- sim_t(seed = 1L)
  g <- attr(rs_t(d), "fits")
  p <- attr(rs_t(d, q_select = "profile_t"), "fits")
  expect_identical(g$q_select, "gaussian")
  expect_identical(p$q_select, "profile_t")
  expect_identical(p$phi, 1)
  expect_true(is.na(g$loglik_profile))
  expect_true(is.finite(p$loglik_profile))
  expect_false(p$q_at_edge)
  expect_true(is.na(p$phi_at_edge))
  # true q of the t model: 1e-3 * 4 / 2
  expect_lt(abs(log10(p$value) - log10(2e-3)), 0.3)
  expect_true(p$irls_converged)
  dt <- sim_t(seed = 1L, trough = 100:105)
  g2 <- attr(rs_t(dt), "fits")
  p2 <- attr(rs_t(dt, q_select = "profile_t"), "fits")
  expect_lt(
    abs(log10(p2$value / p$value)),
    abs(log10(g2$value / g$value))
  )
})

test_that("damping profile finds phi < 1 on a damped series, ~1 otherwise", {
  dd <- sim_t(seed = 2L, phi = 0.9, q = 1e-2)
  f <- attr(rs_t(dd, q_select = "profile_t", damping = "profile"), "fits")
  expect_lt(f$phi, 0.97)
  expect_false(f$phi_at_edge)
  du <- sim_t(seed = 4L)
  fu <- attr(rs_t(du, q_select = "profile_t", damping = "profile"), "fits")
  expect_gte(fu$phi, 0.96)
})

test_that("edge flags of the profile grid", {
  dd <- sim_t(seed = 2L, phi = 0.9, q = 1e-2)
  f <- attr(rs_t(dd, q_select = "profile_t", damping = "profile",
    log10_q_grid = seq(-6, -5, by = 0.5), phi_grid = c(0.98, 0.99, 1)),
    "fits")
  expect_equal(f$value, 1e-5, tolerance = 1e-12)
  expect_true(f$q_at_edge)
  expect_equal(f$phi, 0.98, tolerance = 1e-12)
  expect_true(f$phi_at_edge)
})

test_that("weight and flag_weight columns", {
  d <- sim_rs(n = 130L, spikes = 40, trough = integer(0))
  rs <- robust_smooth(d, "source", "week", "n", method = "smooth_t",
    harmonics = 2L, max_iter = 0L)
  expect_true(all(rs$weight > 0 & rs$weight <= 5 / 4))
  expect_identical(rs$flag_weight, rs$weight < 5 / (4 + 3.5^2))
  expect_true(rs$flag_weight[40])
  expect_lt(rs$weight[40], 0.1)
  # weeks not used in the fit have no weight
  r2 <- robust_smooth(d, "source", "week", "n", method = "smooth_t",
    harmonics = 2L)
  expect_true(is.na(r2$weight[40]) && isTRUE(r2$excluded[40]))
  rw <- robust_smooth(d, "source", "week", "n", method = "whittaker",
    harmonics = 2L)
  expect_true(all(is.na(rw$weight) & is.na(rw$flag_weight)))
  expect_true(all(is.na(attr(rw, "fits")$q_select)))
})

test_that("q_select and damping reject invalid combinations", {
  d <- sim_rs(n = 60L, spikes = integer(0), trough = integer(0))
  expect_error(
    robust_smooth(d, "source", "week", "n", method = "smooth",
      q_select = "profile_t"),
    "only to method = 'smooth_t'"
  )
  expect_error(
    robust_smooth(d, "source", "week", "n", method = "llt",
      damping = "profile"),
    "only to method = 'smooth_t'"
  )
  expect_error(
    robust_smooth(d, "source", "week", "n", method = "smooth_t",
      damping = "profile"),
    "requires q_select = 'profile_t'"
  )
  expect_error(
    robust_smooth(d, "source", "week", "n", method = "smooth_t",
      q_select = "profile_t", phi_grid = c(0.9, 1.1)),
    "phi_grid"
  )
  expect_error(
    robust_smooth(d, "source", "week", "n", method = "smooth_t",
      q_select = "profile_t", log10_q_grid = c(-3, NA)),
    "log10_q_grid"
  )
  expect_error(
    robust_smooth(d, "source", "week", "n", q_select = "x"),
    "should be one of"
  )
})
