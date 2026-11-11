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
