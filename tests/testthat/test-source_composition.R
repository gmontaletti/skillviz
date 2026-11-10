# Tests for source_composition.R -----

# 0. synthetic data helpers -----

# Random key x source x period counts with sources entering and exiting.
sim_sources <- function(seed = 1L, n_keys = 6L, entry = TRUE, exit = TRUE) {
  set.seed(seed)
  sources <- paste0("s", 1:6)
  periods <- 1:4
  tot <- data.table::CJ(source = sources, mese = periods)
  tot[, n := round(stats::runif(.N, 50, 500))]
  if (exit) {
    tot[source == "s5" & mese >= 3, n := 0]
  }
  if (entry) {
    tot[source == "s6" & mese <= 2, n := 0]
  }
  keys <- paste0("k", seq_len(n_keys))
  cnt <- tot[rep(seq_len(.N), each = n_keys)]
  cnt[, code := rep(keys, nrow(tot))]
  cnt[, x := stats::rbinom(.N, n, stats::runif(.N, 0, 0.3))]
  # one key absent from some sources, one key always zero
  cnt[code == "k1" & source %in% c("s1", "s3"), x := 0]
  cnt[code == "k2", x := 0]
  # k2 keeps explicit zero rows so that it appears among the keys
  list(
    counts = cnt[x > 0 | code == "k2", list(code, source, mese, x)],
    totals = tot
  )
}

identity_gap <- function(d) {
  max(abs(d$delta - (d$entro + d$fra + d$ingressi + d$uscite)))
}

# 1. decompose_share_change -----

test_that("decompose_share_change components sum to the change", {
  for (cfg in list(
    list(entry = TRUE, exit = TRUE),
    list(entry = FALSE, exit = TRUE),
    list(entry = TRUE, exit = FALSE),
    list(entry = FALSE, exit = FALSE)
  )) {
    s <- sim_sources(entry = cfg$entry, exit = cfg$exit)
    d <- decompose_share_change(
      s$counts,
      "code",
      time_col = "mese",
      t0 = 1:2,
      t1 = 3:4,
      source_totals = s$totals
    )
    expect_lt(identity_gap(d), 1e-10)
    expect_equal(d$composizione, d$fra + d$ingressi + d$uscite)
    if (!cfg$entry) {
      expect_true(all(d$ingressi == 0))
    }
    if (!cfg$exit) {
      expect_true(all(d$uscite == 0))
    }
  }
})

test_that("decompose_share_change matches the aggregate shares", {
  s <- sim_sources()
  d <- decompose_share_change(
    s$counts,
    "code",
    time_col = "mese",
    t0 = 1:2,
    t1 = 3:4,
    source_totals = s$totals
  )
  x0 <- s$counts[mese %in% 1:2, list(x = sum(x)), by = code]
  n0 <- s$totals[mese %in% 1:2, sum(n)]
  expect_equal(
    d[x0, on = "code", p0],
    x0$x / n0,
    tolerance = 1e-12
  )
  expect_true(all(
    d[code == "k2", c(p0, p1, entro, fra, ingressi, uscite)] == 0
  ))
  fonti <- attr(d, "fonti")
  expect_identical(fonti[source == "s5", classe], "X")
  expect_identical(fonti[source == "s6", classe], "E")
  expect_identical(fonti[source == "s1", classe], "C")
  expect_identical(
    names(d),
    c(
      "code",
      "p0",
      "p1",
      "delta",
      "entro",
      "fra",
      "ingressi",
      "uscite",
      "composizione"
    )
  )
})

test_that("decompose_share_change handles one source and pure reallocation", {
  one <- data.table::data.table(
    code = "a",
    source = "s1",
    mese = 1:2,
    x = c(10, 30),
    n = c(100, 100)
  )
  d <- decompose_share_change(one, "code", time_col = "mese", t0 = 1, t1 = 2)
  expect_equal(d$entro, 0.2)
  expect_equal(d$fra, 0)
  expect_equal(d$composizione, 0)

  # constant source shares, weights move: only the between component
  re <- data.table::data.table(
    code = "a",
    source = rep(c("s1", "s2"), 2),
    mese = rep(1:2, each = 2),
    x = c(10, 50, 30, 50),
    n = c(100, 100, 300, 100)
  )
  d <- decompose_share_change(re, "code", time_col = "mese", t0 = 1, t1 = 2)
  expect_equal(d$entro, 0, tolerance = 1e-12)
  expect_equal(d$fra, d$delta, tolerance = 1e-12)
  expect_lt(identity_gap(d), 1e-12)
})

test_that("decompose_share_change without continuing sources", {
  dt <- data.table::data.table(
    code = "a",
    source = c("s1", "s2"),
    mese = 1:2,
    x = c(10, 40),
    n = c(100, 100)
  )
  d <- decompose_share_change(dt, "code", time_col = "mese", t0 = 1, t1 = 2)
  expect_equal(d$ingressi, 0.4)
  expect_equal(d$uscite, -0.1)
  expect_equal(d$entro + d$fra, 0)
  expect_lt(identity_gap(d), 1e-12)
})

test_that("decompose_share_change validates input", {
  s <- sim_sources()
  expect_error(
    decompose_share_change(
      s$counts,
      "code",
      time_col = "mese",
      t0 = 1:2,
      t1 = 2:3,
      source_totals = s$totals
    ),
    "overlap"
  )
  expect_error(
    decompose_share_change(
      s$counts,
      "code",
      time_col = "mese",
      t0 = 1,
      t1 = 2
    ),
    "missing"
  )
  bad <- data.table::data.table(
    code = c("a", "b"),
    source = "s1",
    mese = 1,
    x = 1,
    n = c(10, 20)
  )
  expect_error(
    decompose_share_change(bad, "code", time_col = "mese", t0 = 1, t1 = 2),
    "varies across keys"
  )
  expect_error(
    decompose_share_change(
      s$counts,
      "code",
      time_col = "mese",
      t0 = 1,
      t1 = 9,
      source_totals = s$totals
    ),
    "positive"
  )
  over <- data.table::copy(s$counts)[1, x := 1e6]
  expect_error(
    decompose_share_change(
      over,
      "code",
      time_col = "mese",
      t0 = 1,
      t1 = 2,
      source_totals = s$totals
    ),
    "exceed"
  )
})

# 2. fixed_weight_share -----

test_that("fixed_weight_share removes the reallocation between sources", {
  re <- data.table::data.table(
    code = "a",
    source = rep(c("s1", "s2"), 2),
    mese = rep(1:2, each = 2),
    x = c(10, 50, 30, 50),
    n = c(100, 100, 300, 100)
  )
  res <- fixed_weight_share(re, "code", time_col = "mese")
  expect_equal(res$quota_fissa[1], res$quota_fissa[2], tolerance = 1e-12)
  expect_equal(res$quota_osservata, c(0.3, 0.2), tolerance = 1e-12)
  expect_identical(res$n_fonti, c(2L, 2L))
  w <- c(s1 = 0.25, s2 = 0.75)
  res_w <- fixed_weight_share(re, "code", time_col = "mese", weights = w)
  expect_equal(res_w$quota_fissa, rep(0.25 * 0.1 + 0.75 * 0.5, 2))
  expect_identical(
    names(res),
    c("code", "mese", "quota_fissa", "quota_osservata", "n_fonti")
  )
})

test_that("fixed_weight_share completes keys and renormalises weights", {
  s <- sim_sources()
  res <- fixed_weight_share(
    s$counts,
    "code",
    time_col = "mese",
    source_totals = s$totals
  )
  expect_identical(nrow(res), 6L * 4L)
  expect_true(all(res[code == "k2", quota_fissa == 0 & quota_osservata == 0]))
  expect_identical(res[code == "k1", n_fonti], c(5L, 5L, 5L, 5L))
  # with equal source shares the fixed-weight series equals that share
  eq <- data.table::copy(s$totals)[n > 0]
  eq[, `:=`(code = "z", x = n / 5)]
  r2 <- fixed_weight_share(
    eq,
    "code",
    time_col = "mese",
    source_totals = s$totals
  )
  expect_equal(r2$quota_fissa, rep(0.2, 4), tolerance = 1e-12)
})

test_that("fixed_weight_share validates weights", {
  s <- sim_sources()
  expect_error(
    fixed_weight_share(
      s$counts,
      "code",
      time_col = "mese",
      source_totals = s$totals,
      weights = c(s1 = 1)
    ),
    "missing for sources"
  )
  expect_error(
    fixed_weight_share(
      s$counts,
      "code",
      time_col = "mese",
      source_totals = s$totals,
      weights = c(1, 2)
    ),
    "named"
  )
})
