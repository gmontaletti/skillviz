# Tests for share_intervals.R -----

# 0. synthetic data helpers -----

# Clustered counts: S sources with n_s postings each and source-specific
# shares drawn from a beta distribution with mean mu and concentration
# kappa (intra-cluster correlation 1 / (1 + kappa)).
sim_clustered <- function(S, n_s, mu, kappa) {
  p_s <- stats::rbeta(S, mu * kappa, (1 - mu) * kappa)
  stats::rbinom(S, n_s, p_s)
}

wilson_by_hand <- function(x, n, z = stats::qnorm(0.975)) {
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z / (1 + z^2 / n) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  cbind(centre - half, centre + half)
}

tree_counts <- function() {
  data.table::data.table(
    code = c("A", "B", "A1", "A2", "A3", "B1", "B2", "A11", "A12"),
    parent = c(NA, NA, "A", "A", "A", "B", "B", "A1", "A1"),
    level = c(1, 1, 2, 2, 2, 2, 2, 3, 3),
    x = c(700, 300, 500, 150, 50, 290, 10, 420, 80),
    n = 1000
  )
}

# 1. share_interval -----

test_that("share_interval reproduces the Wilson interval with deff = 1", {
  x <- c(0, 1, 12, 300, 1000)
  res <- share_interval(x, 1000)
  ref <- wilson_by_hand(x, 1000)
  expect_equal(res$lo, pmax(ref[, 1], 0), tolerance = 1e-10)
  expect_equal(res$hi, pmin(ref[, 2], 1), tolerance = 1e-10)
  expect_equal(res$quota, x / 1000)
  expect_identical(res$metodo, rep("wilson", 5L))
  expect_identical(names(res), c("quota", "lo", "hi", "n_eff", "metodo"))
  # textbook value: 12 / 1000 -> [0.00688, 0.02086]
  expect_equal(res$lo[3], 0.006877648, tolerance = 1e-6)
  expect_equal(res$hi[3], 0.020857268, tolerance = 1e-6)
})

test_that("share_interval uses the effective sample size and t quantile", {
  a <- share_interval(40, 1000, deff = 4)
  b <- share_interval(10, 250)
  expect_equal(a$lo, b$lo, tolerance = 1e-12)
  expect_equal(a$hi, b$hi, tolerance = 1e-12)
  expect_equal(a$n_eff, 250)
  wide <- share_interval(40, 1000, deff = 4, df = 5)
  expect_true(wide$lo < a$lo && wide$hi > a$hi)
  ref <- wilson_by_hand(10, 250, z = stats::qt(0.975, 5))
  expect_equal(c(wide$lo, wide$hi), as.numeric(ref), tolerance = 1e-10)
})

test_that("share_interval jeffreys and logit behave at the boundaries", {
  j <- share_interval(c(0, 5, 100), 100, method = "jeffreys")
  expect_equal(j$lo[1], 0)
  expect_equal(j$hi[3], 1)
  expect_equal(
    j$hi[2],
    stats::qbeta(0.975, 5.5, 95.5),
    tolerance = 1e-12
  )
  l <- share_interval(c(0, 5, 100), 100, method = "logit")
  expect_true(all(l$lo > 0 & l$hi < 1))
  expect_true(all(l$lo <= l$quota + 1e-12 | l$quota == 0))
  se <- sqrt(1 / (100 * 0.05 * 0.95))
  expect_equal(
    l$lo[2],
    stats::plogis(stats::qlogis(0.05) - stats::qnorm(0.975) * se),
    tolerance = 1e-12
  )
})

test_that("share_interval propagates NA and validates input", {
  res <- share_interval(c(5, 5), 100, deff = c(NA, 2))
  expect_true(is.na(res$lo[1]) && is.na(res$hi[1]))
  expect_false(is.na(res$lo[2]))
  expect_identical(nrow(share_interval(numeric(0), 10)), 0L)
  expect_error(share_interval(11, 10), "must not exceed")
  expect_error(share_interval(1, 0), "positive")
  expect_error(share_interval(1, 10, deff = 0), "deff")
  expect_error(share_interval(1, 10, df = -1), "df")
  expect_error(share_interval(1:3, c(10, 10)), "length")
  expect_error(share_interval(1, 10, conf = 1), "conf")
})

test_that("share_interval with the true deff has coverage close to 0.95", {
  set.seed(20261110)
  S <- 30L
  n_s <- 1000
  mu <- 0.1
  kappa <- 49 # rho = 0.02
  deff_true <- 1 + (n_s - 1) / (1 + kappa)
  R <- 400L
  x <- vapply(
    seq_len(R),
    function(i) sum(sim_clustered(S, n_s, mu, kappa)),
    numeric(1)
  )
  n <- S * n_s
  with_deff <- share_interval(x, n, deff = deff_true, df = S - 1)
  naive <- share_interval(x, n)
  cov_deff <- mean(with_deff$lo <= mu & mu <= with_deff$hi)
  cov_naive <- mean(naive$lo <= mu & mu <= naive$hi)
  expect_gt(cov_deff, 0.91)
  expect_lt(cov_deff, 0.99)
  expect_lt(cov_naive, 0.6)
})

# 2. cluster_deff -----

test_that("cluster_deff matches the linearised variance by hand", {
  counts <- data.table::data.table(
    code = rep(c("a", "b"), each = 4),
    source = rep(paste0("s", 1:4), 2),
    x = c(10, 40, 5, 25, 3, 2, 4, 1),
    n = rep(c(200, 400, 100, 300), 2)
  )
  res <- cluster_deff(counts, "code")
  xs <- c(10, 40, 5, 25)
  ns <- c(200, 400, 100, 300)
  p <- sum(xs) / sum(ns)
  v_cl <- 4 / 3 * sum((xs - p * ns)^2) / sum(ns)^2
  v_b <- p * (1 - p) / sum(ns)
  m <- sum(ns^2) / sum(ns)
  ra <- res[code == "a"]
  expect_equal(ra$quota, p)
  expect_equal(ra$v_cluster, v_cl, tolerance = 1e-12)
  expect_equal(ra$deff, v_cl / v_b, tolerance = 1e-12)
  expect_equal(ra$rho, (v_cl / v_b - 1) / (m - 1), tolerance = 1e-12)
  expect_equal(ra$n_cluster_eff, sum(ns)^2 / sum(ns^2), tolerance = 1e-12)
  expect_equal(ra$df, ra$n_cluster_eff - 1, tolerance = 1e-12)
  expect_identical(ra$n_cluster, 4L)
  expect_identical(
    names(res),
    c(
      "code",
      "x",
      "n",
      "quota",
      "v_bin",
      "v_cluster",
      "deff",
      "rho",
      "n_cluster",
      "n_cluster_eff",
      "df"
    )
  )
})

test_that("cluster_deff with cluster_totals counts sources with zero x", {
  full <- data.table::data.table(
    code = "a",
    source = paste0("s", 1:4),
    x = c(10, 0, 5, 0),
    n = c(200, 400, 100, 300)
  )
  sparse <- full[x > 0, list(code, source, x)]
  totals <- full[, list(source, n)]
  a <- cluster_deff(full, "code")
  b <- cluster_deff(sparse, "code", cluster_totals = totals)
  expect_equal(b$v_cluster, a$v_cluster, tolerance = 1e-12)
  expect_equal(b$deff, a$deff, tolerance = 1e-12)
  expect_identical(b$n_cluster, 4L)
  # without totals the zero sources are lost
  c0 <- cluster_deff(full[x > 0], "code")
  expect_identical(c0$n_cluster, 2L)
})

test_that("cluster_deff returns NA at the boundaries and validates input", {
  counts <- data.table::data.table(
    code = c("a", "a", "b", "b"),
    source = c("s1", "s2", "s1", "s2"),
    x = c(0, 0, 10, 20),
    n = c(10, 20, 10, 20)
  )
  res <- cluster_deff(counts, "code")
  expect_true(all(is.na(res$deff)))
  one <- cluster_deff(counts[source == "s1"], "code")
  expect_true(all(is.na(one$v_cluster)))
  expect_error(cluster_deff(counts, "code", x_col = "zz"), "missing")
  bad <- data.table::copy(counts)[1, x := 50]
  expect_error(cluster_deff(bad, "code"), "must not exceed")
  expect_error(
    cluster_deff(rbind(counts, counts), "code"),
    "duplicated"
  )
  expect_error(
    cluster_deff(
      counts[, list(code, source, x)],
      "code",
      cluster_totals = data.frame(source = "s1", n = 10)
    ),
    "missing from"
  )
})

# 3. smooth_deff -----

test_that("smooth_deff shrinks noisy children towards the parent", {
  set.seed(3)
  est <- data.table::data.table(
    code = c("A", "B", paste0("A", 1:6), paste0("B", 1:6)),
    parent = c(NA, NA, rep("A", 6), rep("B", 6)),
    level = c(1, 1, rep(2, 12)),
    quota = c(0.3, 0.2, stats::runif(12, 0.01, 0.1)),
    deff = c(5, 5, exp(stats::rnorm(12, log(5), 0.8))),
    df = c(30, 30, rep(c(2, 30), 6))
  )
  res <- smooth_deff(est, "code", "parent", "level")
  expect_true(all(c("deff_liscio", "peso_contrazione") %in% names(res)))
  expect_true(all(res$deff_liscio >= 1))
  expect_true(all(res$peso_contrazione >= 0 & res$peso_contrazione <= 1))
  ch <- res[level == 2]
  # nodes with fewer degrees of freedom are shrunk more
  expect_true(
    mean(ch[df == 2, peso_contrazione]) >= mean(ch[df == 30, peso_contrazione])
  )
  expect_s3_class(attr(res, "gvf"), "data.table")
  expect_s3_class(attr(res, "tau2"), "data.table")
})

test_that("smooth_deff gvf fits log deff on log quota", {
  est <- data.table::data.table(
    code = paste0("c", 1:5),
    parent = NA_character_,
    level = 1,
    quota = c(0.01, 0.02, 0.05, 0.1, 0.2),
    df = 10
  )
  est[, deff := exp(0.5 + 0.3 * log(quota))]
  res <- smooth_deff(est, "code", "parent", "level", method = "gvf")
  expect_equal(res$deff_liscio, pmax(1, est$deff), tolerance = 1e-8)
  expect_equal(attr(res, "gvf")$pendenza, 0.3, tolerance = 1e-8)
  expect_true(all(res$peso_contrazione == 1))
  # identical observations: no prior variance, full shrinkage to the GVF
  par <- smooth_deff(est, "code", "parent", "level", method = "parent")
  expect_equal(attr(par, "tau2")$tau2, 0)
})

test_that("smooth_deff handles missing deff and validates the tree", {
  est <- data.table::data.table(
    code = c("A", "A1", "A2"),
    parent = c(NA, "A", "A"),
    level = c(1, 2, 2),
    quota = c(0.2, 0.1, 0.1),
    deff = c(4, NA, 3),
    df = c(10, NA, 5)
  )
  res <- smooth_deff(est, "code", "parent", "level", method = "parent")
  expect_equal(res[code == "A1", peso_contrazione], 1)
  expect_equal(
    res[code == "A1", deff_liscio],
    res[code == "A", deff_liscio],
    tolerance = 1e-12
  )
  bad <- data.table::copy(est)[code == "A1", level := 1]
  expect_error(smooth_deff(bad, "code", "parent", "level"), "lower")
  expect_error(
    smooth_deff(rbind(est, est), "code", "parent", "level"),
    "unique"
  )
  expect_error(smooth_deff(est, "code", "parent", "level", df_col = "zz"))
})

# 4. fit_betabinom -----

test_that("fit_betabinom recovers mean and concentration", {
  set.seed(11)
  x <- sim_clustered(400, 300, 0.2, 20)
  fit <- fit_betabinom(x, 300)
  expect_equal(fit$mean, 0.2, tolerance = 0.05)
  expect_gt(fit$kappa, 12)
  expect_lt(fit$kappa, 32)
  expect_equal(fit$rho, 1 / (1 + fit$kappa))
  expect_identical(fit$convergence, 0L)
  fixed <- fit_betabinom(x, 300, mean = 0.2)
  expect_equal(fixed$mean, 0.2)
  expect_true(fixed$loglik <= fit$loglik + 1e-6)
})

test_that("fit_betabinom returns Inf without overdispersion", {
  expect_warning(
    fit <- fit_betabinom(c(10, 10, 10, 10), 100),
    "no overdispersion"
  )
  expect_identical(fit$kappa, Inf)
  expect_equal(fit$mean, 0.1)
  expect_equal(
    fit$loglik,
    sum(stats::dbinom(10, 100, 0.1, log = TRUE)) * 4,
    tolerance = 1e-10
  )
  expect_warning(fit_betabinom(c(0, 0), 10), "no overdispersion")
})

test_that("fit_betabinom validates input", {
  expect_error(fit_betabinom(5, 3), "x <= n")
  expect_error(fit_betabinom(c(1, 2), c(5, 5, 5)), "length")
  expect_error(fit_betabinom(1, 5, mean = 1.5), "mean")
  expect_error(fit_betabinom(0, 0), "positive")
})

# 5. eb_share_tree -----

test_that("eb_share_tree partition keeps children summing to the parent", {
  est <- tree_counts()
  set.seed(5)
  res <- eb_share_tree(est, "code", "parent", "level", draws = 400)
  qeb <- stats::setNames(res$quota_eb, res$code)
  expect_equal(unname(qeb["A"] + qeb["B"]), 1, tolerance = 1e-10)
  expect_lt(abs(sum(qeb[c("A1", "A2", "A3")]) - qeb["A"]), 1e-8)
  expect_lt(abs(sum(qeb[c("B1", "B2")]) - qeb["B"]), 1e-8)
  expect_lt(abs(sum(qeb[c("A11", "A12")]) - qeb["A1"]), 1e-8)
  expect_true(all(res[code %in% c("A", "B", "A1"), vincoli_ok]))
  expect_true(all(is.na(res[code == "A2", vincoli_ok])))
  expect_true(all(res$lo_eb <= res$quota_eb & res$quota_eb <= res$hi_eb))
  expect_true(all(
    c("quota_eb", "lo_eb", "hi_eb", "kappa_famiglia") %in%
      names(res)
  ))
})

test_that("eb_share_tree partition shrinks towards the prior mean", {
  est <- tree_counts()
  est[, pm := c(0.5, 0.5, 1 / 3, 1 / 3, 1 / 3, 0.5, 0.5, 0.5, 0.5)]
  set.seed(5)
  plain <- eb_share_tree(est, "code", "parent", "level", draws = 200)
  set.seed(5)
  infl <- eb_share_tree(
    est[, d := 50],
    "code",
    "parent",
    "level",
    prior_mean = "pm",
    deff_col = "d",
    draws = 200
  )
  # a large design effect means less information and more shrinkage
  dir <- est$x / 1000
  expect_true(all(
    abs(infl$quota_eb - 0.5)[1:2] <= abs(dir - 0.5)[1:2] + 1e-12
  ))
  expect_true(
    abs(infl[code == "B2", quota_eb] - 0.15) <
      abs(plain[code == "B2", quota_eb] - 0.15)
  )
  # draws are random: the seed is set by the caller
  set.seed(9)
  r1 <- eb_share_tree(est, "code", "parent", "level", draws = 200)
  set.seed(9)
  r2 <- eb_share_tree(est, "code", "parent", "level", draws = 200)
  expect_identical(r1$lo_eb, r2$lo_eb)
})

test_that("eb_share_tree partition validates the family sums", {
  est <- tree_counts()
  est[code == "A2", x := 140]
  expect_error(
    eb_share_tree(est, "code", "parent", "level"),
    "do not sum"
  )
  est <- tree_counts()
  est[code == "A11", parent := "Z"]
  expect_error(eb_share_tree(est, "code", "parent", "level"), "not found")
  expect_error(
    eb_share_tree(tree_counts(), "code", "parent", "level", draws = 1),
    "draws"
  )
})

test_that("eb_share_tree overlap returns direct shares without a prior", {
  est <- data.table::data.table(
    code = c("G", "g1", "g2", "g3"),
    parent = c(NA, "G", "G", "G"),
    level = c(1, 2, 2, 2),
    x = c(300, 200, 150, 20),
    n = 1000,
    pm = c(0.28, 0.22, 0.13, 0.03)
  )
  res <- eb_share_tree(est, "code", "parent", "level", type = "overlap")
  expect_equal(res$quota_eb, est$x / est$n)
  expect_equal(
    res$hi_eb[4],
    stats::qbeta(0.975, 20.5, 980.5),
    tolerance = 1e-12
  )
  expect_true(res[code == "G", vincoli_ok])
  shr <- eb_share_tree(
    est,
    "code",
    "parent",
    "level",
    type = "overlap",
    prior_mean = "pm"
  )
  dir <- est$x / est$n
  expect_true(all(abs(shr$quota_eb - est$pm) <= abs(dir - est$pm) + 1e-12))
  # the parent cannot be smaller than its largest child
  est2 <- data.table::copy(est)[code == "G", x := 150]
  bad <- eb_share_tree(est2, "code", "parent", "level", type = "overlap")
  expect_false(bad[code == "G", vincoli_ok])
})

# 6. cluster_boot_share -----

test_that("cluster_boot_share agrees with the linearised variance", {
  set.seed(21)
  S <- 40L
  ns <- round(stats::runif(S, 200, 2000))
  counts <- data.table::rbindlist(lapply(c("a", "b"), function(k) {
    mu <- if (k == "a") 0.1 else 0.02
    data.table::data.table(
      code = k,
      source = paste0("s", seq_len(S)),
      x = stats::rbinom(S, ns, stats::rbeta(S, mu * 30, (1 - mu) * 30))
    )
  }))
  totals <- data.table::data.table(source = paste0("s", seq_len(S)), n = ns)
  set.seed(22)
  bt <- cluster_boot_share(counts, "code", B = 2000, cluster_totals = totals)
  cd <- cluster_deff(counts, "code", cluster_totals = totals)
  expect_equal(bt$quota, cd$quota, tolerance = 1e-12)
  # bootstrap variance ~ (S - 1) / S times the linearised variance
  ratio <- bt$se_boot^2 / (cd$v_cluster * (S - 1) / S)
  expect_true(all(ratio > 0.75 & ratio < 1.35))
  expect_true(all(bt$lo_boot < bt$quota & bt$quota < bt$hi_boot))
  expect_identical(
    names(bt),
    c("code", "quota", "lo_boot", "hi_boot", "se_boot")
  )
  # same result without totals when counts are complete
  full <- merge(counts, totals, by = "source")
  set.seed(22)
  bt2 <- cluster_boot_share(full, "code", B = 2000)
  expect_equal(bt2$lo_boot, bt$lo_boot, tolerance = 1e-12)
})

test_that("cluster_boot_share validates input", {
  counts <- data.table::data.table(code = "a", source = "s1", x = 1, n = 10)
  expect_error(cluster_boot_share(counts, "code"), "two clusters")
  expect_error(cluster_boot_share(counts, "code", B = 1), "B")
})

# 7. tree_fdr -----

test_that("tree_fdr descends only below rejected nodes", {
  res <- tree_fdr(
    p = c(0.001, 0.4, 0.01, 0.03, 0.2, 0.0001),
    node = c("A", "B", "A1", "A2", "B1", "A11"),
    parent = c(NA, "", "A", "A", "B", "A1")
  )
  expect_identical(res$tested, c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_identical(res$rejected, c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(res$q_famiglia[1:2], stats::p.adjust(c(0.001, 0.4), "BH"))
  expect_equal(res$q_famiglia[3:4], stats::p.adjust(c(0.01, 0.03), "BH"))
  expect_true(is.na(res$q_famiglia[5]))
})

test_that("tree_fdr treats NA p-values as untestable branches", {
  res <- tree_fdr(
    p = c(NA, 0.001, 0.001),
    node = c("A", "A1", "B"),
    parent = c(NA, "A", NA)
  )
  expect_identical(res$tested, c(FALSE, FALSE, TRUE))
  expect_identical(res$rejected, c(FALSE, FALSE, TRUE))
  expect_error(tree_fdr(c(0.1, 2), c("a", "b"), c(NA, NA)), "\\[0, 1\\]")
  expect_error(tree_fdr(0.1, c("a", "b"), NA), "same length")
  expect_error(tree_fdr(c(0.1, 0.2), c("a", "a"), c(NA, NA)), "unique")
  expect_error(tree_fdr(0.1, "a", NA, q = 0), "q")
})

# 8. finest_reliable_level -----

test_that("finest_reliable_level walks up to the first reliable ancestor", {
  est <- data.table::data.table(
    code = c("A", "A1", "A11", "B", "B1", "C"),
    parent = c(NA, "A", "A1", NA, "B", NA),
    level = c(1, 2, 3, 1, 2, 1),
    rw = c(0.1, 0.4, 1.2, 0.9, 2, NA)
  )
  res <- finest_reliable_level(est, "code", "parent", "rw", 0.5, "level")
  expect_identical(
    res$codice_affidabile,
    c("A", "A1", "A1", NA, NA, NA)
  )
  expect_equal(res$livello_affidabile, c(1, 2, 2, NA, NA, NA))
  no_lev <- finest_reliable_level(est, "code", "parent", "rw", 0.5)
  expect_false("livello_affidabile" %in% names(no_lev))
  expect_error(
    finest_reliable_level(est, "code", "parent", "rw", -1),
    "positive"
  )
  expect_error(finest_reliable_level(est, "code", "parent", "zz", 0.5))
})
