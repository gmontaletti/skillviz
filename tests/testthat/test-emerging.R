# Tests for emerging.R -----

# 0. synthetic data helpers -----

# Estimates from simulated data are compared on an absolute scale:
# expect_equal() tolerances are relative and fail near zero.

sim_panel <- function(
  betas,
  n_months = 36L,
  n = 20000L,
  intercept = -3,
  jump = 0,
  jump_at = Inf,
  common = 0,
  seed = 1L
) {
  set.seed(seed)
  keys <- sprintf("s%02d", seq_along(betas))
  panel <- data.table::CJ(skill_id = keys, mese_idx = seq_len(n_months))
  panel[, beta := betas[match(skill_id, keys)]]
  panel[, n := n]
  panel[,
    x := stats::rbinom(
      .N,
      n,
      stats::plogis(
        intercept + (beta + common) * mese_idx + jump * (mese_idx >= jump_at)
      )
    )
  ]
  panel[]
}

# 1. compute_share_panel -----

test_that("compute_share_panel counts postings and fills zeros", {
  inc <- data.table::data.table(
    general_id = c(1, 1, 2, 3, 3, 4),
    mese_idx = c(1, 1, 1, 2, 2, 2),
    skill_id = c("a", "b", "a", "a", "c", "b")
  )
  res <- compute_share_panel(inc)
  expect_identical(nrow(res), 6L)
  expect_equal(res[skill_id == "a" & mese_idx == 1, x], 2)
  expect_equal(res[skill_id == "c" & mese_idx == 1, x], 0)
  expect_equal(res[skill_id == "c" & mese_idx == 2, quota], 0.5)
  expect_true(all(res$n == 2))

  grp <- data.table::copy(inc)[, cp4 := c("p", "p", "q", "p", "p", "q")]
  res_g <- compute_share_panel(grp, group_cols = "cp4")
  expect_equal(res_g[cp4 == "q" & skill_id == "a" & mese_idx == 1, quota], 1)
})

test_that("compute_share_panel rejects missing columns and bad input", {
  inc <- data.table::data.table(general_id = 1, mese_idx = 1)
  expect_error(compute_share_panel(inc), "missing required columns")
  expect_error(compute_share_panel(1:3), "must be a data.frame")
})

# 2. compute_share_trend -----

test_that("compute_share_trend recovers a known logit slope", {
  panel <- sim_panel(betas = rep(c(0.05, -0.02, 0), each = 4))
  res <- compute_share_trend(panel, "skill_id")
  res <- merge(res, unique(panel[, list(skill_id, beta)]), by = "skill_id")
  expect_lt(max(abs(res$pendenza - (res$beta))), 0.005)
  expect_true(all(res$phi >= 1))
  expect_true(all(res[beta != 0, p_adj] < 0.001))
  expect_identical(res$n_mesi, rep(36L, 12L))
})

test_that("compute_share_trend removes steps at break months", {
  panel <- sim_panel(betas = rep(0.03, 5), jump = 0.5, jump_at = 20)
  raw <- compute_share_trend(panel, "skill_id")
  brk <- compute_share_trend(panel, "skill_id", break_times = 20)
  expect_lt(max(abs(brk$pendenza - 0.03)), 0.005)
  expect_true(all(raw$pendenza > 0.04))
})

test_that("compute_share_trend nets out the common drift", {
  # most skills share the drift, a minority changes genuinely
  betas <- c(rep(0, 15), 0.05, 0.04, -0.05)
  # large denominators keep the noise of each difference below the
  # genuine changes, the regime the robust median is designed for
  panel <- sim_panel(betas = betas, common = 0.02, n = 1e6L)
  drift <- compute_drift_index(panel, "skill_id")
  res <- compute_share_trend(panel, "skill_id", drift = drift)
  res <- merge(res, unique(panel[, list(skill_id, beta)]), by = "skill_id")
  expect_lt(max(abs(res$pendenza_drift[1L] - (0.02))), 0.005)
  expect_lt(max(abs(res$pendenza - (res$beta))), 0.006)
})

test_that("compute_share_trend uses the trailing window", {
  panel <- sim_panel(betas = 0.04, n_months = 30)
  res <- compute_share_trend(panel, "skill_id", window = 12)
  expect_identical(res$n_mesi, 12L)
  expect_lt(max(abs(res$pendenza - (0.04))), 0.01)
})

test_that("compute_share_trend validates input", {
  panel <- sim_panel(betas = 0.01, n_months = 6)
  expect_error(
    compute_share_trend(panel, "missing_key"),
    "missing required columns"
  )
  expect_error(compute_share_trend(panel, "skill_id", window = 2), "`window`")
  bad <- data.table::copy(panel)[1L, x := n + 1L]
  expect_error(compute_share_trend(bad, "skill_id"), "must not exceed")
})

# 2b. trend weights -----

test_that("pooled weights do not reward a spike that collapses to zero", {
  panel <- data.table::data.table(
    skill_id = "k",
    mese_idx = 1:24,
    n = 5000,
    x = c(rep(0, 12), 400, 900, 1200, rep(0, 9))
  )
  pooled <- compute_share_trend(panel, "skill_id")
  expect_true(pooled$pendenza < 0 || pooled$p_adj >= 0.05)

  # observed weights reproduce the 0.6.0 estimator, a plain weighted lm()
  observed <- compute_share_trend(panel, "skill_id", weights = "observed")
  d <- data.table::copy(panel)
  d[, `:=`(
    y = stats::qlogis((x + 0.5) / (n + 1)),
    w = 1 / (1 / (x + 0.5) + 1 / (n - x + 0.5))
  )]
  ref <- unname(stats::coef(stats::lm(y ~ mese_idx, d, weights = w))[2L])
  expect_equal(observed$pendenza, ref, tolerance = 1e-8)
  expect_true(observed$pendenza > 0.3 && observed$p_adj < 0.05)

  acc_obs <- compute_share_acceleration(panel, "skill_id", weights = "observed")
  acc_pool <- compute_share_acceleration(panel, "skill_id")
  expect_false(isTRUE(all.equal(acc_obs$accelerazione, acc_pool$accelerazione)))
})

test_that("pooled and observed weights agree on a constant share", {
  panel <- sim_panel(betas = rep(0, 6), n = 1e6L)
  pooled <- compute_share_trend(panel, "skill_id")
  observed <- compute_share_trend(panel, "skill_id", weights = "observed")
  expect_lt(max(abs(pooled$pendenza - observed$pendenza)), 1e-4)
  expect_lt(max(abs(pooled$se / observed$se - 1)), 0.01)
  acc_p <- compute_share_acceleration(panel, "skill_id")
  acc_o <- compute_share_acceleration(panel, "skill_id", weights = "observed")
  expect_lt(max(abs(acc_p$accelerazione - acc_o$accelerazione)), 1e-4)
})

test_that("trend functions reject an unknown weights value", {
  panel <- sim_panel(betas = 0, n_months = 24)
  expect_error(compute_share_trend(panel, "skill_id", weights = "equal"),
    "`weights`")
  expect_error(
    compute_share_acceleration(panel, "skill_id", weights = c("a", "b")),
    "`weights`"
  )
  expect_error(
    backtest_emergence(panel, "skill_id", origins = 12, horizon = 6,
      window = 12, trend_weights = "raw"),
    "`weights`"
  )
  inc <- data.table::data.table(general_id = 1, mese_idx = 1, skill_id = "a")
  expect_error(calibrate_min_support(inc, weights = "raw"), "`weights`")
})

# 3. compute_yoy_ratio -----

test_that("compute_yoy_ratio returns ratio and Katz interval", {
  panel <- data.table::CJ(skill_id = "a", mese_idx = 1:15)
  panel[, `:=`(n = 1000L, x = ifelse(mese_idx > 12, 60L, 30L))]
  res <- compute_yoy_ratio(panel, "skill_id")
  expect_equal(res$yoy, 2)
  se <- sqrt(1 / 180 - 1 / 3000 + 1 / 90 - 1 / 3000)
  expect_equal(res$se_log, se)
  expect_equal(res$yoy_lo, exp(log(2) - stats::qnorm(0.975) * se))
  expect_equal(res$x_attuale, 180)
})

test_that("compute_yoy_ratio validates arguments", {
  panel <- data.table::CJ(skill_id = "a", mese_idx = 1:15)
  panel[, `:=`(n = 1000L, x = 10L)]
  expect_error(compute_yoy_ratio(panel, "skill_id", lag = 2), "`lag`")
  expect_error(compute_yoy_ratio(panel, "skill_id", conf = 2), "`conf`")
})

# 4. compute_share_acceleration -----

test_that("compute_share_acceleration measures a change of slope", {
  set.seed(11)
  panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = 1:24)
  panel[, n := 50000L]
  panel[,
    x := stats::rbinom(
      .N,
      n,
      stats::plogis(
        -3 +
          ifelse(skill_id == "a", 0.08, 0) *
            pmax(0, mese_idx - 12)
      )
    )
  ]
  res <- compute_share_acceleration(panel, "skill_id", window = 12)
  expect_lt(max(abs(res[skill_id == "a", accelerazione] - (0.08))), 0.01)
  expect_lt(max(abs(res[skill_id == "b", accelerazione] - (0))), 0.01)
  expect_true(res[skill_id == "a", p_adj] < 0.001)
})

test_that("compute_share_acceleration validates the window", {
  panel <- sim_panel(betas = 0, n_months = 24)
  expect_error(
    compute_share_acceleration(panel, "skill_id", window = 1),
    "`window`"
  )
})

# 5. detect_skill_onset -----

test_that("detect_skill_onset finds the first consolidated month", {
  panel <- data.table::data.table(
    skill_id = rep(c("a", "b"), each = 8),
    mese_idx = rep(1:8, 2),
    x = c(0, 0, 1, 2, 5, 6, 8, 9, 20, 1, 1, 1, 1, 1, 1, 1)
  )
  res <- detect_skill_onset(panel, "skill_id", min_count = 10)
  expect_equal(res[skill_id == "a", prima_osservazione], 3)
  expect_equal(res[skill_id == "a", prima_comparsa], 6)
  expect_equal(res[skill_id == "a", eta_mesi], 2)
  expect_false(res[skill_id == "a", censura_sx])
  expect_true(res[skill_id == "b", censura_sx])

  none <- detect_skill_onset(panel, "skill_id", min_count = 1000)
  expect_true(all(is.na(none$prima_comparsa)))
})

test_that("detect_skill_onset validates min_count", {
  panel <- data.table::data.table(skill_id = "a", mese_idx = 1:3, x = 1)
  expect_error(
    detect_skill_onset(panel, "skill_id", min_count = 0),
    "`min_count`"
  )
})

# 6. compute_rca_panel / 7. compute_diffusion_panel -----

rca_panel <- function() {
  data.table::data.table(
    cp4 = rep(c("p1", "p2"), each = 2),
    skill_id = rep(c("a", "b"), 2),
    mese_idx = 1,
    x = c(30, 5, 10, 20),
    n = 100
  )
}

test_that("compute_rca_panel computes posting-based RCA", {
  res <- compute_rca_panel(
    rca_panel(),
    prof_col = "cp4",
    windows = list(W1 = c(1, 1))
  )
  expect_equal(res[cp4 == "p1" & skill_id == "a", rca], 1.5)
  expect_equal(res[cp4 == "p2" & skill_id == "a", rca], 0.5)
  expect_equal(res[cp4 == "p2" & skill_id == "b", rca], 1.6)
  expect_equal(res[skill_id == "b", quota_skill][1L], 0.125)
})

test_that("compute_rca_panel validates windows and denominators", {
  expect_error(
    compute_rca_panel(rca_panel(), prof_col = "cp4", windows = list(c(1, 1))),
    "named list"
  )
  bad <- rca_panel()[1L, n := 50]
  expect_error(
    compute_rca_panel(bad, prof_col = "cp4", windows = list(W1 = c(1, 1))),
    "constant"
  )
})

test_that("compute_diffusion_panel counts RCA professions and entropy", {
  p <- rca_panel()
  p2 <- data.table::copy(p)[, `:=`(mese_idx = 2, x = c(20, 20, 20, 20))]
  rca <- compute_rca_panel(
    rbind(p, p2),
    prof_col = "cp4",
    windows = list(W1 = c(1, 1), W2 = c(2, 2))
  )
  res <- compute_diffusion_panel(rca, prof_col = "cp4")
  h <- -(0.75 * log(0.75) + 0.25 * log(0.25)) / log(2)
  expect_equal(res[skill_id == "a" & finestra == "W1", entropia], h)
  expect_identical(res[skill_id == "a" & finestra == "W1", n_prof_rca], 1L)
  expect_equal(res[skill_id == "a" & finestra == "W2", entropia], 1)

  wide <- compute_diffusion_panel(
    rca,
    prof_col = "cp4",
    compare = c("W1", "W2")
  )
  expect_equal(wide[skill_id == "a", delta_entropia], 1 - h)
  expect_identical(wide[skill_id == "a", delta_n_prof_rca], 1L)
})

test_that("compute_diffusion_panel validates compare", {
  rca <- compute_rca_panel(
    rca_panel(),
    prof_col = "cp4",
    windows = list(W1 = c(1, 1))
  )
  expect_error(
    compute_diffusion_panel(rca, prof_col = "cp4", compare = c("W1", "W9")),
    "`compare`"
  )
  expect_error(
    compute_diffusion_panel(rca[, -"rca"], prof_col = "cp4"),
    "missing required columns"
  )
})

# 8. compute_drift_index -----

test_that("compute_drift_index recovers a common slope", {
  panel <- sim_panel(betas = rep(0, 15), common = 0.03, n_months = 24)
  res <- compute_drift_index(panel, "skill_id")
  expect_equal(res$drift_livello[1L], 0)
  expect_true(is.na(res$drift[1L]))
  expect_lt(max(abs(mean(res$drift, na.rm = TRUE) - (0.03))), 0.005)
  expect_identical(res$n_serie[-1L], rep(15L, 23L))
})

test_that("compute_drift_index validates input", {
  panel <- sim_panel(betas = 0, n_months = 5)
  expect_error(
    compute_drift_index(panel, "skill_id", x_col = "nope"),
    "missing required columns"
  )
})

# 9. detect_taxonomy_drift -----

test_that("detect_taxonomy_drift flags break months and artefact skills", {
  q <- data.table::data.table(
    mese_idx = 1:30,
    skill_per_annuncio = c(rep(10, 19), rep(12, 11)) + sin(1:30) / 20
  )
  res <- detect_taxonomy_drift(q, metrics = "skill_per_annuncio")
  expect_identical(res$mesi[rottura == TRUE, mese_idx], 20L)

  # skills present from month 1, ordinary new skills and an artefact
  old <- data.table::CJ(
    skill_id = sprintf("o%02d", 1:5),
    cp4 = sprintf("p%02d", 1:20),
    mese_idx = 1:30
  )
  old[, x := 3]
  new <- data.table::rbindlist(lapply(1:8, function(i) {
    data.table::CJ(
      skill_id = sprintf("n%02d", i),
      cp4 = sprintf("p%02d", seq_len(1 + i %% 3)),
      mese_idx = (3 * i + 1):30
    )[, x := 1]
  }))
  art <- data.table::CJ(
    skill_id = "art",
    cp4 = sprintf("p%02d", 1:18),
    mese_idx = 20:30
  )
  art[, x := 2]
  pp <- rbind(old, new, art)
  res2 <- detect_taxonomy_drift(
    q,
    metrics = "skill_per_annuncio",
    panel_prof = pp,
    prof_col = "cp4"
  )
  expect_true(res2$skill[skill_id == "art", flag_tassonomia])
  expect_false(any(res2$skill[skill_id != "art", flag_tassonomia]))
})

test_that("detect_taxonomy_drift validates input", {
  q <- data.table::data.table(mese_idx = 1:5, m = 1:5)
  expect_error(
    detect_taxonomy_drift(q, metrics = "nope"),
    "missing required columns"
  )
  expect_error(detect_taxonomy_drift(q, metrics = "m", prob = 1.5), "`prob`")
  expect_error(
    detect_taxonomy_drift(q, metrics = "m", panel_prof = q),
    "`prof_col`"
  )
})

# 10. calibrate_min_support -----

sim_incidence <- function(
  n_skill = 30L,
  n_months = 12L,
  per_month = 600L,
  seed = 2L
) {
  set.seed(seed)
  slopes <- seq(-0.08, 0.08, length.out = n_skill)
  base <- rep(c(-3, -2, -1.5), length.out = n_skill)
  data.table::rbindlist(lapply(seq_len(n_months), function(m) {
    ids <- m * 10000L + seq_len(per_month)
    core <- data.table::data.table(general_id = ids, skill_id = "base")
    sk <- data.table::rbindlist(lapply(seq_len(n_skill), function(s) {
      p <- stats::plogis(base[s] + slopes[s] * m)
      data.table::data.table(
        general_id = ids[stats::runif(per_month) < p],
        skill_id = sprintf("k%02d", s)
      )
    }))
    rbind(core, sk)[, mese_idx := m]
  }))
}

test_that("calibrate_min_support selects a reliable threshold", {
  inc <- sim_incidence()
  res <- calibrate_min_support(
    inc,
    window = 12,
    grid = c(5, 50, 1e6),
    min_series = 10
  )
  expect_named(res, c("tabella", "n_min"))
  expect_equal(res$n_min, 5)
  expect_gt(res$tabella[n_min == 5, rho], 0.7)
  expect_identical(res$tabella[n_min == 1e6, n_serie], 0L)

  # the split is reproducible and leaves the RNG state untouched
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  again <- calibrate_min_support(inc, window = 12, grid = 5, min_series = 10)
  after <- stats::runif(1)
  expect_equal(before, after)
  expect_equal(again$tabella$rho, res$tabella[n_min == 5, rho])
})

test_that("calibrate_min_support validates input", {
  inc <- data.table::data.table(general_id = 1, mese_idx = 1, skill_id = "a")
  expect_error(calibrate_min_support(inc, grid = "a"), "`grid`")
  expect_error(
    calibrate_min_support(inc[, -"skill_id"]),
    "missing required columns"
  )
})

# 11. score_emergence -----

test_that("score_emergence scores, ranks and labels series", {
  ind <- data.table::data.table(
    skill_id = letters[1:7],
    pendenza = c(0.10, 0.05, 0.00, -0.04, 0.02, 0.08, 0.09),
    p_adj = c(0.001, 0.01, 0.9, 0.01, 0.5, 0.001, 0.001),
    eta_mesi = c(30, 30, 30, 30, 30, 5, 30),
    artefatto = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  res <- score_emergence(
    ind,
    "skill_id",
    weights = "slope",
    age_col = "eta_mesi",
    flag_col = "artefatto",
    q_star = 0.8
  )
  expect_identical(
    res[order(rango), skill_id],
    c("a", "g", "f", "b", "e", "c", "d")
  )
  expect_identical(
    res$stato,
    c(
      "Emergente",
      "In crescita",
      "Stabile",
      "In calo",
      "Stabile",
      "Nuova non consolidata",
      "Artefatto di tassonomia"
    )
  )
  expect_equal(unname(attr(res, "pesi")), 1)

  ind[, novita := -eta_mesi]
  eq <- score_emergence(ind, "skill_id", components = c("pendenza", "novita"))
  expect_equal(unname(attr(eq, "pesi")), c(0.5, 0.5))
  pc <- score_emergence(
    ind,
    "skill_id",
    components = c("pendenza", "novita"),
    weights = "pc1"
  )
  expect_equal(sum(abs(attr(pc, "pesi"))), 1)
})

test_that("score_emergence validates arguments", {
  ind <- data.table::data.table(skill_id = "a", pendenza = 0.1, p_adj = 0.01)
  expect_error(
    score_emergence(ind, "skill_id", directions = c(1, 1)),
    "`directions`"
  )
  expect_error(score_emergence(ind, "skill_id", weights = "other"), "`weights`")
  expect_error(
    score_emergence(ind, "skill_id", components = "nope"),
    "missing required columns"
  )
})

# 12. backtest_emergence -----

test_that("backtest_emergence evaluates the score on a clear signal", {
  set.seed(5)
  panel <- data.table::CJ(skill_id = sprintf("s%02d", 1:30), mese_idx = 1:30)
  panel[, beta := ifelse(as.integer(substr(skill_id, 2, 3)) <= 8, 0.06, 0)]
  panel[, n := 3000L]
  panel[, x := stats::rbinom(.N, n, stats::plogis(-3.5 + beta * mese_idx))]
  bt <- backtest_emergence(
    panel,
    "skill_id",
    origins = c(16, 18),
    horizon = 12,
    window = 16,
    k = 5,
    q_grid = c(0.7, 0.9)
  )
  expect_named(bt, c("griglia", "dettaglio", "migliore"))
  expect_identical(nrow(bt$griglia), 6L)
  expect_identical(nrow(bt$dettaglio), 12L)
  expect_true(all(bt$griglia$f1 >= 0 & bt$griglia$f1 <= 1))
  expect_gt(bt$migliore$f1, 0.5)
  expect_equal(bt$griglia[pesi == "slope", precision_at_k], c(1, 1))
})

test_that("backtest_emergence rejects origins without room", {
  panel <- sim_panel(betas = c(0, 0.01), n_months = 20)
  expect_error(
    backtest_emergence(
      panel,
      "skill_id",
      origins = 15,
      horizon = 12,
      window = 12
    ),
    "leave no room"
  )
  expect_error(
    backtest_emergence(
      panel,
      "skill_id",
      origins = 12,
      horizon = 6,
      window = 12,
      indicators_fun = "x"
    ),
    "`indicators_fun`"
  )
})
