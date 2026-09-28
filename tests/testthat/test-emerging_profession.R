# Tests for emerging_profession.R -----

# 0. synthetic data helpers -----

# Two professions over two months: p1 moves from skills (a, b) to (a, c),
# p2 keeps the same profile. Large denominators make sampling noise
# negligible.
toy_panel <- function(n = 1e6) {
  data.table::data.table(
    cp4 = rep(c("p1", "p2"), each = 6),
    skill_id = rep(c("a", "b", "c"), 4),
    mese_idx = rep(rep(1:2, each = 3), 2),
    x = c(0.5, 0.5, 0, 0.5, 0, 0.5, 0.3, 0.1, 0, 0.3, 0.1, 0) * n,
    n = n
  )
}

toy_pair <- function(n = 1e6) {
  build_profile_pair(toy_panel(n), "cp4", base = c(1, 1), target = c(2, 2))
}

toy_incidence <- function(seed = 6L) {
  set.seed(seed)
  data.table::data.table(
    general_id = rep(1:400, each = 2),
    mese_idx = rep(rep(1:2, each = 200), each = 2),
    cp4 = rep(c("p1", "p2"), each = 2, times = 200),
    skill_id = sample(letters[1:5], 800, replace = TRUE)
  )
}

# 1. build_profile_pair -----

test_that("build_profile_pair builds sparse share profiles", {
  pair <- toy_pair()
  expect_s3_class(pair, "skillviz_profile_pair")
  expect_s4_class(pair$a, "dgCMatrix")
  expect_identical(dim(pair$a), c(3L, 2L))
  expect_equal(pair$a["b", "p1"], 0.5)
  expect_equal(pair$b["c", "p1"], 0.5)
  expect_equal(unname(pair$n_a), c(1e6, 1e6))
  expect_identical(pair$prof_col, "cp4")
})

test_that("build_profile_pair validates windows and denominators", {
  expect_error(
    build_profile_pair(toy_panel(), "cp4", base = c(2, 1), target = c(2, 2)),
    "`base`"
  )
  bad <- toy_panel()[1L, n := 2e6]
  expect_error(
    build_profile_pair(bad, "cp4", base = c(1, 1), target = c(2, 2)),
    "constant"
  )
  expect_error(
    build_profile_pair(toy_panel(), "cp4", base = c(1, 1), target = c(5, 5)),
    "both windows"
  )
})

# 2. compute_profile_turnover -----

test_that("compute_profile_turnover matches hand-computed values", {
  res <- compute_profile_turnover(toy_pair())
  p1 <- res[cp4 == "p1"]
  expect_equal(p1$turnover_coseno, 0.5)
  expect_equal(p1$d2, 0.5)
  # pooled profile (0.5, 0.25, 0.25): sum s(1 - s) = 0.625
  expect_equal(p1$d2_atteso, 0.625 * 2 / 1e6)
  expect_equal(p1$turnover_netto, (0.5 - 1.25e-6) / 0.375)
  p2 <- res[cp4 == "p2"]
  expect_equal(p2$turnover_coseno, 0, tolerance = 1e-12)
  expect_equal(p2$turnover_netto, 0)
})

test_that("compute_profile_turnover requires a profile pair", {
  expect_error(compute_profile_turnover(list(a = 1)), "build_profile_pair")
})

# 3. decompose_profile_change -----

test_that("decompose_profile_change is additive within each group type", {
  pair <- toy_pair()
  groups <- data.table::data.table(
    skill_id = c("a", "b", "c"),
    green = c("no", "no", "si"),
    area = c("x", NA, "y")
  )
  dec <- decompose_profile_change(pair, groups, c("green", "area"))
  tot <- compute_profile_turnover(pair)
  sums <- dec[, list(d2 = sum(d2_gruppo)), by = c("cp4", "tipo_gruppo")]
  sums <- merge(sums, tot[, list(cp4, d2_tot = d2)], by = "cp4")
  expect_equal(sums$d2, sums$d2_tot)
  expect_equal(
    dec[cp4 == "p1" & tipo_gruppo == "green" & gruppo == "si", quota],
    0.5
  )
  expect_true("(mancante)" %in% dec[tipo_gruppo == "area", gruppo])
})

test_that("decompose_profile_change validates skill_groups", {
  pair <- toy_pair()
  dup <- data.table::data.table(skill_id = c("a", "a"), green = c("x", "y"))
  expect_error(decompose_profile_change(pair, dup, "green"), "one row per")
  expect_error(
    decompose_profile_change(
      pair,
      data.table::data.table(skill_id = "a"),
      "green"
    ),
    "missing required columns"
  )
})

# 4. compute_turnover_null -----

test_that("compute_turnover_null returns reproducible null quantiles", {
  inc <- toy_incidence()
  set.seed(42)
  before <- stats::runif(1)
  set.seed(42)
  res <- compute_turnover_null(
    inc,
    "cp4",
    base = c(1, 1),
    target = c(2, 2),
    n_rep = 5
  )
  after <- stats::runif(1)
  expect_equal(before, after)
  expect_identical(res$n_nullo, c(10L, 10L))
  expect_true(all(res$q_nullo >= res$media_nullo))
  expect_true(all(res$q_nullo >= 0))
  again <- compute_turnover_null(
    inc,
    "cp4",
    base = c(1, 1),
    target = c(2, 2),
    n_rep = 5
  )
  expect_equal(again$q_nullo, res$q_nullo)
})

test_that("compute_turnover_null validates arguments", {
  inc <- toy_incidence()
  expect_error(
    compute_turnover_null(
      inc,
      "cp4",
      base = c(1, 1),
      target = c(2, 2),
      prob = 1
    ),
    "`prob`"
  )
  expect_error(
    compute_turnover_null(inc, "nope", base = c(1, 1), target = c(2, 2)),
    "missing required columns"
  )
})

# 5. compute_change_breadth -----

test_that("compute_change_breadth counts and ranks changes", {
  pair <- toy_pair()
  trend <- data.table::data.table(
    cp4 = c("p1", "p1", "p1", "p2"),
    skill_id = c("a", "b", "c", "a"),
    pendenza = c(0, -0.2, 0.3, 0.1),
    p_adj = c(0.8, 0.01, 0.01, 0.5)
  )
  groups <- data.table::data.table(
    skill_id = c("a", "b", "c"),
    area = c("x", "x", "y")
  )
  res <- compute_change_breadth(trend, pair, groups, "area")
  p1 <- res[cp4 == "p1"]
  expect_identical(p1$n_skill_crescita, 1L)
  expect_identical(p1$n_skill_calo, 1L)
  expect_identical(p1$ampiezza_gruppi, 2L)
  expect_equal(p1$entropia_cambi, 1)
  expect_identical(res$rango_ampiezza, c(1L, 2L))
  expect_identical(res[cp4 == "p2", n_skill_cambio], 0L)
})

test_that("compute_change_breadth validates input", {
  pair <- toy_pair()
  groups <- data.table::data.table(skill_id = "a", area = "x")
  bad <- data.table::data.table(cp4 = "p1", skill_id = "a")
  expect_error(
    compute_change_breadth(bad, pair, groups, "area"),
    "missing required columns"
  )
  expect_error(
    compute_change_breadth(bad, list(), groups, "area"),
    "build_profile_pair"
  )
})

# 6. compute_emerging_uptake -----

test_that("compute_emerging_uptake measures the share with emerging skills", {
  inc <- data.table::data.table(
    general_id = c(1, 1, 2, 3, 4, 4, 5, 6),
    mese_idx = c(1, 1, 1, 1, 2, 2, 2, 2),
    cp4 = c("p1", "p1", "p1", "p2", "p1", "p1", "p1", "p2"),
    skill_id = c("a", "e", "b", "a", "e", "a", "e", "a")
  )
  res <- compute_emerging_uptake(
    inc,
    emerging = "e",
    prof_col = "cp4",
    base = c(1, 1),
    target = c(2, 2),
    n_boot = 50
  )
  p1 <- res$professioni[cp4 == "p1"]
  expect_equal(p1$quota_base, 0.5)
  expect_equal(p1$quota_target, 1)
  expect_equal(p1$delta_quota, 0.5)
  expect_identical(p1$n_emergenti_profilo, 1L)
  expect_equal(res$regione$quota_base, 1 / 3)
  expect_equal(res$regione$delta_quota, 2 / 3 - 1 / 3)
  expect_true(res$regione$delta_lo <= res$regione$delta_hi)
})

test_that("compute_emerging_uptake validates arguments", {
  inc <- toy_incidence()
  expect_error(
    compute_emerging_uptake(
      inc,
      emerging = character(0),
      prof_col = "cp4",
      base = c(1, 1),
      target = c(2, 2)
    ),
    "`emerging`"
  )
  expect_error(
    compute_emerging_uptake(
      inc,
      emerging = "a",
      prof_col = "cp4",
      base = c(1, 1),
      target = c(2, 2),
      core_share = 2
    ),
    "`core_share`"
  )
})

# 7. classify_emerging_professions -----

test_that("classify_emerging_professions applies the three conditions", {
  demand <- data.table::data.table(
    cp4 = c("p1", "p2", "p3"),
    pendenza = c(0.05, 0.05, -0.02),
    p_adj = c(0.01, 0.01, 0.01)
  )
  turnover <- data.table::data.table(
    cp4 = c("p1", "p2", "p3"),
    turnover_netto = c(0.3, 0.01, 0.3)
  )
  null <- data.table::data.table(cp4 = c("p1", "p2", "p3"), q_nullo = 0.05)
  uptake <- list(
    professioni = data.table::data.table(
      cp4 = c("p1", "p2", "p3"),
      delta_quota = c(0.2, 0.2, 0.2)
    ),
    regione = data.table::data.table(delta_hi = 0.05)
  )
  res <- classify_emerging_professions(demand, turnover, null, uptake, "cp4")
  expect_identical(res$emergente, c(TRUE, FALSE, FALSE))
  expect_identical(res$turnover_oltre_nullo, c(TRUE, FALSE, TRUE))
  expect_identical(res$crescita_significativa, c(TRUE, TRUE, FALSE))
})

test_that("classify_emerging_professions validates uptake", {
  demand <- data.table::data.table(cp4 = "p1", pendenza = 0.1, p_adj = 0.01)
  turnover <- data.table::data.table(cp4 = "p1", turnover_netto = 0.3)
  null <- data.table::data.table(cp4 = "p1", q_nullo = 0.05)
  expect_error(
    classify_emerging_professions(demand, turnover, null, list(), "cp4"),
    "compute_emerging_uptake"
  )
  expect_error(
    classify_emerging_professions(
      demand[, -"p_adj"],
      turnover,
      null,
      list(),
      "cp4"
    ),
    "missing required columns"
  )
})
