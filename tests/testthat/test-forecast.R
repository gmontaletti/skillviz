# Tests for forecast.R -----

# 0. synthetic data helpers -----

fc_panel <- function(n_months = 30L, seed = 7L) {
  set.seed(seed)
  panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = seq_len(n_months))
  panel[, n := 2000L]
  panel[,
    x := stats::rbinom(
      .N,
      n,
      stats::plogis(-3 + 0.02 * mese_idx + 0.3 * sin(2 * pi * mese_idx / 12))
    )
  ]
  panel[]
}

# 1. prepare_share_tsibble -----

test_that("prepare_share_tsibble builds a monthly tsibble", {
  skip_if_not_installed("tsibble")
  tsb <- prepare_share_tsibble(fc_panel(), "skill_id", origin = "2023-01")
  expect_s3_class(tsb, "tbl_ts")
  expect_identical(tsibble::key_vars(tsb), "skill_id")
  expect_identical(nrow(tsb), 60L)
  expect_equal(tsb$logit_quota, stats::qlogis((tsb$x + 0.5) / (tsb$n + 1)))
  expect_identical(as.character(as.Date(min(tsb$mese))), "2023-01-01")

  gap <- fc_panel()[!(skill_id == "b" & mese_idx == 5)]
  expect_warning(
    tsb2 <- prepare_share_tsibble(gap, "skill_id", origin = "2023-01"),
    "dropped 1 series"
  )
  expect_identical(unique(tsb2$skill_id), "a")
})

test_that("prepare_share_tsibble validates input", {
  skip_if_not_installed("tsibble")
  expect_error(prepare_share_tsibble(fc_panel(), "skill_id"), "`origin`")
  expect_error(
    prepare_share_tsibble(fc_panel(), "nope", origin = "2023-01"),
    "missing required columns"
  )
})

# 2. backtest_share_models -----

test_that("backtest_share_models scores rolling-origin forecasts", {
  skip_if_not_installed("fable")
  skip_if_not_installed("distributional")
  tsb <- prepare_share_tsibble(fc_panel(), "skill_id", origin = "2023-01")
  bt <- backtest_share_models(
    tsb,
    models = c("SNAIVE", "DRIFT"),
    origins = c("2024-06", "2024-09"),
    horizon = 3
  )
  expect_named(bt, c("dettaglio", "sintesi", "key_cols"))
  expect_identical(nrow(bt$dettaglio), 2L * 2L * 2L * 3L)
  expect_identical(sort(unique(bt$dettaglio$h)), 1:3)
  expect_true(all(
    bt$dettaglio$lo95 <= bt$dettaglio$lo80 &
      bt$dettaglio$lo80 <= bt$dettaglio$quota &
      bt$dettaglio$quota <= bt$dettaglio$hi80 &
      bt$dettaglio$hi80 <= bt$dettaglio$hi95
  ))
  expect_identical(nrow(bt$sintesi), 4L)
  expect_true(all(bt$sintesi$mase > 0))
  expect_true(all(bt$sintesi$copertura95 >= 0 & bt$sintesi$copertura95 <= 1))

  # SNAIVE at horizon 1 repeats the observation of 12 months earlier
  obs <- data.table::as.data.table(as.data.frame(tsb))
  obs[, mese := as.Date(mese)]
  sn <- bt$dettaglio[
    modello == "SNAIVE" &
      skill_id == "a" &
      origine == as.Date("2024-06-01") &
      h == 1L
  ]
  expect_equal(
    sn$quota,
    stats::plogis(obs[
      skill_id == "a" & mese == as.Date("2023-07-01"),
      logit_quota
    ])
  )
})

test_that("backtest_share_models rejects unknown models", {
  skip_if_not_installed("fable")
  tsb <- prepare_share_tsibble(fc_panel(), "skill_id", origin = "2023-01")
  expect_error(
    backtest_share_models(tsb, models = "PROPHET", origins = "2024-06"),
    "models must be"
  )
  expect_error(
    backtest_share_models(fc_panel(), origins = "2024-06"),
    "missing required columns"
  )
})

# 3. select_share_model -----

test_that("select_share_model picks the best model with fallback", {
  bt <- list(
    sintesi = data.table::data.table(
      skill_id = rep(c("a", "b", "c"), each = 2),
      modello = rep(c("SNAIVE", "ETS"), 3),
      mase = c(1.0, 0.8, 1.2, 0.9, 0.7, 0.9)
    ),
    key_cols = "skill_id"
  )
  res <- select_share_model(bt)
  expect_identical(res$strati$modello, "ETS")
  expect_equal(res$strati$quota_vittorie, 2 / 3)
  expect_false(res$strati$ripiego)
  expect_identical(res$serie$modello, rep("ETS", 3L))

  strict <- select_share_model(bt, min_win = 0.9)
  expect_identical(strict$strati$modello, "SNAIVE")
  expect_true(strict$strati$ripiego)

  strata <- data.table::data.table(
    skill_id = c("a", "b", "c"),
    strato = c("alto", "alto", "basso")
  )
  by_st <- select_share_model(bt, strata = strata)
  expect_identical(by_st$strati$modello, c("ETS", "SNAIVE"))
})

test_that("select_share_model validates input", {
  bt <- list(
    sintesi = data.table::data.table(skill_id = "a", modello = "ETS", mase = 1),
    key_cols = "skill_id"
  )
  expect_error(select_share_model(bt), "baseline")
  expect_error(select_share_model(list()), "backtest_share_models")
})

# 4. forecast_share -----

test_that("forecast_share returns nested intervals on the share scale", {
  skip_if_not_installed("fable")
  skip_if_not_installed("distributional")
  tsb <- prepare_share_tsibble(fc_panel(24), "skill_id", origin = "2023-01")
  fc <- forecast_share(tsb, model = "DRIFT", horizon = 3)
  expect_named(
    fc,
    c("skill_id", "mese", "quota", "lo80", "hi80", "lo95", "hi95", "modello")
  )
  expect_identical(nrow(fc), 6L)
  expect_identical(unique(fc$modello), "DRIFT")
  expect_identical(
    as.character(fc$mese[1:3]),
    c("2025-01-01", "2025-02-01", "2025-03-01")
  )
  expect_true(all(
    fc$lo95 <= fc$lo80 &
      fc$lo80 <= fc$quota &
      fc$quota <= fc$hi80 &
      fc$hi80 <= fc$hi95
  ))
  expect_true(all(fc$lo95 > 0 & fc$hi95 < 1))

  assign <- data.table::data.table(
    skill_id = c("a", "b"),
    modello = c("SNAIVE", "DRIFT")
  )
  fc2 <- forecast_share(tsb, model = assign, horizon = 2)
  expect_identical(fc2$modello, c("SNAIVE", "SNAIVE", "DRIFT", "DRIFT"))
})

test_that("forecast_share validates the model argument", {
  skip_if_not_installed("fable")
  tsb <- prepare_share_tsibble(fc_panel(24), "skill_id", origin = "2023-01")
  expect_error(forecast_share(tsb, model = "LSTM"), "models must be")
  expect_error(forecast_share(tsb, model = c("ETS", "ARIMA")), "single name")
})
