# Forecast of skill shares -----
#
# Share forecasts on the logit scale with fable: tsibble preparation,
# rolling-origin backtest, model selection by stratum and forecasts
# with back-transformed prediction intervals.

# 0. internal helpers -----

.FC_MODELS <- c("SNAIVE", "DRIFT", "ETS", "ARIMA")

#' Check forecast model names
#'
#' @param models Character vector of model names.
#' @param caller Calling function.
#' @keywords internal
#' @noRd
.fc_check_models <- function(models, caller) {
  if (
    !is.character(models) ||
      length(models) == 0L ||
      !all(models %in% .FC_MODELS)
  ) {
    stop(
      caller,
      ": models must be among ",
      paste(.FC_MODELS, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Convert a share tsibble to a data.table with Date months
#'
#' @param tsb A tsibble from [prepare_share_tsibble()].
#' @param caller Calling function.
#' @return A list with `dt` and `key_cols`.
#' @keywords internal
#' @noRd
.fc_to_dt <- function(tsb, caller) {
  if (!inherits(tsb, "tbl_ts")) {
    stop(
      caller,
      ": `tsb` must be a tsibble from prepare_share_tsibble()",
      call. = FALSE
    )
  }
  check_columns(tsb, c("mese", "quota", "logit_quota"), caller = caller)
  key_cols <- tsibble::key_vars(tsb)
  if (length(key_cols) == 0L) {
    stop(caller, ": `tsb` must have at least one key column", call. = FALSE)
  }
  dt <- data.table::as.data.table(as.data.frame(tsb))
  dt[, mese := as.Date(mese)]
  list(dt = dt, key_cols = key_cols)
}

#' Build a tsibble from a data.table with Date months
#'
#' @param dt data.table with `key_cols`, `mese` (Date) and
#'   `logit_quota`.
#' @param key_cols Key columns.
#' @return A tsibble indexed by yearmonth.
#' @keywords internal
#' @noRd
.fc_to_tsibble <- function(dt, key_cols) {
  d <- data.table::copy(dt)
  d[, mese := tsibble::yearmonth(mese)]
  tsibble::as_tsibble(as.data.frame(d), key = !!key_cols, index = "mese")
}

#' Fit models and forecast back-transformed quantiles
#'
#' Each model is fitted to the logit share `logit_quota`; the quantiles
#' of the forecast distribution are mapped back with the logistic
#' function, which preserves quantiles because it is monotone.
#'
#' @param dt Training data.table (Date months).
#' @param key_cols Key columns.
#' @param models Model names.
#' @param h Horizon.
#' @return A data.table with `key_cols`, `modello`, `mese`, `quota`,
#'   `lo80`, `hi80`, `lo95`, `hi95`.
#' @keywords internal
#' @noRd
.fc_fit_forecast <- function(dt, key_cols, models, h) {
  specs <- list(
    SNAIVE = fable::SNAIVE(logit_quota ~ lag("year")),
    DRIFT = fable::RW(logit_quota ~ drift()),
    ETS = fable::ETS(logit_quota),
    ARIMA = fable::ARIMA(logit_quota)
  )[models]
  tsb <- .fc_to_tsibble(dt, key_cols)
  mb <- suppressWarnings(do.call(fabletools::model, c(list(tsb), specs)))
  fc <- suppressWarnings(fabletools::forecast(mb, h = h))
  q <- function(p) {
    stats::plogis(as.numeric(stats::quantile(fc$logit_quota, p)))
  }
  out <- data.table::as.data.table(lapply(
    stats::setNames(key_cols, key_cols),
    function(k) fc[[k]]
  ))
  out[, `:=`(
    modello = as.character(fc$.model),
    mese = as.Date(fc$mese),
    quota = q(0.5),
    lo80 = q(0.1),
    hi80 = q(0.9),
    lo95 = q(0.025),
    hi95 = q(0.975)
  )]
  out[]
}

#' Month difference between two first-of-month Dates
#'
#' @param a,b Dates.
#' @return Integer number of months from `b` to `a`.
#' @keywords internal
#' @noRd
.month_diff <- function(a, b) {
  la <- as.POSIXlt(a)
  lb <- as.POSIXlt(b)
  as.integer((la$year - lb$year) * 12L + (la$mon - lb$mon))
}

# 1. prepare_share_tsibble -----

#' Prepare a tsibble of skill shares for forecasting
#'
#' Converts a share panel into a monthly tsibble with the share and its
#' logit, the response modelled by [backtest_share_models()] and
#' [forecast_share()].
#'
#' @details
#' `logit_quota` is \eqn{\mathrm{logit}((x + 0.5)/(n + 1))}, clamped to
#' \eqn{[\epsilon, 1 - \epsilon]} before the logit, which is finite for
#' zero counts. Series with missing months between their first and last
#' month, or with `n = 0` in some month, are dropped with a warning,
#' since the models require regular series without gaps.
#'
#' @param panel A share panel (e.g. from [compute_share_panel()]).
#' @param key_cols Series key columns.
#' @param time_col Month column: a numeric month index (requires
#'   `origin`), a Date, or a character `"YYYY-MM"`/`"YYYY-MM-DD"`.
#'   Default `"mese_idx"`.
#' @param x_col,n_col Count and denominator columns.
#' @param origin Month of index 1 when `time_col` is numeric, as
#'   `"YYYY-MM"` or Date. Default `NULL`.
#' @param eps Clamp of the share before the logit. Default `1e-6`.
#' @return A tsibble with key `key_cols`, index `mese` (yearmonth) and
#'   columns `x`, `n`, `quota` (`x / n`) and `logit_quota`.
#' @examples
#' if (requireNamespace("tsibble", quietly = TRUE)) {
#'   panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = 1:24)
#'   panel[, `:=`(n = 1000L, x = 50L)]
#'   prepare_share_tsibble(panel, "skill_id", origin = "2023-01")
#' }
#' @export
prepare_share_tsibble <- function(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  origin = NULL,
  eps = 1e-6
) {
  caller <- "prepare_share_tsibble"
  if (!is.data.frame(panel)) {
    stop(caller, ": `panel` must be a data.frame", call. = FALSE)
  }
  check_columns(panel, c(key_cols, time_col, x_col, n_col), caller = caller)
  check_suggests("tsibble", "to build monthly time series")
  if (!is.numeric(eps) || length(eps) != 1L || eps <= 0 || eps >= 0.5) {
    stop(caller, ": `eps` must be in (0, 0.5)", call. = FALSE)
  }

  dt <- data.table::as.data.table(panel)[,
    c(key_cols, time_col, x_col, n_col),
    with = FALSE
  ]
  data.table::setnames(dt, c(time_col, x_col, n_col), c(".t", "x", "n"))
  if (is.numeric(dt$.t)) {
    if (is.null(origin)) {
      stop(
        caller,
        ": `origin` is required with a numeric month index",
        call. = FALSE
      )
    }
    ym0 <- tsibble::yearmonth(origin)
    dt[, mese := as.Date(ym0 + (.t - 1))]
  } else {
    dt[, mese := as.Date(tsibble::yearmonth(.t))]
  }
  dt[, .t := NULL]
  if (anyDuplicated(dt, by = c(key_cols, "mese")) > 0L) {
    stop(caller, ": duplicated rows for the same key and month", call. = FALSE)
  }

  data.table::setorderv(dt, c(key_cols, "mese"))
  chk <- dt[,
    list(
      .ok = all(n > 0) &&
        .N == .month_diff(max(mese), min(mese)) + 1L
    ),
    by = key_cols
  ]
  n_bad <- sum(!chk$.ok)
  if (n_bad > 0L) {
    warning(
      caller,
      ": dropped ",
      n_bad,
      " series with missing months or ",
      "zero denominators",
      call. = FALSE
    )
    dt <- merge(dt, chk[.ok == TRUE, key_cols, with = FALSE], by = key_cols)
  }
  dt[, `:=`(
    quota = x / n,
    logit_quota = stats::qlogis(pmin(pmax((x + 0.5) / (n + 1), eps), 1 - eps))
  )]
  .fc_to_tsibble(dt, key_cols)
}

# 2. backtest_share_models -----

#' Rolling-origin backtest of share forecasting models
#'
#' Fits each model with the data available at every origin and compares
#' the forecasts of the next `horizon` months with the observed shares.
#'
#' @details
#' Models, all fitted to `logit_quota` with fable: `"SNAIVE"` (seasonal
#' naive, lag 12), `"DRIFT"` (random walk with drift), `"ETS"`
#' (automatic exponential smoothing, damped trends allowed) and
#' `"ARIMA"` (automatic ARIMA). Forecast quantiles are mapped back to
#' the share scale with the logistic function, so intervals stay in
#' \eqn{[0, 1]}.
#'
#' Accuracy is measured on the share scale. The scaled errors use the
#' in-sample mean absolute (squared) seasonal difference of the training
#' series at lag `season` (lag 1 when the training series is not longer
#' than `season`): the MASE is the mean of the scaled absolute errors,
#' the RMSSE the square root of the mean scaled squared errors. Coverage
#' is the share of observations inside the 80% and 95% intervals. Fits
#' that fail give `NA` forecasts; their warnings are suppressed.
#'
#' @param tsb A tsibble from [prepare_share_tsibble()].
#' @param models Character vector among `"SNAIVE"`, `"DRIFT"`, `"ETS"`,
#'   `"ARIMA"`. Default all four.
#' @param origins Origin months (`"YYYY-MM"` or Date); training data are
#'   the months up to and including each origin.
#' @param horizon Forecast horizon in months. Default `12`.
#' @param season Seasonal period for the error scale. Default `12`.
#' @param min_train Minimum number of training months for a series to
#'   enter an origin. Default `13`.
#' @return A list with:
#'   - `dettaglio`: one row per key, model, origin and horizon with
#'     `origine`, `h`, `mese`, `quota_oss`, `quota`, `lo80`, `hi80`,
#'     `lo95`, `hi95`, `err_scalato`, `err2_scalato`, `in80`, `in95`;
#'   - `sintesi`: one row per key and model with `n_prev`, `mase`,
#'     `rmsse`, `copertura80`, `copertura95`;
#'   - `key_cols`: the key columns.
#' @examples
#' if (requireNamespace("fable", quietly = TRUE)) {
#'   set.seed(7)
#'   panel <- data.table::CJ(skill_id = "a", mese_idx = 1:30)
#'   panel[, n := 2000L]
#'   panel[, x := rbinom(.N, n, plogis(-3 + 0.02 * mese_idx))]
#'   tsb <- prepare_share_tsibble(panel, "skill_id", origin = "2023-01")
#'   bt <- backtest_share_models(tsb, models = c("SNAIVE", "DRIFT"),
#'     origins = "2024-12", horizon = 6)
#'   bt$sintesi
#' }
#' @export
backtest_share_models <- function(
  tsb,
  models = c("SNAIVE", "DRIFT", "ETS", "ARIMA"),
  origins,
  horizon = 12L,
  season = 12L,
  min_train = 13L
) {
  caller <- "backtest_share_models"
  check_columns(tsb, c("mese", "quota", "logit_quota"), caller = caller)
  check_suggests("tsibble", "to build monthly time series")
  check_suggests("fabletools", "to fit forecasting models")
  check_suggests("fable", "for the SNAIVE, RW, ETS and ARIMA models")
  check_suggests("distributional", "for forecast distributions")
  .fc_check_models(models, caller)
  if (!is.numeric(horizon) || length(horizon) != 1L || horizon < 1) {
    stop(caller, ": `horizon` must be a positive integer", call. = FALSE)
  }
  x <- .fc_to_dt(tsb, caller)
  dt <- x$dt
  key_cols <- x$key_cols
  orig <- as.Date(tsibble::yearmonth(origins))

  det <- data.table::rbindlist(lapply(orig, function(o) {
    train <- dt[mese <= o]
    train[, .len := .N, by = key_cols]
    train <- train[.len >= min_train]
    train[, .len := NULL]
    if (nrow(train) == 0L) {
      return(NULL)
    }

    # error scale from the training series
    data.table::setorderv(train, c(key_cols, "mese"))
    sc <- train[,
      {
        m <- if (.N > season) season else 1L
        d <- quota[(m + 1L):.N] - quota[seq_len(.N - m)]
        list(.mae = mean(abs(d)), .mse = mean(d^2))
      },
      by = key_cols
    ]

    fc <- .fc_fit_forecast(train, key_cols, models, horizon)
    obs <- dt[mese > o, c(key_cols, "mese", "quota"), with = FALSE]
    data.table::setnames(obs, "quota", "quota_oss")
    res <- merge(fc, obs, by = c(key_cols, "mese"))
    res <- merge(res, sc, by = key_cols)
    res[, `:=`(origine = o, h = .month_diff(mese, o))]
    res
  }))
  if (nrow(det) == 0L) {
    stop(
      caller,
      ": no series has enough training data at the origins",
      call. = FALSE
    )
  }

  det[, `:=`(
    err_scalato = ifelse(.mae > 0, abs(quota_oss - quota) / .mae, NA_real_),
    err2_scalato = ifelse(.mse > 0, (quota_oss - quota)^2 / .mse, NA_real_),
    in80 = quota_oss >= lo80 & quota_oss <= hi80,
    in95 = quota_oss >= lo95 & quota_oss <= hi95
  )]
  det[, c(".mae", ".mse") := NULL]
  data.table::setcolorder(
    det,
    c(
      key_cols,
      "modello",
      "origine",
      "h",
      "mese",
      "quota_oss",
      "quota",
      "lo80",
      "hi80",
      "lo95",
      "hi95",
      "err_scalato",
      "err2_scalato",
      "in80",
      "in95"
    )
  )
  data.table::setorderv(det, c(key_cols, "modello", "origine", "h"))

  sint <- det[,
    list(
      n_prev = sum(!is.na(quota)),
      mase = mean(err_scalato, na.rm = TRUE),
      rmsse = sqrt(mean(err2_scalato, na.rm = TRUE)),
      copertura80 = mean(in80, na.rm = TRUE),
      copertura95 = mean(in95, na.rm = TRUE)
    ),
    by = c(key_cols, "modello")
  ]
  for (col in c("mase", "rmsse", "copertura80", "copertura95")) {
    data.table::set(sint, which(is.nan(sint[[col]])), col, NA_real_)
  }
  list(dettaglio = det[], sintesi = sint[], key_cols = key_cols)
}

# 3. select_share_model -----

#' Select the forecasting model by stratum
#'
#' Chooses, within each stratum of series, the model with the lowest
#' median MASE in the backtest, falling back to a baseline when the
#' chosen model does not beat it on enough series.
#'
#' @details
#' For each stratum, the median over series of the MASE of each model is
#' computed and the model with the lowest median is the candidate. The
#' candidate is kept when its MASE is lower than the baseline's on at
#' least `min_win` of the series of the stratum where both are
#' available; otherwise the baseline is used (`ripiego = TRUE`).
#'
#' @param backtest Output of [backtest_share_models()].
#' @param strata Optional data.frame with the key columns and a column
#'   `strato` (e.g. support tercile x family). Series without a stratum
#'   go to `"(senza strato)"`. Default `NULL`: one stratum `"tutte"`.
#' @param baseline Baseline model. Default `"SNAIVE"`.
#' @param min_win Minimum share of series on which the candidate must
#'   beat the baseline. Default `0.5`.
#' @return A list with:
#'   - `strati`: one row per stratum with `strato`, `modello_migliore`,
#'     `modello` (final choice), `mase_mediana`, `mase_baseline`,
#'     `quota_vittorie`, `ripiego`, `n_serie`;
#'   - `serie`: the key columns, `strato` and `modello` of every series
#'     in the backtest.
#' @examples
#' bt <- list(
#'   sintesi = data.table::data.table(
#'     skill_id = rep(c("a", "b", "c"), each = 2),
#'     modello = rep(c("SNAIVE", "ETS"), 3),
#'     mase = c(1.0, 0.8, 1.2, 0.9, 0.7, 0.9)
#'   ),
#'   key_cols = "skill_id"
#' )
#' select_share_model(bt)$strati
#' @export
select_share_model <- function(
  backtest,
  strata = NULL,
  baseline = "SNAIVE",
  min_win = 0.5
) {
  caller <- "select_share_model"
  if (
    !is.list(backtest) ||
      is.null(backtest$sintesi) ||
      is.null(backtest$key_cols)
  ) {
    stop(
      caller,
      ": `backtest` must be the output of ",
      "backtest_share_models()",
      call. = FALSE
    )
  }
  key_cols <- backtest$key_cols
  check_columns(
    backtest$sintesi,
    c(key_cols, "modello", "mase"),
    caller = caller
  )
  s <- data.table::copy(data.table::as.data.table(backtest$sintesi))
  if (!baseline %in% s$modello) {
    stop(
      caller,
      ": baseline model '",
      baseline,
      "' not in the backtest",
      call. = FALSE
    )
  }
  if (
    !is.numeric(min_win) || length(min_win) != 1L || min_win < 0 || min_win > 1
  ) {
    stop(caller, ": `min_win` must be in [0, 1]", call. = FALSE)
  }

  if (is.null(strata)) {
    s[, strato := "tutte"]
  } else {
    check_columns(strata, c(key_cols, "strato"), caller = caller)
    st <- unique(
      data.table::as.data.table(strata)[,
        c(key_cols, "strato"),
        with = FALSE
      ],
      by = key_cols
    )
    s <- merge(s, st, by = key_cols, all.x = TRUE)
    s[is.na(strato), strato := "(senza strato)"]
    s[, strato := as.character(strato)]
  }

  med <- s[,
    list(mase_mediana = stats::median(mase, na.rm = TRUE)),
    by = c("strato", "modello")
  ]
  med <- med[!is.na(mase_mediana)]
  data.table::setorderv(med, c("strato", "mase_mediana"))
  best <- med[, .SD[1L], by = strato]
  data.table::setnames(best, "modello", "modello_migliore")

  base_s <- s[modello == baseline, c(key_cols, "strato", "mase"), with = FALSE]
  data.table::setnames(base_s, "mase", ".mase_b")
  wins <- merge(
    s,
    best[, c("strato", "modello_migliore"), with = FALSE],
    by = "strato"
  )[modello == modello_migliore]
  wins <- merge(wins, base_s, by = c(key_cols, "strato"))
  wins <- wins[
    !is.na(mase) & !is.na(.mase_b),
    list(quota_vittorie = mean(mase < .mase_b)),
    by = strato
  ]
  n_ser <- unique(s[, c(key_cols, "strato"), with = FALSE])[,
    list(n_serie = .N),
    by = strato
  ]
  mb <- med[modello == baseline, list(strato, mase_baseline = mase_mediana)]

  res <- merge(best, wins, by = "strato", all.x = TRUE)
  res <- merge(res, mb, by = "strato", all.x = TRUE)
  res <- merge(res, n_ser, by = "strato", all.x = TRUE)
  res[,
    ripiego := modello_migliore != baseline &
      (is.na(quota_vittorie) | quota_vittorie < min_win)
  ]
  res[, modello := ifelse(ripiego, baseline, modello_migliore)]
  res[ripiego == TRUE, mase_mediana := mase_baseline]
  data.table::setcolorder(
    res,
    c(
      "strato",
      "modello_migliore",
      "modello",
      "mase_mediana",
      "mase_baseline",
      "quota_vittorie",
      "ripiego",
      "n_serie"
    )
  )

  serie <- merge(
    unique(s[, c(key_cols, "strato"), with = FALSE]),
    res[, list(strato, modello)],
    by = "strato"
  )
  data.table::setcolorder(serie, c(key_cols, "strato", "modello"))
  list(strati = res[], serie = serie[])
}

# 4. forecast_share -----

#' Forecast skill shares with prediction intervals
#'
#' Fits the chosen model to each series and returns the forecast median
#' share and the 80% and 95% prediction intervals on the share scale.
#'
#' @details
#' Models are fitted to `logit_quota` as in [backtest_share_models()];
#' the forecast quantiles (0.5, 0.1, 0.9, 0.025, 0.975) are mapped back
#' with the logistic function, so `lo95 <= lo80 <= quota <= hi80 <= hi95`
#' and all values lie in \eqn{[0, 1]}. `quota` is the forecast median.
#' Failed fits give `NA` values.
#'
#' @param tsb A tsibble from [prepare_share_tsibble()].
#' @param model A single model name (`"SNAIVE"`, `"DRIFT"`, `"ETS"`,
#'   `"ARIMA"`) used for all series, or a data.frame with the key columns
#'   and `modello`, e.g. the `serie` element of [select_share_model()].
#'   Series without a model are skipped.
#' @param horizon Forecast horizon in months. Default `12`.
#' @return A data.table with the key columns, `mese` (Date, first day of
#'   the month), `quota`, `lo80`, `hi80`, `lo95`, `hi95` and `modello`.
#' @examples
#' if (requireNamespace("fable", quietly = TRUE)) {
#'   set.seed(8)
#'   panel <- data.table::CJ(skill_id = "a", mese_idx = 1:24)
#'   panel[, n := 2000L]
#'   panel[, x := rbinom(.N, n, plogis(-3 + 0.02 * mese_idx))]
#'   tsb <- prepare_share_tsibble(panel, "skill_id", origin = "2023-01")
#'   forecast_share(tsb, model = "DRIFT", horizon = 3)
#' }
#' @export
forecast_share <- function(tsb, model = "ETS", horizon = 12L) {
  caller <- "forecast_share"
  check_columns(tsb, c("mese", "quota", "logit_quota"), caller = caller)
  check_suggests("tsibble", "to build monthly time series")
  check_suggests("fabletools", "to fit forecasting models")
  check_suggests("fable", "for the SNAIVE, RW, ETS and ARIMA models")
  check_suggests("distributional", "for forecast distributions")
  if (!is.numeric(horizon) || length(horizon) != 1L || horizon < 1) {
    stop(caller, ": `horizon` must be a positive integer", call. = FALSE)
  }
  x <- .fc_to_dt(tsb, caller)
  dt <- x$dt
  key_cols <- x$key_cols

  if (is.character(model)) {
    if (length(model) != 1L) {
      stop(
        caller,
        ": `model` must be a single name or a data.frame",
        call. = FALSE
      )
    }
    .fc_check_models(model, caller)
    assign_dt <- unique(dt[, key_cols, with = FALSE])[, modello := model]
  } else if (is.data.frame(model)) {
    check_columns(model, c(key_cols, "modello"), caller = caller)
    assign_dt <- unique(
      data.table::as.data.table(model)[,
        c(key_cols, "modello"),
        with = FALSE
      ],
      by = key_cols
    )
    assign_dt <- assign_dt[!is.na(modello)]
    .fc_check_models(unique(assign_dt$modello), caller)
  } else {
    stop(
      caller,
      ": `model` must be a single name or a data.frame",
      call. = FALSE
    )
  }

  res <- data.table::rbindlist(lapply(unique(assign_dt$modello), function(m) {
    sub <- merge(
      dt,
      assign_dt[modello == m, key_cols, with = FALSE],
      by = key_cols
    )
    if (nrow(sub) == 0L) {
      return(NULL)
    }
    .fc_fit_forecast(sub, key_cols, m, horizon)
  }))
  data.table::setcolorder(
    res,
    c(key_cols, "mese", "quota", "lo80", "hi80", "lo95", "hi95", "modello")
  )
  data.table::setorderv(res, c(key_cols, "mese"))
  res[]
}
