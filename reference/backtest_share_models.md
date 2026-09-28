# Rolling-origin backtest of share forecasting models

Fits each model with the data available at every origin and compares the
forecasts of the next `horizon` months with the observed shares.

## Usage

``` r
backtest_share_models(
  tsb,
  models = c("SNAIVE", "DRIFT", "ETS", "ARIMA"),
  origins,
  horizon = 12L,
  season = 12L,
  min_train = 13L
)
```

## Arguments

- tsb:

  A tsibble from
  [`prepare_share_tsibble()`](https://gmontaletti.github.io/skillviz/reference/prepare_share_tsibble.md).

- models:

  Character vector among `"SNAIVE"`, `"DRIFT"`, `"ETS"`, `"ARIMA"`.
  Default all four.

- origins:

  Origin months (`"YYYY-MM"` or Date); training data are the months up
  to and including each origin.

- horizon:

  Forecast horizon in months. Default `12`.

- season:

  Seasonal period for the error scale. Default `12`.

- min_train:

  Minimum number of training months for a series to enter an origin.
  Default `13`.

## Value

A list with:

- `dettaglio`: one row per key, model, origin and horizon with
  `origine`, `h`, `mese`, `quota_oss`, `quota`, `lo80`, `hi80`, `lo95`,
  `hi95`, `err_scalato`, `err2_scalato`, `in80`, `in95`;

- `sintesi`: one row per key and model with `n_prev`, `mase`, `rmsse`,
  `copertura80`, `copertura95`;

- `key_cols`: the key columns.

## Details

Models, all fitted to `logit_quota` with fable: `"SNAIVE"` (seasonal
naive, lag 12), `"DRIFT"` (random walk with drift), `"ETS"` (automatic
exponential smoothing, damped trends allowed) and `"ARIMA"` (automatic
ARIMA). Forecast quantiles are mapped back to the share scale with the
logistic function, so intervals stay in \\\[0, 1\]\\.

Accuracy is measured on the share scale. The scaled errors use the
in-sample mean absolute (squared) seasonal difference of the training
series at lag `season` (lag 1 when the training series is not longer
than `season`): the MASE is the mean of the scaled absolute errors, the
RMSSE the square root of the mean scaled squared errors. Coverage is the
share of observations inside the 80% and 95% intervals. Fits that fail
give `NA` forecasts; their warnings are suppressed.

## Examples

``` r
if (requireNamespace("fable", quietly = TRUE)) {
  set.seed(7)
  panel <- data.table::CJ(skill_id = "a", mese_idx = 1:30)
  panel[, n := 2000L]
  panel[, x := rbinom(.N, n, plogis(-3 + 0.02 * mese_idx))]
  tsb <- prepare_share_tsibble(panel, "skill_id", origin = "2023-01")
  bt <- backtest_share_models(tsb, models = c("SNAIVE", "DRIFT"),
    origins = "2024-12", horizon = 6)
  bt$sintesi
}
#>    skill_id modello n_prev      mase     rmsse copertura80 copertura95
#>      <char>  <char>  <int>     <num>     <num>       <num>       <num>
#> 1:        a   DRIFT      6 0.2407108 0.2829826         1.0           1
#> 2:        a  SNAIVE      6 1.6196441 1.4723139         0.5           1
```
