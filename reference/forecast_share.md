# Forecast skill shares with prediction intervals

Fits the chosen model to each series and returns the forecast median
share and the 80% and 95% prediction intervals on the share scale.

## Usage

``` r
forecast_share(tsb, model = "ETS", horizon = 12L)
```

## Arguments

- tsb:

  A tsibble from
  [`prepare_share_tsibble()`](https://gmontaletti.github.io/skillviz/reference/prepare_share_tsibble.md).

- model:

  A single model name (`"SNAIVE"`, `"DRIFT"`, `"ETS"`, `"ARIMA"`) used
  for all series, or a data.frame with the key columns and `modello`,
  e.g. the `serie` element of
  [`select_share_model()`](https://gmontaletti.github.io/skillviz/reference/select_share_model.md).
  Series without a model are skipped.

- horizon:

  Forecast horizon in months. Default `12`.

## Value

A data.table with the key columns, `mese` (Date, first day of the
month), `quota`, `lo80`, `hi80`, `lo95`, `hi95` and `modello`.

## Details

Models are fitted to `logit_quota` as in
[`backtest_share_models()`](https://gmontaletti.github.io/skillviz/reference/backtest_share_models.md);
the forecast quantiles (0.5, 0.1, 0.9, 0.025, 0.975) are mapped back
with the logistic function, so `lo95 <= lo80 <= quota <= hi80 <= hi95`
and all values lie in \\\[0, 1\]\\. `quota` is the forecast median.
Failed fits give `NA` values.

## Examples

``` r
if (requireNamespace("fable", quietly = TRUE)) {
  set.seed(8)
  panel <- data.table::CJ(skill_id = "a", mese_idx = 1:24)
  panel[, n := 2000L]
  panel[, x := rbinom(.N, n, plogis(-3 + 0.02 * mese_idx))]
  tsb <- prepare_share_tsibble(panel, "skill_id", origin = "2023-01")
  forecast_share(tsb, model = "DRIFT", horizon = 3)
}
#>    skill_id       mese      quota       lo80       hi80       lo95       hi95
#>      <char>     <Date>      <num>      <num>      <num>      <num>      <num>
#> 1:        a 2025-01-01 0.07366612 0.06179791 0.08760079 0.05626190 0.09590707
#> 2:        a 2025-02-01 0.07463007 0.05788942 0.09571997 0.05052009 0.10892618
#> 3:        a 2025-03-01 0.07560560 0.05503082 0.10303408 0.04639495 0.12087629
#>    modello
#>     <char>
#> 1:   DRIFT
#> 2:   DRIFT
#> 3:   DRIFT
```
