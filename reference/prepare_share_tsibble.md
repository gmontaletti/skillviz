# Prepare a tsibble of skill shares for forecasting

Converts a share panel into a monthly tsibble with the share and its
logit, the response modelled by
[`backtest_share_models()`](https://gmontaletti.github.io/skillviz/reference/backtest_share_models.md)
and
[`forecast_share()`](https://gmontaletti.github.io/skillviz/reference/forecast_share.md).

## Usage

``` r
prepare_share_tsibble(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  origin = NULL,
  eps = 1e-06
)
```

## Arguments

- panel:

  A share panel (e.g. from
  [`compute_share_panel()`](https://gmontaletti.github.io/skillviz/reference/compute_share_panel.md)).

- key_cols:

  Series key columns.

- time_col:

  Month column: a numeric month index (requires `origin`), a Date, or a
  character `"YYYY-MM"`/`"YYYY-MM-DD"`. Default `"mese_idx"`.

- x_col, n_col:

  Count and denominator columns.

- origin:

  Month of index 1 when `time_col` is numeric, as `"YYYY-MM"` or Date.
  Default `NULL`.

- eps:

  Clamp of the share before the logit. Default `1e-6`.

## Value

A tsibble with key `key_cols`, index `mese` (yearmonth) and columns `x`,
`n`, `quota` (`x / n`) and `logit_quota`.

## Details

`logit_quota` is \\\mathrm{logit}((x + 0.5)/(n + 1))\\, clamped to
\\\[\epsilon, 1 - \epsilon\]\\ before the logit, which is finite for
zero counts. Series with missing months between their first and last
month, or with `n = 0` in some month, are dropped with a warning, since
the models require regular series without gaps.

## Examples

``` r
if (requireNamespace("tsibble", quietly = TRUE)) {
  panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = 1:24)
  panel[, `:=`(n = 1000L, x = 50L)]
  prepare_share_tsibble(panel, "skill_id", origin = "2023-01")
}
#> # A tsibble: 48 x 6 [1M]
#> # Key:       skill_id [2]
#>    skill_id     x     n     mese quota logit_quota
#>    <chr>    <int> <int>    <mth> <dbl>       <dbl>
#>  1 a           50  1000 2023 Jan  0.05       -2.94
#>  2 a           50  1000 2023 Feb  0.05       -2.94
#>  3 a           50  1000 2023 Mar  0.05       -2.94
#>  4 a           50  1000 2023 Apr  0.05       -2.94
#>  5 a           50  1000 2023 May  0.05       -2.94
#>  6 a           50  1000 2023 Jun  0.05       -2.94
#>  7 a           50  1000 2023 Jul  0.05       -2.94
#>  8 a           50  1000 2023 Aug  0.05       -2.94
#>  9 a           50  1000 2023 Sep  0.05       -2.94
#> 10 a           50  1000 2023 Oct  0.05       -2.94
#> # ℹ 38 more rows
```
