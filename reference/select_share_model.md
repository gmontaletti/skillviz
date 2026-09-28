# Select the forecasting model by stratum

Chooses, within each stratum of series, the model with the lowest median
MASE in the backtest, falling back to a baseline when the chosen model
does not beat it on enough series.

## Usage

``` r
select_share_model(backtest, strata = NULL, baseline = "SNAIVE", min_win = 0.5)
```

## Arguments

- backtest:

  Output of
  [`backtest_share_models()`](https://gmontaletti.github.io/skillviz/reference/backtest_share_models.md).

- strata:

  Optional data.frame with the key columns and a column `strato` (e.g.
  support tercile x family). Series without a stratum go to
  `"(senza strato)"`. Default `NULL`: one stratum `"tutte"`.

- baseline:

  Baseline model. Default `"SNAIVE"`.

- min_win:

  Minimum share of series on which the candidate must beat the baseline.
  Default `0.5`.

## Value

A list with:

- `strati`: one row per stratum with `strato`, `modello_migliore`,
  `modello` (final choice), `mase_mediana`, `mase_baseline`,
  `quota_vittorie`, `ripiego`, `n_serie`;

- `serie`: the key columns, `strato` and `modello` of every series in
  the backtest.

## Details

For each stratum, the median over series of the MASE of each model is
computed and the model with the lowest median is the candidate. The
candidate is kept when its MASE is lower than the baseline's on at least
`min_win` of the series of the stratum where both are available;
otherwise the baseline is used (`ripiego = TRUE`).

## Examples

``` r
bt <- list(
  sintesi = data.table::data.table(
    skill_id = rep(c("a", "b", "c"), each = 2),
    modello = rep(c("SNAIVE", "ETS"), 3),
    mase = c(1.0, 0.8, 1.2, 0.9, 0.7, 0.9)
  ),
  key_cols = "skill_id"
)
select_share_model(bt)$strati
#> Key: <strato>
#> Index: <ripiego>
#>    strato modello_migliore modello mase_mediana mase_baseline quota_vittorie
#>    <char>           <char>  <char>        <num>         <num>          <num>
#> 1:  tutte              ETS     ETS          0.9             1      0.6666667
#>    ripiego n_serie
#>     <lgcl>   <int>
#> 1:   FALSE       3
```
