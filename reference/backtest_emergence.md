# Pseudo-prospective backtest of the emergence score

Evaluates the ability of the emergence score to anticipate significant
share growth, computing indicators with the data available at each
origin and comparing the predicted `"Emergente"` series with realised
growth over the following `horizon` periods.

## Usage

``` r
backtest_emergence(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  origins,
  horizon = 12L,
  window = 24L,
  min_support = 10,
  components = c("pendenza", "accelerazione", "novita"),
  directions = NULL,
  weights_grid = c("equal", "pc1", "slope"),
  q_grid = c(0.8, 0.9, 0.95),
  alpha = 0.05,
  k = 50L,
  net_drift = FALSE,
  break_times = NULL,
  indicators_fun = NULL
)
```

## Arguments

- panel:

  A share panel with one row per key and period, e.g. from
  [`compute_share_panel()`](https://gmontaletti.github.io/skillviz/reference/compute_share_panel.md).
  Periods with `n = 0` are ignored.

- key_cols:

  Character vector of columns identifying a series (e.g. `"skill_id"` or
  `c("cp4", "skill_id")`).

- time_col:

  Numeric period index column. Default `"mese_idx"`.

- x_col:

  Count column. Default `"x"`.

- n_col:

  Denominator column. Default `"n"`.

- origins:

  Numeric vector of origin periods.

- horizon:

  Outcome horizon in periods. Default `12`.

- window:

  Indicator window in periods. Default `24`.

- min_support:

  Minimum support (average count per 3 periods). Default `10`.

- components, directions:

  Passed to
  [`score_emergence()`](https://gmontaletti.github.io/skillviz/reference/score_emergence.md).
  Defaults match the default indicators.

- weights_grid:

  Character vector of weight schemes. Default
  `c("equal", "pc1", "slope")`.

- q_grid:

  Numeric vector of `q_star` values. Default `c(0.8, 0.9, 0.95)`.

- alpha:

  Significance level for trend and outcome. Default `0.05`.

- k:

  Size of the top list for precision@k. Default `50`.

- net_drift:

  Logical; recompute and net out the drift index at each origin. Default
  `FALSE`.

- break_times:

  Optional numeric vector of break periods; each adds a step dummy.
  Default `NULL`.

- indicators_fun:

  Optional function `f(panel, origin)` receiving the standardised panel
  truncated at the origin (columns `key_cols`, `time_col`, `x_col`,
  `n_col`) and returning a data.table with `key_cols`, the `components`,
  `pendenza` and `p_adj`. Default `NULL` (built-in indicators).

## Value

A list with:

- `griglia`: one row per `pesi` x `q_star` with `tp`, `fp`, `fn`,
  `precision`, `recall`, `f1`, `precision_at_k`;

- `dettaglio`: the same metrics by origin;

- `migliore`: the row of `griglia` with the highest F1.

## Details

For each origin \\o\\, only periods \\\le o\\ are used. The default
indicators (when `indicators_fun = NULL`) are, over the `window` periods
ending at \\o\\: the drift-net slope `pendenza` and its `p_adj`
([`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md)),
the acceleration `accelerazione` over two halves of the window
([`compute_share_acceleration()`](https://gmontaletti.github.io/skillviz/reference/compute_share_acceleration.md)),
and `novita = -eta_mesi` from
[`detect_skill_onset()`](https://gmontaletti.github.io/skillviz/reference/detect_skill_onset.md)
with `min_count = min_support`. With `net_drift = TRUE` the drift index
is recomputed from the data up to the origin, avoiding look-ahead. Only
series whose support \\3 \sum x / window\\ in the window is at least
`min_support` are evaluated.

The outcome is significant realised growth: the ratio between the pooled
share of the 3 periods ending at \\o + horizon\\ and of the 3 periods
ending at \\o\\
([`compute_yoy_ratio()`](https://gmontaletti.github.io/skillviz/reference/compute_yoy_ratio.md)
with `lag = horizon`) has a Katz lower bound above 1.

For each combination of `weights_grid` and `q_grid`,
[`score_emergence()`](https://gmontaletti.github.io/skillviz/reference/score_emergence.md)
labels the series; `"Emergente"` is the positive prediction. Counts are
pooled over origins for precision, recall and F1; precision@k is the
share of the `k` top-scored series with positive outcome, averaged over
origins.

## Examples

``` r
set.seed(5)
panel <- data.table::CJ(skill_id = sprintf("s%02d", 1:30),
  mese_idx = 1:30)
panel[, beta := ifelse(as.integer(substr(skill_id, 2, 3)) <= 8, 0.06, 0)]
#> Key: <skill_id, mese_idx>
#>      skill_id mese_idx  beta
#>        <char>    <int> <num>
#>   1:      s01        1  0.06
#>   2:      s01        2  0.06
#>   3:      s01        3  0.06
#>   4:      s01        4  0.06
#>   5:      s01        5  0.06
#>  ---                        
#> 896:      s30       26  0.00
#> 897:      s30       27  0.00
#> 898:      s30       28  0.00
#> 899:      s30       29  0.00
#> 900:      s30       30  0.00
panel[, n := 3000L]
#> Key: <skill_id, mese_idx>
#>      skill_id mese_idx  beta     n
#>        <char>    <int> <num> <int>
#>   1:      s01        1  0.06  3000
#>   2:      s01        2  0.06  3000
#>   3:      s01        3  0.06  3000
#>   4:      s01        4  0.06  3000
#>   5:      s01        5  0.06  3000
#>  ---                              
#> 896:      s30       26  0.00  3000
#> 897:      s30       27  0.00  3000
#> 898:      s30       28  0.00  3000
#> 899:      s30       29  0.00  3000
#> 900:      s30       30  0.00  3000
panel[, x := rbinom(.N, n, plogis(-3.5 + beta * mese_idx))]
#> Key: <skill_id, mese_idx>
#>      skill_id mese_idx  beta     n     x
#>        <char>    <int> <num> <int> <int>
#>   1:      s01        1  0.06  3000    88
#>   2:      s01        2  0.06  3000    89
#>   3:      s01        3  0.06  3000   107
#>   4:      s01        4  0.06  3000   143
#>   5:      s01        5  0.06  3000   117
#>  ---                                    
#> 896:      s30       26  0.00  3000   101
#> 897:      s30       27  0.00  3000    87
#> 898:      s30       28  0.00  3000    95
#> 899:      s30       29  0.00  3000    86
#> 900:      s30       30  0.00  3000    80
bt <- backtest_emergence(panel, "skill_id", origins = 18, horizon = 12,
  window = 18, k = 5, q_grid = 0.7)
bt$griglia
#>      pesi q_star n_origini    tp    fp    fn precision    recall        f1
#>    <char>  <num>     <int> <int> <int> <int>     <num>     <num>     <num>
#> 1:  equal    0.7         1     8     0     1         1 0.8888889 0.9411765
#> 2:    pc1    0.7         1     8     0     1         1 0.8888889 0.9411765
#> 3:  slope    0.7         1     8     0     1         1 0.8888889 0.9411765
#>    precision_at_k
#>             <num>
#> 1:              1
#> 2:              1
#> 3:              1
```
