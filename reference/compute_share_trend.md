# Drift-net logit trend of skill shares

Estimates, for every series, the slope of the empirical logit share on
the time index by weighted least squares, optionally with step dummies
at break periods and net of a common drift slope. The computation is
closed-form and vectorised over series.

## Usage

``` r
compute_share_trend(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  window = NULL,
  end = NULL,
  break_times = NULL,
  drift = NULL
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

- window:

  Number of trailing periods ending at `end` used for the fit (e.g. `12`
  or `24`). `NULL` (default) uses all periods up to `end`.

- end:

  Last period of the window. Default: the maximum period in `panel`.

- break_times:

  Optional numeric vector of break periods; each adds a step dummy.
  Default `NULL`.

- drift:

  Optional data.table with `time_col` and `drift_livello`, as returned
  by
  [`compute_drift_index()`](https://gmontaletti.github.io/skillviz/reference/compute_drift_index.md).
  Default `NULL` (no drift correction).

## Value

A data.table with one row per series: `key_cols`, `n_mesi` (periods
used), `pendenza` (drift-net slope per period on the logit scale), `se`,
`z`, `p_value`, `p_adj` (BH), `phi` (overdispersion factor),
`pendenza_lorda` (slope before drift correction) and `pendenza_drift`
(drift slope subtracted).

## Details

For each series the response is \\y_t = \mathrm{logit}((x_t +
0.5)/(n_t + 1))\\ with weights \\w_t = 1 / (1/(x_t + 0.5) + 1/(n_t -
x_t + 0.5))\\, the inverse of the approximate sampling variance of the
empirical logit. Break periods `break_times` add a step dummy \\1\\t \ge
b\\\\; together with the intercept they define segment-specific
intercepts, so the slope is obtained after weighted centring of \\t\\
and \\y\\ within each segment (Frisch-Waugh-Lovell).

Overdispersion is handled by the quasi-likelihood factor \\\phi =
\max(1, X^2 / df)\\, where \\X^2\\ is the weighted residual sum of
squares (Pearson statistic) and \\df = T - 1 - S\\ with \\S\\ segments;
the standard error is \\\sqrt{\phi / \sum_t w_t \tilde t_t^2}\\.

When `drift` is supplied the slope \\\beta_D\\ of the cumulative drift
level `drift_livello` (see
[`compute_drift_index()`](https://gmontaletti.github.io/skillviz/reference/compute_drift_index.md))
is estimated by OLS on the same window and segments and subtracted:
`pendenza` \\= \beta_k - \beta_D\\. The uncertainty of \\\beta_D\\ is
ignored, since it is common to all series and estimated from all of
them. Two-sided p-values use the normal approximation; `p_adj` applies
the Benjamini-Hochberg correction to all series of the call, which
therefore defines the testing family.

## Examples

``` r
set.seed(1)
panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = 1:24)
panel[, n := 2000L]
#> Key: <skill_id, mese_idx>
#>     skill_id mese_idx     n
#>       <char>    <int> <int>
#>  1:        a        1  2000
#>  2:        a        2  2000
#>  3:        a        3  2000
#>  4:        a        4  2000
#>  5:        a        5  2000
#>  6:        a        6  2000
#>  7:        a        7  2000
#>  8:        a        8  2000
#>  9:        a        9  2000
#> 10:        a       10  2000
#> 11:        a       11  2000
#> 12:        a       12  2000
#> 13:        a       13  2000
#> 14:        a       14  2000
#> 15:        a       15  2000
#> 16:        a       16  2000
#> 17:        a       17  2000
#> 18:        a       18  2000
#> 19:        a       19  2000
#> 20:        a       20  2000
#> 21:        a       21  2000
#> 22:        a       22  2000
#> 23:        a       23  2000
#> 24:        a       24  2000
#> 25:        b        1  2000
#> 26:        b        2  2000
#> 27:        b        3  2000
#> 28:        b        4  2000
#> 29:        b        5  2000
#> 30:        b        6  2000
#> 31:        b        7  2000
#> 32:        b        8  2000
#> 33:        b        9  2000
#> 34:        b       10  2000
#> 35:        b       11  2000
#> 36:        b       12  2000
#> 37:        b       13  2000
#> 38:        b       14  2000
#> 39:        b       15  2000
#> 40:        b       16  2000
#> 41:        b       17  2000
#> 42:        b       18  2000
#> 43:        b       19  2000
#> 44:        b       20  2000
#> 45:        b       21  2000
#> 46:        b       22  2000
#> 47:        b       23  2000
#> 48:        b       24  2000
#>     skill_id mese_idx     n
#>       <char>    <int> <int>
panel[, x := rbinom(.N, n, plogis(-3 + ifelse(skill_id == "a", 0.05, 0) *
  mese_idx))]
#> Key: <skill_id, mese_idx>
#>     skill_id mese_idx     n     x
#>       <char>    <int> <int> <int>
#>  1:        a        1  2000   101
#>  2:        a        2  2000    95
#>  3:        a        3  2000   129
#>  4:        a        4  2000   103
#>  5:        a        5  2000   124
#>  6:        a        6  2000   120
#>  7:        a        7  2000   129
#>  8:        a        8  2000   126
#>  9:        a        9  2000   145
#> 10:        a       10  2000   143
#> 11:        a       11  2000   174
#> 12:        a       12  2000   170
#> 13:        a       13  2000   188
#> 14:        a       14  2000   186
#> 15:        a       15  2000   182
#> 16:        a       16  2000   207
#> 17:        a       17  2000   209
#> 18:        a       18  2000   206
#> 19:        a       19  2000   218
#> 20:        a       20  2000   235
#> 21:        a       21  2000   261
#> 22:        a       22  2000   260
#> 23:        a       23  2000   270
#> 24:        a       24  2000   253
#> 25:        b        1  2000   103
#> 26:        b        2  2000    88
#> 27:        b        3  2000    96
#> 28:        b        4  2000    82
#> 29:        b        5  2000    91
#> 30:        b        6  2000    99
#> 31:        b        7  2000    94
#> 32:        b        8  2000   109
#> 33:        b        9  2000    96
#> 34:        b       10  2000   101
#> 35:        b       11  2000    98
#> 36:        b       12  2000    99
#> 37:        b       13  2000   100
#> 38:        b       14  2000   101
#> 39:        b       15  2000    89
#> 40:        b       16  2000    86
#> 41:        b       17  2000    87
#> 42:        b       18  2000    90
#> 43:        b       19  2000   112
#> 44:        b       20  2000    98
#> 45:        b       21  2000    98
#> 46:        b       22  2000    97
#> 47:        b       23  2000   106
#> 48:        b       24  2000    98
#>     skill_id mese_idx     n     x
#>       <char>    <int> <int> <int>
compute_share_trend(panel, key_cols = "skill_id", window = 24)
#> Key: <skill_id>
#>    skill_id n_mesi    pendenza          se          z      p_value        p_adj
#>      <char>  <int>       <num>       <num>      <num>        <num>        <num>
#> 1:        a     24 0.048173453 0.002407166 20.0125163 4.284646e-89 8.569291e-89
#> 2:        b     24 0.002158329 0.003064733  0.7042471 4.812789e-01 4.812789e-01
#>      phi pendenza_lorda pendenza_drift
#>    <num>          <num>          <num>
#> 1:     1    0.048173453              0
#> 2:     1    0.002158329              0
```
