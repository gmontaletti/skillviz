# Acceleration of the logit share trend

Difference between the drift-net slope of the last `window` periods and
the slope of the `window` periods before them.

## Usage

``` r
compute_share_acceleration(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  window = 12L,
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

  Length of each of the two windows. Default `12`.

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

A data.table with `key_cols`, `pendenza_recente`, `pendenza_precedente`,
`accelerazione`, `se`, `z`, `p_value`, `p_adj`.

## Details

Both slopes are estimated as in
[`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md).
The acceleration is \\a = \beta\_{recent} - \beta\_{previous}\\ with
\\SE(a) = \sqrt{SE\_{recent}^2 + SE\_{previous}^2}\\ (the two windows
are disjoint), \\z = a / SE(a)\\, a two-sided normal p-value and a
BH-adjusted p-value over the series of the call.

## Examples

``` r
set.seed(2)
panel <- data.table::CJ(skill_id = c("a", "b"), mese_idx = 1:24)
panel[, n := 5000L]
#> Key: <skill_id, mese_idx>
#>     skill_id mese_idx     n
#>       <char>    <int> <int>
#>  1:        a        1  5000
#>  2:        a        2  5000
#>  3:        a        3  5000
#>  4:        a        4  5000
#>  5:        a        5  5000
#>  6:        a        6  5000
#>  7:        a        7  5000
#>  8:        a        8  5000
#>  9:        a        9  5000
#> 10:        a       10  5000
#> 11:        a       11  5000
#> 12:        a       12  5000
#> 13:        a       13  5000
#> 14:        a       14  5000
#> 15:        a       15  5000
#> 16:        a       16  5000
#> 17:        a       17  5000
#> 18:        a       18  5000
#> 19:        a       19  5000
#> 20:        a       20  5000
#> 21:        a       21  5000
#> 22:        a       22  5000
#> 23:        a       23  5000
#> 24:        a       24  5000
#> 25:        b        1  5000
#> 26:        b        2  5000
#> 27:        b        3  5000
#> 28:        b        4  5000
#> 29:        b        5  5000
#> 30:        b        6  5000
#> 31:        b        7  5000
#> 32:        b        8  5000
#> 33:        b        9  5000
#> 34:        b       10  5000
#> 35:        b       11  5000
#> 36:        b       12  5000
#> 37:        b       13  5000
#> 38:        b       14  5000
#> 39:        b       15  5000
#> 40:        b       16  5000
#> 41:        b       17  5000
#> 42:        b       18  5000
#> 43:        b       19  5000
#> 44:        b       20  5000
#> 45:        b       21  5000
#> 46:        b       22  5000
#> 47:        b       23  5000
#> 48:        b       24  5000
#>     skill_id mese_idx     n
#>       <char>    <int> <int>
panel[, x := rbinom(.N, n, plogis(-3 + 0.08 * pmax(0, mese_idx - 12)))]
#> Key: <skill_id, mese_idx>
#>     skill_id mese_idx     n     x
#>       <char>    <int> <int> <int>
#>  1:        a        1  5000   225
#>  2:        a        2  5000   258
#>  3:        a        3  5000   208
#>  4:        a        4  5000   219
#>  5:        a        5  5000   242
#>  6:        a        6  5000   255
#>  7:        a        7  5000   232
#>  8:        a        8  5000   231
#>  9:        a        9  5000   278
#> 10:        a       10  5000   255
#> 11:        a       11  5000   212
#> 12:        a       12  5000   248
#> 13:        a       13  5000   256
#> 14:        a       14  5000   272
#> 15:        a       15  5000   292
#> 16:        a       16  5000   325
#> 17:        a       17  5000   356
#> 18:        a       18  5000   402
#> 19:        a       19  5000   450
#> 20:        a       20  5000   431
#> 21:        a       21  5000   421
#> 22:        a       22  5000   552
#> 23:        a       23  5000   531
#> 24:        a       24  5000   575
#> 25:        b        1  5000   226
#> 26:        b        2  5000   244
#> 27:        b        3  5000   257
#> 28:        b        4  5000   252
#> 29:        b        5  5000   241
#> 30:        b        6  5000   245
#> 31:        b        7  5000   240
#> 32:        b        8  5000   234
#> 33:        b        9  5000   224
#> 34:        b       10  5000   225
#> 35:        b       11  5000   238
#> 36:        b       12  5000   233
#> 37:        b       13  5000   244
#> 38:        b       14  5000   259
#> 39:        b       15  5000   340
#> 40:        b       16  5000   303
#> 41:        b       17  5000   389
#> 42:        b       18  5000   372
#> 43:        b       19  5000   418
#> 44:        b       20  5000   452
#> 45:        b       21  5000   453
#> 46:        b       22  5000   492
#> 47:        b       23  5000   581
#> 48:        b       24  5000   566
#>     skill_id mese_idx     n     x
#>       <char>    <int> <int> <int>
compute_share_acceleration(panel, key_cols = "skill_id")
#> Key: <skill_id>
#>    skill_id pendenza_recente pendenza_precedente accelerazione          se
#>      <char>            <num>               <num>         <num>       <num>
#> 1:        a       0.08015222         0.006368103    0.07378412 0.009925526
#> 2:        b       0.08034449        -0.005781780    0.08612627 0.008319623
#>            z      p_value        p_adj
#>        <num>        <num>        <num>
#> 1:  7.433774 1.055419e-13 1.055419e-13
#> 2: 10.352184 4.090450e-25 8.180899e-25
```
