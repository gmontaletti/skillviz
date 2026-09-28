# Year-on-year ratio of pooled shares with Katz confidence interval

Compares the pooled share of the last `months` periods ending at `end`
with the pooled share of the same periods `lag` periods earlier.

## Usage

``` r
compute_yoy_ratio(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  months = 3L,
  lag = 12L,
  end = NULL,
  conf = 0.95
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

- months:

  Number of periods pooled in each block. Default `3`.

- lag:

  Distance in periods between the two blocks. Default `12`.

- end:

  Last period of the window. Default: the maximum period in `panel`.

- conf:

  Confidence level of the interval. Default `0.95`.

## Value

A data.table with `key_cols`, `x_attuale`, `n_attuale`, `x_precedente`,
`n_precedente`, `yoy` (ratio), `yoy_lo`, `yoy_hi`, `log_yoy`, `se_log`
and `p_value` (two-sided test of `yoy = 1`). Series with a zero
denominator in either block get `NA`.

## Details

With \\x_1, n_1\\ the counts and denominators summed over the current
block and \\x_0, n_0\\ over the reference block, the ratio is \\R =
(x_1/n_1)/(x_0/n_0)\\. The Katz interval uses \\SE(\log R) =
\sqrt{1/x_1 - 1/n_1 + 1/x_0 - 1/n_0}\\. When either count is zero, 0.5
is added to both counts and both denominators of that series.

## Examples

``` r
panel <- data.table::CJ(skill_id = "a", mese_idx = 1:15)
panel[, `:=`(n = 1000L, x = ifelse(mese_idx > 12, 60L, 30L))]
#> Key: <skill_id, mese_idx>
#>     skill_id mese_idx     n     x
#>       <char>    <int> <int> <int>
#>  1:        a        1  1000    30
#>  2:        a        2  1000    30
#>  3:        a        3  1000    30
#>  4:        a        4  1000    30
#>  5:        a        5  1000    30
#>  6:        a        6  1000    30
#>  7:        a        7  1000    30
#>  8:        a        8  1000    30
#>  9:        a        9  1000    30
#> 10:        a       10  1000    30
#> 11:        a       11  1000    30
#> 12:        a       12  1000    30
#> 13:        a       13  1000    60
#> 14:        a       14  1000    60
#> 15:        a       15  1000    60
compute_yoy_ratio(panel, key_cols = "skill_id")
#> Key: <skill_id>
#>    skill_id x_attuale n_attuale x_precedente n_precedente   yoy   yoy_lo
#>      <char>     <int>     <int>        <int>        <int> <num>    <num>
#> 1:        a       180      3000           90         3000     2 1.560848
#>     yoy_hi   log_yoy    se_log      p_value
#>      <num>     <num>     <num>        <num>
#> 1: 2.56271 0.6931472 0.1264911 4.257838e-08
```
