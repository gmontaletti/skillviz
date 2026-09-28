# Common drift index of logit shares

Estimates the month-to-month movement shared by all skills, which
captures changes in extraction or taxonomy rather than in demand.

## Usage

``` r
compute_drift_index(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  min_count = 0
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

- min_count:

  Series whose total count over the panel is below this value are
  excluded. Default `0`.

## Value

A data.table with `time_col`, `drift` (\\D_t\\, `NA` in the first
period), `drift_livello` and `n_serie` (series contributing).

## Details

For each series with observations in consecutive periods \\t-1, t\\,
\\\Delta y\_{k,t}\\ is the first difference of the empirical logit share
\\\mathrm{logit}((x + 0.5)/(n + 1))\\, with weight \\1/(v\_{k,t} +
v\_{k,t-1})\\ and \\v = 1/(x + 0.5) + 1/(n - x + 0.5)\\. The drift index
\\D_t\\ is the weighted median of \\\Delta y\_{k,t}\\ across series,
which is robust to the minority of skills with genuine change.
`drift_livello` is the cumulative sum of \\D_t\\ starting from 0 at the
first period; periods without differences add 0.

## Examples

``` r
set.seed(3)
panel <- data.table::CJ(skill_id = letters[1:10], mese_idx = 1:12)
panel[, n := 5000L]
#> Key: <skill_id, mese_idx>
#>      skill_id mese_idx     n
#>        <char>    <int> <int>
#>   1:        a        1  5000
#>   2:        a        2  5000
#>   3:        a        3  5000
#>   4:        a        4  5000
#>   5:        a        5  5000
#>  ---                        
#> 116:        j        8  5000
#> 117:        j        9  5000
#> 118:        j       10  5000
#> 119:        j       11  5000
#> 120:        j       12  5000
panel[, x := rbinom(.N, n, plogis(-3 + 0.03 * mese_idx))]
#> Key: <skill_id, mese_idx>
#>      skill_id mese_idx     n     x
#>        <char>    <int> <int> <int>
#>   1:        a        1  5000   228
#>   2:        a        2  5000   259
#>   3:        a        3  5000   267
#>   4:        a        4  5000   262
#>   5:        a        5  5000   281
#>  ---                              
#> 116:        j        8  5000   299
#> 117:        j        9  5000   307
#> 118:        j       10  5000   320
#> 119:        j       11  5000   311
#> 120:        j       12  5000   329
compute_drift_index(panel, key_cols = "skill_id")
#> Key: <mese_idx>
#>     mese_idx       drift drift_livello n_serie
#>        <int>       <num>         <num>   <int>
#>  1:        1          NA    0.00000000       0
#>  2:        2 0.041872157    0.04187216      10
#>  3:        3 0.027599866    0.06947202      10
#>  4:        4 0.019240296    0.08871232      10
#>  5:        5 0.073899134    0.16261145      10
#>  6:        6 0.033379931    0.19599138      10
#>  7:        7 0.025297796    0.22128918      10
#>  8:        8 0.037605980    0.25889516      10
#>  9:        9 0.003418396    0.26231356      10
#> 10:       10 0.066361241    0.32867480      10
#> 11:       11 0.003281391    0.33195619      10
#> 12:       12 0.046815976    0.37877216      10
```
