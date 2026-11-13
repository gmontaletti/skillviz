# Diagnostics of extracted trends

Summarises, per key, how much of the movement of the seasonally adjusted
series the trend absorbs, how smooth it is and whether the remainder
still has autocorrelation or exceeds the sampling-noise floor.

## Usage

``` r
trend_diagnostics(tr, key_cols, n_col = NULL, time_col = NULL)
```

## Arguments

- tr:

  Output of
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)
  (possibly with an added column of counts).

- key_cols:

  Character vector of key columns.

- n_col:

  Name of a column of counts in `tr` (number of postings for totals,
  denominator for shares), or `NULL`.

- time_col:

  Name of the time column; by default taken from the `time_col`
  attribute of `tr`.

## Value

A data.table with one row per key (and `method`, when present) and
columns `n_obs`, `ratio_trend_remainder`, `var_removed_d1`,
`var_removed_d13`, `lb_p_value`, `roughness`, `remainder_var` and, when
`n_col` is given, `noise_floor` and `ratio_floor`.

## Details

On each key, ordered by time:

- `ratio_trend_remainder`: `var(diff(trend)) / var(diff(remainder))`.

- `var_removed_d1`, `var_removed_d13`: share of the variance of the
  1-week and 13-week differences of the seasonally adjusted series
  (`observed - seasonal`) removed by the trend,
  `1 - var(diff_h(remainder)) / var(diff_h(observed - seasonal))`. The
  seasonally adjusted series is used because the seasonal component, not
  the trend, accounts for the 13-week movement of the raw series.

- `lb_p_value`: Ljung-Box p-value of the remainder at lag 13.

- `roughness`: variance of the second differences of the trend.

- `remainder_var`: variance of the remainder.

- When `n_col` is given: `noise_floor` and
  `ratio_floor = remainder_var / noise_floor`. For `transform = "log"`
  the floor is the Poisson variance of a log count, `1 / mean(n)`; for
  `"logit"` it is the binomial variance of a logit share,
  `1 / mean(n p (1 - p))` with `p` the observed share and `n` the
  denominator; for `"none"` it is `NA`. A ratio near 1 means the
  remainder is close to pure sampling noise.

## Examples

``` r
set.seed(1)
weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:155)
d <- data.table::data.table(
  source = "a",
  week = weeks,
  n = stats::rpois(156, exp(5 + 0.003 * seq_along(weeks)))
)
tr <- extract_trend(d, "source", "week", "n", harmonics = 1L)
tr[d, n := i.n, on = c("source", "week")]
#>      source       week observed      seasonal    trend trend_filtered
#>      <char>     <IDat>    <num>         <num>    <num>          <num>
#>   1:      a 2023-05-01 4.948760 -0.0009100685 4.959591       4.949670
#>   2:      a 2023-05-08 5.105945 -0.0006639365 4.964541       5.056570
#>   3:      a 2023-05-15 5.105945 -0.0004081890 4.969474       5.099954
#>   4:      a 2023-05-22 5.043425 -0.0001465298 4.974388       5.094198
#>   5:      a 2023-05-29 4.875197  0.0001172516 4.979278       5.024122
#>  ---                                                                 
#> 152:      a 2026-03-23 5.446737 -0.0019482201 5.457618       5.449930
#> 153:      a 2026-03-30 5.549076 -0.0018134445 5.460930       5.460289
#> 154:      a 2026-04-06 5.407172 -0.0016524053 5.464243       5.459452
#> 155:      a 2026-04-13 5.533389 -0.0014674348 5.467556       5.468007
#> 156:      a 2026-04-20 5.463832 -0.0012612118 5.470869       5.470869
#>         trend_se trend_lo trend_hi    remainder imputed method     n
#>            <num>    <num>    <num>        <num>  <lgcl> <char> <int>
#>   1: 0.001412207 4.956823 4.962359 -0.009921033   FALSE    llt   141
#>   2: 0.002070876 4.960482 4.968600  0.142068602   FALSE    llt   165
#>   3: 0.002690666 4.964201 4.974748  0.136879184   FALSE    llt   165
#>   4: 0.003274045 4.967971 4.980805  0.069183766   FALSE    llt   155
#>   5: 0.003822534 4.971786 4.986770 -0.104198100   FALSE    llt   131
#>  ---                                                                
#> 152: 0.016103429 5.426056 5.489180 -0.008932646   FALSE    llt   232
#> 153: 0.016727681 5.428144 5.493716  0.089959392   FALSE    llt   257
#> 154: 0.017384372 5.430170 5.498315 -0.055418437   FALSE    llt   223
#> 155: 0.018072777 5.432134 5.502978  0.067301312   FALSE    llt   253
#> 156: 0.018792026 5.434037 5.507700 -0.005775538   FALSE    llt   236
trend_diagnostics(tr, "source", n_col = "n")
#>    source method n_obs ratio_trend_remainder var_removed_d1 var_removed_d13
#>    <char> <char> <int>                 <num>          <num>           <num>
#> 1:      a    llt   156          5.588495e-05   0.0001395971     0.002246588
#>    lb_p_value    roughness remainder_var noise_floor ratio_floor
#>         <num>        <num>         <num>       <num>       <num>
#> 1:  0.3227209 5.524652e-10   0.004612223 0.005248284   0.8788058
```
