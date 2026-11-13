# Revisions of the trend as new data arrive

Re-runs
[`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)
on the data truncated at each origin and compares the trend estimated
with data up to the origin with the full-sample trend, at lags
`0..max_lag` weeks before the origin.

## Usage

``` r
trend_revisions(
  dt,
  key_cols,
  time_col,
  value_col,
  origins,
  max_lag = 26L,
  scale = NULL,
  ...
)
```

## Arguments

- dt, key_cols, time_col, value_col:

  As in
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md).

- origins:

  Vector of origins (Date, IDate or 'YYYY-MM-DD'), each equal to a time
  present in `dt`.

- max_lag:

  Largest lag in weeks (non-negative integer).

- scale:

  `NULL`, or a positive number (e.g. 100) to add revisions in index
  points.

- ...:

  Further arguments passed to
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)
  (e.g. `transform`, `method`, `harmonics`).

## Value

A data.table with columns `key_cols`, `origin`, `lag`, `time_col`,
`trend_full`, `trend_vintage`, `revision` and, when `scale` is given,
`revision_index`. Attribute `summary`: one row per lag with `n`,
`mean_revision`, `mean_abs_revision`, `rmse_revision` and, when `scale`
is given, `mean_abs_revision_index`.

## Details

For origin `o` and lag `h`, the revision is
`trend_full(o - 7h) - trend_vintage(o - 7h)` on the transformed scale.
The size of the revision as a function of `h` tells how many final weeks
of the trend are provisional. With `harmonics = NULL` the number of
harmonics is re-selected at each origin, so revisions also include
changes of `K`. When `scale` is given, `revision_index` expresses the
revision in index points: `scale * (exp(rev) - 1)` for `log`,
`scale * (p_full / p_vintage - 1)` for `logit` (shares) and
`scale * (full / vintage - 1)` for `none`.

## Examples

``` r
set.seed(1)
weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:119)
d <- data.table::data.table(
  source = "a",
  week = weeks,
  n = exp(5 + 0.003 * seq_along(weeks) + rnorm(120, 0, 0.03))
)
rv <- trend_revisions(d, "source", "week", "n",
  origins = weeks[c(100, 110)], max_lag = 4L, harmonics = 0L
)
attr(rv, "summary")
#>      lag     n mean_revision mean_abs_revision rmse_revision
#>    <int> <int>         <num>             <num>         <num>
#> 1:     0     2  -0.002430188       0.003748972   0.004467729
#> 2:     1     2  -0.002299679       0.003466352   0.004159822
#> 3:     2     2  -0.002091301       0.003287943   0.003896679
#> 4:     3     2  -0.001846487       0.003166117   0.003665216
#> 5:     4     2  -0.001515438       0.003150420   0.003495955
```
