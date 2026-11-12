# Concordance of trends across sources

Measures whether sources move together: the weighted share of sources
whose trend change over a lag has the sign of the weighted majority, and
the share of variance of the first principal component of source trend
growth rates, each compared with a placebo distribution obtained by
circular shifts of the source series.

## Usage

``` r
trend_concordance(
  tr,
  source_col,
  time_col,
  lags = c(13L, 52L),
  weights = NULL,
  n_perm = 200L,
  seed = 1L,
  value_col = "trend"
)
```

## Arguments

- tr:

  A data.frame or data.table with one row per source x week (e.g. totals
  from
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)).

- source_col:

  Name of the source column.

- time_col:

  Name of the time column (weekly Date or IDate).

- lags:

  Integer vector of lags in weeks.

- weights:

  `NULL` (equal weights) or a data.frame with columns `source_col` and
  `w`.

- n_perm:

  Number of placebo shifts (non-negative integer).

- seed:

  Seed of the placebo shifts; the global RNG state is restored.

- value_col:

  Name of the trend column (default `"trend"`).

## Value

A data.table with one row per lag x statistic (`concordance`,
`pc1_share`) and columns `lag`, `statistic`, `observed`, `placebo_mean`,
`placebo_q95`, `p_value` and `n_perm`. Attribute `by_time`: columns
`time_col`, `lag`, `concordance` and `n_sources`.

## Details

For lag `h` (weeks), the growth of source `s` is
`g_{s,t} = trend_{s,t} - trend_{s,t-h}`. In week `t`, with `W+` and `W-`
the weights of sources with positive and negative growth and `W` the
weight of sources with non-missing growth, the concordance is
`max(W+, W-) / W` (1 when all sources agree, 0.5 when split evenly); the
statistic `concordance` is its mean over weeks with at least two
sources. `pc1_share` is the share of variance of the first principal
component of the standardised growth matrix (complete weeks only,
unweighted). The placebo shifts the growth series of each source
circularly by an independent random offset, which keeps the
autocorrelation of each source and destroys the alignment across
sources; `p_value = (1 + #(placebo >= observed)) / (n_perm + 1)`.

## Examples

``` r
weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:79)
set.seed(2)
tr <- data.table::data.table(
  source = rep(c("a", "b", "c"), each = 80), week = rep(weeks, 3),
  trend = c(cumsum(rnorm(80)), cumsum(rnorm(80)), cumsum(rnorm(80)))
)
trend_concordance(tr, "source", "week", lags = 4L, n_perm = 50L)
#>      lag   statistic  observed placebo_mean placebo_q95   p_value n_perm
#>    <int>      <char>     <num>        <num>       <num>     <num>  <int>
#> 1:     4 concordance 0.7192982    0.7486842   0.7743421 0.9215686     50
#> 2:     4   pc1_share 0.4815653    0.4283812   0.4876650 0.1176471     50
```
