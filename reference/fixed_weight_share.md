# Share series at fixed source weights

Computes for every key and period the average of the source shares with
weights fixed over the whole period, renormalised over the sources
present in each period, next to the observed share.

## Usage

``` r
fixed_weight_share(
  counts,
  key_cols,
  source_col = "source",
  time_col,
  weights = NULL,
  x_col = "x",
  n_col = "n",
  source_totals = NULL
)
```

## Arguments

- counts:

  data.frame or data.table with one row per key, source and period,
  holding the count `x_col` and, when `source_totals` is `NULL`, the
  source denominator `n_col`.

- key_cols:

  Character vector of key columns.

- source_col:

  Name of the source column (default `"source"`).

- time_col:

  Name of the period column.

- weights:

  Optional named numeric vector of non-negative source weights (names =
  sources). By default the weight of a source is its share of the
  postings over all the periods of `source_totals` (or of `counts`).

- x_col, n_col:

  Names of the count and denominator columns (defaults `"x"`, `"n"`).

- source_totals:

  Optional data.frame with one row per source and period and columns
  `source_col`, `time_col`, `n_col` (postings of the source in the
  period, also where the key has no posting). Without it the
  denominators are read from `counts` and must be the same for all keys
  of a source and period.

## Value

A data.table with one row per key and period (all keys of `counts`
crossed with all periods with at least one source) and columns
`key_cols`, `time_col`, `quota_fissa`, `quota_osservata`, `n_fonti`
(sources with a positive denominator in the period).

## Details

With source shares `p_kst = x_kst / n_st` and fixed weights `w_s`,

`quota_fissa_kt = sum_{s in S_t} w_s p_kst / sum_{s in S_t} w_s`

where `S_t` is the set of sources with `n_st > 0`. The observed share is
`sum_s x_kst / sum_s n_st`. With fixed weights the reallocation of
postings among continuing sources does not move the series, so the
difference between the two series measures the composition effect (see
[`decompose_share_change()`](https://gmontaletti.github.io/skillviz/reference/decompose_share_change.md)).
Entry and exit of sources still change `S_t`. When all the sources of a
period have weight 0, `quota_fissa` is `NA`.

## Examples

``` r
counts <- data.frame(
  code = "a",
  source = c("s1", "s2", "s1", "s2"),
  mese = c(1, 1, 2, 2),
  x = c(10, 5, 10, 20)
)
totals <- data.frame(
  source = c("s1", "s2", "s1", "s2"),
  mese = c(1, 1, 2, 2),
  n = c(100, 100, 100, 400)
)
fixed_weight_share(counts, "code", time_col = "mese",
                   source_totals = totals)
#>      code  mese quota_fissa quota_osservata n_fonti
#>    <char> <num>       <num>           <num>   <int>
#> 1:      a     1  0.06428571           0.075       2
#> 2:      a     2  0.06428571           0.060       2
```
