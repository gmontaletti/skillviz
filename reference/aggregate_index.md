# Weighted aggregation of source indices

Combines per-source indices into one index per key and time with fixed
source weights, renormalised over the sources present in each cell, with
an optional cap on the weight of any source.

## Usage

``` r
aggregate_index(
  idx,
  source_col,
  key_cols = NULL,
  time_col,
  index_col = "indice",
  weights,
  type = c("geometric", "arithmetic"),
  cap = NULL
)
```

## Arguments

- idx:

  A data.frame or data.table of per-source indices.

- source_col:

  Name of the source column.

- key_cols:

  Character vector of further key columns (e.g. skill), or `NULL`.

- time_col:

  Name of the time column.

- index_col:

  Name of the index column (default `"indice"`).

- weights:

  A data.frame with columns `source_col` and `w` (non-negative weights,
  e.g. base-period volumes).

- type:

  `"geometric"` (default) or `"arithmetic"`.

- cap:

  `NULL` or a single number in (0, 1\]: maximum weight of a source.

## Value

A data.table with columns `key_cols`, `time_col`, `index_col`,
`n_sources`, `covered_weight` and `n_eff`, one row per key x time of
`idx`.

## Details

In each key x time cell, the sources with a non-missing index and a
positive weight are used, their weights `w` renormalised to sum to one.
With `cap`, weights above `cap` are set to `cap` and the excess is
redistributed proportionally over the other sources, repeating until no
weight exceeds the cap (water-filling); when fewer than `1 / cap`
sources are present the cap is infeasible and equal weights are used.
`type = "geometric"` returns `exp(sum(w * log(I)))`, `"arithmetic"`
returns `sum(w * I)`. `covered_weight` is the share of the total weight
of `weights` held by the sources present, before renormalisation, and
`n_eff = 1 / sum(w^2)` is the effective number of sources (Kish) after
renormalisation and cap. Sources of `idx` missing from `weights` are
ignored with a warning.

## Examples

``` r
weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:3)
idx <- data.table::data.table(
  source = rep(c("a", "b"), each = 4), week = rep(weeks, 2),
  indice = c(100, 101, 102, 103, 100, 99, 98, 97)
)
w <- data.table::data.table(source = c("a", "b"), w = c(3, 1))
aggregate_index(idx, "source", time_col = "week", weights = w)
#>          week   indice n_sources covered_weight n_eff
#>        <IDat>    <num>     <int>          <num> <num>
#> 1: 2024-01-01 100.0000         2              1   1.6
#> 2: 2024-01-08 100.4962         2              1   1.6
#> 3: 2024-01-15 100.9849         2              1   1.6
#> 4: 2024-01-22 101.4661         2              1   1.6
```
