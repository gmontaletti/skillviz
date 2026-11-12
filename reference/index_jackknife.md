# Leave-one-source-out range of an aggregated index

Recomputes the aggregated index of
[`aggregate_index()`](https://gmontaletti.github.io/skillviz/reference/aggregate_index.md)
leaving out one source at a time and reports the range of the results
and the source whose removal moves the index most.

## Usage

``` r
index_jackknife(idx, source_col, key_cols, time_col, weights, ...)
```

## Arguments

- idx, source_col, key_cols, time_col, weights:

  As in
  [`aggregate_index()`](https://gmontaletti.github.io/skillviz/reference/aggregate_index.md).

- ...:

  Further arguments passed to
  [`aggregate_index()`](https://gmontaletti.github.io/skillviz/reference/aggregate_index.md)
  (`index_col`, `type`, `cap`), used both for the full index and for
  every leave-one-out index: with `type = "arithmetic"` the range refers
  to arithmetic aggregates.

## Value

A data.table with columns `key_cols`, `time_col`, the full index
(`index_col`), `n_sources`, `jk_min`, `jk_max` (range of the
leave-one-out indices), `jk_source` (source whose removal moves the
index most) and `jk_shift` (leave-one-out index minus full index for
that source).

## Details

Without cap, the full weighted mean (of logs for `type = "geometric"`,
of indices for `"arithmetic"`) is a convex combination of the
leave-one-out means, so the full index always lies in
`[jk_min, jk_max]`. In cells where a single source is present, removing
it leaves no index; such leave-one-out values are ignored.

## Examples

``` r
weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:3)
idx <- data.table::data.table(
  source = rep(c("a", "b", "c"), each = 4), week = rep(weeks, 3),
  indice = c(100, 101, 102, 103, 100, 99, 98, 97, 100, 100, 101, 101)
)
w <- data.table::data.table(source = c("a", "b", "c"), w = c(3, 1, 1))
index_jackknife(idx, "source", NULL, "week", w)
#>          week   indice n_sources    jk_min   jk_max jk_source   jk_shift
#>        <IDat>    <num>     <int>     <num>    <num>    <char>      <num>
#> 1: 2024-01-01 100.0000         3 100.00000 100.0000         a  0.0000000
#> 2: 2024-01-08 100.3968         3  99.49874 100.7491         a -0.8980546
#> 3: 2024-01-15 100.9880         3  99.48869 101.7491         a -1.4992657
#> 4: 2024-01-22 101.3727         3  98.97980 102.4963         a -2.3928907
```
