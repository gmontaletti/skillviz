# Compute time-series features per source

Extract STL decomposition features from a tsibble produced by
[`prepare_source_tsibble()`](https://gmontaletti.github.io/skillviz/reference/prepare_source_tsibble.md).
Returns summary statistics (mean, sd, sum) together with trend and
seasonality strength and the coefficient of variation.

## Usage

``` r
compute_source_features(tst)
```

## Arguments

- tst:

  A tsibble as returned by
  [`prepare_source_tsibble()`](https://gmontaletti.github.io/skillviz/reference/prepare_source_tsibble.md),
  with column `N` and key `fonte`.

## Value

A tibble with one row per source containing columns: `fonte`, `mea`
(mean), `sd` (standard deviation), `sum` (total), trend/seasonality
strength from
[`feasts::feat_stl()`](https://feasts.tidyverts.org/reference/feat_stl.html),
and `cv` (coefficient of variation = sd / mean).
