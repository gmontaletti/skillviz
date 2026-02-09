# Filter sources by stability criteria

Select data sources whose coefficient of variation is at most
`cv_threshold` and whose total announcement count is at least
`min_total`. Rows with missing values are excluded.

## Usage

``` r
filter_stable_sources(features, cv_threshold = 0.6, min_total = 0L)
```

## Arguments

- features:

  A tibble as returned by
  [`compute_source_features()`](https://gmontaletti.github.io/skillviz/reference/compute_source_features.md).
  Must contain columns `fonte`, `cv` and `sum`.

- cv_threshold:

  Numeric maximum coefficient of variation (default 0.6).

- min_total:

  Integer minimum total announcement count across all months (default 0,
  i.e. no minimum).

## Value

A character vector of stable source names.
