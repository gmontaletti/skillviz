# Calibrate the minimum support by split-half reliability

Chooses the smallest support threshold at which the trend slope is
reliable, measured by the agreement of slopes estimated on two random
halves of the postings.

## Usage

``` r
calibrate_min_support(
  incidence,
  id_col = "general_id",
  time_col = "mese_idx",
  skill_col = "skill_id",
  group_cols = NULL,
  window = 24L,
  grid = c(5, 10, 20, 30, 50, 100),
  target = 0.7,
  min_series = 10L,
  seed = 1L,
  weights = c("pooled", "observed")
)
```

## Arguments

- incidence:

  A data.frame/data.table with one row per posting and skill (duplicates
  are removed). Must contain `id_col`, `time_col`, `skill_col` and any
  `group_cols`. Each posting must belong to a single period and group.

- id_col:

  Posting identifier column. Default `"general_id"`.

- time_col:

  Period column (typically an integer month index). Default
  `"mese_idx"`.

- skill_col:

  Skill identifier column. Default `"skill_id"`.

- group_cols:

  Optional character vector of grouping columns (e.g. `"cp4"`). Default
  `NULL` (global panel).

- window:

  Number of trailing periods for the slope. Default `24`.

- grid:

  Numeric vector of candidate thresholds. Default
  `c(5, 10, 20, 30, 50, 100)`.

- target:

  Minimum split-half Spearman correlation. Default `0.7`.

- min_series:

  Minimum number of series for a valid correlation. Default `10`.

- seed:

  Integer seed for the split. Default `1`.

- weights:

  Trend weighting, `"pooled"` (default) or `"observed"`; see
  [`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md).

## Value

A list with `tabella` (data.table with `n_min`, `n_serie`, `rho`) and
`n_min` (chosen threshold, `NA` with a warning when no value reaches
`target`).

## Details

Postings are split at random into two halves (by posting id, with a
fixed `seed`; the global RNG state is restored). For each half the share
panel is built and the slope of the last `window` periods is estimated
as in
[`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md)
(no breaks, no drift, the chosen `weights`). The support of a series is
its average count per 3 periods in the full sample over the window, \\3
\sum_t x_t / window\\, the same unit as the rolling count of
[`detect_skill_onset()`](https://gmontaletti.github.io/skillviz/reference/detect_skill_onset.md).
For each value of `grid`, the Spearman correlation between the
half-sample slopes of series with support at least that value is
computed; the chosen threshold is the smallest one with correlation at
least `target` and at least `min_series` series.

## Examples

``` r
set.seed(4)
inc <- data.table::rbindlist(lapply(1:12, function(m) {
  ids <- (m * 1000):(m * 1000 + 299)
  data.table::rbindlist(lapply(letters[1:12], function(s) {
    p <- plogis(-2 + (match(s, letters) - 6) * 0.02 * m)
    data.table::data.table(general_id = ids[runif(300) < p],
      skill_id = s)
  }))[, mese_idx := m]
}))
calibrate_min_support(inc, window = 12, grid = c(5, 20), min_series = 5)
#> $tabella
#>    n_min n_serie       rho
#>    <num>   <int>     <num>
#> 1:     5      12 0.9440559
#> 2:    20      12 0.9440559
#> 
#> $n_min
#> [1] 5
#> 
```
