# Compute year-over-year variation in skill rankings

Takes the output of
[`compute_skill_ranking_series()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_ranking_series.md)
and calculates ranking changes between consecutive time periods. The
function pivots the series to wide format with rank and count columns
per period, filters by a minimum count threshold, computes the rank
variation (previous rank minus current rank, so positive values indicate
improvement), and returns the top-k skills per profession.

## Usage

``` r
compute_skill_variation(serie, min_n = 30L, top_k = 10L)
```

## Arguments

- serie:

  A `data.table` as returned by
  [`compute_skill_ranking_series()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_ranking_series.md),
  with columns `professione`, `mese`, `skill`, `N`, and `rango`.

- min_n:

  Integer, minimum count in the most recent period for a skill to be
  included in the variation analysis. Default: `30L`.

- top_k:

  Integer, number of top-ranked skills to return per profession (by most
  recent rank). Default: `10L`.

## Value

A `data.table` in wide format with one row per profession-skill pair.
Columns include `professione`, `skill`, rank and count columns for each
time period (named as `rango_<period>` and `N_<period>`), and
`variazione` (rank change from previous to current period).
