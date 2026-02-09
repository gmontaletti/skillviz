# Compute skill ranking per profession over time periods

Aggregates skill counts by profession and time period, then ranks skills
within each profession-period combination (descending by count). The
time unit controls the granularity of aggregation.

## Usage

``` r
compute_skill_ranking_series(
  competenze,
  time_unit = "year",
  cutoff_date = NULL
)
```

## Arguments

- competenze:

  A `data.table` with columns `escoskill_level_3` (skill label),
  `nome_3` (profession name), and `gdate` (date, either Date or IDate
  class).

- time_unit:

  Character, time unit for
  [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html).
  Default: `"year"`. Other options: `"quarter"`, `"month"`, etc.

- cutoff_date:

  Date or character in `"YYYY-MM-DD"` format. Observations on or after
  this date are excluded. Default: `NULL` (no filtering).

## Value

A `data.table` with columns `professione` (from `nome_3`), `mese`
(floored date), `skill` (from `escoskill_level_3`), `N` (count), and
`rango` (rank within profession-period, 1 = most frequent).

## Details

This reproduces the temporal ranking logic where skills are ranked by
frequency within each profession at each time step.
