# Aggregate a daily series to weeks

Aggregates daily rows to calendar weeks, taking the mean of some columns
(stocks) and the sum of others (flows).

## Usage

``` r
to_weekly(
  dt,
  by = NULL,
  date_col = "date",
  mean_cols = NULL,
  sum_cols = NULL,
  week_start = 1L
)
```

## Arguments

- dt:

  A data.frame or data.table with one row per `by` x date.

- by:

  Character vector of grouping columns, or `NULL` for no grouping.

- date_col:

  Name of the date column (class Date or IDate).

- mean_cols:

  Character vector of numeric columns averaged over the week, or `NULL`.

- sum_cols:

  Character vector of numeric columns summed over the week, or `NULL`.

- week_start:

  First day of the week, integer in `1..7` with 1 = Monday and 7 =
  Sunday.

## Value

A data.table with columns `by`, `week` (IDate), `n_days` (integer), then
`mean_cols` and `sum_cols` under their own names, ordered by `by` and
`week`.

## Details

Weeks start on `week_start` (ISO numbering: 1 = Monday, ..., 7 = Sunday;
default Monday, as in ISO 8601 weeks). The `week` column holds the first
day of the week. Partial weeks (at the edges of the series or with
missing days) are kept: `n_days` reports the number of days present, so
the caller can drop weeks with `n_days < 7`. `NA` values propagate to
the weekly mean or sum.

## Examples

``` r
d <- data.table::data.table(
  date = data.table::as.IDate("2024-01-01") + 0:13,
  stock = 100:113,
  inflow = rep(1:2, 7)
)
to_weekly(d, mean_cols = "stock", sum_cols = "inflow")
#>          week n_days stock inflow
#>        <IDat>  <int> <num>  <int>
#> 1: 2024-01-01      7   103     10
#> 2: 2024-01-08      7   110     11
```
