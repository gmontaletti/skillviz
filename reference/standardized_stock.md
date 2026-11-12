# Standardised-duration stock

Computes the stock that the observed inflow would generate under a fixed
duration distribution: `S*_d = sum_j F_{d - j} G(j)`, where `F` is the
daily inflow and `G` a survival curve from
[`duration_survival()`](https://gmontaletti.github.io/skillviz/reference/duration_survival.md).

## Usage

``` r
standardized_stock(
  flow,
  survival,
  by = NULL,
  date_col = "date",
  flow_col = "inflow"
)
```

## Arguments

- flow:

  A data.frame or data.table with the daily inflow, one row per `by` x
  date.

- survival:

  A data.table as returned by
  [`duration_survival()`](https://gmontaletti.github.io/skillviz/reference/duration_survival.md),
  with columns `j`, `surv` and optionally a subset of `by`.

- by:

  Character vector of grouping columns of `flow`, or `NULL`.

- date_col:

  Name of the date column of `flow` (class Date or IDate).

- flow_col:

  Name of the inflow column of `flow` (non-negative, `NA` allowed).

## Value

A data.table with columns `by`, `date_col` (IDate), `stock_std` (double)
and `complete` (logical: `TRUE` when the day is at least `max(j)` days
after the first date of the grid), ordered by `by` and date.

## Details

The actual stock is `S_d = sum_j F_{d - j} P_d(duration >= j)` with a
duration distribution that may change over time (for example when a
source starts capping durations). Holding `G` fixed (for example at its
base-period value) removes the level shifts caused by changes in the
duration regime, while keeping the signal carried by the inflow. When
the durations are constant and `G` is computed from them, `S*` equals
the actual stock.

The flow is placed on a complete daily grid from the first to the last
date over all groups; missing days count as zero inflow, while explicit
`NA` values propagate to the `max(j) + 1` days whose window contains
them. The convolution is exact (one-sided,
[`stats::filter()`](https://rdrr.io/r/stats/filter.html) with zero
padding before the first day). Days whose window starts before the first
date of the grid are computed on a truncated window and flagged with
`complete = FALSE`.

The survival curves are matched to the flow on the columns of `by` that
are present in `survival`: a curve per source can be applied to a flow
by source and skill. When `survival` has none of the `by` columns, the
same curve applies to all groups.

## Examples

``` r
flow <- data.table::data.table(
  date = data.table::as.IDate("2024-01-01") + 0:9,
  inflow = c(5L, 3L, 4L, 6L, 2L, 5L, 4L, 3L, 6L, 5L)
)
g <- duration_survival(
  data.table::data.table(duration = c(1L, 3L), n = c(1L, 1L))
)
standardized_stock(flow, g)
#>           date stock_std complete
#>         <IDat>     <num>   <lgcl>
#>  1: 2024-01-01       5.0    FALSE
#>  2: 2024-01-02       8.0    FALSE
#>  3: 2024-01-03       9.5    FALSE
#>  4: 2024-01-04      14.0     TRUE
#>  5: 2024-01-05      11.5     TRUE
#>  6: 2024-01-06      12.0     TRUE
#>  7: 2024-01-07      13.0     TRUE
#>  8: 2024-01-08      10.5     TRUE
#>  9: 2024-01-09      13.5     TRUE
#> 10: 2024-01-10      14.5     TRUE
```
