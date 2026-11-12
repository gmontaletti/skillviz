# Event table from posting intervals

Turns a table of posting intervals (one row per posting, or per group of
postings with the same interval) into the table of dated events consumed
by
[`stock_from_events()`](https://gmontaletti.github.io/skillviz/reference/stock_from_events.md):
each interval adds `n` to the inflow of its start date and `n` to the
outflow of the day after its end date.

## Usage

``` r
events_from_intervals(dt, by = NULL, start_col, end_col, n_col = NULL)
```

## Arguments

- dt:

  A data.frame or data.table of intervals.

- by:

  Character vector of grouping columns, or `NULL` for no grouping.

- start_col:

  Name of the start-date column (class Date or IDate).

- end_col:

  Name of the end-date column (class Date or IDate, inclusive).

- n_col:

  Name of a column of non-negative weights (number of postings per row),
  or `NULL` to count each row once.

## Value

A data.table with columns `by`, `date` (IDate), `inflow` and `outflow`,
one row per `by` x date with at least one event, ordered by `by` and
`date`. Counts are integer when `n_col` is `NULL` or integer.

## Details

A posting is open on day `d` when `start <= d <= end` (end inclusive),
so it leaves the stock on `end + 1`. Events are aggregated by `by` x
date. This helper is meant for tests and small data; on large data the
same event table is better computed in the database.

## Examples

``` r
iv <- data.table::data.table(
  source = c("a", "a", "b"),
  start = data.table::as.IDate(c("2024-01-01", "2024-01-03", "2024-01-02")),
  end = data.table::as.IDate(c("2024-01-02", "2024-01-05", "2024-01-02"))
)
events_from_intervals(iv, by = "source", start_col = "start", end_col = "end")
#>    source       date inflow outflow
#>    <char>     <IDat>  <int>   <int>
#> 1:      a 2024-01-01      1       0
#> 2:      a 2024-01-03      1       1
#> 3:      a 2024-01-06      0       1
#> 4:      b 2024-01-02      1       0
#> 5:      b 2024-01-03      0       1
```
