# Daily stock of open postings from dated events

Builds the daily stock of open postings as the running sum of inflow
minus outflow on a complete daily grid, per group.

## Usage

``` r
stock_from_events(
  events,
  by = NULL,
  date_col = "date",
  in_col = "inflow",
  out_col = "outflow",
  from = NULL,
  to = NULL
)
```

## Arguments

- events:

  A data.frame or data.table of dated events.

- by:

  Character vector of grouping columns, or `NULL` for no grouping.

- date_col:

  Name of the date column (class Date or IDate).

- in_col:

  Name of the inflow column (non-negative counts).

- out_col:

  Name of the outflow column (non-negative counts).

- from, to:

  First and last day of the output (Date, IDate or `"YYYY-MM-DD"`).
  Default: the first and last event date over all groups.

## Value

A data.table with columns `by`, `date_col` (IDate), `inflow`, `outflow`
and `stock`, one row per group and day from `from` to `to`, ordered by
`by` and date. Counts are integer when both input columns are integer,
double otherwise.

## Details

The input holds one row per `by` x date with the number of postings
entering the stock that day (`in_col`) and leaving it that day
(`out_col`; a posting open until `expire_date` inclusive is counted as
outflow on `expire_date + 1`). Rows with the same `by` x date are
summed. The function builds the complete daily grid of every group,
fills the missing days with zero events and computes
`stock = cumsum(inflow - outflow)` per group (a difference array).

The cumulative sum always starts at the first event date of the data, so
events before `from` are carried into the stock of `from`; events after
`to` are ignored. A negative stock means that some postings leave before
they enter (inconsistent events) and raises an error.

## Examples

``` r
iv <- data.table::data.table(
  start = data.table::as.IDate(c("2024-01-01", "2024-01-03", "2024-01-02")),
  end = data.table::as.IDate(c("2024-01-02", "2024-01-05", "2024-01-02"))
)
ev <- events_from_intervals(iv, start_col = "start", end_col = "end")
stock_from_events(ev)
#>          date inflow outflow stock
#>        <IDat>  <int>   <int> <int>
#> 1: 2024-01-01      1       0     1
#> 2: 2024-01-02      1       0     2
#> 3: 2024-01-03      1       2     1
#> 4: 2024-01-04      0       0     1
#> 5: 2024-01-05      0       0     1
#> 6: 2024-01-06      0       1     0
```
