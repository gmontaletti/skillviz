# Survival function of posting durations

From a table of duration counts, returns per group the share of postings
still open `j` days after their start, for `j = 0, ..., max_duration`.

## Usage

``` r
duration_survival(
  durations,
  by = NULL,
  duration_col = "duration",
  n_col = "n",
  max_duration = NULL
)
```

## Arguments

- durations:

  A data.frame or data.table of durations, one row per duration value
  (and group), or one row per posting when `n_col` is `NULL`.

- by:

  Character vector of grouping columns, or `NULL` for no grouping.

- duration_col:

  Name of the duration column (non-negative integer days).

- n_col:

  Name of the count column (number of postings with that duration), or
  `NULL` to count each row once.

- max_duration:

  Last value of `j` in the output (single non-negative integer).
  Default: the largest observed duration over all groups, so that every
  group has the same grid.

## Value

A data.table with columns `by`, `j` (integer, `0..max_duration`) and
`surv` (double, in `[0, 1]`), ordered by `by` and `j`.

## Details

The duration is `end - start` in days, so a posting open on `start..end`
(end inclusive) stays in the stock for `duration + 1` days. The function
returns `surv(j) = P(duration >= j)`, the share of postings still open
on day `start + j`; `surv(0) = 1` and the curve is non-increasing. Used
as weights `G(j)` by
[`standardized_stock()`](https://gmontaletti.github.io/skillviz/reference/standardized_stock.md).

When `max_duration` is smaller than the largest observed duration,
longer durations count as `max_duration`: the values of `surv` for
`j <= max_duration` are unchanged, only the grid is truncated.

## Examples

``` r
d <- data.table::data.table(
  source = c("a", "a", "b"),
  duration = c(2L, 5L, 3L),
  n = c(30L, 70L, 10L)
)
duration_survival(d, by = "source")
#>     source     j  surv
#>     <char> <int> <num>
#>  1:      a     0   1.0
#>  2:      a     1   1.0
#>  3:      a     2   1.0
#>  4:      a     3   0.7
#>  5:      a     4   0.7
#>  6:      a     5   0.7
#>  7:      b     0   1.0
#>  8:      b     1   1.0
#>  9:      b     2   1.0
#> 10:      b     3   1.0
#> 11:      b     4   0.0
#> 12:      b     5   0.0
```
