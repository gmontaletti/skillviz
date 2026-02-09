# Prepare a tsibble of announcement counts by source and month

Convert a data.table of job announcements into a tsibble keyed by data
source (`fonte`) and indexed by year-month, counting unique
announcements per source per month.

## Usage

``` r
prepare_source_tsibble(ann)
```

## Arguments

- ann:

  A data.table of announcements. Must contain columns `general_id` and
  `source`. Also requires either a `gdate` column (IDate) or the raw
  year/month/day columns used by
  [`parse_ymd_columns()`](https://gmontaletti.github.io/skillviz/reference/parse_ymd_columns.md).

## Value

A tsibble with key `fonte`, index `mese` (yearmonth) and column `N`
(unique announcement count per source-month).
