# Index numbers from extracted trends

Turns a trend on the transformed scale into an index equal to `scale` on
average over a base period, separately for each key.

## Usage

``` r
trend_index(
  tr,
  key_cols,
  time_col,
  base_period,
  value_col = "trend",
  transform = c("log", "logit", "none"),
  scale = 100
)
```

## Arguments

- tr:

  A data.frame or data.table of trends (e.g. from
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)).

- key_cols:

  Character vector of key columns.

- time_col:

  Name of the time column (class Date or IDate).

- base_period:

  Either two dates (start and end, inclusive) or the name of a logical
  column of `tr` marking the base rows.

- value_col:

  Name of the trend column (default `"trend"`).

- transform:

  Scale of `value_col`: `"log"`, `"logit"` or `"none"`.

- scale:

  Value of the index in the base period (default 100).

## Value

A data.table with columns `key_cols`, `time_col`, `value_col` and
`indice`, ordered by key and time.

## Details

- `log`: `indice = scale * exp(trend - mean(trend in base))`. The base
  is geometric: the mean of `log(indice)` over the base period is
  exactly `log(scale)`, so the geometric mean of the index in the base
  is `scale`. A geometric base keeps indices multiplicative, which makes
  the decomposition total x share in
  [`compose_skill_index()`](https://gmontaletti.github.io/skillviz/reference/compose_skill_index.md)
  exact.

- `logit`: the trend is back-transformed to a share `p = plogis(trend)`
  and `indice = scale * p / mean(p in base)`: the arithmetic mean of the
  index in the base is `scale`.

- `none`: `indice = scale * trend / mean(trend in base)`; keys whose
  base mean is not positive get `NA`.

Keys without non-missing trend values in the base period get `NA` and a
warning.

## Examples

``` r
weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:103)
tr <- data.table::data.table(
  source = "a", week = weeks, trend = 5 + 0.002 * seq_along(weeks)
)
idx <- trend_index(tr, "source", "week",
  base_period = c("2024-01-01", "2024-12-31")
)
in_base <- idx$week <= as.Date("2024-12-31")
exp(mean(log(idx$indice[in_base])))
#> [1] 100
```
