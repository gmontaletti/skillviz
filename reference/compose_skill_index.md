# Skill index from total and share trends

Builds, for each skill, the index of the skill stock as the product of
the trend of the total stock and the trend of the skill share, source by
source, then aggregates over sources with fixed weights.

## Usage

``` r
compose_skill_index(
  total_tr,
  share_tr,
  source_col,
  skill_col,
  time_col,
  weights,
  base_period,
  min_covered = 0.5,
  scale = 100,
  value_col = "trend",
  cap = NULL,
  type = c("geometric", "arithmetic")
)
```

## Arguments

- total_tr:

  Trends of the total by source: columns `source_col`, `time_col` and
  `value_col` (log scale), e.g. from
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)
  with `transform = "log"`.

- share_tr:

  Trends of skill shares by source: columns `source_col`, `skill_col`,
  `time_col` and `value_col` (logit scale), e.g. from
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)
  with `transform = "logit"`.

- source_col:

  Name of the source column.

- skill_col:

  Name of the skill column.

- time_col:

  Name of the time column.

- weights:

  A data.frame with columns `source_col` and `w`.

- base_period:

  Two dates (start, end, inclusive) or the name of a logical column of
  `share_tr`.

- min_covered:

  Minimum covered weight for a skill to be kept (in \[0, 1\]).

- scale:

  Value of the index in the base period (default 100).

- value_col:

  Name of the trend column in both tables (default `"trend"`).

- cap:

  `NULL` or the maximum weight of a source (see
  [`aggregate_index()`](https://gmontaletti.github.io/skillviz/reference/aggregate_index.md)).

- type:

  Aggregation across sources: `"geometric"` (default) or `"arithmetic"`
  (see
  [`aggregate_index()`](https://gmontaletti.github.io/skillviz/reference/aggregate_index.md)).

## Value

A data.table with columns `skill_col`, `time_col`, `indice`,
`n_sources`, `covered_weight` and `n_eff`. Attributes: `by_source`
(columns `source_col`, `skill_col`, `time_col`, `log_trend`, `indice`:
per-source skill indices, input of
[`index_jackknife()`](https://gmontaletti.github.io/skillviz/reference/index_jackknife.md))
and `dropped` (skills below `min_covered`, with `covered_weight` and
`n_sources`).

## Details

For source `s`, skill `k` and week `t`, the log trend of the skill stock
is \\L\_{s,k,t} = \tau\_{s,t} + \log \mathrm{plogis}(\eta\_{s,k,t})\\,
with \\\tau\\ the log trend of the total and \\\eta\\ the logit trend of
the share. The per-source index is `scale * exp(L - mean(L in base))`
(geometric base, as in
[`trend_index()`](https://gmontaletti.github.io/skillviz/reference/trend_index.md)
with `transform = "log"`), so it factorises exactly as
`I_total * I_share / scale`, where `I_total` is the geometric-base index
of \\\tau\\ and `I_share` the geometric-base index of
`log(plogis(eta))`. Note that
[`trend_index()`](https://gmontaletti.github.io/skillviz/reference/trend_index.md)
with `transform = "logit"` uses an arithmetic base on shares, which
differs from the geometric one by a constant factor per cell.

A cell (source, skill) is modelled when it has a non-missing index. The
coverage of a skill is the share of the total weight held by its
modelled sources; skills with coverage below `min_covered` are dropped
and listed in the attribute `dropped`. The remaining skills are
aggregated over sources with
[`aggregate_index()`](https://gmontaletti.github.io/skillviz/reference/aggregate_index.md)
rules: weights renormalised over the modelled sources in each week,
optional cap. With `type = "geometric"` (default) the skill index is
\\\exp(\sum_s \tilde w_s \log I\_{s,k,t})\\; with `type = "arithmetic"`
it is \\\sum_s \tilde w_s I\_{s,k,t}\\, with \\\tilde w_s\\ the weights
renormalised over the sources whose cell (source, skill) is modelled in
week `t`. The per-source indices (attribute `by_source`) do not depend
on `type`. The exact factorisation total x share holds source by source;
after aggregation it holds only for the geometric mean.

## Examples

``` r
weeks <- data.table::as.IDate("2024-01-01") + 7L * (0:103)
tot <- data.table::data.table(
  source = rep(c("a", "b"), each = 104), week = rep(weeks, 2),
  trend = c(5 + 0.002 * (1:104), 4 + 0.001 * (1:104))
)
sh <- data.table::data.table(
  source = rep(c("a", "b"), each = 104), skill = "k1",
  week = rep(weeks, 2), trend = stats::qlogis(0.1 + 0.0005 * (1:208))
)
w <- data.table::data.table(source = c("a", "b"), w = c(2, 1))
compose_skill_index(tot, sh, "source", "skill", "week", w,
  base_period = c("2024-01-01", "2024-12-31")
)
#>       skill       week    indice n_sources covered_weight n_eff
#>      <char>     <IDat>     <num>     <int>          <num> <num>
#>   1:     k1 2024-01-01  86.08609         2              1   1.8
#>   2:     k1 2024-01-08  86.60990         2              1   1.8
#>   3:     k1 2024-01-15  87.13517         2              1   1.8
#>   4:     k1 2024-01-22  87.66189         2              1   1.8
#>   5:     k1 2024-01-29  88.19007         2              1   1.8
#>  ---                                                           
#> 100:     k1 2025-11-24 145.62487         2              1   1.8
#> 101:     k1 2025-12-01 146.31227         2              1   1.8
#> 102:     k1 2025-12-08 147.00154         2              1   1.8
#> 103:     k1 2025-12-15 147.69267         2              1   1.8
#> 104:     k1 2025-12-22 148.38568         2              1   1.8
compose_skill_index(tot, sh, "source", "skill", "week", w,
  base_period = c("2024-01-01", "2024-12-31"), type = "arithmetic"
)
#>       skill       week    indice n_sources covered_weight n_eff
#>      <char>     <IDat>     <num>     <int>          <num> <num>
#>   1:     k1 2024-01-01  86.12633         2              1   1.8
#>   2:     k1 2024-01-08  86.64708         2              1   1.8
#>   3:     k1 2024-01-15  87.16940         2              1   1.8
#>   4:     k1 2024-01-22  87.69329         2              1   1.8
#>   5:     k1 2024-01-29  88.21877         2              1   1.8
#>  ---                                                           
#> 100:     k1 2025-11-24 146.00105         2              1   1.8
#> 101:     k1 2025-12-01 146.69933         2              1   1.8
#> 102:     k1 2025-12-08 147.39965         2              1   1.8
#> 103:     k1 2025-12-15 148.10201         2              1   1.8
#> 104:     k1 2025-12-22 148.80642         2              1   1.8
```
