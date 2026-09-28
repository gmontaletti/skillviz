# Classify emerging professions

Applies the three-condition rule that defines an emerging profession:
growing demand, profile change beyond sampling noise, and uptake of
emerging skills faster than the regional average.

## Usage

``` r
classify_emerging_professions(
  demand,
  turnover,
  turnover_null,
  uptake,
  prof_col,
  alpha = 0.05,
  slope_col = "pendenza",
  p_col = "p_adj"
)
```

## Arguments

- demand:

  Profession demand trends, e.g.
  [`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md)
  on a panel of profession counts over all postings.

- turnover:

  Output of
  [`compute_profile_turnover()`](https://gmontaletti.github.io/skillviz/reference/compute_profile_turnover.md).

- turnover_null:

  Output of
  [`compute_turnover_null()`](https://gmontaletti.github.io/skillviz/reference/compute_turnover_null.md).

- uptake:

  Output of
  [`compute_emerging_uptake()`](https://gmontaletti.github.io/skillviz/reference/compute_emerging_uptake.md).

- prof_col:

  Profession column.

- alpha:

  Significance level. Default `0.05`.

- slope_col, p_col:

  Slope and adjusted p-value columns of `demand`. Defaults `"pendenza"`,
  `"p_adj"`.

## Value

A data.table with the profession column, `pendenza`, `turnover_netto`,
`q_nullo`, `delta_quota`, the three condition flags and `emergente`.

## Details

A profession is emerging (`emergente = TRUE`) when all hold:

1.  `crescita_significativa`: the drift-net demand slope in `demand` is
    positive with `p_col < alpha`;

2.  `turnover_oltre_nullo`: `turnover_netto` exceeds the `q_nullo`
    quantile of its split-half null distribution;

3.  `assorbimento_oltre_regione`: `delta_quota` of the profession
    exceeds the upper bound `delta_hi` of the regional change.

Missing information makes a condition `FALSE`.

## Examples

``` r
demand <- data.table::data.table(cp4 = c("p1", "p2"),
  pendenza = c(0.05, 0.05), p_adj = c(0.01, 0.01))
turnover <- data.table::data.table(cp4 = c("p1", "p2"),
  turnover_netto = c(0.3, 0.01))
null <- data.table::data.table(cp4 = c("p1", "p2"), q_nullo = 0.05)
uptake <- list(
  professioni = data.table::data.table(cp4 = c("p1", "p2"),
    delta_quota = c(0.2, 0.2)),
  regione = data.table::data.table(delta_hi = 0.05)
)
classify_emerging_professions(demand, turnover, null, uptake, "cp4")
#> Key: <cp4>
#>       cp4 pendenza turnover_netto q_nullo delta_quota crescita_significativa
#>    <char>    <num>          <num>   <num>       <num>                 <lgcl>
#> 1:     p1     0.05           0.30    0.05         0.2                   TRUE
#> 2:     p2     0.05           0.01    0.05         0.2                   TRUE
#>    turnover_oltre_nullo assorbimento_oltre_regione emergente
#>                  <lgcl>                     <lgcl>    <lgcl>
#> 1:                 TRUE                       TRUE      TRUE
#> 2:                FALSE                       TRUE     FALSE
```
