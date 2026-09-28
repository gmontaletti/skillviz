# Breadth of skill change by profession

Summarises how many skills of each profession change significantly and
how widely the change spreads across skill groups, and ranks the
professions by breadth of change.

## Usage

``` r
compute_change_breadth(
  trend,
  pair,
  skill_groups,
  group_col,
  alpha = 0.05,
  slope_col = "pendenza",
  p_col = "p_adj"
)
```

## Arguments

- trend:

  Skill x profession trends, e.g.
  [`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md)
  with `key_cols = c(prof_col, skill_col)`.

- pair:

  Output of
  [`build_profile_pair()`](https://gmontaletti.github.io/skillviz/reference/build_profile_pair.md);
  defines the professions and the profile changes.

- skill_groups:

  A data.frame with the skill column of the pair and `group_col`.

- group_col:

  Group column (e.g. `"hier_label_1"`).

- alpha:

  Significance level. Default `0.05`.

- slope_col:

  Slope column of `trend`. Default `"pendenza"`.

- p_col:

  Adjusted p-value column of `trend`. Default `"p_adj"`.

## Value

A data.table with the profession column, `n_skill_crescita`,
`n_skill_calo`, `n_skill_cambio`, `ampiezza_gruppi`, `entropia_cambi`
and `rango_ampiezza`.

## Details

From the skill x profession trends (`trend`), a skill grows (declines)
when `p_col < alpha` and `slope_col` is positive (negative).
`ampiezza_gruppi` counts the groups of `group_col` touched by at least
one significant change. With \\c_m = \sum\_{k \in G_m} \|b_k - a_k\|\\
the absolute profile change of group \\m\\ (from `pair`),
`entropia_cambi` \\= -\sum_m p_m \log p_m / \log M\\ with \\p_m = c_m /
\sum_m c_m\\ and \\M\\ the number of groups in the skill universe of the
pair: 1 when change is spread evenly over groups, 0 when it is
concentrated in one. `rango_ampiezza` orders the professions by
`n_skill_cambio`, then `ampiezza_gruppi`, then `entropia_cambi` (all
decreasing; 1 = widest change).

## Examples

``` r
panel <- data.table::data.table(
  cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
  mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
)
pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
trend <- data.table::data.table(cp4 = "p1", skill_id = c("a", "b", "c"),
  pendenza = c(0, -0.2, 0.3), p_adj = c(0.8, 0.01, 0.01))
groups <- data.table::data.table(skill_id = c("a", "b", "c"),
  area = c("x", "x", "y"))
compute_change_breadth(trend, pair, groups, "area")
#> Key: <cp4>
#>       cp4 n_skill_crescita n_skill_calo n_skill_cambio ampiezza_gruppi
#>    <char>            <int>        <int>          <int>           <int>
#> 1:     p1                1            1              2               2
#>    entropia_cambi rango_ampiezza
#>             <num>          <int>
#> 1:              1              1
```
