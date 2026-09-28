# Additive decomposition of profile change by skill groups

Splits the squared distance `d2` between base and target profiles of
each profession into the contributions of skill groups.

## Usage

``` r
decompose_profile_change(pair, skill_groups, group_cols)
```

## Arguments

- pair:

  Output of
  [`build_profile_pair()`](https://gmontaletti.github.io/skillviz/reference/build_profile_pair.md).

- skill_groups:

  A data.frame with the skill column of the pair and one or more group
  columns, one row per skill.

- group_cols:

  Character vector of group columns (e.g.
  `c("reusetype", "green", "digital")`).

## Value

A data.table with the profession column, `tipo_gruppo` (group column
name), `gruppo`, `d2_gruppo`, `d2` and `quota` (`d2_gruppo / d2`, 0 when
`d2 = 0`).

## Details

For any partition of the skills into groups \\G_1, \dots, G_M\\, \\d^2 =
\sum_m \sum\_{k \in G_m} (a_k - b_k)^2\\, so the group contributions add
up exactly to `d2` (the same property as
[`compute_decomposed_distance()`](https://gmontaletti.github.io/skillviz/reference/compute_decomposed_distance.md)).
Each column of `group_cols` defines one partition (group type). Skills
without a group, or missing from `skill_groups`, go to the group
`"(mancante)"`.

## Examples

``` r
panel <- data.table::data.table(
  cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
  mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
)
pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
groups <- data.table::data.table(skill_id = c("a", "b", "c"),
  green = c("no", "no", "si"))
decompose_profile_change(pair, groups, "green")
#>       cp4 tipo_gruppo gruppo d2_gruppo    d2 quota
#>    <char>      <char> <char>     <num> <num> <num>
#> 1:     p1       green     no      0.25   0.5   0.5
#> 2:     p1       green     si      0.25   0.5   0.5
```
