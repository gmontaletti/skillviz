# Monthly skill share panel from posting-level incidence

Builds the share panel \\s\_{k,t} = x\_{k,t} / n_t\\, where \\x\_{k,t}\\
is the number of postings of period \\t\\ that mention skill \\k\\ and
\\n_t\\ is the number of postings of period \\t\\ with at least one
skill. When `group_cols` are given (e.g. a profession code) counts and
denominators are computed within each group.

## Usage

``` r
compute_share_panel(
  incidence,
  id_col = "general_id",
  time_col = "mese_idx",
  skill_col = "skill_id",
  group_cols = NULL,
  fill_zero = TRUE
)
```

## Arguments

- incidence:

  A data.frame/data.table with one row per posting and skill (duplicates
  are removed). Must contain `id_col`, `time_col`, `skill_col` and any
  `group_cols`. Each posting must belong to a single period and group.

- id_col:

  Posting identifier column. Default `"general_id"`.

- time_col:

  Period column (typically an integer month index). Default
  `"mese_idx"`.

- skill_col:

  Skill identifier column. Default `"skill_id"`.

- group_cols:

  Optional character vector of grouping columns (e.g. `"cp4"`). Default
  `NULL` (global panel).

- fill_zero:

  Logical. When `TRUE` (default) every group-skill pair observed at
  least once gets a row for every period in which its group has
  postings, with `x = 0` where the skill is absent.

## Value

A data.table with columns `group_cols`, `skill_col`, `time_col`, `x`
(postings with the skill), `n` (postings with at least one skill) and
`quota` (`x / n`), keyed by the first three.

## Examples

``` r
inc <- data.table::data.table(
  general_id = c(1, 1, 2, 3, 3, 4),
  mese_idx = c(1, 1, 1, 2, 2, 2),
  skill_id = c("a", "b", "a", "a", "c", "b")
)
compute_share_panel(inc)
#> Key: <skill_id, mese_idx>
#>    skill_id mese_idx     x     n quota
#>      <char>    <num> <int> <int> <num>
#> 1:        a        1     2     2   1.0
#> 2:        a        2     1     2   0.5
#> 3:        b        1     1     2   0.5
#> 4:        b        2     1     2   0.5
#> 5:        c        1     0     2   0.0
#> 6:        c        2     1     2   0.5
```
