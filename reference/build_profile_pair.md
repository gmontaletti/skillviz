# Skill profiles of professions in two windows

Builds the sparse skill profiles of each profession in a base and a
target window, the input of the profile-change functions.

## Usage

``` r
build_profile_pair(
  panel,
  prof_col,
  skill_col = "skill_id",
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  base,
  target
)
```

## Arguments

- panel:

  Profession-level share panel (e.g. from
  [`compute_share_panel()`](https://gmontaletti.github.io/skillviz/reference/compute_share_panel.md)
  with `group_cols = prof_col`).

- prof_col:

  Profession column.

- skill_col:

  Skill column. Default `"skill_id"`.

- time_col, x_col, n_col:

  See
  [`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md).

- base, target:

  Numeric vectors `c(from, to)` with the bounds of the base and target
  windows.

## Value

An object of class `skillviz_profile_pair`: a list with `a` and `b`
(sparse `dgCMatrix`, skills x professions, base and target shares),
`n_a` and `n_b` (named postings per profession), and the metadata
`prof_col`, `skill_col`, `base`, `target`.

## Details

For profession \\j\\ and window \\W\\, the profile entry of skill \\k\\
is the share of the profession's postings that mention the skill,
\\s\_{kj} = \sum\_{t \in W} x\_{kjt} / \sum\_{t \in W} n\_{jt}\\. The
denominator of a profession-period must be the same on all its rows.
Only professions with postings in both windows are kept; the skill
universe is the union of skills with a positive count in either window.

## Examples

``` r
panel <- data.table::data.table(
  cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
  mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
)
pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
pair$a
#> 3 x 1 sparse Matrix of class "dgCMatrix"
#>    p1
#> a 0.5
#> b 0.5
#> c .  
```
