# Revealed comparative advantage of skills by profession and window

Computes, for each window, the posting-based revealed comparative
advantage of skill \\k\\ in profession \\j\\.

## Usage

``` r
compute_rca_panel(
  panel,
  skill_col = "skill_id",
  prof_col,
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  windows
)
```

## Arguments

- panel:

  Profession-level share panel (e.g. from
  [`compute_share_panel()`](https://gmontaletti.github.io/skillviz/reference/compute_share_panel.md)
  with `group_cols = prof_col`).

- skill_col:

  Skill column. Default `"skill_id"`.

- prof_col:

  Profession column.

- time_col, x_col, n_col:

  See
  [`compute_share_trend()`](https://gmontaletti.github.io/skillviz/reference/compute_share_trend.md).

- windows:

  Named list of numeric vectors `c(from, to)`, e.g.
  `list(W1 = c(1, 12), W3 = c(25, 36))`.

## Value

A data.table with `finestra`, `skill_col`, `prof_col`, `x`, `n`, `quota`
(\\s\_{kj}\\), `quota_skill` (\\s_k\\) and `rca`.

## Details

Within window \\W\\, \\s\_{kj} = \sum\_{t \in W} x\_{kjt} / \sum\_{t \in
W} n\_{jt}\\ is the share of the profession's postings that mention the
skill and \\s_k = \sum_j \sum\_{t} x\_{kjt} / \sum_j \sum_t n\_{jt}\\
the share over all professions. The index is \\RCA\_{kj} = s\_{kj} /
s_k\\; values of at least 1 indicate specialisation. The denominator of
a profession-period must be the same on all its rows; professions are
assumed to partition the postings.

## Examples

``` r
panel <- data.table::data.table(
  cp4 = rep(c("p1", "p2"), each = 2), skill_id = rep(c("a", "b"), 2),
  mese_idx = 1, x = c(30, 5, 10, 20), n = rep(c(100, 100), each = 2)
)
compute_rca_panel(panel, prof_col = "cp4", windows = list(W1 = c(1, 1)))
#>    finestra skill_id    cp4     x     n quota quota_skill   rca
#>      <char>   <char> <char> <num> <num> <num>       <num> <num>
#> 1:       W1        a     p1    30   100  0.30       0.200   1.5
#> 2:       W1        b     p1     5   100  0.05       0.125   0.4
#> 3:       W1        a     p2    10   100  0.10       0.200   0.5
#> 4:       W1        b     p2    20   100  0.20       0.125   1.6
```
