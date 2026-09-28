# Diffusion of skills across professions by window

Counts the professions in which a skill has a revealed comparative
advantage and measures how evenly the skill is spread over professions.

## Usage

``` r
compute_diffusion_panel(
  rca,
  skill_col = "skill_id",
  prof_col,
  threshold = 1,
  compare = NULL
)
```

## Arguments

- rca:

  Output of
  [`compute_rca_panel()`](https://gmontaletti.github.io/skillviz/reference/compute_rca_panel.md).

- skill_col:

  Skill column. Default `"skill_id"`.

- prof_col:

  Profession column.

- threshold:

  RCA threshold. Default `1`.

- compare:

  Optional character vector of two window names, e.g. `c("W1", "W3")`.
  When given, the result has one row per skill with values of both
  windows and their differences.

## Value

When `compare = NULL`, a data.table with `skill_col`, `finestra`,
`n_prof` (professions with `x > 0`), `n_prof_rca` and `entropia`.
Otherwise a data.table with `skill_col`, `n_prof_rca_base`,
`n_prof_rca_target`, `delta_n_prof_rca`, `entropia_base`,
`entropia_target`, `delta_entropia`, where `base` and `target` are the
first and second element of `compare`; a skill absent from a window
counts zero professions and zero entropy.

## Details

For skill \\k\\ in window \\W\\, `n_prof_rca` is the number of
professions with \\RCA\_{kj} \ge\\ `threshold`. With \\p\_{kj} = s\_{kj}
/ \sum_j s\_{kj}\\, the normalised entropy is \\H_k = -\sum_j p\_{kj}
\log p\_{kj} / \log J\\, where \\J\\ is the number of professions
present in the window. \\H_k = 1\\ when the skill has the same share in
all professions and 0 when it appears in one profession only.

## Examples

``` r
panel <- data.table::data.table(
  cp4 = rep(c("p1", "p2"), each = 2), skill_id = rep(c("a", "b"), 2),
  mese_idx = 1, x = c(30, 5, 10, 20), n = rep(c(100, 100), each = 2)
)
rca <- compute_rca_panel(panel, prof_col = "cp4",
  windows = list(W1 = c(1, 1)))
compute_diffusion_panel(rca, prof_col = "cp4")
#>    skill_id finestra n_prof n_prof_rca  entropia
#>      <char>   <char>  <int>      <int>     <num>
#> 1:        a       W1      2          1 0.8112781
#> 2:        b       W1      2          1 0.7219281
```
