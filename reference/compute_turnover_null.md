# Split-half null distribution of profile turnover

Estimates, for each profession, the distribution of `turnover_netto`
when there is no real change, by comparing two random halves of the
postings of the same window.

## Usage

``` r
compute_turnover_null(
  incidence,
  prof_col,
  skill_col = "skill_id",
  id_col = "general_id",
  time_col = "mese_idx",
  base,
  target,
  n_rep = 20L,
  prob = 0.95,
  seed = 1L
)
```

## Arguments

- incidence:

  Posting-level data.frame with `id_col`, `time_col`, `prof_col` and
  `skill_col`, one row per posting and skill.

- prof_col:

  Profession column.

- skill_col:

  Skill column. Default `"skill_id"`.

- id_col:

  Posting identifier. Default `"general_id"`.

- time_col:

  Period column. Default `"mese_idx"`.

- base, target:

  Numeric vectors `c(from, to)`.

- n_rep:

  Number of random splits per window. Default `20`.

- prob:

  Quantile of the null distribution. Default `0.95`.

- seed:

  Integer seed. Default `1`.

## Value

A data.table with the profession column, `n_nullo` (null values),
`media_nullo` and `q_nullo` (the `prob` quantile).

## Details

For each replicate and each window (`base` and `target`), the postings
of the window are split at random into two halves; the two halves play
the role of base and target in
[`compute_profile_turnover()`](https://gmontaletti.github.io/skillviz/reference/compute_profile_turnover.md).
The null sample of a profession collects `2 * n_rep` values of
`turnover_netto`; its `prob` quantile is the threshold above which
observed turnover exceeds the variation expected from sampling alone.
Each posting must belong to one profession. The global RNG state is
restored.

## Examples

``` r
set.seed(6)
inc <- data.table::data.table(general_id = rep(1:200, each = 2),
  mese_idx = rep(rep(1:2, each = 100), each = 2), cp4 = "p1",
  skill_id = sample(letters[1:5], 400, replace = TRUE))
compute_turnover_null(inc, "cp4", base = c(1, 1), target = c(2, 2),
  n_rep = 5)
#>       cp4 n_nullo media_nullo    q_nullo
#>    <char>   <int>       <num>      <num>
#> 1:     p1      10  0.02599631 0.07890718
```
