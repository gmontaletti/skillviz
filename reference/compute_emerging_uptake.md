# Uptake of emerging skills by profession

Measures, for each profession and window, the share of postings that
mention at least one emerging skill, and compares its change with the
change observed over all postings.

## Usage

``` r
compute_emerging_uptake(
  incidence,
  emerging,
  prof_col,
  skill_col = "skill_id",
  id_col = "general_id",
  time_col = "mese_idx",
  base,
  target,
  core_share = 0.8,
  conf = 0.95,
  n_boot = 1000L,
  seed = 1L
)
```

## Arguments

- incidence:

  Posting-level data.frame with `id_col`, `time_col`, `prof_col` and
  `skill_col`, one row per posting and skill.

- emerging:

  Vector of emerging skill identifiers.

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

- core_share:

  Share of mentions defining the core profile. Default `0.8`.

- conf:

  Confidence level of the regional interval. Default `0.95`.

- n_boot:

  Bootstrap replicates. Default `1000`.

- seed:

  Integer seed. Default `1`.

## Value

A list with:

- `professioni`: data.table with the profession column, `n_base`,
  `quota_base`, `n_target`, `quota_target`, `delta_quota`,
  `n_emergenti_profilo`;

- `regione`: one-row data.table with `n_base`, `quota_base`, `n_target`,
  `quota_target`, `delta_quota`, `delta_lo`, `delta_hi`.

## Details

`quota_base` and `quota_target` are the shares of the profession's
postings (with at least one skill) mentioning at least one skill of
`emerging` in the base and target window; `delta_quota` is their
difference. The same quantities over all postings form the regional
reference; its confidence interval for the change is obtained by a
bootstrap of postings, which for a share of independent postings reduces
to binomial resampling in each window.

`n_emergenti_profilo` counts the emerging skills in the core profile of
the profession in the target window: the most mentioned skills that
together cover `core_share` of its skill mentions.

## Examples

``` r
inc <- data.table::data.table(general_id = rep(1:8, each = 2),
  mese_idx = rep(c(1, 1, 1, 1, 2, 2, 2, 2), each = 2),
  cp4 = rep(c("p1", "p2"), each = 2, times = 4),
  skill_id = c("a", "b", "a", "c", "a", "b", "a", "c",
    "e", "b", "a", "c", "e", "a", "a", "c"))
compute_emerging_uptake(inc, emerging = "e", prof_col = "cp4",
  base = c(1, 1), target = c(2, 2), n_boot = 100)
#> $professioni
#> Key: <cp4>
#>       cp4 n_base quota_base n_target quota_target delta_quota
#>    <char>  <int>      <num>    <int>        <num>       <num>
#> 1:     p1      2          0        2            1           1
#> 2:     p2      2          0        2            0           0
#>    n_emergenti_profilo
#>                  <int>
#> 1:                   1
#> 2:                   0
#> 
#> $regione
#>    n_base quota_base n_target quota_target delta_quota delta_lo delta_hi
#>     <int>      <num>    <int>        <num>       <num>    <num>    <num>
#> 1:      4          0        4          0.5         0.5        0  0.88125
#> 
```
