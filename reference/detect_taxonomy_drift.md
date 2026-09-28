# Detect extraction and taxonomy breaks

Flags months with anomalous jumps in data-quality metrics and skills
whose first appearance looks like a taxonomy artefact.

## Usage

``` r
detect_taxonomy_drift(
  quality,
  metrics,
  time_col = "mese_idx",
  prob = 0.995,
  threshold = NULL,
  panel_prof = NULL,
  skill_col = "skill_id",
  prof_col = NULL,
  x_col = "x",
  skill_prob = 0.95
)
```

## Arguments

- quality:

  A data.frame with one row per period: `time_col` and the metric
  columns (e.g. number of postings, sources, skills per posting, new
  skills, drift index).

- metrics:

  Character vector of metric columns in `quality`.

- time_col:

  Numeric period column. Default `"mese_idx"`.

- prob:

  Quantile of the pooled absolute robust z-scores used as threshold.
  Default `0.995`.

- threshold:

  Optional fixed threshold on the absolute robust z-score, overriding
  `prob`. Default `NULL`.

- panel_prof:

  Optional profession-level panel with `skill_col`, `prof_col`,
  `time_col` and `x_col`.

- skill_col, prof_col, x_col:

  Columns of `panel_prof`.

- skill_prob:

  Quantile of the reference distribution of `n_prof_prima`. Default
  `0.95`.

## Value

A list with:

- `mesi`: data.table with `time_col`, one `z_<metric>` column per
  metric, `z_max` (largest absolute z) and `rottura`;

- `soglia`: the threshold used;

- `skill`: `NULL`, or a data.table with `skill_col`, `prima_comparsa`,
  `n_prof_prima`, `flag_tassonomia`;

- `soglia_prof`: the `n_prof_prima` threshold (or `NA`).

## Details

**Months.** For each metric the first difference \\\Delta m_t\\ is
converted to a robust z-score (median and MAD of the differences). The
threshold is the empirical `prob` quantile of the absolute z-scores
pooled over all metrics and months, unless `threshold` is given. A month
is a break (`rottura = TRUE`) when any metric exceeds it.

**Skills.** When `panel_prof` is given, each skill gets its first period
with a positive count and the number of professions with a positive
count in that period (`n_prof_prima`). Skills first seen in the first
period of the panel are left-censored and never flagged. The reference
distribution is `n_prof_prima` of skills first seen in non-break
periods; a skill is a taxonomy artefact (`flag_tassonomia = TRUE`) when
it is first seen in a break month and `n_prof_prima` exceeds the
`skill_prob` quantile of the reference.

## Examples

``` r
q <- data.table::data.table(mese_idx = 1:24,
  skill_per_annuncio = c(rep(10, 12), rep(12, 12)) + sin(1:24) / 10)
detect_taxonomy_drift(q, metrics = "skill_per_annuncio")$mesi
#>     mese_idx z_skill_per_annuncio       z_max rottura
#>        <int>                <num>       <num>  <lgcl>
#>  1:        1                   NA          NA   FALSE
#>  2:        2           0.15030222  0.15030222   FALSE
#>  3:        3          -0.72158703  0.72158703   FALSE
#>  4:        4          -0.85690141  0.85690141   FALSE
#>  5:        5          -0.13123350  0.13123350   FALSE
#>  6:        6           0.78824097  0.78824097   FALSE
#>  7:        7           1.05616142  1.05616142   FALSE
#>  8:        8           0.42620301  0.42620301   FALSE
#>  9:        9          -0.52245339  0.52245339   FALSE
#> 10:       10          -0.91761747  0.91761747   FALSE
#> 11:       11          -0.39597719  0.39597719   FALSE
#> 12:       12           0.56287377  0.56287377   FALSE
#> 13:       13          21.93587061 21.93587061    TRUE
#> 14:       14           0.67449076  0.67449076   FALSE
#> 15:       15          -0.27536336  0.27536336   FALSE
#> 16:       16          -0.89889860  0.89889860   FALSE
#> 17:       17          -0.62283952  0.62283952   FALSE
#> 18:       18           0.29900642  0.29900642   FALSE
#> 19:       19           1.01909833  1.01909833   FALSE
#> 20:       20           0.87538701  0.87538701   FALSE
#> 21:       21           0.00000000  0.00000000   FALSE
#> 22:       22          -0.80223593  0.80223593   FALSE
#> 23:       23          -0.79374876  0.79374876   FALSE
#> 24:       24           0.01765844  0.01765844   FALSE
#>     mese_idx z_skill_per_annuncio       z_max rottura
#>        <int>                <num>       <num>  <lgcl>
```
