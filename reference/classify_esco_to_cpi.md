# Classify unmapped ESCO L4 codes to CPI groups via Naive Bayes

Uses a Bernoulli Naive Bayes classifier to predict CPI 3-digit groups
for ESCO level 4 codes that lack a CP2021 mapping in the postings data.
The crosswalk between ESCO L4 and CPI groups is derived directly from
the postings via majority vote on the `cp2021_id_level_3` column.
Training data comes from postings with a non-missing CP2021 code;
prediction uses the skill profile of unmapped postings.

## Usage

``` r
classify_esco_to_cpi(
  postings,
  skills,
  top_k = 3L,
  alpha = 1,
  crosswalk = NULL,
  verbose = TRUE
)
```

## Arguments

- postings:

  A data.table from `normalize_ojv()$postings`. Needs `general_id`,
  `idesco_level_4`, `cp2021_id_level_3`, and `cp2021_level_3`.

- skills:

  A data.table from `normalize_ojv()$skills`. Needs `general_id` and
  `escoskill_level_3` (or `ESCOSKILL_LEVEL_3`).

- top_k:

  Integer, number of top CPI predictions per ESCO L4 code (default: 3).

- alpha:

  Positive numeric, Laplace smoothing parameter for the Bernoulli
  presence probabilities (default: 1.0).

- crosswalk:

  Optional data.table with an `idesco_level_4` column representing the
  official ESCO-to-CPI mapping (e.g. from
  [`build_cpi_esco_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_cpi_esco_crosswalk.md)).
  When provided, "unmapped" ESCO L4 codes are those **not** in
  `crosswalk$idesco_level_4`. When NULL (default), the function falls
  back to deriving the mapping from the postings.

- verbose:

  Logical, print progress messages (default: TRUE).

## Value

A data.table keyed on `idesco_level_4` with columns:

- idesco_level_4:

  The unmapped ESCO level 4 code.

- cod_3:

  Predicted CPI 3-digit code.

- nome_3:

  Predicted CPI 3-digit label.

- probability:

  Posterior probability (softmax-normalized).

- rank:

  Rank among top_k predictions (1 = best).

- n_postings:

  Number of postings with this ESCO L4.

- n_skills:

  Number of distinct skills observed for this ESCO L4.

## Details

When `crosswalk` is supplied, "unmapped" means ESCO L4 codes present in
postings but absent from `crosswalk$idesco_level_4` (the official
crosswalk). This typically yields more unmapped codes than the default
behaviour, which considers any code with at least one non-empty
`cp2021_id_level_3` posting as mapped.

Each posting is a binary vector over the `V` distinct skills in
`skills`. For CPI class `c` with `n_c` training postings, of which
`m_cs` carry skill `s`, the presence probability is
`theta_cs = (m_cs + alpha) / (n_c + 2 * alpha)`. The postings of an
unmapped ESCO L4 code are scored as independent draws from a single
class: every posting contributes `log(theta_cs)` for each skill it
carries and `log(1 - theta_cs)` for each skill it lacks, added to the
log prior `log(n_c / n)`. Postings without skills are counted as
postings with every skill absent, both in training and in prediction.

Every class is scored against every unmapped code, including classes
that share no skill with it: a skill never observed in a class is scored
at the smoothed floor `alpha / (n_c + 2 * alpha)`, not dropped. Only
ESCO L4 codes with at least one skill are classified.

## Examples

``` r
postings <- data.table::data.table(
  general_id = 1:6,
  idesco_level_4 = c("E001", "E001", "E002", "E002", "E003", "E003"),
  cp2021_id_level_3 = c("2.1.1", "2.1.1", "3.1.2", "3.1.2", NA, NA),
  cp2021_level_3 = c("Informatici", "Informatici",
                      "Ingegneri", "Ingegneri", NA, NA)
)
skills <- data.table::data.table(
  general_id = c(1L, 1L, 2L, 3L, 3L, 4L, 5L, 5L, 6L),
  escoskill_level_3 = c("S01", "S02", "S01", "S03", "S04", "S03",
                        "S01", "S02", "S01")
)
result <- classify_esco_to_cpi(postings, skills, top_k = 2L)
#> classify_esco_to_cpi: 2 mapped, 1 unmapped, 3 total ESCO L4 codes
#> classify_esco_to_cpi: 2 training classes, 4 training documents
#> classify_esco_to_cpi: vocabulary size = 4
#> classify_esco_to_cpi: classified 1 unmapped ESCO L4 codes
```
