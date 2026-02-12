# Classify unmapped ESCO L4 codes to CPI groups via Naive Bayes

Uses a Multinomial Naive Bayes classifier to predict CPI 3-digit groups
for ESCO level 4 codes that lack a CP2021 mapping in the postings data.
The crosswalk between ESCO L4 and CPI groups is derived directly from
the postings via majority vote on the `cp2021_id_level_3` column.
Training data comes from postings with a non-missing CP2021 code;
prediction uses the skill profile of unmapped postings.

## Usage

``` r
classify_esco_to_cpi(postings, skills, top_k = 3L, alpha = 1, verbose = TRUE)
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

  Numeric, Laplace smoothing parameter (default: 1.0).

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
