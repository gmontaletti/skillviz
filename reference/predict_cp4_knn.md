# Predict CP2021 level-4 codes via sector-boosted Jaccard k-NN

Assigns CP2021 level-4 profession codes to unlabeled job announcements
using a two-step approach: (1) restrict candidates to CP4 codes observed
in labeled data for the same ESCO level-4 code (de facto crosswalk), (2)
disambiguate via Jaccard k-NN on binary skill vectors, with optional
sector boosting that gives higher weight to same-sector neighbors.

## Usage

``` r
predict_cp4_knn(postings, skills, k = 7L, sector_boost = 3, verbose = TRUE)
```

## Arguments

- postings:

  A data.table with columns: `general_id` (character), `idesco_level_4`
  (integer or character), `cp2021_id_level_4` (character, NA for
  unlabeled rows). Optionally includes `idsector` (character) for sector
  boosting.

- skills:

  A data.table with columns: `general_id` (character),
  `escoskill_level_3` (character).

- k:

  Integer number of nearest neighbors (default 7).

- sector_boost:

  Numeric multiplier for same-sector neighbors in the weighted vote. Set
  to 1.0 to disable sector boosting (default 3.0).

- verbose:

  Logical: print progress messages (default TRUE).

## Value

A data.table with columns:

- general_id:

  Announcement identifier.

- cp2021_id_level_4:

  Predicted CP2021 level-4 code.

- confidence:

  Weighted vote share of the winning class (0–1).

- method:

  One of `"knn"`, `"frequency"`, `"single_candidate"`, `"no_match"`.

## Details

The function splits `postings` into labeled (non-NA `cp2021_id_level_4`)
and unlabeled rows. Labeled data serves as the training set. For each
unlabeled announcement:

1.  The ESCO level-4 code restricts the CP4 candidate space to codes
    observed in labeled data (de facto crosswalk).

2.  If only one candidate exists, assign it directly
    (`method = "single_candidate"`).

3.  Otherwise, compute Jaccard similarity between the announcement's
    binary skill vector and all labeled announcements in the same ESCO
    group. Select the k nearest neighbors and apply a weighted vote,
    where same-sector neighbors receive a `sector_boost` multiplier.

4.  If no skills are available, fall back to the modal CP4 for that ESCO
    code (`method = "frequency"`).

5.  If the ESCO code is not present in labeled data, return NA
    (`method = "no_match"`).

Validated on 2025 OJA data (80/20 stratified split): CP4 accuracy 83.0%
with k=7 and sector_boost=3.0, vs 62.6% frequency baseline.

## See also

[`classify_esco_to_cpi()`](https://gmontaletti.github.io/skillviz/reference/classify_esco_to_cpi.md)
for Naive Bayes classification of ESCO-to-CPI3 mapping.

## Examples

``` r
postings <- data.table::data.table(
  general_id = as.character(1:10),
  idesco_level_4 = rep(c(1000L, 2000L), each = 5),
  cp2021_id_level_4 = c("1.1.1.1", "1.1.1.2", "1.1.1.1", NA, NA,
                         "2.2.2.1", "2.2.2.1", "2.2.2.2", NA, NA),
  idsector = rep(c("C", "F"), each = 5)
)
skills <- data.table::data.table(
  general_id = as.character(c(1,1,2,2,3,3,4,4,5,5,
                               6,6,7,7,8,8,9,9,10,10)),
  escoskill_level_3 = c("s1","s2","s2","s3","s1","s2","s1","s3","s2","s3",
                         "s4","s5","s4","s5","s5","s6","s4","s6","s5","s6")
)
result <- predict_cp4_knn(postings, skills, k = 3L, sector_boost = 1.0)
#> predict_cp4_knn: 6 labeled, 4 unlabeled
#> predict_cp4_knn: 4 predictions (knn=4)
```
