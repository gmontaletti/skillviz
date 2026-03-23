# Jaccard k-NN vote for a single ESCO group

Computes pairwise Jaccard similarity between test and train
announcements using sparse binary skill vectors, selects k nearest
neighbors, and performs a weighted vote with optional sector boosting.

## Usage

``` r
.jaccard_knn_vote(
  test_gids,
  test_skills,
  train_skills,
  skill_levels,
  test_sectors,
  train_sectors,
  freq_cp4,
  k,
  sector_boost
)
```

## Arguments

- test_gids:

  Character vector of test announcement IDs.

- test_skills:

  data.table with `general_id`, `escoskill_level_3`.

- train_skills:

  data.table with `general_id`, `escoskill_level_3`,
  `cp2021_id_level_4`.

- skill_levels:

  Character vector of all skill codes in this group.

- test_sectors:

  Character vector parallel to `test_gids` with sector codes (NA
  allowed).

- train_sectors:

  Character vector parallel to unique train general_ids with sector
  codes (NA allowed). NULL disables sector boosting.

- freq_cp4:

  Character: frequency cascade fallback CP4 code.

- k:

  Integer: number of neighbors.

- sector_boost:

  Numeric: multiplier for same-sector neighbors.

## Value

data.table with columns `general_id`, `cp2021_id_level_4`, `confidence`,
`method`.
