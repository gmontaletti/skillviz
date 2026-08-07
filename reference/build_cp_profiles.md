# Build skill profiles per occupation-code pair

Aggregates labelled announcements into one skill distribution per (ESCO
group, CP code) pair, the sufficient statistic a centroid classifier
needs. The result is a small sparse object that can be saved, versioned
and shipped:
[`predict_cp5_centroid()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_centroid.md)
scores new announcements against it without seeing the training data
again.

## Usage

``` r
build_cp_profiles(
  postings,
  skills,
  restrictor = "idesco_level_5",
  target = "cp2021_id_level_5",
  restrictor_na = NULL,
  skill_col = "escoskill_level_3",
  min_support = 1L,
  smooth_alpha = 0.1,
  shrink_beta = 50,
  weighting = "none",
  sector = FALSE,
  verbose = TRUE
)
```

## Arguments

- postings:

  A data.table of labelled announcements. Needs `general_id`, the
  `restrictor` column, the `target` column, and `idsector` when
  `sector = TRUE`. Rows whose target is `NA` or empty are ignored.

- skills:

  A data.table with `general_id` and `skill_col`.

- restrictor:

  Character naming the column that restricts the candidate space
  (default `"idesco_level_5"`).

- target:

  Character naming the CP2021 column to learn (default
  `"cp2021_id_level_5"`).

- restrictor_na:

  Sentinel values of `restrictor` meaning "no occupation code",
  normalised to `NA`. `NULL` (default) resolves to `"Unclassifiable"`
  for a level-5 restrictor and to nothing otherwise.

- skill_col:

  Character naming the skill column (default `"escoskill_level_3"`).

- min_support:

  Integer: candidates supported by fewer than this many labelled
  announcements are dropped from their group (default 1, keep all).

- smooth_alpha:

  Numeric additive (Lidstone) smoothing applied to the skill counts
  before normalising (default 0.1).

- shrink_beta:

  Numeric shrinkage of each class profile toward its ESCO group's
  profile: `theta_c = (n_c * theta_hat_c + beta * pi_e) / (n_c + beta)`
  (default 50). `0` disables it; large values collapse every candidate
  onto the group profile, which is the identity the harness tests.

- weighting:

  Character vector of the diagonal skill weightings to precompute, any
  of `"none"`, `"idf"`, `"balassa"`. Only the ones a distance actually
  needs are used at prediction time.

- sector:

  Logical: also learn a smoothed sector distribution per candidate,
  scored as a naive-Bayes term (default FALSE). Requires `idsector` in
  `postings`.

- verbose:

  Logical: print a one-line summary (default TRUE).

## Value

A list of class `cp_profiles` carrying the pair index, the sparse skill
counts per pair and per group, the class sizes, the skill vocabulary,
the optional sector counts, the requested weight vectors, and the
smoothing parameters. Its size is governed by the number of (group,
code) pairs, not by the number of announcements.

## Details

Profiles are stored as **counts**, not as the smoothed distributions.
The smoothing and the shrinkage are applied by
[`predict_cp5_centroid()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_centroid.md)
when it materialises a group's block, so one build serves many scoring
parameters and the stored object stays sparse.

`shrink_beta` interpolates between the class profile and its group's: a
class with few announcements is pulled toward the group, one with many
is left alone. This is the axis the 2026-03 attempt lacked, and it is
also the axis that makes the model falsifiable — see
[`predict_cp5_centroid()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_centroid.md).

## See also

[`predict_cp5_centroid()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_centroid.md),
and
[`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md)
for the neighbour-search model this one is measured against.

## Examples

``` r
postings <- data.table::data.table(
  general_id = as.character(1:6),
  idesco_level_5 = rep(c("1000.1", "2000.1"), each = 3),
  cp2021_id_level_5 = c(
    "1.1.1.1.1", "1.1.1.1.1", "1.1.1.1.2",
    "2.2.2.2.0", "2.2.2.2.0", "2.2.2.2.0"
  )
)
skills <- data.table::data.table(
  general_id = as.character(rep(1:6, each = 2)),
  escoskill_level_3 = c(
    "s1", "s2", "s1", "s2", "s1", "s3",
    "s4", "s5", "s4", "s5", "s4", "s6"
  )
)
p <- build_cp_profiles(postings, skills, verbose = FALSE)
p$pairs
#>       grp      code pair_id grp_id   n_c
#>    <char>    <char>   <int>  <int> <int>
#> 1: 1000.1 1.1.1.1.1       1      1     2
#> 2: 1000.1 1.1.1.1.2       2      1     1
#> 3: 2000.1 2.2.2.2.0       3      2     3
```
