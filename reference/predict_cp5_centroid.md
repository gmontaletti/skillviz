# Predict CP2021 codes against static skill profiles

Scores each announcement against the profiles of the CP codes observed
for its ESCO group, and returns the best one. No training data is
consulted: the whole model is the object returned by
[`build_cp_profiles()`](https://gmontaletti.github.io/skillviz/reference/build_cp_profiles.md).

## Usage

``` r
predict_cp5_centroid(
  profiles,
  postings,
  skills,
  distance = "cosine",
  prior_weight = 1,
  dense_budget = 2e+08,
  explain = FALSE,
  verbose = TRUE
)
```

## Arguments

- profiles:

  A `cp_profiles` object from
  [`build_cp_profiles()`](https://gmontaletti.github.io/skillviz/reference/build_cp_profiles.md).

- postings:

  A data.table of announcements to code. Needs `general_id` and the
  restrictor column the profiles were built on, plus `idsector` when the
  profiles carry a sector term. Every row gets exactly one prediction.

- skills:

  A data.table with `general_id` and the profiles' skill column.

- distance:

  Character naming the scoring rule, one of `"multinomial_nb"`,
  `"complement_nb"`, `"bernoulli_nb"`, `"hellinger"`, `"cosine"`,
  `"l2"`, `"l1"`, `"chisq"`, `"jsd"`, `"dice"`, `"tfidf_cosine"`,
  `"balassa_cosine"`.

- prior_weight:

  Numeric multiplier on `log n_c`, the class-size prior (default 1). `0`
  removes the prior; large values reduce the rule to picking the largest
  class, which is the modal cascade.

- dense_budget:

  Numeric cap on the elements of a dense query block, used to chunk
  large groups (default 2e8).

- explain:

  Logical: also return the per-candidate score matrix for each
  announcement (default FALSE). Costly; intended for diagnosis.

- verbose:

  Logical: print a one-line summary (default TRUE).

## Value

A data.table with `general_id`, the profiles' target column,
`confidence` and `method`. `method` is `"centroid"` where the model
scored, `"single_candidate"` where the group offered only one code,
`"frequency"` where the announcement carries no known skill and the
class prior decided alone, and `"no_match"` where the group is absent
from the profiles. When `explain = TRUE` the result carries a `scores`
attribute.

## Details

The rule is \$\$score(q, c) = \lambda \log n_c + s(q, \theta_c) +
\mathrm{sector}\$\$ with \\s\\ the similarity implied by `distance` and
\\\theta_c\\ the smoothed, group-shrunk profile of candidate \\c\\.

**The shrinkage makes the model falsifiable.** As `shrink_beta` grows,
every \\\theta_c\\ in a group collapses onto that group's profile, the
similarity term becomes constant across candidates, and the ranking
reduces to \\\lambda \log n_c\\ — which is exactly the modal cascade. So
a correct implementation must reproduce the modal baseline in that
limit, and `skillviz_workflow/run_cp5_centroid.R` refuses to report any
accuracy until it has checked that it does. Ties are broken on the
smaller CP code, matching the cascade.

Announcements carrying no skill present in the profiles get
`"frequency"` rather than `"centroid"`: the prior decided them, not the
skill vector. That matches
[`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md)'s
convention, which keeps the two models' model-decided populations
comparable.

## See also

[`build_cp_profiles()`](https://gmontaletti.github.io/skillviz/reference/build_cp_profiles.md),
[`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md)

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
predict_cp5_centroid(p, postings, skills, verbose = FALSE)
#>    general_id cp2021_id_level_5 confidence           method
#>        <char>            <char>      <num>           <char>
#> 1:          1         1.1.1.1.1  0.6685342         centroid
#> 2:          2         1.1.1.1.1  0.6685342         centroid
#> 3:          3         1.1.1.1.1  0.6630519         centroid
#> 4:          4         2.2.2.2.0  1.0000000 single_candidate
#> 5:          5         2.2.2.2.0  1.0000000 single_candidate
#> 6:          6         2.2.2.2.0  1.0000000 single_candidate
```
