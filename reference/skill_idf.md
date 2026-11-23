# Inverse document frequency of skills over postings

Computes \\idf_s = \log(N / n_s)\\, where \\N\\ is the number of
postings with at least one skill and \\n_s\\ the number of those
postings that mention skill \\s\\: the same document-frequency scheme as
[`compute_idf_classification()`](https://gmontaletti.github.io/skillviz/reference/compute_idf_classification.md),
returned as a named vector so that profiles of two classifications can
share one weighting.

## Usage

``` r
skill_idf(
  skills,
  id_col = "general_id",
  skill_col = "idescoskill_level_3",
  ids = NULL
)
```

## Arguments

- skills:

  A `data.frame` with one row per posting and skill (duplicates are
  ignored), with columns `id_col` and `skill_col`.

- id_col:

  Name of the posting id column. Default `"general_id"`.

- skill_col:

  Name of the skill column. Default `"idescoskill_level_3"`.

- ids:

  Optional vector of posting ids: when supplied, only these postings
  enter \\N\\ and \\n_s\\.

## Value

A named numeric vector, one element per skill, named by the skill and
sorted by name. Attribute `n_postings` holds \\N\\.

## Examples

``` r
skills <- data.table::data.table(
  general_id = c(1, 1, 2, 3),
  idescoskill_level_3 = c("a", "b", "a", "c")
)
skill_idf(skills)
#>         a         b         c 
#> 0.4054651 1.0986123 1.0986123 
#> attr(,"n_postings")
#> [1] 3
```
