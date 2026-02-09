# Compute skill co-occurrence networks for multiple professions

Iterates over a set of professions and calls the internal
[`.cooc_single()`](https://gmontaletti.github.io/skillviz/reference/dot-cooc_single.md)
for each one, binding the results into a single data.table with a
`professione` identifier column.

## Usage

``` r
cooc_all_professions(
  skills,
  professions = NULL,
  group_col = "preferredLabel",
  top_n = 30L,
  min_weight = 10L
)
```

## Arguments

- skills:

  A data.table with at least columns `general_id`, `escoskill_level_3`,
  and the column named by `group_col`.

- professions:

  Optional character vector of profession names to process. When `NULL`
  (the default), all unique values of `group_col` are used.

- group_col:

  Character name of the column used to identify professions. Defaults to
  `"preferredLabel"`.

- top_n:

  Integer maximum number of edges per profession (before
  largest-component filtering). Defaults to `30L`.

- min_weight:

  Integer minimum co-occurrence count. Defaults to `10L`.

## Value

A data.table with columns `professione`, `from`, `to`, `weight`.
Professions producing no valid edges are silently dropped.

## Examples

``` r
if (FALSE) { # \dontrun{
skills <- data.table::data.table(
  general_id = c(1, 1, 1, 2, 2, 3, 3, 3),
  escoskill_level_3 = c("A", "B", "C", "A", "B", "A", "B", "C"),
  preferredLabel = rep("Analyst", 8)
)
cooc_all_professions(skills)
} # }
```
