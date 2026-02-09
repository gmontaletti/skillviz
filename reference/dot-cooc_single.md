# Compute skill co-occurrence for a single profession

Builds a sparse term-document matrix for a given profession, computes
the co-occurrence via
[`Matrix::crossprod()`](https://rdrr.io/pkg/Matrix/man/matmult-methods.html),
and extracts the top edges using an igraph largest-component filter.
This is the workhorse behind
[`cooc_all_professions()`](https://gmontaletti.github.io/skillviz/reference/cooc_all_professions.md).

## Usage

``` r
.cooc_single(
  professione,
  data,
  group_col = "preferredLabel",
  top_n = 30L,
  min_weight = 10L
)
```

## Arguments

- professione:

  Character scalar identifying the profession to filter on.

- data:

  A data.table with at least columns `general_id`, `escoskill_level_3`,
  and the column named by `group_col`.

- group_col:

  Character name of the column used to identify professions. Defaults to
  `"preferredLabel"`.

- top_n:

  Integer maximum number of edges to retain (before largest-component
  filtering). Defaults to `30L`.

- min_weight:

  Integer minimum co-occurrence count. Edges below this threshold are
  dropped. Defaults to `10L`.

## Value

A data.table with columns `from`, `to`, `weight`. Returns an empty
data.table (zero rows, same columns) when the profession has no data, a
single skill, or no edges above `min_weight`.
