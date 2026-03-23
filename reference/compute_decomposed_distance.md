# Squared Euclidean distance decomposed by skill subsets

Computes the total squared Euclidean distance between profession pairs
and decomposes it by skill groups. For any disjoint partition \\\\A_1,
\ldots, A_K\\\\, the additive property holds: \\d^2(i, j) = \sum_k
d^2_k(i, j)\\.

## Usage

``` r
compute_decomposed_distance(mat, skill_groups)
```

## Arguments

- mat:

  Balassa-weighted matrix (rows = skills, columns = professions). Can be
  dense or sparse.

- skill_groups:

  `data.table` with columns `escoskill_level_3` and `group`, mapping
  each skill to its group.

## Value

A list with three elements:

- `total`: a `dist` object of total squared Euclidean distances.

- `partial`: a named list of `dist` objects, one per group.

- `contribution`: a `data.table` with columns `prof_i`, `prof_j`,
  `group`, `partial_d2`, `total_d2`, and `share`.
