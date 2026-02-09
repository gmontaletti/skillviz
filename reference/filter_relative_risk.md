# Filter a co-occurrence matrix by relative risk

Given a raw co-occurrence matrix (e.g. from
[`compute_cooc_matrix()`](https://gmontaletti.github.io/skillviz/reference/compute_cooc_matrix.md)),
computes expected frequencies under independence and retains only edges
where observed / expected \> 1. The diagonal and upper triangle are
zeroed so each pair appears at most once.

## Usage

``` r
filter_relative_risk(cooc_matrix)
```

## Arguments

- cooc_matrix:

  A symmetric numeric matrix (dense or sparse) of co-occurrence counts,
  as returned by
  [`compute_cooc_matrix()`](https://gmontaletti.github.io/skillviz/reference/compute_cooc_matrix.md).

## Value

A data.table with columns `from`, `to`, `weight` containing only edges
whose observed co-occurrence exceeds the independence baseline. Edges
are sorted by descending `weight`.

## Examples

``` r
if (FALSE) { # \dontrun{
mat <- compute_cooc_matrix(skills)
rr_edges <- filter_relative_risk(mat)
} # }
```
