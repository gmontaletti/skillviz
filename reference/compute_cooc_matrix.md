# Build a raw co-occurrence sparse matrix

Constructs a symmetric skill co-occurrence matrix from a data.table of
skill assignments. The matrix entry \\(i, j)\\ counts how many
announcements (`general_id`) mention both skill \\i\\ and skill \\j\\.

## Usage

``` r
compute_cooc_matrix(skills)
```

## Arguments

- skills:

  A data.table with columns `general_id` and `escoskill_level_3`.

## Value

A symmetric sparse matrix of class
[Matrix::dgCMatrix](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
with skill names as row/column names.

## Examples

``` r
if (FALSE) { # \dontrun{
skills <- data.table::data.table(
  general_id = c(1, 1, 1, 2, 2),
  escoskill_level_3 = c("A", "B", "C", "A", "B")
)
mat <- compute_cooc_matrix(skills)
} # }
```
