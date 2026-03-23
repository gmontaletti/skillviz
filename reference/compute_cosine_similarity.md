# Pairwise cosine similarity between matrix columns

Computes a dense symmetric matrix of cosine similarities between all
pairs of columns in the input matrix. Works with both dense and sparse
matrices.

## Usage

``` r
compute_cosine_similarity(mat)
```

## Arguments

- mat:

  Numeric matrix (dense or sparse). Columns are the entities to compare.

## Value

Dense symmetric matrix of cosine similarities.
