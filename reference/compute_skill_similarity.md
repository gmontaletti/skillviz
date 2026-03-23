# Skill-skill cosine similarity based on profession profiles

Computes pairwise cosine similarity between skills using their
profession profile vectors. Skills appearing in fewer than
`min_professions` are excluded before computation.

## Usage

``` r
compute_skill_similarity(mat, min_professions = 3L)
```

## Arguments

- mat:

  Numeric matrix (rows = skills, columns = professions). Can be dense or
  sparse.

- min_professions:

  Integer, minimum number of professions a skill must appear in
  (non-zero entries) to be included. Default: `3L`.

## Value

Dense symmetric matrix of skill-skill cosine similarities.
