# Compute pairwise distances between professions based on skill profiles

Builds a profession x skill incidence matrix from a data.table of
profession-skill associations. Applies a Balassa-type filter so that
only cells where the observed count exceeds the expected count are
retained (revealed comparative advantage). The transposed matrix is then
passed to [`stats::dist()`](https://rdrr.io/r/stats/dist.html).

## Usage

``` r
compute_profession_distance(competenze, method = "binary", filter_rca = TRUE)
```

## Arguments

- competenze:

  A `data.table` with at least columns `cod_3` (profession code) and
  `escoskill_level_3` (skill label).

- method:

  Character, distance method passed to
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html). Default:
  `"binary"`. Other common options: `"euclidean"`, `"canberra"`,
  `"maximum"`, `"minkowski"`.

- filter_rca:

  Logical, whether to zero out cells where the observed count is below
  the expected count (Balassa filter). Default: `TRUE`.

## Value

A `dist` object with pairwise distances between professions.
