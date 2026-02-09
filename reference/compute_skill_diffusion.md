# Compute skill diffusion using TF-IDF on aggregated skill frequencies

Calculates Term Frequency (TF) and Inverse Document Frequency (IDF) for
each skill based on aggregated occurrence counts. Skills with very low
IDF are widespread ("alta" diffusion); skills with very high IDF are
rare ("minima").

## Usage

``` r
compute_skill_diffusion(skills_by_profession, quantile_trim = 0.025)
```

## Arguments

- skills_by_profession:

  A `data.table` with columns `escoskill_level_3` and `N` (occurrence
  count), typically obtained by aggregating skills merged with
  announcements. Each row is a skill-profession pair with its count.

- quantile_trim:

  Numeric, quantile threshold for classifying diffusion. Skills with IDF
  \<= quantile(quantile_trim) are "alta" (highly diffuse), skills with
  IDF \>= quantile(1 - quantile_trim) are "minima" (rare). Default:
  0.025.

## Value

A `data.table` with columns: `escoskill_level_3`, `N`, `tf`, `idf`,
`diffusione`.

## Details

The IDF formula is: \\log(N\_{total} / N\_{skill})\\ where counts come
from the aggregated profession-skill matrix.
