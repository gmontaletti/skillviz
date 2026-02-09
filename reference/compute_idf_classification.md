# Compute IDF classification at the document level

Calculates IDF using unique document counts (not aggregated
frequencies). This variant counts the number of distinct documents
(announcements) that mention each skill, providing a
document-frequency-based diffusion measure.

## Usage

``` r
compute_idf_classification(skills_merged, quantile_trim = 0.025)
```

## Arguments

- skills_merged:

  A `data.table` with columns `general_id` (document ID) and
  `escoskill_level_3` (skill label).

- quantile_trim:

  Numeric, quantile threshold. Skills with IDF in the bottom
  `quantile_trim` fraction are classified as highly diffuse ("alta").
  Default: 0.025.

## Value

A named `list` with:

- idf:

  A `data.table` with columns `escoskill_level_3`, `N`, `tf`, `idf`,
  `diffusione`.

- threshold_rows:

  Integer, number of skills classified as "alta" (highly diffuse).

- diffuse_skills:

  Character vector of skill labels classified as "alta" diffusion.

## Details

The IDF formula is: \\log(1 / (n\_{docs\\with\\skill} /
n\_{docs\\total}))\\ which simplifies to \\log(n\_{docs\\total} /
n\_{docs\\with\\skill})\\.
