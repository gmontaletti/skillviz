# Extract non-diffuse skills at the CPI3 profession level

Merges job announcements with skills through a CPI-ESCO crosswalk at the
CPI 3-digit level. Skills classified as highly diffuse (based on IDF)
are removed, retaining only profession-specific skills.

## Usage

``` r
extract_specific_skills_cpi3(ann, ski, cpi_esco, quantile_trim = 0.025)
```

## Arguments

- ann:

  A `data.table` of announcements with at least columns `general_id` and
  `idesco_level_4`.

- ski:

  A `data.table` of skills with at least columns `general_id` and
  `escoskill_level_3`.

- cpi_esco:

  A `data.table` crosswalk mapping `idesco_level_4` to `cod_3` and
  `nome_3`. Typically built by the crosswalk pipeline.

- quantile_trim:

  Numeric, quantile threshold for IDF diffusion classification. Default:
  0.025.

## Value

A `data.table` with columns `general_id`, `escoskill_level_3`, `cod_3`,
`nome_3`. Only non-diffuse (specific) skills are included.

## Details

The pipeline reproduces the logic from the extraction script:

1.  Merge announcements with the crosswalk on `idesco_level_4`.

2.  Merge with skills on `general_id`.

3.  Compute document-level IDF, classify diffuse skills.

4.  Filter to non-diffuse ("specifiche") skills.
