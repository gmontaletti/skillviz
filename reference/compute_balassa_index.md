# Compute the Balassa index (Revealed Comparative Advantage) for skill-profession pairs

Measures the degree of specialization of each skill within each
profession. A Balassa index \>= 1 indicates the skill is more
concentrated in that profession than across all professions (i.e., it is
"specific" to the profession).

## Usage

``` r
compute_balassa_index(skills)
```

## Arguments

- skills:

  A `data.table` with one row per skill occurrence in an announcement.
  Required columns: `preferredLabel` (profession), `escoskill_level_3`
  (skill).

## Value

A `data.table` with columns: `preferredLabel`, `escoskill_level_3`, `N`,
`tutte_professioni`, `tutte_le_skills_della_professione`,
`tutte_le_ricorrenze`, `balassa_index`, and `specializzazione`
("specifica" if \>= 1, "non specifica" otherwise).

## Details

The formula for each profession-skill pair is: \$\$B = (N\_{skill,prof}
/ N\_{prof}) / (N\_{skill,all} / N\_{all})\$\$
