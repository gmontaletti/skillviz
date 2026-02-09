# Prepare announcements with CPI geographic dimension

Merges announcements with ESCO labels and a territorial mapping table to
add the CPI (Centro per l'Impiego) field, then aggregates unique
announcement counts by CPI, profession, and year.

## Usage

``` r
prepare_annunci_geography(ann, esco_mapping, territoriale, regione = 10L)
```

## Arguments

- ann:

  A data.table of announcements with columns `idesco_level_4`, `idcity`,
  `general_id`, and year/month/day grab and expire date columns.

- esco_mapping:

  A data.table from
  [`read_esco_mapping()`](https://gmontaletti.github.io/skillviz/reference/read_esco_mapping.md),
  containing at least `idesco_level_4` and `esco_level_4`.

- territoriale:

  A data.table with territorial classification. Must contain
  `COD_ISTAT`, `CPI`, and `COD_REGIONE_PAUT`.

- regione:

  Integer region code to filter the territorial table. Defaults to
  `10L`.

## Value

A data.table with columns `CPI`, `it_esco_level_4`, `year_grab_date`,
and `N` (unique announcement count), filtered to complete cases and
ordered by CPI, year, descending N.
