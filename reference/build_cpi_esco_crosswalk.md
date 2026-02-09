# Build ESCO-to-CP2021 level 3 crosswalk

Merge the CPI-ESCO mapping with the CP2021 level 3 classification to
produce a lookup table that maps each `idesco_level_4` code to its
Italian ESCO label and CP2021 3-digit group.

## Usage

``` r
build_cpi_esco_crosswalk(esco_mapping, cpi3)
```

## Arguments

- esco_mapping:

  A data.table from
  [`read_esco_mapping()`](https://gmontaletti.github.io/skillviz/reference/read_esco_mapping.md),
  containing at least `idesco_level_4`, `esco_level_4`, and
  `idcp_2011_v`.

- cpi3:

  A data.table with CP2021 level 3 classification. Must contain `cod_3`
  and `nome_3`.

## Value

A data.table with one row per `idesco_level_4` and columns:
`idesco_level_4`, `it_esco_level_4` (collapsed Italian ESCO labels),
`cod_3`, `nome_3`.
