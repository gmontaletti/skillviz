# Read CPI-ESCO mapping table

Read the CPI-to-ESCO level 4 mapping from either a DBI database
connection or an fst file on disk.

## Usage

``` r
read_esco_mapping(conn = NULL, file = NULL)
```

## Arguments

- conn:

  A DBI connection object. If provided, reads the `mappa_cpv_esco_iv`
  table from the database.

- file:

  Character path to an fst file containing the mapping. Used when `conn`
  is NULL.

## Value

A data.table with columns from the mapping table, including at least
`idesco_level_4`, `esco_level_4`, and `idcp_2011_v`.
