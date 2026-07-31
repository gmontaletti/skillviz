# Read normalized OJA data from the itaposts DuckDB store

Loads Online Job Advertisement (OJA) data from the shared DuckDB store
maintained by the itaposts package and returns it in the same
three-table shape produced by
[`normalize_ojv`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md),
with the skill-side column names lower-cased so they match the names the
rest of skillviz consumes.

## Usage

``` r
read_oja_itaposts(
  con,
  snapshots = NULL,
  region_code = NULL,
  years = NULL,
  months = NULL,
  verbose = TRUE
)
```

## Arguments

- con:

  A connection to the itaposts DuckDB store, as returned by
  [`itaposts::oja_connect()`](https://rdrr.io/pkg/itaposts/man/oja_connect.html).

- snapshots:

  Character vector of `snapshot_id` values to include (e.g.
  `"ITC4_2026_2"`), or `NULL` (default) for all snapshots in the store.

- region_code:

  Character vector of NUTS-2 region codes to include (e.g. `"ITC4"`), or
  `NULL` (default) for all regions.

- years:

  Integer vector of years to include, or `NULL` (default) for all
  available years. Intersected with `snapshots` against the store's
  snapshot metadata.

- months:

  Integer vector of months to include, or `NULL` (default) for all
  available months.

- verbose:

  Logical scalar. If `TRUE` (default), itaposts reports the row count of
  each returned table.

## Value

A named list with three `data.table` elements, all keyed on
`general_id`:

- postings:

  One row per `general_id`, with the CP2021 and ESCO occupation
  hierarchies already joined in (including `idesco_level_4`), the
  geography, contract, education, sector, experience, working-hours,
  salary and source attributes, and `companyname`.

- skills:

  Long-format skill rows (one per posting/skill pair) with lower-cased
  ESCO skill attribute columns.

- companies:

  The `general_id`/`companyname` pair, with missing and empty company
  names dropped.

## Details

This is the recommended way to load OJA data into skillviz.
[`normalize_ojv`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md)
reads the raw vendor ZIP archives directly and duplicates read/dedup
logic that now lives in itaposts; it is retained for backward
compatibility only.

The connection is caller-owned, following the itaposts idiom:


    con <- itaposts::oja_connect()
    on.exit(itaposts::oja_disconnect(con), add = TRUE)

[`itaposts::oja_normalised()`](https://rdrr.io/pkg/itaposts/man/oja_normalised.html)
re-emits the skill columns under their historical Lightcast SHOUTING
names. This function lower-cases them, so `IDESCOSKILL_LEVEL_3`,
`ESCOSKILL_LEVEL_3`, `ESCO_V0101_OBSOLETE`, `ESCO_v0101_DESCRIPTION`,
`ESCO_V0101_URI`, `ESCO_V0101_SKILLSTYPE`, `ESCO_V0101_REUSETYPE`,
`ESCO_V0101_GREEN` and `ESCO_V0101_LANGUAGE` become
`idescoskill_level_3`, `escoskill_level_3`, `esco_v0101_obsolete`,
`esco_v0101_description`, `esco_v0101_uri`, `esco_v0101_skillstype`,
`esco_v0101_reusetype`, `esco_v0101_green` and `esco_v0101_language`.
Only columns actually present are renamed, so the function tolerates
changes to the itaposts shim's column set.

`pillar_softskills` and `esco_v0101_ict` are *not* returned: the vendor
dropped them in the `data_v2` delivery and itaposts has no substitute
for them. Downstream code must treat them as optional.

## See also

[`normalize_ojv`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md)
for the legacy ZIP-based reader.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- itaposts::oja_connect()
on.exit(itaposts::oja_disconnect(con), add = TRUE)

ojv <- read_oja_itaposts(con)
ojv$postings
ojv$skills
ojv$companies

# Lombardy, 2024 only
ojv24 <- read_oja_itaposts(con, region_code = "ITC4", years = 2024L)
} # }
```
