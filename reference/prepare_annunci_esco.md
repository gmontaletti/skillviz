# Merge announcements with ESCO mapping and parse dates

Joins the announcements table with the ESCO mapping to add Italian
profession labels, and parses year/month/day columns into proper IDate
fields.

## Usage

``` r
prepare_annunci_esco(ann, esco_mapping)
```

## Arguments

- ann:

  A data.table of announcements with columns `idesco_level_4`,
  `year_grab_date`, `month_grab_date`, `day_grab_date`,
  `year_expire_date`, `month_expire_date`, `day_expire_date`, and
  `general_id`.

- esco_mapping:

  A data.table from
  [`read_esco_mapping()`](https://gmontaletti.github.io/skillviz/reference/read_esco_mapping.md),
  containing at least `idesco_level_4` and `esco_level_4`.

## Value

A data.table with added columns `it_esco_level_4`, `gdate` (grab date as
IDate), and `edate` (expire date as IDate). Deduplicated to unique
combinations of `general_id`, `gdate`, `idesco_level_4`,
`it_esco_level_4`.
