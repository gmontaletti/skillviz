# Deduplicate job announcements

Collapse duplicate rows sharing the same `general_id` into a single row,
keeping modal values for categorical columns via
[`collapse::fmode()`](https://fastverse.org/collapse/reference/fmode.html)
and computing an activity flag based on `xdata`.

## Usage

``` r
deduplicate_annunci(ann, active_date = Sys.Date())
```

## Arguments

- ann:

  A data.table of announcements. Must contain columns `general_id`,
  `idcity`, `idesco_level_4`, and `xdata`.

- active_date:

  An IDate or Date cutoff: announcements with `xdata >= active_date` are
  flagged as active. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

## Value

A data.table with one row per `general_id` and columns: `general_id`,
`N` (original row count), `idcity` (modal), `idesco_level_4` (modal),
`attivo` (1 if any row active, 0 otherwise).
