# Normalize OJV data from ZIP files

Reads all three OJV file types (postings, skills, postings_raw) from a
directory of ZIP files, deduplicates postings to one row per
`general_id`, and ensures referential integrity across tables.

## Usage

``` r
normalize_ojv(path, years = NULL, months = NULL, verbose = TRUE)
```

## Arguments

- path:

  Character scalar. Directory containing the ZIP files.

- years:

  Integer vector of years to include, or `NULL` (default) for all
  available years. Passed to
  [`read_ojv_zip`](https://gmontaletti.github.io/skillviz/reference/read_ojv_zip.md).

- months:

  Integer vector of months to include, or `NULL` (default) for all
  available months. Passed to
  [`read_ojv_zip`](https://gmontaletti.github.io/skillviz/reference/read_ojv_zip.md).

- verbose:

  Logical scalar. If `TRUE` (default), prints progress messages to the
  console.

## Value

A named list with three `data.table` elements, all keyed on
`general_id`:

- postings:

  Deduplicated job posting metadata (one row per `general_id`).

- skills:

  Skill-level data, filtered to `general_id` values present in
  `postings`.

- companies:

  Company name data from postings_raw, filtered to `general_id` values
  present in `postings`.

## Details

Deduplication keeps the most recent observation per `general_id`,
determined by `year_grab_date` and `month_grab_date` columns (descending
sort). If these columns are absent, the first occurrence is kept.

## Examples

``` r
if (FALSE) { # \dontrun{
ojv <- normalize_ojv("/path/to/zip/dir")
ojv$postings
ojv$skills
ojv$companies

# Filter to 2024 data only
ojv24 <- normalize_ojv("/path/to/zip/dir", years = 2024L)
} # }
```
