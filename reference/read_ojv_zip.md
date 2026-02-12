# Read OJV data from ZIP files

Reads Online Job Vacancy (OJV) data from a directory of ZIP files
following the ITC4 naming convention. Each ZIP file contains a single
CSV that is read directly via piped `unzip -p` without extracting to
disk.

## Usage

``` r
read_ojv_zip(
  path,
  type = c("postings", "skills", "postings_raw"),
  years = NULL,
  months = NULL,
  select = NULL,
  nrows = Inf,
  verbose = TRUE
)
```

## Arguments

- path:

  Character scalar. Directory containing the ZIP files.

- type:

  Character scalar. File type to read: one of `"postings"`, `"skills"`,
  or `"postings_raw"`. Defaults to `"postings"`.

- years:

  Integer vector of years to include, or `NULL` (default) for all
  available years.

- months:

  Integer vector of months to include, or `NULL` (default) for all
  available months.

- select:

  Character vector of column names to read, passed to
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html)'s `select`
  argument. `NULL` (default) reads all columns.

- nrows:

  Numeric scalar. Maximum number of rows to read per file, passed to
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html)'s `nrows`
  argument. Default `Inf` reads all rows.

- verbose:

  Logical scalar. If `TRUE` (default), prints progress messages to the
  console.

## Value

A `data.table` containing the row-bound contents of all matching ZIP
files. Returns an empty `data.table` if no matching files are found.

## Details

ZIP files are expected to follow the naming pattern
`ITC4_{year}_{month}_{type}.zip` where `type` is one of `"postings"`,
`"skills"`, or `"postings_raw"`.

The function uses
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
with the `cmd` argument to pipe `unzip -p` output directly, avoiding
temporary file extraction. This is efficient for large CSV files
compressed inside ZIP archives.

The three file types correspond to different OJV datasets:

- postings:

  Job posting metadata including location, contract, education, sector,
  salary, and occupation classification columns.

- skills:

  Skill-level data linked to postings via `general_id`, including ESCO
  skill taxonomy fields.

- postings_raw:

  Minimal posting data with company name.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read all postings data
dt <- read_ojv_zip("/path/to/zip/dir", type = "postings")

# Read skills for 2023, months 1-6, selected columns only
sk <- read_ojv_zip(
  "/path/to/zip/dir",
  type = "skills",
  years = 2023L,
  months = 1:6,
  select = c("general_id", "ESCOSKILL_LEVEL_3", "ESCO_V0101_REUSETYPE")
)

# Read first 1000 rows per file for exploration
sample <- read_ojv_zip("/path/to/zip/dir", type = "postings", nrows = 1000)
} # }
```
