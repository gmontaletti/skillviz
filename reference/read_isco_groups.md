# Read ESCO ISCO classification from CSV directory

Reads the ISCOGroups CSV file from an ESCO classification dataset
directory and returns it as a data.table.

## Usage

``` r
read_isco_groups(esco_dir)
```

## Arguments

- esco_dir:

  Character path to the ESCO dataset directory (e.g.
  `"ESCO dataset - v1.1.1 - classification - it - csv"`). The function
  looks for a file matching `ISCOGroups*.csv` inside this directory.

## Value

A data.table with ISCO group classification columns including `code` and
`preferredLabel`.
