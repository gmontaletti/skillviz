# Filter announcements to stable sources

Subset a data.table of announcements keeping only rows whose `source`
column matches one of the provided stable source names.

## Usage

``` r
filter_annunci_by_source(ann, stable_sources)
```

## Arguments

- ann:

  A data.table of announcements with a `source` column.

- stable_sources:

  Character vector of source names to keep, typically returned by
  [`filter_stable_sources()`](https://gmontaletti.github.io/skillviz/reference/filter_stable_sources.md).

## Value

A data.table containing only rows with `source` in `stable_sources`.
