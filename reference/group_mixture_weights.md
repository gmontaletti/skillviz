# Mixture weights from the doubly-coded postings of a parent group

For every code of the starting classification, the distribution of the
arrival codes among the doubly-coded postings of the code's whole parent
group, optionally leaving out the code's own postings. The weights feed
[`transfer_skill_profile_mixture()`](https://gmontaletti.github.io/skillviz/reference/transfer_skill_profile_mixture.md).

## Usage

``` r
group_mixture_weights(
  postings,
  from_col,
  to_col,
  group,
  leave_one_out = FALSE,
  from_na = c("", "Unclassifiable"),
  to_na = c("", "Unclassifiable")
)
```

## Arguments

- postings:

  A `data.frame` with one row per posting and the columns `from_col` and
  `to_col`.

- from_col, to_col:

  Names of the starting and arrival code columns.

- group:

  A `data.frame` with columns `code` and `parent`: the parent group of
  each starting code (for example the universe of
  [`build_reference_professions()`](https://gmontaletti.github.io/skillviz/reference/build_reference_professions.md)).
  Every code of `group` gets the weights of its parent; postings whose
  starting code is not in `group` are ignored.

- leave_one_out:

  If `TRUE`, the doubly-coded postings of the code itself are removed
  from its weights. Default `FALSE`.

- from_na, to_na:

  Values of the two code columns that mean "no code".

## Value

A `data.table` with `code`, `parent`, `ref_code` (arrival code) and
`weight` (number of doubly-coded postings of the group with that arrival
code, minus the code's own with `leave_one_out = TRUE`), one row per
code and arrival code with positive weight, sorted by code and
`ref_code`. Codes whose group has no doubly-coded postings (or, leaving
one out, none besides their own) have no row.

## Examples

``` r
postings <- data.table::data.table(
  esco = c("1000.1", "1000.1", "1000.2", "2000.1"),
  cp = c("1.1.1.1.0", "1.1.1.1.0", "1.1.1.2.0", "")
)
group <- data.table::data.table(
  code = c("1000.1", "1000.2", "2000.1"),
  parent = c("1000", "1000", "2000")
)
group_mixture_weights(postings, "esco", "cp", group)
#>      code parent  ref_code weight
#>    <char> <char>    <char>  <num>
#> 1: 1000.1   1000 1.1.1.1.0      2
#> 2: 1000.1   1000 1.1.1.2.0      1
#> 3: 1000.2   1000 1.1.1.1.0      2
#> 4: 1000.2   1000 1.1.1.2.0      1
group_mixture_weights(postings, "esco", "cp", group, leave_one_out = TRUE)
#> Key: <code, ref_code>
#>      code parent  ref_code weight
#>    <char> <char>    <char>  <num>
#> 1: 1000.1   1000 1.1.1.2.0      1
#> 2: 1000.2   1000 1.1.1.1.0      2
```
