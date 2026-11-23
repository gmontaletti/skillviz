# Transfer skill profiles through reference professions

Gives each code the skill profile of its reference profession in the
other classification.

## Usage

``` r
transfer_skill_profile(
  profiles_from,
  reference,
  code_col = "code",
  ref_col = "ref_code"
)
```

## Arguments

- profiles_from:

  Profile matrix of the classification the profiles come from (rows
  named by its codes), as returned by
  [`skill_profiles()`](https://gmontaletti.github.io/skillviz/reference/skill_profiles.md).

- reference:

  A `data.frame` with the arrival codes and their references (for
  example the output of
  [`build_reference_professions()`](https://gmontaletti.github.io/skillviz/reference/build_reference_professions.md)),
  where the reference codes are codes of `profiles_from`.

- code_col, ref_col:

  Column names of the arrival code and of the reference code in
  `reference`. Default `"code"` and `"ref_code"`.

## Value

A sparse matrix with one row per code of `reference` whose reference has
a profile in `profiles_from` (sorted by code), the columns of
`profiles_from`, and attribute `ref_code` (named character vector: the
reference of each row).

## Examples

``` r
profiles <- Matrix::Matrix(
  c(0.5, 0, 0.2, 1), nrow = 2, sparse = TRUE,
  dimnames = list(c("1000.1", "2000.1"), c("a", "b"))
)
reference <- data.frame(
  code = c("1.1.1.1.0", "2.2.2.2.0", "3.3.3.3.0"),
  ref_code = c("1000.1", "2000.1", NA)
)
as.matrix(transfer_skill_profile(profiles, reference))
#>             a   b
#> 1.1.1.1.0 0.5 0.2
#> 2.2.2.2.0 0.0 1.0
```
