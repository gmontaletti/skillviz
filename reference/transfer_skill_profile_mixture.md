# Transfer skill profiles as a weighted mixture of profiles

Gives each code a weighted mean of the skill profiles of several codes
of the other classification, for example the arrival codes of the
doubly-coded postings of its parent group
([`group_mixture_weights()`](https://gmontaletti.github.io/skillviz/reference/group_mixture_weights.md)).
Only codes with a usable profile enter the mixture, and the weights are
renormalised over them.

## Usage

``` r
transfer_skill_profile_mixture(
  profiles_from,
  weights,
  code_col = "code",
  ref_col = "ref_code",
  weight_col = "weight",
  usable = rownames(profiles_from)
)
```

## Arguments

- profiles_from:

  Profile matrix of the classification the profiles come from (rows
  named by its codes), as returned by
  [`skill_profiles()`](https://gmontaletti.github.io/skillviz/reference/skill_profiles.md).

- weights:

  A `data.frame` with one row per code and contributing code: columns
  `code_col`, `ref_col` and `weight_col` (non-negative).

- code_col, ref_col, weight_col:

  Column names in `weights`. Defaults `"code"`, `"ref_code"`,
  `"weight"`.

- usable:

  Row names of `profiles_from` allowed in the mixture. Default: all
  rows.

## Value

A sparse matrix with one row per code with a positive covered weight
(sorted by code) and the columns of `profiles_from`. Row \\e\\ is
\\\sum\_{c \in U} w\_{ec} x_c / \sum\_{c \in U} w\_{ec}\\, where \\U\\
is the set of usable codes. Attribute `coverage` is a `data.table` with
one row per code of `weights`: `weight_total`, `weight_covered`,
`covered_share` (`weight_covered / weight_total`), `n_refs` and
`n_refs_covered`.

## Examples

``` r
profiles <- Matrix::Matrix(
  rbind(c(1, 0), c(0, 1), c(1, 1)), sparse = TRUE,
  dimnames = list(c("1.1.1.1.0", "1.1.1.2.0", "9.9.9.9.0"), c("a", "b"))
)
weights <- data.frame(
  code = c("1000.1", "1000.1", "1000.1"),
  ref_code = c("1.1.1.1.0", "1.1.1.2.0", "9.9.9.9.0"),
  weight = c(3, 1, 4)
)
m <- transfer_skill_profile_mixture(
  profiles, weights, usable = c("1.1.1.1.0", "1.1.1.2.0")
)
as.matrix(m)
#>           a    b
#> 1000.1 0.75 0.25
attr(m, "coverage")
#>      code weight_total weight_covered covered_share n_refs n_refs_covered
#>    <char>        <num>          <num>         <num>  <int>          <int>
#> 1: 1000.1            8              4           0.5      3              2
```
