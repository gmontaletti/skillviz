# Validate transferred skill profiles against observed ones

For the codes that have both a transferred and an observed profile,
measures the cosine between the two and compares it with two baselines
built from the code's own classification: the profile of the parent
group without the code, and the profile of a random code of the same
parent.

## Usage

``` r
validate_profile_transfer(
  transferred,
  observed,
  parent,
  n_postings = attr(observed, "n_postings"),
  seed = 1L
)
```

## Arguments

- transferred:

  Profile matrix from
  [`transfer_skill_profile()`](https://gmontaletti.github.io/skillviz/reference/transfer_skill_profile.md).

- observed:

  Profile matrix of the same codes built from their own postings, from
  [`skill_profiles()`](https://gmontaletti.github.io/skillviz/reference/skill_profiles.md),
  on the same weighting.

- parent:

  A `data.frame` with columns `code` and `parent`: the parent group of
  each code of `observed`.

- n_postings:

  Named numeric vector of the postings behind each row of `observed`,
  used to weight the parent profile. Default: the `n_postings` attribute
  of `observed`; when absent, each code weighs 1.

- seed:

  Integer seed for the choice of the random sibling. The global RNG
  state is restored on exit.

## Value

A list with:

- codes:

  `data.table` with one row per code with both profiles: `code`,
  `parent`, `cosine_transfer`, `cosine_parent` (parent group without the
  code), `sibling_code` and `cosine_sibling`.

- summary:

  `data.table` with one row per measure (`transfer`, `parent`,
  `sibling`): `n` (codes with the measure), `mean` and `median`, and
  `n_common`, `mean_common` on the codes where all three measures exist.

- share_better:

  Shares of the common codes where the transferred profile is closer
  than the parent baseline (`parent`) and than the sibling baseline
  (`sibling`).

## Details

The parent baseline of code \\c\\ in group \\g\\ is \\\sum\_{i \in g, i
\ne c} w_i x_i / \sum\_{i \in g, i \ne c} w_i\\, the profile of the
other codes of the group pooled by their postings; leaving the code out
keeps its own postings from the baseline. Codes alone in their group
have no baseline (`NA`). The baselines are reported for comparison, not
as acceptance thresholds.

## Examples

``` r
observed <- Matrix::Matrix(
  rbind(c(1, 0, 0), c(0.9, 0.1, 0), c(0, 0, 1)),
  sparse = TRUE,
  dimnames = list(c("A1", "A2", "B1"), c("s1", "s2", "s3"))
)
transferred <- observed[c("A1", "A2"), ]
parent <- data.frame(code = c("A1", "A2", "B1"), parent = c("A", "A", "B"))
validate_profile_transfer(transferred, observed, parent)$summary
#>     measure     n      mean    median n_common mean_common
#>      <char> <int>     <num>     <num>    <int>       <num>
#> 1: transfer     2 1.0000000 1.0000000        2   1.0000000
#> 2:   parent     2 0.9938837 0.9938837        2   0.9938837
#> 3:  sibling     2 0.9938837 0.9938837        2   0.9938837
```
