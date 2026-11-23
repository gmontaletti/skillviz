# Calibrate the frequency threshold of the reference professions

Measures how many doubly-coded postings the frequency rule needs to beat
the similarity rule. On the codes with at least `n_large` postings with
both codes, it draws subsamples of `k` of those postings and records how
often the mode of the subsample equals the mode of all of them; the
threshold is the smallest `k` whose agreement exceeds the agreement of
the similarity reference with the full mode on the same codes.

## Usage

``` r
calibrate_reference_threshold(
  postings,
  from_col,
  to_col,
  sim_reference,
  k_grid = c(1L, 2L, 3L, 5L, 10L, 20L, 50L),
  n_large = 200L,
  n_rep = 100L,
  seed = 1L,
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

- sim_reference:

  A `data.frame` with columns `code` and `sim_code` (for example the
  output of
  [`build_reference_professions()`](https://gmontaletti.github.io/skillviz/reference/build_reference_professions.md)):
  the similarity reference of each starting code.

- k_grid:

  Integer vector of subsample sizes. Default
  `c(1, 2, 3, 5, 10, 20, 50)`.

- n_large:

  Minimum number of doubly-coded postings for a code to enter the
  calibration. Default 200.

- n_rep:

  Number of subsamples per code and size. Default 100.

- seed:

  Integer seed of the subsampling. The global RNG state is restored on
  exit.

- from_na, to_na:

  Values of the two code columns that mean "no code".

## Value

A list with:

- curve:

  `data.table` with `k`, `agreement` (mean over codes of the share of
  subsamples whose mode equals the full mode), `agreement_weighted` (the
  same, weighted by doubly-coded postings) and `n_codes`.

- similarity_agreement:

  Share of the calibration codes whose similarity reference equals the
  full mode.

- n_min:

  The smallest `k` of `k_grid` with `agreement` strictly above
  `similarity_agreement`, or `NA` if none.

- codes:

  `data.table` with one row per code with at least `n_large`
  doubly-coded postings: `code`, `n_cocoded`, `code_modal`, `sim_code`,
  `included` (it has a similarity reference) and the per-size agreements
  in columns `k_<k>`.

## Details

Modes are computed as in
[`build_esco_cp_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_esco_cp_crosswalk.md):
ties are resolved on the code (byte order), both for the full mode and
for the subsample modes, so the calibration measures the rule as it is
applied. Subsamples are drawn without replacement. Codes with at least
`n_large` doubly-coded postings but no similarity reference are listed
in `codes` with `included = FALSE` and do not enter either agreement.
The comparison favours neither rule on the codes it uses; it does not
measure the agreement of the two rules on the codes with few postings,
where the threshold is applied.

## Examples

``` r
set.seed(1)
postings <- data.table::data.table(
  from = rep(c("A", "B"), each = 300),
  to = c(sample(c("x", "y"), 300, TRUE, c(0.8, 0.2)),
         sample(c("y", "z"), 300, TRUE, c(0.6, 0.4)))
)
sim <- data.table::data.table(code = c("A", "B"), sim_code = c("x", "z"))
calibrate_reference_threshold(postings, "from", "to", sim, n_rep = 20)
#> $curve
#>        k agreement agreement_weighted n_codes
#>    <int>     <num>              <num>   <int>
#> 1:     1     0.575              0.575       2
#> 2:     2     0.950              0.950       2
#> 3:     3     0.750              0.750       2
#> 4:     5     0.875              0.875       2
#> 5:    10     1.000              1.000       2
#> 6:    20     0.950              0.950       2
#> 7:    50     0.950              0.950       2
#> 
#> $similarity_agreement
#> [1] 0.5
#> 
#> $n_min
#> [1] 1
#> 
#> $codes
#>      code n_cocoded code_modal sim_code included   k_1   k_2   k_3   k_5  k_10
#>    <char>     <int>     <char>   <char>   <lgcl> <num> <num> <num> <num> <num>
#> 1:      A       300          x        x     TRUE  0.70   1.0   1.0  1.00     1
#> 2:      B       300          y        z     TRUE  0.45   0.9   0.5  0.75     1
#>     k_20  k_50
#>    <num> <num>
#> 1:   1.0   1.0
#> 2:   0.9   0.9
#> 
```
