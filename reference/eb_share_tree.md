# Empirical Bayes shares on a code hierarchy

Shrinks the shares of the codes of a hierarchy towards a prior mean with
a concentration estimated from the data, and returns posterior means and
credible intervals.

## Usage

``` r
eb_share_tree(
  est,
  key_col,
  parent_col,
  level_col,
  type = c("partition", "overlap"),
  prior_mean = NULL,
  deff_col = NULL,
  conf = 0.95,
  draws = 2000L,
  x_col = "x",
  n_col = "n",
  tol = 1e-08
)
```

## Arguments

- est:

  data.frame or data.table with one row per node and columns `key_col`,
  `parent_col`, `level_col`, `x_col`, `n_col`.

- key_col:

  Name of the node identifier column.

- parent_col:

  Name of the parent column (`NA` or `""` for roots).

- level_col:

  Name of the level column; parents must have a lower level than their
  children.

- type:

  `"partition"` (children partition the parent, e.g. professions with an
  "unknown" category) or `"overlap"` (each node has its own count with
  no sum constraint, e.g. postings with at least one skill of a group).

- prior_mean:

  Optional name of a column with the prior mean. For `"partition"` it is
  the conditional share child \| parent (renormalised within each
  family); for `"overlap"` it is the share of the node. See Details for
  the default.

- deff_col:

  Optional name of a design effect column; counts are divided by it
  before the fit.

- conf:

  Credibility level, default 0.95.

- draws:

  Number of posterior draws for `"partition"` (default 2000). Set the
  seed with [`set.seed()`](https://rdrr.io/r/base/Random.html) before
  the call for reproducible intervals; the function does not set it.

- x_col, n_col:

  Names of the count and denominator columns (defaults `"x"`, `"n"`).

- tol:

  Tolerance of the coherence checks (default `1e-8`).

## Value

A copy of `est` with columns `quota_eb` (posterior mean), `lo_eb`,
`hi_eb` (credible interval), `kappa_famiglia` (estimated concentration
of the family the node belongs to; `NA` when not estimable, `Inf` when
the data show no extra dispersion around the prior mean) and
`vincoli_ok` (coherence check on parent nodes, `NA` on leaves).

## Details

**Partition.** A family is the set of children of one parent (the roots
form one family with total `n`). Within a family the children counts
`x_c` must sum to the parent count (to `n` for the roots). Counts are
divided by the family design effect, the `x`-weighted mean of the
children `deff`. The family vector follows a Dirichlet-multinomial with
mean `m` (the prior mean, or the uniform share `1 / K` when `prior_mean`
is `NULL`) and concentration `kappa`, estimated by maximising the
Dirichlet-multinomial likelihood of the family. The joint likelihood is
used instead of separate beta-binomial margins because it is the exact
law of the family vector: the margins share the same `kappa` and are
dependent, so their product would count the same information more than
once. The posterior of the conditional shares is
Dirichlet(`kappa m + x_eff`); families are independent, so the posterior
mean of a marginal share is the product of the conditional posterior
means along the path to the root, and the posterior means of the
children sum to that of the parent exactly. Intervals are quantiles of
`draws` posterior draws (normalised Gamma variables) multiplied along
the path.

**Overlap.** For each family `kappa` is estimated from the beta-binomial
likelihood of the children with their own prior means (see
[`fit_betabinom()`](https://gmontaletti.github.io/skillviz/reference/fit_betabinom.md));
the posterior of each node is Beta(`kappa m + x_eff`,
`kappa (1 - m) + n_eff - x_eff`) and the interval uses its quantiles.
Without `prior_mean` no shrinkage of the mean is applied:
`quota_eb = x / n` and the interval is the Jeffreys interval on the
effective counts; the hierarchy then acts only through the design
effect. `vincoli_ok` checks, on parent nodes,
`max(children) <= parent <= min(1, sum(children))` up to `tol`.

A concentration at the upper bound (`1e8`) is reported as `Inf` and the
posterior collapses on the prior mean; the uncertainty about `kappa` is
not propagated (empirical Bayes).

## References

Mosimann, J. E. (1962). On the compound multinomial distribution, the
multivariate beta-distribution, and correlations among proportions.
*Biometrika*, 49(1/2), 65-82.

Efron, B. and Morris, C. (1975). Data analysis using Stein's estimator
and its generalizations. *Journal of the American Statistical
Association*, 70(350), 311-319.

## Examples

``` r
est <- data.frame(
  code = c("A", "B", "A1", "A2", "B1", "B2"),
  parent = c(NA, NA, "A", "A", "B", "B"),
  level = c(1, 1, 2, 2, 2, 2),
  x = c(600, 400, 590, 10, 200, 200),
  n = 1000
)
set.seed(1)
eb_share_tree(est, "code", "parent", "level", draws = 500)
#>      code parent level     x     n   quota_eb       lo_eb      hi_eb
#>    <char> <char> <num> <num> <num>      <num>       <num>      <num>
#> 1:      A   <NA>     1   600  1000 0.59750376 0.565621973 0.62639415
#> 2:      B   <NA>     1   400  1000 0.40249624 0.373605848 0.43437803
#> 3:     A1      A     2   590  1000 0.58727570 0.555143383 0.61531074
#> 4:     A2      A     2    10  1000 0.01022806 0.005398466 0.01713123
#> 5:     B1      B     2   200  1000 0.20124812 0.186813982 0.21720580
#> 6:     B2      B     2   200  1000 0.20124812 0.186791866 0.21717547
#>    kappa_famiglia vincoli_ok
#>             <num>     <lgcl>
#> 1:     25.6014867       TRUE
#> 2:     25.6014867       TRUE
#> 3:      0.5607706         NA
#> 4:      0.5607706         NA
#> 5:            Inf         NA
#> 6:            Inf         NA
```
