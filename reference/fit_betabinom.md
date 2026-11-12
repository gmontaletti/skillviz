# Maximum likelihood fit of a beta-binomial distribution

Estimates the mean `mu` and the concentration `kappa` of a beta-binomial
distribution from counts `x` out of `n`, with parametrisation
`alpha = mu kappa`, `beta = (1 - mu) kappa`.

## Usage

``` r
fit_betabinom(x, n, mean = NULL)
```

## Arguments

- x:

  Numeric vector of counts (non-integer values, e.g. counts divided by a
  design effect, are allowed).

- n:

  Numeric vector of trials, same length as `x` or length 1. Rows with
  `n = 0` are dropped.

- mean:

  `NULL` (default) to estimate the mean, or a known mean: a single value
  or one value per element of `x`, in (0, 1). With a known mean only
  `kappa` is estimated.

## Value

A list with `mean` (estimated or given mean), `kappa` (concentration;
`Inf` when the data show no overdispersion), `rho` (intra-class
correlation `1 / (1 + kappa)`), `loglik` (maximised log-likelihood,
binomial coefficients included) and `convergence` (0 on success, as in
[`stats::optim()`](https://rdrr.io/r/stats/optim.html)).

## Details

The log-likelihood is
`sum(lchoose(n, x) + lbeta(x + alpha, n - x + beta) - lbeta(alpha, beta))`,
with the binomial coefficient computed through
[`lgamma()`](https://rdrr.io/r/base/Special.html). With an unknown mean
the maximisation runs on `(logit(mu), log(kappa))` with
[`stats::optim()`](https://rdrr.io/r/stats/optim.html) (method
`"L-BFGS-B"`, `kappa` in `[1e-6, 1e8]`); with a known mean it runs on
`log(kappa)` with
[`stats::optimize()`](https://rdrr.io/r/stats/optimize.html).

When the maximum is reached at the upper bound of `kappa` the data are
compatible with a binomial distribution: the function then returns
`kappa = Inf`, `rho = 0`, the binomial log-likelihood and a warning. A
warning and `kappa = Inf` are also returned when all counts are 0 or all
equal `n` and the mean is unknown.

## Examples

``` r
set.seed(1)
p <- stats::rbeta(40, 2, 18)
x <- stats::rbinom(40, 500, p)
fit_betabinom(x, 500)
#> $mean
#> [1] 0.1094461
#> 
#> $kappa
#> [1] 21.47674
#> 
#> $rho
#> [1] 0.04449043
#> 
#> $loglik
#> [1] -192.8856
#> 
#> $convergence
#> [1] 0
#> 
fit_betabinom(x, 500, mean = 0.1)
#> $mean
#> [1] 0.1
#> 
#> $kappa
#> [1] 22.78952
#> 
#> $rho
#> [1] 0.04203532
#> 
#> $loglik
#> [1] -193.3201
#> 
#> $convergence
#> [1] 0
#> 
```
