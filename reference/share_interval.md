# Confidence interval for a share with a design effect

Computes Wilson, Jeffreys or logit intervals for the share `x / n` using
the effective sample size `n / deff`, so that the extra variance due to
clustering (for example of postings by source) widens the interval.

## Usage

``` r
share_interval(
  x,
  n,
  method = c("wilson", "jeffreys", "logit"),
  deff = 1,
  df = Inf,
  conf = 0.95
)
```

## Arguments

- x:

  Numeric vector of counts (postings with the code).

- n:

  Numeric vector of denominators, same length as `x` or length 1.

- method:

  Interval method: `"wilson"` (default), `"jeffreys"` or `"logit"`.

- deff:

  Design effect, length 1 or `length(x)`. Must be positive; `NA` gives
  `NA` bounds.

- df:

  Degrees of freedom of the t quantile, length 1 or `length(x)`. `Inf`
  (default) uses the normal quantile. Must be positive; `NA` gives `NA`
  bounds. Not used by `"jeffreys"`.

- conf:

  Confidence level, default 0.95.

## Value

A data.table with one row per element of `x` and columns `quota`
(`x / n`), `lo`, `hi`, `n_eff` (`n / deff`) and `metodo`.

## Details

With effective counts `n_eff = n / deff`, `x_eff = x / deff`, share
`p = x / n` and quantile `z = qt(1 - (1 - conf) / 2, df)` (equal to the
normal quantile when `df = Inf`):

- **Wilson**:
  `(p + z^2 / (2 n_eff)) / (1 + z^2 / n_eff) +/- z / (1 + z^2 / n_eff) * sqrt(p (1 - p) / n_eff + z^2 / (4 n_eff^2))`.

- **Jeffreys**: quantiles `(1 - conf) / 2` and `1 - (1 - conf) / 2` of
  Beta(`x_eff + 0.5`, `n_eff - x_eff + 0.5`); the lower bound is 0 when
  `x = 0` and the upper bound is 1 when `x = n` (Brown, Cai and
  DasGupta, 2001).

- **Logit**: `plogis(logit(p) +/- z * sqrt(deff / (n p (1 - p))))`; when
  `x = 0` or `x = n` the Haldane correction replaces `p` with
  `(x + 0.5) / (n + 1)` and `n` with `n + 1`.

The design effect is the ratio between the variance of the estimator
under the actual design and its binomial variance (Kish, 1965); see
[`cluster_deff()`](https://gmontaletti.github.io/skillviz/reference/cluster_deff.md)
for its estimation with sources as clusters and
[`smooth_deff()`](https://gmontaletti.github.io/skillviz/reference/smooth_deff.md)
for its stabilisation along a code hierarchy.

## References

Kish, L. (1965). *Survey Sampling*. Wiley.

Brown, L. D., Cai, T. T. and DasGupta, A. (2001). Interval estimation
for a binomial proportion. *Statistical Science*, 16(2), 101-133.

## Examples

``` r
share_interval(x = c(0, 12, 300), n = 1000)
#>    quota           lo          hi n_eff metodo
#>    <num>        <num>       <num> <num> <char>
#> 1: 0.000 2.168404e-19 0.003826758  1000 wilson
#> 2: 0.012 6.877648e-03 0.020857268  1000 wilson
#> 3: 0.300 2.724068e-01 0.329123861  1000 wilson
share_interval(x = c(0, 12, 300), n = 1000, deff = 4, df = 20)
#>    quota          lo         hi n_eff metodo
#>    <num>       <num>      <num> <num> <char>
#> 1: 0.000 0.000000000 0.01710722   250 wilson
#> 2: 0.012 0.003840213 0.03685644   250 wilson
#> 3: 0.300 0.243386262 0.36345663   250 wilson
share_interval(x = 12, n = 1000, method = "logit", deff = 2)
#>    quota          lo         hi n_eff metodo
#>    <num>       <num>      <num> <num> <char>
#> 1: 0.012 0.005400909 0.02644777   500  logit
```
