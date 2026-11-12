# Design effect of a share with sources as clusters

Treats the share of a code as a ratio estimator `x / n` summed over
clusters (sources) and computes its linearised between-cluster variance,
the design effect with respect to the binomial variance, the
intra-cluster correlation and the effective number of clusters.

## Usage

``` r
cluster_deff(
  counts,
  key_cols,
  cluster_col = "source",
  x_col = "x",
  n_col = "n",
  cluster_totals = NULL
)
```

## Arguments

- counts:

  data.frame or data.table with one row per key and cluster, holding the
  count `x_col` and (when `cluster_totals` is `NULL`) the cluster
  denominator `n_col`.

- key_cols:

  Character vector of key columns (e.g. the code).

- cluster_col:

  Name of the cluster column (default `"source"`).

- x_col:

  Name of the count column (default `"x"`).

- n_col:

  Name of the denominator column (default `"n"`), in `counts` or in
  `cluster_totals`.

- cluster_totals:

  Optional data.frame with one row per cluster and columns `cluster_col`
  and `n_col`. When given, the denominator of every cluster is taken
  from it and every cluster with a positive denominator enters the
  variance of every key, also when the key has no row (zero count) for
  that cluster. Recommended.

## Value

A data.table with one row per key and columns `key_cols`, `x`, `n`,
`quota`, `v_bin`, `v_cluster`, `deff`, `rho`, `n_cluster`,
`n_cluster_eff`, `df`.

## Details

For a key with cluster counts `x_s` and denominators `n_s`
(`s = 1, ..., S`, clusters with `n_s > 0`), `x = sum(x_s)`,
`n = sum(n_s)` and `p = x / n`. The linearised (Taylor) variance of the
ratio estimator with clusters as primary units (Wolter, 2007) is

`v_cluster = S / (S - 1) * sum_s (x_s - p n_s)^2 / n^2`

and the binomial variance is `v_bin = p (1 - p) / n`. The design effect
(Kish, 1965) is `deff = v_cluster / v_bin` (`NA` when `p` is 0 or 1, or
when `S < 2`). With the weighted mean cluster size `m = sum(n_s^2) / n`,
the intra-cluster correlation implied by `deff = 1 + (m - 1) rho` is
`rho = (deff - 1) / (m - 1)`. The effective number of clusters is
`n_cluster_eff = n^2 / sum(n_s^2)` and `df = n_cluster_eff - 1` is the
suggested number of degrees of freedom for
[`share_interval()`](https://gmontaletti.github.io/skillviz/reference/share_interval.md).

Clusters with a zero count for the key contribute `(p n_s)^2` to the
sum. Without `cluster_totals` only the rows present in `counts` enter
the computation, so `counts` must then be complete (one row per key and
cluster, zeros included).

## References

Kish, L. (1965). *Survey Sampling*. Wiley.

Wolter, K. M. (2007). *Introduction to Variance Estimation* (2nd ed.).
Springer.

## Examples

``` r
counts <- data.frame(
  code = rep(c("a", "b"), each = 4),
  source = rep(c("s1", "s2", "s3", "s4"), 2),
  x = c(10, 40, 5, 25, 3, 2, 4, 1)
)
totals <- data.frame(source = c("s1", "s2", "s3", "s4"),
                     n = c(200, 400, 100, 300))
cluster_deff(counts, key_cols = "code", cluster_totals = totals)
#>      code     x     n quota    v_bin    v_cluster     deff         rho
#>    <char> <num> <num> <num>    <num>        <num>    <num>       <num>
#> 1:      a    80  1000  0.08 7.36e-05 0.0001466667 1.992754 0.003320246
#> 2:      b    10  1000  0.01 9.90e-06 0.0000240000 2.424242 0.004763353
#>    n_cluster n_cluster_eff       df
#>        <int>         <num>    <num>
#> 1:         4      3.333333 2.333333
#> 2:         4      3.333333 2.333333
```
