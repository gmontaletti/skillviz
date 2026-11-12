# Cluster bootstrap interval for shares

Resamples clusters (sources) with replacement and returns percentile
intervals and bootstrap standard errors of the share of every key.

## Usage

``` r
cluster_boot_share(
  counts,
  key_cols,
  cluster_col = "source",
  B = 1000L,
  conf = 0.95,
  cluster_totals = NULL,
  x_col = "x",
  n_col = "n"
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

- B:

  Number of bootstrap replicates (default 1000).

- conf:

  Confidence level, default 0.95.

- cluster_totals:

  Optional data.frame with one row per cluster and columns `cluster_col`
  and `n_col`. When given, the denominator of every cluster is taken
  from it and every cluster with a positive denominator enters the
  variance of every key, also when the key has no row (zero count) for
  that cluster. Recommended.

- x_col:

  Name of the count column (default `"x"`).

- n_col:

  Name of the denominator column (default `"n"`), in `counts` or in
  `cluster_totals`.

## Value

A data.table with one row per key and columns `key_cols`, `quota`,
`lo_boot`, `hi_boot`, `se_boot`.

## Details

Let `X` be the sparse key x cluster count matrix and `n_s` the cluster
denominators. The multiplicities of the `S` clusters in `B` replicates
form the matrix `M` (`S x B`, columns drawn from a multinomial with `S`
trials and equal probabilities); the replicate counts are `X M` and the
replicate denominators `n_s M`, so all replicates are computed with two
matrix products (processed in blocks of keys to bound memory). The
interval is the percentile interval of the replicate shares; replicates
with a zero denominator are dropped. Set the seed with
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the call; the
function does not set it. The bootstrap of the clusters is the
resampling counterpart of the linearised variance of
[`cluster_deff()`](https://gmontaletti.github.io/skillviz/reference/cluster_deff.md)
and shares its assumptions: clusters are treated as a sample of sources.

## References

Davison, A. C. and Hinkley, D. V. (1997). *Bootstrap Methods and their
Application*. Cambridge University Press, section 3.8.

## Examples

``` r
counts <- data.frame(
  code = rep(c("a", "b"), each = 4),
  source = rep(c("s1", "s2", "s3", "s4"), 2),
  x = c(10, 40, 5, 25, 3, 2, 4, 1)
)
totals <- data.frame(source = c("s1", "s2", "s3", "s4"),
                     n = c(200, 400, 100, 300))
set.seed(1)
cluster_boot_share(counts, "code", B = 200, cluster_totals = totals)
#>      code quota     lo_boot    hi_boot     se_boot
#>    <char> <num>       <num>      <num>       <num>
#> 1:      a  0.08 0.050000000 0.09616667 0.011088630
#> 2:      b  0.01 0.004285714 0.02333333 0.005436773
```
