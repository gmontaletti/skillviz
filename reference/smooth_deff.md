# Smooth design effects along a code hierarchy

Stabilises the noisy design effects of small codes with a generalised
variance function (GVF) by level and with a normal-normal shrinkage of
`log(deff)` towards the parent node.

## Usage

``` r
smooth_deff(
  est,
  key_col,
  parent_col,
  level_col,
  method = c("gvf_parent", "gvf", "parent"),
  deff_col = "deff",
  quota_col = "quota",
  df_col = "df"
)
```

## Arguments

- est:

  data.frame or data.table with one row per node, e.g. the output of
  [`cluster_deff()`](https://gmontaletti.github.io/skillviz/reference/cluster_deff.md)
  with the hierarchy columns added.

- key_col:

  Name of the node identifier column.

- parent_col:

  Name of the parent identifier column (`NA` or `""` for roots; parents
  absent from `est` are treated as roots).

- level_col:

  Name of the level column; parents must have a lower level than their
  children.

- method:

  `"gvf_parent"` (default), `"gvf"` or `"parent"`.

- deff_col, quota_col, df_col:

  Names of the design effect, share and degrees of freedom columns
  (defaults `"deff"`, `"quota"`, `"df"`).

## Value

A copy of `est` with columns `deff_liscio` (smoothed design effect,
truncated at 1) and `peso_contrazione` (weight of the prior mean, from 0
= direct estimate to 1 = prior mean). The attributes `gvf` (intercept,
slope and number of fitted nodes by level) and `tau2` (prior variance by
level) document the fit.

## Details

**GVF.** For every level the weighted least squares regression
`log(deff) = a + b log(quota)`, weights `df`, is fitted on the nodes
with finite positive `deff`, `quota` in (0, 1) and `df > 0`; the
prediction `g` is the GVF value (quotas outside the fitted range are
clamped to it). With fewer than three nodes or no variation in `quota`
the slope is set to 0, and with no usable node `g = 0`.

**Shrinkage.** Levels are processed from the top. For node `k`, the
direct value `y_k = log(deff_k)` has sampling variance approximately
`s_k^2 = 2 / df_k` (variance of the log of a scaled chi-square). The
prior mean `m_k` is

- `"parent"`: the smoothed `log(deff)` of the parent;

- `"gvf_parent"`: `g_k` plus the smoothed residual of the parent,
  `log(deff_liscio_parent) - g_parent`, so that the GVF carries the
  dependence on the share and the parent the code-specific clustering;

- roots: `g_k` for both.

The prior variance `tau^2` of each level is the moment estimator of
DerSimonian and Laird (1986) with known means,
`tau^2 = max(0, (sum w (y - m)^2 - K) / sum w)`, `w = 1 / s^2`. The
posterior mean is `B m + (1 - B) y` with `B = s^2 / (s^2 + tau^2)`,
reported as `peso_contrazione`. Nodes without a usable direct value
receive the prior mean (`B = 1`). With `"gvf"` the smoothed value is `g`
itself (`B = 1`). The truncation at 1 is applied to the output only;
untruncated values are passed down the tree.

## References

Wolter, K. M. (2007). *Introduction to Variance Estimation* (2nd ed.).
Springer, chapter 7 (generalized variance functions).

DerSimonian, R. and Laird, N. (1986). Meta-analysis in clinical trials.
*Controlled Clinical Trials*, 7(3), 177-188.

## Examples

``` r
est <- data.frame(
  code = c("A", "A1", "A2", "B", "B1"),
  parent = c(NA, "A", "A", NA, "B"),
  level = c(1, 2, 2, 1, 2),
  quota = c(0.3, 0.2, 0.1, 0.05, 0.05),
  deff = c(8, 6, 1.5, 3, 12),
  df = c(20, 15, 4, 10, 2)
)
smooth_deff(est, "code", "parent", "level")
#>      code parent level quota  deff    df deff_liscio peso_contrazione
#>    <char> <char> <num> <num> <num> <num>       <num>            <num>
#> 1:      A   <NA>     1  0.30   8.0    20    6.674269        0.5541694
#> 2:     A1      A     2  0.20   6.0    15    5.995506        0.4364332
#> 3:     A2      A     2  0.10   1.5     4    3.806304        0.7438556
#> 4:      B   <NA>     1  0.05   3.0    10    4.782312        0.7131390
#> 5:     B1      B     2  0.05  12.0     2    3.979930        0.8531160
```
