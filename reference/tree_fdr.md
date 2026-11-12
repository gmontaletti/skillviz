# Hierarchical false discovery rate control on a tree

Tests the hypotheses of a tree from the top: the Benjamini-Hochberg
procedure is applied within each family of siblings, and the children of
a node are tested only when the node is rejected.

## Usage

``` r
tree_fdr(p, node, parent, q = 0.05)
```

## Arguments

- p:

  Numeric vector of p-values, one per node (`NA` = not testable; the
  node and its descendants are not tested).

- node:

  Character vector of node identifiers (unique).

- parent:

  Character vector of parent identifiers; `NA`, `""` or an identifier
  not in `node` marks a node of the first family.

- q:

  Target level within each family (default 0.05).

## Value

A data.table in the order of `node` with columns `node`, `parent`, `p`,
`q_famiglia` (BH-adjusted p-value within the family, `NA` when not
tested), `tested`, `rejected`.

## Details

The procedure follows Yekutieli (2008): the family of the nodes without
a parent is tested first with Benjamini-Hochberg at level `q`; for each
rejected node, the family of its children is tested in the same way, and
so on down the tree. Yekutieli shows that, with independent p-values,
the expected proportion of false discoveries over the whole tree is
bounded by `q` times a factor that depends on the tree (about 2 times
the number of levels in the worst case); Benjamini and Bogomolov (2014)
give the selective FDR interpretation of the family-wise adjusted
values.

## References

Yekutieli, D. (2008). Hierarchical false discovery rate-controlling
methodology. *Journal of the American Statistical Association*,
103(481), 309-316.

Benjamini, Y. and Bogomolov, M. (2014). Selective inference on multiple
families of hypotheses. *Journal of the Royal Statistical Society:
Series B*, 76(1), 297-318.

## Examples

``` r
tree_fdr(
  p = c(0.001, 0.4, 0.01, 0.03, 0.2),
  node = c("A", "B", "A1", "A2", "B1"),
  parent = c(NA, NA, "A", "A", "B")
)
#>      node parent     p q_famiglia tested rejected
#>    <char> <char> <num>      <num> <lgcl>   <lgcl>
#> 1:      A   <NA> 0.001      0.002   TRUE     TRUE
#> 2:      B   <NA> 0.400      0.400   TRUE    FALSE
#> 3:     A1      A 0.010      0.020   TRUE     TRUE
#> 4:     A2      A 0.030      0.030   TRUE     TRUE
#> 5:     B1      B 0.200         NA  FALSE    FALSE
```
