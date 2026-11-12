# Finest reliable level of a code hierarchy

For every node, returns the node itself when its interval is narrow
enough, otherwise the closest ancestor whose interval is.

## Usage

``` r
finest_reliable_level(
  est,
  key_col,
  parent_col,
  rel_width_col,
  max_rel_width,
  level_col = NULL
)
```

## Arguments

- est:

  data.frame or data.table with one row per node.

- key_col:

  Name of the node identifier column.

- parent_col:

  Name of the parent column (`NA` or `""` for roots).

- rel_width_col:

  Name of the relative width column (e.g. `(hi - lo) / quota`); `NA`
  counts as not reliable.

- max_rel_width:

  Maximum relative width of a reliable node.

- level_col:

  Optional name of a level column; when given, the level of the reliable
  node is returned as well.

## Value

A copy of `est` with column `codice_affidabile` (the node or its closest
reliable ancestor, `NA` when none is reliable) and, when `level_col` is
given, `livello_affidabile`.

## Details

The search walks up the parent links one level at a time, vectorised
over all nodes, and stops at the first node with
`rel_width <= max_rel_width`.

## Examples

``` r
est <- data.frame(
  code = c("A", "A1", "A11", "B", "B1"),
  parent = c(NA, "A", "A1", NA, "B"),
  level = c(1, 2, 3, 1, 2),
  ampiezza_rel = c(0.1, 0.4, 1.2, 0.9, 2)
)
finest_reliable_level(est, "code", "parent", "ampiezza_rel", 0.5,
                      level_col = "level")
#>      code parent level ampiezza_rel codice_affidabile livello_affidabile
#>    <char> <char> <num>        <num>            <char>              <num>
#> 1:      A   <NA>     1          0.1                 A                  1
#> 2:     A1      A     2          0.4                A1                  2
#> 3:    A11     A1     3          1.2                A1                  2
#> 4:      B   <NA>     1          0.9              <NA>                 NA
#> 5:     B1      B     2          2.0              <NA>                 NA
```
