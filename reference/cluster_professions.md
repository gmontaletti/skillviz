# Hierarchical clustering of professions

Wrapper around [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)
for clustering a profession distance matrix. Returns the `hclust` object
which can be plotted as a dendrogram or cut into groups with
[`stats::cutree()`](https://rdrr.io/r/stats/cutree.html).

## Usage

``` r
cluster_professions(distance, method = "complete")
```

## Arguments

- distance:

  A `dist` object, typically from
  [`compute_profession_distance()`](https://gmontaletti.github.io/skillviz/reference/compute_profession_distance.md).

- method:

  Character, agglomeration method passed to
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html). Default:
  `"complete"`. Other options: `"ward.D2"`, `"average"`, `"single"`,
  etc.

## Value

An `hclust` object.
