# Plot a skill co-occurrence network graph

Builds an undirected graph from co-occurrence edge data and renders it
with `ggraph`. Edge width and transparency scale with co-occurrence
weight. Nodes are labelled with repelled text to avoid overlap.

## Usage

``` r
plot_cooc_graph(cooc_data, profession = NULL, layout = "fr")
```

## Arguments

- cooc_data:

  A `data.table` (or data.frame) with columns `from`, `to`, and `weight`
  representing pairwise skill co-occurrences.

- profession:

  Optional character string. When supplied, the graph is drawn with this
  profession name as plot title.

- layout:

  Character, graph layout algorithm passed to
  [`ggraph::ggraph()`](https://ggraph.data-imaginist.com/reference/ggraph.html).
  Default `"fr"` (Fruchterman-Reingold).

## Value

A `ggplot` object produced by `ggraph`.

## Details

Only the largest connected component is plotted, so isolated nodes or
disconnected sub-graphs are dropped.
