# Detect skill communities from a co-occurrence edge list

Builds an igraph graph from a co-occurrence data.table (edge list with
`from`, `to`, `weight` columns) and applies a community detection
algorithm. Supported methods: infomap, label propagation, walktrap, and
spinglass.

## Usage

``` r
detect_skill_communities(cooc, method = "infomap")
```

## Arguments

- cooc:

  A `data.table` with columns `from`, `to`, and `weight` representing
  the co-occurrence edge list. Weights must be positive.

- method:

  Character, community detection method. One of `"infomap"`,
  `"label_prop"`, `"walktrap"`, `"spinglass"`. Default: `"infomap"`.

## Value

A named `list` with:

- graph:

  An `igraph` graph object built from the edge list.

- communities:

  The community detection result object (class depends on the method
  used).

- membership:

  A named integer vector mapping each vertex to its community ID.
