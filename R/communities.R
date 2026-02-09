# Community detection on skill co-occurrence networks -----

# 1. detect_skill_communities -----

#' Detect skill communities from a co-occurrence edge list
#'
#' Builds an igraph graph from a co-occurrence data.table (edge list with
#' `from`, `to`, `weight` columns) and applies a community detection
#' algorithm. Supported methods: infomap, label propagation, walktrap,
#' and spinglass.
#'
#' @param cooc A `data.table` with columns `from`, `to`, and `weight`
#'   representing the co-occurrence edge list. Weights must be positive.
#' @param method Character, community detection method. One of `"infomap"`,
#'   `"label_prop"`, `"walktrap"`, `"spinglass"`. Default: `"infomap"`.
#' @return A named `list` with:
#'   \describe{
#'     \item{graph}{An `igraph` graph object built from the edge list.}
#'     \item{communities}{The community detection result object (class
#'       depends on the method used).}
#'     \item{membership}{A named integer vector mapping each vertex to its
#'       community ID.}
#'   }
#' @export
detect_skill_communities <- function(cooc, method = "infomap") {
  # 1. input validation -----
  check_columns(
    cooc,
    c("from", "to", "weight"),
    caller = "detect_skill_communities"
  )

  valid_methods <- c("infomap", "label_prop", "walktrap", "spinglass")
  if (!method %in% valid_methods) {
    stop(
      "detect_skill_communities: method must be one of: ",
      paste(valid_methods, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.data.table(cooc)) {
    cooc <- data.table::as.data.table(cooc)
  }

  # 2. build graph -----
  g <- igraph::graph_from_data_frame(
    cooc[, .(from, to, weight)],
    directed = FALSE
  )
  igraph::E(g)$weight <- cooc$weight

  # 3. apply community detection -----
  communities <- switch(
    method,
    infomap = igraph::cluster_infomap(g, e.weights = igraph::E(g)$weight),
    label_prop = igraph::cluster_label_prop(g, weights = igraph::E(g)$weight),
    walktrap = igraph::cluster_walktrap(g, weights = igraph::E(g)$weight),
    spinglass = igraph::cluster_spinglass(g, weights = igraph::E(g)$weight)
  )

  # 4. extract membership -----
  membership <- igraph::membership(communities)

  list(
    graph = g,
    communities = communities,
    membership = membership
  )
}
