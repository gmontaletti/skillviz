# Visualization functions -----

# 1. Co-occurrence network graph -----

#' Plot a skill co-occurrence network graph
#'
#' Builds an undirected graph from co-occurrence edge data and renders it with
#' `ggraph`. Edge width and transparency scale with co-occurrence weight.
#' Nodes are labelled with repelled text to avoid overlap.
#'
#' Only the largest connected component is plotted, so isolated nodes or
#' disconnected sub-graphs are dropped.
#'
#' @param cooc_data A `data.table` (or data.frame) with columns `from`, `to`,
#'   and `weight` representing pairwise skill co-occurrences.
#' @param profession Optional character string. When supplied, the graph is
#'   drawn with this profession name as plot title.
#' @param layout Character, graph layout algorithm passed to
#'   [ggraph::ggraph()]. Default `"fr"` (Fruchterman-Reingold).
#' @return A `ggplot` object produced by `ggraph`.
#' @export
plot_cooc_graph <- function(cooc_data, profession = NULL, layout = "fr") {
  check_suggests("ggraph", reason = "for network visualisation")
  check_suggests("tidygraph", reason = "for tidy graph construction")
  check_suggests("ggplot2", reason = "for plot rendering")

  if (!is.data.frame(cooc_data) || nrow(cooc_data) == 0) {
    stop(
      "plot_cooc_graph: cooc_data must be a non-empty data.frame",
      call. = FALSE
    )
  }
  check_columns(
    cooc_data,
    c("from", "to", "weight"),
    caller = "plot_cooc_graph"
  )

  # 1.1 Build igraph object and keep largest component -----
  g <- igraph::graph_from_data_frame(
    cooc_data[, c("from", "to", "weight")],
    directed = FALSE
  )
  if (!igraph::is_connected(g)) {
    g <- igraph::largest_component(g)
  }

  # 1.2 Convert to tidygraph -----
  tg <- tidygraph::as_tbl_graph(g)

  # 1.3 Render with ggraph -----
  p <- ggraph::ggraph(tg, layout = layout) +
    ggraph::geom_edge_link0(
      ggplot2::aes(width = .data$weight, alpha = .data$weight)
    ) +
    ggraph::geom_node_point() +
    ggraph::geom_node_label(
      ggplot2::aes(label = .data$name),
      repel = TRUE
    ) +
    ggraph::theme_graph()

  if (!is.null(profession)) {
    p <- p + ggplot2::labs(title = profession)
  }

  p
}


# 2. Skill ranking line plot -----

#' Plot temporal skill ranking for a profession
#'
#' Draws a line chart showing how skill ranks evolve over time for a single
#' profession. Each line represents a skill; the y-axis is the rank position
#' and the x-axis is the time period.
#'
#' When `skills` is `NULL` the function selects the 10 most frequently
#' occurring skills (summing counts across all time periods).
#'
#' @param serie A `data.table` with columns `mese` (date), `skill`
#'   (character), `rango` (integer rank), `professione` (character), and `N`
#'   (count). Typically produced by a ranking computation step.
#' @param profession Character, the profession label to filter on.
#' @param skills Optional character vector of skill labels to include. When
#'   `NULL` (the default), the top 10 skills by total count are used.
#' @return A `ggplot` object.
#' @export
plot_skill_ranking <- function(serie, profession, skills = NULL) {
  check_suggests("ggplot2", reason = "for plotting")

  if (!is.data.frame(serie) || nrow(serie) == 0) {
    stop(
      "plot_skill_ranking: serie must be a non-empty data.frame",
      call. = FALSE
    )
  }
  check_columns(
    serie,
    c("mese", "skill", "rango", "professione"),
    caller = "plot_skill_ranking"
  )

  dt <- data.table::as.data.table(serie)

  # 2.1 Filter to the requested profession -----
  dt <- dt[professione == profession]
  if (nrow(dt) == 0) {
    stop(
      "plot_skill_ranking: no rows for profession '",
      profession,
      "'",
      call. = FALSE
    )
  }

  # 2.2 Determine which skills to plot -----
  if (is.null(skills)) {
    top <- dt[, .(tot = sum(N)), .(skill)][order(-tot)][1:min(.N, 10), skill]
    dt <- dt[skill %in% top]
  } else {
    dt <- dt[skill %in% skills]
    if (nrow(dt) == 0) {
      stop(
        "plot_skill_ranking: none of the requested skills found for '",
        profession,
        "'",
        call. = FALSE
      )
    }
  }

  # 2.3 Build line plot -----
  ggplot2::ggplot(dt) +
    ggplot2::aes(
      x = .data$mese,
      y = .data$rango,
      group = .data$skill,
      colour = .data$skill
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = profession,
      x = NULL,
      y = "Rank",
      colour = "Skill"
    ) +
    ggplot2::theme_minimal()
}
