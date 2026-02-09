# Co-occurrence analysis -----

# 1. Internal: single-profession co-occurrence -----

#' Compute skill co-occurrence for a single profession
#'
#' Builds a sparse term-document matrix for a given profession, computes the
#' co-occurrence via [Matrix::crossprod()], and extracts the top edges using
#' an igraph largest-component filter. This is the workhorse behind
#' [cooc_all_professions()].
#'
#' @param professione Character scalar identifying the profession to filter on.
#' @param data A data.table with at least columns `general_id`,
#'   `escoskill_level_3`, and the column named by `group_col`.
#' @param group_col Character name of the column used to identify professions.
#'   Defaults to `"preferredLabel"`.
#' @param top_n Integer maximum number of edges to retain (before
#'   largest-component filtering). Defaults to `30L`.
#' @param min_weight Integer minimum co-occurrence count. Edges below this
#'   threshold are dropped. Defaults to `10L`.
#' @return A data.table with columns `from`, `to`, `weight`. Returns an empty
#'   data.table (zero rows, same columns) when the profession has no data, a
#'   single skill, or no edges above `min_weight`.
#' @keywords internal
.cooc_single <- function(
  professione,
  data,
  group_col = "preferredLabel",
  top_n = 30L,
  min_weight = 10L
) {
  empty_dt <- data.table::data.table(
    from = character(0L),
    to = character(0L),
    weight = numeric(0L)
  )

  # 1a. Filter to profession -----
  subset <- data[
    get(group_col) == professione,
    .(general_id, escoskill_level_3)
  ]

  if (nrow(subset) == 0L) {
    return(empty_dt)
  }
  if (data.table::uniqueN(subset$escoskill_level_3) < 2L) {
    return(empty_dt)
  }

  # 1b. Sparse term-document matrix -----
  tdm <- xtabs(~ general_id + escoskill_level_3, data = subset, sparse = TRUE)

  # 1c. Co-occurrence matrix -----
  cooc_mat <- Matrix::crossprod(tdm, tdm)
  diag(cooc_mat) <- 0
  cooc_mat[upper.tri(cooc_mat)] <- 0

  # 1d. Threshold low weights -----
  cooc_mat[cooc_mat < min_weight] <- 0

  if (sum(cooc_mat) == 0) {
    return(empty_dt)
  }

  # 1e. Build igraph, extract edges -----
  g <- igraph::graph_from_adjacency_matrix(
    cooc_mat,
    mode = "max",
    weighted = TRUE
  )
  edges <- igraph::as_data_frame(g)
  data.table::setDT(edges)
  data.table::setorder(edges, -weight)

  # 1f. Top-n and largest component -----
  if (nrow(edges) > top_n) {
    edges <- edges[seq_len(top_n)]
    g_sub <- igraph::graph_from_data_frame(edges)
    g_sub <- igraph::largest_component(g_sub)
    edges <- igraph::as_data_frame(g_sub)
    data.table::setDT(edges)
  }

  if (nrow(edges) == 0L) {
    return(empty_dt)
  }

  edges
}

# 2. Batch co-occurrence for all professions -----

#' Compute skill co-occurrence networks for multiple professions
#'
#' Iterates over a set of professions and calls the internal
#' [.cooc_single()] for each one, binding the results into a single
#' data.table with a `professione` identifier column.
#'
#' @param skills A data.table with at least columns `general_id`,
#'   `escoskill_level_3`, and the column named by `group_col`.
#' @param professions Optional character vector of profession names to process.
#'   When `NULL` (the default), all unique values of `group_col` are used.
#' @param group_col Character name of the column used to identify professions.
#'   Defaults to `"preferredLabel"`.
#' @param top_n Integer maximum number of edges per profession (before
#'   largest-component filtering). Defaults to `30L`.
#' @param min_weight Integer minimum co-occurrence count. Defaults to `10L`.
#' @return A data.table with columns `professione`, `from`, `to`, `weight`.
#'   Professions producing no valid edges are silently dropped.
#'
#' @examples
#' \dontrun{
#' skills <- data.table::data.table(
#'   general_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   escoskill_level_3 = c("A", "B", "C", "A", "B", "A", "B", "C"),
#'   preferredLabel = rep("Analyst", 8)
#' )
#' cooc_all_professions(skills)
#' }
#'
#' @export
cooc_all_professions <- function(
  skills,
  professions = NULL,
  group_col = "preferredLabel",
  top_n = 30L,
  min_weight = 10L
) {
  check_columns(
    skills,
    c("general_id", "escoskill_level_3", group_col),
    caller = "cooc_all_professions"
  )

  if (is.null(professions)) {
    professions <- unique(skills[[group_col]])
  }

  prof_list <- as.list(professions)
  names(prof_list) <- professions

  results <- lapply(prof_list, function(p) {
    .cooc_single(
      professione = p,
      data = skills,
      group_col = group_col,
      top_n = top_n,
      min_weight = min_weight
    )
  })

  out <- data.table::rbindlist(results, idcol = "professione", fill = TRUE)
  out <- out[nchar(from) > 0L]
  out
}

# 3. Raw co-occurrence matrix -----

#' Build a raw co-occurrence sparse matrix
#'
#' Constructs a symmetric skill co-occurrence matrix from a data.table of
#' skill assignments. The matrix entry \eqn{(i, j)} counts how many
#' announcements (`general_id`) mention both skill \eqn{i} and skill
#' \eqn{j}.
#'
#' @param skills A data.table with columns `general_id` and
#'   `escoskill_level_3`.
#' @return A symmetric sparse matrix of class [Matrix::dgCMatrix-class]
#'   with skill names as row/column names.
#'
#' @examples
#' \dontrun{
#' skills <- data.table::data.table(
#'   general_id = c(1, 1, 1, 2, 2),
#'   escoskill_level_3 = c("A", "B", "C", "A", "B")
#' )
#' mat <- compute_cooc_matrix(skills)
#' }
#'
#' @export
compute_cooc_matrix <- function(skills) {
  check_columns(
    skills,
    c("general_id", "escoskill_level_3"),
    caller = "compute_cooc_matrix"
  )

  tdm <- xtabs(
    ~ general_id + escoskill_level_3,
    data = skills[, .(general_id, escoskill_level_3)],
    sparse = TRUE
  )

  Matrix::crossprod(tdm, tdm)
}

# 4. Relative-risk filtering -----

#' Filter a co-occurrence matrix by relative risk
#'
#' Given a raw co-occurrence matrix (e.g. from [compute_cooc_matrix()]),
#' computes expected frequencies under independence and retains only
#' edges where observed / expected > 1. The diagonal and upper triangle
#' are zeroed so each pair appears at most once.
#'
#' @param cooc_matrix A symmetric numeric matrix (dense or sparse) of
#'   co-occurrence counts, as returned by [compute_cooc_matrix()].
#' @return A data.table with columns `from`, `to`, `weight` containing
#'   only edges whose observed co-occurrence exceeds the independence
#'   baseline. Edges are sorted by descending `weight`.
#'
#' @examples
#' \dontrun{
#' mat <- compute_cooc_matrix(skills)
#' rr_edges <- filter_relative_risk(mat)
#' }
#'
#' @export
filter_relative_risk <- function(cooc_matrix) {
  mat <- as.matrix(cooc_matrix)
  diag(mat) <- 0
  mat[upper.tri(mat)] <- 0

  total <- sum(mat)

  if (total == 0) {
    return(data.table::data.table(
      from = character(0L),
      to = character(0L),
      weight = numeric(0L)
    ))
  }

  # 4a. Expected frequencies under independence -----
  expected <- (colSums(mat) %*% t(rowSums(mat))) / total

  # 4b. Zero cells below baseline -----
  mat[expected > 0 & (mat / expected) <= 1] <- 0

  # 4c. Convert to edge list -----
  df <- as.data.frame(mat)
  df$from <- rownames(df)
  data.table::setDT(df)

  edges <- data.table::melt.data.table(
    df,
    id.vars = "from",
    variable.name = "to",
    value.name = "weight",
    variable.factor = FALSE
  )

  edges <- edges[weight > 0]
  data.table::setorder(edges, -weight)
  edges
}
