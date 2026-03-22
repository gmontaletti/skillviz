# Profession distance and clustering -----

# 1. compute_profession_distance -----

#' Compute pairwise distances between professions based on skill profiles
#'
#' Builds a profession x skill incidence matrix from a data.table of
#' profession-skill associations. Applies a Balassa-type filter so that
#' only cells where the observed count exceeds the expected count are
#' retained (revealed comparative advantage). The transposed matrix is
#' then passed to `stats::dist()`.
#'
#' @param competenze A `data.table` with at least columns `cod_3`
#'   (profession code) and `escoskill_level_3` (skill label).
#' @param method Character, distance method passed to [stats::dist()].
#'   Default: `"binary"`. Other common options: `"euclidean"`,
#'   `"canberra"`, `"maximum"`, `"minkowski"`.
#' @param filter_rca Logical, whether to zero out cells where the observed
#'   count is below the expected count (Balassa filter). Default: `TRUE`.
#' @return A `dist` object with pairwise distances between professions.
#' @export
compute_profession_distance <- function(
  competenze,
  method = "binary",
  filter_rca = TRUE
) {
  # 1. input validation -----
  check_columns(
    competenze,
    c("cod_3", "escoskill_level_3"),
    caller = "compute_profession_distance"
  )

  if (!is.data.table(competenze)) {
    competenze <- data.table::as.data.table(competenze)
  }

  # 2. build incidence matrix -----
  mat <- data.table::dcast(
    competenze,
    escoskill_level_3 ~ cod_3,
    fun.aggregate = length
  )
  rn <- mat$escoskill_level_3
  mat[, escoskill_level_3 := NULL]
  mat <- as.matrix(mat)
  rownames(mat) <- rn

  # 3. Balassa filter (RCA) -----
  if (filter_rca) {
    expected <- (rowSums(mat) %*% t(colSums(mat))) / sum(mat)
    mat[mat / expected < 1] <- 0
  }

  # 4. transpose and compute distance -----
  mat <- t(mat)
  stats::dist(mat, method = method)
}


# 2. cluster_professions -----

#' Hierarchical clustering of professions
#'
#' Wrapper around [stats::hclust()] for clustering a profession distance
#' matrix. Returns the `hclust` object which can be plotted as a
#' dendrogram or cut into groups with [stats::cutree()].
#'
#' @param distance A `dist` object, typically from
#'   [compute_profession_distance()].
#' @param method Character, agglomeration method passed to
#'   [stats::hclust()]. Default: `"complete"`. Other options:
#'   `"ward.D2"`, `"average"`, `"single"`, etc.
#' @return An `hclust` object.
#' @export
cluster_professions <- function(distance, method = "complete") {
  if (!inherits(distance, "dist")) {
    stop(
      "cluster_professions: 'distance' must be a dist object",
      call. = FALSE
    )
  }

  stats::hclust(distance, method = method)
}


# 3. build_skill_prof_sparse -----

#' Build a sparse skill-profession matrix
#'
#' Converts a long-format `data.table` with skill-profession associations
#' into a sparse `dgCMatrix`. Rows represent skills and columns represent
#' professions.
#'
#' @param balassa `data.table` with skill-profession associations. Must
#'   contain columns `escoskill_level_3`, `prof_col`, and the column named
#'   by `value_col`.
#' @param value_col Character, column name for cell values. Use `"N"` for
#'   counts or `"balassa_index"` for RCA values. Default: `"N"`.
#' @param prof_col Character, column name for profession identifiers.
#'   Default: `"cod_3"`.
#' @return A `dgCMatrix` with skills as rows and professions as columns.
#' @export
build_skill_prof_sparse <- function(
  balassa,
  value_col = "N",
  prof_col = "cod_3"
) {
  check_columns(
    balassa,
    c("escoskill_level_3", prof_col, value_col),
    caller = "build_skill_prof_sparse"
  )

  if (!data.table::is.data.table(balassa)) {
    balassa <- data.table::as.data.table(balassa)
  }

  skill_f <- factor(balassa$escoskill_level_3)
  prof_f <- factor(balassa[[prof_col]])

  Matrix::sparseMatrix(
    i = as.integer(skill_f),
    j = as.integer(prof_f),
    x = as.numeric(balassa[[value_col]]),
    dims = c(nlevels(skill_f), nlevels(prof_f)),
    dimnames = list(levels(skill_f), levels(prof_f))
  )
}


# 4. compute_cosine_similarity -----

#' Pairwise cosine similarity between matrix columns
#'
#' Computes a dense symmetric matrix of cosine similarities between all
#' pairs of columns in the input matrix. Works with both dense and sparse
#' matrices.
#'
#' @param mat Numeric matrix (dense or sparse). Columns are the entities
#'   to compare.
#' @return Dense symmetric matrix of cosine similarities.
#' @export
compute_cosine_similarity <- function(mat) {
  cp <- Matrix::crossprod(mat)
  norms <- sqrt(Matrix::diag(cp))
  norms[norms == 0] <- 1
  sim <- as.matrix(cp / (norms %o% norms))
  sim
}


# 5. compute_decomposed_distance -----

#' Squared Euclidean distance decomposed by skill subsets
#'
#' Computes the total squared Euclidean distance between profession
#' pairs and decomposes it by skill groups. For any disjoint partition
#' \eqn{\{A_1, \ldots, A_K\}}, the additive property holds:
#' \eqn{d^2(i, j) = \sum_k d^2_k(i, j)}.
#'
#' @param mat Balassa-weighted matrix (rows = skills, columns =
#'   professions). Can be dense or sparse.
#' @param skill_groups `data.table` with columns `escoskill_level_3` and
#'   `group`, mapping each skill to its group.
#' @return A list with three elements:
#'   - `total`: a `dist` object of total squared Euclidean distances.
#'   - `partial`: a named list of `dist` objects, one per group.
#'   - `contribution`: a `data.table` with columns `prof_i`, `prof_j`,
#'     `group`, `partial_d2`, `total_d2`, and `share`.
#' @export
compute_decomposed_distance <- function(mat, skill_groups) {
  check_columns(
    skill_groups,
    c("escoskill_level_3", "group"),
    caller = "compute_decomposed_distance"
  )

  if (!data.table::is.data.table(skill_groups)) {
    skill_groups <- data.table::as.data.table(skill_groups)
  }

  groups <- unique(skill_groups$group)
  all_skills <- rownames(mat)
  n_prof <- ncol(mat)
  prof_labels <- colnames(mat)

  # 1. total squared Euclidean distance -----
  total <- stats::dist(t(as.matrix(mat)), method = "euclidean")^2

  # 2. partial distances by group -----
  partial <- lapply(stats::setNames(groups, groups), function(g) {
    skills_g <- skill_groups[group == g, escoskill_level_3]
    idx <- which(all_skills %in% skills_g)
    if (length(idx) == 0L) {
      return(stats::dist(matrix(0, n_prof, 1)))
    }
    stats::dist(
      t(as.matrix(mat[idx, , drop = FALSE])),
      method = "euclidean"
    )^2
  })

  # 3. contribution data.table -----
  pairs <- utils::combn(length(prof_labels), 2)
  total_vec <- as.numeric(total)

  contribution <- data.table::rbindlist(lapply(groups, function(g) {
    partial_vec <- as.numeric(partial[[g]])
    data.table::data.table(
      prof_i = prof_labels[pairs[1L, ]],
      prof_j = prof_labels[pairs[2L, ]],
      group = g,
      partial_d2 = partial_vec,
      total_d2 = total_vec,
      share = ifelse(total_vec == 0, 0, partial_vec / total_vec)
    )
  }))

  list(total = total, partial = partial, contribution = contribution)
}


# 6. compute_skill_similarity -----

#' Skill-skill cosine similarity based on profession profiles
#'
#' Computes pairwise cosine similarity between skills using their
#' profession profile vectors. Skills appearing in fewer than
#' `min_professions` are excluded before computation.
#'
#' @param mat Numeric matrix (rows = skills, columns = professions).
#'   Can be dense or sparse.
#' @param min_professions Integer, minimum number of professions a skill
#'   must appear in (non-zero entries) to be included. Default: `3L`.
#' @return Dense symmetric matrix of skill-skill cosine similarities.
#' @export
compute_skill_similarity <- function(mat, min_professions = 3L) {
  if (inherits(mat, "sparseMatrix")) {
    n_prof <- Matrix::rowSums(mat != 0)
  } else {
    n_prof <- rowSums(mat != 0)
  }

  keep <- n_prof >= min_professions
  mat_f <- mat[keep, , drop = FALSE]

  if (nrow(mat_f) < 2L) {
    stop(
      "compute_skill_similarity: fewer than 2 skills meet the ",
      "min_professions threshold (",
      min_professions,
      ")",
      call. = FALSE
    )
  }

  compute_cosine_similarity(Matrix::t(mat_f))
}
