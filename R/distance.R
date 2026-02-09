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
