# 1. deprecated functions index -----

#' Deprecated functions in skillviz
#'
#' The functions listed below are deprecated. Each one still works, but
#' emits a deprecation warning and will be removed in a future release.
#' Alternatives are given for each.
#'
#' @details
#' \describe{
#'   \item{\code{\link{read_ojv_zip}}}{Use
#'     \code{\link{read_oja_itaposts}}.}
#'   \item{\code{\link{normalize_ojv}}}{Use
#'     \code{\link{read_oja_itaposts}}.}
#' }
#'
#' Both read the raw Lightcast ZIP archives directly. That access path is
#' superseded by the DuckDB store maintained by the \pkg{itaposts}
#' package, which owns the read, dedup and join logic these two functions
#' duplicate. \code{\link{read_oja_itaposts}} returns the same
#' \code{list(postings, skills, companies)} shape as
#' \code{\link{normalize_ojv}}, so migrating a call site means changing
#' only its data-loading line.
#'
#' @name skillviz-deprecated
#' @keywords internal
NULL
