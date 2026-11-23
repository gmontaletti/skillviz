# Reference professions between two occupational classifications -----
#
# For every code of one classification (for example CP2021 level 5) the
# functions in this file name a reference profession of the other
# classification (for example ESCO level 5):
#
# - by frequency, from the postings that carry both codes (the de facto
#   crosswalk of build_esco_cp_crosswalk(), which is reused unchanged);
# - by skill similarity, as the code of the other classification whose
#   IDF-weighted coverage profile has the largest cosine with the profile of
#   the code, when the code has too few doubly-coded postings.
#
# The threshold between the two rules is calibrated on the data by
# calibrate_reference_threshold(). transfer_skill_profile() gives a code the
# profile of its reference profession and validate_profile_transfer()
# compares the transferred profile with the observed one against two
# baselines.
#
# Contents:
# 1. internal helpers;
# 2. skill_idf();
# 3. skill_profiles();
# 4. build_reference_professions();
# 5. calibrate_reference_threshold();
# 6. transfer_skill_profile();
# 7. validate_profile_transfer();
# 8. group_mixture_weights();
# 9. transfer_skill_profile_mixture().

utils::globalVariables(c(
  "grp_parent",
  "n_grp",
  "n_own",
  "weight",
  "weight_total",
  "weight_covered",
  "n_refs",
  "n_refs_covered",
  "covered",
  "covered_share",
  "code",
  "label",
  "parent",
  "skill",
  "n_skill_postings",
  "n_cocoded",
  "n_postings",
  "n_profile",
  "method",
  "reason",
  "ref_code",
  "ref_label",
  "ref_parent",
  "ref_in_universe",
  "sim_code",
  "sim_cosine",
  "sim2_code",
  "sim2_cosine",
  "sim_tied",
  "cosine_ref",
  "code_modal",
  "n_labelled",
  "usable",
  "target",
  "agreement",
  "agreement_weighted",
  "cosine_transfer",
  "cosine_parent",
  "cosine_sibling",
  "sibling_code",
  "measure",
  "id",
  "n",
  "from",
  "to",
  "stratum",
  "included",
  "n_eff",
  "modal_share",
  "modal_tied",
  "modal_reliable",
  "modal_stable"
))

# 1. Internal helpers -----

#' Methods of build_reference_professions()
#'
#' @return Character vector of the three method labels.
#' @noRd
.ref_methods <- function() {
  c("frequenza", paste0("similarit", intToUtf8(224L)), "nessuno")
}

#' Check that an argument is a single positive integer
#'
#' @param x Value to check.
#' @param arg Argument name, for the message.
#' @param caller Calling function, for the message.
#' @return `x` as integer.
#' @noRd
.ref_count_arg <- function(x, arg, caller, min = 1L) {
  ok <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    x == round(x) &&
    x >= min
  if (!ok) {
    stop(
      caller,
      ": `",
      arg,
      "` must be a single integer >= ",
      min,
      ".",
      call. = FALSE
    )
  }
  as.integer(x)
}

#' Normalise a code column: missing markers become NA
#'
#' @param x Vector of codes.
#' @param na Character vector of values that mean "no code".
#' @return Character vector.
#' @noRd
.ref_clean_code <- function(x, na) {
  x <- as.character(x)
  x[!is.na(x) & x %chin% as.character(na)] <- NA_character_
  x
}

#' Validate and normalise a universe table
#'
#' @param universe data.frame with column `code` and optional `label` and
#'   `parent`.
#' @param arg Argument name, for the message.
#' @param caller Calling function.
#' @return data.table with character columns `code`, `label`, `parent`.
#' @noRd
.ref_universe <- function(universe, arg, caller) {
  if (!is.data.frame(universe) || !"code" %in% names(universe)) {
    stop(
      caller,
      ": `",
      arg,
      "` must be a data.frame with a `code` column.",
      call. = FALSE
    )
  }
  u <- data.table::data.table(code = as.character(universe[["code"]]))
  u[,
    label := if ("label" %in% names(universe)) {
      as.character(universe[["label"]])
    } else {
      NA_character_
    }
  ]
  u[,
    parent := if ("parent" %in% names(universe)) {
      as.character(universe[["parent"]])
    } else {
      NA_character_
    }
  ]
  if (anyNA(u$code) || anyDuplicated(u$code)) {
    stop(
      caller,
      ": `",
      arg,
      "$code` must be unique and not missing.",
      call. = FALSE
    )
  }
  data.table::setorderv(u, "code")
  u[]
}

#' Evaluate an expression with a fixed RNG seed, then restore the RNG
#'
#' @param seed Integer seed.
#' @param expr Expression.
#' @return Value of `expr`.
#' @noRd
.ref_with_seed <- function(seed, expr) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  })
  set.seed(seed)
  expr
}

#' Coerce a matrix to a column-compressed sparse matrix
#'
#' @param x A base or `Matrix` matrix.
#' @return A `CsparseMatrix` with the same dimnames.
#' @noRd
.ref_as_csparse <- function(x) {
  if (inherits(x, "CsparseMatrix")) {
    return(x)
  }
  out <- Matrix::Matrix(as.matrix(x), sparse = TRUE)
  dimnames(out) <- dimnames(x)
  out
}

#' Put two profile matrices on the same skill columns
#'
#' @param a,b Sparse matrices with column names.
#' @return List with `a` and `b` on the union of the columns, in the order of
#'   `a` followed by the columns only in `b`.
#' @noRd
.ref_align_cols <- function(a, b) {
  if (identical(colnames(a), colnames(b))) {
    return(list(a = a, b = b))
  }
  cols <- union(colnames(a), colnames(b))
  pad <- function(m) {
    tm <- Matrix::summary(.ref_as_csparse(m))
    Matrix::sparseMatrix(
      i = tm$i,
      j = match(colnames(m), cols)[tm$j],
      x = tm$x,
      dims = c(nrow(m), length(cols)),
      dimnames = list(rownames(m), cols)
    )
  }
  list(a = pad(a), b = pad(b))
}

#' Scale the rows of a sparse matrix to unit Euclidean norm
#'
#' Rows with zero norm stay zero.
#'
#' @param m Sparse matrix.
#' @return Sparse matrix of the same shape.
#' @noRd
.ref_row_normalize <- function(m) {
  nr <- sqrt(Matrix::rowSums(m^2))
  inv <- ifelse(nr > 0, 1 / nr, 0)
  out <- Matrix::Diagonal(x = inv) %*% m
  dimnames(out) <- dimnames(m)
  out
}

#' Cosine of paired rows of two profile matrices
#'
#' @param a,b Sparse matrices with row and column names.
#' @param ra,rb Row names of `a` and `b` to pair, same length; NA or absent
#'   names give NA.
#' @return Numeric vector of cosines.
#' @noRd
.ref_cosine_pairs <- function(a, b, ra, rb) {
  out <- rep(NA_real_, length(ra))
  ia <- match(ra, rownames(a))
  ib <- match(rb, rownames(b))
  ok <- which(!is.na(ia) & !is.na(ib))
  if (!length(ok)) {
    return(out)
  }
  al <- .ref_align_cols(a, b)
  an <- .ref_row_normalize(al$a)
  bn <- .ref_row_normalize(al$b)
  na_a <- Matrix::rowSums(an^2) == 0
  na_b <- Matrix::rowSums(bn^2) == 0
  out[ok] <- Matrix::rowSums(
    an[ia[ok], , drop = FALSE] *
      bn[ib[ok], , drop = FALSE]
  )
  out[ok][na_a[ia[ok]] | na_b[ib[ok]]] <- NA_real_
  out
}

#' Best and second-best cosine match of every row of `a` among rows of `b`
#'
#' Ties are resolved on the code: the rows of `b` are sorted by name and the
#' first maximum wins.
#'
#' @param a,b Sparse matrices with row and column names.
#' @param block Rows of `a` processed at a time.
#' @return data.table with `code`, `sim_code`, `sim_cosine`, `sim2_code`,
#'   `sim2_cosine`, `sim_tied`, one row per row of `a`.
#' @noRd
.ref_cosine_top2 <- function(a, b, block = 500L) {
  out <- data.table::data.table(
    code = rownames(a),
    sim_code = NA_character_,
    sim_cosine = NA_real_,
    sim2_code = NA_character_,
    sim2_cosine = NA_real_,
    sim_tied = NA
  )
  if (!nrow(a) || !nrow(b)) {
    return(out)
  }
  b <- b[sort(rownames(b), method = "radix"), , drop = FALSE]
  al <- .ref_align_cols(a, b)
  an <- .ref_row_normalize(al$a)
  bn <- .ref_row_normalize(al$b)
  b_ok <- Matrix::rowSums(bn^2) > 0
  a_ok <- Matrix::rowSums(an^2) > 0
  if (!any(b_ok)) {
    return(out)
  }
  bn <- bn[b_ok, , drop = FALSE]
  bnames <- rownames(bn)
  starts <- seq.int(1L, nrow(an), by = block)
  for (s in starts) {
    idx <- s:min(s + block - 1L, nrow(an))
    sim <- as.matrix(Matrix::tcrossprod(an[idx, , drop = FALSE], bn))
    j1 <- max.col(sim, ties.method = "first")
    c1 <- sim[cbind(seq_along(idx), j1)]
    if (ncol(sim) > 1L) {
      sim[cbind(seq_along(idx), j1)] <- -Inf
      j2 <- max.col(sim, ties.method = "first")
      c2 <- sim[cbind(seq_along(idx), j2)]
      n2 <- bnames[j2]
    } else {
      c2 <- rep(NA_real_, length(idx))
      n2 <- rep(NA_character_, length(idx))
    }
    data.table::set(out, idx, "sim_code", bnames[j1])
    data.table::set(out, idx, "sim_cosine", c1)
    data.table::set(out, idx, "sim2_code", n2)
    data.table::set(out, idx, "sim2_cosine", c2)
    data.table::set(out, idx, "sim_tied", !is.na(c2) & abs(c1 - c2) < 1e-12)
  }
  # Rows of `a` with an all-zero weighted profile have no defined cosine.
  out[
    !a_ok,
    `:=`(
      sim_code = NA_character_,
      sim_cosine = NA_real_,
      sim2_code = NA_character_,
      sim2_cosine = NA_real_,
      sim_tied = NA
    )
  ]
  out[]
}

#' Rows of a profile matrix that are usable
#'
#' @param profiles Matrix from [skill_profiles()].
#' @param min_postings Minimum number of postings.
#' @return Character vector of row names.
#' @noRd
.ref_usable_rows <- function(profiles, min_postings) {
  rn <- rownames(profiles)
  np <- attr(profiles, "n_postings")
  if (is.null(np)) {
    return(rn)
  }
  rn[!is.na(np[rn]) & np[rn] >= min_postings]
}

# 2. skill_idf() -----

#' Inverse document frequency of skills over postings
#'
#' Computes \eqn{idf_s = \log(N / n_s)}, where \eqn{N} is the number of
#' postings with at least one skill and \eqn{n_s} the number of those postings
#' that mention skill \eqn{s}: the same document-frequency scheme as
#' [compute_idf_classification()], returned as a named vector so that profiles
#' of two classifications can share one weighting.
#'
#' @param skills A `data.frame` with one row per posting and skill (duplicates
#'   are ignored), with columns `id_col` and `skill_col`.
#' @param id_col Name of the posting id column. Default `"general_id"`.
#' @param skill_col Name of the skill column. Default
#'   `"idescoskill_level_3"`.
#' @param ids Optional vector of posting ids: when supplied, only these
#'   postings enter \eqn{N} and \eqn{n_s}.
#' @return A named numeric vector, one element per skill, named by the skill
#'   and sorted by name. Attribute `n_postings` holds \eqn{N}.
#' @examples
#' skills <- data.table::data.table(
#'   general_id = c(1, 1, 2, 3),
#'   idescoskill_level_3 = c("a", "b", "a", "c")
#' )
#' skill_idf(skills)
#' @export
skill_idf <- function(
  skills,
  id_col = "general_id",
  skill_col = "idescoskill_level_3",
  ids = NULL
) {
  if (!is.data.frame(skills)) {
    stop("skill_idf: `skills` must be a data.frame.", call. = FALSE)
  }
  check_columns(skills, c(id_col, skill_col), caller = "skill_idf")
  s <- data.table::data.table(
    id = skills[[id_col]],
    skill = as.character(skills[[skill_col]])
  )
  s <- s[!is.na(id) & !is.na(skill)]
  if (!is.null(ids)) {
    s <- s[id %in% ids]
  }
  s <- unique(s)
  if (!nrow(s)) {
    stop("skill_idf: no posting with a skill.", call. = FALSE)
  }
  n_tot <- data.table::uniqueN(s$id)
  df <- s[, list(n = .N), by = skill]
  data.table::setorderv(df, "skill")
  out <- stats::setNames(log(n_tot / df$n), df$skill)
  attr(out, "n_postings") <- n_tot
  out
}

# 3. skill_profiles() -----

#' Skill coverage profiles of occupation codes
#'
#' Builds one profile per occupation code: for every skill, the share of the
#' code's postings that mention it, optionally weighted by the inverse
#' document frequency of the skill. The profiles are returned as a sparse
#' code-by-skill matrix.
#'
#' @param postings A `data.frame` with one row per posting and the columns
#'   `id_col` and `code_col`. Postings whose code is missing (`NA` or a value
#'   in `code_na`) are ignored.
#' @param skills A `data.frame` with columns `id_col` and `skill_col`, one row
#'   per posting and skill (duplicates are ignored).
#' @param code_col Name of the occupation code column of `postings`.
#' @param id_col Name of the posting id column. Default `"general_id"`.
#' @param skill_col Name of the skill column of `skills`. Default
#'   `"idescoskill_level_3"`.
#' @param weighting `"idf"` (default) multiplies each share by the IDF of the
#'   skill; `"none"` keeps the plain shares.
#' @param min_postings Minimum number of postings with at least one skill for
#'   a code to get a row. Default 1.
#' @param code_na Values of `code_col` that mean "no code". Default
#'   `c("", "Unclassifiable")`.
#' @param idf Optional named numeric vector of IDF weights (from
#'   [skill_idf()]). When supplied it also fixes the skill columns, so that
#'   profiles of different classifications are comparable; skills absent
#'   from it are dropped. When `NULL` and `weighting = "idf"`, the IDF is
#'   computed on the postings with a code.
#' @return A sparse matrix (`dgCMatrix`) with one row per code with at least
#'   `min_postings` postings (sorted by code) and one column per skill. The
#'   attributes are `n_postings` (named integer: postings with at least one
#'   skill, for every code seen, including those below `min_postings`),
#'   `idf` (the weights used, or `NULL`) and `weighting`.
#'
#' @details The denominator of the share is the number of the code's
#'   postings with at least one skill: postings without skills carry no
#'   information on the profile. With `weighting = "idf"` the entry for code
#'   \eqn{c} and skill \eqn{s} is \eqn{(n_{cs} / n_c) \cdot idf_s}. Each
#'   posting must carry one code: the function stops if an id has two codes.
#'
#'   The matrix attributes are dropped by most `Matrix` operations; read
#'   them before subsetting.
#'
#' @examples
#' postings <- data.table::data.table(
#'   general_id = 1:4,
#'   cp2021_id_level_5 = c("1.1.1.1.1", "1.1.1.1.1", "2.2.2.2.0", "")
#' )
#' skills <- data.table::data.table(
#'   general_id = c(1, 1, 2, 3, 4),
#'   idescoskill_level_3 = c("a", "b", "a", "c", "c")
#' )
#' p <- skill_profiles(postings, skills, "cp2021_id_level_5")
#' as.matrix(p)
#' attr(p, "n_postings")
#' @export
skill_profiles <- function(
  postings,
  skills,
  code_col,
  id_col = "general_id",
  skill_col = "idescoskill_level_3",
  weighting = c("idf", "none"),
  min_postings = 1L,
  code_na = c("", "Unclassifiable"),
  idf = NULL
) {
  caller <- "skill_profiles"
  weighting <- match.arg(weighting)
  if (!is.data.frame(postings) || !is.data.frame(skills)) {
    stop(
      caller,
      ": `postings` and `skills` must be data.frames.",
      call. = FALSE
    )
  }
  stopifnot(is.character(code_col), length(code_col) == 1L)
  check_columns(postings, c(id_col, code_col), caller = caller)
  check_columns(skills, c(id_col, skill_col), caller = caller)
  min_postings <- .ref_count_arg(min_postings, "min_postings", caller)
  if (!is.null(idf)) {
    if (!is.numeric(idf) || is.null(names(idf)) || anyDuplicated(names(idf))) {
      stop(
        caller,
        ": `idf` must be a numeric vector with unique skill names.",
        call. = FALSE
      )
    }
  }

  # 3a. Postings with a code -----
  p <- data.table::data.table(
    id = postings[[id_col]],
    code = .ref_clean_code(postings[[code_col]], code_na)
  )
  p <- unique(p[!is.na(id) & !is.na(code)])
  if (anyDuplicated(p$id)) {
    stop(
      caller,
      ": some postings carry more than one code in `",
      code_col,
      "`.",
      call. = FALSE
    )
  }

  # 3b. Skills of those postings -----
  s <- data.table::data.table(
    id = skills[[id_col]],
    skill = as.character(skills[[skill_col]])
  )
  s <- unique(s[!is.na(id) & !is.na(skill)])
  ps <- s[p, on = "id", nomatch = NULL]

  if (weighting == "idf" && is.null(idf)) {
    idf <- if (nrow(ps)) {
      skill_idf(ps, id_col = "id", skill_col = "skill")
    } else {
      stats::setNames(numeric(), character())
    }
  }
  cols <- if (!is.null(idf)) {
    names(idf)
  } else {
    sort(unique(ps$skill), method = "radix")
  }
  w <- if (weighting == "idf") {
    as.numeric(idf)
  } else {
    rep(1, length(cols))
  }

  # 3c. Counts and shares -----
  n_code <- ps[, list(n_skill_postings = data.table::uniqueN(id)), by = code]
  data.table::setorderv(n_code, "code")
  n_all <- stats::setNames(n_code$n_skill_postings, n_code$code)
  keep <- n_code[n_skill_postings >= min_postings, code]

  cnt <- ps[
    code %chin% keep & skill %chin% cols,
    list(n = .N),
    by = list(code, skill)
  ]
  i <- match(cnt$code, keep)
  j <- match(cnt$skill, cols)
  out <- Matrix::sparseMatrix(
    i = i,
    j = j,
    x = cnt$n / as.numeric(n_all[cnt$code]) * w[j],
    dims = c(length(keep), length(cols)),
    dimnames = list(keep, cols)
  )
    attr(out, "n_postings") <- n_all
  attr(out, "idf") <- if (weighting == "idf") idf else NULL
  attr(out, "weighting") <- weighting
  out
}

# 4. build_reference_professions() -----

#' Reference professions of one classification in another
#'
#' For every code of an official universe of the starting classification
#' (`from_col`), names a reference profession of the arrival classification
#' (`to_col`): the most frequent arrival code among the postings that carry
#' both codes when there are at least `n_min` of them, otherwise the arrival
#' code with the most similar skill profile, otherwise none. One direction per
#' call: swap `from_col` and `to_col` for the other.
#'
#' @param postings A `data.frame` with one row per posting and the columns
#'   `from_col`, `to_col`, `strata_col` (when used) and, when the profiles are
#'   not supplied, `id_col`. Only codes observed on the postings are used:
#'   the function never imputes a code.
#' @param skills A `data.frame` of posting skills (columns `id_col` and
#'   `skill_col`), used only when `profiles_from` or `profiles_to` is `NULL`.
#' @param from_col,to_col Names of the starting and arrival code columns.
#' @param universe_from,universe_to `data.frame`s of the official codes of
#'   the two classifications, with column `code` and optional `label` and
#'   `parent`. The output has one row per code of `universe_from`; similarity
#'   references are chosen among the codes of `universe_to`.
#' @param n_min Minimum number of postings with both codes for the frequency
#'   rule. Calibrate it with [calibrate_reference_threshold()].
#' @param min_postings Minimum number of postings with at least one skill for
#'   a profile to be usable. Default 30.
#' @param strata_col Optional stratum column (typically `"source"`), passed to
#'   [build_esco_cp_crosswalk()] to flag modes that depend on which postings
#'   were coded (`modal_stable`).
#' @param id_col,skill_col Posting id and skill column names.
#' @param from_na,to_na Values of the two code columns that mean "no code".
#'   Default `c("", "Unclassifiable")`.
#' @param weighting Profile weighting, passed to [skill_profiles()].
#' @param profiles_from,profiles_to Optional precomputed profile matrices from
#'   [skill_profiles()] for the two classifications, on the same skill
#'   weighting. When `NULL` they are computed from `postings` and `skills`
#'   with an IDF shared by both, computed on all postings of `postings`.
#'
#' @return A `data.table`, one row per code of `universe_from`, sorted by
#'   code, with columns:
#'   \describe{
#'     \item{code, label, parent}{The starting code, its label and parent.}
#'     \item{method}{`"frequenza"`, `"similarità"` or `"nessuno"`.}
#'     \item{reason}{Why the method was chosen: `"annunci_in_comune"`
#'       (frequency), `"annunci_in_comune_sotto_soglia"` or
#'       `"nessun_annuncio_in_comune"` (similarity), `"nessun_annuncio"`,
#'       `"profilo_insufficiente"` or `"nessun_candidato"` (none).}
#'     \item{ref_code, ref_label, ref_parent}{The reference profession.}
#'     \item{ref_in_universe}{Whether `ref_code` belongs to `universe_to`
#'       (a frequency reference may be a code observed on postings but absent
#'       from the universe).}
#'     \item{n_postings}{Postings with the starting code.}
#'     \item{n_cocoded}{Postings with the starting code and an arrival code.}
#'     \item{code_modal, modal_share, n_eff, modal_tied, modal_reliable,
#'       modal_stable}{From [build_esco_cp_crosswalk()]: the most frequent
#'       arrival code, its share, the inverse-Simpson number of candidates,
#'       whether the mode is tied (resolved on the code), and the
#'       reliability and stability flags.}
#'     \item{n_profile}{Postings with at least one skill behind the starting
#'       code's profile.}
#'     \item{cosine_ref}{Cosine between the profiles of the code and of its
#'       reference, when both exist.}
#'     \item{sim_code, sim_cosine, sim2_code, sim2_cosine, sim_tied}{Best and
#'       second-best arrival code by cosine, for every code with a usable
#'       profile, whatever the method.}
#'   }
#'   Attributes `n_min` and `min_postings` record the thresholds.
#'
#' @details **Frequency.** The mode is computed by [build_esco_cp_crosswalk()]
#'   with `from_col` as restrictor and `to_col` as target, after the missing
#'   markers of both columns are set to `NA`: ties are resolved on the code
#'   and flagged in `modal_tied`. The frequency reference is the most typical
#'   arrival profession of the code, not an equivalence: about one posting in
#'   four with an ESCO level-5 code carries a CP5 code other than the mode.
#'
#'   **Similarity.** Profiles are coverage shares weighted by IDF
#'   ([skill_profiles()]); the reference is the arrival code of the universe
#'   with a usable profile and the largest cosine. When the two profiles are
#'   built on overlapping postings, the cosine between a code and the
#'   arrival code that codes the same postings is inflated.
#'
#' @examples
#' postings <- data.table::data.table(
#'   general_id = 1:8,
#'   cp = c(rep("1.1.1.1.0", 4), rep("2.2.2.2.0", 3), ""),
#'   esco = c("1000.1", "1000.1", "1000.1", "1000.2",
#'            "2000.1", "2000.1", "Unclassifiable", "2000.2")
#' )
#' skills <- data.table::data.table(
#'   general_id = rep(1:8, each = 2),
#'   idescoskill_level_3 = c("a", "b", "a", "b", "a", "c", "a", "b",
#'                           "d", "e", "d", "e", "d", "f", "d", "e")
#' )
#' ucp <- data.table::data.table(
#'   code = c("1.1.1.1.0", "2.2.2.2.0", "3.3.3.3.0"),
#'   parent = c("1.1.1.1", "2.2.2.2", "3.3.3.3")
#' )
#' uesco <- data.table::data.table(
#'   code = c("1000.1", "1000.2", "2000.1", "2000.2"),
#'   parent = c("1000", "1000", "2000", "2000")
#' )
#' build_reference_professions(
#'   postings, skills, "cp", "esco", ucp, uesco,
#'   n_min = 3, min_postings = 1
#' )
#' @export
build_reference_professions <- function(
  postings,
  skills = NULL,
  from_col,
  to_col,
  universe_from,
  universe_to,
  n_min,
  min_postings = 30L,
  strata_col = NULL,
  id_col = "general_id",
  skill_col = "idescoskill_level_3",
  from_na = c("", "Unclassifiable"),
  to_na = c("", "Unclassifiable"),
  weighting = c("idf", "none"),
  profiles_from = NULL,
  profiles_to = NULL
) {
  caller <- "build_reference_professions"
  weighting <- match.arg(weighting)
  if (!is.data.frame(postings)) {
    stop(caller, ": `postings` must be a data.frame.", call. = FALSE)
  }
  stopifnot(
    is.character(from_col),
    length(from_col) == 1L,
    is.character(to_col),
    length(to_col) == 1L
  )
  if (identical(from_col, to_col)) {
    stop(caller, ": `from_col` and `to_col` must differ.", call. = FALSE)
  }
  check_columns(postings, c(from_col, to_col, strata_col), caller = caller)
  n_min <- .ref_count_arg(n_min, "n_min", caller)
  min_postings <- .ref_count_arg(min_postings, "min_postings", caller)
  u_from <- .ref_universe(universe_from, "universe_from", caller)
  u_to <- .ref_universe(universe_to, "universe_to", caller)
  need_profiles <- is.null(profiles_from) || is.null(profiles_to)
  if (need_profiles) {
    if (is.null(skills)) {
      stop(
        caller,
        ": `skills` is required when the profiles are not supplied.",
        call. = FALSE
      )
    }
    check_columns(postings, id_col, caller = caller)
  }

  # 4a. Postings with missing markers set to NA -----
  pp <- data.table::data.table(
    from = .ref_clean_code(postings[[from_col]], from_na),
    to = .ref_clean_code(postings[[to_col]], to_na)
  )
  if (!is.null(strata_col)) {
    pp[, stratum := as.character(postings[[strata_col]])]
  }

  # 4b. Frequency: the de facto crosswalk, reused unchanged -----
  cw <- build_esco_cp_crosswalk(
    pp,
    restrictor_col = "from",
    target_col = "to",
    strata_col = if (!is.null(strata_col)) "stratum",
    restrictor_na = character(),
    verbose = FALSE
  )
  grp <- cw$groups[, list(
    code = from,
    n_postings = as.integer(n_postings),
    n_cocoded = as.integer(n_labelled),
    code_modal,
    modal_share,
    n_eff,
    modal_tied,
    modal_reliable,
    modal_stable
  )]

  # 4c. Profiles with a shared weighting -----
  if (need_profiles) {
    ids <- postings[[id_col]]
    idf <- if (weighting == "idf") {
      skill_idf(skills, id_col = id_col, skill_col = skill_col, ids = ids)
    }
    if (is.null(profiles_from)) {
      profiles_from <- skill_profiles(
        postings,
        skills,
        from_col,
        id_col = id_col,
        skill_col = skill_col,
        weighting = weighting,
        code_na = from_na,
        idf = idf
      )
    }
    if (is.null(profiles_to)) {
      profiles_to <- skill_profiles(
        postings,
        skills,
        to_col,
        id_col = id_col,
        skill_col = skill_col,
        weighting = weighting,
        code_na = to_na,
        idf = idf
      )
    }
  }
  np_from <- attr(profiles_from, "n_postings")
  use_from <- intersect(
    .ref_usable_rows(profiles_from, min_postings),
    u_from$code
  )
  use_to <- intersect(.ref_usable_rows(profiles_to, min_postings), u_to$code)

  # 4d. Similarity among usable arrival profiles of the universe -----
  sim <- .ref_cosine_top2(
    profiles_from[use_from, , drop = FALSE],
    profiles_to[use_to, , drop = FALSE]
  )

  # 4e. One row per code of the universe -----
  out <- merge(u_from, grp, by = "code", all.x = TRUE, sort = FALSE)
  out <- merge(out, sim, by = "code", all.x = TRUE, sort = FALSE)
  out[is.na(n_postings), n_postings := 0L]
  out[is.na(n_cocoded), n_cocoded := 0L]
  out[is.na(modal_tied), modal_tied := FALSE]
  out[is.na(modal_reliable), modal_reliable := FALSE]
  out[,
    n_profile := if (is.null(np_from)) {
      NA_integer_
    } else {
      as.integer(np_from[code])
    }
  ]
  out[is.na(n_profile) & !is.null(np_from), n_profile := 0L]
  out[, usable := code %chin% use_from]

  m <- .ref_methods()
  out[,
    method := data.table::fcase(
      n_cocoded >= n_min & !is.na(code_modal) , m[1L] ,
      usable & !is.na(sim_code)               , m[2L] ,
      default = m[3L]
    )
  ]
  out[,
    reason := data.table::fcase(
      method == m[1L]                  , "annunci_in_comune"              ,
      method == m[2L] & n_cocoded > 0L , "annunci_in_comune_sotto_soglia" ,
      method == m[2L]                  , "nessun_annuncio_in_comune"      ,
      n_postings == 0L                 , "nessun_annuncio"                ,
      !usable                          , "profilo_insufficiente"          ,
      default = "nessun_candidato"
    )
  ]
  out[,
    ref_code := data.table::fcase(
      method == m[1L] , code_modal ,
      method == m[2L] , sim_code   ,
      default = NA_character_
    )
  ]
  out[, ref_in_universe := !is.na(ref_code) & ref_code %chin% u_to$code]
  out[, ref_label := u_to$label[match(ref_code, u_to$code)]]
  out[, ref_parent := u_to$parent[match(ref_code, u_to$code)]]
  out[,
    cosine_ref := .ref_cosine_pairs(
      profiles_from,
      profiles_to,
      code,
      ref_code
    )
  ]
  out[, usable := NULL]

  data.table::setcolorder(
    out,
    c(
      "code",
      "label",
      "parent",
      "method",
      "reason",
      "ref_code",
      "ref_label",
      "ref_parent",
      "ref_in_universe",
      "n_postings",
      "n_cocoded",
      "code_modal",
      "modal_share",
      "n_eff",
      "modal_tied",
      "modal_reliable",
      "modal_stable",
      "n_profile",
      "cosine_ref",
      "sim_code",
      "sim_cosine",
      "sim2_code",
      "sim2_cosine",
      "sim_tied"
    )
  )
  data.table::setorderv(out, "code")
  data.table::setattr(out, "n_min", n_min)
  data.table::setattr(out, "min_postings", min_postings)
  out[]
}

# 5. calibrate_reference_threshold() -----

#' Calibrate the frequency threshold of the reference professions
#'
#' Measures how many doubly-coded postings the frequency rule needs to beat
#' the similarity rule. On the codes with at least `n_large` postings with
#' both codes, it draws subsamples of `k` of those postings and records how
#' often the mode of the subsample equals the mode of all of them; the
#' threshold is the smallest `k` whose agreement exceeds the agreement of the
#' similarity reference with the full mode on the same codes.
#'
#' @param postings A `data.frame` with one row per posting and the columns
#'   `from_col` and `to_col`.
#' @param from_col,to_col Names of the starting and arrival code columns.
#' @param sim_reference A `data.frame` with columns `code` and `sim_code`
#'   (for example the output of [build_reference_professions()]): the
#'   similarity reference of each starting code.
#' @param k_grid Integer vector of subsample sizes. Default
#'   `c(1, 2, 3, 5, 10, 20, 50)`.
#' @param n_large Minimum number of doubly-coded postings for a code to enter
#'   the calibration. Default 200.
#' @param n_rep Number of subsamples per code and size. Default 100.
#' @param seed Integer seed of the subsampling. The global RNG state is
#'   restored on exit.
#' @param from_na,to_na Values of the two code columns that mean "no code".
#'
#' @return A list with:
#'   \describe{
#'     \item{curve}{`data.table` with `k`, `agreement` (mean over codes of the
#'       share of subsamples whose mode equals the full mode),
#'       `agreement_weighted` (the same, weighted by doubly-coded postings)
#'       and `n_codes`.}
#'     \item{similarity_agreement}{Share of the calibration codes whose
#'       similarity reference equals the full mode.}
#'     \item{n_min}{The smallest `k` of `k_grid` with `agreement` strictly
#'       above `similarity_agreement`, or `NA` if none.}
#'     \item{codes}{`data.table` with one row per code with at least
#'       `n_large` doubly-coded postings: `code`, `n_cocoded`, `code_modal`,
#'       `sim_code`, `included` (it has a similarity reference) and the
#'       per-size agreements in columns `k_<k>`.}
#'   }
#'
#' @details Modes are computed as in [build_esco_cp_crosswalk()]: ties are
#'   resolved on the code (byte order), both for the full mode and for the
#'   subsample modes, so the calibration measures the rule as it is applied.
#'   Subsamples are drawn without replacement. Codes with at least `n_large`
#'   doubly-coded postings but no similarity reference are listed in `codes`
#'   with `included = FALSE` and do not enter either agreement. The
#'   comparison favours neither rule on the codes it uses; it does not
#'   measure the agreement of the two rules on the codes with few postings,
#'   where the threshold is applied.
#'
#' @examples
#' set.seed(1)
#' postings <- data.table::data.table(
#'   from = rep(c("A", "B"), each = 300),
#'   to = c(sample(c("x", "y"), 300, TRUE, c(0.8, 0.2)),
#'          sample(c("y", "z"), 300, TRUE, c(0.6, 0.4)))
#' )
#' sim <- data.table::data.table(code = c("A", "B"), sim_code = c("x", "z"))
#' calibrate_reference_threshold(postings, "from", "to", sim, n_rep = 20)
#' @export
calibrate_reference_threshold <- function(
  postings,
  from_col,
  to_col,
  sim_reference,
  k_grid = c(1L, 2L, 3L, 5L, 10L, 20L, 50L),
  n_large = 200L,
  n_rep = 100L,
  seed = 1L,
  from_na = c("", "Unclassifiable"),
  to_na = c("", "Unclassifiable")
) {
  caller <- "calibrate_reference_threshold"
  if (!is.data.frame(postings)) {
    stop(caller, ": `postings` must be a data.frame.", call. = FALSE)
  }
  check_columns(postings, c(from_col, to_col), caller = caller)
  if (!is.data.frame(sim_reference)) {
    stop(caller, ": `sim_reference` must be a data.frame.", call. = FALSE)
  }
  check_columns(sim_reference, c("code", "sim_code"), caller = caller)
  if (
    !is.numeric(k_grid) ||
      !length(k_grid) ||
      anyNA(k_grid) ||
      any(k_grid < 1) ||
      any(k_grid != round(k_grid))
  ) {
    stop(caller, ": `k_grid` must be positive integers.", call. = FALSE)
  }
  k_grid <- sort(unique(as.integer(k_grid)))
  n_large <- .ref_count_arg(n_large, "n_large", caller)
  n_rep <- .ref_count_arg(n_rep, "n_rep", caller)
  seed <- .ref_count_arg(seed, "seed", caller, min = 0L)
  if (max(k_grid) > n_large) {
    stop(caller, ": `k_grid` must not exceed `n_large`.", call. = FALSE)
  }

  # 5a. Doubly-coded postings of the large codes -----
  d <- data.table::data.table(
    code = .ref_clean_code(postings[[from_col]], from_na),
    target = .ref_clean_code(postings[[to_col]], to_na)
  )
  d <- d[!is.na(code) & !is.na(target)]
  d[, n_cocoded := .N, by = code]
  d <- d[n_cocoded >= n_large]
  data.table::setorderv(d, c("code", "target"))

  sr <- data.table::data.table(
    code = as.character(sim_reference[["code"]]),
    sim_code = as.character(sim_reference[["sim_code"]])
  )
  sr <- unique(sr, by = "code")

  codes <- unique(d[, list(code, n_cocoded)])
  if (!nrow(codes)) {
    stop(
      caller,
      ": no code has at least `n_large` doubly-coded postings.",
      call. = FALSE
    )
  }

  # 5b. Full modes and subsample agreement, code by code -----
  # Within a code the targets are coded 1..m in byte order, so which.max()
  # on the tabulated counts resolves ties on the code, as the crosswalk does.
  split_t <- split(d$target, d$code)
  res <- .ref_with_seed(seed, {
    lapply(names(split_t), function(cc) {
      x <- split_t[[cc]]
      lev <- sort(unique(x), method = "radix")
      xi <- match(x, lev)
      m <- length(lev)
      full <- which.max(tabulate(xi, nbins = m))
      agr <- vapply(
        k_grid,
        function(k) {
          hits <- vapply(
            seq_len(n_rep),
            function(r) {
              which.max(tabulate(xi[sample.int(length(xi), k)], nbins = m)) ==
                full
            },
            logical(1)
          )
          mean(hits)
        },
        numeric(1)
      )
      list(code = cc, code_modal = lev[full], agr = agr)
    })
  })
  codes <- data.table::data.table(
    code = vapply(res, `[[`, character(1), "code"),
    code_modal = vapply(res, `[[`, character(1), "code_modal")
  )
  codes[, n_cocoded := as.integer(lengths(split_t)[code])]
  agr <- do.call(rbind, lapply(res, `[[`, "agr"))
  colnames(agr) <- paste0("k_", k_grid)
  codes <- cbind(codes, data.table::as.data.table(agr))
  codes[, sim_code := sr$sim_code[match(code, sr$code)]]
  codes[, included := !is.na(sim_code)]
  data.table::setcolorder(
    codes,
    c("code", "n_cocoded", "code_modal", "sim_code", "included")
  )

  # 5c. Curve, similarity agreement, threshold -----
  inc <- codes$included
  if (!any(inc)) {
    stop(
      caller,
      ": no calibration code has a similarity reference.",
      call. = FALSE
    )
  }
  w <- codes$n_cocoded[inc]
  curve <- data.table::data.table(
    k = k_grid,
    agreement = colMeans(agr[inc, , drop = FALSE]),
    agreement_weighted = colSums(agr[inc, , drop = FALSE] * w) / sum(w),
    n_codes = sum(inc)
  )
  sim_agr <- mean(codes$sim_code[inc] == codes$code_modal[inc])
  above <- curve$k[curve$agreement > sim_agr]
  n_min <- if (length(above)) min(above) else NA_integer_

  list(
    curve = curve[],
    similarity_agreement = sim_agr,
    n_min = n_min,
    codes = codes[]
  )
}

# 6. transfer_skill_profile() -----

#' Transfer skill profiles through reference professions
#'
#' Gives each code the skill profile of its reference profession in the
#' other classification.
#'
#' @param profiles_from Profile matrix of the classification the profiles come
#'   from (rows named by its codes), as returned by [skill_profiles()].
#' @param reference A `data.frame` with the arrival codes and their
#'   references (for example the output of [build_reference_professions()]),
#'   where the reference codes are codes of `profiles_from`.
#' @param code_col,ref_col Column names of the arrival code and of the
#'   reference code in `reference`. Default `"code"` and `"ref_code"`.
#' @return A sparse matrix with one row per code of `reference` whose
#'   reference has a profile in `profiles_from` (sorted by code), the columns
#'   of `profiles_from`, and attribute `ref_code` (named character vector:
#'   the reference of each row).
#' @examples
#' profiles <- Matrix::Matrix(
#'   c(0.5, 0, 0.2, 1), nrow = 2, sparse = TRUE,
#'   dimnames = list(c("1000.1", "2000.1"), c("a", "b"))
#' )
#' reference <- data.frame(
#'   code = c("1.1.1.1.0", "2.2.2.2.0", "3.3.3.3.0"),
#'   ref_code = c("1000.1", "2000.1", NA)
#' )
#' as.matrix(transfer_skill_profile(profiles, reference))
#' @export
transfer_skill_profile <- function(
  profiles_from,
  reference,
  code_col = "code",
  ref_col = "ref_code"
) {
  caller <- "transfer_skill_profile"
  if (!inherits(profiles_from, "Matrix") && !is.matrix(profiles_from)) {
    stop(caller, ": `profiles_from` must be a matrix.", call. = FALSE)
  }
  if (is.null(rownames(profiles_from))) {
    stop(caller, ": `profiles_from` must have row names.", call. = FALSE)
  }
  if (!is.data.frame(reference)) {
    stop(caller, ": `reference` must be a data.frame.", call. = FALSE)
  }
  check_columns(reference, c(code_col, ref_col), caller = caller)
  r <- data.table::data.table(
    code = as.character(reference[[code_col]]),
    ref_code = as.character(reference[[ref_col]])
  )
  if (anyNA(r$code) || anyDuplicated(r$code)) {
    stop(
      caller,
      ": the codes of `reference` must be unique and not missing.",
      call. = FALSE
    )
  }
  r <- r[!is.na(ref_code) & ref_code %chin% rownames(profiles_from)]
  data.table::setorderv(r, "code")
  out <- .ref_as_csparse(
    .ref_as_csparse(profiles_from)[r$ref_code, , drop = FALSE]
  )
  rownames(out) <- r$code
  attr(out, "ref_code") <- stats::setNames(r$ref_code, r$code)
  out
}

# 7. validate_profile_transfer() -----

#' Validate transferred skill profiles against observed ones
#'
#' For the codes that have both a transferred and an observed profile,
#' measures the cosine between the two and compares it with two baselines
#' built from the code's own classification: the profile of the parent group
#' without the code, and the profile of a random code of the same parent.
#'
#' @param transferred Profile matrix from [transfer_skill_profile()].
#' @param observed Profile matrix of the same codes built from their own
#'   postings, from [skill_profiles()], on the same weighting.
#' @param parent A `data.frame` with columns `code` and `parent`: the parent
#'   group of each code of `observed`.
#' @param n_postings Named numeric vector of the postings behind each row of
#'   `observed`, used to weight the parent profile. Default: the `n_postings`
#'   attribute of `observed`; when absent, each code weighs 1.
#' @param seed Integer seed for the choice of the random sibling. The global
#'   RNG state is restored on exit.
#'
#' @return A list with:
#'   \describe{
#'     \item{codes}{`data.table` with one row per code with both profiles:
#'       `code`, `parent`, `cosine_transfer`, `cosine_parent` (parent group
#'       without the code), `sibling_code` and `cosine_sibling`.}
#'     \item{summary}{`data.table` with one row per measure (`transfer`,
#'       `parent`, `sibling`): `n` (codes with the measure), `mean` and
#'       `median`, and `n_common`, `mean_common` on the codes where all three
#'       measures exist.}
#'     \item{share_better}{Shares of the common codes where the transferred
#'       profile is closer than the parent baseline (`parent`) and than the
#'       sibling baseline (`sibling`).}
#'   }
#'
#' @details The parent baseline of code \eqn{c} in group \eqn{g} is
#'   \eqn{\sum_{i \in g, i \ne c} w_i x_i / \sum_{i \in g, i \ne c} w_i},
#'   the profile of the other codes of the group pooled by their postings;
#'   leaving the code out keeps its own postings from the baseline. Codes
#'   alone in their group have no baseline (`NA`). The baselines are
#'   reported for comparison, not as acceptance thresholds.
#'
#' @examples
#' observed <- Matrix::Matrix(
#'   rbind(c(1, 0, 0), c(0.9, 0.1, 0), c(0, 0, 1)),
#'   sparse = TRUE,
#'   dimnames = list(c("A1", "A2", "B1"), c("s1", "s2", "s3"))
#' )
#' transferred <- observed[c("A1", "A2"), ]
#' parent <- data.frame(code = c("A1", "A2", "B1"), parent = c("A", "A", "B"))
#' validate_profile_transfer(transferred, observed, parent)$summary
#' @export
validate_profile_transfer <- function(
  transferred,
  observed,
  parent,
  n_postings = attr(observed, "n_postings"),
  seed = 1L
) {
  caller <- "validate_profile_transfer"
  for (nm in c("transferred", "observed")) {
    m <- get(nm)
    if (!inherits(m, "Matrix") && !is.matrix(m)) {
      stop(caller, ": `", nm, "` must be a matrix.", call. = FALSE)
    }
    if (is.null(rownames(m)) || is.null(colnames(m))) {
      stop(
        caller,
        ": `",
        nm,
        "` must have row and column names.",
        call. = FALSE
      )
    }
  }
  if (!is.data.frame(parent)) {
    stop(caller, ": `parent` must be a data.frame.", call. = FALSE)
  }
  check_columns(parent, c("code", "parent"), caller = caller)
  seed <- .ref_count_arg(seed, "seed", caller, min = 0L)

  transferred <- .ref_as_csparse(transferred)
  observed <- .ref_as_csparse(observed)
  al <- .ref_align_cols(observed, transferred)
  obs <- al$a
  trf <- al$b

  par <- data.table::data.table(
    code = as.character(parent[["code"]]),
    parent = as.character(parent[["parent"]])
  )
  par <- unique(par, by = "code")
  obs_par <- par$parent[match(rownames(obs), par$code)]
  w <- if (is.null(n_postings)) {
    rep(1, nrow(obs))
  } else {
    as.numeric(n_postings[rownames(obs)])
  }
  w[is.na(w)] <- 0

  codes <- sort(intersect(rownames(trf), rownames(obs)), method = "radix")
  out <- data.table::data.table(
    code = codes,
    parent = obs_par[match(codes, rownames(obs))]
  )

  # 7a. Transferred against observed -----
  out[, cosine_transfer := .ref_cosine_pairs(trf, obs, code, code)]

  # 7b. Parent group without the code -----
  has_par <- !is.na(obs_par)
  if (any(has_par)) {
    g_lev <- sort(unique(obs_par[has_par]), method = "radix")
    gi <- match(obs_par, g_lev)
    ind <- Matrix::sparseMatrix(
      i = gi[has_par],
      j = which(has_par),
      x = w[has_par],
      dims = c(length(g_lev), nrow(obs))
    )
    g_sum <- ind %*% obs
    g_w <- as.numeric(ind %*% rep(1, nrow(obs)))
    io <- match(out$code, rownames(obs))
    ig <- gi[io]
    ok <- which(!is.na(ig) & (g_w[ig] - w[io]) > 0)
    cos_par <- rep(NA_real_, nrow(out))
    if (length(ok)) {
      loo <- g_sum[ig[ok], , drop = FALSE] -
        Matrix::Diagonal(x = w[io[ok]]) %*% obs[io[ok], , drop = FALSE]
      rownames(loo) <- out$code[ok]
      cos_par[ok] <- .ref_cosine_pairs(
        .ref_as_csparse(loo),
        obs,
        out$code[ok],
        out$code[ok]
      )
    }
    out[, cosine_parent := cos_par]
  } else {
    out[, cosine_parent := NA_real_]
  }

  # 7c. A random code of the same parent -----
  obs_dt <- data.table::data.table(code = rownames(obs), parent = obs_par)
  sib <- .ref_with_seed(seed, {
    vapply(
      seq_len(nrow(out)),
      function(i) {
        p <- out$parent[i]
        if (is.na(p)) {
          return(NA_character_)
        }
        cand <- obs_dt$code[
          !is.na(obs_dt$parent) &
            obs_dt$parent == p &
            obs_dt$code != out$code[i]
        ]
        if (!length(cand)) {
          return(NA_character_)
        }
        cand[sample.int(length(cand), 1L)]
      },
      character(1)
    )
  })
  out[, sibling_code := sib]
  out[, cosine_sibling := .ref_cosine_pairs(obs, obs, sibling_code, code)]

  # 7d. Summary -----
  common <- stats::complete.cases(out[, list(
    cosine_transfer,
    cosine_parent,
    cosine_sibling
  )])
  meas <- list(
    transfer = out$cosine_transfer,
    parent = out$cosine_parent,
    sibling = out$cosine_sibling
  )
  summ <- data.table::data.table(
    measure = names(meas),
    n = vapply(meas, function(x) sum(!is.na(x)), integer(1)),
    mean = vapply(meas, function(x) mean(x, na.rm = TRUE), numeric(1)),
    median = vapply(
      meas,
      function(x) stats::median(x, na.rm = TRUE),
      numeric(1)
    ),
    n_common = sum(common),
    mean_common = vapply(meas, function(x) mean(x[common]), numeric(1))
  )
  share_better <- c(
    parent = mean(out$cosine_transfer[common] > out$cosine_parent[common]),
    sibling = mean(out$cosine_transfer[common] > out$cosine_sibling[common])
  )

  list(codes = out[], summary = summ[], share_better = share_better)
}

# 8. group_mixture_weights() -----

#' Mixture weights from the doubly-coded postings of a parent group
#'
#' For every code of the starting classification, the distribution of the
#' arrival codes among the doubly-coded postings of the code's whole parent
#' group, optionally leaving out the code's own postings. The weights feed
#' [transfer_skill_profile_mixture()].
#'
#' @param postings A `data.frame` with one row per posting and the columns
#'   `from_col` and `to_col`.
#' @param from_col,to_col Names of the starting and arrival code columns.
#' @param group A `data.frame` with columns `code` and `parent`: the parent
#'   group of each starting code (for example the universe of
#'   [build_reference_professions()]). Every code of `group` gets the weights
#'   of its parent; postings whose starting code is not in `group` are
#'   ignored.
#' @param leave_one_out If `TRUE`, the doubly-coded postings of the code
#'   itself are removed from its weights. Default `FALSE`.
#' @param from_na,to_na Values of the two code columns that mean "no code".
#'
#' @return A `data.table` with `code`, `parent`, `ref_code` (arrival code)
#'   and `weight` (number of doubly-coded postings of the group with that
#'   arrival code, minus the code's own with `leave_one_out = TRUE`), one row
#'   per code and arrival code with positive weight, sorted by code and
#'   `ref_code`. Codes whose group has no doubly-coded postings (or, leaving
#'   one out, none besides their own) have no row.
#'
#' @examples
#' postings <- data.table::data.table(
#'   esco = c("1000.1", "1000.1", "1000.2", "2000.1"),
#'   cp = c("1.1.1.1.0", "1.1.1.1.0", "1.1.1.2.0", "")
#' )
#' group <- data.table::data.table(
#'   code = c("1000.1", "1000.2", "2000.1"),
#'   parent = c("1000", "1000", "2000")
#' )
#' group_mixture_weights(postings, "esco", "cp", group)
#' group_mixture_weights(postings, "esco", "cp", group, leave_one_out = TRUE)
#' @export
group_mixture_weights <- function(
  postings,
  from_col,
  to_col,
  group,
  leave_one_out = FALSE,
  from_na = c("", "Unclassifiable"),
  to_na = c("", "Unclassifiable")
) {
  caller <- "group_mixture_weights"
  if (!is.data.frame(postings)) {
    stop(caller, ": `postings` must be a data.frame.", call. = FALSE)
  }
  check_columns(postings, c(from_col, to_col), caller = caller)
  if (!is.data.frame(group)) {
    stop(caller, ": `group` must be a data.frame.", call. = FALSE)
  }
  check_columns(group, c("code", "parent"), caller = caller)
  if (!is.logical(leave_one_out) || length(leave_one_out) != 1L ||
    is.na(leave_one_out)) {
    stop(caller, ": `leave_one_out` must be TRUE or FALSE.", call. = FALSE)
  }
  g <- data.table::data.table(
    code = as.character(group[["code"]]),
    parent = as.character(group[["parent"]])
  )
  if (anyNA(g$code) || anyDuplicated(g$code)) {
    stop(
      caller,
      ": the codes of `group` must be unique and not missing.",
      call. = FALSE
    )
  }
  g <- g[!is.na(parent)]

  # 8a. Doubly-coded postings with their group -----
  d <- data.table::data.table(
    code = .ref_clean_code(postings[[from_col]], from_na),
    ref_code = .ref_clean_code(postings[[to_col]], to_na)
  )
  d <- d[!is.na(code) & !is.na(ref_code)]
  d[, grp_parent := g$parent[match(code, g$code)]]
  d <- d[!is.na(grp_parent)]

  # 8b. Group counts, given to every code of the group -----
  n_g <- d[, list(n_grp = .N), by = list(grp_parent, ref_code)]
  out <- merge(
    g[, list(code, grp_parent = parent)],
    n_g,
    by = "grp_parent",
    allow.cartesian = TRUE
  )
  if (leave_one_out) {
    n_e <- d[, list(n_own = .N), by = list(code, ref_code)]
    out <- merge(out, n_e, by = c("code", "ref_code"), all.x = TRUE)
    out[is.na(n_own), n_own := 0L]
    out[, weight := as.numeric(n_grp - n_own)]
  } else {
    out[, weight := as.numeric(n_grp)]
  }
  out <- out[weight > 0, list(code, parent = grp_parent, ref_code, weight)]
  data.table::setorderv(out, c("code", "ref_code"))
  out[]
}

# 9. transfer_skill_profile_mixture() -----

#' Transfer skill profiles as a weighted mixture of profiles
#'
#' Gives each code a weighted mean of the skill profiles of several codes of
#' the other classification, for example the arrival codes of the
#' doubly-coded postings of its parent group ([group_mixture_weights()]).
#' Only codes with a usable profile enter the mixture, and the weights are
#' renormalised over them.
#'
#' @param profiles_from Profile matrix of the classification the profiles come
#'   from (rows named by its codes), as returned by [skill_profiles()].
#' @param weights A `data.frame` with one row per code and contributing code:
#'   columns `code_col`, `ref_col` and `weight_col` (non-negative).
#' @param code_col,ref_col,weight_col Column names in `weights`. Defaults
#'   `"code"`, `"ref_code"`, `"weight"`.
#' @param usable Row names of `profiles_from` allowed in the mixture.
#'   Default: all rows.
#'
#' @return A sparse matrix with one row per code with a positive covered
#'   weight (sorted by code) and the columns of `profiles_from`. Row \eqn{e}
#'   is \eqn{\sum_{c \in U} w_{ec} x_c / \sum_{c \in U} w_{ec}}, where
#'   \eqn{U} is the set of usable codes. Attribute `coverage` is a
#'   `data.table` with one row per code of `weights`: `weight_total`,
#'   `weight_covered`, `covered_share` (`weight_covered / weight_total`),
#'   `n_refs` and `n_refs_covered`.
#'
#' @examples
#' profiles <- Matrix::Matrix(
#'   rbind(c(1, 0), c(0, 1), c(1, 1)), sparse = TRUE,
#'   dimnames = list(c("1.1.1.1.0", "1.1.1.2.0", "9.9.9.9.0"), c("a", "b"))
#' )
#' weights <- data.frame(
#'   code = c("1000.1", "1000.1", "1000.1"),
#'   ref_code = c("1.1.1.1.0", "1.1.1.2.0", "9.9.9.9.0"),
#'   weight = c(3, 1, 4)
#' )
#' m <- transfer_skill_profile_mixture(
#'   profiles, weights, usable = c("1.1.1.1.0", "1.1.1.2.0")
#' )
#' as.matrix(m)
#' attr(m, "coverage")
#' @export
transfer_skill_profile_mixture <- function(
  profiles_from,
  weights,
  code_col = "code",
  ref_col = "ref_code",
  weight_col = "weight",
  usable = rownames(profiles_from)
) {
  caller <- "transfer_skill_profile_mixture"
  if (!inherits(profiles_from, "Matrix") && !is.matrix(profiles_from)) {
    stop(caller, ": `profiles_from` must be a matrix.", call. = FALSE)
  }
  if (is.null(rownames(profiles_from))) {
    stop(caller, ": `profiles_from` must have row names.", call. = FALSE)
  }
  if (!is.data.frame(weights)) {
    stop(caller, ": `weights` must be a data.frame.", call. = FALSE)
  }
  check_columns(weights, c(code_col, ref_col, weight_col), caller = caller)
  w <- data.table::data.table(
    code = as.character(weights[[code_col]]),
    ref_code = as.character(weights[[ref_col]]),
    weight = as.numeric(weights[[weight_col]])
  )
  if (anyNA(w$code) || anyNA(w$weight) || any(w$weight < 0)) {
    stop(
      caller,
      ": `weights` must have codes and non-negative weights.",
      call. = FALSE
    )
  }
  if (anyDuplicated(w, by = c("code", "ref_code"))) {
    stop(
      caller,
      ": `weights` must have one row per code and reference.",
      call. = FALSE
    )
  }
  usable <- intersect(as.character(usable), rownames(profiles_from))
  w[, covered := !is.na(ref_code) & ref_code %chin% usable & weight > 0]

  # 9a. Coverage of the weights -----
  cov <- w[,
    list(
      weight_total = sum(weight),
      weight_covered = sum(weight[covered]),
      n_refs = sum(weight > 0),
      n_refs_covered = sum(covered)
    ),
    by = code
  ]
  cov[,
    covered_share := data.table::fifelse(
      weight_total > 0,
      weight_covered / weight_total,
      NA_real_
    )
  ]
  data.table::setcolorder(
    cov,
    c("code", "weight_total", "weight_covered", "covered_share")
  )
  data.table::setorderv(cov, "code")

  # 9b. Weighted mean of the usable profiles -----
  wc <- w[covered == TRUE]
  rows <- cov[weight_covered > 0, code]
  refs <- sort(unique(wc$ref_code), method = "radix")
  wc[, weight := weight / sum(weight), by = code]
  wm <- Matrix::sparseMatrix(
    i = match(wc$code, rows),
    j = match(wc$ref_code, refs),
    x = wc$weight,
    dims = c(length(rows), length(refs))
  )
  p <- .ref_as_csparse(profiles_from)[refs, , drop = FALSE]
  out <- .ref_as_csparse(wm %*% p)
  dimnames(out) <- list(rows, colnames(profiles_from))
  attr(out, "coverage") <- cov[]
  out
}
