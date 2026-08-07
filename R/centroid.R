# Static centroid classifier for CP2021 codes -----
#
# predict_cp5_knn() keeps the whole labelled pool at prediction time and pays
# ~98.6% of its runtime searching it. A centroid (Rocchio) model replaces that
# search with one sparse skills-by-class matrix, computed once: prediction
# becomes a sparse product, the model can be versioned and shipped, and the
# training data need not be present when coding runs.
#
# The scoring rule is deliberately of the form
#
#     score(q, c) = prior_weight * log n_c  +  s(q, theta_c)  [+ sector term]
#
# so that when the class profiles collapse onto the group profile the second
# term is constant across candidates and the ranking reduces to log n_c -- which
# IS the modal cascade. That identity is what makes the shrinkage testable, and
# `skillviz_workflow/run_cp5_centroid.R` asserts it at shrink_beta -> infinity
# before reading any accuracy.

# 1. Internal helpers -----

.cp_resolve_na <- function(restrictor_na, restrictor) {
  if (!is.null(restrictor_na)) {
    return(restrictor_na)
  }
  if (identical(restrictor, "idesco_level_5")) {
    "Unclassifiable"
  } else {
    character(0)
  }
}

# Row-normalise to a probability distribution, guarding empty rows.
.cp_rownorm <- function(m) {
  rs <- rowSums(m)
  rs[rs <= 0] <- 1
  m / rs
}

# 2. build_cp_profiles -----

#' Build skill profiles per occupation-code pair
#'
#' Aggregates labelled announcements into one skill distribution per
#' (ESCO group, CP code) pair, the sufficient statistic a centroid classifier
#' needs. The result is a small sparse object that can be saved, versioned and
#' shipped: [predict_cp5_centroid()] scores new announcements against it without
#' seeing the training data again.
#'
#' @param postings A data.table of labelled announcements. Needs `general_id`,
#'   the `restrictor` column, the `target` column, and `idsector` when
#'   `sector = TRUE`. Rows whose target is `NA` or empty are ignored.
#' @param skills A data.table with `general_id` and `skill_col`.
#' @param restrictor Character naming the column that restricts the candidate
#'   space (default `"idesco_level_5"`).
#' @param target Character naming the CP2021 column to learn (default
#'   `"cp2021_id_level_5"`).
#' @param restrictor_na Sentinel values of `restrictor` meaning "no occupation
#'   code", normalised to `NA`. `NULL` (default) resolves to `"Unclassifiable"`
#'   for a level-5 restrictor and to nothing otherwise.
#' @param skill_col Character naming the skill column (default
#'   `"escoskill_level_3"`).
#' @param min_support Integer: candidates supported by fewer than this many
#'   labelled announcements are dropped from their group (default 1, keep all).
#' @param smooth_alpha Numeric additive (Lidstone) smoothing applied to the
#'   skill counts before normalising (default 0.1).
#' @param shrink_beta Numeric shrinkage of each class profile toward its ESCO
#'   group's profile: `theta_c = (n_c * theta_hat_c + beta * pi_e) / (n_c + beta)`
#'   (default 50). `0` disables it; large values collapse every candidate onto
#'   the group profile, which is the identity the harness tests.
#' @param weighting Character vector of the diagonal skill weightings to
#'   precompute, any of `"none"`, `"idf"`, `"balassa"`. Only the ones a distance
#'   actually needs are used at prediction time.
#' @param sector Logical: also learn a smoothed sector distribution per
#'   candidate, scored as a naive-Bayes term (default FALSE). Requires
#'   `idsector` in `postings`.
#' @param verbose Logical: print a one-line summary (default TRUE).
#'
#' @return A list of class `cp_profiles` carrying the pair index, the sparse
#'   skill counts per pair and per group, the class sizes, the skill vocabulary,
#'   the optional sector counts, the requested weight vectors, and the
#'   smoothing parameters. Its size is governed by the number of
#'   (group, code) pairs, not by the number of announcements.
#'
#' @details
#' Profiles are stored as **counts**, not as the smoothed distributions. The
#' smoothing and the shrinkage are applied by [predict_cp5_centroid()] when it
#' materialises a group's block, so one build serves many scoring parameters and
#' the stored object stays sparse.
#'
#' `shrink_beta` interpolates between the class profile and its group's:
#' a class with few announcements is pulled toward the group, one with many is
#' left alone. This is the axis the 2026-03 attempt lacked, and it is also the
#' axis that makes the model falsifiable — see [predict_cp5_centroid()].
#'
#' @examples
#' postings <- data.table::data.table(
#'   general_id = as.character(1:6),
#'   idesco_level_5 = rep(c("1000.1", "2000.1"), each = 3),
#'   cp2021_id_level_5 = c(
#'     "1.1.1.1.1", "1.1.1.1.1", "1.1.1.1.2",
#'     "2.2.2.2.0", "2.2.2.2.0", "2.2.2.2.0"
#'   )
#' )
#' skills <- data.table::data.table(
#'   general_id = as.character(rep(1:6, each = 2)),
#'   escoskill_level_3 = c(
#'     "s1", "s2", "s1", "s2", "s1", "s3",
#'     "s4", "s5", "s4", "s5", "s4", "s6"
#'   )
#' )
#' p <- build_cp_profiles(postings, skills, verbose = FALSE)
#' p$pairs
#'
#' @seealso [predict_cp5_centroid()], and [predict_cp5_knn()] for the
#'   neighbour-search model this one is measured against.
#' @export
build_cp_profiles <- function(
  postings,
  skills,
  restrictor = "idesco_level_5",
  target = "cp2021_id_level_5",
  restrictor_na = NULL,
  skill_col = "escoskill_level_3",
  min_support = 1L,
  smooth_alpha = 0.1,
  shrink_beta = 50,
  weighting = "none",
  sector = FALSE,
  verbose = TRUE
) {
  check_columns(
    postings,
    c("general_id", restrictor, target),
    caller = "build_cp_profiles"
  )
  check_columns(
    skills,
    c("general_id", skill_col),
    caller = "build_cp_profiles"
  )
  weighting <- match.arg(
    weighting,
    c("none", "idf", "balassa"),
    several.ok = TRUE
  )
  if (isTRUE(sector)) {
    check_columns(postings, "idsector", caller = "build_cp_profiles")
  }
  restrictor_na <- .cp_resolve_na(restrictor_na, restrictor)

  # 2a. Labelled training rows, canonically named -----
  keep <- c("general_id", restrictor, target)
  if (isTRUE(sector)) {
    keep <- c(keep, "idsector")
  }
  dt <- data.table::copy(postings)[, keep, with = FALSE]
  data.table::setnames(dt, c(restrictor, target), c("grp", "code"))
  dt[, general_id := as.character(general_id)]
  dt[, grp := as.character(grp)]
  dt[, code := as.character(code)]
  if (length(restrictor_na)) {
    dt[grp %in% restrictor_na, grp := NA_character_]
  }
  dt <- dt[!is.na(grp) & !is.na(code) & nzchar(code)]
  if (nrow(dt) == 0L) {
    stop("build_cp_profiles: no labelled rows with a usable restrictor")
  }

  # 2b. Candidate index, after the support filter -----
  pairs <- dt[, .(n_c = .N), by = .(grp, code)]
  if (min_support > 1L) {
    pairs <- pairs[n_c >= min_support]
  }
  # Ordered by group then code, so that a first-maximum argmax downstream
  # breaks ties on the smaller code -- the same rule the modal cascade uses.
  data.table::setorder(pairs, grp, code)
  pairs[, pair_id := .I]
  if (nrow(pairs) == 0L) {
    stop("build_cp_profiles: min_support removed every candidate")
  }

  groups <- pairs[, .(grp = unique(grp))]
  data.table::setorder(groups, grp)
  groups[, grp_id := .I]
  pairs <- groups[pairs, on = "grp"]
  data.table::setorder(pairs, pair_id)

  dt <- pairs[, .(grp, code, pair_id, grp_id, n_c)][
    dt,
    on = c("grp", "code"),
    nomatch = NULL
  ]

  # 2c. Skills as sparse counts per pair and per group -----
  sk <- data.table::copy(skills)[, c("general_id", skill_col), with = FALSE]
  data.table::setnames(sk, skill_col, "skill")
  sk[, general_id := as.character(general_id)]
  sk <- unique(sk[!is.na(skill)])
  skill_levels <- sort(unique(sk$skill))

  sk <- dt[, .(general_id, pair_id, grp_id)][
    sk,
    on = "general_id",
    nomatch = NULL
  ]
  sk[, sid := match(skill, skill_levels)]

  counts <- Matrix::sparseMatrix(
    i = sk$pair_id,
    j = sk$sid,
    x = 1,
    dims = c(nrow(pairs), length(skill_levels))
  )
  group_counts <- Matrix::sparseMatrix(
    i = sk$grp_id,
    j = sk$sid,
    x = 1,
    dims = c(nrow(groups), length(skill_levels))
  )

  # 2d. Diagonal skill weightings -----
  # Both are per-skill rescalings, applied to query and profile alike, so a
  # weighted cosine is just a cosine in the rescaled space.
  n_docs <- data.table::uniqueN(dt$general_id)
  df <- Matrix::colSums(counts)
  w <- list(none = rep(1, length(skill_levels)))
  if ("idf" %chin% weighting) {
    w$idf <- log((1 + n_docs) / (1 + df))
  }
  if ("balassa" %chin% weighting) {
    # Inverse of the global skill share: a skill that is everywhere carries
    # less signal about which occupation this is.
    p <- df / max(1, sum(df))
    w$balassa <- 1 / pmax(p, .Machine$double.eps)
  }

  # 2e. Optional sector counts -----
  sector_counts <- NULL
  sector_levels <- NULL
  group_sector <- NULL
  if (isTRUE(sector)) {
    dt[, sect := as.character(idsector)]
    dt[is.na(sect), sect := ""]
    sector_levels <- sort(unique(dt$sect))
    dt[, sect_id := match(sect, sector_levels)]
    sector_counts <- Matrix::sparseMatrix(
      i = dt$pair_id,
      j = dt$sect_id,
      x = 1,
      dims = c(nrow(pairs), length(sector_levels))
    )
    group_sector <- Matrix::sparseMatrix(
      i = dt$grp_id,
      j = dt$sect_id,
      x = 1,
      dims = c(nrow(groups), length(sector_levels))
    )
  }

  out <- list(
    pairs = pairs[, .(grp, code, pair_id, grp_id, n_c)],
    groups = groups,
    counts = counts,
    group_counts = group_counts,
    skill_levels = skill_levels,
    weights = w,
    sector_counts = sector_counts,
    group_sector = group_sector,
    sector_levels = sector_levels,
    params = list(
      restrictor = restrictor,
      target = target,
      restrictor_na = restrictor_na,
      skill_col = skill_col,
      min_support = as.integer(min_support),
      smooth_alpha = as.numeric(smooth_alpha),
      shrink_beta = as.numeric(shrink_beta),
      weighting = weighting,
      sector = isTRUE(sector),
      n_docs = n_docs
    )
  )
  class(out) <- c("cp_profiles", "list")

  if (verbose) {
    message(sprintf(
      "build_cp_profiles: %d groups, %d candidate pairs, %d skills, %d labelled rows",
      nrow(groups),
      nrow(pairs),
      length(skill_levels),
      nrow(dt)
    ))
  }
  out
}

# 3. Scoring kernels -----

# Every kernel takes a dense query block Q (n_q x V, rows are the raw binary
# skill vectors) and a dense profile block TH (n_c x V, rows are probability
# distributions) and returns an n_q x n_c matrix of SIMILARITIES: higher is
# better, always, so the caller can argmax without knowing which measure ran.
.cp_kernel <- function(distance, Q, TH, TH_comp = NULL) {
  eps <- .Machine$double.eps
  qs <- rowSums(Q)
  qs[qs <= 0] <- 1
  Qn <- Q / qs

  switch(
    distance,
    multinomial_nb = Q %*% t(log(TH)),
    # The complement form scores how badly the OTHER classes explain the query,
    # which is the more stable statistic when class sizes are very unequal.
    complement_nb = -(Q %*% t(log(TH_comp))),
    bernoulli_nb = {
      # Presence/absence, not emission. The absence term is log(1 - theta),
      # which is ~0 for a rare skill; using a multinomial emission probability
      # here is the defect that sank the 2026-03 attempt.
      lo <- log(pmin(pmax(TH, eps), 1 - eps))
      hi <- log(1 - pmin(pmax(TH, eps), 1 - eps))
      sweep(Q %*% t(lo - hi), 2L, rowSums(hi), "+")
    },
    hellinger = sqrt(Qn) %*% t(sqrt(TH)),
    cosine = {
      qn <- sqrt(rowSums(Q^2))
      qn[qn <= 0] <- 1
      tn <- sqrt(rowSums(TH^2))
      tn[tn <= 0] <- 1
      (Q %*% t(TH)) / outer(qn, tn)
    },
    dice = {
      2 * (Q %*% t(TH)) / outer(rowSums(Q), rowSums(TH), "+")
    },
    l2 = -sqrt(pmax(
      outer(rowSums(Qn^2), rowSums(TH^2), "+") - 2 * (Qn %*% t(TH)),
      0
    )),
    l1 = {
      # No closed form through a product: computed on the union of supports,
      # which is why the caller keeps the group blocks small.
      out <- matrix(0, nrow(Qn), nrow(TH))
      for (k in seq_len(nrow(TH))) {
        out[, k] <- -rowSums(abs(sweep(Qn, 2L, TH[k, ], "-")))
      }
      out
    },
    chisq = {
      out <- matrix(0, nrow(Qn), nrow(TH))
      for (k in seq_len(nrow(TH))) {
        d <- sweep(Qn, 2L, TH[k, ], "-")
        s <- sweep(Qn, 2L, TH[k, ], "+")
        out[, k] <- -rowSums(d^2 / pmax(s, eps))
      }
      out
    },
    jsd = {
      out <- matrix(0, nrow(Qn), nrow(TH))
      lg <- function(x) ifelse(x > 0, log(x), 0)
      for (k in seq_len(nrow(TH))) {
        m <- sweep(Qn, 2L, TH[k, ], "+") / 2
        kl1 <- rowSums(Qn * (lg(Qn) - lg(m)))
        kl2 <- rowSums(
          sweep(-lg(m), 2L, lg(TH[k, ]), "+") * rep(TH[k, ], each = nrow(Qn))
        )
        out[, k] <- -(kl1 + kl2) / 2
      }
      out
    },
    stop("unknown distance: ", distance)
  )
}

# Which stored weighting a distance needs.
.cp_weight_for <- function(distance) {
  switch(
    distance,
    tfidf_cosine = "idf",
    balassa_cosine = "balassa",
    "none"
  )
}

.cp_base_distance <- function(distance) {
  if (distance %chin% c("tfidf_cosine", "balassa_cosine")) {
    "cosine"
  } else {
    distance
  }
}

# 4. predict_cp5_centroid -----

#' Predict CP2021 codes against static skill profiles
#'
#' Scores each announcement against the profiles of the CP codes observed for
#' its ESCO group, and returns the best one. No training data is consulted: the
#' whole model is the object returned by [build_cp_profiles()].
#'
#' @param profiles A `cp_profiles` object from [build_cp_profiles()].
#' @param postings A data.table of announcements to code. Needs `general_id`
#'   and the restrictor column the profiles were built on, plus `idsector` when
#'   the profiles carry a sector term. Every row gets exactly one prediction.
#' @param skills A data.table with `general_id` and the profiles' skill column.
#' @param distance Character naming the scoring rule, one of
#'   `"multinomial_nb"`, `"complement_nb"`, `"bernoulli_nb"`, `"hellinger"`,
#'   `"cosine"`, `"l2"`, `"l1"`, `"chisq"`, `"jsd"`, `"dice"`,
#'   `"tfidf_cosine"`, `"balassa_cosine"`.
#' @param prior_weight Numeric multiplier on `log n_c`, the class-size prior
#'   (default 1). `0` removes the prior; large values reduce the rule to picking
#'   the largest class, which is the modal cascade.
#' @param dense_budget Numeric cap on the elements of a dense query block, used
#'   to chunk large groups (default 2e8).
#' @param explain Logical: also return the per-candidate score matrix for each
#'   announcement (default FALSE). Costly; intended for diagnosis.
#' @param verbose Logical: print a one-line summary (default TRUE).
#'
#' @return A data.table with `general_id`, the profiles' target column,
#'   `confidence` and `method`. `method` is `"centroid"` where the model
#'   scored, `"single_candidate"` where the group offered only one code,
#'   `"frequency"` where the announcement carries no known skill and the class
#'   prior decided alone, and `"no_match"` where the group is absent from the
#'   profiles. When `explain = TRUE` the result carries a `scores` attribute.
#'
#' @details
#' The rule is
#' \deqn{score(q, c) = \lambda \log n_c + s(q, \theta_c) + \mathrm{sector}}
#' with \eqn{s} the similarity implied by `distance` and \eqn{\theta_c} the
#' smoothed, group-shrunk profile of candidate \eqn{c}.
#'
#' **The shrinkage makes the model falsifiable.** As `shrink_beta` grows, every
#' \eqn{\theta_c} in a group collapses onto that group's profile, the similarity
#' term becomes constant across candidates, and the ranking reduces to
#' \eqn{\lambda \log n_c} — which is exactly the modal cascade. So a correct
#' implementation must reproduce the modal baseline in that limit, and
#' `skillviz_workflow/run_cp5_centroid.R` refuses to report any accuracy until
#' it has checked that it does. Ties are broken on the smaller CP code, matching
#' the cascade.
#'
#' Announcements carrying no skill present in the profiles get `"frequency"`
#' rather than `"centroid"`: the prior decided them, not the skill vector. That
#' matches [predict_cp5_knn()]'s convention, which keeps the two models'
#' model-decided populations comparable.
#'
#' @examples
#' postings <- data.table::data.table(
#'   general_id = as.character(1:6),
#'   idesco_level_5 = rep(c("1000.1", "2000.1"), each = 3),
#'   cp2021_id_level_5 = c(
#'     "1.1.1.1.1", "1.1.1.1.1", "1.1.1.1.2",
#'     "2.2.2.2.0", "2.2.2.2.0", "2.2.2.2.0"
#'   )
#' )
#' skills <- data.table::data.table(
#'   general_id = as.character(rep(1:6, each = 2)),
#'   escoskill_level_3 = c(
#'     "s1", "s2", "s1", "s2", "s1", "s3",
#'     "s4", "s5", "s4", "s5", "s4", "s6"
#'   )
#' )
#' p <- build_cp_profiles(postings, skills, verbose = FALSE)
#' predict_cp5_centroid(p, postings, skills, verbose = FALSE)
#'
#' **Not adopted in production.** Measured against [predict_cp5_knn()] on a
#' sealed 3-month block it scores 84.13% against 83.92% on the rows both decide
#' -- equivalent, since the difference is under this project's 0.244 pp noise
#' floor -- with identical coverage and 16x faster coding (7.3s against 118.7s,
#' plus a 2.4s build). The pre-registered gate nonetheless failed, on memory
#' alone: 0.531x against a 0.50x threshold. Nothing in the pipeline or the
#' container calls this function.
#'
#' The full record -- flow, measured optima for every hyper-parameter, the gate,
#' what the experiment settles about the 2026-03 attempt, and what a
#' re-specification would have to decide first -- is in
#' `reference/skillviz/centroide.md` (in Italian).
#'
#' @seealso [build_cp_profiles()], [predict_cp5_knn()]
#' @export
predict_cp5_centroid <- function(
  profiles,
  postings,
  skills,
  distance = "cosine",
  prior_weight = 1,
  dense_budget = 2e8,
  explain = FALSE,
  verbose = TRUE
) {
  if (!inherits(profiles, "cp_profiles")) {
    stop("predict_cp5_centroid: `profiles` must come from build_cp_profiles()")
  }
  distance <- match.arg(
    distance,
    c(
      "multinomial_nb",
      "complement_nb",
      "bernoulli_nb",
      "hellinger",
      "cosine",
      "l2",
      "l1",
      "chisq",
      "jsd",
      "dice",
      "tfidf_cosine",
      "balassa_cosine"
    )
  )
  pr <- profiles$params
  wname <- .cp_weight_for(distance)
  if (!wname %chin% names(profiles$weights)) {
    stop(sprintf(
      "predict_cp5_centroid: distance '%s' needs weighting '%s', which build_cp_profiles() was not asked to compute",
      distance,
      wname
    ))
  }
  check_columns(
    postings,
    c("general_id", pr$restrictor),
    caller = "predict_cp5_centroid"
  )
  if (pr$sector) {
    check_columns(postings, "idsector", caller = "predict_cp5_centroid")
  }

  # 4a. Test rows, canonically named -----
  keep <- c("general_id", pr$restrictor)
  if (pr$sector) {
    keep <- c(keep, "idsector")
  }
  q <- data.table::copy(postings)[, keep, with = FALSE]
  data.table::setnames(q, pr$restrictor, "grp")
  q[, general_id := as.character(general_id)]
  q[, grp := as.character(grp)]
  if (length(pr$restrictor_na)) {
    q[grp %in% pr$restrictor_na, grp := NA_character_]
  }
  q[, row_id := .I]

  V <- length(profiles$skill_levels)
  sk <- data.table::copy(skills)[, c("general_id", pr$skill_col), with = FALSE]
  data.table::setnames(sk, pr$skill_col, "skill")
  sk[, general_id := as.character(general_id)]
  sk <- unique(sk[!is.na(skill)])
  sk[, sid := match(skill, profiles$skill_levels)]
  sk <- sk[!is.na(sid)]
  sk <- q[, .(general_id, row_id)][sk, on = "general_id", nomatch = NULL]

  Qall <- Matrix::sparseMatrix(
    i = sk$row_id,
    j = sk$sid,
    x = 1,
    dims = c(nrow(q), V)
  )
  has_skill <- Matrix::rowSums(Qall) > 0

  # 4b. Group bookkeeping -----
  q <- profiles$groups[q, on = "grp"]
  data.table::setorder(q, row_id)
  pairs <- profiles$pairs
  data.table::setorder(pairs, grp_id, code)
  n_cand <- pairs[, .(n_cand = .N), by = grp_id]

  out_code <- rep(NA_character_, nrow(q))
  out_conf <- rep(0, nrow(q))
  out_meth <- rep("no_match", nrow(q))
  score_store <- if (isTRUE(explain)) vector("list", nrow(q)) else NULL

  alpha <- pr$smooth_alpha
  beta <- pr$shrink_beta
  wv <- profiles$weights[[wname]]
  base_dist <- .cp_base_distance(distance)
  needs_comp <- identical(base_dist, "complement_nb")

  todo <- q[!is.na(grp_id), unique(grp_id)]
  n_scored <- 0L

  for (g in todo) {
    rows <- q[grp_id == g, row_id]
    pr_g <- pairs[grp_id == g]
    n_c <- pr_g$n_c
    prior <- prior_weight * log(n_c)

    # A single candidate needs no model.
    if (nrow(pr_g) == 1L) {
      out_code[rows] <- pr_g$code
      out_conf[rows] <- 1
      out_meth[rows] <- "single_candidate"
      next
    }

    # 4c. Materialise this group's profile block -----
    # theta_hat_c: smoothed class distribution. pi_e: the group's own.
    Cg <- as.matrix(profiles$counts[pr_g$pair_id, , drop = FALSE]) + alpha
    TH <- .cp_rownorm(Cg)
    PI <- as.numeric(profiles$group_counts[g, ]) + alpha
    PI <- PI / sum(PI)
    # Shrink toward the group. As beta grows every row tends to PI, the
    # similarity term goes constant, and the ranking falls to prior alone.
    lam <- n_c / (n_c + beta)
    TH <- lam * TH + (1 - lam) * rep(PI, each = nrow(TH))

    TH_comp <- NULL
    if (needs_comp) {
      # The complement profile is shrunk too, toward the same group profile and
      # on its own sample size. Without this the complement arm ignores
      # shrink_beta entirely -- its whole beta column comes out constant, and
      # the beta -> infinity identity silently fails for it while holding for
      # every other measure.
      tot <- colSums(Cg)
      TH_comp <- .cp_rownorm(pmax(
        rep(tot, each = nrow(Cg)) - Cg + alpha,
        .Machine$double.eps
      ))
      n_comp <- pmax(sum(n_c) - n_c, 0)
      lam_c <- n_comp / (n_comp + beta)
      TH_comp <- lam_c * TH_comp + (1 - lam_c) * rep(PI, each = nrow(TH_comp))
    }

    if (!identical(wname, "none")) {
      TH <- .cp_rownorm(sweep(TH, 2L, wv, "*"))
      if (needs_comp) {
        TH_comp <- .cp_rownorm(sweep(TH_comp, 2L, wv, "*"))
      }
    }

    # 4d. Sector term -----
    sect_add <- NULL
    if (pr$sector) {
      Sg <- as.matrix(profiles$sector_counts[pr_g$pair_id, , drop = FALSE]) +
        alpha
      SP <- log(.cp_rownorm(Sg))
      qs <- match(
        {
          s <- as.character(q[row_id %in% rows, idsector])
          s[is.na(s)] <- ""
          s
        },
        profiles$sector_levels
      )
      sect_add <- matrix(0, length(rows), nrow(pr_g))
      ok <- !is.na(qs)
      if (any(ok)) {
        sect_add[ok, ] <- t(SP)[qs[ok], , drop = FALSE]
      }
    }

    # 4e. Score, chunked on the query block -----
    chunk <- max(1L, as.integer(dense_budget / max(1L, V)))
    for (b0 in seq(1L, length(rows), by = chunk)) {
      b1 <- min(b0 + chunk - 1L, length(rows))
      idx <- rows[b0:b1]
      Q <- as.matrix(Qall[idx, , drop = FALSE])
      if (!identical(wname, "none")) {
        Q <- sweep(Q, 2L, wv, "*")
      }
      S <- .cp_kernel(base_dist, Q, TH, TH_comp)
      S <- sweep(S, 2L, prior, "+")
      if (!is.null(sect_add)) {
        S <- S + sect_add[b0:b1, , drop = FALSE]
      }
      # A query with no known skill carries no evidence: the similarity term is
      # the same for every candidate and only the prior separates them.
      blank <- !has_skill[idx]
      if (any(blank)) {
        S[blank, ] <- rep(prior, each = sum(blank))
      }

      win <- max.col(S, ties.method = "first")
      out_code[idx] <- pr_g$code[win]
      out_meth[idx] <- ifelse(blank, "frequency", "centroid")
      # Softmax share of the winner, on the scale of the scores themselves.
      mx <- S[cbind(seq_along(idx), win)]
      ex <- exp(S - mx)
      out_conf[idx] <- 1 / rowSums(ex)
      if (isTRUE(explain)) {
        for (i in seq_along(idx)) {
          score_store[[idx[i]]] <- stats::setNames(S[i, ], pr_g$code)
        }
      }
      n_scored <- n_scored + length(idx)
    }
  }

  res <- data.table::data.table(
    general_id = q$general_id,
    code = out_code,
    confidence = out_conf,
    method = out_meth
  )
  data.table::setnames(res, "code", pr$target)

  if (verbose) {
    mix <- res[, .N, by = method][order(-N)]
    message(sprintf(
      "predict_cp5_centroid: %d rows (%s)",
      nrow(res),
      paste(sprintf("%s=%d", mix$method, mix$N), collapse = ", ")
    ))
  }
  if (isTRUE(explain)) {
    data.table::setattr(res, "scores", score_store)
  }
  res[]
}
