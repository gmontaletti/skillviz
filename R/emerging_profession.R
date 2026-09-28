# Changing and emerging professions -----
#
# Change of skill profiles between two windows: turnover, decomposition
# by skill groups, split-half null distribution, breadth of change,
# uptake of emerging skills and the emerging-profession rule.

# 0. internal helpers -----

#' Build a profile pair from counts
#'
#' @param cnt_a,cnt_b data.tables with `prof_col`, `skill_col`, `x`
#'   (postings of the profession mentioning the skill in the window).
#' @param n_a,n_b data.tables with `prof_col`, `n` (postings of the
#'   profession with at least one skill in the window).
#' @param prof_col,skill_col Column names.
#' @param base,target Window bounds stored in the result.
#' @return An object of class `skillviz_profile_pair`.
#' @keywords internal
#' @noRd
.make_profile_pair <- function(
  cnt_a,
  cnt_b,
  n_a,
  n_b,
  prof_col,
  skill_col,
  base = NULL,
  target = NULL
) {
  nn <- merge(n_a, n_b, by = prof_col, suffixes = c("_a", "_b"))
  nn <- nn[n_a > 0 & n_b > 0]
  if (nrow(nn) == 0L) {
    stop(
      "build_profile_pair: no profession has postings in both windows",
      call. = FALSE
    )
  }
  profs <- sort(as.character(nn[[prof_col]]))
  nn <- nn[match(profs, as.character(nn[[prof_col]]))]

  keep <- function(cnt) {
    cnt[as.character(get(prof_col)) %chin% profs & x > 0]
  }
  cnt_a <- keep(cnt_a)
  cnt_b <- keep(cnt_b)
  skills <- sort(unique(as.character(c(
    cnt_a[[skill_col]],
    cnt_b[[skill_col]]
  ))))

  to_mat <- function(cnt, n) {
    j <- match(as.character(cnt[[prof_col]]), profs)
    Matrix::sparseMatrix(
      i = match(as.character(cnt[[skill_col]]), skills),
      j = j,
      x = cnt$x / n[j],
      dims = c(length(skills), length(profs)),
      dimnames = list(skills, profs)
    )
  }
  structure(
    list(
      a = to_mat(cnt_a, nn$n_a),
      b = to_mat(cnt_b, nn$n_b),
      n_a = stats::setNames(as.numeric(nn$n_a), profs),
      n_b = stats::setNames(as.numeric(nn$n_b), profs),
      prof_col = prof_col,
      skill_col = skill_col,
      base = base,
      target = target
    ),
    class = "skillviz_profile_pair"
  )
}

#' Check a profile pair argument
#'
#' @param pair Object to check.
#' @param caller Calling function.
#' @keywords internal
#' @noRd
.check_pair <- function(pair, caller) {
  if (!inherits(pair, "skillviz_profile_pair")) {
    stop(
      caller,
      ": `pair` must be the output of build_profile_pair()",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Profile counts of one window from posting-level incidence
#'
#' @param inc data.table with `id_col`, `prof_col`, `skill_col`.
#' @return A list with `cnt` (prof, skill, x) and `n` (prof, n).
#' @keywords internal
#' @noRd
.window_counts <- function(inc, id_col, prof_col, skill_col) {
  inc <- unique(inc[, c(id_col, prof_col, skill_col), with = FALSE])
  n <- unique(inc[, c(id_col, prof_col), with = FALSE])[,
    list(n = .N),
    by = prof_col
  ]
  cnt <- inc[, list(x = .N), by = c(prof_col, skill_col)]
  list(cnt = cnt, n = n)
}

#' Turnover statistics of a profile pair
#'
#' @param pair A `skillviz_profile_pair`.
#' @return A data.table with one row per profession.
#' @keywords internal
#' @noRd
.pair_turnover <- function(pair) {
  a <- pair$a
  b <- pair$b
  n_a <- pair$n_a
  n_b <- pair$n_b
  ab <- Matrix::colSums(a * b)
  na2 <- Matrix::colSums(a^2)
  nb2 <- Matrix::colSums(b^2)
  cosv <- ifelse(na2 > 0 & nb2 > 0, ab / sqrt(na2 * nb2), NA_real_)
  d2 <- Matrix::colSums((a - b)^2)
  s <- (a %*% Matrix::Diagonal(x = n_a) + b %*% Matrix::Diagonal(x = n_b)) %*%
    Matrix::Diagonal(x = 1 / (n_a + n_b))
  e_d2 <- Matrix::colSums(s - s^2) * (1 / n_a + 1 / n_b)
  sbar2 <- Matrix::colSums(((a + b) / 2)^2)
  out <- data.table::data.table(
    prof = colnames(a),
    n_base = as.numeric(n_a),
    n_target = as.numeric(n_b),
    n_skill = as.integer(Matrix::colSums((a + b) > 0)),
    turnover_coseno = 1 - cosv,
    d2 = as.numeric(d2),
    d2_atteso = as.numeric(e_d2),
    turnover_netto = ifelse(sbar2 > 0, pmax(0, d2 - e_d2) / sbar2, NA_real_)
  )
  data.table::setnames(out, "prof", pair$prof_col)
  out
}

# 1. build_profile_pair -----

#' Skill profiles of professions in two windows
#'
#' Builds the sparse skill profiles of each profession in a base and a
#' target window, the input of the profile-change functions.
#'
#' @details
#' For profession \eqn{j} and window \eqn{W}, the profile entry of skill
#' \eqn{k} is the share of the profession's postings that mention the
#' skill, \eqn{s_{kj} = \sum_{t \in W} x_{kjt} / \sum_{t \in W} n_{jt}}.
#' The denominator of a profession-period must be the same on all its
#' rows. Only professions with postings in both windows are kept; the
#' skill universe is the union of skills with a positive count in either
#' window.
#'
#' @param panel Profession-level share panel (e.g. from
#'   [compute_share_panel()] with `group_cols = prof_col`).
#' @param prof_col Profession column.
#' @param skill_col Skill column. Default `"skill_id"`.
#' @param time_col,x_col,n_col See [compute_share_trend()].
#' @param base,target Numeric vectors `c(from, to)` with the bounds of
#'   the base and target windows.
#' @return An object of class `skillviz_profile_pair`: a list with `a`
#'   and `b` (sparse `dgCMatrix`, skills x professions, base and target
#'   shares), `n_a` and `n_b` (named postings per profession), and the
#'   metadata `prof_col`, `skill_col`, `base`, `target`.
#' @examples
#' panel <- data.table::data.table(
#'   cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
#'   mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
#' )
#' pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
#' pair$a
#' @export
build_profile_pair <- function(
  panel,
  prof_col,
  skill_col = "skill_id",
  time_col = "mese_idx",
  x_col = "x",
  n_col = "n",
  base,
  target
) {
  caller <- "build_profile_pair"
  dt <- .em_prepare(
    panel,
    c(prof_col, skill_col),
    time_col,
    x_col,
    n_col,
    caller
  )
  base <- .em_check_window(base, "base", caller)
  target <- .em_check_window(target, "target", caller)

  den <- unique(dt[, c(prof_col, ".t", ".n"), with = FALSE])
  if (anyDuplicated(den, by = c(prof_col, ".t")) > 0L) {
    stop(
      caller,
      ": `",
      n_col,
      "` must be constant within profession ",
      "and period",
      call. = FALSE
    )
  }
  agg <- function(w) {
    cnt <- dt[
      .t >= w[1L] & .t <= w[2L],
      list(x = sum(.x)),
      by = c(prof_col, skill_col)
    ]
    n <- den[.t >= w[1L] & .t <= w[2L], list(n = sum(.n)), by = prof_col]
    list(cnt = cnt, n = n)
  }
  wa <- agg(base)
  wb <- agg(target)
  .make_profile_pair(
    wa$cnt,
    wb$cnt,
    wa$n,
    wb$n,
    prof_col,
    skill_col,
    base,
    target
  )
}

# 2. compute_profile_turnover -----

#' Turnover of profession skill profiles
#'
#' Measures how much the skill profile of each profession changed
#' between the base and the target window, net of sampling noise.
#'
#' @details
#' With \eqn{a} and \eqn{b} the base and target profiles of a
#' profession:
#' - `turnover_coseno` \eqn{= 1 - a \cdot b / (\|a\| \|b\|)};
#' - `d2` \eqn{= \sum_k (a_k - b_k)^2};
#' - `d2_atteso` \eqn{= \sum_k s_k (1 - s_k) (1/n_a + 1/n_b)}, the
#'   expected value of `d2` when both windows share the pooled profile
#'   \eqn{s = (n_a a + n_b b)/(n_a + n_b)} (binomial sampling noise);
#' - `turnover_netto` \eqn{= \max(0, d2 - E[d2]) / \sum_k \bar s_k^2},
#'   with \eqn{\bar s = (a + b)/2}, a scale-free change net of noise.
#'
#' @param pair Output of [build_profile_pair()].
#' @return A data.table with one row per profession: the profession
#'   column, `n_base`, `n_target`, `n_skill`, `turnover_coseno`, `d2`,
#'   `d2_atteso`, `turnover_netto`.
#' @examples
#' panel <- data.table::data.table(
#'   cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
#'   mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
#' )
#' pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
#' compute_profile_turnover(pair)
#' @export
compute_profile_turnover <- function(pair) {
  .check_pair(pair, "compute_profile_turnover")
  .pair_turnover(pair)[]
}

# 3. decompose_profile_change -----

#' Additive decomposition of profile change by skill groups
#'
#' Splits the squared distance `d2` between base and target profiles of
#' each profession into the contributions of skill groups.
#'
#' @details
#' For any partition of the skills into groups \eqn{G_1, \dots, G_M},
#' \eqn{d^2 = \sum_m \sum_{k \in G_m} (a_k - b_k)^2}, so the group
#' contributions add up exactly to `d2` (the same property as
#' [compute_decomposed_distance()]). Each column of `group_cols` defines
#' one partition (group type). Skills without a group, or missing from
#' `skill_groups`, go to the group `"(mancante)"`.
#'
#' @param pair Output of [build_profile_pair()].
#' @param skill_groups A data.frame with the skill column of the pair
#'   and one or more group columns, one row per skill.
#' @param group_cols Character vector of group columns (e.g.
#'   `c("reusetype", "green", "digital")`).
#' @return A data.table with the profession column, `tipo_gruppo`
#'   (group column name), `gruppo`, `d2_gruppo`, `d2` and `quota`
#'   (`d2_gruppo / d2`, 0 when `d2 = 0`).
#' @examples
#' panel <- data.table::data.table(
#'   cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
#'   mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
#' )
#' pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
#' groups <- data.table::data.table(skill_id = c("a", "b", "c"),
#'   green = c("no", "no", "si"))
#' decompose_profile_change(pair, groups, "green")
#' @export
decompose_profile_change <- function(pair, skill_groups, group_cols) {
  caller <- "decompose_profile_change"
  .check_pair(pair, caller)
  if (!is.data.frame(skill_groups)) {
    stop(caller, ": `skill_groups` must be a data.frame", call. = FALSE)
  }
  check_columns(skill_groups, c(pair$skill_col, group_cols), caller = caller)
  sg <- data.table::as.data.table(skill_groups)
  if (anyDuplicated(sg[[pair$skill_col]]) > 0L) {
    stop(caller, ": `skill_groups` must have one row per skill", call. = FALSE)
  }

  skills <- rownames(pair$a)
  dsq <- (pair$a - pair$b)^2
  d2 <- Matrix::colSums(dsq)
  profs <- colnames(pair$a)
  idx <- match(skills, as.character(sg[[pair$skill_col]]))

  res <- data.table::rbindlist(lapply(group_cols, function(gc) {
    g <- as.character(sg[[gc]])[idx]
    g[is.na(g)] <- "(mancante)"
    lev <- sort(unique(g))
    gm <- Matrix::sparseMatrix(
      i = match(g, lev),
      j = seq_along(skills),
      x = 1,
      dims = c(length(lev), length(skills))
    )
    part <- as.matrix(gm %*% dsq)
    data.table::data.table(
      prof = rep(profs, each = length(lev)),
      tipo_gruppo = gc,
      gruppo = rep(lev, times = length(profs)),
      d2_gruppo = as.numeric(part),
      d2 = rep(as.numeric(d2), each = length(lev))
    )
  }))
  res[, quota := ifelse(d2 > 0, d2_gruppo / d2, 0)]
  data.table::setnames(res, "prof", pair$prof_col)
  res[]
}

# 4. compute_turnover_null -----

#' Split-half null distribution of profile turnover
#'
#' Estimates, for each profession, the distribution of `turnover_netto`
#' when there is no real change, by comparing two random halves of the
#' postings of the same window.
#'
#' @details
#' For each replicate and each window (`base` and `target`), the
#' postings of the window are split at random into two halves; the two
#' halves play the role of base and target in
#' [compute_profile_turnover()]. The null sample of a profession
#' collects `2 * n_rep` values of `turnover_netto`; its `prob` quantile
#' is the threshold above which observed turnover exceeds the variation
#' expected from sampling alone. Each posting must belong to one
#' profession. The global RNG state is restored.
#'
#' @param incidence Posting-level data.frame with `id_col`, `time_col`,
#'   `prof_col` and `skill_col`, one row per posting and skill.
#' @param prof_col Profession column.
#' @param skill_col Skill column. Default `"skill_id"`.
#' @param id_col Posting identifier. Default `"general_id"`.
#' @param time_col Period column. Default `"mese_idx"`.
#' @param base,target Numeric vectors `c(from, to)`.
#' @param n_rep Number of random splits per window. Default `20`.
#' @param prob Quantile of the null distribution. Default `0.95`.
#' @param seed Integer seed. Default `1`.
#' @return A data.table with the profession column, `n_nullo` (null
#'   values), `media_nullo` and `q_nullo` (the `prob` quantile).
#' @examples
#' set.seed(6)
#' inc <- data.table::data.table(general_id = rep(1:200, each = 2),
#'   mese_idx = rep(rep(1:2, each = 100), each = 2), cp4 = "p1",
#'   skill_id = sample(letters[1:5], 400, replace = TRUE))
#' compute_turnover_null(inc, "cp4", base = c(1, 1), target = c(2, 2),
#'   n_rep = 5)
#' @export
compute_turnover_null <- function(
  incidence,
  prof_col,
  skill_col = "skill_id",
  id_col = "general_id",
  time_col = "mese_idx",
  base,
  target,
  n_rep = 20L,
  prob = 0.95,
  seed = 1L
) {
  caller <- "compute_turnover_null"
  if (!is.data.frame(incidence)) {
    stop(caller, ": `incidence` must be a data.frame", call. = FALSE)
  }
  check_columns(
    incidence,
    c(id_col, time_col, prof_col, skill_col),
    caller = caller
  )
  base <- .em_check_window(base, "base", caller)
  target <- .em_check_window(target, "target", caller)
  if (!is.numeric(n_rep) || length(n_rep) != 1L || n_rep < 1) {
    stop(caller, ": `n_rep` must be a positive integer", call. = FALSE)
  }
  if (!is.numeric(prob) || length(prob) != 1L || prob <= 0 || prob >= 1) {
    stop(caller, ": `prob` must be in (0, 1)", call. = FALSE)
  }
  inc <- data.table::as.data.table(incidence)[,
    c(id_col, time_col, prof_col, skill_col),
    with = FALSE
  ]
  wins <- list(base, target)

  sims <- .with_seed(seed, {
    data.table::rbindlist(lapply(seq_len(n_rep), function(r) {
      data.table::rbindlist(lapply(wins, function(w) {
        iw <- inc[get(time_col) >= w[1L] & get(time_col) <= w[2L]]
        ids <- unique(iw[[id_col]])
        half <- ids[stats::runif(length(ids)) < 0.5]
        in_a <- iw[[id_col]] %in% half
        ca <- .window_counts(iw[in_a], id_col, prof_col, skill_col)
        cb <- .window_counts(iw[!in_a], id_col, prof_col, skill_col)
        pr <- tryCatch(
          .make_profile_pair(ca$cnt, cb$cnt, ca$n, cb$n, prof_col, skill_col),
          error = function(e) NULL
        )
        if (is.null(pr)) {
          return(NULL)
        }
        .pair_turnover(pr)[, c(prof_col, "turnover_netto"), with = FALSE]
      }))
    }))
  })

  if (nrow(sims) == 0L) {
    stop(caller, ": no profession has postings in both halves", call. = FALSE)
  }
  sims[,
    list(
      n_nullo = sum(!is.na(turnover_netto)),
      media_nullo = mean(turnover_netto, na.rm = TRUE),
      q_nullo = as.numeric(stats::quantile(turnover_netto, prob, na.rm = TRUE))
    ),
    by = prof_col
  ][]
}

# 5. compute_change_breadth -----

#' Breadth of skill change by profession
#'
#' Summarises how many skills of each profession change significantly
#' and how widely the change spreads across skill groups, and ranks the
#' professions by breadth of change.
#'
#' @details
#' From the skill x profession trends (`trend`), a skill grows (declines)
#' when `p_col < alpha` and `slope_col` is positive (negative).
#' `ampiezza_gruppi` counts the groups of `group_col` touched by at
#' least one significant change. With \eqn{c_m = \sum_{k \in G_m}
#' |b_k - a_k|} the absolute profile change of group \eqn{m} (from
#' `pair`), `entropia_cambi` \eqn{= -\sum_m p_m \log p_m / \log M} with
#' \eqn{p_m = c_m / \sum_m c_m} and \eqn{M} the number of groups in the
#' skill universe of the pair: 1 when change is spread evenly over
#' groups, 0 when it is concentrated in one. `rango_ampiezza` orders the
#' professions by `n_skill_cambio`, then `ampiezza_gruppi`, then
#' `entropia_cambi` (all decreasing; 1 = widest change).
#'
#' @param trend Skill x profession trends, e.g. [compute_share_trend()]
#'   with `key_cols = c(prof_col, skill_col)`.
#' @param pair Output of [build_profile_pair()]; defines the professions
#'   and the profile changes.
#' @param skill_groups A data.frame with the skill column of the pair and
#'   `group_col`.
#' @param group_col Group column (e.g. `"hier_label_1"`).
#' @param alpha Significance level. Default `0.05`.
#' @param slope_col Slope column of `trend`. Default `"pendenza"`.
#' @param p_col Adjusted p-value column of `trend`. Default `"p_adj"`.
#' @return A data.table with the profession column, `n_skill_crescita`,
#'   `n_skill_calo`, `n_skill_cambio`, `ampiezza_gruppi`,
#'   `entropia_cambi` and `rango_ampiezza`.
#' @examples
#' panel <- data.table::data.table(
#'   cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
#'   mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
#' )
#' pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
#' trend <- data.table::data.table(cp4 = "p1", skill_id = c("a", "b", "c"),
#'   pendenza = c(0, -0.2, 0.3), p_adj = c(0.8, 0.01, 0.01))
#' groups <- data.table::data.table(skill_id = c("a", "b", "c"),
#'   area = c("x", "x", "y"))
#' compute_change_breadth(trend, pair, groups, "area")
#' @export
compute_change_breadth <- function(
  trend,
  pair,
  skill_groups,
  group_col,
  alpha = 0.05,
  slope_col = "pendenza",
  p_col = "p_adj"
) {
  caller <- "compute_change_breadth"
  .check_pair(pair, caller)
  prof_col <- pair$prof_col
  skill_col <- pair$skill_col
  if (!is.data.frame(trend) || !is.data.frame(skill_groups)) {
    stop(
      caller,
      ": `trend` and `skill_groups` must be data.frames",
      call. = FALSE
    )
  }
  check_columns(
    trend,
    c(prof_col, skill_col, slope_col, p_col),
    caller = caller
  )
  check_columns(skill_groups, c(skill_col, group_col), caller = caller)
  sg <- unique(
    data.table::as.data.table(skill_groups)[,
      c(skill_col, group_col),
      with = FALSE
    ],
    by = skill_col
  )
  data.table::setnames(sg, c(skill_col, group_col), c(".skill", ".grp"))
  sg[, `:=`(.skill = as.character(.skill), .grp = as.character(.grp))]

  profs <- data.table::data.table(.prof = colnames(pair$a))

  # significant changes
  tr <- data.table::as.data.table(trend)[,
    c(prof_col, skill_col, slope_col, p_col),
    with = FALSE
  ]
  data.table::setnames(tr, c(".prof", ".skill", ".b", ".p"))
  tr[, `:=`(.prof = as.character(.prof), .skill = as.character(.skill))]
  tr <- tr[.prof %chin% profs$.prof & !is.na(.p) & .p < alpha & !is.na(.b)]
  tr <- merge(tr, sg, by = ".skill", all.x = TRUE)
  tr[is.na(.grp), .grp := "(mancante)"]
  sig <- tr[,
    list(
      n_skill_crescita = sum(.b > 0),
      n_skill_calo = sum(.b < 0),
      ampiezza_gruppi = data.table::uniqueN(.grp[.b != 0])
    ),
    by = .prof
  ]

  # entropy of absolute change across groups
  skills <- rownames(pair$a)
  g <- sg$.grp[match(skills, sg$.skill)]
  g[is.na(g)] <- "(mancante)"
  lev <- sort(unique(g))
  gm <- Matrix::sparseMatrix(
    i = match(g, lev),
    j = seq_along(skills),
    x = 1,
    dims = c(length(lev), length(skills))
  )
  cm <- as.matrix(gm %*% abs(pair$b - pair$a))
  n_lev <- length(lev)
  ent <- apply(cm, 2L, function(v) {
    tot <- sum(v)
    if (tot <= 0 || n_lev < 2L) {
      return(0)
    }
    p <- v[v > 0] / tot
    -sum(p * log(p)) / log(n_lev)
  })
  ent <- data.table::data.table(
    .prof = colnames(pair$a),
    entropia_cambi = as.numeric(ent)
  )

  res <- merge(profs, sig, by = ".prof", all.x = TRUE)
  res[
    is.na(n_skill_crescita),
    `:=`(
      n_skill_crescita = 0L,
      n_skill_calo = 0L,
      ampiezza_gruppi = 0L
    )
  ]
  res <- merge(res, ent, by = ".prof")
  res[, n_skill_cambio := n_skill_crescita + n_skill_calo]
  res[,
    rango_ampiezza := data.table::frankv(
      res,
      cols = c("n_skill_cambio", "ampiezza_gruppi", "entropia_cambi"),
      order = -1L,
      ties.method = "min"
    )
  ]
  data.table::setcolorder(
    res,
    c(
      ".prof",
      "n_skill_crescita",
      "n_skill_calo",
      "n_skill_cambio",
      "ampiezza_gruppi",
      "entropia_cambi",
      "rango_ampiezza"
    )
  )
  data.table::setnames(res, ".prof", prof_col)
  data.table::setorderv(res, "rango_ampiezza")
  res[]
}

# 6. compute_emerging_uptake -----

#' Uptake of emerging skills by profession
#'
#' Measures, for each profession and window, the share of postings that
#' mention at least one emerging skill, and compares its change with the
#' change observed over all postings.
#'
#' @details
#' `quota_base` and `quota_target` are the shares of the profession's
#' postings (with at least one skill) mentioning at least one skill of
#' `emerging` in the base and target window; `delta_quota` is their
#' difference. The same quantities over all postings form the regional
#' reference; its confidence interval for the change is obtained by a
#' bootstrap of postings, which for a share of independent postings
#' reduces to binomial resampling in each window.
#'
#' `n_emergenti_profilo` counts the emerging skills in the core profile
#' of the profession in the target window: the most mentioned skills
#' that together cover `core_share` of its skill mentions.
#'
#' @param incidence Posting-level data.frame with `id_col`, `time_col`,
#'   `prof_col` and `skill_col`, one row per posting and skill.
#' @param emerging Vector of emerging skill identifiers.
#' @param prof_col Profession column.
#' @param skill_col Skill column. Default `"skill_id"`.
#' @param id_col Posting identifier. Default `"general_id"`.
#' @param time_col Period column. Default `"mese_idx"`.
#' @param base,target Numeric vectors `c(from, to)`.
#' @param core_share Share of mentions defining the core profile.
#'   Default `0.8`.
#' @param conf Confidence level of the regional interval. Default
#'   `0.95`.
#' @param n_boot Bootstrap replicates. Default `1000`.
#' @param seed Integer seed. Default `1`.
#' @return A list with:
#'   - `professioni`: data.table with the profession column, `n_base`,
#'     `quota_base`, `n_target`, `quota_target`, `delta_quota`,
#'     `n_emergenti_profilo`;
#'   - `regione`: one-row data.table with `n_base`, `quota_base`,
#'     `n_target`, `quota_target`, `delta_quota`, `delta_lo`,
#'     `delta_hi`.
#' @examples
#' inc <- data.table::data.table(general_id = rep(1:8, each = 2),
#'   mese_idx = rep(c(1, 1, 1, 1, 2, 2, 2, 2), each = 2),
#'   cp4 = rep(c("p1", "p2"), each = 2, times = 4),
#'   skill_id = c("a", "b", "a", "c", "a", "b", "a", "c",
#'     "e", "b", "a", "c", "e", "a", "a", "c"))
#' compute_emerging_uptake(inc, emerging = "e", prof_col = "cp4",
#'   base = c(1, 1), target = c(2, 2), n_boot = 100)
#' @export
compute_emerging_uptake <- function(
  incidence,
  emerging,
  prof_col,
  skill_col = "skill_id",
  id_col = "general_id",
  time_col = "mese_idx",
  base,
  target,
  core_share = 0.8,
  conf = 0.95,
  n_boot = 1000L,
  seed = 1L
) {
  caller <- "compute_emerging_uptake"
  if (!is.data.frame(incidence)) {
    stop(caller, ": `incidence` must be a data.frame", call. = FALSE)
  }
  check_columns(
    incidence,
    c(id_col, time_col, prof_col, skill_col),
    caller = caller
  )
  base <- .em_check_window(base, "base", caller)
  target <- .em_check_window(target, "target", caller)
  if (length(emerging) == 0L) {
    stop(caller, ": `emerging` must contain at least one skill", call. = FALSE)
  }
  if (
    !is.numeric(core_share) ||
      length(core_share) != 1L ||
      core_share <= 0 ||
      core_share > 1
  ) {
    stop(caller, ": `core_share` must be in (0, 1]", call. = FALSE)
  }
  inc <- unique(data.table::as.data.table(incidence)[,
    c(id_col, time_col, prof_col, skill_col),
    with = FALSE
  ])
  data.table::setnames(inc, c(".id", ".t", ".prof", ".skill"))
  inc[, .em := .skill %in% emerging]

  win <- function(w) {
    iw <- inc[.t >= w[1L] & .t <= w[2L]]
    iw[, list(.hit = any(.em)), by = c(".id", ".prof")]
  }
  pa <- win(base)
  pb <- win(target)
  by_prof <- function(p, suffix) {
    out <- p[, list(n = .N, quota = mean(.hit)), by = .prof]
    data.table::setnames(
      out,
      c("n", "quota"),
      paste0(c("n_", "quota_"), suffix)
    )
    out
  }
  res <- merge(by_prof(pa, "base"), by_prof(pb, "target"), by = ".prof")
  res[, delta_quota := quota_target - quota_base]

  # core profile in the target window
  it <- inc[.t >= target[1L] & .t <= target[2L]]
  men <- it[, list(.m = .N, .em = .em[1L]), by = c(".prof", ".skill")]
  data.table::setorderv(men, c(".prof", ".m"), c(1L, -1L))
  men[, .prev := (cumsum(.m) - .m) / sum(.m), by = .prof]
  core <- men[
    .prev < core_share,
    list(n_emergenti_profilo = sum(.em)),
    by = .prof
  ]
  res <- merge(res, core, by = ".prof", all.x = TRUE)
  res[is.na(n_emergenti_profilo), n_emergenti_profilo := 0L]
  data.table::setnames(res, ".prof", prof_col)

  # regional reference with binomial bootstrap
  n0 <- nrow(pa)
  n1 <- nrow(pb)
  p0 <- mean(pa$.hit)
  p1 <- mean(pb$.hit)
  boot <- .with_seed(seed, {
    stats::rbinom(n_boot, n1, p1) / n1 - stats::rbinom(n_boot, n0, p0) / n0
  })
  qq <- stats::quantile(
    boot,
    c((1 - conf) / 2, 1 - (1 - conf) / 2),
    names = FALSE
  )
  reg <- data.table::data.table(
    n_base = n0,
    quota_base = p0,
    n_target = n1,
    quota_target = p1,
    delta_quota = p1 - p0,
    delta_lo = qq[1L],
    delta_hi = qq[2L]
  )
  list(professioni = res[], regione = reg)
}

# 7. classify_emerging_professions -----

#' Classify emerging professions
#'
#' Applies the three-condition rule that defines an emerging profession:
#' growing demand, profile change beyond sampling noise, and uptake of
#' emerging skills faster than the regional average.
#'
#' @details
#' A profession is emerging (`emergente = TRUE`) when all hold:
#' 1. `crescita_significativa`: the drift-net demand slope in `demand` is
#'    positive with `p_col < alpha`;
#' 2. `turnover_oltre_nullo`: `turnover_netto` exceeds the `q_nullo`
#'    quantile of its split-half null distribution;
#' 3. `assorbimento_oltre_regione`: `delta_quota` of the profession
#'    exceeds the upper bound `delta_hi` of the regional change.
#'
#' Missing information makes a condition `FALSE`.
#'
#' @param demand Profession demand trends, e.g. [compute_share_trend()]
#'   on a panel of profession counts over all postings.
#' @param turnover Output of [compute_profile_turnover()].
#' @param turnover_null Output of [compute_turnover_null()].
#' @param uptake Output of [compute_emerging_uptake()].
#' @param prof_col Profession column.
#' @param alpha Significance level. Default `0.05`.
#' @param slope_col,p_col Slope and adjusted p-value columns of
#'   `demand`. Defaults `"pendenza"`, `"p_adj"`.
#' @return A data.table with the profession column, `pendenza`,
#'   `turnover_netto`, `q_nullo`, `delta_quota`, the three condition
#'   flags and `emergente`.
#' @examples
#' demand <- data.table::data.table(cp4 = c("p1", "p2"),
#'   pendenza = c(0.05, 0.05), p_adj = c(0.01, 0.01))
#' turnover <- data.table::data.table(cp4 = c("p1", "p2"),
#'   turnover_netto = c(0.3, 0.01))
#' null <- data.table::data.table(cp4 = c("p1", "p2"), q_nullo = 0.05)
#' uptake <- list(
#'   professioni = data.table::data.table(cp4 = c("p1", "p2"),
#'     delta_quota = c(0.2, 0.2)),
#'   regione = data.table::data.table(delta_hi = 0.05)
#' )
#' classify_emerging_professions(demand, turnover, null, uptake, "cp4")
#' @export
classify_emerging_professions <- function(
  demand,
  turnover,
  turnover_null,
  uptake,
  prof_col,
  alpha = 0.05,
  slope_col = "pendenza",
  p_col = "p_adj"
) {
  caller <- "classify_emerging_professions"
  check_columns(demand, c(prof_col, slope_col, p_col), caller = caller)
  check_columns(turnover, c(prof_col, "turnover_netto"), caller = caller)
  check_columns(turnover_null, c(prof_col, "q_nullo"), caller = caller)
  if (
    !is.list(uptake) || is.null(uptake$professioni) || is.null(uptake$regione)
  ) {
    stop(
      caller,
      ": `uptake` must be the output of ",
      "compute_emerging_uptake()",
      call. = FALSE
    )
  }
  check_columns(uptake$professioni, c(prof_col, "delta_quota"), caller = caller)
  check_columns(uptake$regione, "delta_hi", caller = caller)

  pick <- function(d, cols) {
    out <- data.table::as.data.table(d)[, c(prof_col, cols), with = FALSE]
    out[, (prof_col) := as.character(get(prof_col))]
    out
  }
  dm <- pick(demand, c(slope_col, p_col))
  data.table::setnames(dm, c(slope_col, p_col), c("pendenza", ".p"))
  profs <- unique(data.table::rbindlist(list(
    dm[, prof_col, with = FALSE],
    pick(turnover, character(0)),
    pick(uptake$professioni, character(0))
  )))
  res <- merge(profs, dm, by = prof_col, all.x = TRUE)
  res <- merge(
    res,
    pick(turnover, "turnover_netto"),
    by = prof_col,
    all.x = TRUE
  )
  res <- merge(res, pick(turnover_null, "q_nullo"), by = prof_col, all.x = TRUE)
  res <- merge(
    res,
    pick(uptake$professioni, "delta_quota"),
    by = prof_col,
    all.x = TRUE
  )
  soglia <- uptake$regione$delta_hi[1L]

  res[, `:=`(
    crescita_significativa = !is.na(.p) &
      .p < alpha &
      !is.na(pendenza) &
      pendenza > 0,
    turnover_oltre_nullo = !is.na(turnover_netto) &
      !is.na(q_nullo) &
      turnover_netto > q_nullo,
    assorbimento_oltre_regione = !is.na(delta_quota) &
      !is.na(soglia) &
      delta_quota > soglia
  )]
  res[,
    emergente := crescita_significativa &
      turnover_oltre_nullo &
      assorbimento_oltre_regione
  ]
  res[, .p := NULL]
  res[]
}
