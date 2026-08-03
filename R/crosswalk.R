# Classification crosswalks and announcement merging -----

# 1. build_cpi_esco_crosswalk -----

#' Build ESCO-to-CP2021 level 3 crosswalk
#'
#' Merge the CPI-ESCO mapping with the CP2021 level 3 classification
#' to produce a lookup table that maps each `idesco_level_4` code to
#' its Italian ESCO label and CP2021 3-digit group.
#'
#' @param esco_mapping A data.table from [read_esco_mapping()],
#'   containing at least `idesco_level_4`, `esco_level_4`, and
#'   `idcp_2011_v`.
#' @param cpi3 A data.table with CP2021 level 3 classification.
#'   Must contain `cod_3` and `nome_3`.
#' @return A data.table with one row per `idesco_level_4` and columns:
#'   `idesco_level_4`, `it_esco_level_4` (collapsed Italian ESCO
#'   labels), `cod_3`, `nome_3`.
#' @export
build_cpi_esco_crosswalk <- function(esco_mapping, cpi3) {
  check_columns(
    esco_mapping,
    c("idesco_level_4", "esco_level_4", "idcp_2011_v"),
    caller = "build_cpi_esco_crosswalk"
  )
  check_columns(cpi3, c("cod_3", "nome_3"), caller = "build_cpi_esco_crosswalk")

  mapping <- data.table::copy(esco_mapping)
  mapping[, cod_3 := substring(idcp_2011_v, 1L, 5L)]

  esco <- merge(mapping, cpi3, by = "cod_3")

  cpi_esco <- esco[,
    .(
      it_esco_level_4 = paste(unique(esco_level_4), collapse = ", "),
      cod_3 = paste(data.table::first(cod_3), collapse = ", "),
      nome_3 = paste(data.table::first(nome_3), collapse = ", ")
    ),
    keyby = .(idesco_level_4)
  ]

  cpi_esco
}

# 2. prepare_annunci_esco -----

#' Merge announcements with ESCO mapping and parse dates
#'
#' Joins the announcements table with the ESCO mapping to add Italian
#' profession labels, and parses year/month/day columns into proper
#' IDate fields.
#'
#' @param ann A data.table of announcements with columns
#'   `idesco_level_4`, `year_grab_date`, `month_grab_date`,
#'   `day_grab_date`, `year_expire_date`, `month_expire_date`,
#'   `day_expire_date`, and `general_id`.
#' @param esco_mapping A data.table from [read_esco_mapping()],
#'   containing at least `idesco_level_4` and `esco_level_4`.
#' @return A data.table with added columns `it_esco_level_4`,
#'   `gdate` (grab date as IDate), and `edate` (expire date as
#'   IDate). Deduplicated to unique combinations of `general_id`,
#'   `gdate`, `idesco_level_4`, `it_esco_level_4`.
#' @export
prepare_annunci_esco <- function(ann, esco_mapping) {
  check_columns(
    ann,
    c(
      "general_id",
      "idesco_level_4",
      "year_grab_date",
      "month_grab_date",
      "day_grab_date",
      "year_expire_date",
      "month_expire_date",
      "day_expire_date"
    ),
    caller = "prepare_annunci_esco"
  )
  check_columns(
    esco_mapping,
    c("idesco_level_4", "esco_level_4"),
    caller = "prepare_annunci_esco"
  )

  # 2a. build id-to-label lookup -----
  idesco <- esco_mapping[,
    .(
      it_esco_level_4 = paste(unique(esco_level_4), collapse = ", ")
    ),
    keyby = .(idesco_level_4)
  ]

  dt <- data.table::copy(ann)
  data.table::setkey(dt, general_id)

  # 2b. parse date columns -----
  parse_ymd_columns(dt, "grab_date", "gdate")
  parse_ymd_columns(dt, "expire_date", "edate")

  # 2c. merge with ESCO labels -----
  dt <- merge(dt, idesco, by = "idesco_level_4", all.x = TRUE, all.y = FALSE)

  dt <- unique(dt[, .(general_id, gdate, idesco_level_4, it_esco_level_4)])
  dt
}

# 3. prepare_annunci_geography -----

#' Prepare announcements with CPI geographic dimension
#'
#' Merges announcements with ESCO labels and a territorial mapping
#' table to add the CPI (Centro per l'Impiego) field, then
#' aggregates unique announcement counts by CPI, profession, and
#' year.
#'
#' @param ann A data.table of announcements with columns
#'   `idesco_level_4`, `idcity`, `general_id`, and year/month/day
#'   grab and expire date columns.
#' @param esco_mapping A data.table from [read_esco_mapping()],
#'   containing at least `idesco_level_4` and `esco_level_4`.
#' @param territoriale A data.table with territorial classification.
#'   Must contain `COD_ISTAT`, `CPI`, and `COD_REGIONE_PAUT`.
#' @param regione Integer region code to filter the territorial
#'   table. Defaults to `10L`.
#' @return A data.table with columns `CPI`, `it_esco_level_4`,
#'   `year_grab_date`, and `N` (unique announcement count), filtered
#'   to complete cases and ordered by CPI, year, descending N.
#' @export
prepare_annunci_geography <- function(
  ann,
  esco_mapping,
  territoriale,
  regione = 10L
) {
  check_columns(
    ann,
    c(
      "general_id",
      "idesco_level_4",
      "idcity",
      "year_grab_date",
      "month_grab_date",
      "day_grab_date",
      "year_expire_date",
      "month_expire_date",
      "day_expire_date"
    ),
    caller = "prepare_annunci_geography"
  )
  check_columns(
    esco_mapping,
    c("idesco_level_4", "esco_level_4"),
    caller = "prepare_annunci_geography"
  )
  check_columns(
    territoriale,
    c("COD_ISTAT", "CPI", "COD_REGIONE_PAUT"),
    caller = "prepare_annunci_geography"
  )

  # 3a. filter territorial table -----
  terr <- territoriale[
    COD_REGIONE_PAUT == regione & CPI != "",
    .(idcity = COD_ISTAT, CPI)
  ]

  # 3b. build id-to-label lookup -----
  idesco <- esco_mapping[,
    .(
      it_esco_level_4 = paste(unique(esco_level_4), collapse = ", ")
    ),
    keyby = .(idesco_level_4)
  ]

  dt <- data.table::copy(ann)
  data.table::setkey(dt, general_id)

  # 3c. parse date columns -----
  parse_ymd_columns(dt, "grab_date", "gdate")
  parse_ymd_columns(dt, "expire_date", "edate")

  # 3d. merge with ESCO labels -----
  dt <- merge(dt, idesco, by = "idesco_level_4", all.x = TRUE, all.y = FALSE)

  # 3e. merge with territorial data -----
  dt <- merge(dt, terr, by = "idcity", all.x = TRUE, all.y = FALSE)

  # 3f. aggregate by CPI, profession, year -----
  cpi <- dt[,
    .(N = data.table::uniqueN(general_id)),
    keyby = .(CPI, it_esco_level_4, year_grab_date)
  ]
  cpi <- cpi[stats::complete.cases(cpi)]
  data.table::setorder(cpi, CPI, year_grab_date, -N)
  cpi
}

# 4. classify_esco_to_cpi -----

#' Classify unmapped ESCO L4 codes to CPI groups via Naive Bayes
#'
#' Uses a Multinomial Naive Bayes classifier to predict CPI 3-digit groups
#' for ESCO level 4 codes that lack a CP2021 mapping in the postings data.
#' The crosswalk between ESCO L4 and CPI groups is derived directly from
#' the postings via majority vote on the `cp2021_id_level_3` column.
#' Training data comes from postings with a non-missing CP2021 code;
#' prediction uses the skill profile of unmapped postings.
#'
#' When `crosswalk` is supplied, "unmapped" means ESCO L4 codes present in
#' postings but absent from `crosswalk$idesco_level_4` (the official
#' crosswalk). This typically yields more unmapped codes than the default
#' behaviour, which considers any code with at least one non-empty
#' `cp2021_id_level_3` posting as mapped.
#'
#' @param postings A data.table from `normalize_ojv()$postings`. Needs
#'   `general_id`, `idesco_level_4`, `cp2021_id_level_3`, and
#'   `cp2021_level_3`.
#' @param skills A data.table from `normalize_ojv()$skills`. Needs
#'   `general_id` and `escoskill_level_3` (or `ESCOSKILL_LEVEL_3`).
#' @param top_k Integer, number of top CPI predictions per ESCO L4 code
#'   (default: 3).
#' @param alpha Numeric, Laplace smoothing parameter (default: 1.0).
#' @param crosswalk Optional data.table with an `idesco_level_4` column
#'   representing the official ESCO-to-CPI mapping (e.g. from
#'   `build_cpi_esco_crosswalk()`). When provided, "unmapped" ESCO L4 codes
#'   are those **not** in `crosswalk$idesco_level_4`. When NULL (default),
#'   the function falls back to deriving the mapping from the postings.
#' @param verbose Logical, print progress messages (default: TRUE).
#' @return A data.table keyed on `idesco_level_4` with columns:
#'   \describe{
#'     \item{idesco_level_4}{The unmapped ESCO level 4 code.}
#'     \item{cod_3}{Predicted CPI 3-digit code.}
#'     \item{nome_3}{Predicted CPI 3-digit label.}
#'     \item{probability}{Posterior probability (softmax-normalized).}
#'     \item{rank}{Rank among top_k predictions (1 = best).}
#'     \item{n_postings}{Number of postings with this ESCO L4.}
#'     \item{n_skills}{Number of distinct skills observed for this ESCO L4.}
#'   }
#' @export
#' @examples
#' postings <- data.table::data.table(
#'   general_id = 1:6,
#'   idesco_level_4 = c("E001", "E001", "E002", "E002", "E003", "E003"),
#'   cp2021_id_level_3 = c("2.1.1", "2.1.1", "3.1.2", "3.1.2", NA, NA),
#'   cp2021_level_3 = c("Informatici", "Informatici",
#'                       "Ingegneri", "Ingegneri", NA, NA)
#' )
#' skills <- data.table::data.table(
#'   general_id = c(1L, 1L, 2L, 3L, 3L, 4L, 5L, 5L, 6L),
#'   escoskill_level_3 = c("S01", "S02", "S01", "S03", "S04", "S03",
#'                         "S01", "S02", "S01")
#' )
#' result <- classify_esco_to_cpi(postings, skills, top_k = 2L)
classify_esco_to_cpi <- function(
  postings,
  skills,
  top_k = 3L,
  alpha = 1.0,
  crosswalk = NULL,
  verbose = TRUE
) {
  # 1. input validation -----
  check_columns(
    postings,
    c("general_id", "idesco_level_4", "cp2021_id_level_3", "cp2021_level_3"),
    caller = "classify_esco_to_cpi"
  )

  # 2. normalize skill column name -----
  skill_col <- if ("ESCOSKILL_LEVEL_3" %in% names(skills)) {
    "ESCOSKILL_LEVEL_3"
  } else if ("escoskill_level_3" %in% names(skills)) {
    "escoskill_level_3"
  } else {
    stop(
      "classify_esco_to_cpi: skills must contain ESCOSKILL_LEVEL_3 ",
      "or escoskill_level_3",
      call. = FALSE
    )
  }

  # 3. identify mapped vs unmapped ESCO L4 codes -----
  is_mapped_vec <- !is.na(postings$cp2021_id_level_3) &
    nzchar(postings$cp2021_id_level_3)

  # Build ESCO-level lookup via majority vote from mapped postings
  esco_cpi_lookup <- postings[
    is_mapped_vec,
    .N,
    by = .(idesco_level_4, cp2021_id_level_3, cp2021_level_3)
  ]
  setorder(esco_cpi_lookup, idesco_level_4, -N)
  esco_cpi_lookup <- esco_cpi_lookup[, .SD[1L], by = idesco_level_4]
  esco_cpi_lookup[, N := NULL]
  setnames(
    esco_cpi_lookup,
    c("cp2021_id_level_3", "cp2021_level_3"),
    c("cod_3", "nome_3")
  )

  all_esco <- postings[, unique(idesco_level_4)]

  if (!is.null(crosswalk)) {
    # Use official crosswalk as reference: unmapped = not in crosswalk
    ref_esco <- unique(crosswalk$idesco_level_4)
    unmapped_esco <- setdiff(all_esco, ref_esco)
    mapped_esco <- intersect(all_esco, ref_esco)
  } else {
    # Default: derive mapping from postings majority vote
    mapped_esco <- esco_cpi_lookup[, unique(idesco_level_4)]
    unmapped_esco <- setdiff(all_esco, mapped_esco)
  }

  if (verbose) {
    message(
      "classify_esco_to_cpi: ",
      length(mapped_esco),
      " mapped, ",
      length(unmapped_esco),
      " unmapped, ",
      length(all_esco),
      " total ESCO L4 codes"
    )
  }

  if (length(unmapped_esco) == 0L) {
    if (verbose) {
      message("classify_esco_to_cpi: no unmapped codes, returning empty table")
    }
    return(data.table(
      idesco_level_4 = character(0),
      cod_3 = character(0),
      nome_3 = character(0),
      probability = numeric(0),
      rank = integer(0),
      n_postings = integer(0),
      n_skills = integer(0),
      key = "idesco_level_4"
    ))
  }

  # 4. build training set -----
  train_postings <- postings[
    is_mapped_vec,
    .(general_id, idesco_level_4, cp2021_id_level_3, cp2021_level_3)
  ]
  setnames(
    train_postings,
    c("cp2021_id_level_3", "cp2021_level_3"),
    c("cod_3", "nome_3")
  )

  train <- merge(
    skills[, .SD, .SDcols = c("general_id", skill_col)],
    train_postings[, .(general_id, cod_3)],
    by = "general_id"
  )

  setnames(train, skill_col, "skill")

  # 5. compute class priors -----
  class_counts <- train_postings[, .(n_docs = uniqueN(general_id)), by = cod_3]
  n_total <- sum(class_counts$n_docs)
  class_counts[, log_prior := log(n_docs / n_total)]

  if (verbose) {
    message(
      "classify_esco_to_cpi: ",
      nrow(class_counts),
      " training classes, ",
      n_total,
      " training documents"
    )
  }

  # 6. compute skill likelihoods with Laplace smoothing -----
  skill_class <- train[, .(count = uniqueN(general_id)), by = .(cod_3, skill)]
  V <- skills[, uniqueN(get(skill_col))]

  skill_class <- merge(
    skill_class,
    class_counts[, .(cod_3, n_docs)],
    by = "cod_3"
  )
  skill_class[, log_lik := log((count + alpha) / (n_docs + alpha * V))]

  class_counts[, log_absent := log(alpha / (n_docs + alpha * V))]

  if (verbose) {
    message("classify_esco_to_cpi: vocabulary size = ", V)
  }

  # 7. build prediction skill profiles -----
  pred_postings <- postings[
    idesco_level_4 %in% unmapped_esco,
    .(general_id, idesco_level_4)
  ]
  pred_skills <- merge(
    skills[, .SD, .SDcols = c("general_id", skill_col)],
    pred_postings,
    by = "general_id"
  )
  setnames(pred_skills, skill_col, "skill")

  esco_profiles <- pred_skills[,
    .(n_docs_with_skill = uniqueN(general_id)),
    by = .(idesco_level_4, skill)
  ]

  # 8. compute log-posteriors -----
  scores <- merge(
    esco_profiles,
    skill_class[, .(cod_3, skill, log_lik)],
    by = "skill",
    allow.cartesian = TRUE
  )
  scores <- scores[,
    .(sum_log_lik = sum(n_docs_with_skill * log_lik)),
    by = .(idesco_level_4, cod_3)
  ]

  scores <- merge(
    scores,
    class_counts[, .(cod_3, log_prior, log_absent)],
    by = "cod_3"
  )
  esco_n_skills <- esco_profiles[,
    .(n_observed = uniqueN(skill)),
    by = idesco_level_4
  ]
  scores <- merge(scores, esco_n_skills, by = "idesco_level_4")
  scores[,
    log_posterior := log_prior + sum_log_lik + (V - n_observed) * log_absent
  ]

  # 9. softmax normalization and top-k selection -----
  scores[, max_lp := max(log_posterior), by = idesco_level_4]
  scores[, probability := exp(log_posterior - max_lp)]
  scores[, probability := probability / sum(probability), by = idesco_level_4]

  setorder(scores, idesco_level_4, -probability)
  scores[, rank := seq_len(.N), by = idesco_level_4]
  result <- scores[rank <= top_k]

  # 10. attach labels and metadata -----
  cpi_labels <- unique(train_postings[, .(cod_3, nome_3)])
  cpi_labels <- cpi_labels[, .(nome_3 = nome_3[1L]), by = cod_3]
  result <- merge(result, cpi_labels, by = "cod_3", all.x = TRUE)

  posting_counts <- pred_postings[,
    .(n_postings = uniqueN(general_id)),
    by = idesco_level_4
  ]
  skill_counts <- esco_profiles[,
    .(n_skills = uniqueN(skill)),
    by = idesco_level_4
  ]
  result <- merge(result, posting_counts, by = "idesco_level_4")
  result <- merge(result, skill_counts, by = "idesco_level_4")

  result <- result[, .(
    idesco_level_4,
    cod_3,
    nome_3,
    probability,
    rank,
    n_postings,
    n_skills
  )]
  setkeyv(result, "idesco_level_4")

  if (verbose) {
    message(
      "classify_esco_to_cpi: classified ",
      uniqueN(result$idesco_level_4),
      " unmapped ESCO L4 codes"
    )
  }

  result[]
}


# 4. Jaccard k-NN helper -----

#' Jaccard k-NN vote for a single ESCO group
#'
#' Computes pairwise Jaccard similarity between test and train announcements
#' using sparse binary skill vectors, selects k nearest neighbors, and
#' performs a weighted vote with optional sector boosting.
#'
#' @param test_gids Character vector of test announcement IDs.
#' @param test_skills data.table with `general_id`, `escoskill_level_3`.
#' @param train_skills data.table with `general_id`, `escoskill_level_3`,
#'   `cp2021_id_level_4`.
#' @param skill_levels Character vector of all skill codes in this group.
#' @param test_sectors Character vector parallel to `test_gids` with sector
#'   codes (NA allowed).
#' @param train_sectors Character vector parallel to unique train general_ids
#'   with sector codes (NA allowed). NULL disables sector boosting.
#' @param freq_cp4 Character: frequency cascade fallback CP4 code.
#' @param k Integer: number of neighbors.
#' @param sector_boost Numeric: multiplier for same-sector neighbors.
#'
#' @return data.table with columns `general_id`, `cp2021_id_level_4`,
#'   `confidence`, `method`.
#' @keywords internal
.jaccard_knn_vote <- function(
  test_gids,
  test_skills,
  train_skills,
  skill_levels,
  test_sectors,
  train_sectors,
  freq_cp4,
  k,
  sector_boost,
  max_train = 50000L,
  dense_budget = 2e8
) {
  # 4a. Edge case: no skills -----
  if (
    nrow(test_skills) == 0L ||
      nrow(train_skills) == 0L ||
      length(skill_levels) == 0L
  ) {
    return(data.table::data.table(
      general_id = test_gids,
      cp2021_id_level_4 = freq_cp4,
      confidence = 0.1,
      method = "frequency"
    ))
  }

  # 4b. Build sparse matrices -----
  train_gids <- unique(train_skills$general_id)

  .to_sparse <- function(gids, sk_dt) {
    sk <- unique(sk_dt[general_id %in% gids, .(general_id, escoskill_level_3)])
    if (nrow(sk) == 0L) {
      return(NULL)
    }
    gf <- factor(sk$general_id, levels = gids)
    sf <- factor(sk$escoskill_level_3, levels = skill_levels)
    Matrix::sparseMatrix(
      i = as.integer(gf),
      j = as.integer(sf),
      x = 1,
      dims = c(length(gids), length(skill_levels)),
      dimnames = list(gids, skill_levels)
    )
  }

  test_mat <- .to_sparse(test_gids, test_skills)
  train_mat <- .to_sparse(train_gids, train_skills)

  if (is.null(test_mat) || is.null(train_mat)) {
    return(data.table::data.table(
      general_id = test_gids,
      cp2021_id_level_4 = freq_cp4,
      confidence = 0.1,
      method = "frequency"
    ))
  }

  # 4c. Cap train size to avoid OOM on large ESCO groups -----
  # The subsample is a deterministic stride over the existing train_gids order
  # rather than sample.int(). That order is already fixed by the caller, so the
  # stride is reproducible, and since general_id is unrelated to the CP4 label
  # it subsamples without bias. The order is deliberately NOT re-sorted: it also
  # breaks similarity ties downstream, so re-ordering would shift predictions
  # for every group, not just the capped ones.
  #
  # This fires in production: ESCO group 5223 carries ~52.7k labelled rows on
  # the 24-month window, so under the previous unseeded draw two runs on
  # identical input returned different predictions for it.
  if (length(train_gids) > max_train) {
    warning(
      "an ESCO group has ",
      length(train_gids),
      " labelled rows, above max_train = ",
      max_train,
      "; subsampling by a deterministic stride. Raise max_train to use the ",
      "whole group.",
      call. = FALSE
    )
    keep <- unique(as.integer(seq(
      1L,
      length(train_gids),
      length.out = max_train
    )))
    train_gids <- train_gids[keep]
    train_mat <- train_mat[keep, , drop = FALSE]
    if (!is.null(train_sectors)) {
      train_sectors <- train_sectors[keep]
    }
  }

  # 4d. Precompute row sums and CP4 lookup -----
  rs_test <- Matrix::rowSums(test_mat)
  rs_train <- Matrix::rowSums(train_mat)

  lu <- unique(train_skills[, .(general_id, cp2021_id_level_4)])
  tcp4 <- lu[match(train_gids, general_id), cp2021_id_level_4]
  actual_k <- min(k, length(train_gids))

  # 4d. Batched Jaccard k-NN vote -----
  # Process test rows in batches to avoid dense matrix OOM on large groups
  # dense_budget caps the ELEMENTS of the dense test x train block. Each batch
  # holds three such matrices at once (the intersection, the outer() union and
  # the ratio), so peak memory is roughly 24 bytes per budgeted element: the
  # 2e8 default costs ~4.8 GB and OOM-killed a 7.75 GB container on the
  # 24-month window. Batching only chunks the work -- each test row is scored
  # independently -- so lowering this changes memory, never results.
  batch_size <- max(1L, as.integer(dense_budget / length(train_gids)))
  n_test <- length(test_gids)
  out <- vector("list", n_test)
  oi <- 0L

  for (b_start in seq(1L, n_test, by = batch_size)) {
    b_end <- min(b_start + batch_size - 1L, n_test)
    b_idx <- b_start:b_end

    intersection <- as.matrix(
      Matrix::tcrossprod(test_mat[b_idx, , drop = FALSE], train_mat)
    )
    union_batch <- outer(rs_test[b_idx], rs_train, "+") - intersection
    union_batch[union_batch == 0] <- 1
    jac_batch <- intersection / union_batch

    for (j in seq_len(nrow(jac_batch))) {
      oi <- oi + 1L
      sims <- jac_batch[j, ]
      idx <- order(sims, decreasing = TRUE)[seq_len(actual_k)]
      ts <- sims[idx]
      tc <- tcp4[idx]
      valid <- !is.na(tc) & ts > 0

      if (!any(valid)) {
        out[[oi]] <- data.table::data.table(
          general_id = test_gids[oi],
          cp2021_id_level_4 = freq_cp4,
          confidence = 0.1,
          method = "frequency"
        )
        next
      }

      # Apply sector boost
      boosted <- ts
      if (sector_boost != 1.0 && !is.null(train_sectors)) {
        my_sect <- test_sectors[oi]
        if (!is.na(my_sect)) {
          same <- train_sectors[idx] == my_sect & !is.na(train_sectors[idx])
          boosted[same] <- boosted[same] * sector_boost
        }
      }

      va <- data.table::data.table(cp4 = tc[valid], w = boosted[valid])
      va <- va[, .(wt = sum(w)), by = cp4]
      winner <- va[which.max(wt)]
      out[[oi]] <- data.table::data.table(
        general_id = test_gids[oi],
        cp2021_id_level_4 = winner$cp4,
        confidence = winner$wt / sum(boosted[valid]),
        method = "knn"
      )
    }
  }

  data.table::rbindlist(out)
}


# 4b. .global_knn_vote -----

# Unrestricted Jaccard k-NN for announcements that carry no idesco_level_4 and
# therefore have no candidate set to restrict to.
#
# This was pure R and looped per test row, which made it ~92% of the imputation
# runtime on the 24-month window (18221s of 19856s) and is why the rescue path
# had to be disabled in production. The search is now the compiled kernel in
# src/global_knn.cpp, which walks an inverted index rather than materialising a
# 221M-entry intersection: ~6.7x faster at production pool size (165ms -> 25ms
# per rescue row), with byte-identical output. The vote stays in R because its
# tie-break depends on grouping order and it only ever touches k elements.
#
# The train pool is capped by systematic stride over general_id order. That is
# deterministic and unrelated to the CP4 label, so it subsamples without bias
# and without touching the RNG (predict_cp4_knn() must stay reproducible).
.global_knn_vote <- function(
  test_gids,
  test_skills,
  train_skills,
  test_sectors,
  train_sectors,
  k,
  sector_boost,
  max_train,
  nnz_budget = 2e7
) {
  skill_levels <- sort(unique(c(
    test_skills$escoskill_level_3,
    train_skills$escoskill_level_3
  )))
  train_gids <- sort(unique(train_skills$general_id))
  if (length(train_gids) == 0L || length(skill_levels) == 0L) {
    return(NULL)
  }

  if (length(train_gids) > max_train) {
    stride <- seq(1L, length(train_gids), length.out = max_train)
    train_gids <- train_gids[unique(as.integer(stride))]
    train_skills <- train_skills[general_id %chin% train_gids]
  }
  if (!is.null(train_sectors)) {
    train_sectors <- train_sectors[match(train_gids, names(train_sectors))]
  }

  .sparse <- function(gids, sk_dt) {
    sk <- unique(sk_dt[
      general_id %chin% gids,
      .(general_id, escoskill_level_3)
    ])
    if (nrow(sk) == 0L) {
      return(NULL)
    }
    Matrix::sparseMatrix(
      i = match(sk$general_id, gids),
      j = match(sk$escoskill_level_3, skill_levels),
      x = 1,
      dims = c(length(gids), length(skill_levels))
    )
  }

  train_mat <- .sparse(train_gids, train_skills)
  test_mat <- .sparse(test_gids, test_skills)
  if (is.null(train_mat) || is.null(test_mat)) {
    return(NULL)
  }

  rs_train <- Matrix::rowSums(train_mat)
  rs_test <- Matrix::rowSums(test_mat)
  lu <- unique(train_skills[, .(general_id, cp2021_id_level_4)])
  tcp4 <- lu[match(train_gids, general_id), cp2021_id_level_4]
  tsect <- if (is.null(train_sectors)) {
    rep(NA_character_, length(train_gids))
  } else {
    as.character(train_sectors)
  }
  cp4_ok <- !is.na(tcp4)

  # No tcrossprod and no batching: the kernel walks train_mat as an inverted
  # index (its CSC `p` indexes skills, `i` the train rows carrying them) and
  # accumulates intersections per test row, so the 221M-entry product is never
  # built. The transpose hands the kernel each test row's skill list.
  # nnz_budget is unused now and kept only for signature compatibility.
  test_t <- Matrix::t(test_mat)
  nn <- global_topk(
    train_mat@p,
    train_mat@i,
    test_t@p,
    test_t@i,
    rs_test,
    rs_train,
    cp4_ok,
    length(train_gids),
    as.integer(k)
  )
  if (length(nn$sim) == 0L) {
    return(NULL)
  }

  nb <- data.table::data.table(row = nn$col, idx = nn$idx, w = nn$sim)

  # Boost after selection, exactly as the reference does.
  if (sector_boost != 1) {
    same <- !is.na(tsect[nb$idx]) &
      !is.na(test_sectors[nb$row]) &
      tsect[nb$idx] == test_sectors[nb$row]
    nb[same, w := w * sector_boost]
  }
  nb[, cp4 := tcp4[idx]]

  # `by=` groups in first-appearance order and the neighbour table is emitted in
  # selection order, so this reproduces the per-row data.table the reference
  # built; .I[which.max()] then picks the FIRST maximum, as which.max() did.
  agg <- nb[, list(wt = sum(w)), by = list(row, cp4)]
  tot <- nb[, list(tw = sum(w)), by = row]
  win <- agg[agg[, .I[which.max(wt)], by = row]$V1]
  win <- tot[win, on = "row"]
  data.table::setorder(win, row)

  data.table::data.table(
    general_id = test_gids[win$row],
    cp2021_id_level_4 = win$cp4,
    confidence = win$wt / win$tw,
    method = "knn_global"
  )
}


# 5. predict_cp4_knn -----

#' Predict CP2021 level-4 codes via sector-boosted Jaccard k-NN
#'
#' Assigns CP2021 level-4 profession codes to unlabeled job announcements
#' using a two-step approach: (1) restrict candidates to CP4 codes observed
#' in labeled data for the same ESCO level-4 code (de facto crosswalk),
#' (2) disambiguate via Jaccard k-NN on binary skill vectors, with optional
#' sector boosting that gives higher weight to same-sector neighbors.
#'
#' @param postings A data.table with columns: `general_id` (character),
#'   `idesco_level_4` (integer or character), `cp2021_id_level_4` (character,
#'   NA for unlabeled rows). Optionally includes `idsector` (character) for
#'   sector boosting. Only `NA` disables the boost for a row: itaposts stores an
#'   unknown sector as the empty string, so two announcements of unknown sector
#'   count as same-sector. That is deliberate, see Details.
#' @param skills A data.table with columns: `general_id` (character),
#'   `escoskill_level_3` (character).
#' @param k Integer number of nearest neighbors (default 7).
#' @param sector_boost Numeric multiplier for same-sector neighbors in the
#'   weighted vote. Set to 1.0 to disable sector boosting (default 3.0).
#' @param max_train Integer cap on the labelled pool used per ESCO group
#'   (default 50000). Groups above it are subsampled by a deterministic stride
#'   over the existing row order, so results are reproducible and the RNG is
#'   untouched. The cap does fire on the current 24-month window — ESCO group
#'   5223 carries about 52,700 labelled rows — so raise it to use those groups
#'   whole, at the cost of a dense `test x train` block that grows with it.
#' @param dense_budget Numeric cap on the number of elements in the dense
#'   `test x train` similarity block (default 2e8). Each batch holds three such
#'   matrices at once, so peak memory is roughly 24 bytes per budgeted element
#'   -- the default costs about 4.8 GB, which OOM-kills an 8 GB container on the
#'   24-month window. Lowering it only chunks the work into more batches; every
#'   test row is scored independently, so results are unchanged.
#' @param rescue_no_match Logical: when TRUE, announcements that would be
#'   `no_match` for want of an `idesco_level_4` but that do carry skills are
#'   classified by an unrestricted k-NN over the whole labeled pool, and
#'   returned with `method = "knn_global"`. Defaults to FALSE, which reproduces
#'   the previous behaviour exactly. See Details for measured accuracy: these
#'   predictions are markedly less accurate than the ESCO-restricted ones, so
#'   `confidence` should be used to filter them.
#' @param rescue_k Integer number of neighbors for the rescue pass (default
#'   10). Only used when `rescue_no_match = TRUE`.
#' @param rescue_max_train Integer cap on the labeled pool used by the rescue
#'   pass (default 200000). The pool is subsampled by a deterministic stride
#'   over `general_id`, so results are reproducible and the RNG is untouched.
#'   Raising it improves accuracy at roughly linear cost in time.
#' @param verbose Logical: print progress messages (default TRUE).
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{general_id}{Announcement identifier.}
#'     \item{cp2021_id_level_4}{Predicted CP2021 level-4 code.}
#'     \item{confidence}{Weighted vote share of the winning class (0--1).}
#'     \item{method}{One of `"knn"`, `"frequency"`, `"single_candidate"`,
#'       `"no_match"`, or `"knn_global"` when `rescue_no_match = TRUE`.}
#'   }
#'
#' @details
#' The function splits `postings` into labeled (non-NA `cp2021_id_level_4`)
#' and unlabeled rows. Labeled data serves as the training set. For each
#' unlabeled announcement:
#'
#' 1. The ESCO level-4 code restricts the CP4 candidate space to codes
#'    observed in labeled data (de facto crosswalk).
#' 2. If only one candidate exists, assign it directly
#'    (`method = "single_candidate"`).
#' 3. Otherwise, compute Jaccard similarity between the announcement's
#'    binary skill vector and all labeled announcements in the same ESCO
#'    group. Select the k nearest neighbors and apply a weighted vote, where
#'    same-sector neighbors receive a `sector_boost` multiplier.
#' 4. If no skills are available, fall back to the modal CP4 for that ESCO
#'    code (`method = "frequency"`).
#' 5. If the ESCO code is not present in labeled data, return NA
#'    (`method = "no_match"`).
#'
#' Validated on 2025 OJA data (80/20 stratified split): CP4 accuracy 83.0%
#' with k=7 and sector_boost=3.0, vs 62.6% frequency baseline. Re-swept on the
#' 24-month production window (7x7 grid over k and sector_boost, 5 stratified
#' splits, `skillviz_workflow/run_cp4_hyperparameter_sweep.R`): CP4 accuracy
#' 80.29% with k=7 and sector_boost=5.0, vs 59.1% frequency baseline. The
#' single-year figure is the easier setting; prefer the windowed one when
#' comparing against pipeline output.
#'
#' Both figures above come from splits that draw train and test from the same
#' window at random, so near-duplicate postings can land on both sides.
#'
#' The pipeline calls this function **once on the whole 24-month window**
#' (`skillviz_workflow/_targets.R`), so an unlabeled posting draws neighbours
#' from every month, including later ones: production is *contemporaneous*, not
#' walk-forward. A walk-forward check
#' (`skillviz_workflow/run_cp4_temporal_validation.R`) trains on every labeled
#' month before each of the last 6 months and scores that month alone; CP4
#' accuracy falls to 75.9% and every grid cell loses 3.3-4.4 pp, stable across
#' the 6 months (75.0-76.6%, no drift). That is a lower bound for a
#' *future-deployment* scenario, not the regime the pipeline runs in.
#'
#' Accuracy quoted over all test rows conflates three populations. Decomposed
#' on a contemporaneous stratified holdout at k=7/sector_boost=5.0
#' (`skillviz_workflow/run_cp4_kernel_sweep.R`): the k-NN vote decides 82.6% of
#' production rows and is 86.1% accurate on them, against 62.0% for the modal
#' fallback on the same rows, so the vote is worth +24.1 pp. The frequency
#' fallback covers 4.4% at 69.9%, and 13.1% of unlabeled rows get `no_match`
#' because they carry no `idesco_level_4` at all -- only 5.6% of *labeled* rows
#' do, so a labeled holdout cannot reproduce production coverage. Weighted
#' together that gives **~74% expected production CP4 accuracy**, and that
#' assumes k-NN accuracy transfers unchanged from labeled to unlabeled rows,
#' which the covariate shift below makes optimistic.
#'
#' Unlabeled postings are systematically longer than labeled ones: 12.7 skills
#' against 8.2 on k-NN-eligible rows, a 1.55x gap stable across all 25 months
#' and 78% within-source. Six alternative similarity kernels were tested for
#' length robustness (`run_cp4_kernel_sweep.R`, contemporaneous holdout, 3
#' splits), parameterised as Tversky `I / (I + alpha*(A-I) + beta*(B-I))`:
#' **Jaccard `(1,1)` wins at k=7 and nothing displaces it.** The best challenger
#' `(1,0.5)` is -0.013 pp, 19x smaller than the 0.244 pp swing produced by
#' flipping an arbitrary tie-break; Dice `(0.5,0.5)`, which is rank-equivalent
#' to Jaccard and therefore selects identical neighbours, still moves -0.33 pp
#' through vote weights alone, so anything under ~0.3 pp here is noise. The
#' most length-robust kernel, containment `I/|B|` `(0,1)`, loses **18.1 pp**:
#' Jaccard's union denominator is load-bearing, because `I/|B|` rewards short
#' neighbours and lets a 2-skill posting contained in a 28-skill query score
#' 1.0 while carrying almost no information. Do not re-test this axis.
#'
#' The length effect is real but is not a kernel problem. Long queries are
#' twin-poor: top-1 similarity 0.45 with 0.2% exact twins at 21+ skills,
#' against 0.76 and 48% at 1-4 skills. No kernel invents a neighbour that does
#' not exist.
#'
#' The occupation hierarchy was also tested and closed
#' (`skillviz_workflow/run_cp3_vote.R`). **ESCO level 3 is a worse restrictor
#' than level 4, not a better one**: weighted by row volume over 761,521
#' doubly-labelled postings, ESCO4 -> CP3 has 37.4 mean candidates and 68.4%
#' modal-share accuracy, against 52.3 and 59.8% for ESCO3 -> CP3. Coarsening the
#' *target* gains 5.7 pp; coarsening the *predictor* loses 8.6 pp, because ESCO
#' L3 pools unit groups that map to different CP3 codes. ESCO L3 also cannot
#' improve coverage: postings without an `idesco_level_4` carry
#' `idesco_level_5 = "Unclassifiable"`, so no ESCO code exists at any level
#' (and CP-unlabelled rows carry `cp2021_id_level_5 = ""`).
#'
#' Predicting CP3 by pooling the k-NN vote across sibling CP4 codes, rather than
#' truncating the CP4 winner as `build_annunci_cp4()` does, is likewise **not
#' worth it**: +0.063 pp CP3 on k-NN rows (88.54% -> 88.60% at k=7), changing
#' only 0.31% of predictions, and when the two rules disagree pooling is right
#' 60% of the time -- barely above chance, and far under this harness's 0.244 pp
#' tie-break noise floor. The predicted mechanism is refuted: the gain is flat
#' across ESCO-group ambiguity (0.00 pp at 2 candidates, +0.05 pp at 21+)
#' instead of growing with it. Truncating the CP4 argmax is very nearly optimal
#' for CP3. A second stage picking the best CP4 sibling inside the winning CP3
#' also loses (86.011% vs 86.018%), as its break-even arithmetic predicted:
#' it needs 93.5-97.8% conditional accuracy while modal-CP4-within-CP3 is 68.5%
#' over 4.6 candidates. Do not re-test this axis.
#'
#' `rescue_no_match = TRUE` addresses a different population: the 13.1% of
#' unlabeled rows that carry no `idesco_level_4`, so there is no candidate set
#' to restrict to. 94.4% of them do have skills and all have an `idsector`.
#' Validation used the population analogue rather than a simulation -- 45,029
#' *labeled* rows also lack an ESCO code, so they carry ground truth in the
#' target's shape (`skillviz_workflow/run_cp4_no_match_rescue.R`). Unrestricted
#' k-NN at `rescue_k = 10` with `sector_boost = 5` scores 68.8% CP4 / 74.0% CP3
#' there, against 36.6% for CP4-centroid cosine, 19.4% for sector-modal and
#' 3.8% for global-modal; reweighted to the target's length distribution, which
#' is longer than the analogue's, the expected figure is **73.8% CP4 / 77.8%
#' CP3**. That is well below the 86.1% of the ESCO-restricted path, which is
#' why the argument defaults to FALSE and why `confidence` matters here.
#'
#' Confidence is well calibrated on this population and is the intended filter:
#' the top 10% of rescued rows by confidence is 99.1% accurate, the top 30%
#' 96.8%, the top 50% 91.1%. Accuracy also rises steeply with posting length,
#' from 44.3% at 1-4 skills to 91.9% at 21+. The high-confidence slice is not
#' an artefact of near-duplicate retrieval: exact skill-set twins are 61.3% of
#' this population but score *worse* than non-twins (66.4% vs 71.8%), because a
#' short skill set has many twins without determining the occupation.
#'
#' The rescue pass reads accuracy from a pool capped at `rescue_max_train`.
#' Accuracy was still climbing with pool size when measured (+2.0 pp from 200k
#' to 400k), so the default 200000 trades some accuracy for runtime.
#'
#' An unknown `idsector` arrives from itaposts as the empty string, never as
#' NA, so unknown-sector announcements boost each other. Normalising the empty
#' string to NA was measured and **reduces** accuracy: -1.51 pp on the 2.8% of
#' announcements it affects (78.09% -> 76.59%) and -0.04 pp overall, on 5 of 5
#' splits. Missing sector is itself predictive of the occupation, so the
#' behaviour is kept on purpose -- do not "fix" it without re-measuring.
#'
#' @seealso [classify_esco_to_cpi()] for Naive Bayes classification of
#'   ESCO-to-CPI3 mapping.
#'
#' @examples
#' postings <- data.table::data.table(
#'   general_id = as.character(1:10),
#'   idesco_level_4 = rep(c(1000L, 2000L), each = 5),
#'   cp2021_id_level_4 = c("1.1.1.1", "1.1.1.2", "1.1.1.1", NA, NA,
#'                          "2.2.2.1", "2.2.2.1", "2.2.2.2", NA, NA),
#'   idsector = rep(c("C", "F"), each = 5)
#' )
#' skills <- data.table::data.table(
#'   general_id = as.character(c(1,1,2,2,3,3,4,4,5,5,
#'                                6,6,7,7,8,8,9,9,10,10)),
#'   escoskill_level_3 = c("s1","s2","s2","s3","s1","s2","s1","s3","s2","s3",
#'                          "s4","s5","s4","s5","s5","s6","s4","s6","s5","s6")
#' )
#' result <- predict_cp4_knn(postings, skills, k = 3L, sector_boost = 1.0)
#'
#' @export
predict_cp4_knn <- function(
  postings,
  skills,
  k = 7L,
  sector_boost = 3.0,
  max_train = 50000L,
  dense_budget = 2e8,
  rescue_no_match = FALSE,
  rescue_k = 10L,
  rescue_max_train = 200000L,
  verbose = TRUE
) {
  # 5a. Input validation -----
  check_columns(
    postings,
    c("general_id", "idesco_level_4", "cp2021_id_level_4"),
    caller = "predict_cp4_knn"
  )
  check_columns(
    skills,
    c("general_id", "escoskill_level_3"),
    caller = "predict_cp4_knn"
  )

  has_sector <- "idsector" %in% names(postings)
  if (!has_sector && sector_boost != 1.0) {
    if (verbose) {
      message("predict_cp4_knn: idsector not found, sector boost disabled")
    }
    sector_boost <- 1.0
  }

  # 5b. Split labeled / unlabeled -----
  dt <- data.table::copy(postings)
  dt[, general_id := as.character(general_id)]
  skills_dt <- data.table::copy(skills)
  skills_dt[, general_id := as.character(general_id)]

  labeled <- dt[!is.na(cp2021_id_level_4) & nzchar(cp2021_id_level_4)]
  unlabeled <- dt[is.na(cp2021_id_level_4) | !nzchar(cp2021_id_level_4)]

  if (nrow(unlabeled) == 0L) {
    if (verbose) {
      message("predict_cp4_knn: no unlabeled rows, returning empty table")
    }
    return(data.table::data.table(
      general_id = character(0),
      cp2021_id_level_4 = character(0),
      confidence = numeric(0),
      method = character(0)
    ))
  }

  if (verbose) {
    message(sprintf(
      "predict_cp4_knn: %d labeled, %d unlabeled",
      nrow(labeled),
      nrow(unlabeled)
    ))
  }

  # 5c. Build de facto crosswalk -----
  esco_cp4 <- labeled[, .N, by = .(idesco_level_4, cp2021_id_level_4)]
  data.table::setorder(esco_cp4, idesco_level_4, -N)
  esco_mode <- esco_cp4[, .SD[1], by = idesco_level_4]
  esco_candidates <- esco_cp4[, .(n_cand = .N), by = idesco_level_4]

  # 5d. Merge skills with labeled data -----
  train_skills <- merge(
    skills_dt,
    labeled[, .(general_id, cp2021_id_level_4, idesco_level_4)],
    by = "general_id"
  )
  test_skills <- skills_dt[general_id %in% unlabeled$general_id]

  # 5e. Process ESCO groups -----
  processable <- intersect(
    unique(unlabeled$idesco_level_4[!is.na(unlabeled$idesco_level_4)]),
    unique(labeled$idesco_level_4)
  )

  no_match_gids <- unlabeled[
    is.na(idesco_level_4) | !idesco_level_4 %in% processable,
    general_id
  ]

  results <- vector("list", length(processable) + 3L)
  ri <- 0L

  # Rows with no usable ESCO code. With rescue_no_match = TRUE those that carry
  # skills are sent to an unrestricted k-NN over the whole labeled pool instead
  # of being abandoned; the rest stay no_match.
  rescued <- NULL
  if (length(no_match_gids) > 0L && isTRUE(rescue_no_match)) {
    rescue_skills <- skills_dt[general_id %chin% no_match_gids]
    rescue_gids <- unique(rescue_skills$general_id)
    if (length(rescue_gids) > 0L) {
      if (verbose) {
        message(sprintf(
          "predict_cp4_knn: rescuing %d of %d no_match rows via global k-NN",
          length(rescue_gids),
          length(no_match_gids)
        ))
      }
      if (has_sector) {
        r_sect <- unlabeled[match(rescue_gids, general_id), idsector]
        tr_gids_all <- unique(train_skills$general_id)
        tr_sect_all <- stats::setNames(
          labeled[match(tr_gids_all, general_id), idsector],
          tr_gids_all
        )
      } else {
        r_sect <- rep(NA_character_, length(rescue_gids))
        tr_sect_all <- NULL
      }
      rescued <- .global_knn_vote(
        test_gids = rescue_gids,
        test_skills = rescue_skills,
        train_skills = train_skills,
        test_sectors = r_sect,
        train_sectors = tr_sect_all,
        k = rescue_k,
        sector_boost = sector_boost,
        max_train = rescue_max_train
      )
    }
  }

  if (!is.null(rescued)) {
    ri <- ri + 1L
    results[[ri]] <- rescued
    no_match_gids <- setdiff(no_match_gids, rescued$general_id)
  }

  if (length(no_match_gids) > 0L) {
    ri <- ri + 1L
    results[[ri]] <- data.table::data.table(
      general_id = no_match_gids,
      cp2021_id_level_4 = NA_character_,
      confidence = 0,
      method = "no_match"
    )
  }

  # Sector lookups
  if (has_sector) {
    train_sector_lu <- labeled[, .(general_id, idsector)]
    test_sector_lu <- unlabeled[, .(general_id, idsector)]
  }

  n_processed <- 0L
  for (esco in processable) {
    test_gids <- unlabeled[idesco_level_4 == esco, general_id]
    if (length(test_gids) == 0L) {
      next
    }

    freq_cp4 <- esco_mode[idesco_level_4 == esco, cp2021_id_level_4]
    n_cand <- esco_candidates[idesco_level_4 == esco, n_cand]

    # Single candidate: direct assignment
    if (n_cand == 1L) {
      ri <- ri + 1L
      results[[ri]] <- data.table::data.table(
        general_id = test_gids,
        cp2021_id_level_4 = freq_cp4,
        confidence = 1.0,
        method = "single_candidate"
      )
      next
    }

    # Get skills for this ESCO group
    trsk <- train_skills[idesco_level_4 == esco]
    tsk <- test_skills[general_id %in% test_gids]
    local_skills <- sort(unique(c(
      tsk$escoskill_level_3,
      trsk$escoskill_level_3
    )))

    # Sector vectors
    if (has_sector && sector_boost != 1.0) {
      tsect <- test_sector_lu[match(test_gids, general_id), idsector]
      train_gids_esco <- unique(trsk$general_id)
      trsect <- train_sector_lu[match(train_gids_esco, general_id), idsector]
    } else {
      tsect <- rep(NA_character_, length(test_gids))
      trsect <- NULL
    }

    ri <- ri + 1L
    results[[ri]] <- .jaccard_knn_vote(
      test_gids = test_gids,
      test_skills = tsk,
      train_skills = trsk,
      skill_levels = local_skills,
      test_sectors = tsect,
      train_sectors = trsect,
      freq_cp4 = freq_cp4,
      k = k,
      sector_boost = sector_boost,
      max_train = max_train,
      dense_budget = dense_budget
    )

    n_processed <- n_processed + 1L
    if (verbose && n_processed %% 100L == 0L) {
      message(sprintf(
        "  %d / %d ESCO groups",
        n_processed,
        length(processable)
      ))
    }
  }

  result <- data.table::rbindlist(results[seq_len(ri)], use.names = TRUE)

  if (verbose) {
    msg <- result[, .N, by = method]
    message(sprintf(
      "predict_cp4_knn: %d predictions (%s)",
      nrow(result),
      paste(sprintf("%s=%d", msg$method, msg$N), collapse = ", ")
    ))
  }

  result[]
}
