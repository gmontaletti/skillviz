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
  # The restrictor, the sentinel set and the mode tie-break are pinned to their
  # legacy values here on purpose. This function is consumed by the targets
  # pipeline and the container, so its output must not move; anyone wanting the
  # sharper ESCO level-5 restrictor at level 4 should truncate a
  # predict_cp5_knn() prediction instead.
  .predict_cp_knn(
    postings = postings,
    skills = skills,
    target_col = "cp2021_id_level_4",
    restrictor_col = "idesco_level_4",
    restrictor_na = character(0),
    mode_tiebreak = "emission",
    k = k,
    sector_boost = sector_boost,
    max_train = max_train,
    dense_budget = dense_budget,
    rescue_no_match = rescue_no_match,
    rescue_k = rescue_k,
    rescue_max_train = rescue_max_train,
    verbose = verbose,
    caller = "predict_cp4_knn"
  )
}


# 6. .predict_cp_knn -----

# The level-generic engine behind predict_cp4_knn() and predict_cp5_knn().
#
# The algorithm never inspects the target code: it restricts candidates to the
# codes observed for an ESCO group, then votes among neighbours. Only the
# column NAME is level-specific. So rather than parameterise every expression,
# the target column is renamed to the canonical `cp2021_id_level_4` on entry
# and renamed back on exit. .jaccard_knn_vote(), .global_knn_vote() and
# src/global_knn.cpp are therefore untouched by the addition of level 5, and
# the CP4 path runs literally the same code it ran before -- the rename is a
# no-op when target_col is already `cp2021_id_level_4`.
#
# The consequence to keep in mind when reading the internals: a column named
# `cp2021_id_level_4` holds whatever level the caller asked for.
.predict_cp_knn <- function(
  postings,
  skills,
  target_col,
  restrictor_col = "idesco_level_4",
  restrictor_na = character(0),
  mode_tiebreak = c("emission", "code"),
  k = 7L,
  sector_boost = 3.0,
  max_train = 50000L,
  dense_budget = 2e8,
  rescue_no_match = FALSE,
  rescue_k = 10L,
  rescue_max_train = 200000L,
  verbose = TRUE,
  caller = "predict_cp_knn"
) {
  mode_tiebreak <- match.arg(mode_tiebreak)

  # 5a. Input validation -----
  check_columns(
    postings,
    c("general_id", restrictor_col, target_col),
    caller = caller
  )
  check_columns(
    skills,
    c("general_id", "escoskill_level_3"),
    caller = caller
  )

  has_sector <- "idsector" %in% names(postings)
  if (!has_sector && sector_boost != 1.0) {
    if (verbose) {
      message(caller, ": idsector not found, sector boost disabled")
    }
    sector_boost <- 1.0
  }

  # 5b. Split labeled / unlabeled -----
  # Only the four columns the algorithm reads are carried through. Selecting
  # them is what makes the renames below safe: postings from the container hold
  # cp2021_id_level_4 AND cp2021_id_level_5, and idesco_level_4 AND
  # idesco_level_5. Renaming one onto a live column would leave two identically
  # named columns with the wrong one shadowing.
  stopifnot(
    "restrictor_col and target_col must differ" = !identical(
      restrictor_col,
      target_col
    )
  )
  keep <- c("general_id", restrictor_col, target_col)
  if (has_sector) {
    keep <- c(keep, "idsector")
  }
  dt <- data.table::copy(postings)[, keep, with = FALSE]
  if (restrictor_col != "idesco_level_4") {
    data.table::setnames(dt, restrictor_col, "idesco_level_4")
  }
  if (target_col != "cp2021_id_level_4") {
    data.table::setnames(dt, target_col, "cp2021_id_level_4")
  }
  dt[, general_id := as.character(general_id)]

  # Sentinel restrictor values, normalised to NA so they fall through to the
  # no_match branch below rather than forming a group.
  #
  # This matters most at ESCO level 5, whose missing marker is the literal
  # string "Unclassifiable", never NA. Left alone it is a perfectly good group
  # value, so ~123k unlabeled and ~45k labeled rows would collapse into one
  # pseudo-group: no error, just a very large similarity block and confident
  # nonsense. Unlike an empty idsector -- which is deliberately NOT normalised,
  # because doing so was measured to cost 1.51 pp (see predict_cp4_knn Details)
  # -- the sentinel carries no signal: it means no ESCO code exists at any level.
  #
  # Defaulting to character(0) keeps the level-4 path a provable no-op.
  if (length(restrictor_na)) {
    # %in%, not %chin%: the restrictor column is character at level 5 but
    # arrives integer from some level-4 callers, and %chin% errors on those.
    # The column is deliberately NOT coerced -- its type decides the grouping
    # order, and so the "emission" tie-break.
    n_sentinel <- dt[idesco_level_4 %in% restrictor_na, .N]
    if (n_sentinel > 0L) {
      dt[
        idesco_level_4 %in% restrictor_na,
        idesco_level_4 := NA_character_
      ]
      if (verbose) {
        message(sprintf(
          "%s: %d rows carry a sentinel restrictor (%s), treated as no ESCO code",
          caller,
          n_sentinel,
          paste(restrictor_na, collapse = ", ")
        ))
      }
    }
  }
  skills_dt <- data.table::copy(skills)
  skills_dt[, general_id := as.character(general_id)]

  labeled <- dt[!is.na(cp2021_id_level_4) & nzchar(cp2021_id_level_4)]
  unlabeled <- dt[is.na(cp2021_id_level_4) | !nzchar(cp2021_id_level_4)]

  if (nrow(unlabeled) == 0L) {
    if (verbose) {
      message(caller, ": no unlabeled rows, returning empty table")
    }
    empty <- data.table::data.table(
      general_id = character(0),
      cp2021_id_level_4 = character(0),
      confidence = numeric(0),
      method = character(0)
    )
    if (target_col != "cp2021_id_level_4") {
      data.table::setnames(empty, "cp2021_id_level_4", target_col)
    }
    return(empty)
  }

  if (verbose) {
    message(sprintf(
      "%s: %d labeled, %d unlabeled",
      caller,
      nrow(labeled),
      nrow(unlabeled)
    ))
  }

  # 5c. Build de facto crosswalk -----
  esco_cp4 <- labeled[, .N, by = .(idesco_level_4, cp2021_id_level_4)]
  # mode_tiebreak = "emission" orders on the count alone, so codes with equal
  # counts keep the order `by=` emitted them in -- which depends on the caller's
  # row order. That is the legacy level-4 behaviour and is pinned there because
  # changing it would move predictions. It is not safe at level 5: the median
  # labelled pool per group falls from 485 to 33 over a mean 8.94 candidates,
  # so exact ties at the top of a group stop being exotic. "code" appends the
  # CP code as a third key, matching build_cpi_esco_crosswalk_sql().
  if (mode_tiebreak == "code") {
    data.table::setorder(esco_cp4, idesco_level_4, -N, cp2021_id_level_4)
  } else {
    data.table::setorder(esco_cp4, idesco_level_4, -N)
  }
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
          "%s: rescuing %d of %d no_match rows via global k-NN",
          caller,
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
      "%s: %d predictions (%s)",
      caller,
      nrow(result),
      paste(sprintf("%s=%d", msg$method, msg$N), collapse = ", ")
    ))
  }

  if (target_col != "cp2021_id_level_4") {
    data.table::setnames(result, "cp2021_id_level_4", target_col)
  }
  result[]
}


# 7. predict_cp5_knn -----

#' Predict CP2021 level-5 codes via sector-boosted Jaccard k-NN
#'
#' Assigns CP2021 level-5 codes (unità professionali) to unlabeled job
#' announcements. Same algorithm as [predict_cp4_knn()] one level down: the
#' ESCO level-4 code restricts the candidate space to the CP5 codes observed
#' in labeled data for that group (de facto crosswalk), then Jaccard k-NN on
#' binary skill vectors disambiguates, with same-sector neighbors boosted.
#'
#' @inheritParams predict_cp4_knn
#' @param postings A data.table with columns: `general_id` (character),
#'   `idesco_level_4` (integer or character), `cp2021_id_level_5` (character,
#'   `NA` or `""` for unlabeled rows). Optionally `idsector` (character).
#'   A `cp2021_id_level_4` column, if present, is ignored.
#' @param sector_boost Numeric multiplier for same-sector neighbors in the
#'   weighted vote. Set to 1.0 to disable sector boosting. **Defaults to 5.0
#'   here, not 3.0**: that is the measured optimum at this level (boost 1 loses
#'   2.3 pp) and it matches what the pipeline and container already pass to
#'   [predict_cp4_knn()], whose own 3.0 default is a legacy value production
#'   overrides.
#' @param restrictor Character naming the column that restricts the candidate
#'   space, one of `"idesco_level_4"` (default) or `"idesco_level_5"`. Level 5
#'   is the sharper restrictor — 2,714 groups against 399, a mean 8.94 CP5
#'   candidates against 32.64 — and scores 89.1% against 85.9% on
#'   k-NN-decided rows. It is not yet the default; see Details.
#' @param restrictor_na Character vector of sentinel values in `restrictor`
#'   that mean "no occupation code", normalised to `NA` so those rows fall
#'   through to `method = "no_match"`. `NULL` (default) resolves to
#'   `"Unclassifiable"` when `restrictor = "idesco_level_5"` and to nothing
#'   otherwise. **Do not set this to `character(0)` at level 5**: the sentinel
#'   is a string, not `NA`, so leaving it in place silently merges every
#'   ESCO-less announcement into a single enormous pseudo-group.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{general_id}{Announcement identifier.}
#'     \item{cp2021_id_level_5}{Predicted CP2021 level-5 code.}
#'     \item{confidence}{Weighted vote share of the winning class (0--1).}
#'     \item{method}{One of `"knn"`, `"frequency"`, `"single_candidate"`,
#'       `"no_match"`, or `"knn_global"` when `rescue_no_match = TRUE`.}
#'   }
#'
#' @details
#' Validated on the 24-month production window
#' (`skillviz_workflow/run_cp5_knn.R`, 5 stratified contemporaneous splits of
#' 50,000 test rows, holdout stratified on CP5). **CP5 accuracy 85.9% on
#' k-NN-decided rows at k=7 and `sector_boost = 5.0`**, against 86.2% for
#' [predict_cp4_knn()] on the same rows: the finer level costs about 0.25 pp.
#' Over all test rows, including the fallback cascade, CP5 is 79.9%.
#'
#' Level 5 is barely a harder restricted problem than level 4, which is why the
#' cost is so small. Row-weighted over the labeled window, an ESCO level-4 group
#' offers 65.8 CP5 candidates against 61.7 CP4 ones (modal-share accuracy 61.9%
#' vs 62.7%). By contrast CP3 -> CP4 costs 5.7 pp of modal share. The hierarchy
#' is also narrow: of 813 CP5 codes in `staging.dim_cp2021_5`, 510 CP4 parents
#' carry a mean 1.60 children, and 340 have exactly one, so 67.2% of labeled
#' rows sit under a CP4 whose CP5 is determined by the classification alone.
#'
#' Two axes were closed by that harness. **Do not re-test them.**
#'
#' First, **predicting CP5 directly beats refining the CP4 winner.** Mapping
#' the [predict_cp4_knn()] argmax to the modal CP5 within its (ESCO group, CP4)
#' cell loses 0.877 pp at k=7, on 5 of 5 splits (sd 0.039), changing 1.30% of
#' predictions. This looked like the favourite going in: modal CP5 within the
#' group and the *true* CP4 is 98.66% accurate over 1.60 candidates, which
#' inverts the break-even arithmetic that closed the CP3 -> CP4 second stage in
#' [predict_cp4_knn()]. The mechanism it missed is that conditioning on the
#' *predicted* CP4 inherits every CP4 error, while a direct CP5 vote can land
#' on the right code under a CP4 the level-4 argmax got wrong.
#'
#' Second, **truncating the CP5 winner reproduces the CP4 winner.** Taking
#' `substring(pred, 1L, 7L)` instead of running [predict_cp4_knn()] moves CP4
#' accuracy by +0.004 pp at k=7 (|delta| <= 0.010 across k in 5, 7, 10),
#' changing 0.07% of predictions -- two orders of magnitude under this
#' harness's 0.244 pp tie-break noise floor. One model therefore serves both
#' levels, and the neighbour search need not be run twice: it is 98.6% of
#' runtime, the vote 1.4%.
#'
#' `k = 7` and `sector_boost = 5.0` remain optimal at this cardinality (swept
#' over k in 5, 7, 10 and boost in 1, 3, 5; boost 1 loses 2.3 pp). The Jaccard
#' kernel is inherited unchanged and was closed at level 4.
#'
#' Confidence is monotone in accuracy across all ten deciles and is the
#' intended filter: the top 10% of k-NN rows by confidence is 97.1% accurate,
#' the top 50% 96.9%, against 85.9% overall. It is mildly overconfident at the
#' top, where a mean confidence of 100% scores 96.7%. **CP5 thresholds must be
#' derived from this table rather than inherited from level 4**: the same k
#' votes spread over more classes, so the same confidence value means something
#' different here.
#'
#' **ESCO level 5 is a better restrictor than level 4** and is available via
#' `restrictor = "idesco_level_5"`, but is not yet the default. It scores +3.2 pp
#' at CP5 (89.1% vs 85.9%) and +3.1 pp at CP4, halving the candidate space (23.9
#' vs 65.8) for a coverage loss of 0.018 pp, and wins in every training-pool-size
#' band above 10 labeled rows — losing only in the thinnest (66.0% vs 73.1% at
#' 1-10 rows). It also shifts `single_candidate` from 0.01% to 0.54% of rows and
#' puts 3.2% of rows in groups with fewer than 50 neighbors.
#'
#' **ESCO level 5 is the default restrictor.** It was adopted after the
#' walk-forward gate in `skillviz_workflow/run_cp5_restrictor_temporal.R`, which
#' trains on every labeled month before each of the last 6 and scores that month
#' alone. The restrictor did better under temporal shift than contemporaneously,
#' not worse:
#'
#' - CP5 **+3.95 pp** paired on k-NN-decided rows, positive in **6 of 6** months
#'   (range +3.61 to +4.54), against a +1.0 pp bar;
#' - CP4 +3.81 pp, and CP4 recovered by truncating the level-5 argmax is within
#'   0.02 pp of that, so one pass would serve both levels;
#' - coverage cost +0.042 pp; positive in **every** training-pool band, including
#'   1-10 rows (+5.8 pp), which reverses the contemporaneous finding;
#' - absolute levels fall as expected under walk-forward: ESCO4 CP4 80.3% here
#'   against 86.2% contemporaneously.
#'
#' The first run of that gate failed a secondary criterion — confidence
#' monotone across deciles in at least 5 of 6 months, of which level 5 managed
#' 4 — and the restrictor was **not** adopted on that run. The criterion was
#' then found to be defective rather than merely inconvenient: it rejected the
#' *incumbent* level-4 arm harder still (3 of 6), because deciles 5 through 10
#' all sit at exactly 100% confidence, one tied block covering ~60% of rows
#' where the decile split is arbitrary and there is no ordering to test.
#'
#' It was replaced, **before** re-running, by two comparative criteria:
#' expected calibration error relative to level 4 (margin +1.0 pp), and
#' monotonicity restricted to the deciles where confidence actually varies. On
#' the corrected gate level 5 passes everything, and on a proper calibration
#' metric it is **better** calibrated than the incumbent, not worse: ECE
#' -0.81 pp on average and lower in all 6 months, with monotonicity 6 of 6.
#'
#' Two caveats survive regardless: every figure above is labeled-on-labeled,
#' unlabeled postings are 1.55x longer, and labelling is strongly non-random
#' across ESCO groups and sources (see [build_esco_cp_crosswalk()]). The gain is
#' *not* established on the population the coder is actually applied to.
#'
#' One measurement from that run matters to the container even though the
#' restrictor was not adopted: an ESCO5-restricted level-5 prediction and an
#' ESCO4-restricted level-4 prediction **disagree on 12.2% of rows**. Running
#' the two levels under different restrictors is therefore not viable — the
#' container drops a level-5 code whose parent contradicts the level-4 column,
#' so a split configuration would silently discard those rows. Both levels move
#' together or neither does.
#'
#' A **hierarchical backoff** — use level 5 where its labeled pool is thick
#' enough, else fall back to the level-4 parent — was measured for pool
#' thresholds 10, 30, 50 and 100, in sample and held out in time, and **loses**
#' 0.3 to 3.0 pp while never improving coverage: it only moves rows between
#' branches, and just 0.15% of held-out rows have no level-5 group in training.
#' Pure level 5 with a level-4 null-guard for the residual is the right shape.
#' Do not re-test this axis. (Established on modal-share accuracy, a proxy for
#' k-NN accuracy, so it is strong evidence rather than a direct measurement.)
#'
#' Rows carrying `idesco_level_5 = "Unclassifiable"` have no ESCO code at any
#' level and so get `method = "no_match"` (5.6% of rows) unless
#' `rescue_no_match = TRUE`. The container's vtreat + xgboost path, which
#' serves those rows at level 3, cannot be pushed to level 5.
#'
#' @seealso [predict_cp4_knn()] for the level-4 model and the full record of
#'   closed experiments at that level.
#'
#' @examples
#' postings <- data.table::data.table(
#'   general_id = as.character(1:10),
#'   idesco_level_5 = rep(c("1000.1", "2000.1"), each = 5),
#'   cp2021_id_level_5 = c("1.1.1.1.1", "1.1.1.1.2", "1.1.1.1.1", NA, NA,
#'                          "2.2.2.1.0", "2.2.2.1.0", "2.2.2.2.0", NA, NA),
#'   idsector = rep(c("C", "F"), each = 5)
#' )
#' skills <- data.table::data.table(
#'   general_id = as.character(c(1,1,2,2,3,3,4,4,5,5,
#'                                6,6,7,7,8,8,9,9,10,10)),
#'   escoskill_level_3 = c("s1","s2","s2","s3","s1","s2","s1","s3","s2","s3",
#'                          "s4","s5","s4","s5","s5","s6","s4","s6","s5","s6")
#' )
#' result <- predict_cp5_knn(postings, skills, k = 3L, sector_boost = 1.0)
#'
#' @export
predict_cp5_knn <- function(
  postings,
  skills,
  restrictor = c("idesco_level_5", "idesco_level_4"),
  restrictor_na = NULL,
  k = 7L,
  sector_boost = 5.0,
  max_train = 50000L,
  dense_budget = 2e8,
  rescue_no_match = FALSE,
  rescue_k = 10L,
  rescue_max_train = 200000L,
  verbose = TRUE
) {
  restrictor <- match.arg(restrictor)
  # NULL means "the sentinel this restrictor actually uses". Level 5 marks a
  # missing occupation with the string "Unclassifiable"; level 4 arrives NA.
  if (is.null(restrictor_na)) {
    restrictor_na <- if (restrictor == "idesco_level_5") {
      "Unclassifiable"
    } else {
      character(0)
    }
  }

  .predict_cp_knn(
    postings = postings,
    skills = skills,
    target_col = "cp2021_id_level_5",
    restrictor_col = restrictor,
    restrictor_na = restrictor_na,
    # Deterministic, unlike the level-4 path: level 5 has no production
    # consumer yet, so pinning the tie-break is free today and will not be
    # once the container runs it.
    mode_tiebreak = "code",
    k = k,
    sector_boost = sector_boost,
    max_train = max_train,
    dense_budget = dense_budget,
    rescue_no_match = rescue_no_match,
    rescue_k = rescue_k,
    rescue_max_train = rescue_max_train,
    verbose = verbose,
    caller = "predict_cp5_knn"
  )
}


# 8. build_esco_cp_crosswalk -----

#' Build a full ESCO-to-CP2021 crosswalk with its candidate structure
#'
#' Derives, from labelled announcements, the empirical mapping between an ESCO
#' occupation code and the CP2021 codes it is observed with. Returns both the
#' full candidate structure and a per-group summary covering the whole ESCO
#' classification, not only the codes that happen to appear in the data.
#'
#' @param postings A data.table with at least `restrictor_col` and
#'   `target_col`. Rows whose target is `NA` or the empty string are treated as
#'   unlabelled: they contribute to `n_postings` and `coverage_labelled` but not
#'   to the candidate counts.
#' @param restrictor_col Character naming the ESCO column, default
#'   `"idesco_level_5"`.
#' @param target_col Character naming the CP2021 column, default
#'   `"cp2021_id_level_5"`.
#' @param universe Optional data.table giving the full classification domain of
#'   `restrictor_col`, so codes that never appear in `postings` still get a row
#'   with `status = "absent"`. Must contain `restrictor_col`; a second column,
#'   if present, is carried through as the parent code. `NULL` (default)
#'   restricts the output to observed codes.
#' @param strata_col Optional character naming a column — in practice `source`
#'   — along which labelling is known to be non-random. When supplied, each
#'   group's modal code is recomputed with every stratum post-stratified to its
#'   share of that group's *unlabelled* rows, and `modal_stable` reports whether
#'   the mode survives. `NULL` (default) leaves the three reweighting columns
#'   `NA`.
#' @param restrictor_na Character vector of sentinel values meaning "no
#'   occupation code" (default `"Unclassifiable"`). These get
#'   `status = "unclassifiable"` and contribute no candidates.
#' @param min_n Integer: drop candidate pairs supported by fewer than this many
#'   labelled announcements (default 0, keep all). Shares and ranks are computed
#'   *after* the drop.
#' @param verbose Logical: print a one-line summary (default TRUE).
#'
#' @return A list of two data.tables.
#'   \describe{
#'     \item{candidates}{One row per observed (ESCO, CP) pair: the two code
#'       columns, the parent CP4 code when the target is level 5, `n`, `share`,
#'       `rank` and `cum_share`. Ordered by group, then descending `n`, then
#'       code.}
#'     \item{groups}{One row per ESCO code, covering `universe` when supplied:
#'       the code, its parent, `status`, `n_labelled`, `n_postings`,
#'       `coverage_labelled`, `n_candidates`, `n_eff`, `code_modal`,
#'       `modal_share`, `top3_share`, `modal_tied` and `modal_reliable`.}
#'   }
#'
#' @details
#' **This is a candidate restrictor, not a lookup.** Joining an ESCO code to its
#' modal CP code and stopping there is wrong for about one announcement in four.
#' Measured on the 24-month window at ESCO level 5 (2,714 groups, 761,917
#' doubly-labelled rows): the modal CP5 is right **73.4%** of the time,
#' row-weighted. Concentration rises slowly — top-2 85.8%, top-3 90.3%, top-5
#' 94.3% — and only **0.46%** of rows sit in a group whose modal code takes
#' 100% of it. Several of the largest groups are genuinely split: `8322.6`
#' *autista privato* has 30 candidates over 13,607 rows with a 49.3% mode.
#'
#' What closes the gap is the k-NN vote inside the candidate set:
#' [predict_cp5_knn()] scores 89.1% on the rows it decides. Use `modal_reliable`
#' (`modal_share >= 0.90 & n_labelled >= 30`, about a third of rows) if a
#' deterministic map is genuinely needed, and treat the rest as ambiguous.
#'
#' **Read `n_eff`, not `n_candidates`.** The nominal candidate count is
#' dominated by a long tail of pairs seen once or twice, and that tail grows
#' with the evidence rather than shrinking: over the whole store an ESCO
#' level-5 group lists 32.1 candidates row-weighted, against 8.9 over a
#' 24-month window, while the modal share barely moves (73.0% against 73.4%).
#' The inverse-Simpson `n_eff` is 2.28 either way — the extra candidates carry
#' almost no mass. Use `min_n` to trim the tail when the table is consumed as a
#' restrictor rather than as a description.
#'
#' **`modal_stable` is not decoration — 13.7% of unlabelled rows fail it.**
#' Supplying `strata_col = "source"` recomputes each mode with the coded sample
#' post-stratified to the source mix of the group's *uncoded* rows, which is the
#' population the crosswalk is applied to. Measured over the whole store: the
#' modal CP5 changes for 497 of 2,708 reweightable groups (18.4%), covering
#' **13.66% of unlabelled announcements**. The overall modal share barely moves
#' (51.35% to 50.85%), so this is codes swapping places within ambiguous groups
#' rather than a wholesale collapse.
#'
#' The movement tracks coverage exactly as the mechanism predicts, which is what
#' distinguishes it from noise: 24.8% of groups move in the 0-10% labelled band,
#' 24.4% at 10-25%, 18.2% at 25-50%, 10.3% at 50-75% and 3.7% at 75-100%. The
#' underlying heterogeneity is large — within a single ESCO level-5 group, the
#' highest- and lowest-coverage source differ in their CP5 distribution by a
#' median total-variation distance of 0.19, and 38% of comparable groups exceed
#' 0.25.
#'
#' This threatens the *crosswalk*, not the choice of restrictor: the same bias
#' applies at ESCO level 4. Treat a group with `modal_stable = FALSE` as
#' describing its coded announcements rather than its occupation.
#'
#' `coverage_labelled` is reported because labelling is strongly non-random:
#' 1,200 ESCO level-5 groups covering 491,739 announcements are under 25%
#' labelled, and labelling rates vary by source from 16.6% to 55.8%. A group's
#' modal code is estimated from whichever announcements happened to be coded, so
#' a low `coverage_labelled` is a warning about that group's row in this table.
#'
#' Level 5 is a much sharper restrictor than level 4 — 2,714 groups against 399,
#' a mean 8.94 candidates against 32.64, modal share 73.4% against 61.9%. A
#' hierarchical backoff (use level 5 where the pool is thick, else the level-4
#' parent) was measured for pool thresholds 10, 30, 50 and 100, both in sample
#' and held out in time, and **loses** 0.3 to 3.0 pp while never improving
#' coverage: it only moves rows between branches. Do not re-test that axis.
#'
#' The container does **not** read this table, and should not be changed to.
#' [predict_cp5_knn()] derives its candidate set from the labelled rows of the
#' run it is given, which keeps the invariant that every candidate has at least
#' one training row. A persisted candidate set would break that — a group could
#' present a single candidate the model has never seen an example of, and be
#' assigned at `confidence = 1.0`.
#'
#' @examples
#' postings <- data.table::data.table(
#'   idesco_level_5 = c(rep("1000.1", 4), rep("1000.2", 3), "Unclassifiable"),
#'   cp2021_id_level_5 = c(
#'     "1.1.1.1.1", "1.1.1.1.1", "1.1.1.1.2", NA,
#'     "2.2.2.2.0", "2.2.2.2.0", "2.2.2.2.0", "3.3.3.3.0"
#'   )
#' )
#' cw <- build_esco_cp_crosswalk(postings, verbose = FALSE)
#' cw$candidates
#' cw$groups
#'
#' @seealso [predict_cp5_knn()], which consumes the same structure derived
#'   in-memory.
#' @export
build_esco_cp_crosswalk <- function(
  postings,
  restrictor_col = "idesco_level_5",
  target_col = "cp2021_id_level_5",
  universe = NULL,
  strata_col = NULL,
  restrictor_na = "Unclassifiable",
  min_n = 0L,
  verbose = TRUE
) {
  check_columns(
    postings,
    c(restrictor_col, target_col),
    caller = "build_esco_cp_crosswalk"
  )
  stopifnot(
    "restrictor_col and target_col must differ" = !identical(
      restrictor_col,
      target_col
    )
  )

  # 8a. Canonical internal names -----
  keep <- c(restrictor_col, target_col)
  if (!is.null(strata_col)) {
    check_columns(postings, strata_col, caller = "build_esco_cp_crosswalk")
    keep <- c(keep, strata_col)
  }
  dt <- data.table::copy(postings)[, keep, with = FALSE]
  data.table::setnames(
    dt,
    if (is.null(strata_col)) c("grp", "code") else c("grp", "code", "stratum")
  )
  dt[, grp := as.character(grp)]
  dt[, code := as.character(code)]
  dt[, sentinel := !is.na(grp) & grp %chin% restrictor_na]

  # A row is labelled when it carries a target code. itaposts writes the
  # missing marker as the empty string in some stores and NA in others.
  dt[, labelled := !is.na(code) & nzchar(code)]

  # 8b. Candidate pairs -----
  lab <- dt[!sentinel & !is.na(grp) & labelled, .(n = .N), by = .(grp, code)]
  if (min_n > 0L) {
    lab <- lab[n >= min_n]
  }
  data.table::setorder(lab, grp, -n, code)
  lab[,
    `:=`(
      share = n / sum(n),
      rank = seq_len(.N),
      cum_share = cumsum(n) / sum(n)
    ),
    by = grp
  ]

  # 8c. Per-group summary -----
  # n_postings counts every announcement in the group, labelled or not, so
  # coverage_labelled says how much of the group the mode was estimated from.
  seen <- dt[
    !sentinel & !is.na(grp),
    .(
      n_postings = .N,
      n_labelled = sum(labelled)
    ),
    by = grp
  ]

  agg <- lab[,
    .(
      n_candidates = .N,
      # Inverse Simpson: how many candidates the group effectively has, as
      # opposed to how many it nominally lists.
      n_eff = 1 / sum(share^2),
      code_modal = code[1L],
      modal_share = share[1L],
      top3_share = cum_share[min(.N, 3L)],
      modal_tied = .N > 1L && n[1L] == n[2L]
    ),
    by = grp
  ]

  groups <- merge(seen, agg, by = "grp", all.x = TRUE)

  # Sentinel groups get a row so the table accounts for every announcement.
  sent <- dt[
    sentinel == TRUE,
    .(n_postings = .N, n_labelled = sum(labelled)),
    by = grp
  ]
  if (nrow(sent)) {
    groups <- rbind(groups, sent, fill = TRUE)
  }

  # 8d. Extend to the full classification domain -----
  parent <- NULL
  if (!is.null(universe)) {
    check_columns(universe, restrictor_col, caller = "build_esco_cp_crosswalk")
    uni <- unique(data.table::as.data.table(universe), by = restrictor_col)
    pcol <- setdiff(names(uni), restrictor_col)[1L]
    uni <- if (is.na(pcol)) {
      uni[, .(grp = as.character(get(restrictor_col)))]
    } else {
      parent <- pcol
      uni[, .(
        grp = as.character(get(restrictor_col)),
        parent = as.character(get(pcol))
      )]
    }
    groups <- merge(groups, uni, by = "grp", all = TRUE)
    groups[is.na(n_postings), `:=`(n_postings = 0L, n_labelled = 0L)]
  }

  groups[,
    status := data.table::fcase(
      grp %chin% restrictor_na , "unclassifiable" ,
      n_postings == 0L         , "absent"         ,
      is.na(n_candidates)      , "no_target"      ,
      default = "covered"
    )
  ]
  groups[,
    coverage_labelled := data.table::fifelse(
      n_postings > 0L,
      n_labelled / n_postings,
      NA_real_
    )
  ]
  # A mode is safe to use as a lookup only when it is both concentrated and
  # estimated from enough announcements to mean anything.
  groups[,
    modal_reliable := !is.na(modal_share) &
      modal_share >= 0.90 &
      n_labelled >= 30L
  ]
  groups[is.na(modal_tied), modal_tied := FALSE]

  # 8d bis. Is the mode an artefact of WHICH rows got labelled? -----
  # Post-stratify each stratum's contribution to its share of the group's
  # UNLABELLED rows and see whether the mode survives. This tests
  #   code independent of labelled | (group, stratum)
  # in place of the far stronger assumption the naive count makes,
  #   code independent of labelled | group
  # which the data contradicts: within one ESCO level-5 group, sources differ in
  # their CP5 distribution by a median total-variation distance of 0.19, and 38%
  # of comparable groups exceed 0.25.
  if (!is.null(strata_col)) {
    w_target <- dt[
      !sentinel & !is.na(grp) & !labelled,
      .N,
      by = .(grp, stratum)
    ]
    w_target[, w := N / sum(N), by = grp]
    n_lab_s <- dt[!sentinel & !is.na(grp) & labelled, .N, by = .(grp, stratum)]
    data.table::setnames(n_lab_s, "N", "n_lab_s")
    rw <- merge(
      dt[!sentinel & !is.na(grp) & labelled, .(grp, stratum, code)],
      merge(w_target[, .(grp, stratum, w)], n_lab_s, by = c("grp", "stratum")),
      by = c("grp", "stratum")
    )
    if (nrow(rw)) {
      rw_agg <- rw[, .(wt = sum(w / n_lab_s)), by = .(grp, code)]
      rw_agg[, sh := wt / sum(wt), by = grp]
      data.table::setorder(rw_agg, grp, -sh, code)
      rw_mode <- rw_agg[
        !duplicated(rw_agg, by = "grp"),
        .(grp, code_modal_rw = code, modal_share_rw = sh)
      ]
      groups <- merge(groups, rw_mode, by = "grp", all.x = TRUE)
      groups[,
        modal_stable := data.table::fifelse(
          is.na(code_modal) | is.na(code_modal_rw),
          NA,
          code_modal == code_modal_rw
        )
      ]
    }
  }
  if (!"modal_stable" %in% names(groups)) {
    groups[, `:=`(
      code_modal_rw = NA_character_,
      modal_share_rw = NA_real_,
      modal_stable = NA
    )]
  }

  # 8e. Restore the caller's column names -----
  data.table::setnames(lab, c("grp", "code"), c(restrictor_col, target_col))
  if (target_col == "cp2021_id_level_5") {
    lab[, cp2021_id_level_4 := substr(get(target_col), 1L, 7L)]
    data.table::setcolorder(
      lab,
      c(restrictor_col, target_col, "cp2021_id_level_4")
    )
  }
  data.table::setnames(groups, "grp", restrictor_col)
  if (!is.null(parent)) {
    data.table::setnames(groups, "parent", parent)
  }
  data.table::setcolorder(
    groups,
    c(
      restrictor_col,
      if (!is.null(parent)) parent,
      "status",
      "n_postings",
      "n_labelled",
      "coverage_labelled",
      "n_candidates",
      "n_eff",
      "code_modal",
      "modal_share",
      "top3_share",
      "modal_tied",
      "modal_reliable",
      "code_modal_rw",
      "modal_share_rw",
      "modal_stable"
    )
  )
  data.table::setorder(groups, status)
  data.table::setorderv(groups, restrictor_col)

  if (verbose) {
    st <- groups[, .N, by = status][order(-N)]
    message(sprintf(
      "build_esco_cp_crosswalk: %d groups (%s), %d candidate pairs, modal share %.1f%% row-weighted",
      nrow(groups),
      paste(sprintf("%s=%d", st$status, st$N), collapse = ", "),
      nrow(lab),
      groups[
        status == "covered",
        sum(modal_share * n_labelled) / sum(n_labelled) * 100
      ]
    ))
  }

  list(candidates = lab[], groups = groups[])
}
