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
  sector_boost
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
  max_train <- 50000L
  if (length(train_gids) > max_train) {
    sample_idx <- sample.int(length(train_gids), max_train)
    train_gids <- train_gids[sample_idx]
    train_mat <- train_mat[sample_idx, , drop = FALSE]
    if (!is.null(train_sectors)) {
      train_sectors <- train_sectors[sample_idx]
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
  batch_size <- max(1L, as.integer(2e8 / length(train_gids)))
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
#'   sector boosting.
#' @param skills A data.table with columns: `general_id` (character),
#'   `escoskill_level_3` (character).
#' @param k Integer number of nearest neighbors (default 7).
#' @param sector_boost Numeric multiplier for same-sector neighbors in the
#'   weighted vote. Set to 1.0 to disable sector boosting (default 3.0).
#' @param verbose Logical: print progress messages (default TRUE).
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{general_id}{Announcement identifier.}
#'     \item{cp2021_id_level_4}{Predicted CP2021 level-4 code.}
#'     \item{confidence}{Weighted vote share of the winning class (0--1).}
#'     \item{method}{One of `"knn"`, `"frequency"`, `"single_candidate"`,
#'       `"no_match"`.}
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
#' with k=7 and sector_boost=3.0, vs 62.6% frequency baseline.
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

  results <- vector("list", length(processable) + 2L)
  ri <- 0L

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
      sector_boost = sector_boost
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
