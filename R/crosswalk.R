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
#' @param postings A data.table from `normalize_ojv()$postings`. Needs
#'   `general_id`, `idesco_level_4`, `cp2021_id_level_3`, and
#'   `cp2021_level_3`.
#' @param skills A data.table from `normalize_ojv()$skills`. Needs
#'   `general_id` and `escoskill_level_3` (or `ESCOSKILL_LEVEL_3`).
#' @param top_k Integer, number of top CPI predictions per ESCO L4 code
#'   (default: 3).
#' @param alpha Numeric, Laplace smoothing parameter (default: 1.0).
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

  mapped_esco <- esco_cpi_lookup[, unique(idesco_level_4)]
  all_esco <- postings[, unique(idesco_level_4)]
  unmapped_esco <- setdiff(all_esco, mapped_esco)

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
