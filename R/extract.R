# Skill extraction and profession-skill profiles -----

# 1. extract_specific_skills_cpi3 -----

#' Extract non-diffuse skills at the CPI3 profession level
#'
#' Merges job announcements with skills through a CPI-ESCO crosswalk at
#' the CPI 3-digit level. Skills classified as highly diffuse (based on
#' IDF) are removed, retaining only profession-specific skills.
#'
#' The pipeline reproduces the logic from the extraction script:
#' 1. Merge announcements with the crosswalk on `idesco_level_4`.
#' 2. Merge with skills on `general_id`.
#' 3. Compute document-level IDF, classify diffuse skills.
#' 4. Filter to non-diffuse ("specifiche") skills.
#'
#' @param ann A `data.table` of announcements with at least columns
#'   `general_id` and `idesco_level_4`.
#' @param ski A `data.table` of skills with at least columns `general_id`
#'   and `escoskill_level_3`.
#' @param cpi_esco A `data.table` crosswalk mapping `idesco_level_4` to
#'   `cod_3` and `nome_3`. Typically built by the crosswalk pipeline.
#' @param quantile_trim Numeric, quantile threshold for IDF diffusion
#'   classification. Default: 0.025.
#' @return A `data.table` with columns `general_id`, `escoskill_level_3`,
#'   `cod_3`, `nome_3`. Only non-diffuse (specific) skills are included.
#' @export
extract_specific_skills_cpi3 <- function(
  ann,
  ski,
  cpi_esco,
  quantile_trim = 0.025
) {
  # 1. input validation -----
  check_columns(
    ann,
    c("general_id", "idesco_level_4"),
    caller = "extract_specific_skills_cpi3"
  )
  check_columns(
    ski,
    c("general_id", "escoskill_level_3"),
    caller = "extract_specific_skills_cpi3"
  )
  check_columns(
    cpi_esco,
    c("idesco_level_4", "cod_3", "nome_3"),
    caller = "extract_specific_skills_cpi3"
  )

  # 2. merge announcements with crosswalk -----
  df <- merge(ann, cpi_esco, by = "idesco_level_4", all.x = TRUE, all.y = FALSE)
  df <- unique(df[, .(general_id, idesco_level_4, cod_3, nome_3)])
  df <- df[!is.na(cod_3)]

  # 3. merge with skills -----
  data.table::setkey(ski, general_id)
  data.table::setkey(df, general_id)
  ds <- merge(ski, df, by = "general_id", all.x = TRUE, all.y = FALSE)

  # 4. IDF classification -----
  enne <- data.table::uniqueN(ds$general_id)
  idf_dt <- ds[,
    .(.N, tf = data.table::uniqueN(general_id) / enne),
    .(escoskill_level_3)
  ][, idf := log(1 / tf)][order(idf)]

  threshold_rows <- max(round(nrow(idf_dt) * quantile_trim, 0) - 1L, 0L)
  diffuse_skills <- idf_dt[seq_len(threshold_rows), escoskill_level_3]

  # 5. filter to specific skills -----
  ds[, competenze := "specifiche"]
  ds[escoskill_level_3 %in% diffuse_skills, competenze := "diffuse"]

  out <- ds[
    competenze == "specifiche" & !is.na(cod_3),
    .(general_id, escoskill_level_3, cod_3, nome_3)
  ]

  out[]
}


# 2. build_profession_skill_profile -----

#' Build a comprehensive profession-skill profile table
#'
#' Joins announcements, skills, ISCO classification, a master skills list,
#' and co-occurrence data into a single profession-skill table. This
#' reproduces the join logic from the profession-skill exploration script
#' (08_ski_ann_stru.R).
#'
#' Steps:
#' 1. Merge announcements with ISCO to get `preferredLabel`.
#' 2. Merge skills with announcements on `general_id`.
#' 3. Merge with the master skills list for diffusion metadata.
#' 4. Aggregate to profession-skill level with announcement counts.
#'
#' @param ann A `data.table` of announcements with at least `general_id`,
#'   `idesco_level_4`, `gdate`, `edate`.
#' @param ski A `data.table` of skills with at least `general_id` and
#'   `ESCOSKILL_LEVEL_3`.
#' @param isco A `data.table` of ISCO groups with at least `code` and
#'   `preferredLabel`.
#' @param skillist A `data.table` master skills list with at least
#'   `ESCOSKILL_LEVEL_3` (or `escoskill_level_3`) and `diffusione`.
#' @param cooc Optional `data.table` co-occurrence edge list with `from`,
#'   `to`, `weight`. If provided, it is returned unmodified as an
#'   attribute of the output for downstream graph analysis.
#' @return A `data.table` with columns `preferredLabel`, `ESCOSKILL_LEVEL_3`,
#'   `diffusione`, and `annunci` (number of unique announcements per
#'   profession-skill pair).
#' @export
build_profession_skill_profile <- function(
  ann,
  ski,
  isco,
  skillist,
  cooc = NULL
) {
  # 1. input validation -----
  check_columns(
    ann,
    c("general_id", "idesco_level_4"),
    caller = "build_profession_skill_profile"
  )
  check_columns(ski, c("general_id"), caller = "build_profession_skill_profile")
  check_columns(
    isco,
    c("code", "preferredLabel"),
    caller = "build_profession_skill_profile"
  )

  # Normalize skill column name
  ski_col <- if ("ESCOSKILL_LEVEL_3" %in% names(ski)) {
    "ESCOSKILL_LEVEL_3"
  } else if ("escoskill_level_3" %in% names(ski)) {
    "escoskill_level_3"
  } else {
    stop(
      "build_profession_skill_profile: ski must contain ESCOSKILL_LEVEL_3 ",
      "or escoskill_level_3",
      call. = FALSE
    )
  }

  skillist_col <- if ("ESCOSKILL_LEVEL_3" %in% names(skillist)) {
    "ESCOSKILL_LEVEL_3"
  } else if ("escoskill_level_3" %in% names(skillist)) {
    "escoskill_level_3"
  } else {
    stop(
      "build_profession_skill_profile: skillist must contain ESCOSKILL_LEVEL_3 ",
      "or escoskill_level_3",
      call. = FALSE
    )
  }

  check_columns(
    skillist,
    c(skillist_col, "diffusione"),
    caller = "build_profession_skill_profile"
  )

  # 2. merge announcements with ISCO -----
  ann_prof <- merge(
    ann,
    isco[, .(code, preferredLabel)],
    by.x = "idesco_level_4",
    by.y = "code",
    all.x = TRUE,
    all.y = FALSE
  )
  ann_prof <- unique(ann_prof[, .(general_id, preferredLabel)])
  ann_prof <- ann_prof[!is.na(preferredLabel)]

  # 3. merge skills with announcements -----
  data.table::setkey(ann_prof, general_id)
  ski_sub <- ski[, .SD, .SDcols = c("general_id", ski_col)]
  data.table::setkey(ski_sub, general_id)

  ski_merged <- merge(
    ski_sub,
    ann_prof,
    by = "general_id",
    all.x = TRUE,
    all.y = FALSE
  )

  # 4. merge with skillist -----
  ski_merged <- merge(
    ski_merged,
    skillist,
    by = skillist_col,
    all.x = TRUE,
    all.y = FALSE
  )

  # 5. aggregate -----
  proski <- ski_merged[,
    .(annunci = data.table::uniqueN(general_id)),
    .(preferredLabel, .skill = get(ski_col), diffusione)
  ]
  data.table::setnames(proski, ".skill", ski_col)

  # 6. attach cooc as attribute -----
  if (!is.null(cooc)) {
    data.table::setattr(proski, "cooc", cooc)
  }

  proski[]
}
