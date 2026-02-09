# Skill relevance and specialization indices -----

# 1. Balassa index (specialization) -----

#' Compute the Balassa index (Revealed Comparative Advantage) for skill-profession pairs
#'
#' Measures the degree of specialization of each skill within each profession.
#' A Balassa index >= 1 indicates the skill is more concentrated in that profession
#' than across all professions (i.e., it is "specific" to the profession).
#'
#' The formula for each profession-skill pair is:
#' \deqn{B = (N_{skill,prof} / N_{prof}) / (N_{skill,all} / N_{all})}
#'
#' @param skills A `data.table` with one row per skill occurrence in an announcement.
#'   Required columns: `preferredLabel` (profession), `escoskill_level_3` (skill).
#' @return A `data.table` with columns: `preferredLabel`, `escoskill_level_3`, `N`,
#'   `tutte_professioni`, `tutte_le_skills_della_professione`, `tutte_le_ricorrenze`,
#'   `balassa_index`, and `specializzazione` ("specifica" if >= 1, "non specifica"
#'   otherwise).
#' @export
compute_balassa_index <- function(skills) {
  check_columns(
    skills,
    c("preferredLabel", "escoskill_level_3"),
    caller = "compute_balassa_index"
  )

  msk <- skills[, .(N = .N), .(preferredLabel, escoskill_level_3)]
  msk[, tutte_professioni := sum(N), .(escoskill_level_3)]
  msk[, tutte_le_skills_della_professione := sum(N), .(preferredLabel)]
  msk[, tutte_le_ricorrenze := sum(N)]
  msk[,
    balassa_index := (N / tutte_le_skills_della_professione) /
      (tutte_professioni / tutte_le_ricorrenze)
  ]
  msk[,
    specializzazione := data.table::fcase(
      balassa_index >= 1 , "specifica" ,
      default = "non specifica"
    )
  ]

  msk[]
}

# 2. Skill diffusion (TF-IDF) -----

#' Compute skill diffusion using TF-IDF on aggregated skill frequencies
#'
#' Calculates Term Frequency (TF) and Inverse Document Frequency (IDF) for each
#' skill based on aggregated occurrence counts. Skills with very low IDF are
#' widespread ("alta" diffusion); skills with very high IDF are rare ("minima").
#'
#' The IDF formula is: \eqn{log(N_{total} / N_{skill})} where counts come from
#' the aggregated profession-skill matrix.
#'
#' @param skills_by_profession A `data.table` with columns `escoskill_level_3`
#'   and `N` (occurrence count), typically obtained by aggregating skills merged
#'   with announcements. Each row is a skill-profession pair with its count.
#' @param quantile_trim Numeric, quantile threshold for classifying diffusion.
#'   Skills with IDF <= quantile(quantile_trim) are "alta" (highly diffuse),
#'   skills with IDF >= quantile(1 - quantile_trim) are "minima" (rare).
#'   Default: 0.025.
#' @return A `data.table` with columns: `escoskill_level_3`, `N`, `tf`, `idf`,
#'   `diffusione`.
#' @export
compute_skill_diffusion <- function(
  skills_by_profession,
  quantile_trim = 0.025
) {
  check_columns(
    skills_by_profession,
    c("escoskill_level_3", "N"),
    caller = "compute_skill_diffusion"
  )

  enne <- sum(skills_by_profession$N)
  rilevanza <- skills_by_profession[,
    .(N = sum(N), tf = sum(N) / enne * 100, idf = log(enne / sum(N))),
    .(escoskill_level_3)
  ]

  qtl <- stats::quantile(
    rilevanza$idf,
    probs = c(quantile_trim, 1 - quantile_trim)
  )
  rilevanza[,
    diffusione := data.table::fcase(
      idf <= qtl[1L] , "alta"   ,
      idf >= qtl[2L] , "minima" ,
      default = "centrale"
    )
  ]

  rilevanza[]
}

# 3. Master skill list -----

#' Build a master skills list with metadata and diffusion classification
#'
#' Aggregates skill metadata (reuse type, flag columns) and merges with diffusion
#' scores. The ESCO reuse type labels are translated to Italian.
#'
#' @param skills A `data.table` of skill occurrences with columns:
#'   `escoskill_level_3`, `esco_v0101_reusetype`, `pillar_softskills`,
#'   `esco_v0101_ict`, `esco_v0101_green`, `esco_v0101_language`.
#' @param diffusion A `data.table` as returned by [compute_skill_diffusion()],
#'   with columns `escoskill_level_3`, `N`, `tf`, `idf`, `diffusione`.
#' @return A `data.table` with one row per unique skill, including metadata
#'   columns, the Italian type label (`tipo`), and diffusion scores.
#' @export
build_skillist <- function(skills, diffusion) {
  check_columns(
    skills,
    c(
      "escoskill_level_3",
      "esco_v0101_reusetype",
      "pillar_softskills",
      "esco_v0101_ict",
      "esco_v0101_green",
      "esco_v0101_language"
    ),
    caller = "build_skillist"
  )
  check_columns(
    diffusion,
    c("escoskill_level_3", "N", "tf", "idf", "diffusione"),
    caller = "build_skillist"
  )

  skillist <- skills[,
    .(N = .N),
    .(
      escoskill_level_3,
      esco_v0101_reusetype,
      pillar_softskills,
      esco_v0101_ict,
      esco_v0101_green,
      esco_v0101_language
    )
  ]

  skillist[,
    tipo := data.table::fcase(
      esco_v0101_reusetype == "sector-specific"     , "settoriale"  ,
      esco_v0101_reusetype == "transversal"         , "trasversale" ,
      esco_v0101_reusetype == "occupation-specific" , "specifico"   ,
      default = "multisettoriale"
    )
  ][, N := NULL]

  skillist <- merge(diffusion, skillist, by = "escoskill_level_3")

  skillist[]
}

# 4. IDF classification for document-level data -----

#' Compute IDF classification at the document level
#'
#' Calculates IDF using unique document counts (not aggregated frequencies).
#' This variant counts the number of distinct documents (announcements) that
#' mention each skill, providing a document-frequency-based diffusion measure.
#'
#' The IDF formula is: \eqn{log(1 / (n_{docs\_with\_skill} / n_{docs\_total}))}
#' which simplifies to \eqn{log(n_{docs\_total} / n_{docs\_with\_skill})}.
#'
#' @param skills_merged A `data.table` with columns `general_id` (document ID)
#'   and `escoskill_level_3` (skill label).
#' @param quantile_trim Numeric, quantile threshold. Skills with IDF in the
#'   bottom `quantile_trim` fraction are classified as highly diffuse ("alta").
#'   Default: 0.025.
#' @return A named `list` with:
#'   \describe{
#'     \item{idf}{A `data.table` with columns `escoskill_level_3`, `N`,
#'       `tf`, `idf`, `diffusione`.}
#'     \item{threshold_rows}{Integer, number of skills classified as "alta"
#'       (highly diffuse).}
#'     \item{diffuse_skills}{Character vector of skill labels classified as
#'       "alta" diffusion.}
#'   }
#' @export
compute_idf_classification <- function(skills_merged, quantile_trim = 0.025) {
  check_columns(
    skills_merged,
    c("general_id", "escoskill_level_3"),
    caller = "compute_idf_classification"
  )

  enne <- data.table::uniqueN(skills_merged$general_id)

  idf_dt <- skills_merged[,
    .(.N, tf = data.table::uniqueN(general_id) / enne),
    .(escoskill_level_3)
  ][, idf := log(1 / tf)][order(idf)]

  qtl <- stats::quantile(
    idf_dt$idf,
    probs = c(quantile_trim, 1 - quantile_trim)
  )
  idf_dt[,
    diffusione := data.table::fcase(
      idf <= qtl[1L] , "alta"   ,
      idf >= qtl[2L] , "minima" ,
      default = "centrale"
    )
  ]

  threshold_rows <- round(nrow(idf_dt) * quantile_trim, 0) - 1L
  diffuse_skills <- idf_dt[1:threshold_rows, escoskill_level_3]

  list(
    idf = idf_dt[],
    threshold_rows = threshold_rows,
    diffuse_skills = diffuse_skills
  )
}
