# Pipeline helper functions -----
# Data loading and merging utilities for the skillviz targets workflow.
# Functions here supplement the skillviz package with workflow-specific
# helpers that don't belong in the package itself.

# 1. load_cpi3 -----

#' Load CP2021 level 3 classification from Excel
#'
#' Reads the third sheet of the CP2021 Excel file and returns it as
#' a data.table. The sheet must contain columns `cod_3` and `nome_3`.
#'
#' @param path Character path to the CP2021 xlsx file.
#' @param sheet Integer sheet number to read. Default: 3.
#' @return A data.table with CP2021 level 3 classification.
load_cpi3 <- function(path, sheet = 3L) {
  dt <- readxl::read_xlsx(path, sheet = sheet)
  data.table::setDT(dt)
  dt
}

# 2. augment_crosswalk -----

#' Augment official crosswalk with Naive Bayes predictions
#'
#' Extracts rank-1 predictions from the NB classifier output and appends
#' them to the official ESCO-to-CPI crosswalk. Uses `nome_3` (CPI group
#' name) as the `it_esco_level_4` label for NB-classified codes, since
#' they lack an official Italian ESCO label.
#'
#' @param cpi_esco data.table from `build_cpi_esco_crosswalk()`.
#' @param cpi_nb data.table from `classify_esco_to_cpi()`.
#' @return A data.table with the same columns as `cpi_esco`, augmented
#'   with NB-classified rows.
augment_crosswalk <- function(cpi_esco, cpi_nb) {
  nb_rank1 <- cpi_nb[rank == 1L, .(idesco_level_4, cod_3, nome_3)]
  # Match cpi_esco column structure: use nome_3 as fallback label
  nb_rank1[, it_esco_level_4 := nome_3]
  # Keep only columns present in the official crosswalk
  common_cols <- intersect(names(cpi_esco), names(nb_rank1))
  nb_subset <- nb_rank1[, .SD, .SDcols = common_cols]
  cpi_full <- data.table::rbindlist(
    list(cpi_esco, nb_subset),
    use.names = TRUE,
    fill = TRUE
  )
  cpi_full[]
}

# 3. merge_skills_annunci -----

#' Merge skills with deduplicated announcements
#'
#' Joins the skills table with deduplicated announcements on `general_id`
#' to produce a combined table suitable for Balassa index, co-occurrence,
#' and other downstream analyses. The merge also adds a `preferredLabel`
#' column via the ESCO crosswalk for functions that require profession names.
#'
#' Column names from ZIP-sourced CSVs may be uppercase; this function
#' normalises them defensively via `setnames(tolower)`.
#'
#' @param annunci A data.table of deduplicated announcements (from
#'   `deduplicate_annunci()`), with at least `general_id` and
#'   `idesco_level_4`.
#' @param skills A data.table of skill assignments with at least
#'   `general_id` and `escoskill_level_3`.
#' @param cpi_esco A data.table crosswalk from `build_cpi_esco_crosswalk()`,
#'   with at least `idesco_level_4` and `it_esco_level_4`.
#' @return A data.table with columns `general_id`, `escoskill_level_3`,
#'   `idesco_level_4`, and `preferredLabel` (mapped from
#'   `it_esco_level_4`).
merge_skills_annunci <- function(annunci, skills, cpi_esco) {
  # Normalise column names (ZIP CSVs may have uppercase headers)
  data.table::setnames(skills, tolower)

  # Join announcements with crosswalk to get profession labels
  ann_prof <- merge(
    annunci[, .(general_id, idesco_level_4)],
    cpi_esco[, .(idesco_level_4, preferredLabel = it_esco_level_4)],
    by = "idesco_level_4",
    all.x = TRUE,
    all.y = FALSE
  )

  # Join with skills on general_id
  merged <- merge(
    skills[, .(general_id, escoskill_level_3)],
    ann_prof,
    by = "general_id",
    all.x = TRUE,
    all.y = FALSE
  )

  merged <- merged[!is.na(preferredLabel) & !is.na(escoskill_level_3)]
  merged[]
}
