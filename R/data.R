# Data documentation -----

#' ESCO ISCO group classification
#'
#' Mapping table from ESCO level 4 occupation codes to profession names
#' and hierarchy labels.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{code}{ESCO occupation code}
#'   \item{preferredLabel}{Profession name (Italian)}
#'   \item{esco_v0101_hier_label_1}{Hierarchy level 1}
#'   \item{esco_v0101_hier_label_2}{Hierarchy level 2}
#'   \item{esco_v0101_hier_label_3}{Hierarchy level 3}
#' }
#' @source ESCO classification v1.1
"isco_gruppi"

#' Master skills list
#'
#' Pre-computed skills metadata including frequency, reuse type,
#' and domain flags (soft-skills, ICT, green, language).
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{escoskill_level_3}{Skill identifier (ESCO level 3)}
#'   \item{esco_v0101_reusetype}{Reuse type: sector-specific, transversal, occupation-specific, multisettoriale}
#'   \item{N}{Recurrence count across all announcements}
#'   \item{tipo}{Mapped Italian type: settoriale, trasversale, specifico, multisettoriale}
#'   \item{pillar_softskills}{Soft-skill flag}
#'   \item{esco_v0101_ict}{ICT skill flag}
#'   \item{esco_v0101_green}{Green skill flag}
#'   \item{esco_v0101_language}{Language skill flag}
#' }
#' @source Computed from OJA data via Lightcast/EMSI
"skillist"

#' Skill co-occurrence network
#'
#' Pre-computed global skill co-occurrence edges with relative-risk
#' filtering applied. Each row represents a pair of skills that
#' co-occur in job announcements above the expected baseline.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{from}{First skill in the pair}
#'   \item{to}{Second skill in the pair}
#'   \item{weight}{Co-occurrence count (filtered by relative risk > 1)}
#' }
#' @source Computed from OJA data via crossprod on term-document matrix
"cooccorrenza"
