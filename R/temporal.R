# Temporal skill ranking and variation -----

# 1. compute_skill_ranking_series -----

#' Compute skill ranking per profession over time periods
#'
#' Aggregates skill counts by profession and time period, then ranks
#' skills within each profession-period combination (descending by count).
#' The time unit controls the granularity of aggregation.
#'
#' This reproduces the temporal ranking logic where skills are ranked
#' by frequency within each profession at each time step.
#'
#' @param competenze A `data.table` with columns `escoskill_level_3`
#'   (skill label), `nome_3` (profession name), and `gdate` (date,
#'   either Date or IDate class).
#' @param time_unit Character, time unit for
#'   `lubridate::floor_date()`. Default: `"year"`. Other options:
#'   `"quarter"`, `"month"`, etc.
#' @param cutoff_date Date or character in `"YYYY-MM-DD"` format.
#'   Observations on or after this date are excluded. Default: `NULL`
#'   (no filtering).
#' @return A `data.table` with columns `professione` (from `nome_3`),
#'   `mese` (floored date), `skill` (from `escoskill_level_3`), `N`
#'   (count), and `rango` (rank within profession-period, 1 = most
#'   frequent).
#' @export
compute_skill_ranking_series <- function(
  competenze,
  time_unit = "year",
  cutoff_date = NULL
) {
  # 1. input validation -----
  check_columns(
    competenze,
    c("escoskill_level_3", "nome_3", "gdate"),
    caller = "compute_skill_ranking_series"
  )
  check_suggests("lubridate", "for floor_date()")

  if (!is.data.table(competenze)) {
    competenze <- data.table::as.data.table(competenze)
  }

  # 2. aggregate by time period -----
  serie <- competenze[,
    .N,
    .(
      mese = lubridate::floor_date(gdate, unit = time_unit),
      skill = escoskill_level_3,
      professione = nome_3
    )
  ]

  # 3. apply cutoff -----
  if (!is.null(cutoff_date)) {
    cutoff_date <- as.Date(cutoff_date)
    serie <- serie[mese < cutoff_date]
  }

  # 4. rank within profession-period -----
  data.table::setorder(serie, professione, mese, -N, skill)
  serie[, rango := seq_len(.N), .(professione, mese)]

  serie[]
}


# 2. compute_skill_variation -----

#' Compute year-over-year variation in skill rankings
#'
#' Takes the output of [compute_skill_ranking_series()] and calculates
#' ranking changes between consecutive time periods. The function
#' pivots the series to wide format with rank and count columns per
#' period, filters by a minimum count threshold, computes the rank
#' variation (previous rank minus current rank, so positive values
#' indicate improvement), and returns the top-k skills per profession.
#'
#' @param serie A `data.table` as returned by
#'   [compute_skill_ranking_series()], with columns `professione`,
#'   `mese`, `skill`, `N`, and `rango`.
#' @param min_n Integer, minimum count in the most recent period for a
#'   skill to be included in the variation analysis. Default: `30L`.
#' @param top_k Integer, number of top-ranked skills to return per
#'   profession (by most recent rank). Default: `10L`.
#' @return A `data.table` in wide format with one row per
#'   profession-skill pair. Columns include `professione`, `skill`,
#'   rank and count columns for each time period (named as
#'   `rango_<period>` and `N_<period>`), and `variazione` (rank change
#'   from previous to current period).
#' @export
compute_skill_variation <- function(serie, min_n = 30L, top_k = 10L) {
  # 1. input validation -----
  check_columns(
    serie,
    c("professione", "mese", "skill", "N", "rango"),
    caller = "compute_skill_variation"
  )

  if (!is.data.table(serie)) {
    serie <- data.table::as.data.table(serie)
  }

  # 2. identify periods -----
  periods <- sort(unique(serie$mese))
  if (length(periods) < 2L) {
    stop(
      "compute_skill_variation: need at least 2 time periods, found ",
      length(periods),
      call. = FALSE
    )
  }

  current_period <- periods[length(periods)]
  previous_period <- periods[length(periods) - 1L]

  # 3. pivot to wide format -----
  variazioni <- data.table::dcast(
    serie,
    professione + skill ~ mese,
    value.var = c("rango", "N")
  )
  cnames <- make.names(names(variazioni))
  data.table::setnames(variazioni, cnames)

  # 4. build column names for current and previous periods -----
  current_rango_col <- make.names(paste0("rango_", current_period))
  previous_rango_col <- make.names(paste0("rango_", previous_period))
  current_n_col <- make.names(paste0("N_", current_period))

  # 5. filter by min_n in current period -----
  if (current_n_col %in% names(variazioni)) {
    variazioni <- variazioni[get(current_n_col) >= min_n]
  }

  # 6. compute variation -----
  if (
    previous_rango_col %in%
      names(variazioni) &&
      current_rango_col %in% names(variazioni)
  ) {
    variazioni[, variazione := get(previous_rango_col) - get(current_rango_col)]
  } else {
    variazioni[, variazione := NA_real_]
  }

  # 7. fill NA ranks with 0 -----
  rango_cols <- grep("^rango_", names(variazioni), value = TRUE)
  for (rc in rango_cols) {
    data.table::set(variazioni, which(is.na(variazioni[[rc]])), rc, 0L)
  }

  # 8. top-k per profession -----
  if (current_rango_col %in% names(variazioni)) {
    data.table::setorderv(variazioni, c("professione", current_rango_col))
  }
  variazioni <- variazioni[, head(.SD, top_k), .(professione)]

  variazioni[]
}
