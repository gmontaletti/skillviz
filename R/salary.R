# Salary analysis -----

# 1. extract_salary_data -----

#' Extract announcements with salary information
#'
#' Filter job announcements to those with a positive salary value.
#'
#' @param ann A data.table of announcements. Must contain column
#'   `salaryvalue` (numeric).
#' @return A data.table containing only rows where `salaryvalue > 0`.
#' @export
extract_salary_data <- function(ann) {
  check_columns(ann, "salaryvalue", caller = "extract_salary_data")
  ann[salaryvalue > 0]
}

# 2. compute_salary_by_period -----

#' Compute salary statistics by grouping period
#'
#' Aggregate salary data computing count, mean and median salary per
#' group. The default grouping produces one row per salary band and
#' month combination.
#'
#' @param salary_data A data.table returned by [extract_salary_data()].
#'   Must contain `salaryvalue` and the columns listed in `by`.
#' @param by Character vector of column names to group by. When `"mese"`
#'   is included and does not yet exist in `salary_data`, it is derived
#'   from the `data` column as a year-month string (`"YYYY-MM"`).
#'   Defaults to `c("salary", "mese")`.
#' @return A data.table with columns from `by` plus `N` (count),
#'   `media` (mean salary) and `mediana` (median salary).
#' @export
compute_salary_by_period <- function(salary_data, by = c("salary", "mese")) {
  check_columns(salary_data, "salaryvalue", caller = "compute_salary_by_period")
  dt <- data.table::copy(salary_data)

  # Derive mese from data column when needed
  if ("mese" %in% by && !"mese" %in% names(dt)) {
    check_columns(dt, "data", caller = "compute_salary_by_period")
    dt[, mese := format(data, "%Y-%m")]
  }

  missing_by <- setdiff(by, names(dt))
  if (length(missing_by) > 0L) {
    stop(
      "compute_salary_by_period: missing grouping columns: ",
      paste(missing_by, collapse = ", "),
      call. = FALSE
    )
  }

  dt[,
    .(
      N = .N,
      media = mean(salaryvalue),
      mediana = median(salaryvalue)
    ),
    by = by
  ]
}

# 3. compute_salary_by_skill -----

#' Compute median salary per skill
#'
#' Join salary data with a skills table and compute the median salary
#' for each ESCO level-3 skill, ordered from highest to lowest.
#'
#' @param salary_data A data.table returned by [extract_salary_data()].
#'   Must contain `salaryvalue` and a key column suitable for joining
#'   with `skills` (typically `general_id`).
#' @param skills A data.table of skills with at least columns
#'   `general_id` and `escoskill_level_3`.
#' @return A data.table with columns `escoskill_level_3`, `N` (count)
#'   and `mediana` (median salary), sorted by descending median.
#' @export
compute_salary_by_skill <- function(salary_data, skills) {
  check_columns(salary_data, "salaryvalue", caller = "compute_salary_by_skill")
  check_columns(skills, "escoskill_level_3", caller = "compute_salary_by_skill")

  sks <- merge(salary_data, skills, all.x = TRUE, all.y = FALSE)

  sks[,
    .(N = .N, mediana = median(salaryvalue)),
    keyby = escoskill_level_3
  ][order(-mediana)]
}
