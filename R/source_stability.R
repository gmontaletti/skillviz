# Source stability analysis -----

# 1. prepare_source_tsibble -----

#' Prepare a tsibble of announcement counts by source and month
#'
#' Convert a data.table of job announcements into a tsibble keyed by
#' data source (`fonte`) and indexed by year-month, counting unique
#' announcements per source per month.
#'
#' @param ann A data.table of announcements. Must contain columns
#'   `general_id` and `source`. Also requires either a `gdate` column
#'   (IDate) or the raw year/month/day columns used by
#'   [parse_ymd_columns()].
#' @return A tsibble with key `fonte`, index `mese` (yearmonth) and
#'   column `N` (unique announcement count per source-month).
#' @export
prepare_source_tsibble <- function(ann) {
  check_suggests("tsibble", "to create time-series tibbles")
  check_columns(
    ann,
    c("general_id", "source"),
    caller = "prepare_source_tsibble"
  )

  dt <- data.table::copy(ann)

  # Ensure gdate exists

  if (!"gdate" %in% names(dt)) {
    parse_ymd_columns(dt, "grab_date", "gdate")
  }

  sta <- dt[,
    .(N = data.table::uniqueN(general_id)),
    .(fonte = source, mese = tsibble::yearmonth(gdate))
  ]

  tsibble::as_tsibble(sta, key = fonte, index = mese)
}

# 2. compute_source_features -----

#' Compute time-series features per source
#'
#' Extract STL decomposition features from a tsibble produced by
#' [prepare_source_tsibble()]. Returns summary statistics (mean, sd,
#' sum) together with trend and seasonality strength and the
#' coefficient of variation.
#'
#' @param tst A tsibble as returned by [prepare_source_tsibble()],
#'   with column `N` and key `fonte`.
#' @return A tibble with one row per source containing columns:
#'   `fonte`, `mea` (mean), `sd` (standard deviation), `sum` (total),
#'   trend/seasonality strength from [feasts::feat_stl()], and `cv`
#'   (coefficient of variation = sd / mean).
#' @export
compute_source_features <- function(tst) {
  check_suggests("feasts", "to compute STL features")
  check_suggests("fabletools", "for the features() generic")

  fet <- fabletools::features(
    tst,
    N,
    features = list(mea = mean, sd = stats::sd, sum = sum, feasts::feat_stl)
  )

  fet$cv <- fet$sd / fet$mea
  fet
}

# 3. filter_stable_sources -----

#' Filter sources by stability criteria
#'
#' Select data sources whose coefficient of variation is at most
#' `cv_threshold` and whose total announcement count is at least
#' `min_total`. Rows with missing values are excluded.
#'
#' @param features A tibble as returned by [compute_source_features()].
#'   Must contain columns `fonte`, `cv` and `sum`.
#' @param cv_threshold Numeric maximum coefficient of variation
#'   (default 0.6).
#' @param min_total Integer minimum total announcement count across
#'   all months (default 0, i.e. no minimum).
#' @return A character vector of stable source names.
#' @export
filter_stable_sources <- function(
  features,
  cv_threshold = 0.6,
  min_total = 0L
) {
  complete <- features[stats::complete.cases(features), ]
  stable <- complete[complete$cv <= cv_threshold & complete$sum >= min_total, ]
  as.character(stable$fonte)
}

# 4. filter_annunci_by_source -----

#' Filter announcements to stable sources
#'
#' Subset a data.table of announcements keeping only rows whose
#' `source` column matches one of the provided stable source names.
#'
#' @param ann A data.table of announcements with a `source` column.
#' @param stable_sources Character vector of source names to keep,
#'   typically returned by [filter_stable_sources()].
#' @return A data.table containing only rows with `source` in
#'   `stable_sources`.
#' @export
filter_annunci_by_source <- function(ann, stable_sources) {
  check_columns(ann, "source", caller = "filter_annunci_by_source")
  ann[source %chin% stable_sources]
}
