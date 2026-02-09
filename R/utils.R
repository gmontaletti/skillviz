# Internal utilities -----

#' Parse year/month/day columns into IDate
#'
#' @param dt data.table with year, month, day integer columns
#' @param prefix Column prefix (e.g. "grab_date" or "expire_date")
#' @param col_name Name for the output IDate column
#' @return data.table with added IDate column (modified by reference)
#' @keywords internal
parse_ymd_columns <- function(dt, prefix, col_name) {
  ycol <- paste0("year_", prefix)
  mcol <- paste0("month_", prefix)
  dcol <- paste0("day_", prefix)
  dt[,
    (col_name) := data.table::as.IDate(
      paste(get(ycol), get(mcol), get(dcol), sep = "-")
    )
  ]
  invisible(dt)
}

#' Check that required columns exist in a data.table
#'
#' @param dt data.table to check
#' @param required Character vector of required column names
#' @param caller Name of calling function for error messages
#' @keywords internal
check_columns <- function(dt, required, caller = "function") {
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop(
      caller,
      ": missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Check if a suggested package is available
#'
#' @param pkg Package name
#' @param reason Why the package is needed
#' @keywords internal
check_suggests <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Package '", pkg, "' is required")
    if (!is.null(reason)) {
      msg <- paste0(msg, " ", reason)
    }
    msg <- paste0(msg, ". Install it with install.packages('", pkg, "')")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}
