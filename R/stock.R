# Daily stock of open postings, standardised-duration stock, weekly aggregation -----

# 1. internal helpers -----

#' Check an optional grouping argument
#' @keywords internal
#' @noRd
.st_check_by <- function(by, caller, reserved = character(0)) {
  if (is.null(by)) {
    return(invisible(TRUE))
  }
  if (!is.character(by) || length(by) == 0L || anyNA(by) || !all(nzchar(by))) {
    stop(
      caller,
      ": `by` must be NULL or a non-empty character vector of column names",
      call. = FALSE
    )
  }
  if (anyDuplicated(by)) {
    stop(caller, ": `by` contains duplicated column names", call. = FALSE)
  }
  if (any(startsWith(by, "."))) {
    stop(
      caller,
      ": `by` columns cannot start with '.' (reserved for internal columns)",
      call. = FALSE
    )
  }
  clash <- intersect(by, reserved)
  if (length(clash) > 0L) {
    stop(
      caller,
      ": `by` cannot contain the column name(s) ",
      paste(clash, collapse = ", "),
      " (used by the input or the output)",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Coerce a date column to IDate, rejecting non-dates and NA
#' @keywords internal
#' @noRd
.st_as_idate <- function(x, name, caller) {
  if (!inherits(x, "Date")) {
    stop(
      caller,
      ": column `",
      name,
      "` must be of class Date or IDate",
      call. = FALSE
    )
  }
  if (anyNA(x)) {
    stop(caller, ": column `", name, "` contains NA dates", call. = FALSE)
  }
  data.table::as.IDate(x)
}

#' Coerce a single date argument (Date, IDate or "YYYY-MM-DD") to IDate
#' @keywords internal
#' @noRd
.st_date_arg <- function(x, name, caller) {
  if (is.null(x)) {
    return(NULL)
  }
  out <- if (length(x) == 1L && (inherits(x, "Date") || is.character(x))) {
    tryCatch(data.table::as.IDate(x), error = function(e) NA)
  } else {
    NA
  }
  if (length(out) != 1L || is.na(out)) {
    stop(
      caller,
      ": `",
      name,
      "` must be a single Date, IDate or 'YYYY-MM-DD' string",
      call. = FALSE
    )
  }
  out
}

#' Check a count column: numeric, finite, non-negative, optionally no NA
#' @keywords internal
#' @noRd
.st_check_count <- function(x, name, caller, allow_na = FALSE) {
  if (!is.numeric(x)) {
    stop(caller, ": column `", name, "` must be numeric", call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop(caller, ": column `", name, "` contains NA values", call. = FALSE)
  }
  ok <- x[!is.na(x)]
  if (any(!is.finite(ok)) || any(ok < 0)) {
    stop(
      caller,
      ": column `",
      name,
      "` must contain finite non-negative values",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Complete grid of groups x values
#'
#' @param groups data.table of unique group combinations, or NULL.
#' @param values Vector of values (dates or integers) for the grid column.
#' @param col Name of the grid column.
#' @keywords internal
#' @noRd
.st_grid <- function(groups, values, col) {
  nv <- length(values)
  if (is.null(groups)) {
    grid <- data.table::data.table(values)
    data.table::setnames(grid, col)
    return(grid)
  }
  grid <- groups[rep(seq_len(nrow(groups)), each = nv)]
  data.table::set(grid, j = col, value = rep(values, nrow(groups)))
  grid
}

#' Unique group combinations, or NULL when there is no grouping
#' @keywords internal
#' @noRd
.st_groups <- function(dt, by) {
  if (is.null(by)) {
    return(NULL)
  }
  unique(dt[, by, with = FALSE])
}

#' Describe the group of the first offending row for error messages
#' @keywords internal
#' @noRd
.st_describe_group <- function(row, by) {
  if (is.null(by)) {
    return("")
  }
  vals <- vapply(by, function(b) as.character(row[[b]][1L]), character(1))
  paste0(" (", paste(by, vals, sep = " = ", collapse = ", "), ")")
}

#' One-sided convolution with zero padding before the series
#'
#' Returns `y` with `y[i] = sum_{k = 0}^{M} g[k + 1] * x[i - k]`, where
#' values of `x` before the first element are taken as 0.
#' @keywords internal
#' @noRd
.st_convolve <- function(x, g) {
  m <- length(g) - 1L
  x <- as.numeric(x)
  if (m == 0L) {
    return(x * g[1L])
  }
  y <- stats::filter(
    c(rep(0, m), x),
    filter = g,
    method = "convolution",
    sides = 1L
  )
  as.numeric(y)[-seq_len(m)]
}

# 2. events_from_intervals -----

#' Event table from posting intervals
#'
#' Turns a table of posting intervals (one row per posting, or per group of
#' postings with the same interval) into the table of dated events consumed
#' by [stock_from_events()]: each interval adds `n` to the inflow of its
#' start date and `n` to the outflow of the day after its end date.
#'
#' @details
#' A posting is open on day `d` when `start <= d <= end` (end inclusive),
#' so it leaves the stock on `end + 1`. Events are aggregated by `by` x date.
#' This helper is meant for tests and small data; on large data the same
#' event table is better computed in the database.
#'
#' @param dt A data.frame or data.table of intervals.
#' @param by Character vector of grouping columns, or `NULL` for no grouping.
#' @param start_col Name of the start-date column (class Date or IDate).
#' @param end_col Name of the end-date column (class Date or IDate, inclusive).
#' @param n_col Name of a column of non-negative weights (number of postings
#'   per row), or `NULL` to count each row once.
#'
#' @return A data.table with columns `by`, `date` (IDate), `inflow` and
#'   `outflow`, one row per `by` x date with at least one event, ordered by
#'   `by` and `date`. Counts are integer when `n_col` is `NULL` or integer.
#'
#' @examples
#' iv <- data.table::data.table(
#'   source = c("a", "a", "b"),
#'   start = data.table::as.IDate(c("2024-01-01", "2024-01-03", "2024-01-02")),
#'   end = data.table::as.IDate(c("2024-01-02", "2024-01-05", "2024-01-02"))
#' )
#' events_from_intervals(iv, by = "source", start_col = "start", end_col = "end")
#'
#' @export
events_from_intervals <- function(
  dt,
  by = NULL,
  start_col,
  end_col,
  n_col = NULL
) {
  caller <- "events_from_intervals"
  .si_check_df(dt, "dt", caller)
  .si_check_name(start_col, "start_col", caller)
  .si_check_name(end_col, "end_col", caller)
  if (!is.null(n_col)) {
    .si_check_name(n_col, "n_col", caller)
  }
  .st_check_by(
    by,
    caller,
    reserved = c(start_col, end_col, n_col, "date", "inflow", "outflow")
  )
  check_columns(dt, c(by, start_col, end_col, n_col), caller)

  x <- data.table::as.data.table(dt)[,
    c(by, start_col, end_col, n_col),
    with = FALSE
  ]
  data.table::setnames(
    x,
    c(by, ".start", ".end", if (!is.null(n_col)) ".n")
  )
  x[, .start := .st_as_idate(.start, start_col, caller)]
  x[, .end := .st_as_idate(.end, end_col, caller)]
  n_bad <- x[.end < .start, .N]
  if (n_bad > 0L) {
    stop(
      caller,
      ": ",
      n_bad,
      " interval(s) end before they start (`",
      end_col,
      "` < `",
      start_col,
      "`)",
      call. = FALSE
    )
  }
  if (is.null(n_col)) {
    x[, .n := 1L]
  } else {
    .st_check_count(x$.n, n_col, caller)
  }

  ins <- x[, c(by, ".start", ".n"), with = FALSE]
  data.table::setnames(ins, c(by, "date", "inflow"))
  ins[, outflow := inflow * 0L]
  outs <- x[, c(by, ".end", ".n"), with = FALSE]
  data.table::setnames(outs, c(by, "date", "outflow"))
  outs[, date := date + 1L]
  outs[, inflow := outflow * 0L]

  ev <- data.table::rbindlist(list(ins, outs), use.names = TRUE)
  ev <- ev[,
    list(inflow = sum(inflow), outflow = sum(outflow)),
    by = c(by, "date")
  ]
  data.table::setorderv(ev, c(by, "date"))
  ev[]
}

# 3. stock_from_events -----

#' Daily stock of open postings from dated events
#'
#' Builds the daily stock of open postings as the running sum of inflow
#' minus outflow on a complete daily grid, per group.
#'
#' @details
#' The input holds one row per `by` x date with the number of postings
#' entering the stock that day (`in_col`) and leaving it that day
#' (`out_col`; a posting open until `expire_date` inclusive is counted as
#' outflow on `expire_date + 1`). Rows with the same `by` x date are summed.
#' The function builds the complete daily grid of every group, fills the
#' missing days with zero events and computes
#' `stock = cumsum(inflow - outflow)` per group (a difference array).
#'
#' The cumulative sum always starts at the first event date of the data, so
#' events before `from` are carried into the stock of `from`; events after
#' `to` are ignored. A negative stock means that some postings leave before
#' they enter (inconsistent events) and raises an error.
#'
#' @param events A data.frame or data.table of dated events.
#' @param by Character vector of grouping columns, or `NULL` for no grouping.
#' @param date_col Name of the date column (class Date or IDate).
#' @param in_col Name of the inflow column (non-negative counts).
#' @param out_col Name of the outflow column (non-negative counts).
#' @param from,to First and last day of the output (Date, IDate or
#'   `"YYYY-MM-DD"`). Default: the first and last event date over all groups.
#'
#' @return A data.table with columns `by`, `date_col` (IDate), `inflow`,
#'   `outflow` and `stock`, one row per group and day from `from` to `to`,
#'   ordered by `by` and date. Counts are integer when both input columns are
#'   integer, double otherwise.
#'
#' @examples
#' iv <- data.table::data.table(
#'   start = data.table::as.IDate(c("2024-01-01", "2024-01-03", "2024-01-02")),
#'   end = data.table::as.IDate(c("2024-01-02", "2024-01-05", "2024-01-02"))
#' )
#' ev <- events_from_intervals(iv, start_col = "start", end_col = "end")
#' stock_from_events(ev)
#'
#' @export
stock_from_events <- function(
  events,
  by = NULL,
  date_col = "date",
  in_col = "inflow",
  out_col = "outflow",
  from = NULL,
  to = NULL
) {
  caller <- "stock_from_events"

  # 3.1 validation -----
  .si_check_df(events, "events", caller)
  .si_check_name(date_col, "date_col", caller)
  .si_check_name(in_col, "in_col", caller)
  .si_check_name(out_col, "out_col", caller)
  if (in_col == out_col) {
    stop(caller, ": `in_col` and `out_col` must differ", call. = FALSE)
  }
  .st_check_by(
    by,
    caller,
    reserved = c(date_col, in_col, out_col, "inflow", "outflow", "stock")
  )
  if (date_col %in% c("inflow", "outflow", "stock")) {
    stop(
      caller,
      ": `date_col` cannot be 'inflow', 'outflow' or 'stock'",
      call. = FALSE
    )
  }
  check_columns(events, c(by, date_col, in_col, out_col), caller)
  if (nrow(events) == 0L) {
    stop(caller, ": `events` has no rows", call. = FALSE)
  }
  from <- .st_date_arg(from, "from", caller)
  to <- .st_date_arg(to, "to", caller)

  dt <- data.table::as.data.table(events)[,
    c(by, date_col, in_col, out_col),
    with = FALSE
  ]
  data.table::setnames(dt, c(by, ".d", ".fin", ".fout"))
  dt[, .d := .st_as_idate(.d, date_col, caller)]
  .st_check_count(dt$.fin, in_col, caller)
  .st_check_count(dt$.fout, out_col, caller)

  # 3.2 window -----
  first_event <- min(dt$.d)
  out_from <- if (is.null(from)) first_event else from
  out_to <- if (is.null(to)) max(dt$.d) else to
  if (out_from > out_to) {
    stop(
      caller,
      ": the output window is empty (`from` = ",
      format(out_from),
      " is after `to` = ",
      format(out_to),
      ")",
      call. = FALSE
    )
  }
  grid_from <- min(out_from, first_event)

  # 3.3 complete grid and running sum -----
  groups <- .st_groups(dt, by)
  dt <- dt[.d <= out_to]
  dt <- dt[, list(.fin = sum(.fin), .fout = sum(.fout)), by = c(by, ".d")]
  grid <- .st_grid(groups, seq(grid_from, out_to, by = 1L), ".d")
  res <- dt[grid, on = c(by, ".d")]
  zero_in <- if (is.integer(res$.fin)) 0L else 0
  zero_out <- if (is.integer(res$.fout)) 0L else 0
  res[is.na(.fin), .fin := zero_in]
  res[is.na(.fout), .fout := zero_out]
  data.table::setorderv(res, c(by, ".d"))
  res[, stock := cumsum(.fin - .fout), by = by]

  # 3.4 consistency -----
  neg <- res[stock < 0]
  if (nrow(neg) > 0L) {
    stop(
      caller,
      ": negative stock on ",
      format(neg$.d[1L]),
      .st_describe_group(neg, by),
      ": outflow exceeds the postings entered so far. ",
      "Check that outflow is dated expire_date + 1 and that no posting ",
      "leaves before it enters (",
      nrow(neg),
      " group-day(s) affected)",
      call. = FALSE
    )
  }

  res <- res[.d >= out_from]
  data.table::setnames(
    res,
    c(".d", ".fin", ".fout"),
    c(date_col, "inflow", "outflow")
  )
  res[]
}

# 4. duration_survival -----

#' Survival function of posting durations
#'
#' From a table of duration counts, returns per group the share of postings
#' still open `j` days after their start, for `j = 0, ..., max_duration`.
#'
#' @details
#' The duration is `end - start` in days, so a posting open on `start..end`
#' (end inclusive) stays in the stock for `duration + 1` days. The function
#' returns `surv(j) = P(duration >= j)`, the share of postings still open on
#' day `start + j`; `surv(0) = 1` and the curve is non-increasing. Used as
#' weights `G(j)` by [standardized_stock()].
#'
#' When `max_duration` is smaller than the largest observed duration, longer
#' durations count as `max_duration`: the values of `surv` for
#' `j <= max_duration` are unchanged, only the grid is truncated.
#'
#' @param durations A data.frame or data.table of durations, one row per
#'   duration value (and group), or one row per posting when `n_col` is
#'   `NULL`.
#' @param by Character vector of grouping columns, or `NULL` for no grouping.
#' @param duration_col Name of the duration column (non-negative integer days).
#' @param n_col Name of the count column (number of postings with that
#'   duration), or `NULL` to count each row once.
#' @param max_duration Last value of `j` in the output (single non-negative
#'   integer). Default: the largest observed duration over all groups, so
#'   that every group has the same grid.
#'
#' @return A data.table with columns `by`, `j` (integer, `0..max_duration`)
#'   and `surv` (double, in `[0, 1]`), ordered by `by` and `j`.
#'
#' @examples
#' d <- data.table::data.table(
#'   source = c("a", "a", "b"),
#'   duration = c(2L, 5L, 3L),
#'   n = c(30L, 70L, 10L)
#' )
#' duration_survival(d, by = "source")
#'
#' @export
duration_survival <- function(
  durations,
  by = NULL,
  duration_col = "duration",
  n_col = "n",
  max_duration = NULL
) {
  caller <- "duration_survival"

  # 4.1 validation -----
  .si_check_df(durations, "durations", caller)
  .si_check_name(duration_col, "duration_col", caller)
  if (!is.null(n_col)) {
    .si_check_name(n_col, "n_col", caller)
  }
  .st_check_by(by, caller, reserved = c(duration_col, n_col, "j", "surv"))
  check_columns(durations, c(by, duration_col, n_col), caller)
  if (nrow(durations) == 0L) {
    stop(caller, ": `durations` has no rows", call. = FALSE)
  }
  x <- data.table::as.data.table(durations)[,
    c(by, duration_col, n_col),
    with = FALSE
  ]
  data.table::setnames(x, c(by, ".dur", if (!is.null(n_col)) ".n"))
  .st_check_count(x$.dur, duration_col, caller)
  if (any(abs(x$.dur - round(x$.dur)) > 1e-8)) {
    stop(
      caller,
      ": column `",
      duration_col,
      "` must contain whole numbers of days",
      call. = FALSE
    )
  }
  x[, .dur := as.integer(round(.dur))]
  if (is.null(n_col)) {
    x[, .n := 1L]
  } else {
    .st_check_count(x$.n, n_col, caller)
  }
  if (is.null(max_duration)) {
    max_duration <- max(x$.dur)
  } else if (
    !is.numeric(max_duration) ||
      length(max_duration) != 1L ||
      is.na(max_duration) ||
      max_duration < 0 ||
      max_duration != round(max_duration)
  ) {
    stop(
      caller,
      ": `max_duration` must be a single non-negative whole number",
      call. = FALSE
    )
  }
  max_duration <- as.integer(max_duration)

  # 4.2 counts on the complete j grid -----
  x[.dur > max_duration, .dur := max_duration]
  cnt <- x[, list(.n = sum(.n)), by = c(by, ".dur")]
  tot <- cnt[, list(.tot = sum(.n)), by = by]
  empty <- tot[.tot <= 0]
  if (nrow(empty) > 0L) {
    stop(
      caller,
      ": no postings in ",
      nrow(empty),
      " group(s)",
      .st_describe_group(empty, by),
      call. = FALSE
    )
  }
  grid <- .st_grid(.st_groups(cnt, by), 0:max_duration, ".dur")
  res <- cnt[grid, on = c(by, ".dur")]
  res[is.na(.n), .n := 0L]
  data.table::setorderv(res, c(by, ".dur"))

  # 4.3 survival = reverse cumulative share -----
  res[,
    surv := {
      rc <- rev(cumsum(rev(as.numeric(.n))))
      rc / rc[1L]
    },
    by = by
  ]
  res[, .n := NULL]
  data.table::setnames(res, ".dur", "j")
  res[]
}

# 5. standardized_stock -----

#' Standardised-duration stock
#'
#' Computes the stock that the observed inflow would generate under a fixed
#' duration distribution: `S*_d = sum_j F_{d - j} G(j)`, where `F` is the
#' daily inflow and `G` a survival curve from [duration_survival()].
#'
#' @details
#' The actual stock is `S_d = sum_j F_{d - j} P_d(duration >= j)` with a
#' duration distribution that may change over time (for example when a
#' source starts capping durations). Holding `G` fixed (for example at its
#' base-period value) removes the level shifts caused by changes in the
#' duration regime, while keeping the signal carried by the inflow. When the
#' durations are constant and `G` is computed from them, `S*` equals the
#' actual stock.
#'
#' The flow is placed on a complete daily grid from the first to the last
#' date over all groups; missing days count as zero inflow, while explicit
#' `NA` values propagate to the `max(j) + 1` days whose window contains them.
#' The convolution is exact (one-sided, [stats::filter()] with zero padding
#' before the first day). Days whose window starts before the first date of
#' the grid are computed on a truncated window and flagged with
#' `complete = FALSE`.
#'
#' The survival curves are matched to the flow on the columns of `by` that
#' are present in `survival`: a curve per source can be applied to a flow by
#' source and skill. When `survival` has none of the `by` columns, the same
#' curve applies to all groups.
#'
#' @param flow A data.frame or data.table with the daily inflow, one row per
#'   `by` x date.
#' @param survival A data.table as returned by [duration_survival()], with
#'   columns `j`, `surv` and optionally a subset of `by`.
#' @param by Character vector of grouping columns of `flow`, or `NULL`.
#' @param date_col Name of the date column of `flow` (class Date or IDate).
#' @param flow_col Name of the inflow column of `flow` (non-negative, `NA`
#'   allowed).
#'
#' @return A data.table with columns `by`, `date_col` (IDate), `stock_std`
#'   (double) and `complete` (logical: `TRUE` when the day is at least
#'   `max(j)` days after the first date of the grid), ordered by `by` and
#'   date.
#'
#' @examples
#' flow <- data.table::data.table(
#'   date = data.table::as.IDate("2024-01-01") + 0:9,
#'   inflow = c(5L, 3L, 4L, 6L, 2L, 5L, 4L, 3L, 6L, 5L)
#' )
#' g <- duration_survival(
#'   data.table::data.table(duration = c(1L, 3L), n = c(1L, 1L))
#' )
#' standardized_stock(flow, g)
#'
#' @export
standardized_stock <- function(
  flow,
  survival,
  by = NULL,
  date_col = "date",
  flow_col = "inflow"
) {
  caller <- "standardized_stock"

  # 5.1 validation -----
  .si_check_df(flow, "flow", caller)
  .si_check_df(survival, "survival", caller)
  .si_check_name(date_col, "date_col", caller)
  .si_check_name(flow_col, "flow_col", caller)
  .st_check_by(
    by,
    caller,
    reserved = c(date_col, flow_col, "j", "surv", "stock_std", "complete")
  )
  if (date_col %in% c("stock_std", "complete")) {
    stop(
      caller,
      ": `date_col` cannot be 'stock_std' or 'complete'",
      call. = FALSE
    )
  }
  check_columns(flow, c(by, date_col, flow_col), caller)
  check_columns(survival, c("j", "surv"), caller)
  if (nrow(flow) == 0L) {
    stop(caller, ": `flow` has no rows", call. = FALSE)
  }
  if (nrow(survival) == 0L) {
    stop(caller, ": `survival` has no rows", call. = FALSE)
  }

  f <- data.table::as.data.table(flow)[,
    c(by, date_col, flow_col),
    with = FALSE
  ]
  data.table::setnames(f, c(by, ".d", ".f"))
  f[, .d := .st_as_idate(.d, date_col, caller)]
  .st_check_count(f$.f, flow_col, caller, allow_na = TRUE)
  if (anyDuplicated(f, by = c(by, ".d"))) {
    stop(
      caller,
      ": `flow` must have one row per `by` x date",
      call. = FALSE
    )
  }

  # 5.2 survival curves -----
  surv_by <- intersect(by, names(survival))
  s <- data.table::as.data.table(survival)[,
    c(surv_by, "j", "surv"),
    with = FALSE
  ]
  if (!is.numeric(s$j) || anyNA(s$j) || any(s$j != round(s$j))) {
    stop(
      caller,
      ": column `j` of `survival` must contain whole numbers",
      call. = FALSE
    )
  }
  .st_check_count(s$surv, "surv", caller)
  data.table::setorderv(s, c(surv_by, "j"))
  if (length(surv_by) > 0L) {
    s[, .gid := .GRP, by = surv_by]
  } else {
    s[, .gid := 1L]
  }
  bad <- s[,
    list(.ok = identical(as.numeric(j), as.numeric(seq_len(.N) - 1L))),
    by = .gid
  ][!(.ok)]
  if (nrow(bad) > 0L) {
    stop(
      caller,
      ": each survival curve must have one row for every j = 0, ..., max(j)",
      if (length(surv_by) == 0L) {
        " (several curves without grouping columns in common with `by`?)"
      },
      call. = FALSE
    )
  }
  g_list <- split(s$surv, factor(s$.gid, levels = seq_len(max(s$.gid))))

  # 5.3 complete grid -----
  dates <- seq(min(f$.d), max(f$.d), by = 1L)
  f[, .p := TRUE]
  res <- f[.st_grid(.st_groups(f, by), dates, ".d"), on = c(by, ".d")]
  res[is.na(.p), .f := 0]
  res[, .p := NULL]
  if (length(surv_by) > 0L) {
    gmap <- unique(s[, c(surv_by, ".gid"), with = FALSE])
    res[gmap, .gid := i..gid, on = surv_by]
    miss <- res[is.na(.gid)]
    if (nrow(miss) > 0L) {
      stop(
        caller,
        ": no survival curve for group",
        .st_describe_group(miss, surv_by),
        call. = FALSE
      )
    }
  } else {
    res[, .gid := 1L]
  }
  data.table::setorderv(res, c(by, ".d"))

  # 5.4 convolution -----
  first_day <- dates[1L]
  res[,
    c("stock_std", "complete") := {
      g <- g_list[[.gid[1L]]]
      list(
        .st_convolve(.f, g),
        as.integer(.d - first_day) >= length(g) - 1L
      )
    },
    by = by
  ]

  res <- res[, c(by, ".d", "stock_std", "complete"), with = FALSE]
  data.table::setnames(res, ".d", date_col)
  res[]
}

# 6. to_weekly -----

#' Aggregate a daily series to weeks
#'
#' Aggregates daily rows to calendar weeks, taking the mean of some columns
#' (stocks) and the sum of others (flows).
#'
#' @details
#' Weeks start on `week_start` (ISO numbering: 1 = Monday, ..., 7 = Sunday;
#' default Monday, as in ISO 8601 weeks). The `week` column holds the first
#' day of the week. Partial weeks (at the edges of the series or with
#' missing days) are kept: `n_days` reports the number of days present, so
#' the caller can drop weeks with `n_days < 7`. `NA` values propagate to the
#' weekly mean or sum.
#'
#' @param dt A data.frame or data.table with one row per `by` x date.
#' @param by Character vector of grouping columns, or `NULL` for no grouping.
#' @param date_col Name of the date column (class Date or IDate).
#' @param mean_cols Character vector of numeric columns averaged over the
#'   week, or `NULL`.
#' @param sum_cols Character vector of numeric columns summed over the week,
#'   or `NULL`.
#' @param week_start First day of the week, integer in `1..7` with
#'   1 = Monday and 7 = Sunday.
#'
#' @return A data.table with columns `by`, `week` (IDate), `n_days`
#'   (integer), then `mean_cols` and `sum_cols` under their own names,
#'   ordered by `by` and `week`.
#'
#' @examples
#' d <- data.table::data.table(
#'   date = data.table::as.IDate("2024-01-01") + 0:13,
#'   stock = 100:113,
#'   inflow = rep(1:2, 7)
#' )
#' to_weekly(d, mean_cols = "stock", sum_cols = "inflow")
#'
#' @export
to_weekly <- function(
  dt,
  by = NULL,
  date_col = "date",
  mean_cols = NULL,
  sum_cols = NULL,
  week_start = 1L
) {
  caller <- "to_weekly"

  # 6.1 validation -----
  .si_check_df(dt, "dt", caller)
  .si_check_name(date_col, "date_col", caller)
  for (arg in c("mean_cols", "sum_cols")) {
    v <- get(arg)
    if (
      !is.null(v) &&
        (!is.character(v) || length(v) == 0L || anyNA(v) || anyDuplicated(v))
    ) {
      stop(
        caller,
        ": `",
        arg,
        "` must be NULL or a character vector of distinct column names",
        call. = FALSE
      )
    }
  }
  value_cols <- c(mean_cols, sum_cols)
  if (anyDuplicated(value_cols)) {
    stop(
      caller,
      ": a column cannot be in both `mean_cols` and `sum_cols`",
      call. = FALSE
    )
  }
  .st_check_by(
    by,
    caller,
    reserved = c(date_col, value_cols, "week", "n_days")
  )
  if (any(c(date_col, value_cols) %in% c("week", "n_days"))) {
    stop(
      caller,
      ": `date_col`, `mean_cols` and `sum_cols` cannot be 'week' or 'n_days'",
      call. = FALSE
    )
  }
  if (date_col %in% value_cols) {
    stop(caller, ": `date_col` cannot be aggregated", call. = FALSE)
  }
  if (
    !is.numeric(week_start) ||
      length(week_start) != 1L ||
      is.na(week_start) ||
      !(week_start %in% 1:7)
  ) {
    stop(
      caller,
      ": `week_start` must be a single integer in 1..7 (1 = Monday)",
      call. = FALSE
    )
  }
  check_columns(dt, c(by, date_col, value_cols), caller)
  x <- data.table::as.data.table(dt)[,
    c(by, date_col, value_cols),
    with = FALSE
  ]
  for (col in value_cols) {
    if (!is.numeric(x[[col]])) {
      stop(caller, ": column `", col, "` must be numeric", call. = FALSE)
    }
  }
  data.table::setnames(x, date_col, ".d")
  x[, .d := .st_as_idate(.d, date_col, caller)]
  if (anyDuplicated(x, by = c(by, ".d"))) {
    stop(caller, ": `dt` must have one row per `by` x date", call. = FALSE)
  }

  # 6.2 week of each day and aggregation -----
  # ISO weekday: Monday = 1, ..., Sunday = 7
  x[,
    week := {
      iso <- (data.table::wday(.d) + 5L) %% 7L + 1L
      .d - (iso - as.integer(week_start)) %% 7L
    }
  ]
  funs <- c(
    rep(list(mean), length(mean_cols)),
    rep(list(sum), length(sum_cols))
  )
  res <- x[,
    c(
      list(n_days = .N),
      Map(function(v, fun) fun(v), .SD, funs)
    ),
    by = c(by, "week"),
    .SDcols = value_cols
  ]
  data.table::setorderv(res, c(by, "week"))
  res[]
}
