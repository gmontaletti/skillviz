# Data reading and deduplication -----

# 1. read_esco_mapping -----

#' Read CPI-ESCO mapping table
#'
#' Read the CPI-to-ESCO level 4 mapping from either a DBI database
#' connection or an fst file on disk.
#'
#' @param conn A DBI connection object. If provided, reads the
#'   `mappa_cpv_esco_iv` table from the database.
#' @param file Character path to an fst file containing the mapping.
#'   Used when `conn` is NULL.
#' @return A data.table with columns from the mapping table, including
#'   at least `idesco_level_4`, `esco_level_4`, and `idcp_2011_v`.
#' @export
read_esco_mapping <- function(conn = NULL, file = NULL) {
  if (!is.null(conn)) {
    check_suggests("DBI", "to read from a database connection")
    dt <- DBI::dbReadTable(conn, "mappa_cpv_esco_iv")
    data.table::setDT(dt)
  } else if (!is.null(file)) {
    check_suggests("fst", "to read fst files")
    dt <- fst::read_fst(file, as.data.table = TRUE)
  } else {
    stop("read_esco_mapping: provide either 'conn' or 'file'", call. = FALSE)
  }
  dt
}

# 2. read_isco_groups -----

#' Read ESCO ISCO classification from CSV directory
#'
#' Reads the ISCOGroups CSV file from an ESCO classification dataset
#' directory and returns it as a data.table.
#'
#' @param esco_dir Character path to the ESCO dataset directory
#'   (e.g. `"ESCO dataset - v1.1.1 - classification - it - csv"`).
#'   The function looks for a file matching `ISCOGroups*.csv` inside
#'   this directory.
#' @return A data.table with ISCO group classification columns
#'   including `code` and `preferredLabel`.
#' @export
read_isco_groups <- function(esco_dir) {
  if (!dir.exists(esco_dir)) {
    stop("read_isco_groups: directory not found: ", esco_dir, call. = FALSE)
  }
  csv_files <- list.files(
    esco_dir,
    pattern = "^ISCOGroups.*\\.csv$",
    full.names = TRUE
  )
  if (length(csv_files) == 0L) {
    stop(
      "read_isco_groups: no ISCOGroups CSV found in ",
      esco_dir,
      call. = FALSE
    )
  }
  dt <- data.table::fread(csv_files[[1L]], fill = TRUE)
  dt
}

# 3. deduplicate_annunci -----

#' Deduplicate job announcements
#'
#' Collapse duplicate rows sharing the same `general_id` into a
#' single row, keeping modal values for categorical columns via
#' [collapse::fmode()] and computing an activity flag based on
#' `xdata`.
#'
#' @param ann A data.table of announcements. Must contain columns
#'   `general_id`, `idcity`, `idesco_level_4`, and `xdata`.
#' @param active_date An IDate or Date cutoff: announcements with
#'   `xdata >= active_date` are flagged as active. Defaults to
#'   `Sys.Date()`.
#' @return A data.table with one row per `general_id` and columns:
#'   `general_id`, `N` (original row count), `idcity` (modal),
#'   `idesco_level_4` (modal), `attivo` (1 if any row active, 0
#'   otherwise).
#' @export
deduplicate_annunci <- function(ann, active_date = Sys.Date()) {
  check_columns(
    ann,
    c("general_id", "idcity", "idesco_level_4", "xdata"),
    caller = "deduplicate_annunci"
  )
  check_suggests("collapse", "for modal aggregation (fmode)")
  ann <- data.table::copy(ann)
  active_date <- data.table::as.IDate(active_date)
  ann[, attivo := 0L]
  ann[xdata >= active_date, attivo := 1L]
  out <- ann[,
    .(
      N = .N,
      idcity = collapse::fmode(idcity),
      idesco_level_4 = collapse::fmode(idesco_level_4),
      attivo = max(attivo)
    ),
    keyby = general_id
  ]
  out
}
