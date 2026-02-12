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

# 4. read_ojv_zip -----

#' Read OJV data from ZIP files
#'
#' Reads Online Job Vacancy (OJV) data from a directory of ZIP files
#' following the ITC4 naming convention. Each ZIP file contains a single
#' CSV that is read directly via piped \code{unzip -p} without extracting
#' to disk.
#'
#' ZIP files are expected to follow the naming pattern
#' \code{ITC4_{year}_{month}_{type}.zip} where \code{type} is one of
#' \code{"postings"}, \code{"skills"}, or \code{"postings_raw"}.
#'
#' @param path Character scalar. Directory containing the ZIP files.
#' @param type Character scalar. File type to read: one of
#'   \code{"postings"}, \code{"skills"}, or \code{"postings_raw"}.
#'   Defaults to \code{"postings"}.
#' @param years Integer vector of years to include, or \code{NULL}
#'   (default) for all available years.
#' @param months Integer vector of months to include, or \code{NULL}
#'   (default) for all available months.
#' @param select Character vector of column names to read, passed to
#'   \code{\link[data.table]{fread}}'s \code{select} argument.
#'   \code{NULL} (default) reads all columns.
#' @param nrows Numeric scalar. Maximum number of rows to read per
#'   file, passed to \code{\link[data.table]{fread}}'s \code{nrows}
#'   argument. Default \code{Inf} reads all rows.
#' @param verbose Logical scalar. If \code{TRUE} (default), prints
#'   progress messages to the console.
#'
#' @return A \code{data.table} containing the row-bound contents of
#'   all matching ZIP files. Returns an empty \code{data.table} if no
#'   matching files are found.
#'
#' @details
#' The function uses \code{data.table::fread()} with the \code{cmd}
#' argument to pipe \code{unzip -p} output directly, avoiding temporary
#' file extraction. This is efficient for large CSV files compressed
#' inside ZIP archives.
#'
#' The three file types correspond to different OJV datasets:
#' \describe{
#'   \item{postings}{Job posting metadata including location, contract,
#'     education, sector, salary, and occupation classification columns.}
#'   \item{skills}{Skill-level data linked to postings via
#'     \code{general_id}, including ESCO skill taxonomy fields.}
#'   \item{postings_raw}{Minimal posting data with company name.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Read all postings data
#' dt <- read_ojv_zip("/path/to/zip/dir", type = "postings")
#'
#' # Read skills for 2023, months 1-6, selected columns only
#' sk <- read_ojv_zip(
#'   "/path/to/zip/dir",
#'   type = "skills",
#'   years = 2023L,
#'   months = 1:6,
#'   select = c("general_id", "ESCOSKILL_LEVEL_3", "ESCO_V0101_REUSETYPE")
#' )
#'
#' # Read first 1000 rows per file for exploration
#' sample <- read_ojv_zip("/path/to/zip/dir", type = "postings", nrows = 1000)
#' }
read_ojv_zip <- function(
  path,
  type = c("postings", "skills", "postings_raw"),
  years = NULL,
  months = NULL,
  select = NULL,
  nrows = Inf,
  verbose = TRUE
) {
  # 1. input validation -----
  type <- match.arg(type)

  if (!is.character(path) || length(path) != 1L) {
    stop(
      "read_ojv_zip: 'path' must be a single character string",
      call. = FALSE
    )
  }
  if (!dir.exists(path)) {
    stop("read_ojv_zip: directory not found: ", path, call. = FALSE)
  }
  if (!is.null(years)) {
    years <- as.integer(years)
    if (anyNA(years)) {
      stop("read_ojv_zip: 'years' must be coercible to integer", call. = FALSE)
    }
  }
  if (!is.null(months)) {
    months <- as.integer(months)
    if (anyNA(months) || any(months < 1L | months > 12L)) {
      stop(
        "read_ojv_zip: 'months' must be integers between 1 and 12",
        call. = FALSE
      )
    }
  }
  if (!is.null(select) && !is.character(select)) {
    stop(
      "read_ojv_zip: 'select' must be a character vector or NULL",
      call. = FALSE
    )
  }

  # 2. discover matching zip files -----
  pattern <- paste0("^ITC4_\\d+_\\d+_", type, "\\.zip$")
  all_zips <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(all_zips) == 0L) {
    if (verbose) {
      message(
        "read_ojv_zip: no ZIP files found for type '",
        type,
        "' in ",
        path
      )
    }
    return(data.table::data.table())
  }

  # 3. parse year/month from filenames and filter -----
  basenames <- basename(all_zips)
  parts <- strsplit(basenames, "_", fixed = TRUE)
  file_years <- vapply(parts, function(p) as.integer(p[[2L]]), integer(1L))
  file_months <- vapply(parts, function(p) as.integer(p[[3L]]), integer(1L))

  keep <- rep(TRUE, length(all_zips))
  if (!is.null(years)) {
    keep <- keep & file_years %in% years
  }
  if (!is.null(months)) {
    keep <- keep & file_months %in% months
  }

  zips <- all_zips[keep]

  if (length(zips) == 0L) {
    if (verbose) {
      message(
        "read_ojv_zip: no ZIP files match the requested years/months ",
        "(found ",
        length(all_zips),
        " files of type '",
        type,
        "')"
      )
    }
    return(data.table::data.table())
  }

  # sort by year then month for deterministic order
  ord <- order(file_years[keep], file_months[keep])
  zips <- zips[ord]

  if (verbose) {
    message(
      "read_ojv_zip: reading ",
      length(zips),
      " file(s) of type '",
      type,
      "'"
    )
  }

  # 4. read each zip via fread cmd -----
  t0 <- proc.time()
  chunks <- vector("list", length(zips))

  for (idx in seq_along(zips)) {
    zip_path <- zips[[idx]]
    if (verbose) {
      message("  [", idx, "/", length(zips), "] ", basename(zip_path))
    }

    fread_args <- list(input = zip_path)
    if (!is.null(select)) {
      fread_args$select <- select
    }
    if (is.finite(nrows)) {
      fread_args$nrows <- as.integer(nrows)
    }

    chunks[[idx]] <- do.call(data.table::fread, fread_args)
  }

  # 5. bind and return -----
  dt <- data.table::rbindlist(chunks, use.names = TRUE, fill = TRUE)

  if (verbose) {
    elapsed <- (proc.time() - t0)["elapsed"]
    message(
      "read_ojv_zip: done. ",
      format(nrow(dt), big.mark = ","),
      " rows, ",
      ncol(dt),
      " columns in ",
      round(elapsed, 1),
      "s"
    )
  }

  dt
}
