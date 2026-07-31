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
#' \strong{Deprecated}. Use \code{\link{read_oja_itaposts}} instead.
#' Reading the raw Lightcast ZIP archives is superseded by the DuckDB
#' store maintained by the \pkg{itaposts} package, which owns the read,
#' dedup and join logic this function duplicates. Retained for backward
#' compatibility; scheduled for removal in a future release.
#'
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
#' @seealso \code{\link{read_oja_itaposts}} for the supported replacement.
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
  .Deprecated("read_oja_itaposts", package = "skillviz")

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

# 5. normalize_ojv -----

#' Normalize OJV data from ZIP files
#'
#' Reads all three OJV file types (postings, skills, postings_raw) from a
#' directory of ZIP files, deduplicates postings to one row per
#' \code{general_id}, and ensures referential integrity across tables.
#'
#' Deduplication keeps the most recent observation per \code{general_id},
#' determined by \code{year_grab_date} and \code{month_grab_date} columns
#' (descending sort). If these columns are absent, the first occurrence is
#' kept.
#'
#' @param path Character scalar. Directory containing the ZIP files.
#' @param years Integer vector of years to include, or \code{NULL}
#'   (default) for all available years. Passed to \code{\link{read_ojv_zip}}.
#' @param months Integer vector of months to include, or \code{NULL}
#'   (default) for all available months. Passed to \code{\link{read_ojv_zip}}.
#' @param verbose Logical scalar. If \code{TRUE} (default), prints
#'   progress messages to the console.
#'
#' @return A named list with three \code{data.table} elements, all keyed
#'   on \code{general_id}:
#'   \describe{
#'     \item{postings}{Deduplicated job posting metadata (one row per
#'       \code{general_id}).}
#'     \item{skills}{Skill-level data, filtered to \code{general_id}
#'       values present in \code{postings}.}
#'     \item{companies}{Company name data from postings_raw, filtered to
#'       \code{general_id} values present in \code{postings}.}
#'   }
#'
#' @details
#' \strong{Deprecated}. Use \code{\link{read_oja_itaposts}} instead.
#' Reading the raw Lightcast ZIP archives is superseded by the DuckDB
#' store maintained by the \pkg{itaposts} package, which performs the same
#' dedup and referential-integrity steps at ingest time. The replacement
#' returns the same \code{list(postings, skills, companies)} shape, so
#' call sites only need their data-loading line changed. Retained for
#' backward compatibility; scheduled for removal in a future release.
#'
#' @seealso \code{\link{read_oja_itaposts}} for the supported replacement.
#'
#' @export
#' @examples
#' \dontrun{
#' ojv <- normalize_ojv("/path/to/zip/dir")
#' ojv$postings
#' ojv$skills
#' ojv$companies
#'
#' # Filter to 2024 data only
#' ojv24 <- normalize_ojv("/path/to/zip/dir", years = 2024L)
#' }
normalize_ojv <- function(path, years = NULL, months = NULL, verbose = TRUE) {
  .Deprecated("read_oja_itaposts", package = "skillviz")

  # 1. read all three types -----
  # read_ojv_zip() is deprecated too: muffle its warning so a single
  # normalize_ojv() call reports the deprecation once, not four times.
  read_type <- function(type) {
    withCallingHandlers(
      read_ojv_zip(
        path,
        type = type,
        years = years,
        months = months,
        verbose = verbose
      ),
      deprecatedWarning = function(w) invokeRestart("muffleWarning")
    )
  }

  postings <- read_type("postings")
  skills <- read_type("skills")
  companies <- read_type("postings_raw")

  # 2. deduplicate postings -----
  if (nrow(postings) > 0L && "general_id" %in% names(postings)) {
    grab_cols <- intersect(
      c("year_grab_date", "month_grab_date"),
      names(postings)
    )
    if (length(grab_cols) > 0L) {
      data.table::setorderv(
        postings,
        grab_cols,
        order = rep(-1L, length(grab_cols))
      )
    }
    n_before <- nrow(postings)
    postings <- unique(postings, by = "general_id", fromLast = FALSE)
    if (verbose) {
      n_dropped <- n_before - nrow(postings)
      message(
        "normalize_ojv: deduplicated postings from ",
        format(n_before, big.mark = ","),
        " to ",
        format(nrow(postings), big.mark = ","),
        " rows (",
        format(n_dropped, big.mark = ","),
        " duplicates removed)"
      )
    }
  }

  # 3. enforce referential integrity -----
  if (nrow(postings) > 0L && "general_id" %in% names(postings)) {
    valid_ids <- postings[["general_id"]]
    if (nrow(skills) > 0L && "general_id" %in% names(skills)) {
      skills <- skills[general_id %in% valid_ids]
    }
    if (nrow(companies) > 0L && "general_id" %in% names(companies)) {
      companies <- companies[general_id %in% valid_ids]
    }
  }

  # 4. set keys -----
  if (nrow(postings) > 0L && "general_id" %in% names(postings)) {
    data.table::setkey(postings, general_id)
  }
  if (nrow(skills) > 0L && "general_id" %in% names(skills)) {
    data.table::setkey(skills, general_id)
  }
  if (nrow(companies) > 0L && "general_id" %in% names(companies)) {
    data.table::setkey(companies, general_id)
  }

  list(postings = postings, skills = skills, companies = companies)
}

# 6. read_oja_itaposts -----

#' Read normalized OJA data from the itaposts DuckDB store
#'
#' Loads Online Job Advertisement (OJA) data from the shared DuckDB store
#' maintained by the \pkg{itaposts} package and returns it in the same
#' three-table shape produced by \code{\link{normalize_ojv}}, with the
#' skill-side column names lower-cased so they match the names the rest of
#' \pkg{skillviz} consumes.
#'
#' @param con A connection to the itaposts DuckDB store, as returned by
#'   \code{itaposts::oja_connect()}.
#' @param snapshots Character vector of \code{snapshot_id} values to
#'   include (e.g. \code{"ITC4_2026_2"}), or \code{NULL} (default) for all
#'   snapshots in the store.
#' @param region_code Character vector of NUTS-2 region codes to include
#'   (e.g. \code{"ITC4"}), or \code{NULL} (default) for all regions.
#' @param years Integer vector of years to include, or \code{NULL}
#'   (default) for all available years. Intersected with \code{snapshots}
#'   against the store's snapshot metadata.
#' @param months Integer vector of months to include, or \code{NULL}
#'   (default) for all available months.
#' @param verbose Logical scalar. If \code{TRUE} (default), itaposts
#'   reports the row count of each returned table.
#'
#' @return A named list with three \code{data.table} elements, all keyed
#'   on \code{general_id}:
#'   \describe{
#'     \item{postings}{One row per \code{general_id}, with the CP2021 and
#'       ESCO occupation hierarchies already joined in (including
#'       \code{idesco_level_4}), the geography, contract, education,
#'       sector, experience, working-hours, salary and source attributes,
#'       and \code{companyname}.}
#'     \item{skills}{Long-format skill rows (one per posting/skill pair)
#'       with lower-cased ESCO skill attribute columns.}
#'     \item{companies}{The \code{general_id}/\code{companyname} pair, with
#'       missing and empty company names dropped.}
#'   }
#'
#' @details
#' This is the recommended way to load OJA data into \pkg{skillviz}.
#' \code{\link{normalize_ojv}} reads the raw vendor ZIP archives directly
#' and duplicates read/dedup logic that now lives in \pkg{itaposts}; it is
#' retained for backward compatibility only.
#'
#' The connection is caller-owned, following the itaposts idiom:
#'
#' \preformatted{
#' con <- itaposts::oja_connect()
#' on.exit(itaposts::oja_disconnect(con), add = TRUE)
#' }
#'
#' \code{itaposts::oja_normalised()} re-emits the skill columns under their
#' historical Lightcast SHOUTING names. This function lower-cases them, so
#' \code{IDESCOSKILL_LEVEL_3}, \code{ESCOSKILL_LEVEL_3},
#' \code{ESCO_V0101_OBSOLETE}, \code{ESCO_v0101_DESCRIPTION},
#' \code{ESCO_V0101_URI}, \code{ESCO_V0101_SKILLSTYPE},
#' \code{ESCO_V0101_REUSETYPE}, \code{ESCO_V0101_GREEN} and
#' \code{ESCO_V0101_LANGUAGE} become \code{idescoskill_level_3},
#' \code{escoskill_level_3}, \code{esco_v0101_obsolete},
#' \code{esco_v0101_description}, \code{esco_v0101_uri},
#' \code{esco_v0101_skillstype}, \code{esco_v0101_reusetype},
#' \code{esco_v0101_green} and \code{esco_v0101_language}. Only columns
#' actually present are renamed, so the function tolerates changes to the
#' itaposts shim's column set.
#'
#' \code{pillar_softskills} and \code{esco_v0101_ict} are \emph{not}
#' returned: the vendor dropped them in the \code{data_v2} delivery and
#' itaposts has no substitute for them. Downstream code must treat them as
#' optional.
#'
#' @seealso \code{\link{normalize_ojv}} for the legacy ZIP-based reader.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- itaposts::oja_connect()
#' on.exit(itaposts::oja_disconnect(con), add = TRUE)
#'
#' ojv <- read_oja_itaposts(con)
#' ojv$postings
#' ojv$skills
#' ojv$companies
#'
#' # Lombardy, 2024 only
#' ojv24 <- read_oja_itaposts(con, region_code = "ITC4", years = 2024L)
#' }
read_oja_itaposts <- function(
  con,
  snapshots = NULL,
  region_code = NULL,
  years = NULL,
  months = NULL,
  verbose = TRUE
) {
  # 1. input validation -----
  if (missing(con) || !inherits(con, "DBIConnection")) {
    stop(
      "read_oja_itaposts: 'con' must be a DBI connection to the itaposts ",
      "store, as returned by itaposts::oja_connect()",
      call. = FALSE
    )
  }

  # 2. delegate to the itaposts compatibility shim -----
  ojv <- itaposts::oja_normalised(
    con,
    snapshots = snapshots,
    region_code = region_code,
    years = years,
    months = months,
    verbose = verbose
  )

  postings <- ojv$postings
  skills <- ojv$skills
  companies <- ojv$companies

  # 3. lower-case the legacy SHOUTING skill columns -----
  nm <- names(skills)
  upper <- nm[nm != tolower(nm) & !tolower(nm) %in% nm]
  if (length(upper) > 0L) {
    data.table::setnames(skills, upper, tolower(upper))
  }

  # 4. set keys -----
  for (dt in list(postings, skills, companies)) {
    if ("general_id" %in% names(dt)) {
      data.table::setkeyv(dt, "general_id")
    }
  }

  list(postings = postings, skills = skills, companies = companies)
}
