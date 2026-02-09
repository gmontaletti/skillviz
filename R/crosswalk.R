# Classification crosswalks and announcement merging -----

# 1. build_cpi_esco_crosswalk -----

#' Build ESCO-to-CP2021 level 3 crosswalk
#'
#' Merge the CPI-ESCO mapping with the CP2021 level 3 classification
#' to produce a lookup table that maps each `idesco_level_4` code to
#' its Italian ESCO label and CP2021 3-digit group.
#'
#' @param esco_mapping A data.table from [read_esco_mapping()],
#'   containing at least `idesco_level_4`, `esco_level_4`, and
#'   `idcp_2011_v`.
#' @param cpi3 A data.table with CP2021 level 3 classification.
#'   Must contain `cod_3` and `nome_3`.
#' @return A data.table with one row per `idesco_level_4` and columns:
#'   `idesco_level_4`, `it_esco_level_4` (collapsed Italian ESCO
#'   labels), `cod_3`, `nome_3`.
#' @export
build_cpi_esco_crosswalk <- function(esco_mapping, cpi3) {
  check_columns(
    esco_mapping,
    c("idesco_level_4", "esco_level_4", "idcp_2011_v"),
    caller = "build_cpi_esco_crosswalk"
  )
  check_columns(cpi3, c("cod_3", "nome_3"), caller = "build_cpi_esco_crosswalk")

  mapping <- data.table::copy(esco_mapping)
  mapping[, cod_3 := substring(idcp_2011_v, 1L, 5L)]

  esco <- merge(mapping, cpi3, by = "cod_3")

  cpi_esco <- esco[,
    .(
      it_esco_level_4 = paste(unique(esco_level_4), collapse = ", "),
      cod_3 = paste(data.table::first(cod_3), collapse = ", "),
      nome_3 = paste(data.table::first(nome_3), collapse = ", ")
    ),
    keyby = .(idesco_level_4)
  ]

  cpi_esco
}

# 2. prepare_annunci_esco -----

#' Merge announcements with ESCO mapping and parse dates
#'
#' Joins the announcements table with the ESCO mapping to add Italian
#' profession labels, and parses year/month/day columns into proper
#' IDate fields.
#'
#' @param ann A data.table of announcements with columns
#'   `idesco_level_4`, `year_grab_date`, `month_grab_date`,
#'   `day_grab_date`, `year_expire_date`, `month_expire_date`,
#'   `day_expire_date`, and `general_id`.
#' @param esco_mapping A data.table from [read_esco_mapping()],
#'   containing at least `idesco_level_4` and `esco_level_4`.
#' @return A data.table with added columns `it_esco_level_4`,
#'   `gdate` (grab date as IDate), and `edate` (expire date as
#'   IDate). Deduplicated to unique combinations of `general_id`,
#'   `gdate`, `idesco_level_4`, `it_esco_level_4`.
#' @export
prepare_annunci_esco <- function(ann, esco_mapping) {
  check_columns(
    ann,
    c(
      "general_id",
      "idesco_level_4",
      "year_grab_date",
      "month_grab_date",
      "day_grab_date",
      "year_expire_date",
      "month_expire_date",
      "day_expire_date"
    ),
    caller = "prepare_annunci_esco"
  )
  check_columns(
    esco_mapping,
    c("idesco_level_4", "esco_level_4"),
    caller = "prepare_annunci_esco"
  )

  # 2a. build id-to-label lookup -----
  idesco <- esco_mapping[,
    .(
      it_esco_level_4 = paste(unique(esco_level_4), collapse = ", ")
    ),
    keyby = .(idesco_level_4)
  ]

  dt <- data.table::copy(ann)
  data.table::setkey(dt, general_id)

  # 2b. parse date columns -----
  parse_ymd_columns(dt, "grab_date", "gdate")
  parse_ymd_columns(dt, "expire_date", "edate")

  # 2c. merge with ESCO labels -----
  dt <- merge(dt, idesco, by = "idesco_level_4", all.x = TRUE, all.y = FALSE)

  dt <- unique(dt[, .(general_id, gdate, idesco_level_4, it_esco_level_4)])
  dt
}

# 3. prepare_annunci_geography -----

#' Prepare announcements with CPI geographic dimension
#'
#' Merges announcements with ESCO labels and a territorial mapping
#' table to add the CPI (Centro per l'Impiego) field, then
#' aggregates unique announcement counts by CPI, profession, and
#' year.
#'
#' @param ann A data.table of announcements with columns
#'   `idesco_level_4`, `idcity`, `general_id`, and year/month/day
#'   grab and expire date columns.
#' @param esco_mapping A data.table from [read_esco_mapping()],
#'   containing at least `idesco_level_4` and `esco_level_4`.
#' @param territoriale A data.table with territorial classification.
#'   Must contain `COD_ISTAT`, `CPI`, and `COD_REGIONE_PAUT`.
#' @param regione Integer region code to filter the territorial
#'   table. Defaults to `10L`.
#' @return A data.table with columns `CPI`, `it_esco_level_4`,
#'   `year_grab_date`, and `N` (unique announcement count), filtered
#'   to complete cases and ordered by CPI, year, descending N.
#' @export
prepare_annunci_geography <- function(
  ann,
  esco_mapping,
  territoriale,
  regione = 10L
) {
  check_columns(
    ann,
    c(
      "general_id",
      "idesco_level_4",
      "idcity",
      "year_grab_date",
      "month_grab_date",
      "day_grab_date",
      "year_expire_date",
      "month_expire_date",
      "day_expire_date"
    ),
    caller = "prepare_annunci_geography"
  )
  check_columns(
    esco_mapping,
    c("idesco_level_4", "esco_level_4"),
    caller = "prepare_annunci_geography"
  )
  check_columns(
    territoriale,
    c("COD_ISTAT", "CPI", "COD_REGIONE_PAUT"),
    caller = "prepare_annunci_geography"
  )

  # 3a. filter territorial table -----
  terr <- territoriale[
    COD_REGIONE_PAUT == regione & CPI != "",
    .(idcity = COD_ISTAT, CPI)
  ]

  # 3b. build id-to-label lookup -----
  idesco <- esco_mapping[,
    .(
      it_esco_level_4 = paste(unique(esco_level_4), collapse = ", ")
    ),
    keyby = .(idesco_level_4)
  ]

  dt <- data.table::copy(ann)
  data.table::setkey(dt, general_id)

  # 3c. parse date columns -----
  parse_ymd_columns(dt, "grab_date", "gdate")
  parse_ymd_columns(dt, "expire_date", "edate")

  # 3d. merge with ESCO labels -----
  dt <- merge(dt, idesco, by = "idesco_level_4", all.x = TRUE, all.y = FALSE)

  # 3e. merge with territorial data -----
  dt <- merge(dt, terr, by = "idcity", all.x = TRUE, all.y = FALSE)

  # 3f. aggregate by CPI, profession, year -----
  cpi <- dt[,
    .(N = data.table::uniqueN(general_id)),
    keyby = .(CPI, it_esco_level_4, year_grab_date)
  ]
  cpi <- cpi[stats::complete.cases(cpi)]
  data.table::setorder(cpi, CPI, year_grab_date, -N)
  cpi
}
