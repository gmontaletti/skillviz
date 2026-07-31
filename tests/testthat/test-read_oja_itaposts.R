# Tests for read_oja_itaposts -----

# The function delegates to itaposts::oja_normalised(), which needs a live
# DuckDB store. Tests mock that binding and feed it synthetic tables shaped
# like the itaposts shim output (see itaposts R/shim.R).

# Helper: a stand-in for an itaposts DuckDB connection -----
fake_con <- function() {
  structure(list(), class = "DBIConnection")
}

# Fixture generators mirroring itaposts::oja_normalised() -----
make_itaposts_postings <- function(ids) {
  data.table::data.table(
    general_id = as.integer(ids),
    year_grab_date = 2026L,
    month_grab_date = 2L,
    idcity = paste0("city_", ids),
    city = paste0("Comune ", ids),
    province = "Milano",
    region = "Lombardia",
    idcontract = "1",
    contract = "tempo indeterminato",
    idsector = "10",
    sector = "manifattura",
    cp2021_id_level_5 = paste0("2.1.1.1.", ids),
    cp2021_id_level_4 = "2.1.1.1",
    cp2021_id_level_3 = "2.1.1",
    idesco_level_5 = paste0("2511.", ids),
    idesco_level_4 = "2511",
    esco_level_4 = "analisti di sistema",
    source = "portale",
    snapshot_id = "ITC4_2026_2",
    region_code = "ITC4",
    companyname = paste0("company_", ids)
  )
}

# Skills carry the historical Lightcast SHOUTING names re-emitted by the
# itaposts shim; note the mixed-case ESCO_v0101_DESCRIPTION alias.
make_itaposts_skills <- function(ids) {
  data.table::data.table(
    general_id = as.integer(ids),
    year_grab_date = 2026L,
    month_grab_date = 2L,
    IDESCOSKILL_LEVEL_3 = paste0("S1.4.", ids),
    ESCOSKILL_LEVEL_3 = paste0("skill_", ids),
    ESCO_V0101_OBSOLETE = 0L,
    ESCO_v0101_DESCRIPTION = paste0("descrizione ", ids),
    ESCO_V0101_URI = paste0("http://data.europa.eu/esco/skill/", ids),
    ESCO_V0101_SKILLSTYPE = "skill/competence",
    ESCO_V0101_REUSETYPE = "transversal",
    ESCO_V0101_GREEN = 0L,
    ESCO_V0101_LANGUAGE = 0L,
    snapshot_id = "ITC4_2026_2"
  )
}

make_itaposts_companies <- function(ids) {
  data.table::data.table(
    general_id = as.integer(ids),
    companyname = paste0("company_", ids)
  )
}

# Helper: install a mocked oja_normalised returning the given tables and
# recording the arguments it was called with.
local_mocked_oja <- function(
  postings = make_itaposts_postings(1:3),
  skills = make_itaposts_skills(1:3),
  companies = make_itaposts_companies(1:3),
  record = NULL,
  env = parent.frame()
) {
  fake <- function(
    con,
    snapshots = NULL,
    region_code = NULL,
    years = NULL,
    months = NULL,
    verbose = TRUE
  ) {
    if (!is.null(record)) {
      record$args <- list(
        con = con,
        snapshots = snapshots,
        region_code = region_code,
        years = years,
        months = months,
        verbose = verbose
      )
    }
    list(postings = postings, skills = skills, companies = companies)
  }

  testthat::local_mocked_bindings(
    oja_normalised = fake,
    .package = "itaposts",
    .env = env
  )
}

# 1. return structure -----

test_that("returns a named list with three data.tables", {
  local_mocked_oja()

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_type(result, "list")
  expect_named(result, c("postings", "skills", "companies"))
  expect_s3_class(result$postings, "data.table")
  expect_s3_class(result$skills, "data.table")
  expect_s3_class(result$companies, "data.table")
  expect_equal(nrow(result$postings), 3L)
  expect_equal(nrow(result$skills), 3L)
  expect_equal(nrow(result$companies), 3L)
})

test_that("all three tables are keyed on general_id", {
  local_mocked_oja()

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_equal(data.table::key(result$postings), "general_id")
  expect_equal(data.table::key(result$skills), "general_id")
  expect_equal(data.table::key(result$companies), "general_id")
})

# 2. skill column lower-casing -----

test_that("skills columns are lower-cased to the names skillviz consumes", {
  local_mocked_oja()

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_named(
    result$skills,
    c(
      "general_id",
      "year_grab_date",
      "month_grab_date",
      "idescoskill_level_3",
      "escoskill_level_3",
      "esco_v0101_obsolete",
      "esco_v0101_description",
      "esco_v0101_uri",
      "esco_v0101_skillstype",
      "esco_v0101_reusetype",
      "esco_v0101_green",
      "esco_v0101_language",
      "snapshot_id"
    ),
    ignore.order = TRUE
  )
  # no residual upper-case column survives the rename
  expect_identical(names(result$skills), tolower(names(result$skills)))
})

test_that("lower-casing preserves values and column count", {
  local_mocked_oja()

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_equal(length(names(result$skills)), 13L)
  expect_equal(
    sort(result$skills$escoskill_level_3),
    c("skill_1", "skill_2", "skill_3")
  )
  expect_equal(
    sort(result$skills$idescoskill_level_3),
    c("S1.4.1", "S1.4.2", "S1.4.3")
  )
  expect_true(all(result$skills$esco_v0101_reusetype == "transversal"))
})

test_that("postings and companies column names are left untouched", {
  local_mocked_oja()

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_true("idesco_level_4" %in% names(result$postings))
  expect_true("cp2021_id_level_3" %in% names(result$postings))
  expect_true("companyname" %in% names(result$postings))
  expect_named(result$companies, c("general_id", "companyname"))
})

# 3. argument forwarding -----

test_that("filter arguments are forwarded to itaposts::oja_normalised", {
  record <- new.env(parent = emptyenv())
  local_mocked_oja(record = record)

  con <- fake_con()
  read_oja_itaposts(
    con,
    snapshots = "ITC4_2026_2",
    region_code = "ITC4",
    years = 2026L,
    months = 2L,
    verbose = FALSE
  )

  expect_identical(record$args$con, con)
  expect_identical(record$args$snapshots, "ITC4_2026_2")
  expect_identical(record$args$region_code, "ITC4")
  expect_identical(record$args$years, 2026L)
  expect_identical(record$args$months, 2L)
  expect_false(record$args$verbose)
})

test_that("defaults forward NULL filters and verbose = TRUE", {
  record <- new.env(parent = emptyenv())
  local_mocked_oja(record = record)

  read_oja_itaposts(fake_con())

  expect_null(record$args$snapshots)
  expect_null(record$args$region_code)
  expect_null(record$args$years)
  expect_null(record$args$months)
  expect_true(record$args$verbose)
})

# 4. input validation -----

test_that("missing con errors with a clear message", {
  local_mocked_oja()

  expect_error(
    read_oja_itaposts(),
    "'con' must be a DBI connection"
  )
})

test_that("con of the wrong class errors with a clear message", {
  local_mocked_oja()

  expect_error(
    read_oja_itaposts("~/oja/itposts.duckdb"),
    "'con' must be a DBI connection"
  )
  expect_error(
    read_oja_itaposts(list(host = "localhost")),
    "'con' must be a DBI connection"
  )
  expect_error(
    read_oja_itaposts(NULL),
    "'con' must be a DBI connection"
  )
})

test_that("validation error names the itaposts constructor", {
  local_mocked_oja()

  expect_error(
    read_oja_itaposts(42L),
    "itaposts::oja_connect\\(\\)",
    fixed = FALSE
  )
})

# 5. tolerance of a changed itaposts column set -----

test_that("a reduced skills column set is renamed without error", {
  # only two of the historical upper-case columns survive
  skills <- data.table::data.table(
    general_id = 1:3,
    IDESCOSKILL_LEVEL_3 = paste0("S1.4.", 1:3),
    ESCOSKILL_LEVEL_3 = paste0("skill_", 1:3)
  )
  local_mocked_oja(skills = skills)

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_named(
    result$skills,
    c("general_id", "idescoskill_level_3", "escoskill_level_3")
  )
  expect_equal(nrow(result$skills), 3L)
})

test_that("already lower-cased skills pass through unchanged", {
  skills <- data.table::data.table(
    general_id = 1:3,
    idescoskill_level_3 = paste0("S1.4.", 1:3),
    escoskill_level_3 = paste0("skill_", 1:3)
  )
  local_mocked_oja(skills = skills)

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_named(
    result$skills,
    c("general_id", "idescoskill_level_3", "escoskill_level_3")
  )
})

test_that("a mixed-case collision leaves the upper-case column alone", {
  # both cases present: renaming would duplicate a name, so it is skipped
  skills <- data.table::data.table(
    general_id = 1:3,
    escoskill_level_3 = paste0("skill_", 1:3),
    ESCOSKILL_LEVEL_3 = paste0("SKILL_", 1:3)
  )
  local_mocked_oja(skills = skills)

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_equal(anyDuplicated(names(result$skills)), 0L)
  expect_true(all(
    c("escoskill_level_3", "ESCOSKILL_LEVEL_3") %in% names(result$skills)
  ))
})

test_that("empty tables are returned without error and left unkeyed", {
  local_mocked_oja(
    postings = make_itaposts_postings(1L)[0],
    skills = make_itaposts_skills(1L)[0],
    companies = data.table::data.table()
  )

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  expect_equal(nrow(result$postings), 0L)
  expect_equal(nrow(result$skills), 0L)
  expect_equal(nrow(result$companies), 0L)
  expect_null(data.table::key(result$companies))
  expect_true("escoskill_level_3" %in% names(result$skills))
})

# 6. downstream compatibility -----

test_that("skills output satisfies build_skillist's required columns", {
  local_mocked_oja()

  result <- read_oja_itaposts(fake_con(), verbose = FALSE)

  diffusion <- data.table::data.table(
    escoskill_level_3 = paste0("skill_", 1:3),
    N = c(3L, 2L, 1L),
    tf = c(50, 30, 20),
    idf = c(0.5, 1, 2),
    diffusione = c("alta", "centrale", "minima")
  )

  skillist <- build_skillist(result$skills, diffusion)

  expect_s3_class(skillist, "data.table")
  expect_equal(nrow(skillist), 3L)
  expect_true(all(is.na(skillist$pillar_softskills)))
  expect_true(all(is.na(skillist$esco_v0101_ict)))
  expect_true(all(skillist$tipo == "trasversale"))
})
