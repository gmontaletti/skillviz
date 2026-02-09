# Tests for source_stability.R -----

# 1. prepare_source_tsibble -----

test_that("prepare_source_tsibble returns tsibble", {
  skip_if_not_installed("tsibble")

  ann <- data.table::data.table(
    general_id = 1:12,
    source = rep(c("SourceA", "SourceB"), each = 6),
    gdate = data.table::as.IDate(
      rep(seq.Date(as.Date("2023-01-15"), by = "month", length.out = 6), 2)
    )
  )

  result <- prepare_source_tsibble(ann)

  expect_s3_class(result, "tbl_ts")
  expect_true("fonte" %in% names(result))
  expect_true("mese" %in% names(result))
  expect_true("N" %in% names(result))
})

test_that("prepare_source_tsibble parses dates from columns when gdate missing", {
  skip_if_not_installed("tsibble")

  ann <- data.table::data.table(
    general_id = 1:4,
    source = rep("SourceA", 4),
    year_grab_date = rep(2023L, 4),
    month_grab_date = c(1L, 2L, 3L, 4L),
    day_grab_date = rep(15L, 4)
  )

  result <- prepare_source_tsibble(ann)

  expect_s3_class(result, "tbl_ts")
  expect_equal(nrow(result), 4L)
})

test_that("prepare_source_tsibble errors on missing columns", {
  skip_if_not_installed("tsibble")

  bad_dt <- data.table::data.table(id = 1:3)

  expect_error(
    prepare_source_tsibble(bad_dt),
    "missing required columns"
  )
})

# 2. filter_stable_sources -----

test_that("filter_stable_sources returns character vector", {
  features <- data.frame(
    fonte = c("A", "B", "C"),
    cv = c(0.3, 0.8, 0.5),
    sum = c(100, 200, 50),
    stringsAsFactors = FALSE
  )

  result <- filter_stable_sources(features)

  expect_type(result, "character")
})

test_that("filter_stable_sources filters by cv_threshold", {
  features <- data.frame(
    fonte = c("A", "B", "C"),
    cv = c(0.3, 0.8, 0.5),
    sum = c(100, 200, 50),
    stringsAsFactors = FALSE
  )

  result <- filter_stable_sources(features, cv_threshold = 0.6)

  expect_true("A" %in% result)
  expect_true("C" %in% result)
  expect_false("B" %in% result)
})

test_that("filter_stable_sources filters by min_total", {
  features <- data.frame(
    fonte = c("A", "B", "C"),
    cv = c(0.3, 0.4, 0.5),
    sum = c(100, 200, 50),
    stringsAsFactors = FALSE
  )

  result <- filter_stable_sources(features, cv_threshold = 0.6, min_total = 80)

  expect_true("A" %in% result)
  expect_true("B" %in% result)
  expect_false("C" %in% result)
})

test_that("filter_stable_sources excludes rows with NA", {
  features <- data.frame(
    fonte = c("A", "B"),
    cv = c(0.3, NA),
    sum = c(100, 200),
    stringsAsFactors = FALSE
  )

  result <- filter_stable_sources(features, cv_threshold = 0.6)

  expect_equal(result, "A")
})

# 3. filter_annunci_by_source -----

test_that("filter_annunci_by_source keeps only stable sources", {
  ann <- data.table::data.table(
    general_id = 1:5,
    source = c("A", "B", "A", "C", "B")
  )

  result <- filter_annunci_by_source(ann, stable_sources = c("A", "C"))

  expect_s3_class(result, "data.table")
  expect_true(all(result$source %in% c("A", "C")))
  expect_equal(nrow(result), 3L)
})

test_that("filter_annunci_by_source errors on missing source column", {
  bad_dt <- data.table::data.table(id = 1:3)

  expect_error(
    filter_annunci_by_source(bad_dt, c("A")),
    "missing required columns"
  )
})
