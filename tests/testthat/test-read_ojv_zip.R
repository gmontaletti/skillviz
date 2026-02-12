# Tests for read_ojv_zip -----

# Helper: create a ZIP fixture in a temp directory -----
# Writes a data.table as CSV, zips it, and returns the zip path.
create_zip_fixture <- function(dir, dt, year, month, type = "postings") {
  fname <- paste0("ITC4_", year, "_", month, "_", type, ".csv")
  csv_path <- file.path(dir, fname)
  data.table::fwrite(dt, csv_path)
  zip_name <- sub("\\.csv$", ".zip", fname)
  zip_path <- file.path(dir, zip_name)
  # zip() needs relative paths so the archive contains only the filename
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)

  utils::zip(zip_path, fname, flags = "-q")
  unlink(csv_path)
  zip_path
}

# Fixture data generators -----
make_postings <- function(n, id_start = 1L) {
  data.table::data.table(
    general_id = seq.int(id_start, length.out = n),
    year_grab_date = 2024L,
    month_grab_date = 1L,
    city = paste0("city_", seq_len(n)),
    source = paste0("src_", seq_len(n))
  )
}

make_postings_raw <- function(n, id_start = 1L) {
  data.table::data.table(
    general_id = seq.int(id_start, length.out = n),
    companyname = paste0("company_", seq_len(n))
  )
}

# 1. input validation errors -----

test_that("path must be a single character string", {
  expect_error(
    read_ojv_zip(123),
    "must be a single character string"
  )
  expect_error(
    read_ojv_zip(c("/a", "/b")),
    "must be a single character string"
  )
  expect_error(
    read_ojv_zip(NULL),
    "must be a single character string"
  )
})

test_that("path directory must exist", {
  expect_error(
    read_ojv_zip("/nonexistent/path/nowhere"),
    "directory not found"
  )
})

test_that("months must be integers between 1 and 12", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_ojv_zip(tmp, months = 0L),
    "must be integers between 1 and 12"
  )
  expect_error(
    read_ojv_zip(tmp, months = 13L),
    "must be integers between 1 and 12"
  )
  expect_error(
    read_ojv_zip(tmp, months = c(1L, NA_integer_)),
    "must be integers between 1 and 12"
  )
})

test_that("years with NA values trigger an error", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_ojv_zip(tmp, years = c(2024, NA)),
    "must be coercible to integer"
  )
  expect_error(
    suppressWarnings(read_ojv_zip(tmp, years = "abc")),
    "must be coercible to integer"
  )
})

test_that("select must be character or NULL", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_ojv_zip(tmp, select = 1:3),
    "must be a character vector or NULL"
  )
  expect_error(
    read_ojv_zip(tmp, select = TRUE),
    "must be a character vector or NULL"
  )
})

test_that("invalid type triggers match.arg error", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_ojv_zip(tmp, type = "invalid_type"),
    "should be one of"
  )
})

# 2. empty and no-match scenarios -----

test_that("empty directory returns empty data.table with message", {
  tmp <- withr::local_tempdir()
  expect_message(
    result <- read_ojv_zip(tmp, verbose = TRUE),
    "no ZIP files found"
  )
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

test_that("no files match year/month filter returns empty data.table with message", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  expect_message(
    result <- read_ojv_zip(tmp, years = 2025L, verbose = TRUE),
    "no ZIP files match the requested years/months"
  )
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

test_that("no files match month filter returns empty data.table", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  expect_message(
    result <- read_ojv_zip(tmp, months = 6L, verbose = TRUE),
    "no ZIP files match the requested years/months"
  )
  expect_equal(nrow(result), 0L)
})

test_that("type mismatch returns empty data.table", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(
    tmp,
    make_postings(3),
    year = 2024,
    month = 1,
    type = "postings"
  )

  expect_message(
    result <- read_ojv_zip(tmp, type = "skills", verbose = TRUE),
    "no ZIP files found"
  )
  expect_equal(nrow(result), 0L)
})

# 3. successful single-file reads -----

test_that("reads a single ZIP correctly", {
  tmp <- withr::local_tempdir()
  dt_in <- make_postings(5)
  create_zip_fixture(tmp, dt_in, year = 2024, month = 1)

  result <- read_ojv_zip(tmp, type = "postings", verbose = FALSE)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5L)
  expect_true(all(c("general_id", "city", "source") %in% names(result)))
})

test_that("reads postings_raw type correctly", {
  tmp <- withr::local_tempdir()
  dt_in <- make_postings_raw(5)
  create_zip_fixture(tmp, dt_in, year = 2024, month = 1, type = "postings_raw")

  result <- read_ojv_zip(tmp, type = "postings_raw", verbose = FALSE)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5L)
  expect_true(all(c("general_id", "companyname") %in% names(result)))
})

# 4. nrows parameter -----

test_that("nrows limits rows per file", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(10), year = 2024, month = 1)
  create_zip_fixture(
    tmp,
    make_postings(10, id_start = 11L),
    year = 2024,
    month = 2
  )

  result <- read_ojv_zip(tmp, nrows = 3, verbose = FALSE)

  # 3 rows per file x 2 files = 6 total

  expect_equal(nrow(result), 6L)
})

test_that("nrows = Inf reads all rows", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(7), year = 2024, month = 1)

  result <- read_ojv_zip(tmp, nrows = Inf, verbose = FALSE)

  expect_equal(nrow(result), 7L)
})

# 5. select parameter -----

test_that("select limits columns returned", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(5), year = 2024, month = 1)

  result <- read_ojv_zip(
    tmp,
    select = c("general_id", "city"),
    verbose = FALSE
  )

  expect_equal(ncol(result), 2L)
  expect_named(result, c("general_id", "city"))
})

test_that("select = NULL reads all columns", {
  tmp <- withr::local_tempdir()
  dt_in <- make_postings(5)
  create_zip_fixture(tmp, dt_in, year = 2024, month = 1)

  result <- read_ojv_zip(tmp, select = NULL, verbose = FALSE)

  expect_equal(ncol(result), ncol(dt_in))
})

# 6. multiple file rbindlist -----

test_that("multiple files are bound together correctly", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(
    tmp,
    make_postings(5, id_start = 1L),
    year = 2024,
    month = 1
  )
  create_zip_fixture(
    tmp,
    make_postings(3, id_start = 6L),
    year = 2024,
    month = 2
  )
  create_zip_fixture(
    tmp,
    make_postings(4, id_start = 9L),
    year = 2024,
    month = 3
  )

  result <- read_ojv_zip(tmp, verbose = FALSE)

  expect_equal(nrow(result), 12L)
  expect_equal(sort(result$general_id), 1:12)
})

test_that("files are returned in chronological order by year then month", {
  tmp <- withr::local_tempdir()

  # Create files deliberately out of order in the filesystem
  dt_late <- data.table::data.table(general_id = 3L, label = "2025-02")
  dt_early <- data.table::data.table(general_id = 1L, label = "2024-06")
  dt_mid <- data.table::data.table(general_id = 2L, label = "2024-12")

  create_zip_fixture(tmp, dt_late, year = 2025, month = 2)
  create_zip_fixture(tmp, dt_early, year = 2024, month = 6)
  create_zip_fixture(tmp, dt_mid, year = 2024, month = 12)

  result <- read_ojv_zip(tmp, verbose = FALSE)

  # Should be sorted: 2024-06, 2024-12, 2025-02
  expect_equal(result$label, c("2024-06", "2024-12", "2025-02"))
})

test_that("rbindlist with fill = TRUE handles different column sets", {
  tmp <- withr::local_tempdir()

  dt1 <- data.table::data.table(general_id = 1L, city = "Rome")
  dt2 <- data.table::data.table(
    general_id = 2L,
    city = "Milan",
    extra_col = "x"
  )

  create_zip_fixture(tmp, dt1, year = 2024, month = 1)
  create_zip_fixture(tmp, dt2, year = 2024, month = 2)

  result <- read_ojv_zip(tmp, verbose = FALSE)

  expect_equal(nrow(result), 2L)
  expect_true("extra_col" %in% names(result))
  # First file had no extra_col, so it should be NA

  expect_true(is.na(result[general_id == 1L, extra_col]))
  expect_equal(result[general_id == 2L, extra_col], "x")
})

# 7. year/month filtering -----

test_that("years filter selects only matching years", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(
    tmp,
    make_postings(2, id_start = 1L),
    year = 2023,
    month = 6
  )
  create_zip_fixture(
    tmp,
    make_postings(3, id_start = 3L),
    year = 2024,
    month = 6
  )
  create_zip_fixture(
    tmp,
    make_postings(4, id_start = 6L),
    year = 2025,
    month = 6
  )

  result <- read_ojv_zip(tmp, years = 2024L, verbose = FALSE)

  expect_equal(nrow(result), 3L)
  expect_equal(result$general_id, 3:5)
})

test_that("months filter selects only matching months", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(
    tmp,
    make_postings(2, id_start = 1L),
    year = 2024,
    month = 1
  )
  create_zip_fixture(
    tmp,
    make_postings(3, id_start = 3L),
    year = 2024,
    month = 6
  )
  create_zip_fixture(
    tmp,
    make_postings(4, id_start = 6L),
    year = 2024,
    month = 12
  )

  result <- read_ojv_zip(tmp, months = c(1L, 12L), verbose = FALSE)

  expect_equal(nrow(result), 6L)
})

test_that("years and months filters combine correctly", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 1L),
    year = 2023,
    month = 1
  )
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 2L),
    year = 2023,
    month = 6
  )
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 3L),
    year = 2024,
    month = 1
  )
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 4L),
    year = 2024,
    month = 6
  )

  result <- read_ojv_zip(tmp, years = 2024L, months = 6L, verbose = FALSE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$general_id, 4L)
})

test_that("years as numeric are coerced to integer", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  # Pass years as double -- should work via as.integer()
  result <- read_ojv_zip(tmp, years = 2024, verbose = FALSE)

  expect_equal(nrow(result), 3L)
})

# 8. verbose control -----

test_that("verbose = TRUE produces progress messages", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  expect_message(
    read_ojv_zip(tmp, verbose = TRUE),
    "reading 1 file"
  )
})

test_that("verbose = TRUE prints per-file progress", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  expect_message(
    read_ojv_zip(tmp, verbose = TRUE),
    "ITC4_2024_1_postings\\.zip"
  )
})

test_that("verbose = TRUE prints summary with row count", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  expect_message(
    read_ojv_zip(tmp, verbose = TRUE),
    "done\\."
  )
})

test_that("verbose = FALSE suppresses all messages", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  expect_no_message(
    read_ojv_zip(tmp, verbose = FALSE)
  )
})

# 9. edge cases -----

test_that("non-matching filenames in directory are ignored", {
  tmp <- withr::local_tempdir()
  # Create a valid fixture
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)
  # Create decoy files that should not match the pattern
  file.create(file.path(tmp, "random_file.zip"))
  file.create(file.path(tmp, "ITC4_postings.zip"))
  file.create(file.path(tmp, "ITC4_2024_postings.zip"))
  file.create(file.path(tmp, "data.csv"))

  result <- read_ojv_zip(tmp, verbose = FALSE)

  # Only the valid fixture should be read
  expect_equal(nrow(result), 3L)
})

test_that("multiple years filter works with vector input", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 1L),
    year = 2022,
    month = 1
  )
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 2L),
    year = 2023,
    month = 1
  )
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 3L),
    year = 2024,
    month = 1
  )
  create_zip_fixture(
    tmp,
    make_postings(1, id_start = 4L),
    year = 2025,
    month = 1
  )

  result <- read_ojv_zip(tmp, years = c(2022L, 2024L), verbose = FALSE)

  expect_equal(nrow(result), 2L)
  expect_equal(sort(result$general_id), c(1L, 3L))
})

test_that("result is a data.table, not a plain data.frame", {
  tmp <- withr::local_tempdir()
  create_zip_fixture(tmp, make_postings(3), year = 2024, month = 1)

  result <- read_ojv_zip(tmp, verbose = FALSE)

  expect_true(data.table::is.data.table(result))
})
