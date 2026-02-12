# Tests for normalize_ojv -----

# Helper: create a ZIP fixture in a temp directory -----
create_zip_fixture <- function(dir, dt, year, month, type = "postings") {
  fname <- paste0("ITC4_", year, "_", month, "_", type, ".csv")
  csv_path <- file.path(dir, fname)
  data.table::fwrite(dt, csv_path)
  zip_name <- sub("\\.csv$", ".zip", fname)
  zip_path <- file.path(dir, zip_name)
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zip_path, fname, flags = "-q")
  unlink(csv_path)
  zip_path
}

# Fixture data generators -----
make_postings_norm <- function(ids, year_grab = 2024L, month_grab = 1L) {
  data.table::data.table(
    general_id = as.integer(ids),
    year_grab_date = year_grab,
    month_grab_date = month_grab,
    city = paste0("city_", ids),
    source = paste0("src_", ids)
  )
}

make_skills_norm <- function(ids) {
  data.table::data.table(
    general_id = as.integer(ids),
    skill_label = paste0("skill_", ids),
    skill_level = rep("A", length(ids))
  )
}

make_companies_norm <- function(ids) {
  data.table::data.table(
    general_id = as.integer(ids),
    companyname = paste0("company_", ids)
  )
}

# Helper to set up a full fixture directory with all three types -----
setup_full_fixtures <- function(
  dir,
  postings_dt,
  skills_dt,
  companies_dt,
  year = 2024,
  month = 1
) {
  create_zip_fixture(dir, postings_dt, year, month, "postings")
  create_zip_fixture(dir, skills_dt, year, month, "skills")
  create_zip_fixture(dir, companies_dt, year, month, "postings_raw")
}

# 1. return structure -----

test_that("returns a named list with three data.tables", {
  tmp <- withr::local_tempdir()
  setup_full_fixtures(
    tmp,
    make_postings_norm(1:3),
    make_skills_norm(1:3),
    make_companies_norm(1:3)
  )

  result <- normalize_ojv(tmp, verbose = FALSE)

  expect_type(result, "list")
  expect_named(result, c("postings", "skills", "companies"))
  expect_s3_class(result$postings, "data.table")
  expect_s3_class(result$skills, "data.table")
  expect_s3_class(result$companies, "data.table")
})

# 2. deduplication -----

test_that("postings has unique general_id after deduplication", {
  tmp <- withr::local_tempdir()

  # Same IDs across two months = duplicates
  p1 <- make_postings_norm(1:3, year_grab = 2024L, month_grab = 1L)
  p2 <- make_postings_norm(1:3, year_grab = 2024L, month_grab = 6L)
  create_zip_fixture(tmp, p1, year = 2024, month = 1, type = "postings")
  create_zip_fixture(tmp, p2, year = 2024, month = 6, type = "postings")

  # Skills and companies for both months
  sk <- make_skills_norm(1:3)
  co <- make_companies_norm(1:3)
  create_zip_fixture(tmp, sk, year = 2024, month = 1, type = "skills")
  create_zip_fixture(tmp, sk, year = 2024, month = 6, type = "skills")
  create_zip_fixture(tmp, co, year = 2024, month = 1, type = "postings_raw")
  create_zip_fixture(tmp, co, year = 2024, month = 6, type = "postings_raw")

  result <- normalize_ojv(tmp, verbose = FALSE)

  expect_equal(nrow(result$postings), 3L)
  expect_equal(length(unique(result$postings$general_id)), 3L)
})

test_that("deduplication keeps most recent grab date", {
  tmp <- withr::local_tempdir()

  # ID 1 appears in two months with different cities
  p_old <- data.table::data.table(
    general_id = 1L,
    year_grab_date = 2024L,
    month_grab_date = 1L,
    city = "old_city"
  )
  p_new <- data.table::data.table(
    general_id = 1L,
    year_grab_date = 2024L,
    month_grab_date = 6L,
    city = "new_city"
  )
  create_zip_fixture(tmp, p_old, year = 2024, month = 1, type = "postings")
  create_zip_fixture(tmp, p_new, year = 2024, month = 6, type = "postings")

  sk <- make_skills_norm(1L)
  co <- make_companies_norm(1L)
  create_zip_fixture(tmp, sk, year = 2024, month = 1, type = "skills")
  create_zip_fixture(tmp, sk, year = 2024, month = 6, type = "skills")
  create_zip_fixture(tmp, co, year = 2024, month = 1, type = "postings_raw")
  create_zip_fixture(tmp, co, year = 2024, month = 6, type = "postings_raw")

  result <- normalize_ojv(tmp, verbose = FALSE)

  expect_equal(nrow(result$postings), 1L)
  expect_equal(result$postings$city, "new_city")
})

# 3. referential integrity -----

test_that("skills only contains general_id values present in postings", {
  tmp <- withr::local_tempdir()

  # Postings: IDs 1-3, skills: IDs 1-5 (extra IDs 4,5 should be dropped)
  p <- make_postings_norm(1:3)
  sk <- make_skills_norm(1:5)
  co <- make_companies_norm(1:3)
  setup_full_fixtures(tmp, p, sk, co)

  result <- normalize_ojv(tmp, verbose = FALSE)

  expect_true(all(result$skills$general_id %in% result$postings$general_id))
  expect_equal(nrow(result$skills), 3L)
})

test_that("companies only contains general_id values present in postings", {
  tmp <- withr::local_tempdir()

  p <- make_postings_norm(1:3)
  sk <- make_skills_norm(1:3)
  co <- make_companies_norm(1:5) # extra IDs 4,5
  setup_full_fixtures(tmp, p, sk, co)

  result <- normalize_ojv(tmp, verbose = FALSE)

  expect_true(all(result$companies$general_id %in% result$postings$general_id))
  expect_equal(nrow(result$companies), 3L)
})

# 4. keys -----

test_that("all tables are keyed on general_id", {
  tmp <- withr::local_tempdir()
  setup_full_fixtures(
    tmp,
    make_postings_norm(1:3),
    make_skills_norm(1:3),
    make_companies_norm(1:3)
  )

  result <- normalize_ojv(tmp, verbose = FALSE)

  expect_equal(data.table::key(result$postings), "general_id")
  expect_equal(data.table::key(result$skills), "general_id")
  expect_equal(data.table::key(result$companies), "general_id")
})

# 5. empty directory -----

test_that("empty directory returns list of three empty data.tables", {
  tmp <- withr::local_tempdir()

  result <- suppressMessages(normalize_ojv(tmp, verbose = TRUE))

  expect_type(result, "list")
  expect_named(result, c("postings", "skills", "companies"))
  expect_equal(nrow(result$postings), 0L)
  expect_equal(nrow(result$skills), 0L)
  expect_equal(nrow(result$companies), 0L)
})

# 6. year/month filtering -----

test_that("years/months filtering propagates to all three types", {
  tmp <- withr::local_tempdir()

  # 2024 data
  setup_full_fixtures(
    tmp,
    make_postings_norm(1:3, year_grab = 2024L, month_grab = 1L),
    make_skills_norm(1:3),
    make_companies_norm(1:3),
    year = 2024,
    month = 1
  )
  # 2025 data
  setup_full_fixtures(
    tmp,
    make_postings_norm(4:6, year_grab = 2025L, month_grab = 1L),
    make_skills_norm(4:6),
    make_companies_norm(4:6),
    year = 2025,
    month = 1
  )

  result <- normalize_ojv(tmp, years = 2024L, verbose = FALSE)

  expect_equal(sort(result$postings$general_id), 1:3)
  expect_true(all(result$skills$general_id %in% 1:3))
  expect_true(all(result$companies$general_id %in% 1:3))
})

# 7. verbose messages -----

test_that("verbose = TRUE produces deduplication message", {
  tmp <- withr::local_tempdir()

  p1 <- make_postings_norm(1:3, year_grab = 2024L, month_grab = 1L)
  p2 <- make_postings_norm(1:3, year_grab = 2024L, month_grab = 6L)
  create_zip_fixture(tmp, p1, year = 2024, month = 1, type = "postings")
  create_zip_fixture(tmp, p2, year = 2024, month = 6, type = "postings")
  create_zip_fixture(
    tmp,
    make_skills_norm(1:3),
    year = 2024,
    month = 1,
    type = "skills"
  )
  create_zip_fixture(
    tmp,
    make_companies_norm(1:3),
    year = 2024,
    month = 1,
    type = "postings_raw"
  )

  expect_message(
    normalize_ojv(tmp, verbose = TRUE),
    "deduplicated postings"
  )
})

test_that("verbose = FALSE suppresses deduplication message", {
  tmp <- withr::local_tempdir()
  setup_full_fixtures(
    tmp,
    make_postings_norm(1:3),
    make_skills_norm(1:3),
    make_companies_norm(1:3)
  )

  expect_no_message(
    normalize_ojv(tmp, verbose = FALSE)
  )
})
