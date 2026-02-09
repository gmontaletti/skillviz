# Tests for salary.R -----

# 1. extract_salary_data -----

test_that("extract_salary_data filters positive salaries", {
  ann <- data.table::data.table(
    general_id = 1:5,
    salaryvalue = c(30000, 0, -100, 50000, 0)
  )

  result <- extract_salary_data(ann)

  expect_s3_class(result, "data.table")
  expect_true(all(result$salaryvalue > 0))
  expect_equal(nrow(result), 2L)
  expect_equal(result$salaryvalue, c(30000, 50000))
})

test_that("extract_salary_data returns empty table when no salaries", {
  ann <- data.table::data.table(
    general_id = 1:3,
    salaryvalue = c(0, 0, 0)
  )

  result <- extract_salary_data(ann)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

test_that("extract_salary_data errors on missing salaryvalue column", {
  bad_dt <- data.table::data.table(general_id = 1:3)

  expect_error(
    extract_salary_data(bad_dt),
    "missing required columns"
  )
})

# 2. compute_salary_by_period -----

test_that("compute_salary_by_period aggregates correctly", {
  salary_data <- data.table::data.table(
    salary = c("low", "low", "high", "high"),
    mese = c("2023-01", "2023-01", "2023-01", "2023-02"),
    salaryvalue = c(1000, 2000, 5000, 6000)
  )

  result <- compute_salary_by_period(salary_data, by = c("salary", "mese"))

  expect_s3_class(result, "data.table")
  expect_true(all(c("N", "media", "mediana") %in% names(result)))

  # Check aggregation for low/2023-01
  low_jan <- result[salary == "low" & mese == "2023-01"]
  expect_equal(low_jan$N, 2L)
  expect_equal(low_jan$media, 1500)
  expect_equal(low_jan$mediana, 1500)
})

test_that("compute_salary_by_period derives mese from data column", {
  salary_data <- data.table::data.table(
    salary = c("low", "high"),
    data = as.Date(c("2023-01-15", "2023-02-20")),
    salaryvalue = c(1000, 5000)
  )

  result <- compute_salary_by_period(salary_data, by = c("salary", "mese"))

  expect_s3_class(result, "data.table")
  expect_true("mese" %in% names(result))
  expect_equal(sort(result$mese), c("2023-01", "2023-02"))
})

test_that("compute_salary_by_period errors on missing salaryvalue", {
  bad_dt <- data.table::data.table(salary = "low", mese = "2023-01")

  expect_error(
    compute_salary_by_period(bad_dt),
    "missing required columns"
  )
})

test_that("compute_salary_by_period errors on missing grouping columns", {
  salary_data <- data.table::data.table(
    salaryvalue = c(1000, 2000),
    mese = c("2023-01", "2023-02")
  )

  expect_error(
    compute_salary_by_period(salary_data, by = c("nonexistent", "mese")),
    "missing grouping columns"
  )
})

# 3. compute_salary_by_skill -----

test_that("compute_salary_by_skill returns expected structure", {
  salary_data <- data.table::data.table(
    general_id = c(1L, 2L, 3L),
    salaryvalue = c(30000, 40000, 50000)
  )

  skills <- data.table::data.table(
    general_id = c(1L, 1L, 2L, 3L),
    escoskill_level_3 = c("Python", "SQL", "Python", "SQL")
  )

  result <- compute_salary_by_skill(salary_data, skills)

  expect_s3_class(result, "data.table")
  expect_true(all(c("escoskill_level_3", "N", "mediana") %in% names(result)))
  expect_true(all(result$N > 0))
})

test_that("compute_salary_by_skill is sorted by descending median", {
  salary_data <- data.table::data.table(
    general_id = c(1L, 2L, 3L),
    salaryvalue = c(30000, 50000, 40000)
  )

  skills <- data.table::data.table(
    general_id = c(1L, 2L, 3L),
    escoskill_level_3 = c("Low", "High", "Mid")
  )

  result <- compute_salary_by_skill(salary_data, skills)

  # Should be ordered by descending mediana
  expect_true(all(diff(result$mediana) <= 0))
})
