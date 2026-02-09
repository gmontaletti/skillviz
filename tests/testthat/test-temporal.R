# Tests for temporal.R -----

# 1. compute_skill_ranking_series -----

test_that("compute_skill_ranking_series returns expected structure", {
  skip_if_not_installed("lubridate")

  competenze <- data.table::data.table(
    escoskill_level_3 = rep(c("A", "B", "C"), each = 6),
    nome_3 = rep("Prof1", 18),
    gdate = data.table::as.IDate(rep(
      seq.Date(as.Date("2022-01-01"), by = "month", length.out = 6),
      3
    ))
  )

  result <- compute_skill_ranking_series(competenze, time_unit = "month")

  expect_s3_class(result, "data.table")
  expect_true(all(
    c("professione", "mese", "skill", "N", "rango") %in% names(result)
  ))
  expect_type(result$rango, "integer")
})

test_that("compute_skill_ranking_series ranks correctly", {
  skip_if_not_installed("lubridate")

  # A appears 3 times, B appears 2 times, C appears 1 time in same month
  competenze <- data.table::data.table(
    escoskill_level_3 = c("A", "A", "A", "B", "B", "C"),
    nome_3 = rep("Prof1", 6),
    gdate = data.table::as.IDate(rep("2023-01-15", 6))
  )

  result <- compute_skill_ranking_series(competenze, time_unit = "month")

  expect_equal(result[skill == "A", rango], 1L)
  expect_equal(result[skill == "B", rango], 2L)
  expect_equal(result[skill == "C", rango], 3L)
})

test_that("compute_skill_ranking_series respects cutoff_date", {
  skip_if_not_installed("lubridate")

  competenze <- data.table::data.table(
    escoskill_level_3 = c("A", "A", "B", "B"),
    nome_3 = rep("Prof1", 4),
    gdate = data.table::as.IDate(
      c("2023-01-15", "2023-06-15", "2023-01-15", "2023-06-15")
    )
  )

  result <- compute_skill_ranking_series(
    competenze,
    time_unit = "month",
    cutoff_date = "2023-03-01"
  )

  # Only January data should remain
  expect_true(all(result$mese < as.Date("2023-03-01")))
})

test_that("compute_skill_ranking_series supports year aggregation", {
  skip_if_not_installed("lubridate")

  competenze <- data.table::data.table(
    escoskill_level_3 = c("A", "A", "B"),
    nome_3 = rep("Prof1", 3),
    gdate = data.table::as.IDate(
      c("2022-03-15", "2023-06-15", "2023-09-15")
    )
  )

  result <- compute_skill_ranking_series(competenze, time_unit = "year")

  # Should produce at most 2 periods (2022, 2023)
  expect_true(length(unique(result$mese)) <= 2L)
})

test_that("compute_skill_ranking_series errors on missing columns", {
  skip_if_not_installed("lubridate")

  bad_dt <- data.table::data.table(skill = "A", date = Sys.Date())

  expect_error(
    compute_skill_ranking_series(bad_dt),
    "missing required columns"
  )
})

# 2. compute_skill_variation -----

test_that("compute_skill_variation returns expected columns", {
  serie <- data.table::data.table(
    professione = rep("Prof1", 8),
    mese = as.Date(c(
      rep("2022-01-01", 4),
      rep("2023-01-01", 4)
    )),
    skill = rep(c("A", "B", "C", "D"), 2),
    N = c(100L, 80L, 60L, 40L, 90L, 85L, 50L, 45L),
    rango = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L)
  )

  result <- compute_skill_variation(serie, min_n = 1L, top_k = 10L)

  expect_s3_class(result, "data.table")
  expect_true("professione" %in% names(result))
  expect_true("skill" %in% names(result))
  expect_true("variazione" %in% names(result))
})

test_that("compute_skill_variation computes correct rank change", {
  serie <- data.table::data.table(
    professione = rep("Prof1", 6),
    mese = as.Date(c(
      rep("2022-01-01", 3),
      rep("2023-01-01", 3)
    )),
    skill = rep(c("A", "B", "C"), 2),
    N = c(100L, 80L, 60L, 90L, 95L, 50L),
    rango = c(1L, 2L, 3L, 2L, 1L, 3L)
  )

  result <- compute_skill_variation(serie, min_n = 1L, top_k = 10L)

  # Skill B: was rank 2, now rank 1 -> variazione = 2 - 1 = 1 (improved)
  b_var <- result[skill == "B", variazione]
  expect_equal(b_var, 1)

  # Skill A: was rank 1, now rank 2 -> variazione = 1 - 2 = -1 (declined)
  a_var <- result[skill == "A", variazione]
  expect_equal(a_var, -1)
})

test_that("compute_skill_variation respects min_n filter", {
  serie <- data.table::data.table(
    professione = rep("Prof1", 6),
    mese = as.Date(c(
      rep("2022-01-01", 3),
      rep("2023-01-01", 3)
    )),
    skill = rep(c("A", "B", "C"), 2),
    N = c(100L, 80L, 5L, 90L, 85L, 3L),
    rango = c(1L, 2L, 3L, 1L, 2L, 3L)
  )

  result <- compute_skill_variation(serie, min_n = 30L, top_k = 10L)

  # Skill C has N < 30 in current period, should be filtered out
  expect_false("C" %in% result$skill)
})

test_that("compute_skill_variation respects top_k", {
  serie <- data.table::data.table(
    professione = rep("Prof1", 10),
    mese = as.Date(c(rep("2022-01-01", 5), rep("2023-01-01", 5))),
    skill = rep(paste0("S", 1:5), 2),
    N = c(100L, 80L, 60L, 40L, 20L, 95L, 85L, 55L, 45L, 25L),
    rango = c(1:5, 1:5)
  )

  result <- compute_skill_variation(serie, min_n = 1L, top_k = 3L)

  expect_true(nrow(result) <= 3L)
})

test_that("compute_skill_variation errors with fewer than 2 periods", {
  serie <- data.table::data.table(
    professione = "Prof1",
    mese = as.Date("2023-01-01"),
    skill = "A",
    N = 100L,
    rango = 1L
  )

  expect_error(
    compute_skill_variation(serie),
    "need at least 2 time periods"
  )
})

test_that("compute_skill_variation errors on missing columns", {
  bad_dt <- data.table::data.table(prof = "A", date = Sys.Date())

  expect_error(
    compute_skill_variation(bad_dt),
    "missing required columns"
  )
})
