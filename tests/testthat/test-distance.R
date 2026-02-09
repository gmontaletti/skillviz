# Tests for distance.R -----

# 1. compute_profession_distance -----

test_that("compute_profession_distance returns dist object", {
  competenze <- data.table::data.table(
    cod_3 = c(
      rep("1.1.1", 4),
      rep("2.2.2", 3),
      rep("3.3.3", 2)
    ),
    escoskill_level_3 = c(
      "A",
      "B",
      "C",
      "D",
      "A",
      "B",
      "E",
      "C",
      "D"
    )
  )

  result <- compute_profession_distance(competenze)

  expect_s3_class(result, "dist")
  expect_equal(attr(result, "Size"), 3L)
})

test_that("compute_profession_distance uses specified method", {
  competenze <- data.table::data.table(
    cod_3 = c(rep("P1", 3), rep("P2", 3)),
    escoskill_level_3 = c("A", "B", "C", "A", "D", "E")
  )

  result_binary <- compute_profession_distance(competenze, method = "binary")
  result_euclid <- compute_profession_distance(competenze, method = "euclidean")

  expect_s3_class(result_binary, "dist")
  expect_s3_class(result_euclid, "dist")
  expect_equal(attr(result_binary, "method"), "binary")
  expect_equal(attr(result_euclid, "method"), "euclidean")
})

test_that("compute_profession_distance filter_rca can be disabled", {
  competenze <- data.table::data.table(
    cod_3 = c(rep("P1", 3), rep("P2", 3)),
    escoskill_level_3 = c("A", "B", "C", "A", "B", "D")
  )

  result_rca <- compute_profession_distance(competenze, filter_rca = TRUE)
  result_no_rca <- compute_profession_distance(competenze, filter_rca = FALSE)

  expect_s3_class(result_rca, "dist")
  expect_s3_class(result_no_rca, "dist")

  # Results may differ when RCA filtering is toggled
  # Both should be valid dist objects
  expect_equal(attr(result_rca, "Size"), 2L)
  expect_equal(attr(result_no_rca, "Size"), 2L)
})

test_that("compute_profession_distance errors on missing columns", {
  bad_dt <- data.table::data.table(profession = "A", skill = "X")

  expect_error(
    compute_profession_distance(bad_dt),
    "missing required columns"
  )
})

# 2. cluster_professions -----

test_that("cluster_professions returns hclust object", {
  competenze <- data.table::data.table(
    cod_3 = c(rep("P1", 3), rep("P2", 3), rep("P3", 3)),
    escoskill_level_3 = c("A", "B", "C", "A", "D", "E", "B", "D", "F")
  )

  d <- compute_profession_distance(competenze)
  result <- cluster_professions(d)

  expect_s3_class(result, "hclust")
  expect_equal(result$method, "complete")
})

test_that("cluster_professions respects method argument", {
  competenze <- data.table::data.table(
    cod_3 = c(rep("P1", 3), rep("P2", 3), rep("P3", 3)),
    escoskill_level_3 = c("A", "B", "C", "A", "D", "E", "B", "D", "F")
  )

  d <- compute_profession_distance(competenze)
  result <- cluster_professions(d, method = "ward.D2")

  expect_s3_class(result, "hclust")
  expect_equal(result$method, "ward.D2")
})

test_that("cluster_professions errors on non-dist input", {
  expect_error(
    cluster_professions(matrix(1:4, 2, 2)),
    "must be a dist object"
  )

  expect_error(
    cluster_professions("not a dist"),
    "must be a dist object"
  )
})
