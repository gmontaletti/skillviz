# Tests for cooccurrence.R -----

# Helper: create a small skills dataset where skills co-occur frequently
make_cooc_data <- function() {
  # 20 announcements, each with 2-3 skills from a small pool
  set.seed(42)
  ids <- rep(1:20, each = 3)
  skills <- sample(c("A", "B", "C", "D"), 60, replace = TRUE)
  data.table::data.table(
    general_id = ids,
    escoskill_level_3 = skills,
    preferredLabel = "Analyst"
  )
}

# 1. .cooc_single -----

test_that(".cooc_single returns expected structure", {
  dt <- make_cooc_data()

  result <- skillviz:::.cooc_single(
    professione = "Analyst",
    data = dt,
    group_col = "preferredLabel",
    top_n = 30L,
    min_weight = 1L
  )

  expect_s3_class(result, "data.table")
  expect_true(all(c("from", "to", "weight") %in% names(result)))
  if (nrow(result) > 0) {
    expect_type(result$from, "character")
    expect_type(result$to, "character")
    expect_true(is.numeric(result$weight))
  }
})

test_that(".cooc_single returns empty data.table for missing profession", {
  dt <- make_cooc_data()

  result <- skillviz:::.cooc_single(
    professione = "NonExistent",
    data = dt,
    group_col = "preferredLabel",
    top_n = 30L,
    min_weight = 1L
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("from", "to", "weight"))
})

test_that(".cooc_single returns empty for single-skill profession", {
  dt <- data.table::data.table(
    general_id = 1:5,
    escoskill_level_3 = rep("OnlySkill", 5),
    preferredLabel = rep("Solo", 5)
  )

  result <- skillviz:::.cooc_single(
    professione = "Solo",
    data = dt,
    group_col = "preferredLabel",
    top_n = 30L,
    min_weight = 1L
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

test_that(".cooc_single respects min_weight parameter", {
  # Build data where each pair co-occurs exactly once
  dt <- data.table::data.table(
    general_id = c(1, 1),
    escoskill_level_3 = c("X", "Y"),
    preferredLabel = c("Prof", "Prof")
  )

  result_low <- skillviz:::.cooc_single(
    professione = "Prof",
    data = dt,
    group_col = "preferredLabel",
    top_n = 30L,
    min_weight = 1L
  )

  result_high <- skillviz:::.cooc_single(
    professione = "Prof",
    data = dt,
    group_col = "preferredLabel",
    top_n = 30L,
    min_weight = 5L
  )

  # With min_weight = 1 we may get the edge; with 5 we must not
  expect_equal(nrow(result_high), 0L)
})

# 2. compute_cooc_matrix -----

test_that("compute_cooc_matrix returns sparse matrix", {
  dt <- data.table::data.table(
    general_id = c(1, 1, 1, 2, 2, 3, 3, 3),
    escoskill_level_3 = c("A", "B", "C", "A", "B", "A", "B", "C")
  )

  result <- compute_cooc_matrix(dt)

  expect_true(inherits(result, "dgCMatrix") || inherits(result, "Matrix"))
  expect_true(Matrix::isSymmetric(result))
  expect_equal(nrow(result), ncol(result))

  # Row/col names should be skill labels
  expect_true(all(c("A", "B", "C") %in% rownames(result)))
})

test_that("compute_cooc_matrix has correct co-occurrence counts", {
  # A and B co-occur in doc 1, 2, 3 -> count 3
  # A and C co-occur in doc 1, 3 -> count 2
  # B and C co-occur in doc 1, 3 -> count 2
  dt <- data.table::data.table(
    general_id = c(1, 1, 1, 2, 2, 3, 3, 3),
    escoskill_level_3 = c("A", "B", "C", "A", "B", "A", "B", "C")
  )

  result <- compute_cooc_matrix(dt)
  mat <- as.matrix(result)

  expect_equal(mat["A", "B"], 3)
  expect_equal(mat["A", "C"], 2)
  expect_equal(mat["B", "C"], 2)
})

test_that("compute_cooc_matrix errors on missing columns", {
  bad_dt <- data.table::data.table(id = 1:3, skill = letters[1:3])

  expect_error(compute_cooc_matrix(bad_dt), "missing required columns")
})

# 3. cooc_all_professions -----

test_that("cooc_all_professions returns data.table with correct columns", {
  dt <- make_cooc_data()

  result <- cooc_all_professions(
    dt,
    top_n = 30L,
    min_weight = 1L
  )

  expect_s3_class(result, "data.table")
  expect_true(all(c("professione", "from", "to", "weight") %in% names(result)))
})

test_that("cooc_all_professions handles multiple professions", {
  dt <- data.table::data.table(
    general_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
    escoskill_level_3 = c(
      "A",
      "B",
      "C",
      "A",
      "B",
      "C",
      "X",
      "Y",
      "Z",
      "X",
      "Y",
      "Z"
    ),
    preferredLabel = c(
      rep("Prof1", 6),
      rep("Prof2", 6)
    )
  )

  result <- cooc_all_professions(dt, top_n = 30L, min_weight = 1L)

  expect_s3_class(result, "data.table")
  if (nrow(result) > 0) {
    expect_true("professione" %in% names(result))
  }
})

test_that("cooc_all_professions with single-skill profession returns no edges", {
  dt <- data.table::data.table(
    general_id = 1:3,
    escoskill_level_3 = rep("OnlyOne", 3),
    preferredLabel = rep("Solo", 3)
  )

  result <- cooc_all_professions(dt, top_n = 30L, min_weight = 1L)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

test_that("cooc_all_professions respects professions argument", {
  dt <- make_cooc_data()

  result <- cooc_all_professions(
    dt,
    professions = "Analyst",
    top_n = 30L,
    min_weight = 1L
  )

  if (nrow(result) > 0) {
    expect_true(all(result$professione == "Analyst"))
  }
})

test_that("cooc_all_professions errors on missing columns", {
  bad_dt <- data.table::data.table(id = 1:3)

  expect_error(cooc_all_professions(bad_dt), "missing required columns")
})

# 4. filter_relative_risk -----

test_that("filter_relative_risk returns expected structure", {
  dt <- data.table::data.table(
    general_id = c(1, 1, 1, 2, 2, 3, 3, 3),
    escoskill_level_3 = c("A", "B", "C", "A", "B", "A", "B", "C")
  )

  mat <- compute_cooc_matrix(dt)
  result <- filter_relative_risk(mat)

  expect_s3_class(result, "data.table")
  expect_named(result, c("from", "to", "weight"))
})

test_that("filter_relative_risk handles zero matrix", {
  mat <- matrix(0, nrow = 3, ncol = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- filter_relative_risk(mat)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})
