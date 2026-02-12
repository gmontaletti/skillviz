# Tests for classify_esco_to_cpi -----

# Shared fixture -----

make_test_data <- function() {
  # E001 (mapped -> 2.1.1): postings 1-2, skills S01, S02, S05
  # E002 (mapped -> 3.1.2): postings 3-4, skills S03, S04, S05
  # E003 (unmapped):         postings 5-6, skills S01, S02
  # S05 is a shared skill between both classes (for alpha sensitivity).
  # E003 has only S01+S02 which are exclusive to class 2.1.1.
  postings <- data.table::data.table(
    general_id = 1:6,
    idesco_level_4 = c("E001", "E001", "E002", "E002", "E003", "E003"),
    cp2021_id_level_3 = c("2.1.1", "2.1.1", "3.1.2", "3.1.2", NA, NA),
    cp2021_level_3 = c(
      "Informatici",
      "Informatici",
      "Ingegneri",
      "Ingegneri",
      NA,
      NA
    )
  )
  skills <- data.table::data.table(
    general_id = c(
      1L,
      1L,
      1L,
      2L,
      2L,
      3L,
      3L,
      3L,
      4L,
      4L,
      5L,
      5L,
      6L,
      6L
    ),
    escoskill_level_3 = c(
      "S01",
      "S02",
      "S05",
      "S01",
      "S02",
      "S03",
      "S04",
      "S05",
      "S03",
      "S04",
      "S01",
      "S02",
      "S01",
      "S02"
    )
  )
  list(postings = postings, skills = skills)
}

# 1. Return structure -----

test_that("classify_esco_to_cpi returns expected structure and key", {
  td <- make_test_data()
  result <- classify_esco_to_cpi(
    td$postings,
    td$skills,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expected_cols <- c(
    "idesco_level_4",
    "cod_3",
    "nome_3",
    "probability",
    "rank",
    "n_postings",
    "n_skills"
  )
  expect_true(all(expected_cols %in% names(result)))
  expect_equal(data.table::key(result), "idesco_level_4")
})

# 2. Probabilities sum to 1 -----

test_that("probabilities sum to 1 within each ESCO L4 code", {
  td <- make_test_data()
  result <- classify_esco_to_cpi(
    td$postings,
    td$skills,
    verbose = FALSE
  )

  prob_sums <- result[, .(total = sum(probability)), by = idesco_level_4]
  for (i in seq_len(nrow(prob_sums))) {
    expect_equal(prob_sums$total[i], 1.0, tolerance = 1e-6)
  }
})

# 3. Top-k respected -----

test_that("top_k = 1 returns at most 1 row per ESCO L4", {
  td <- make_test_data()
  result <- classify_esco_to_cpi(
    td$postings,
    td$skills,
    top_k = 1L,
    verbose = FALSE
  )

  rows_per_code <- result[, .N, by = idesco_level_4]
  expect_true(all(rows_per_code$N <= 1L))
})

# 4. Rank ordering -----

test_that("rank 1 has highest probability and probabilities are non-increasing", {
  td <- make_test_data()
  result <- classify_esco_to_cpi(
    td$postings,
    td$skills,
    verbose = FALSE
  )

  result_ordered <- result[order(idesco_level_4, rank)]
  by_code <- split(result_ordered, by = "idesco_level_4")

  for (grp in by_code) {
    probs <- grp$probability
    ranks <- grp$rank
    # probability should be non-increasing with rank
    expect_true(
      all(diff(probs) <= 0),
      info = paste("Non-increasing probabilities for", grp$idesco_level_4[1])
    )
    # rank 1 should exist and have the highest probability
    expect_equal(ranks[1], 1L)
  }
})

# 5. No unmapped codes returns empty result -----

test_that("no unmapped codes returns 0-row data.table with correct columns", {
  td <- make_test_data()
  td$postings <- td$postings[idesco_level_4 %in% c("E001", "E002")]
  result <- classify_esco_to_cpi(
    td$postings,
    td$skills,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expected_cols <- c(
    "idesco_level_4",
    "cod_3",
    "nome_3",
    "probability",
    "rank",
    "n_postings",
    "n_skills"
  )
  expect_true(all(expected_cols %in% names(result)))
})

# 6. Known predictions -----

test_that("E003 is classified as 2.1.1 (Informatici) based on shared skills", {
  td <- make_test_data()
  result <- classify_esco_to_cpi(
    td$postings,
    td$skills,
    top_k = 1L,
    verbose = FALSE
  )

  top_pred <- result[idesco_level_4 == "E003"]
  expect_equal(top_pred$cod_3, "2.1.1")
})

# 7. Alpha smoothing -----

test_that("different alpha values produce valid but distinct results", {
  # Use data where E003 shares skills with both classes so both
  # classes score and alpha affects posterior probabilities.
  postings_alpha <- data.table::data.table(
    general_id = 1:6,
    idesco_level_4 = c("E001", "E001", "E002", "E002", "E003", "E003"),
    cp2021_id_level_3 = c("2.1.1", "2.1.1", "3.1.2", "3.1.2", NA, NA),
    cp2021_level_3 = c(
      "Informatici",
      "Informatici",
      "Ingegneri",
      "Ingegneri",
      NA,
      NA
    )
  )
  skills_alpha <- data.table::data.table(
    general_id = c(
      1L,
      1L,
      1L,
      2L,
      2L,
      3L,
      3L,
      3L,
      4L,
      4L,
      5L,
      5L,
      5L,
      6L,
      6L
    ),
    escoskill_level_3 = c(
      "S01",
      "S02",
      "S05",
      "S01",
      "S02",
      "S03",
      "S04",
      "S05",
      "S03",
      "S04",
      "S01",
      "S02",
      "S05",
      "S01",
      "S05"
    )
  )

  result_low <- classify_esco_to_cpi(
    postings_alpha,
    skills_alpha,
    alpha = 0.001,
    verbose = FALSE
  )
  result_high <- classify_esco_to_cpi(
    postings_alpha,
    skills_alpha,
    alpha = 10.0,
    verbose = FALSE
  )

  # Both should have correct structure
  expected_cols <- c(
    "idesco_level_4",
    "cod_3",
    "nome_3",
    "probability",
    "rank",
    "n_postings",
    "n_skills"
  )
  expect_true(all(expected_cols %in% names(result_low)))
  expect_true(all(expected_cols %in% names(result_high)))

  # Both should have probabilities summing to 1
  prob_low <- result_low[, .(total = sum(probability)), by = idesco_level_4]
  prob_high <- result_high[, .(total = sum(probability)), by = idesco_level_4]
  for (i in seq_len(nrow(prob_low))) {
    expect_equal(prob_low$total[i], 1.0, tolerance = 1e-6)
  }
  for (i in seq_len(nrow(prob_high))) {
    expect_equal(prob_high$total[i], 1.0, tolerance = 1e-6)
  }

  # Probabilities should differ between the two alpha values
  merged <- merge(
    result_low,
    result_high,
    by = c("idesco_level_4", "cod_3"),
    suffixes = c("_low", "_high")
  )
  expect_false(all(merged$probability_low == merged$probability_high))
})

# 8. Column name handling -----

test_that("uppercase ESCOSKILL_LEVEL_3 column is handled correctly", {
  td <- make_test_data()
  data.table::setnames(td$skills, "escoskill_level_3", "ESCOSKILL_LEVEL_3")
  result <- classify_esco_to_cpi(
    td$postings,
    td$skills,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

# 9. Verbose messages -----

test_that("verbose controls message output", {
  td <- make_test_data()

  expect_message(
    classify_esco_to_cpi(td$postings, td$skills, verbose = TRUE)
  )

  expect_silent(
    classify_esco_to_cpi(td$postings, td$skills, verbose = FALSE)
  )
})

# 10. Mixed mapped/unmapped postings for same ESCO L4 -----

test_that("ESCO L4 with some mapped and some NA postings is treated as mapped", {
  postings_mixed <- data.table::data.table(
    general_id = 1:6,
    idesco_level_4 = c("E001", "E001", "E001", "E002", "E003", "E003"),
    cp2021_id_level_3 = c("2.1.1", "2.1.1", NA, "3.1.2", NA, NA),
    cp2021_level_3 = c(
      "Informatici",
      "Informatici",
      NA,
      "Ingegneri",
      NA,
      NA
    )
  )
  skills_mixed <- data.table::data.table(
    general_id = c(1L, 1L, 2L, 3L, 4L, 4L, 5L, 5L, 6L),
    escoskill_level_3 = c(
      "S01",
      "S02",
      "S01",
      "S01",
      "S03",
      "S04",
      "S01",
      "S02",
      "S01"
    )
  )

  result <- classify_esco_to_cpi(
    postings_mixed,
    skills_mixed,
    top_k = 1L,
    verbose = FALSE
  )

  # E001 has at least one mapped posting, so it should be mapped
  expect_false("E001" %in% result$idesco_level_4)
  # E003 is fully unmapped, so it should be classified
  expect_true("E003" %in% result$idesco_level_4)
})

# 11. Empty string cp2021_id_level_3 treated as unmapped -----

test_that("empty string cp2021_id_level_3 is treated as unmapped", {
  postings_empty <- data.table::data.table(
    general_id = 1:4,
    idesco_level_4 = c("E001", "E001", "E002", "E002"),
    cp2021_id_level_3 = c("2.1.1", "2.1.1", "", ""),
    cp2021_level_3 = c("Informatici", "Informatici", "", "")
  )
  skills_empty <- data.table::data.table(
    general_id = c(1L, 1L, 2L, 3L, 3L, 4L),
    escoskill_level_3 = c("S01", "S02", "S01", "S01", "S02", "S01")
  )

  result <- classify_esco_to_cpi(
    postings_empty,
    skills_empty,
    top_k = 1L,
    verbose = FALSE
  )

  # E002 has empty string CPI codes, so it should be treated as unmapped
  expect_true("E002" %in% result$idesco_level_4)
  # E001 is mapped
  expect_false("E001" %in% result$idesco_level_4)
})
