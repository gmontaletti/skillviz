# Tests for crosswalk.R -----

# 1. build_cpi_esco_crosswalk -----

test_that("build_cpi_esco_crosswalk returns expected structure", {
  esco_mapping <- data.table::data.table(
    idesco_level_4 = c("E001", "E001", "E002"),
    esco_level_4 = c("Analyst IT", "Analista informatico", "Engineer"),
    idcp_2011_v = c("2.1.1.4.1", "2.1.1.4.1", "3.1.2.1.0")
  )

  cpi3 <- data.table::data.table(
    cod_3 = c("2.1.1", "3.1.2"),
    nome_3 = c("Informatici", "Ingegneri")
  )

  result <- build_cpi_esco_crosswalk(esco_mapping, cpi3)

  expect_s3_class(result, "data.table")
  expect_true(all(
    c("idesco_level_4", "it_esco_level_4", "cod_3", "nome_3") %in% names(result)
  ))
})

test_that("build_cpi_esco_crosswalk collapses labels correctly", {
  esco_mapping <- data.table::data.table(
    idesco_level_4 = c("E001", "E001"),
    esco_level_4 = c("Label A", "Label B"),
    idcp_2011_v = c("2.1.1.4.1", "2.1.1.4.2")
  )

  cpi3 <- data.table::data.table(
    cod_3 = "2.1.1",
    nome_3 = "Informatici"
  )

  result <- build_cpi_esco_crosswalk(esco_mapping, cpi3)

  expect_equal(nrow(result), 1L)
  # The it_esco_level_4 should contain both labels
  expect_true(grepl("Label A", result$it_esco_level_4))
  expect_true(grepl("Label B", result$it_esco_level_4))
})

test_that("build_cpi_esco_crosswalk derives cod_3 from first 5 chars", {
  esco_mapping <- data.table::data.table(
    idesco_level_4 = "E001",
    esco_level_4 = "Test",
    idcp_2011_v = "1.2.3.4.5"
  )

  cpi3 <- data.table::data.table(
    cod_3 = "1.2.3",
    nome_3 = "Test group"
  )

  result <- build_cpi_esco_crosswalk(esco_mapping, cpi3)

  expect_equal(result$cod_3, "1.2.3")
})

test_that("build_cpi_esco_crosswalk errors on missing columns", {
  bad_esco <- data.table::data.table(id = "E001")
  cpi3 <- data.table::data.table(cod_3 = "1.2.3", nome_3 = "Test")

  expect_error(
    build_cpi_esco_crosswalk(bad_esco, cpi3),
    "missing required columns"
  )

  esco_mapping <- data.table::data.table(
    idesco_level_4 = "E001",
    esco_level_4 = "Test",
    idcp_2011_v = "1.2.3.4.5"
  )
  bad_cpi3 <- data.table::data.table(code = "1.2.3")

  expect_error(
    build_cpi_esco_crosswalk(esco_mapping, bad_cpi3),
    "missing required columns"
  )
})

# 2. prepare_annunci_esco -----

test_that("prepare_annunci_esco merges and returns expected columns", {
  ann <- data.table::data.table(
    general_id = c(1L, 2L),
    idesco_level_4 = c("E001", "E002"),
    year_grab_date = c(2023L, 2023L),
    month_grab_date = c(1L, 6L),
    day_grab_date = c(15L, 20L),
    year_expire_date = c(2023L, 2023L),
    month_expire_date = c(3L, 9L),
    day_expire_date = c(15L, 20L)
  )

  esco_mapping <- data.table::data.table(
    idesco_level_4 = c("E001", "E002"),
    esco_level_4 = c("Analyst", "Engineer")
  )

  result <- prepare_annunci_esco(ann, esco_mapping)

  expect_s3_class(result, "data.table")
  expect_true(all(
    c("general_id", "gdate", "idesco_level_4", "it_esco_level_4") %in%
      names(result)
  ))
  expect_true(inherits(result$gdate, "IDate"))
})

test_that("prepare_annunci_esco returns unique rows", {
  ann <- data.table::data.table(
    general_id = c(1L, 1L),
    idesco_level_4 = c("E001", "E001"),
    year_grab_date = c(2023L, 2023L),
    month_grab_date = c(1L, 1L),
    day_grab_date = c(15L, 15L),
    year_expire_date = c(2023L, 2023L),
    month_expire_date = c(3L, 3L),
    day_expire_date = c(15L, 15L)
  )

  esco_mapping <- data.table::data.table(
    idesco_level_4 = "E001",
    esco_level_4 = "Analyst"
  )

  result <- prepare_annunci_esco(ann, esco_mapping)

  # Duplicates should be collapsed
  expect_equal(nrow(result), 1L)
})

test_that("prepare_annunci_esco errors on missing columns in ann", {
  bad_ann <- data.table::data.table(general_id = 1L)
  esco_mapping <- data.table::data.table(
    idesco_level_4 = "E001",
    esco_level_4 = "Test"
  )

  expect_error(
    prepare_annunci_esco(bad_ann, esco_mapping),
    "missing required columns"
  )
})

test_that("prepare_annunci_esco errors on missing columns in esco_mapping", {
  ann <- data.table::data.table(
    general_id = 1L,
    idesco_level_4 = "E001",
    year_grab_date = 2023L,
    month_grab_date = 1L,
    day_grab_date = 15L,
    year_expire_date = 2023L,
    month_expire_date = 3L,
    day_expire_date = 15L
  )
  bad_esco <- data.table::data.table(idesco_level_4 = "E001")

  expect_error(
    prepare_annunci_esco(ann, bad_esco),
    "missing required columns"
  )
})

# 3. predict_cp4_knn -----

test_that("predict_cp4_knn returns predictions for unlabeled rows", {
  postings <- data.table::data.table(
    general_id = as.character(1:20),
    idesco_level_4 = rep(c(1000L, 2000L), each = 10),
    cp2021_id_level_4 = c(
      rep("1.1.1.1", 4),
      rep("1.1.1.2", 3),
      rep(NA, 3),
      rep("2.2.2.1", 5),
      rep("2.2.2.2", 2),
      rep(NA, 3)
    ),
    idsector = rep(c("C", "F"), each = 10)
  )
  set.seed(42)
  skills <- data.table::data.table(
    general_id = as.character(rep(1:20, each = 3)),
    escoskill_level_3 = paste0("s", sample(1:8, 60, replace = TRUE))
  )
  result <- predict_cp4_knn(postings, skills, k = 3L, verbose = FALSE)
  expect_s3_class(result, "data.table")
  expect_true(all(
    c("general_id", "cp2021_id_level_4", "confidence", "method") %in%
      names(result)
  ))
  expect_equal(nrow(result), 6L)
  expect_true(all(
    result$method %in% c("knn", "frequency", "single_candidate", "no_match")
  ))
})

test_that("predict_cp4_knn handles single-candidate ESCO groups", {
  postings <- data.table::data.table(
    general_id = as.character(1:5),
    idesco_level_4 = rep(1000L, 5),
    cp2021_id_level_4 = c("1.1.1.1", "1.1.1.1", "1.1.1.1", NA, NA)
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:5, each = 2)),
    escoskill_level_3 = paste0("s", 1:10)
  )
  result <- predict_cp4_knn(postings, skills, k = 3L, verbose = FALSE)
  expect_equal(result$method, rep("single_candidate", 2))
  expect_equal(result$cp2021_id_level_4, rep("1.1.1.1", 2))
})

test_that("predict_cp4_knn works without idsector column", {
  postings <- data.table::data.table(
    general_id = as.character(1:10),
    idesco_level_4 = rep(1000L, 10),
    cp2021_id_level_4 = c(
      rep("1.1.1.1", 4),
      rep("1.1.1.2", 3),
      rep(NA, 3)
    )
  )
  set.seed(42)
  skills <- data.table::data.table(
    general_id = as.character(rep(1:10, each = 3)),
    escoskill_level_3 = paste0("s", sample(1:6, 30, replace = TRUE))
  )
  expect_no_error(
    predict_cp4_knn(postings, skills, k = 3L, verbose = FALSE)
  )
})

test_that("predict_cp4_knn returns empty table when no unlabeled rows", {
  postings <- data.table::data.table(
    general_id = as.character(1:3),
    idesco_level_4 = rep(1000L, 3),
    cp2021_id_level_4 = rep("1.1.1.1", 3)
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:3, each = 2)),
    escoskill_level_3 = paste0("s", 1:6)
  )
  result <- predict_cp4_knn(postings, skills, verbose = FALSE)
  expect_equal(nrow(result), 0L)
})

test_that("predict_cp4_knn handles ESCO not in training", {
  postings <- data.table::data.table(
    general_id = as.character(1:4),
    idesco_level_4 = c(1000L, 1000L, 9999L, 9999L),
    cp2021_id_level_4 = c("1.1.1.1", "1.1.1.1", NA, NA)
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:4, each = 2)),
    escoskill_level_3 = paste0("s", 1:8)
  )
  result <- predict_cp4_knn(postings, skills, k = 3L, verbose = FALSE)
  no_match <- result[method == "no_match"]
  expect_equal(nrow(no_match), 2L)
  expect_true(all(is.na(no_match$cp2021_id_level_4)))
})
