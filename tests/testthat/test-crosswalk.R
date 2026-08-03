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

test_that("predict_cp4_knn boosts an empty idsector as a sector of its own", {
  # itaposts writes an unknown sector as "" and never as NA, so unknown-sector
  # announcements boost each other. Normalising "" to NA was measured on the
  # 24-month window and costs 1.51 pp on the rows it touches, so the behaviour
  # is deliberate; this pins it against a well-meant "fix".
  postings <- data.table::data.table(
    general_id = as.character(1:8),
    idesco_level_4 = rep(1000L, 8),
    cp2021_id_level_4 = c(
      "1.1.1.1",
      "1.1.1.1",
      "1.1.1.2",
      "1.1.1.2",
      "1.1.1.2",
      "1.1.1.2",
      NA,
      NA
    ),
    idsector = c("", "", "C", "C", "C", "C", "", "C")
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:8, each = 2)),
    escoskill_level_3 = rep(c("a", "b"), 8)
  )

  result <- predict_cp4_knn(
    postings,
    skills,
    k = 6L,
    sector_boost = 5,
    verbose = FALSE
  )

  # Every row carries identical skills, so Jaccard alone would hand both test
  # rows the 1.1.1.2 majority; only the sector boost can split them.
  expect_identical(
    result[general_id == "7", cp2021_id_level_4],
    "1.1.1.1"
  )
  expect_identical(
    result[general_id == "8", cp2021_id_level_4],
    "1.1.1.2"
  )

  # An NA sector must still switch the boost off for that row.
  postings_na <- data.table::copy(postings)
  postings_na[!nzchar(idsector), idsector := NA_character_]
  result_na <- predict_cp4_knn(
    postings_na,
    skills,
    k = 6L,
    sector_boost = 5,
    verbose = FALSE
  )
  expect_identical(
    result_na[general_id == "7", cp2021_id_level_4],
    "1.1.1.2"
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

# 3b. predict_cp4_knn rescue_no_match -----

# Fixture: two ESCO groups that behave normally, plus three unlabeled rows with
# no idesco_level_4 -- two carrying skills (rescuable) and one with none.
.rescue_fixture <- function() {
  postings <- data.table::data.table(
    general_id = as.character(1:12),
    idesco_level_4 = c(rep(c(1000L, 2000L), each = 4), rep(NA_integer_, 4)),
    cp2021_id_level_4 = c(
      "1.1.1.1",
      "1.1.1.2",
      NA,
      NA,
      "2.2.2.1",
      "2.2.2.2",
      NA,
      NA,
      "1.1.1.1",
      NA,
      NA,
      NA
    ),
    idsector = c(rep("C", 4), rep("F", 4), "C", "C", "F", "F")
  )
  skills <- data.table::data.table(
    general_id = as.character(c(
      1,
      1,
      2,
      2,
      3,
      3,
      4,
      4,
      5,
      5,
      6,
      6,
      7,
      7,
      8,
      8,
      9,
      9,
      10,
      10,
      11,
      11
    )),
    escoskill_level_3 = c(
      "s1",
      "s2",
      "s2",
      "s3",
      "s1",
      "s2",
      "s2",
      "s3",
      "s4",
      "s5",
      "s5",
      "s6",
      "s4",
      "s5",
      "s5",
      "s6",
      "s1",
      "s2",
      "s1",
      "s2",
      "s4",
      "s5"
    )
  )
  list(postings = postings, skills = skills)
}

test_that("rescue_no_match = FALSE is bit-identical to the previous behaviour", {
  f <- .rescue_fixture()
  a <- predict_cp4_knn(f$postings, f$skills, k = 3L, verbose = FALSE)
  b <- predict_cp4_knn(
    f$postings,
    f$skills,
    k = 3L,
    rescue_no_match = FALSE,
    verbose = FALSE
  )
  expect_identical(a, b)
  expect_false("knn_global" %in% a$method)
})

test_that("rescue_no_match = TRUE classifies no-ESCO rows that have skills", {
  f <- .rescue_fixture()
  base <- predict_cp4_knn(f$postings, f$skills, k = 3L, verbose = FALSE)
  res <- predict_cp4_knn(
    f$postings,
    f$skills,
    k = 3L,
    rescue_no_match = TRUE,
    rescue_k = 3L,
    verbose = FALSE
  )

  # Same rows in, same rows out -- the rescue relabels, it does not add or drop.
  expect_setequal(res$general_id, base$general_id)

  rescued <- res[method == "knn_global"]
  expect_true(nrow(rescued) > 0L)
  expect_true(all(!is.na(rescued$cp2021_id_level_4)))
  expect_true(all(rescued$confidence > 0 & rescued$confidence <= 1))

  # id 12 has no skills at all, so it can never be rescued.
  expect_true("12" %in% res[method == "no_match", general_id])

  # Rows the ESCO-restricted path already handled must be untouched.
  keep <- base[method != "no_match"]
  expect_identical(
    res[general_id %in% keep$general_id][order(general_id)],
    keep[order(general_id)]
  )
})

test_that("rescue_no_match respects rescue_max_train and stays deterministic", {
  f <- .rescue_fixture()
  a <- predict_cp4_knn(
    f$postings,
    f$skills,
    k = 3L,
    rescue_no_match = TRUE,
    rescue_k = 3L,
    rescue_max_train = 2L,
    verbose = FALSE
  )
  b <- predict_cp4_knn(
    f$postings,
    f$skills,
    k = 3L,
    rescue_no_match = TRUE,
    rescue_k = 3L,
    rescue_max_train = 2L,
    verbose = FALSE
  )
  expect_identical(a, b)
})

# 3c. predict_cp4_knn max_train determinism -----

test_that("max_train subsampling is deterministic and warns", {
  set.seed(11)
  n_tr <- 60L
  postings <- data.table::data.table(
    general_id = as.character(seq_len(n_tr + 6L)),
    idesco_level_4 = 1000L,
    cp2021_id_level_4 = c(
      rep(c("1.1.1.1", "1.1.1.2"), length.out = n_tr),
      rep(NA_character_, 6L)
    ),
    idsector = "C"
  )
  skills <- data.table::rbindlist(lapply(seq_len(n_tr + 6L), function(i) {
    data.table::data.table(
      general_id = as.character(i),
      escoskill_level_3 = paste0("s", sample.int(6L, 3L))
    )
  }))

  # Cap well below the group size so the stride path is exercised.
  expect_warning(
    a <- predict_cp4_knn(
      postings, skills, k = 3L, max_train = 10L, verbose = FALSE
    ),
    "above max_train"
  )
  expect_warning(
    b <- predict_cp4_knn(
      postings, skills, k = 3L, max_train = 10L, verbose = FALSE
    ),
    "above max_train"
  )
  # Deterministic: two runs agree exactly, with no seed set between them.
  expect_identical(a, b)
})

test_that("max_train default leaves small groups untouched and silent", {
  f <- .rescue_fixture()
  expect_no_warning(
    a <- predict_cp4_knn(f$postings, f$skills, k = 3L, verbose = FALSE)
  )
  b <- predict_cp4_knn(
    f$postings, f$skills, k = 3L, max_train = 50000L, verbose = FALSE
  )
  expect_identical(a, b)
})

# 3d. .global_knn_vote (Rcpp) semantics -----

# These pin the exact selection and vote behaviour of the compiled kernel. The
# expectations were snapshotted from an implementation proven identical to the
# former pure-R reference across 144 randomised tie-dense configurations and 9
# edge cases; the reference has since been removed, so these are the contract.
.gkv_fixture <- function() {
  sk <- data.table::data.table(
    general_id = c("a", "a", "b", "b", "c", "c", "d", "d", "e", "e"),
    escoskill_level_3 = c("s1", "s2", "s1", "s2", "s1", "s3", "s2", "s3", "s1", "s2")
  )
  tr <- data.table::data.table(
    general_id = c("a", "b", "c", "d", "e"),
    cp2021_id_level_4 = c("1.1.1.1", "1.1.1.1", "1.1.1.2", "1.1.1.2", "1.1.1.3")
  )[sk, on = "general_id"]
  list(
    tr = tr,
    te = data.table::data.table(
      general_id = c("t1", "t1"), escoskill_level_3 = c("s1", "s2")
    )
  )
}

test_that(".global_knn_vote returns the expected winner and confidence", {
  d <- .gkv_fixture()
  r <- skillviz:::.global_knn_vote(
    "t1", d$te, d$tr, NA_character_, NULL, 3L, 1, 1e6
  )
  expect_equal(nrow(r), 1L)
  expect_equal(r$cp2021_id_level_4, "1.1.1.1")
  expect_equal(r$confidence, 2 / 3, tolerance = 1e-9)
  expect_equal(r$method, "knn_global")
})

test_that(".global_knn_vote applies the sector boost after selection", {
  d <- .gkv_fixture()
  sect <- stats::setNames(c("C", "F", "C", "F", "C"), c("a", "b", "c", "d", "e"))
  r <- skillviz:::.global_knn_vote("t1", d$te, d$tr, "C", sect, 3L, 5, 1e6)
  expect_equal(r$cp2021_id_level_4, "1.1.1.1")
  # Boosting the same-sector neighbours changes the vote share, not the winner.
  expect_equal(r$confidence, 6 / 11, tolerance = 1e-9)
})

test_that(".global_knn_vote honours k", {
  d <- .gkv_fixture()
  r <- skillviz:::.global_knn_vote(
    "t1", d$te, d$tr, NA_character_, NULL, 1L, 1, 1e6
  )
  # A single neighbour means the winner takes the whole vote.
  expect_equal(r$confidence, 1)
})

test_that(".global_knn_vote returns NULL on degenerate input", {
  d <- .gkv_fixture()
  empty <- data.table::data.table(
    general_id = character(), escoskill_level_3 = character(),
    cp2021_id_level_4 = character()
  )
  expect_null(skillviz:::.global_knn_vote(
    "t1", d$te, empty, NA_character_, NULL, 3L, 1, 1e6
  ))
  # No shared skills => no non-zero similarity => nothing to vote on.
  disjoint <- data.table::copy(d$te)[
    , escoskill_level_3 := paste0("z", escoskill_level_3)
  ]
  expect_null(skillviz:::.global_knn_vote(
    "t1", disjoint, d$tr, NA_character_, NULL, 3L, 1, 1e6
  ))
  # Every training CP4 missing => every candidate filtered out.
  allna <- data.table::copy(d$tr)[, cp2021_id_level_4 := NA_character_]
  expect_null(skillviz:::.global_knn_vote(
    "t1", d$te, allna, NA_character_, NULL, 3L, 1, 1e6
  ))
})

test_that(".global_knn_vote is deterministic under the train-pool cap", {
  d <- .gkv_fixture()
  a <- skillviz:::.global_knn_vote("t1", d$te, d$tr, NA_character_, NULL, 3L, 1, 2L)
  b <- skillviz:::.global_knn_vote("t1", d$te, d$tr, NA_character_, NULL, 3L, 1, 2L)
  expect_identical(a, b)
})
