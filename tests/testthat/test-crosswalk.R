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
      postings,
      skills,
      k = 3L,
      max_train = 10L,
      verbose = FALSE
    ),
    "above max_train"
  )
  expect_warning(
    b <- predict_cp4_knn(
      postings,
      skills,
      k = 3L,
      max_train = 10L,
      verbose = FALSE
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
    f$postings,
    f$skills,
    k = 3L,
    max_train = 50000L,
    verbose = FALSE
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
    escoskill_level_3 = c(
      "s1",
      "s2",
      "s1",
      "s2",
      "s1",
      "s3",
      "s2",
      "s3",
      "s1",
      "s2"
    )
  )
  tr <- data.table::data.table(
    general_id = c("a", "b", "c", "d", "e"),
    cp2021_id_level_4 = c("1.1.1.1", "1.1.1.1", "1.1.1.2", "1.1.1.2", "1.1.1.3")
  )[sk, on = "general_id"]
  list(
    tr = tr,
    te = data.table::data.table(
      general_id = c("t1", "t1"),
      escoskill_level_3 = c("s1", "s2")
    )
  )
}

test_that(".global_knn_vote returns the expected winner and confidence", {
  d <- .gkv_fixture()
  r <- skillviz:::.global_knn_vote(
    "t1",
    d$te,
    d$tr,
    NA_character_,
    NULL,
    3L,
    1,
    1e6
  )
  expect_equal(nrow(r), 1L)
  expect_equal(r$cp2021_id_level_4, "1.1.1.1")
  expect_equal(r$confidence, 2 / 3, tolerance = 1e-9)
  expect_equal(r$method, "knn_global")
})

test_that(".global_knn_vote applies the sector boost after selection", {
  d <- .gkv_fixture()
  sect <- stats::setNames(
    c("C", "F", "C", "F", "C"),
    c("a", "b", "c", "d", "e")
  )
  r <- skillviz:::.global_knn_vote("t1", d$te, d$tr, "C", sect, 3L, 5, 1e6)
  expect_equal(r$cp2021_id_level_4, "1.1.1.1")
  # Boosting the same-sector neighbours changes the vote share, not the winner.
  expect_equal(r$confidence, 6 / 11, tolerance = 1e-9)
})

test_that(".global_knn_vote honours k", {
  d <- .gkv_fixture()
  r <- skillviz:::.global_knn_vote(
    "t1",
    d$te,
    d$tr,
    NA_character_,
    NULL,
    1L,
    1,
    1e6
  )
  # A single neighbour means the winner takes the whole vote.
  expect_equal(r$confidence, 1)
})

test_that(".global_knn_vote returns NULL on degenerate input", {
  d <- .gkv_fixture()
  empty <- data.table::data.table(
    general_id = character(),
    escoskill_level_3 = character(),
    cp2021_id_level_4 = character()
  )
  expect_null(skillviz:::.global_knn_vote(
    "t1",
    d$te,
    empty,
    NA_character_,
    NULL,
    3L,
    1,
    1e6
  ))
  # No shared skills => no non-zero similarity => nothing to vote on.
  disjoint <- data.table::copy(d$te)[,
    escoskill_level_3 := paste0("z", escoskill_level_3)
  ]
  expect_null(skillviz:::.global_knn_vote(
    "t1",
    disjoint,
    d$tr,
    NA_character_,
    NULL,
    3L,
    1,
    1e6
  ))
  # Every training CP4 missing => every candidate filtered out.
  allna <- data.table::copy(d$tr)[, cp2021_id_level_4 := NA_character_]
  expect_null(skillviz:::.global_knn_vote(
    "t1",
    d$te,
    allna,
    NA_character_,
    NULL,
    3L,
    1,
    1e6
  ))
})

test_that(".global_knn_vote is deterministic under the train-pool cap", {
  d <- .gkv_fixture()
  a <- skillviz:::.global_knn_vote(
    "t1",
    d$te,
    d$tr,
    NA_character_,
    NULL,
    3L,
    1,
    2L
  )
  b <- skillviz:::.global_knn_vote(
    "t1",
    d$te,
    d$tr,
    NA_character_,
    NULL,
    3L,
    1,
    2L
  )
  expect_identical(a, b)
})


# 3f. CP4 regression after the level-5 parameterisation -----

# predict_cp4_knn() is consumed by both skillviz_workflow's targets pipeline and
# container/impute_cp4.R, so CLAUDE.md requires an output comparison against a
# known baseline. reference/skillviz/make_cp4_baseline.R captured that baseline
# from the unmodified v0.2.0 function before .predict_cp_knn() was extracted.
#
# expect_identical(), not expect_equal(): `confidence` is a ratio of doubles and
# the claim under test is bit-identity, not approximate agreement.
test_that("predict_cp4_knn is bit-identical to the pre-CP5 baseline", {
  path <- testthat::test_path("fixtures", "cp4_baseline.rds")
  skip_if_not(file.exists(path), "baseline fixture not built")
  b <- readRDS(path)

  for (case in b$baseline) {
    a <- case$args
    f <- b$fixtures[[a$fixture]]
    w <- character(0)
    got <- withCallingHandlers(
      predict_cp4_knn(
        f$postings,
        f$skills,
        k = a$k,
        sector_boost = a$sector_boost,
        max_train = a$max_train,
        rescue_no_match = a$rescue_no_match,
        verbose = FALSE
      ),
      warning = function(cond) {
        w <<- c(w, conditionMessage(cond))
        invokeRestart("muffleWarning")
      }
    )
    data.table::setorder(got, general_id)
    label <- sprintf(
      "fixture=%s k=%d boost=%g max_train=%d rescue=%s",
      a$fixture,
      a$k,
      a$sector_boost,
      a$max_train,
      a$rescue_no_match
    )
    # The DECISIONS must be bit-identical: which code, by which route, for which
    # announcement. Those are what a refactor can break, and they are discrete.
    expect_identical(got$general_id, case$result$general_id, info = label)
    expect_identical(
      got$cp2021_id_level_4,
      case$result$cp2021_id_level_4,
      info = label
    )
    expect_identical(got$method, case$result$method, info = label)
    expect_identical(names(got), names(case$result), info = label)

    # `confidence` is compared to a tolerance, not bit-for-bit. It is a ratio of
    # sums produced by Matrix::tcrossprod(), so its last bits depend on the BLAS
    # summation order: the fixture was generated on macOS/Accelerate and CI runs
    # on Ubuntu with a different BLAS, which moves values by 1-2 ULP
    # (0.519999999999999907 against 0.520000000000000018). Pinning that would be
    # pinning the build machine, not the behaviour. 1e-12 is ~4 orders of
    # magnitude above the observed drift and far below any change a real
    # regression would cause -- which would move a code, not the 16th digit.
    expect_equal(
      got$confidence,
      case$result$confidence,
      tolerance = 1e-12,
      info = label
    )
    expect_identical(w, case$warnings, info = label)
  }
})

# 3g. predict_cp5_knn -----

.cp5_fixture <- function() {
  postings <- data.table::data.table(
    general_id = as.character(1:12),
    idesco_level_4 = c(rep(c(1000L, 2000L), each = 5), NA_integer_, 3000L),
    # 1000 has two CP5 codes under one CP4; 2000 has two CP5 under two CP4.
    cp2021_id_level_5 = c(
      "1.1.1.1.1",
      "1.1.1.1.2",
      "1.1.1.1.1",
      NA,
      NA,
      "2.2.2.1.0",
      "2.2.2.1.0",
      "2.2.2.2.0",
      NA,
      NA,
      NA,
      NA
    ),
    cp2021_id_level_4 = c(
      "1.1.1.1",
      "1.1.1.1",
      "1.1.1.1",
      NA,
      NA,
      "2.2.2.1",
      "2.2.2.1",
      "2.2.2.2",
      NA,
      NA,
      NA,
      NA
    ),
    idsector = c(rep("C", 5), rep("F", 5), "C", "F")
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
      "s1",
      "s3",
      "s2",
      "s3",
      "s4",
      "s5",
      "s5",
      "s6",
      "s4",
      "s6",
      "s4",
      "s5",
      "s5",
      "s6",
      "s1",
      "s2"
    )
  )
  list(postings = postings, skills = skills)
}

test_that("predict_cp5_knn returns level-5 codes for unlabeled rows", {
  f <- .cp5_fixture()
  res <- predict_cp5_knn(
    f$postings,
    f$skills,
    restrictor = "idesco_level_4",
    k = 3L,
    verbose = FALSE
  )

  expect_true(data.table::is.data.table(res))
  expect_named(
    res,
    c("general_id", "cp2021_id_level_5", "confidence", "method")
  )
  # Every unlabeled row gets exactly one prediction.
  expect_setequal(res$general_id, as.character(c(4, 5, 9, 10, 11, 12)))
  # Predictions are drawn from the codes observed in the row's ESCO group.
  pred <- res[!is.na(cp2021_id_level_5)]
  expect_true(all(nchar(pred$cp2021_id_level_5) == 9L))
})

test_that("predict_cp5_knn ignores a cp2021_id_level_4 column in postings", {
  f <- .cp5_fixture()
  with_cp4 <- predict_cp5_knn(
    f$postings,
    f$skills,
    restrictor = "idesco_level_4",
    k = 3L,
    verbose = FALSE
  )
  without <- predict_cp5_knn(
    f$postings[, !"cp2021_id_level_4"],
    f$skills,
    restrictor = "idesco_level_4",
    k = 3L,
    verbose = FALSE
  )
  # The rename must not let a stale cp2021_id_level_4 shadow the target.
  expect_identical(with_cp4, without)
})

test_that("predict_cp5_knn errors when the level-5 column is absent", {
  f <- .cp5_fixture()
  expect_error(
    predict_cp5_knn(
      f$postings[, !"cp2021_id_level_5"],
      f$skills,
      verbose = FALSE
    ),
    "cp2021_id_level_5"
  )
})

test_that("predict_cp5_knn returns an empty table named at level 5", {
  f <- .cp5_fixture()
  full <- data.table::copy(f$postings)
  full[, cp2021_id_level_5 := "1.1.1.1.1"]
  res <- predict_cp5_knn(
    full,
    f$skills,
    restrictor = "idesco_level_4",
    verbose = FALSE
  )
  expect_identical(nrow(res), 0L)
  expect_named(
    res,
    c("general_id", "cp2021_id_level_5", "confidence", "method")
  )
})

# The level-4 and level-5 engines share one code path, so a CP5 run over CP4
# labels must reproduce predict_cp4_knn() exactly. This is what licenses
# deriving CP4 by truncation from a single neighbour search.
#
# This goes through .predict_cp_knn() rather than predict_cp5_knn() so that
# mode_tiebreak can be pinned to the level-4 value. predict_cp5_knn() hardcodes
# "code"; comparing across different tie-breaks would make the assertion depend
# on whether the fixture happens to contain a tie, and it would pass for the
# wrong reason on a fixture that does not.
test_that("the level-generic engine agrees with predict_cp4_knn on CP4 input", {
  f <- .rescue_fixture()
  cp4 <- predict_cp4_knn(f$postings, f$skills, k = 3L, verbose = FALSE)
  as5 <- data.table::copy(f$postings)
  data.table::setnames(as5, "cp2021_id_level_4", "cp2021_id_level_5")
  via5 <- .predict_cp_knn(
    as5,
    f$skills,
    target_col = "cp2021_id_level_5",
    restrictor_col = "idesco_level_4",
    restrictor_na = character(0),
    mode_tiebreak = "emission",
    k = 3L,
    sector_boost = 3.0,
    verbose = FALSE
  )
  data.table::setnames(via5, "cp2021_id_level_5", "cp2021_id_level_4")
  expect_identical(via5, cp4)
})


# 3h. ESCO level-5 restrictor -----

# Two ESCO5 codes under one ESCO4 parent, each mapping to its own CP5. The
# finer restrictor separates them; the coarser one pools them. That separation
# is the whole reason level 5 is a better restrictor.
.esco5_fixture <- function() {
  postings <- data.table::data.table(
    general_id = as.character(1:12),
    idesco_level_4 = rep("1000", 12L),
    idesco_level_5 = rep(c("1000.1", "1000.2"), each = 6L),
    cp2021_id_level_5 = c(
      "1.1.1.1.1",
      "1.1.1.1.1",
      "1.1.1.1.1",
      "1.1.1.1.1",
      NA,
      NA,
      "2.2.2.2.0",
      "2.2.2.2.0",
      "2.2.2.2.0",
      "2.2.2.2.0",
      NA,
      NA
    ),
    idsector = rep(c("C", "F"), each = 6L)
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:12, each = 2L)),
    escoskill_level_3 = c(
      rbind(rep("s1", 6L), rep("s2", 6L)),
      rbind(rep("s4", 6L), rep("s5", 6L))
    )
  )
  list(postings = postings, skills = skills)
}

test_that("the level-5 restrictor splits an ESCO4 group its parent pools", {
  f <- .esco5_fixture()

  at5 <- predict_cp5_knn(
    f$postings,
    f$skills,
    restrictor = "idesco_level_5",
    k = 3L,
    verbose = FALSE
  )
  at4 <- predict_cp5_knn(
    f$postings,
    f$skills,
    restrictor = "idesco_level_4",
    k = 3L,
    verbose = FALSE
  )

  data.table::setorder(at5, general_id)
  data.table::setorder(at4, general_id)
  expect_identical(at5$general_id, at4$general_id)

  # Each ESCO5 group holds exactly one CP5 code, so level 5 assigns directly.
  expect_true(all(at5$method == "single_candidate"))
  expect_identical(at5$confidence, rep(1.0, nrow(at5)))
  # The pooled ESCO4 group holds two, so level 4 has to vote.
  expect_false(any(at4$method == "single_candidate"))

  # And level 5 gets them right by construction: rows 5-6 sit in group 1000.1,
  # rows 11-12 in group 1000.2. Asserted by id rather than by position, because
  # general_id is character and "11" sorts before "5".
  expect_identical(
    at5[general_id %chin% c("5", "6"), unique(cp2021_id_level_5)],
    "1.1.1.1.1"
  )
  expect_identical(
    at5[general_id %chin% c("11", "12"), unique(cp2021_id_level_5)],
    "2.2.2.2.0"
  )
})

# The trap. idesco_level_5 marks a missing occupation with the literal string
# "Unclassifiable", not NA. Left un-normalised it is a perfectly good group
# value, and every ESCO-less announcement -- labelled and unlabelled alike --
# merges into one pseudo-group that returns confident nonsense instead of
# no_match.
.unclassifiable_fixture <- function() {
  postings <- data.table::data.table(
    general_id = as.character(1:10),
    idesco_level_4 = c(rep("1000", 4L), rep(NA_character_, 6L)),
    idesco_level_5 = c(rep("1000.1", 4L), rep("Unclassifiable", 6L)),
    # The ESCO-less labelled rows carry deliberately different CP5 codes, so a
    # pseudo-group would have something to vote on.
    cp2021_id_level_5 = c(
      "1.1.1.1.1",
      "1.1.1.1.1",
      NA,
      NA,
      "3.3.3.3.0",
      "4.4.4.4.0",
      "5.5.5.5.0",
      NA,
      NA,
      NA
    ),
    idsector = rep("C", 10L)
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:10, each = 2L)),
    escoskill_level_3 = rep(c("s1", "s2"), 10L)
  )
  list(postings = postings, skills = skills)
}

test_that("an Unclassifiable restrictor is treated as no ESCO code", {
  f <- .unclassifiable_fixture()
  res <- predict_cp5_knn(
    f$postings,
    f$skills,
    restrictor = "idesco_level_5",
    k = 3L,
    verbose = FALSE
  )
  data.table::setorder(res, general_id)

  unresolved <- res[general_id %chin% c("10", "8", "9")]
  expect_true(all(unresolved$method == "no_match"))
  expect_true(all(is.na(unresolved$cp2021_id_level_5)))

  # The real group is unaffected and still resolves.
  real <- res[general_id %chin% c("3", "4")]
  expect_true(all(real$cp2021_id_level_5 == "1.1.1.1.1"))
})

test_that("restrictor_na is load-bearing, not decorative", {
  f <- .unclassifiable_fixture()
  # Negative control: disable the normalisation and the sentinel becomes a
  # group. If this ever starts returning no_match, the guard above has stopped
  # testing anything.
  leaky <- predict_cp5_knn(
    f$postings,
    f$skills,
    restrictor = "idesco_level_5",
    restrictor_na = character(0),
    k = 3L,
    verbose = FALSE
  )
  data.table::setorder(leaky, general_id)
  sentinel <- leaky[general_id %chin% c("10", "8", "9")]
  expect_false(any(sentinel$method == "no_match"))
  expect_true(all(!is.na(sentinel$cp2021_id_level_5)))
})

test_that("predict_cp5_knn ignores the columns the restrictor does not name", {
  f <- .esco5_fixture()
  # Container-shaped: both ESCO levels and both CP levels present at once.
  wide <- data.table::copy(f$postings)
  wide[, cp2021_id_level_4 := substr(cp2021_id_level_5, 1L, 7L)]
  narrow <- f$postings[, .SD, .SDcols = !"idesco_level_4"]

  a <- predict_cp5_knn(
    wide,
    f$skills,
    restrictor = "idesco_level_5",
    k = 3L,
    verbose = FALSE
  )
  b <- predict_cp5_knn(
    narrow,
    f$skills,
    restrictor = "idesco_level_5",
    k = 3L,
    verbose = FALSE
  )
  expect_identical(a, b)
})

test_that("predict_cp5_knn errors naming the missing restrictor column", {
  f <- .esco5_fixture()
  expect_error(
    predict_cp5_knn(
      f$postings[, .SD, .SDcols = !"idesco_level_5"],
      f$skills,
      restrictor = "idesco_level_5",
      verbose = FALSE
    ),
    "idesco_level_5"
  )
})

test_that("mode_tiebreak = 'code' is invariant under row order, 'emission' is not", {
  # A group with two CP codes at an exact count tie, plus an unlabeled row
  # carrying no skills so it falls through to the modal fallback -- which is
  # where the tie-break actually decides the answer.
  base <- data.table::data.table(
    general_id = as.character(1:5),
    idesco_level_4 = rep("1000", 5L),
    cp2021_id_level_4 = c("2.2.2.2", "2.2.2.2", "1.1.1.1", "1.1.1.1", NA),
    idsector = rep("C", 5L)
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:4, each = 2L)),
    escoskill_level_3 = rep(c("s1", "s2"), 4L)
  )
  flipped <- base[c(3L, 4L, 1L, 2L, 5L)]

  .run <- function(dt, tb) {
    .predict_cp_knn(
      dt,
      skills,
      target_col = "cp2021_id_level_4",
      restrictor_col = "idesco_level_4",
      mode_tiebreak = tb,
      k = 3L,
      verbose = FALSE
    )$cp2021_id_level_4
  }

  # The deterministic rule picks the smaller code either way.
  expect_identical(.run(base, "code"), "1.1.1.1")
  expect_identical(.run(flipped, "code"), "1.1.1.1")

  # The legacy rule follows whichever code the caller happened to list first.
  expect_identical(.run(base, "emission"), "2.2.2.2")
  expect_identical(.run(flipped, "emission"), "1.1.1.1")
})

test_that("the level-5 restrictor still routes no_match rows to the rescue", {
  f <- .unclassifiable_fixture()
  rescued <- predict_cp5_knn(
    f$postings,
    f$skills,
    restrictor = "idesco_level_5",
    rescue_no_match = TRUE,
    k = 3L,
    rescue_k = 3L,
    verbose = FALSE
  )
  data.table::setorder(rescued, general_id)
  sentinel <- rescued[general_id %chin% c("10", "8", "9")]
  # They carry skills, so the global k-NN claims them instead of abandoning them.
  expect_true(all(sentinel$method == "knn_global"))
  expect_true(all(!is.na(sentinel$cp2021_id_level_5)))
})


# 3i. build_esco_cp_crosswalk -----

test_that("build_esco_cp_crosswalk accounts for every code in the universe", {
  postings <- data.table::data.table(
    idesco_level_5 = c(
      rep("1000.1", 4L),
      rep("1000.2", 3L),
      "2000.1",
      "Unclassifiable"
    ),
    cp2021_id_level_5 = c(
      "1.1.1.1.1",
      "1.1.1.1.1",
      "1.1.1.1.2",
      NA,
      "2.2.2.2.0",
      "2.2.2.2.0",
      "2.2.2.2.0",
      # 2000.1 appears in postings but never carries a code
      NA,
      "3.3.3.3.0"
    )
  )
  universe <- data.table::data.table(
    idesco_level_5 = c("1000.1", "1000.2", "2000.1", "9999.9"),
    idesco_level_4 = c("1000", "1000", "2000", "9999")
  )
  cw <- build_esco_cp_crosswalk(postings, universe = universe, verbose = FALSE)

  # Every one of the four classification statuses is reachable, and the table
  # covers the whole domain rather than only what the data happened to show.
  expect_setequal(
    cw$groups$status,
    c("covered", "covered", "no_target", "absent", "unclassifiable")
  )
  expect_true(all(universe$idesco_level_5 %in% cw$groups$idesco_level_5))
  expect_identical(
    cw$groups[status == "absent", idesco_level_5],
    "9999.9"
  )
  expect_identical(
    cw$groups[status == "no_target", idesco_level_5],
    "2000.1"
  )

  # coverage_labelled is the share of the group that carries a code at all.
  expect_equal(cw$groups[idesco_level_5 == "1000.1", coverage_labelled], 0.75)
  expect_equal(cw$groups[idesco_level_5 == "2000.1", coverage_labelled], 0)

  # Candidates carry the derived CP4 parent and a well-formed rank ordering.
  expect_true(all(
    cw$candidates$cp2021_id_level_4 ==
      substr(cw$candidates$cp2021_id_level_5, 1L, 7L)
  ))
  expect_identical(
    cw$candidates[idesco_level_5 == "1000.1", rank],
    c(1L, 2L)
  )
  expect_equal(
    cw$candidates[idesco_level_5 == "1000.1", cum_share],
    c(2 / 3, 1)
  )
})

test_that("build_esco_cp_crosswalk excludes sentinel groups from candidates", {
  postings <- data.table::data.table(
    idesco_level_5 = c("Unclassifiable", "Unclassifiable", "1000.1"),
    cp2021_id_level_5 = c("3.3.3.3.0", "4.4.4.4.0", "1.1.1.1.1")
  )
  cw <- build_esco_cp_crosswalk(postings, verbose = FALSE)
  expect_false(any(cw$candidates$idesco_level_5 == "Unclassifiable"))
  expect_identical(
    cw$groups[status == "unclassifiable", n_postings],
    2L
  )
})

# The artefact and the model must agree, or the published crosswalk describes
# something other than what the coder actually does.
test_that("crosswalk rank-1 rows equal the engine's internal modal code", {
  set.seed(11)
  n <- 400L
  postings <- data.table::data.table(
    general_id = as.character(seq_len(n)),
    idesco_level_4 = sample(sprintf("%d", 1:12 * 100L), n, replace = TRUE),
    cp2021_id_level_4 = sample(
      sprintf("%d.%d.%d.%d", 1:4, 1L, 1L, 1:4),
      n,
      replace = TRUE
    )
  )
  # Leave a slice unlabelled so the engine has something to predict.
  postings[sample(n, 80L), cp2021_id_level_4 := NA_character_]
  skills <- data.table::data.table(
    general_id = as.character(rep(seq_len(n), each = 3L)),
    escoskill_level_3 = sample(sprintf("s%02d", 1:20), n * 3L, replace = TRUE)
  )

  cw <- build_esco_cp_crosswalk(
    postings,
    restrictor_col = "idesco_level_4",
    target_col = "cp2021_id_level_4",
    restrictor_na = character(0),
    verbose = FALSE
  )
  modal <- cw$candidates[rank == 1L][order(idesco_level_4)]

  # The engine's frequency fallback is exactly this modal code, so a group with
  # no usable skills must return it. Reproduce that path by giving the engine
  # the same labelled data with skills stripped from the unlabelled rows.
  test_ids <- postings[is.na(cp2021_id_level_4), general_id]
  pred <- .predict_cp_knn(
    postings,
    skills[!general_id %chin% test_ids],
    target_col = "cp2021_id_level_4",
    restrictor_col = "idesco_level_4",
    mode_tiebreak = "code",
    k = 3L,
    verbose = FALSE
  )
  pred <- merge(
    pred,
    postings[, .(general_id, idesco_level_4)],
    by = "general_id"
  )
  expect_true(all(pred$method == "frequency"))

  got <- unique(pred[, .(idesco_level_4, cp2021_id_level_4)])
  data.table::setorder(got, idesco_level_4)
  expect_identical(
    got$cp2021_id_level_4,
    modal[idesco_level_4 %chin% got$idesco_level_4, cp2021_id_level_4]
  )
})

test_that("strata_col flags a mode that only holds for the coded sample", {
  # Group G is coded almost entirely by source A, which does one job; the
  # uncoded rows are almost all source B, which does another. The naive mode
  # follows A, the reweighted mode follows B. This is the shape of the real
  # finding: 497 groups, 13.66% of unlabelled rows.
  postings <- data.table::data.table(
    idesco_level_5 = rep("G", 30L),
    source = c(rep("A", 12L), rep("B", 18L)),
    cp2021_id_level_5 = c(
      # source A: 10 coded, mostly X
      rep("1.1.1.1.1", 8L),
      rep("2.2.2.2.0", 2L),
      NA,
      NA,
      # source B: 4 coded, all Y -- and 14 uncoded, so B dominates the target
      rep("2.2.2.2.0", 4L),
      rep(NA_character_, 14L)
    )
  )

  plain <- build_esco_cp_crosswalk(postings, verbose = FALSE)
  expect_true(all(is.na(plain$groups$modal_stable)))
  expect_identical(plain$groups[idesco_level_5 == "G", code_modal], "1.1.1.1.1")

  wt <- build_esco_cp_crosswalk(
    postings,
    strata_col = "source",
    verbose = FALSE
  )
  g <- wt$groups[idesco_level_5 == "G"]
  expect_identical(g$code_modal, "1.1.1.1.1")
  expect_identical(g$code_modal_rw, "2.2.2.2.0")
  expect_false(g$modal_stable)
})

test_that("strata_col leaves a mode alone when coding is balanced", {
  postings <- data.table::data.table(
    idesco_level_5 = rep("G", 20L),
    source = rep(c("A", "B"), each = 10L),
    # Both sources agree, and both are half coded.
    cp2021_id_level_5 = rep(c(rep("1.1.1.1.1", 4L), NA, NA), length.out = 20L)
  )
  wt <- build_esco_cp_crosswalk(
    postings,
    strata_col = "source",
    verbose = FALSE
  )
  g <- wt$groups[idesco_level_5 == "G"]
  expect_true(g$modal_stable)
  expect_identical(g$code_modal, g$code_modal_rw)
})
