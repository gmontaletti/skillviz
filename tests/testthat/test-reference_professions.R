# Tests for reference_professions.R -----

# 1. Synthetic fixtures -----

# Eight postings: CP code "1.1.1.1.0" is co-coded 3 times with ESCO
# "1000.1" and once with "1000.2"; "2.2.2.2.0" twice with "2000.1" and once
# with an unclassifiable ESCO code; posting 8 has no CP code.
.rp_fixture <- function() {
  postings <- data.table::data.table(
    general_id = 1:8,
    cp = c(rep("1.1.1.1.0", 4), rep("2.2.2.2.0", 3), ""),
    esco = c(
      "1000.1",
      "1000.1",
      "1000.1",
      "1000.2",
      "2000.1",
      "2000.1",
      "Unclassifiable",
      "2000.2"
    ),
    source = rep(c("s1", "s2"), 4)
  )
  skills <- data.table::data.table(
    general_id = rep(1:8, each = 2),
    idescoskill_level_3 = c(
      "a",
      "b",
      "a",
      "b",
      "a",
      "c",
      "a",
      "b",
      "d",
      "e",
      "d",
      "e",
      "d",
      "f",
      "d",
      "e"
    )
  )
  ucp <- data.table::data.table(
    code = c("1.1.1.1.0", "2.2.2.2.0", "3.3.3.3.0"),
    label = c("cp uno", "cp due", "cp tre"),
    parent = c("1.1.1.1", "2.2.2.2", "3.3.3.3")
  )
  uesco <- data.table::data.table(
    code = c("1000.1", "1000.2", "2000.1", "2000.2", "3000.1"),
    label = c("esco a", "esco b", "esco c", "esco d", "esco e"),
    parent = c("1000", "1000", "2000", "2000", "3000")
  )
  list(postings = postings, skills = skills, ucp = ucp, uesco = uesco)
}

# 2. skill_idf() -----

test_that("skill_idf uses document frequency over postings with skills", {
  skills <- data.table::data.table(
    general_id = c(1, 1, 1, 2, 3),
    idescoskill_level_3 = c("a", "a", "b", "a", "c")
  )
  idf <- skill_idf(skills)
  expect_identical(names(idf), c("a", "b", "c"))
  expect_identical(attr(idf, "n_postings"), 3L)
  expect_equal(
    as.numeric(idf),
    c(log(3 / 2), log(3), log(3)),
    tolerance = 1e-12
  )
  idf2 <- skill_idf(skills, ids = c(1, 2))
  expect_identical(names(idf2), c("a", "b"))
  expect_equal(as.numeric(idf2), c(0, log(2)), tolerance = 1e-12)
})

test_that("skill_idf validates its input", {
  expect_error(skill_idf(list(a = 1)), "data.frame")
  expect_error(
    skill_idf(data.table::data.table(general_id = 1)),
    "missing required columns"
  )
  expect_error(
    skill_idf(
      data.table::data.table(general_id = 1, idescoskill_level_3 = "a"),
      ids = 2
    ),
    "no posting"
  )
})

# 3. skill_profiles() -----

test_that("skill_profiles returns coverage shares weighted by IDF", {
  fx <- .rp_fixture()
  p <- skill_profiles(fx$postings, fx$skills, "cp", weighting = "none")
  expect_s4_class(p, "dgCMatrix")
  expect_identical(rownames(p), c("1.1.1.1.0", "2.2.2.2.0"))
  expect_identical(colnames(p), c("a", "b", "c", "d", "e", "f"))
  expect_equal(p["1.1.1.1.0", "a"], 1, tolerance = 1e-12)
  expect_equal(p["1.1.1.1.0", "b"], 0.75, tolerance = 1e-12)
  expect_equal(p["2.2.2.2.0", "f"], 1 / 3, tolerance = 1e-12)
  expect_identical(
    attr(p, "n_postings"),
    c("1.1.1.1.0" = 4L, "2.2.2.2.0" = 3L)
  )

  idf <- skill_idf(fx$skills)
  pw <- skill_profiles(fx$postings, fx$skills, "cp", idf = idf)
  expect_equal(
    pw["1.1.1.1.0", "b"],
    0.75 * idf[["b"]],
    tolerance = 1e-12
  )
  expect_identical(colnames(pw), names(idf))
})

test_that("skill_profiles drops missing codes and applies min_postings", {
  fx <- .rp_fixture()
  p <- skill_profiles(fx$postings, fx$skills, "esco", min_postings = 2)
  expect_false("Unclassifiable" %in% names(attr(p, "n_postings")))
  expect_identical(rownames(p), c("1000.1", "2000.1"))
  expect_identical(
    attr(p, "n_postings")[c("1000.2", "2000.2")],
    c("1000.2" = 1L, "2000.2" = 1L)
  )
})

test_that("skill_profiles rejects postings with two codes and bad input", {
  fx <- .rp_fixture()
  bad <- rbind(
    fx$postings,
    data.table::data.table(
      general_id = 1L,
      cp = "9.9.9.9.0",
      esco = "1000.1",
      source = "s1"
    )
  )
  expect_error(skill_profiles(bad, fx$skills, "cp"), "more than one code")
  expect_error(
    skill_profiles(fx$postings, fx$skills, "cp", min_postings = 0),
    "min_postings"
  )
  expect_error(
    skill_profiles(fx$postings, fx$skills, "nope"),
    "missing required columns"
  )
  expect_error(
    skill_profiles(fx$postings, fx$skills, "cp", idf = c(1, 2)),
    "idf"
  )
})

# 4. build_reference_professions() -----

test_that("frequency references are the mode, with one row per universe code", {
  fx <- .rp_fixture()
  r <- build_reference_professions(
    fx$postings,
    fx$skills,
    "cp",
    "esco",
    fx$ucp,
    fx$uesco,
    n_min = 2,
    min_postings = 1
  )
  expect_s3_class(r, "data.table")
  expect_identical(r$code, fx$ucp$code)
  expect_identical(anyDuplicated(r$code), 0L)
  expect_identical(r$method, c("frequenza", "frequenza", "nessuno"))
  expect_identical(r$ref_code, c("1000.1", "2000.1", NA))
  expect_identical(r$ref_label, c("esco a", "esco c", NA))
  expect_identical(r$ref_parent, c("1000", "2000", NA))
  expect_identical(r$n_cocoded, c(4L, 2L, 0L))
  expect_identical(r$n_postings, c(4L, 3L, 0L))
  expect_identical(r$reason[3L], "nessun_annuncio")
  expect_equal(r$modal_share[1L], 0.75, tolerance = 1e-12)
  expect_identical(attr(r, "n_min"), 2L)
})

test_that("the frequency reference equals code_modal of the crosswalk", {
  fx <- .rp_fixture()
  r <- build_reference_professions(
    fx$postings,
    fx$skills,
    "esco",
    "cp",
    fx$uesco,
    fx$ucp,
    n_min = 1,
    min_postings = 1
  )
  pp <- data.table::copy(fx$postings)
  pp[cp == "", cp := NA]
  cw <- build_esco_cp_crosswalk(
    pp,
    restrictor_col = "esco",
    target_col = "cp",
    verbose = FALSE
  )$groups
  fr <- r[method == "frequenza"]
  expect_identical(
    fr$ref_code,
    cw$code_modal[match(fr$code, cw$esco)]
  )
})

test_that("ties in the mode are resolved on the code and flagged", {
  postings <- data.table::data.table(
    general_id = 1:4,
    cp = c("1.1.1.1.0", "1.1.1.1.0", "1.1.1.1.0", "1.1.1.1.0"),
    esco = c("1000.2", "1000.2", "1000.1", "1000.1")
  )
  skills <- data.table::data.table(
    general_id = 1:4,
    idescoskill_level_3 = "a"
  )
  r <- build_reference_professions(
    postings,
    skills,
    "cp",
    "esco",
    data.table::data.table(code = "1.1.1.1.0"),
    data.table::data.table(code = c("1000.1", "1000.2")),
    n_min = 1,
    min_postings = 1
  )
  expect_identical(r$ref_code, "1000.1")
  expect_true(r$modal_tied)
})

test_that("codes below n_min or never co-coded fall back on similarity", {
  fx <- .rp_fixture()
  r <- build_reference_professions(
    fx$postings,
    fx$skills,
    "esco",
    "cp",
    fx$uesco,
    fx$ucp,
    n_min = 3,
    min_postings = 1
  )
  expect_identical(r$code, fx$uesco$code)
  expect_identical(
    r$method,
    c("frequenza", "similarità", "similarità", "similarità", "nessuno")
  )
  expect_identical(
    r$reason,
    c(
      "annunci_in_comune",
      "annunci_in_comune_sotto_soglia",
      "annunci_in_comune_sotto_soglia",
      "nessun_annuncio_in_comune",
      "nessun_annuncio"
    )
  )
  # ESCO 2000.2 is never co-coded: its skills (d, e) point to the CP code
  # whose postings carry d and e.
  expect_identical(r[code == "2000.2", ref_code], "2.2.2.2.0")
  expect_identical(r[code == "2000.2", sim_code], "2.2.2.2.0")
  expect_true(all(r[method != "nessuno", ref_in_universe]))
  expect_equal(
    r[code == "2000.2", cosine_ref],
    r[code == "2000.2", sim_cosine],
    tolerance = 1e-12
  )
  expect_true(all(r$sim_cosine >= r$sim2_cosine, na.rm = TRUE))
})

test_that("codes without a usable profile get no reference, with the reason", {
  fx <- .rp_fixture()
  r <- build_reference_professions(
    fx$postings,
    fx$skills,
    "esco",
    "cp",
    fx$uesco,
    fx$ucp,
    n_min = 10,
    min_postings = 2
  )
  # 1000.2 and 2000.2 have one posting each: below min_postings.
  expect_identical(r[code == "1000.2", method], "nessuno")
  expect_identical(r[code == "1000.2", reason], "profilo_insufficiente")
  expect_identical(r[code == "2000.2", reason], "profilo_insufficiente")
  expect_identical(r[code == "1000.1", method], "similarità")
  expect_true(all(is.na(r[method == "nessuno", ref_code])))
})

test_that("doubly-coded totals match in the two directions", {
  fx <- .rp_fixture()
  a <- build_reference_professions(
    fx$postings,
    fx$skills,
    "cp",
    "esco",
    fx$ucp,
    fx$uesco,
    n_min = 2,
    min_postings = 1
  )
  b <- build_reference_professions(
    fx$postings,
    fx$skills,
    "esco",
    "cp",
    fx$uesco,
    fx$ucp,
    n_min = 2,
    min_postings = 1
  )
  expect_identical(sum(a$n_cocoded), sum(b$n_cocoded))
  expect_identical(sum(a$n_cocoded), 6L)
})

test_that("precomputed profiles give the same table", {
  fx <- .rp_fixture()
  idf <- skill_idf(fx$skills, ids = fx$postings$general_id)
  pc <- skill_profiles(fx$postings, fx$skills, "cp", idf = idf)
  pe <- skill_profiles(fx$postings, fx$skills, "esco", idf = idf)
  a <- build_reference_professions(
    fx$postings,
    fx$skills,
    "cp",
    "esco",
    fx$ucp,
    fx$uesco,
    n_min = 5,
    min_postings = 1
  )
  b <- build_reference_professions(
    fx$postings,
    NULL,
    "cp",
    "esco",
    fx$ucp,
    fx$uesco,
    n_min = 5,
    min_postings = 1,
    profiles_from = pc,
    profiles_to = pe
  )
  expect_identical(a$method, b$method)
  expect_identical(a$ref_code, b$ref_code)
  expect_equal(a$sim_cosine, b$sim_cosine, tolerance = 1e-12)
})

test_that("strata_col fills modal_stable", {
  fx <- .rp_fixture()
  r <- build_reference_professions(
    fx$postings,
    fx$skills,
    "esco",
    "cp",
    fx$uesco,
    fx$ucp,
    n_min = 1,
    min_postings = 1,
    strata_col = "source"
  )
  expect_true(is.logical(r$modal_stable))
  expect_true("modal_stable" %in% names(r))
})

test_that("build_reference_professions validates its input", {
  fx <- .rp_fixture()
  expect_error(
    build_reference_professions(
      fx$postings,
      fx$skills,
      "cp",
      "cp",
      fx$ucp,
      fx$uesco,
      n_min = 1
    ),
    "must differ"
  )
  expect_error(
    build_reference_professions(
      fx$postings,
      fx$skills,
      "cp",
      "esco",
      fx$ucp,
      fx$uesco,
      n_min = 0
    ),
    "n_min"
  )
  expect_error(
    build_reference_professions(
      fx$postings,
      NULL,
      "cp",
      "esco",
      fx$ucp,
      fx$uesco,
      n_min = 1
    ),
    "skills"
  )
  expect_error(
    build_reference_professions(
      fx$postings,
      fx$skills,
      "cp",
      "esco",
      data.table::data.table(code = c("a", "a")),
      fx$uesco,
      n_min = 1
    ),
    "unique"
  )
  expect_error(
    build_reference_professions(
      fx$postings,
      fx$skills,
      "cp",
      "esco",
      list(),
      fx$uesco,
      n_min = 1
    ),
    "universe_from"
  )
})

# 5. calibrate_reference_threshold() -----

.rp_calibration_data <- function() {
  # "A": 90% x; "B": 55% y, 45% z; "C": below n_large.
  data.table::data.table(
    from = c(rep("A", 300), rep("B", 300), rep("C", 50)),
    to = c(
      rep(c("x", "y"), c(270, 30)),
      rep(c("y", "z"), c(165, 135)),
      rep("x", 50)
    )
  )
}

test_that("calibration measures subsample agreement with the full mode", {
  d <- .rp_calibration_data()
  sim <- data.table::data.table(
    code = c("A", "B", "C"),
    sim_code = c("x", "z", "x")
  )
  cal <- calibrate_reference_threshold(
    d,
    "from",
    "to",
    sim,
    k_grid = c(1, 5, 50),
    n_rep = 200,
    seed = 7
  )
  expect_identical(cal$codes$code, c("A", "B"))
  expect_identical(cal$codes$code_modal, c("x", "y"))
  expect_identical(cal$curve$k, c(1L, 5L, 50L))
  expect_identical(cal$curve$n_codes, c(2L, 2L, 2L))
  expect_equal(cal$similarity_agreement, 0.5, tolerance = 1e-12)
  # k = 1 agrees with the mode with probability 0.9 for A and 0.55 for B.
  expect_equal(cal$curve$agreement[1L], (0.9 + 0.55) / 2, tolerance = 0.06)
  expect_true(cal$curve$agreement[3L] > 0.85)
  expect_identical(cal$n_min, 1L)
})

test_that("calibration is reproducible and restores the RNG", {
  d <- .rp_calibration_data()
  sim <- data.table::data.table(code = c("A", "B"), sim_code = c("x", "z"))
  set.seed(42)
  before <- stats::runif(1)
  set.seed(42)
  a <- calibrate_reference_threshold(d, "from", "to", sim, n_rep = 20)
  after <- stats::runif(1)
  b <- calibrate_reference_threshold(d, "from", "to", sim, n_rep = 20)
  expect_identical(before, after)
  expect_identical(a$codes, b$codes)
})

test_that("calibration returns NA when no k beats similarity", {
  d <- .rp_calibration_data()
  sim <- data.table::data.table(code = c("A", "B"), sim_code = c("x", "y"))
  cal <- calibrate_reference_threshold(
    d,
    "from",
    "to",
    sim,
    k_grid = c(1, 2),
    n_rep = 50
  )
  expect_equal(cal$similarity_agreement, 1, tolerance = 1e-12)
  expect_true(is.na(cal$n_min))
})

test_that("codes without a similarity reference are excluded and listed", {
  d <- .rp_calibration_data()
  sim <- data.table::data.table(code = "A", sim_code = "x")
  cal <- calibrate_reference_threshold(d, "from", "to", sim, n_rep = 10)
  expect_identical(cal$codes$included, c(TRUE, FALSE))
  expect_identical(unique(cal$curve$n_codes), 1L)
})

test_that("calibrate_reference_threshold validates its input", {
  d <- .rp_calibration_data()
  sim <- data.table::data.table(code = "A", sim_code = "x")
  expect_error(
    calibrate_reference_threshold(d, "from", "to", sim, k_grid = 0),
    "k_grid"
  )
  expect_error(
    calibrate_reference_threshold(d, "from", "to", sim, k_grid = 500),
    "n_large"
  )
  expect_error(
    calibrate_reference_threshold(d, "from", "to", sim, n_large = 1000),
    "no code"
  )
  expect_error(
    calibrate_reference_threshold(d, "from", "to", data.frame(code = "A")),
    "missing required columns"
  )
})

# 6. transfer_skill_profile() -----

test_that("the transfer copies the profile of the reference", {
  fx <- .rp_fixture()
  pe <- skill_profiles(fx$postings, fx$skills, "esco", min_postings = 1)
  r <- data.table::data.table(
    code = c("1.1.1.1.0", "2.2.2.2.0", "3.3.3.3.0"),
    ref_code = c("1000.1", "2000.1", NA)
  )
  tr <- transfer_skill_profile(pe, r)
  expect_identical(rownames(tr), c("1.1.1.1.0", "2.2.2.2.0"))
  expect_identical(colnames(tr), colnames(pe))
  expect_identical(
    attr(tr, "ref_code"),
    c("1.1.1.1.0" = "1000.1", "2.2.2.2.0" = "2000.1")
  )
  expect_equal(
    as.matrix(tr)["1.1.1.1.0", ],
    as.matrix(pe)["1000.1", ],
    tolerance = 1e-12
  )
  # Identity: a code that is its own reference keeps its profile.
  self <- data.table::data.table(code = rownames(pe), ref_code = rownames(pe))
  expect_equal(
    as.matrix(transfer_skill_profile(pe, self)),
    as.matrix(pe),
    tolerance = 1e-12
  )
})

test_that("transfer_skill_profile validates its input", {
  m <- Matrix::Matrix(1, 1, 1, sparse = TRUE, dimnames = list("a", "s"))
  expect_error(transfer_skill_profile(1:3, data.frame()), "matrix")
  expect_error(
    transfer_skill_profile(m, data.frame(code = c("x", "x"), ref_code = "a")),
    "unique"
  )
  expect_error(transfer_skill_profile(m, data.frame(code = "x")), "missing")
})

# 7. validate_profile_transfer() -----

test_that("validation compares the transfer with the two baselines", {
  observed <- Matrix::Matrix(
    rbind(
      c(1, 0, 0, 0),
      c(0.8, 0.2, 0, 0),
      c(0, 1, 0, 0),
      c(0, 0, 1, 0)
    ),
    sparse = TRUE,
    dimnames = list(c("A1", "A2", "A3", "B1"), c("s1", "s2", "s3", "s4"))
  )
  attr(observed, "n_postings") <- c(A1 = 10, A2 = 10, A3 = 30, B1 = 5)
  # A perfect transfer for A1 and A2, a wrong one for B1.
  transferred <- Matrix::Matrix(
    rbind(c(1, 0, 0, 0), c(0.8, 0.2, 0, 0), c(0, 0, 0, 1)),
    sparse = TRUE,
    dimnames = list(c("A1", "A2", "B1"), c("s1", "s2", "s3", "s4"))
  )
  parent <- data.frame(
    code = c("A1", "A2", "A3", "B1"),
    parent = c("A", "A", "A", "B")
  )
  v <- validate_profile_transfer(transferred, observed, parent)
  expect_identical(v$codes$code, c("A1", "A2", "B1"))
  expect_equal(v$codes$cosine_transfer, c(1, 1, 0), tolerance = 1e-12)
  # Parent of A1 without A1: (10 * A2 + 30 * A3) / 40 = (0.2, 0.8, 0, 0).
  expect_equal(
    v$codes$cosine_parent[1L],
    0.2 / sqrt(0.2^2 + 0.8^2),
    tolerance = 1e-12
  )
  # B1 is alone in its group: no baseline.
  expect_true(is.na(v$codes$cosine_parent[3L]))
  expect_true(is.na(v$codes$sibling_code[3L]))
  expect_true(all(v$codes$sibling_code[1:2] %in% c("A1", "A2", "A3")))
  expect_false(any(v$codes$sibling_code[1:2] == v$codes$code[1:2]))
  expect_identical(v$summary$measure, c("transfer", "parent", "sibling"))
  expect_identical(v$summary$n, c(3L, 2L, 2L))
  expect_identical(unique(v$summary$n_common), 2L)
  expect_equal(unname(v$share_better[["parent"]]), 1, tolerance = 1e-12)
})

test_that("validate_profile_transfer validates its input", {
  m <- Matrix::Matrix(1, 1, 1, sparse = TRUE, dimnames = list("a", "s"))
  expect_error(validate_profile_transfer(1, m, data.frame()), "matrix")
  expect_error(validate_profile_transfer(m, m, list()), "data.frame")
  expect_error(
    validate_profile_transfer(m, m, data.frame(code = "a")),
    "missing required columns"
  )
})
