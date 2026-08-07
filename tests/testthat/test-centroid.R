# Tests for centroid.R -----

.centroid_fixture <- function() {
  # Group A: three candidates with DISTINCT class sizes, so the modal rule has
  # no tie to break and the shrinkage identity below is exact rather than
  # asymptotic. Group B: one candidate. Group C: a sentinel restrictor.
  postings <- data.table::data.table(
    general_id = as.character(1:22),
    idesco_level_5 = c(
      rep("1000.1", 12L), # 6 / 4 / 2
      rep("2000.1", 6L), # single candidate
      rep("Unclassifiable", 4L)
    ),
    cp2021_id_level_5 = c(
      rep("1.1.1.1.1", 6L),
      rep("1.1.1.1.2", 4L),
      rep("1.1.1.1.3", 2L),
      rep("2.2.2.2.0", 6L),
      rep("3.3.3.3.0", 4L)
    ),
    idsector = rep(c("C", "F"), 11L)
  )
  skills <- data.table::data.table(
    general_id = as.character(rep(1:22, each = 3L)),
    escoskill_level_3 = c(
      rep(c("s1", "s2", "s3"), 6L), # candidate 1
      rep(c("s2", "s3", "s4"), 4L), # candidate 2
      rep(c("s5", "s6", "s7"), 2L), # candidate 3
      rep(c("s8", "s9", "s10"), 6L), # group B
      rep(c("s1", "s5", "s9"), 4L) # sentinel group
    )
  )
  list(postings = postings, skills = skills)
}

# The modal cascade the identity must reproduce, written exactly as
# skillviz_workflow/run_cp5_centroid.R writes it.
.modal_cascade <- function(train, test) {
  t5 <- train[
    idesco_level_5 != "Unclassifiable",
    .N,
    by = list(grp = idesco_level_5, code = cp2021_id_level_5)
  ]
  data.table::setorder(t5, grp, -N, code)
  g <- t5[, list(code = code[1L], n_cand = .N), by = grp]
  out <- data.table::data.table(
    general_id = test$general_id,
    grp = test$idesco_level_5
  )
  out[grp == "Unclassifiable", grp := NA_character_]
  out <- g[out, on = "grp"]
  out[, list(general_id, modal = code)]
}

test_that("build_cp_profiles indexes one row per observed pair", {
  f <- .centroid_fixture()
  p <- build_cp_profiles(f$postings, f$skills, verbose = FALSE)

  expect_s3_class(p, "cp_profiles")
  # The sentinel group contributes no candidates.
  expect_setequal(p$pairs$grp, c("1000.1", "2000.1"))
  expect_identical(nrow(p$pairs), 4L)
  expect_identical(
    p$pairs[grp == "1000.1"][order(code), n_c],
    c(6L, 4L, 2L)
  )
  # The model is the profiles, not the training data: no announcement survives.
  expect_false(any(grepl("general_id", names(p$pairs))))
})

test_that("min_support drops thin candidates", {
  f <- .centroid_fixture()
  p <- build_cp_profiles(
    f$postings,
    f$skills,
    min_support = 5L,
    verbose = FALSE
  )
  expect_setequal(
    p$pairs$code,
    c("1.1.1.1.1", "2.2.2.2.0")
  )
})

test_that("every distance returns one scored row per posting", {
  f <- .centroid_fixture()
  p <- build_cp_profiles(
    f$postings,
    f$skills,
    weighting = c("none", "idf", "balassa"),
    verbose = FALSE
  )
  for (d in c(
    "multinomial_nb",
    "complement_nb",
    "bernoulli_nb",
    "hellinger",
    "cosine",
    "l2",
    "l1",
    "chisq",
    "jsd",
    "dice",
    "tfidf_cosine",
    "balassa_cosine"
  )) {
    res <- predict_cp5_centroid(
      p,
      f$postings,
      f$skills,
      distance = d,
      verbose = FALSE
    )
    expect_identical(nrow(res), nrow(f$postings), info = d)
    expect_named(
      res,
      c("general_id", "cp2021_id_level_5", "confidence", "method"),
      info = d
    )
    expect_true(all(res$confidence >= 0 & res$confidence <= 1), info = d)
  }
})

test_that("the cascade routes single-candidate and sentinel groups", {
  f <- .centroid_fixture()
  p <- build_cp_profiles(f$postings, f$skills, verbose = FALSE)
  res <- predict_cp5_centroid(p, f$postings, f$skills, verbose = FALSE)
  res <- merge(
    res,
    f$postings[, list(general_id, grp = idesco_level_5)],
    by = "general_id"
  )

  expect_true(all(res[grp == "2000.1", method] == "single_candidate"))
  expect_true(all(res[grp == "2000.1", confidence] == 1))
  # No ESCO code at any level: nothing to restrict to.
  expect_true(all(res[grp == "Unclassifiable", method] == "no_match"))
  expect_true(all(is.na(res[grp == "Unclassifiable", cp2021_id_level_5])))
  expect_true(all(res[grp == "1000.1", method] == "centroid"))
})

# This is the contract that makes the shrinkage falsifiable, and the harness
# refuses to report any accuracy until it holds.
test_that("shrink_beta to infinity reproduces the modal cascade", {
  f <- .centroid_fixture()
  modal <- .modal_cascade(f$postings, f$postings)

  for (d in c("multinomial_nb", "cosine", "hellinger", "bernoulli_nb")) {
    p <- build_cp_profiles(
      f$postings,
      f$skills,
      shrink_beta = 1e18,
      min_support = 1L,
      sector = FALSE,
      verbose = FALSE
    )
    res <- predict_cp5_centroid(
      p,
      f$postings,
      f$skills,
      distance = d,
      prior_weight = 1,
      verbose = FALSE
    )
    cmp <- merge(res, modal, by = "general_id")
    expect_identical(cmp$cp2021_id_level_5, cmp$modal, info = d)
  }
})

test_that("shrink_beta = 0 lets the skills decide instead", {
  f <- .centroid_fixture()
  # Candidate 3 is the smallest class but owns s5/s6/s7 outright, so a query
  # made of those must beat the class-size prior once shrinkage is off.
  q <- data.table::data.table(
    general_id = "q1",
    idesco_level_5 = "1000.1",
    cp2021_id_level_5 = NA_character_,
    idsector = "C"
  )
  qs <- data.table::data.table(
    general_id = rep("q1", 3L),
    escoskill_level_3 = c("s5", "s6", "s7")
  )
  p0 <- build_cp_profiles(
    f$postings,
    f$skills,
    shrink_beta = 0,
    smooth_alpha = 0.01,
    verbose = FALSE
  )
  expect_identical(
    predict_cp5_centroid(
      p0,
      q,
      qs,
      distance = "cosine",
      prior_weight = 0,
      verbose = FALSE
    )$cp2021_id_level_5,
    "1.1.1.1.3"
  )
  # And a large enough prior overrides it again.
  expect_identical(
    predict_cp5_centroid(
      p0,
      q,
      qs,
      distance = "cosine",
      prior_weight = 100,
      verbose = FALSE
    )$cp2021_id_level_5,
    "1.1.1.1.1"
  )
})

test_that("a skill-less announcement is decided by the prior, not the model", {
  f <- .centroid_fixture()
  p <- build_cp_profiles(f$postings, f$skills, verbose = FALSE)
  q <- data.table::data.table(
    general_id = "q2",
    idesco_level_5 = "1000.1",
    idsector = "C"
  )
  res <- predict_cp5_centroid(
    p,
    q,
    f$skills[0L],
    distance = "cosine",
    verbose = FALSE
  )
  # "frequency", not "centroid": it matches predict_cp5_knn's convention, so the
  # two models' model-decided populations stay comparable.
  expect_identical(res$method, "frequency")
  expect_identical(res$cp2021_id_level_5, "1.1.1.1.1")
})

test_that("a distance errors when its weighting was not built", {
  f <- .centroid_fixture()
  p <- build_cp_profiles(
    f$postings,
    f$skills,
    weighting = "none",
    verbose = FALSE
  )
  expect_error(
    predict_cp5_centroid(p, f$postings, f$skills, distance = "tfidf_cosine"),
    "idf"
  )
})

test_that("the sector term needs idsector and changes nothing without it", {
  f <- .centroid_fixture()
  expect_error(
    build_cp_profiles(
      f$postings[, !"idsector"],
      f$skills,
      sector = TRUE,
      verbose = FALSE
    ),
    "idsector"
  )
  p <- build_cp_profiles(f$postings, f$skills, sector = TRUE, verbose = FALSE)
  expect_identical(nrow(p$sector_counts), nrow(p$pairs))
})
