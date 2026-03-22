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


# 3. build_skill_prof_sparse -----

describe("build_skill_prof_sparse", {
  dt <- data.table::data.table(
    escoskill_level_3 = c("A", "A", "B", "B", "C"),
    cod_3 = c("P1", "P2", "P1", "P2", "P1"),
    N = c(10, 5, 3, 7, 2),
    balassa_index = c(1.2, 0.8, 0.5, 1.5, 0.9)
  )

  it("returns a dgCMatrix", {
    mat <- build_skill_prof_sparse(dt, value_col = "N")
    expect_s4_class(mat, "dgCMatrix")
  })

  it("has correct dimensions (n_skills x n_professions)", {
    mat <- build_skill_prof_sparse(dt, value_col = "N")
    n_skills <- data.table::uniqueN(dt$escoskill_level_3)
    n_profs <- data.table::uniqueN(dt$cod_3)
    expect_equal(nrow(mat), n_skills)
    expect_equal(ncol(mat), n_profs)
  })

  it("has dimnames matching unique skills and professions", {
    mat <- build_skill_prof_sparse(dt, value_col = "N")
    expect_true(all(sort(rownames(mat)) == sort(unique(dt$escoskill_level_3))))
    expect_true(all(sort(colnames(mat)) == sort(unique(dt$cod_3))))
  })

  it("populates cell values correctly", {
    mat <- build_skill_prof_sparse(dt, value_col = "N")
    for (i in seq_len(nrow(dt))) {
      expect_equal(
        mat[dt$escoskill_level_3[i], dt$cod_3[i]],
        dt$N[i],
        info = paste(
          "row",
          i,
          "skill",
          dt$escoskill_level_3[i],
          "prof",
          dt$cod_3[i]
        )
      )
    }
  })

  it("works with a different value column", {
    mat <- build_skill_prof_sparse(dt, value_col = "balassa_index")
    expect_equal(mat["A", "P1"], 1.2)
    expect_equal(mat["B", "P2"], 1.5)
  })

  it("fills missing skill-profession pairs with zero", {
    mat <- build_skill_prof_sparse(dt, value_col = "N")
    expect_equal(mat["C", "P2"], 0)
  })

  it("works with a custom prof_col", {
    dt_label <- data.table::data.table(
      escoskill_level_3 = c("A", "A", "B", "B", "C"),
      preferredLabel = c(
        "Prof Alpha",
        "Prof Beta",
        "Prof Alpha",
        "Prof Beta",
        "Prof Alpha"
      ),
      N = c(10, 5, 3, 7, 2)
    )
    mat <- build_skill_prof_sparse(
      dt_label,
      value_col = "N",
      prof_col = "preferredLabel"
    )
    expect_s4_class(mat, "dgCMatrix")
    expect_equal(nrow(mat), 3L)
    expect_equal(ncol(mat), 2L)
    expect_true(all(c("Prof Alpha", "Prof Beta") %in% colnames(mat)))
    expect_equal(mat["A", "Prof Alpha"], 10)
  })
})


# 4. compute_cosine_similarity -----

describe("compute_cosine_similarity", {
  it("returns 1 for identical columns", {
    mat <- matrix(c(1, 2, 3, 1, 2, 3), ncol = 2)
    sim <- compute_cosine_similarity(mat)
    expect_equal(sim[1, 2], 1, tolerance = 1e-10)
  })

  it("returns 0 for orthogonal columns", {
    mat <- matrix(c(1, 0, 0, 1), ncol = 2)
    sim <- compute_cosine_similarity(mat)
    expect_equal(sim[1, 2], 0, tolerance = 1e-10)
  })

  it("produces a symmetric matrix", {
    mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)
    sim <- compute_cosine_similarity(mat)
    expect_equal(sim, t(sim), tolerance = 1e-10)
  })

  it("has diagonal of 1 for non-zero columns", {
    mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)
    sim <- compute_cosine_similarity(mat)
    expect_equal(diag(sim), rep(1, 3), tolerance = 1e-10)
  })

  it("returns values in [-1, 1] range", {
    set.seed(42)
    mat <- matrix(rnorm(20), ncol = 4)
    sim <- compute_cosine_similarity(mat)
    expect_true(all(sim >= -1 - 1e-10 & sim <= 1 + 1e-10))
  })
})


# 5. compute_decomposed_distance -----

describe("compute_decomposed_distance", {
  build_decomp_data <- function() {
    # rows = skills, cols = professions
    mat <- matrix(
      c(1, 0, 2, 0, 3, 1, 1, 1, 0),
      nrow = 3,
      ncol = 3,
      byrow = TRUE,
      dimnames = list(c("S1", "S2", "S3"), c("P1", "P2", "P3"))
    )
    skill_groups <- data.table::data.table(
      escoskill_level_3 = c("S1", "S2", "S3"),
      group = c("G1", "G1", "G2")
    )
    list(mat = mat, skill_groups = skill_groups)
  }

  it("partial distances sum to total distance (additivity)", {
    td <- build_decomp_data()
    result <- compute_decomposed_distance(td$mat, td$skill_groups)

    total_from_parts <- Reduce("+", lapply(result$partial, as.matrix))
    total_direct <- as.matrix(result$total)

    expect_equal(total_from_parts, total_direct, tolerance = 1e-10)
  })

  it("contribution shares sum to approximately 1 for each pair", {
    td <- build_decomp_data()
    result <- compute_decomposed_distance(td$mat, td$skill_groups)

    contrib <- result$contribution
    pair_sums <- contrib[,
      list(total_share = sum(share)),
      by = list(prof_i, prof_j)
    ]

    for (s in pair_sums$total_share) {
      expect_equal(s, 1, tolerance = 1e-8)
    }
  })

  it("each partial dist has correct labels", {
    td <- build_decomp_data()
    result <- compute_decomposed_distance(td$mat, td$skill_groups)

    expected_labels <- sort(colnames(td$mat))
    for (nm in names(result$partial)) {
      partial_labels <- sort(attr(result$partial[[nm]], "Labels"))
      expect_equal(partial_labels, expected_labels)
    }
  })

  it("returns one partial dist per group", {
    td <- build_decomp_data()
    result <- compute_decomposed_distance(td$mat, td$skill_groups)

    group_names <- sort(unique(td$skill_groups$group))
    expect_equal(sort(names(result$partial)), group_names)
  })
})


# 6. compute_skill_similarity -----

describe("compute_skill_similarity", {
  build_skill_matrix <- function() {
    # S1: 4 professions, S2: 3, S3: 3, S4: 2, S5: 1
    Matrix::sparseMatrix(
      i = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5),
      j = c(1, 2, 3, 4, 1, 2, 3, 2, 3, 4, 1, 4, 3),
      x = rep(1, 13),
      dims = c(5, 4),
      dimnames = list(
        c("S1", "S2", "S3", "S4", "S5"),
        c("P1", "P2", "P3", "P4")
      )
    )
  }

  it("returns a square matrix", {
    mat <- build_skill_matrix()
    sim <- compute_skill_similarity(mat, min_professions = 1L)
    expect_equal(nrow(sim), ncol(sim))
  })

  it("has diagonal values of 1", {
    mat <- build_skill_matrix()
    sim <- compute_skill_similarity(mat, min_professions = 1L)
    expect_equal(unname(diag(sim)), rep(1, nrow(sim)), tolerance = 1e-10)
  })

  it("filters out skills appearing in fewer than min_professions", {
    mat <- build_skill_matrix()
    sim <- compute_skill_similarity(mat, min_professions = 3L)

    expect_equal(nrow(sim), 3L)
    expect_true(all(c("S1", "S2", "S3") %in% rownames(sim)))
    expect_false("S4" %in% rownames(sim))
    expect_false("S5" %in% rownames(sim))
  })

  it("dimensions reflect filtered skill count", {
    mat <- build_skill_matrix()

    sim_all <- compute_skill_similarity(mat, min_professions = 1L)
    expect_equal(nrow(sim_all), 5L)

    sim_filtered <- compute_skill_similarity(mat, min_professions = 3L)
    expect_equal(nrow(sim_filtered), 3L)
  })

  it("produces a symmetric matrix", {
    mat <- build_skill_matrix()
    sim <- compute_skill_similarity(mat, min_professions = 1L)
    expect_equal(sim, t(sim), tolerance = 1e-10)
  })
})
