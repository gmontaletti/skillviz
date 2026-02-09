# Tests for relevance.R -----

# 1. compute_balassa_index -----

test_that("compute_balassa_index returns expected columns and types", {
  skills <- data.table::data.table(
    preferredLabel = c(
      rep("Analyst", 5),
      rep("Engineer", 3),
      rep("Designer", 2)
    ),
    escoskill_level_3 = c(
      "SQL",
      "Python",
      "Excel",
      "SQL",
      "Python",
      "Python",
      "CAD",
      "CAD",
      "Excel",
      "CAD"
    )
  )

  result <- compute_balassa_index(skills)

  expect_s3_class(result, "data.table")
  expect_named(
    result,
    c(
      "preferredLabel",
      "escoskill_level_3",
      "N",
      "tutte_professioni",
      "tutte_le_skills_della_professione",
      "tutte_le_ricorrenze",
      "balassa_index",
      "specializzazione"
    ),
    ignore.order = TRUE
  )
  expect_type(result$balassa_index, "double")
  expect_true(all(result$specializzazione %in% c("specifica", "non specifica")))
})

test_that("compute_balassa_index computes correct values for known data", {
  # 3 professions, 5 skills with known expected values
  # Analyst:  SQL(3), Python(2)          -> total 5

  # Engineer: Python(1), CAD(2), Java(1) -> total 4
  # Designer: Excel(1)                   -> total 1
  # Total across all: 10
  skills <- data.table::data.table(
    preferredLabel = c(
      rep("Analyst", 5),
      rep("Engineer", 4),
      "Designer"
    ),
    escoskill_level_3 = c(
      "SQL",
      "SQL",
      "SQL",
      "Python",
      "Python",
      "Python",
      "CAD",
      "CAD",
      "Java",
      "Excel"
    )
  )

  result <- compute_balassa_index(skills)

  # SQL only in Analyst: B = (3/5) / (3/10) = 0.6/0.3 = 2.0
  sql_analyst <- result[
    preferredLabel == "Analyst" & escoskill_level_3 == "SQL",
    balassa_index
  ]
  expect_equal(sql_analyst, 2.0)

  # Excel only in Designer: B = (1/1) / (1/10) = 1/0.1 = 10.0
  excel_designer <- result[
    preferredLabel == "Designer" & escoskill_level_3 == "Excel",
    balassa_index
  ]
  expect_equal(excel_designer, 10.0)

  # Python in Analyst: B = (2/5) / (3/10) = 0.4/0.3 = 1.333...
  python_analyst <- result[
    preferredLabel == "Analyst" & escoskill_level_3 == "Python",
    balassa_index
  ]
  expect_equal(python_analyst, 4 / 3, tolerance = 1e-10)
})

test_that("compute_balassa_index specializzazione label is correct", {
  skills <- data.table::data.table(
    preferredLabel = c("A", "A", "B", "B"),
    escoskill_level_3 = c("X", "Y", "X", "Y")
  )

  result <- compute_balassa_index(skills)

  # Perfectly uniform distribution: all Balassa = 1.0
  expect_true(all(result$balassa_index == 1.0))
  expect_true(all(result$specializzazione == "specifica"))
})

test_that("compute_balassa_index errors on missing columns", {
  bad_dt <- data.table::data.table(col_a = 1:3)

  expect_error(
    compute_balassa_index(bad_dt),
    "missing required columns"
  )
})

# 2. compute_skill_diffusion -----

test_that("compute_skill_diffusion returns expected structure", {
  skills_by_profession <- data.table::data.table(
    escoskill_level_3 = c("A", "B", "C", "A", "B", "C", "A", "C"),
    N = c(100L, 50L, 10L, 80L, 40L, 5L, 90L, 3L)
  )

  result <- compute_skill_diffusion(skills_by_profession)

  expect_s3_class(result, "data.table")
  expect_named(result, c("escoskill_level_3", "N", "tf", "idf", "diffusione"))
  expect_type(result$tf, "double")
  expect_type(result$idf, "double")
  expect_true(all(result$diffusione %in% c("alta", "centrale", "minima")))
})

test_that("compute_skill_diffusion TF sums to 100", {
  skills_by_profession <- data.table::data.table(
    escoskill_level_3 = c("A", "B", "C"),
    N = c(100L, 200L, 300L)
  )

  result <- compute_skill_diffusion(skills_by_profession)

  expect_equal(sum(result$tf), 100, tolerance = 1e-10)
})

test_that("compute_skill_diffusion errors on missing columns", {
  bad_dt <- data.table::data.table(skill = "A", count = 10L)

  expect_error(
    compute_skill_diffusion(bad_dt),
    "missing required columns"
  )
})

# 3. build_skillist -----

test_that("build_skillist returns expected columns and types", {
  skills <- data.table::data.table(
    escoskill_level_3 = c("A", "A", "B", "B", "C"),
    esco_v0101_reusetype = c(
      "sector-specific",
      "sector-specific",
      "transversal",
      "transversal",
      "occupation-specific"
    ),
    pillar_softskills = c(0L, 0L, 1L, 1L, 0L),
    esco_v0101_ict = c(1L, 1L, 0L, 0L, 0L),
    esco_v0101_green = c(0L, 0L, 0L, 0L, 1L),
    esco_v0101_language = c(0L, 0L, 0L, 0L, 0L)
  )

  diffusion <- data.table::data.table(
    escoskill_level_3 = c("A", "B", "C"),
    N = c(100L, 80L, 20L),
    tf = c(50.0, 40.0, 10.0),
    idf = c(0.5, 1.0, 2.0),
    diffusione = c("alta", "centrale", "minima")
  )

  result <- build_skillist(skills, diffusion)

  expect_s3_class(result, "data.table")
  expect_true("tipo" %in% names(result))
  expect_true("diffusione" %in% names(result))
  expect_true("escoskill_level_3" %in% names(result))
  expect_true("N" %in% names(result))
})

test_that("build_skillist translates reuse types to Italian", {
  skills <- data.table::data.table(
    escoskill_level_3 = c("A", "B", "C", "D"),
    esco_v0101_reusetype = c(
      "sector-specific",
      "transversal",
      "occupation-specific",
      "cross-sector"
    ),
    pillar_softskills = rep(0L, 4),
    esco_v0101_ict = rep(0L, 4),
    esco_v0101_green = rep(0L, 4),
    esco_v0101_language = rep(0L, 4)
  )

  diffusion <- data.table::data.table(
    escoskill_level_3 = c("A", "B", "C", "D"),
    N = c(10L, 20L, 30L, 40L),
    tf = c(10, 20, 30, 40),
    idf = c(1, 2, 3, 4),
    diffusione = rep("centrale", 4)
  )

  result <- build_skillist(skills, diffusion)

  expect_equal(result[escoskill_level_3 == "A", tipo], "settoriale")
  expect_equal(result[escoskill_level_3 == "B", tipo], "trasversale")
  expect_equal(result[escoskill_level_3 == "C", tipo], "specifico")
  expect_equal(result[escoskill_level_3 == "D", tipo], "multisettoriale")
})

test_that("build_skillist errors on missing columns in skills", {
  bad_skills <- data.table::data.table(escoskill_level_3 = "A")
  diffusion <- data.table::data.table(
    escoskill_level_3 = "A",
    N = 1L,
    tf = 1,
    idf = 1,
    diffusione = "alta"
  )

  expect_error(
    build_skillist(bad_skills, diffusion),
    "missing required columns"
  )
})

test_that("build_skillist errors on missing columns in diffusion", {
  skills <- data.table::data.table(
    escoskill_level_3 = "A",
    esco_v0101_reusetype = "transversal",
    pillar_softskills = 0L,
    esco_v0101_ict = 0L,
    esco_v0101_green = 0L,
    esco_v0101_language = 0L
  )
  bad_diffusion <- data.table::data.table(escoskill_level_3 = "A", N = 1L)

  expect_error(
    build_skillist(skills, bad_diffusion),
    "missing required columns"
  )
})

# 4. compute_idf_classification -----

test_that("compute_idf_classification returns expected structure", {
  set.seed(42)
  skills_merged <- data.table::data.table(
    general_id = rep(1:100, each = 3),
    escoskill_level_3 = sample(
      c("A", "B", "C", "D", "E"),
      300,
      replace = TRUE
    )
  )

  result <- compute_idf_classification(skills_merged)

  expect_type(result, "list")
  expect_named(result, c("idf", "threshold_rows", "diffuse_skills"))
  expect_s3_class(result$idf, "data.table")
  expect_named(
    result$idf,
    c("escoskill_level_3", "N", "tf", "idf", "diffusione"),
    ignore.order = TRUE
  )
  expect_type(result$threshold_rows, "double")
  expect_type(result$diffuse_skills, "character")
})

test_that("compute_idf_classification diffuse_skills is subset of all skills", {
  set.seed(123)
  skills_merged <- data.table::data.table(
    general_id = rep(1:50, each = 4),
    escoskill_level_3 = sample(
      paste0("skill_", 1:10),
      200,
      replace = TRUE
    )
  )

  result <- compute_idf_classification(skills_merged)
  all_skills <- unique(skills_merged$escoskill_level_3)

  expect_true(all(result$diffuse_skills %in% all_skills))
})

test_that("compute_idf_classification errors on missing columns", {
  bad_dt <- data.table::data.table(id = 1:5, skill = letters[1:5])

  expect_error(
    compute_idf_classification(bad_dt),
    "missing required columns"
  )
})
