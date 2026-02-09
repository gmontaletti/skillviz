# Tests for plot.R -----

# 1. plot_cooc_graph -----

test_that("plot_cooc_graph returns ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")

  cooc_data <- data.table::data.table(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "C", "D"),
    weight = c(10, 5, 8, 3)
  )

  result <- plot_cooc_graph(cooc_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_cooc_graph uses profession as title", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")

  cooc_data <- data.table::data.table(
    from = c("A", "B"),
    to = c("B", "C"),
    weight = c(10, 5)
  )

  result <- plot_cooc_graph(cooc_data, profession = "Data Analyst")

  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$title, "Data Analyst")
})

test_that("plot_cooc_graph errors on empty data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")

  empty_dt <- data.table::data.table(
    from = character(0),
    to = character(0),
    weight = numeric(0)
  )

  expect_error(
    plot_cooc_graph(empty_dt),
    "non-empty data.frame"
  )
})

test_that("plot_cooc_graph errors on missing columns", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")

  bad_dt <- data.table::data.table(a = 1:3, b = 4:6)

  expect_error(
    plot_cooc_graph(bad_dt),
    "missing required columns"
  )
})

# 2. plot_skill_ranking -----

test_that("plot_skill_ranking returns ggplot object", {
  skip_if_not_installed("ggplot2")

  serie <- data.table::data.table(
    mese = as.Date(c(
      rep("2022-01-01", 3),
      rep("2023-01-01", 3)
    )),
    skill = rep(c("Python", "SQL", "Excel"), 2),
    rango = c(1L, 2L, 3L, 2L, 1L, 3L),
    professione = rep("Analyst", 6),
    N = c(100L, 80L, 60L, 90L, 95L, 55L)
  )

  result <- plot_skill_ranking(serie, profession = "Analyst")

  expect_s3_class(result, "ggplot")
})

test_that("plot_skill_ranking filters to specified skills", {
  skip_if_not_installed("ggplot2")

  serie <- data.table::data.table(
    mese = as.Date(c(
      rep("2022-01-01", 3),
      rep("2023-01-01", 3)
    )),
    skill = rep(c("Python", "SQL", "Excel"), 2),
    rango = c(1L, 2L, 3L, 2L, 1L, 3L),
    professione = rep("Analyst", 6),
    N = c(100L, 80L, 60L, 90L, 95L, 55L)
  )

  result <- plot_skill_ranking(
    serie,
    profession = "Analyst",
    skills = c("Python", "SQL")
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_skill_ranking errors on empty data", {
  skip_if_not_installed("ggplot2")

  empty_dt <- data.table::data.table(
    mese = as.Date(character(0)),
    skill = character(0),
    rango = integer(0),
    professione = character(0),
    N = integer(0)
  )

  expect_error(
    plot_skill_ranking(empty_dt, profession = "Analyst"),
    "non-empty data.frame"
  )
})

test_that("plot_skill_ranking errors on nonexistent profession", {
  skip_if_not_installed("ggplot2")

  serie <- data.table::data.table(
    mese = as.Date("2023-01-01"),
    skill = "Python",
    rango = 1L,
    professione = "Analyst",
    N = 100L
  )

  expect_error(
    plot_skill_ranking(serie, profession = "NonExistent"),
    "no rows for profession"
  )
})

test_that("plot_skill_ranking errors when no requested skills found", {
  skip_if_not_installed("ggplot2")

  serie <- data.table::data.table(
    mese = as.Date("2023-01-01"),
    skill = "Python",
    rango = 1L,
    professione = "Analyst",
    N = 100L
  )

  expect_error(
    plot_skill_ranking(serie, profession = "Analyst", skills = "NonExistent"),
    "none of the requested skills"
  )
})

test_that("plot_skill_ranking errors on missing columns", {
  skip_if_not_installed("ggplot2")

  bad_dt <- data.table::data.table(x = 1:3, y = 4:6)

  expect_error(
    plot_skill_ranking(bad_dt, profession = "Test"),
    "missing required columns"
  )
})
