# Compute median salary per skill

Join salary data with a skills table and compute the median salary for
each ESCO level-3 skill, ordered from highest to lowest.

## Usage

``` r
compute_salary_by_skill(salary_data, skills)
```

## Arguments

- salary_data:

  A data.table returned by
  [`extract_salary_data()`](https://gmontaletti.github.io/skillviz/reference/extract_salary_data.md).
  Must contain `salaryvalue` and a key column suitable for joining with
  `skills` (typically `general_id`).

- skills:

  A data.table of skills with at least columns `general_id` and
  `escoskill_level_3`.

## Value

A data.table with columns `escoskill_level_3`, `N` (count) and `mediana`
(median salary), sorted by descending median.
