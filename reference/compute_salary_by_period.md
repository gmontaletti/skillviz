# Compute salary statistics by grouping period

Aggregate salary data computing count, mean and median salary per group.
The default grouping produces one row per salary band and month
combination.

## Usage

``` r
compute_salary_by_period(salary_data, by = c("salary", "mese"))
```

## Arguments

- salary_data:

  A data.table returned by
  [`extract_salary_data()`](https://gmontaletti.github.io/skillviz/reference/extract_salary_data.md).
  Must contain `salaryvalue` and the columns listed in `by`.

- by:

  Character vector of column names to group by. When `"mese"` is
  included and does not yet exist in `salary_data`, it is derived from
  the `data` column as a year-month string (`"YYYY-MM"`). Defaults to
  `c("salary", "mese")`.

## Value

A data.table with columns from `by` plus `N` (count), `media` (mean
salary) and `mediana` (median salary).
