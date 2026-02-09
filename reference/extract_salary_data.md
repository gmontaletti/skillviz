# Extract announcements with salary information

Filter job announcements to those with a positive salary value.

## Usage

``` r
extract_salary_data(ann)
```

## Arguments

- ann:

  A data.table of announcements. Must contain column `salaryvalue`
  (numeric).

## Value

A data.table containing only rows where `salaryvalue > 0`.
