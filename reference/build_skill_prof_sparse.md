# Build a sparse skill-profession matrix

Converts a long-format `data.table` with skill-profession associations
into a sparse `dgCMatrix`. Rows represent skills and columns represent
professions.

## Usage

``` r
build_skill_prof_sparse(balassa, value_col = "N", prof_col = "cod_3")
```

## Arguments

- balassa:

  `data.table` with skill-profession associations. Must contain columns
  `escoskill_level_3`, `prof_col`, and the column named by `value_col`.

- value_col:

  Character, column name for cell values. Use `"N"` for counts or
  `"balassa_index"` for RCA values. Default: `"N"`.

- prof_col:

  Character, column name for profession identifiers. Default: `"cod_3"`.

## Value

A `dgCMatrix` with skills as rows and professions as columns.
