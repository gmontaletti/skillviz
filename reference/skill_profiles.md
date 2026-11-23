# Skill coverage profiles of occupation codes

Builds one profile per occupation code: for every skill, the share of
the code's postings that mention it, optionally weighted by the inverse
document frequency of the skill. The profiles are returned as a sparse
code-by-skill matrix.

## Usage

``` r
skill_profiles(
  postings,
  skills,
  code_col,
  id_col = "general_id",
  skill_col = "idescoskill_level_3",
  weighting = c("idf", "none"),
  min_postings = 1L,
  code_na = c("", "Unclassifiable"),
  idf = NULL
)
```

## Arguments

- postings:

  A `data.frame` with one row per posting and the columns `id_col` and
  `code_col`. Postings whose code is missing (`NA` or a value in
  `code_na`) are ignored.

- skills:

  A `data.frame` with columns `id_col` and `skill_col`, one row per
  posting and skill (duplicates are ignored).

- code_col:

  Name of the occupation code column of `postings`.

- id_col:

  Name of the posting id column. Default `"general_id"`.

- skill_col:

  Name of the skill column of `skills`. Default `"idescoskill_level_3"`.

- weighting:

  `"idf"` (default) multiplies each share by the IDF of the skill;
  `"none"` keeps the plain shares.

- min_postings:

  Minimum number of postings with at least one skill for a code to get a
  row. Default 1.

- code_na:

  Values of `code_col` that mean "no code". Default
  `c("", "Unclassifiable")`.

- idf:

  Optional named numeric vector of IDF weights (from
  [`skill_idf()`](https://gmontaletti.github.io/skillviz/reference/skill_idf.md)).
  When supplied it also fixes the skill columns, so that profiles of
  different classifications are comparable; skills absent from it are
  dropped. When `NULL` and `weighting = "idf"`, the IDF is computed on
  the postings with a code.

## Value

A sparse matrix (`dgCMatrix`) with one row per code with at least
`min_postings` postings (sorted by code) and one column per skill. The
attributes are `n_postings` (named integer: postings with at least one
skill, for every code seen, including those below `min_postings`), `idf`
(the weights used, or `NULL`) and `weighting`.

## Details

The denominator of the share is the number of the code's postings with
at least one skill: postings without skills carry no information on the
profile. With `weighting = "idf"` the entry for code \\c\\ and skill
\\s\\ is \\(n\_{cs} / n_c) \cdot idf_s\\. Each posting must carry one
code: the function stops if an id has two codes.

The matrix attributes are dropped by most `Matrix` operations; read them
before subsetting.

## Examples

``` r
postings <- data.table::data.table(
  general_id = 1:4,
  cp2021_id_level_5 = c("1.1.1.1.1", "1.1.1.1.1", "2.2.2.2.0", "")
)
skills <- data.table::data.table(
  general_id = c(1, 1, 2, 3, 4),
  idescoskill_level_3 = c("a", "b", "a", "c", "c")
)
p <- skill_profiles(postings, skills, "cp2021_id_level_5")
as.matrix(p)
#>                   a         b        c
#> 1.1.1.1.1 0.4054651 0.5493061 0.000000
#> 2.2.2.2.0 0.0000000 0.0000000 1.098612
attr(p, "n_postings")
#> 1.1.1.1.1 2.2.2.2.0 
#>         2         1 
```
