# Build a master skills list with metadata and diffusion classification

Aggregates skill metadata (reuse type, flag columns) and merges with
diffusion scores. The ESCO reuse type labels are translated to Italian.

## Usage

``` r
build_skillist(skills, diffusion)
```

## Arguments

- skills:

  A `data.table` of skill occurrences with required columns
  `escoskill_level_3`, `esco_v0101_reusetype`, `esco_v0101_green`,
  `esco_v0101_language`, and the optional columns `pillar_softskills`
  and `esco_v0101_ict`.

- diffusion:

  A `data.table` as returned by
  [`compute_skill_diffusion()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_diffusion.md),
  with columns `escoskill_level_3`, `N`, `tf`, `idf`, `diffusione`.

## Value

A `data.table` with one row per unique skill, including metadata
columns, the Italian type label (`tipo`), and diffusion scores. The
columns `pillar_softskills` and `esco_v0101_ict` are always present, and
are `NA_integer_` when absent from `skills`.

## Details

The `tipo` classification derives from `esco_v0101_reusetype` alone;
`pillar_softskills` and `esco_v0101_ict` are descriptive pass-through
flags. They are optional because the Lightcast `data_v2` delivery
consumed through `itaposts` no longer supplies them: data read with
[`read_oja_itaposts()`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md)
yields `NA` in both columns, while legacy sources that still carry the
flags pass them through unchanged.
