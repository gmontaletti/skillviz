# Build a master skills list with metadata and diffusion classification

Aggregates skill metadata (reuse type, flag columns) and merges with
diffusion scores. The ESCO reuse type labels are translated to Italian.

## Usage

``` r
build_skillist(skills, diffusion)
```

## Arguments

- skills:

  A `data.table` of skill occurrences with columns: `escoskill_level_3`,
  `esco_v0101_reusetype`, `pillar_softskills`, `esco_v0101_ict`,
  `esco_v0101_green`, `esco_v0101_language`.

- diffusion:

  A `data.table` as returned by
  [`compute_skill_diffusion()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_diffusion.md),
  with columns `escoskill_level_3`, `N`, `tf`, `idf`, `diffusione`.

## Value

A `data.table` with one row per unique skill, including metadata
columns, the Italian type label (`tipo`), and diffusion scores.
