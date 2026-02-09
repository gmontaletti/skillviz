# Build a comprehensive profession-skill profile table

Joins announcements, skills, ISCO classification, a master skills list,
and co-occurrence data into a single profession-skill table. This
reproduces the join logic from the profession-skill exploration script
(08_ski_ann_stru.R).

## Usage

``` r
build_profession_skill_profile(ann, ski, isco, skillist, cooc = NULL)
```

## Arguments

- ann:

  A `data.table` of announcements with at least `general_id`,
  `idesco_level_4`, `gdate`, `edate`.

- ski:

  A `data.table` of skills with at least `general_id` and
  `ESCOSKILL_LEVEL_3`.

- isco:

  A `data.table` of ISCO groups with at least `code` and
  `preferredLabel`.

- skillist:

  A `data.table` master skills list with at least `ESCOSKILL_LEVEL_3`
  (or `escoskill_level_3`) and `diffusione`.

- cooc:

  Optional `data.table` co-occurrence edge list with `from`, `to`,
  `weight`. If provided, it is returned unmodified as an attribute of
  the output for downstream graph analysis.

## Value

A `data.table` with columns `preferredLabel`, `ESCOSKILL_LEVEL_3`,
`diffusione`, and `annunci` (number of unique announcements per
profession-skill pair).

## Details

Steps:

1.  Merge announcements with ISCO to get `preferredLabel`.

2.  Merge skills with announcements on `general_id`.

3.  Merge with the master skills list for diffusion metadata.

4.  Aggregate to profession-skill level with announcement counts.
