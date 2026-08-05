# Build a full ESCO-to-CP2021 crosswalk with its candidate structure

Derives, from labelled announcements, the empirical mapping between an
ESCO occupation code and the CP2021 codes it is observed with. Returns
both the full candidate structure and a per-group summary covering the
whole ESCO classification, not only the codes that happen to appear in
the data.

## Usage

``` r
build_esco_cp_crosswalk(
  postings,
  restrictor_col = "idesco_level_5",
  target_col = "cp2021_id_level_5",
  universe = NULL,
  strata_col = NULL,
  restrictor_na = "Unclassifiable",
  min_n = 0L,
  verbose = TRUE
)
```

## Arguments

- postings:

  A data.table with at least `restrictor_col` and `target_col`. Rows
  whose target is `NA` or the empty string are treated as unlabelled:
  they contribute to `n_postings` and `coverage_labelled` but not to the
  candidate counts.

- restrictor_col:

  Character naming the ESCO column, default `"idesco_level_5"`.

- target_col:

  Character naming the CP2021 column, default `"cp2021_id_level_5"`.

- universe:

  Optional data.table giving the full classification domain of
  `restrictor_col`, so codes that never appear in `postings` still get a
  row with `status = "absent"`. Must contain `restrictor_col`; a second
  column, if present, is carried through as the parent code. `NULL`
  (default) restricts the output to observed codes.

- strata_col:

  Optional character naming a column — in practice `source` — along
  which labelling is known to be non-random. When supplied, each group's
  modal code is recomputed with every stratum post-stratified to its
  share of that group's *unlabelled* rows, and `modal_stable` reports
  whether the mode survives. `NULL` (default) leaves the three
  reweighting columns `NA`.

- restrictor_na:

  Character vector of sentinel values meaning "no occupation code"
  (default `"Unclassifiable"`). These get `status = "unclassifiable"`
  and contribute no candidates.

- min_n:

  Integer: drop candidate pairs supported by fewer than this many
  labelled announcements (default 0, keep all). Shares and ranks are
  computed *after* the drop.

- verbose:

  Logical: print a one-line summary (default TRUE).

## Value

A list of two data.tables.

- candidates:

  One row per observed (ESCO, CP) pair: the two code columns, the parent
  CP4 code when the target is level 5, `n`, `share`, `rank` and
  `cum_share`. Ordered by group, then descending `n`, then code.

- groups:

  One row per ESCO code, covering `universe` when supplied: the code,
  its parent, `status`, `n_labelled`, `n_postings`, `coverage_labelled`,
  `n_candidates`, `n_eff`, `code_modal`, `modal_share`, `top3_share`,
  `modal_tied` and `modal_reliable`.

## Details

**This is a candidate restrictor, not a lookup.** Joining an ESCO code
to its modal CP code and stopping there is wrong for about one
announcement in four. Measured on the 24-month window at ESCO level 5
(2,714 groups, 761,917 doubly-labelled rows): the modal CP5 is right
**73.4%** of the time, row-weighted. Concentration rises slowly — top-2
85.8%, top-3 90.3%, top-5 94.3% — and only **0.46%** of rows sit in a
group whose modal code takes 100% of it. Several of the largest groups
are genuinely split: `8322.6` *autista privato* has 30 candidates over
13,607 rows with a 49.3% mode.

What closes the gap is the k-NN vote inside the candidate set:
[`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md)
scores 89.1% on the rows it decides. Use `modal_reliable`
(`modal_share >= 0.90 & n_labelled >= 30`, about a third of rows) if a
deterministic map is genuinely needed, and treat the rest as ambiguous.

**Read `n_eff`, not `n_candidates`.** The nominal candidate count is
dominated by a long tail of pairs seen once or twice, and that tail
grows with the evidence rather than shrinking: over the whole store an
ESCO level-5 group lists 32.1 candidates row-weighted, against 8.9 over
a 24-month window, while the modal share barely moves (73.0% against
73.4%). The inverse-Simpson `n_eff` is 2.28 either way — the extra
candidates carry almost no mass. Use `min_n` to trim the tail when the
table is consumed as a restrictor rather than as a description.

**`modal_stable` is not decoration — 13.7% of unlabelled rows fail it.**
Supplying `strata_col = "source"` recomputes each mode with the coded
sample post-stratified to the source mix of the group's *uncoded* rows,
which is the population the crosswalk is applied to. Measured over the
whole store: the modal CP5 changes for 497 of 2,708 reweightable groups
(18.4%), covering **13.66% of unlabelled announcements**. The overall
modal share barely moves (51.35% to 50.85%), so this is codes swapping
places within ambiguous groups rather than a wholesale collapse.

The movement tracks coverage exactly as the mechanism predicts, which is
what distinguishes it from noise: 24.8% of groups move in the 0-10%
labelled band, 24.4% at 10-25%, 18.2% at 25-50%, 10.3% at 50-75% and
3.7% at 75-100%. The underlying heterogeneity is large — within a single
ESCO level-5 group, the highest- and lowest-coverage source differ in
their CP5 distribution by a median total-variation distance of 0.19, and
38% of comparable groups exceed 0.25.

This threatens the *crosswalk*, not the choice of restrictor: the same
bias applies at ESCO level 4. Treat a group with `modal_stable = FALSE`
as describing its coded announcements rather than its occupation.

`coverage_labelled` is reported because labelling is strongly
non-random: 1,200 ESCO level-5 groups covering 491,739 announcements are
under 25% labelled, and labelling rates vary by source from 16.6% to
55.8%. A group's modal code is estimated from whichever announcements
happened to be coded, so a low `coverage_labelled` is a warning about
that group's row in this table.

Level 5 is a much sharper restrictor than level 4 — 2,714 groups against
399, a mean 8.94 candidates against 32.64, modal share 73.4% against
61.9%. A hierarchical backoff (use level 5 where the pool is thick, else
the level-4 parent) was measured for pool thresholds 10, 30, 50 and 100,
both in sample and held out in time, and **loses** 0.3 to 3.0 pp while
never improving coverage: it only moves rows between branches. Do not
re-test that axis.

The container does **not** read this table, and should not be changed
to.
[`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md)
derives its candidate set from the labelled rows of the run it is given,
which keeps the invariant that every candidate has at least one training
row. A persisted candidate set would break that — a group could present
a single candidate the model has never seen an example of, and be
assigned at `confidence = 1.0`.

## See also

[`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md),
which consumes the same structure derived in-memory.

## Examples

``` r
postings <- data.table::data.table(
  idesco_level_5 = c(rep("1000.1", 4), rep("1000.2", 3), "Unclassifiable"),
  cp2021_id_level_5 = c(
    "1.1.1.1.1", "1.1.1.1.1", "1.1.1.1.2", NA,
    "2.2.2.2.0", "2.2.2.2.0", "2.2.2.2.0", "3.3.3.3.0"
  )
)
cw <- build_esco_cp_crosswalk(postings, verbose = FALSE)
cw$candidates
#>    idesco_level_5 cp2021_id_level_5 cp2021_id_level_4     n     share  rank
#>            <char>            <char>            <char> <int>     <num> <int>
#> 1:         1000.1         1.1.1.1.1           1.1.1.1     2 0.6666667     1
#> 2:         1000.1         1.1.1.1.2           1.1.1.1     1 0.3333333     2
#> 3:         1000.2         2.2.2.2.0           2.2.2.2     3 1.0000000     1
#>    cum_share
#>        <num>
#> 1: 0.6666667
#> 2: 1.0000000
#> 3: 1.0000000
cw$groups
#>    idesco_level_5         status n_postings n_labelled coverage_labelled
#>            <char>         <char>      <int>      <int>             <num>
#> 1:         1000.1        covered          4          3              0.75
#> 2:         1000.2        covered          3          3              1.00
#> 3: Unclassifiable unclassifiable          1          1              1.00
#>    n_candidates n_eff code_modal modal_share top3_share modal_tied
#>           <int> <num>     <char>       <num>      <num>     <lgcl>
#> 1:            2   1.8  1.1.1.1.1   0.6666667          1      FALSE
#> 2:            1   1.0  2.2.2.2.0   1.0000000          1      FALSE
#> 3:           NA    NA       <NA>          NA         NA      FALSE
#>    modal_reliable code_modal_rw modal_share_rw modal_stable
#>            <lgcl>        <char>          <num>       <lgcl>
#> 1:          FALSE          <NA>             NA           NA
#> 2:          FALSE          <NA>             NA           NA
#> 3:          FALSE          <NA>             NA           NA
```
