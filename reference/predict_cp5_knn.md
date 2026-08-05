# Predict CP2021 level-5 codes via sector-boosted Jaccard k-NN

Assigns CP2021 level-5 codes (unità professionali) to unlabeled job
announcements. Same algorithm as
[`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
one level down: the ESCO level-4 code restricts the candidate space to
the CP5 codes observed in labeled data for that group (de facto
crosswalk), then Jaccard k-NN on binary skill vectors disambiguates,
with same-sector neighbors boosted.

## Usage

``` r
predict_cp5_knn(
  postings,
  skills,
  restrictor = c("idesco_level_5", "idesco_level_4"),
  restrictor_na = NULL,
  k = 7L,
  sector_boost = 5,
  max_train = 50000L,
  dense_budget = 2e+08,
  rescue_no_match = FALSE,
  rescue_k = 10L,
  rescue_max_train = 200000L,
  verbose = TRUE
)
```

## Arguments

- postings:

  A data.table with columns: `general_id` (character), `idesco_level_4`
  (integer or character), `cp2021_id_level_5` (character, `NA` or `""`
  for unlabeled rows). Optionally `idsector` (character). A
  `cp2021_id_level_4` column, if present, is ignored.

- skills:

  A data.table with columns: `general_id` (character),
  `escoskill_level_3` (character).

- restrictor:

  Character naming the column that restricts the candidate space, one of
  `"idesco_level_4"` (default) or `"idesco_level_5"`. Level 5 is the
  sharper restrictor — 2,714 groups against 399, a mean 8.94 CP5
  candidates against 32.64 — and scores 89.1% against 85.9% on
  k-NN-decided rows. It is not yet the default; see Details.

- restrictor_na:

  Character vector of sentinel values in `restrictor` that mean "no
  occupation code", normalised to `NA` so those rows fall through to
  `method = "no_match"`. `NULL` (default) resolves to `"Unclassifiable"`
  when `restrictor = "idesco_level_5"` and to nothing otherwise. **Do
  not set this to `character(0)` at level 5**: the sentinel is a string,
  not `NA`, so leaving it in place silently merges every ESCO-less
  announcement into a single enormous pseudo-group.

- k:

  Integer number of nearest neighbors (default 7).

- sector_boost:

  Numeric multiplier for same-sector neighbors in the weighted vote. Set
  to 1.0 to disable sector boosting. **Defaults to 5.0 here, not 3.0**:
  that is the measured optimum at this level (boost 1 loses 2.3 pp) and
  it matches what the pipeline and container already pass to
  [`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md),
  whose own 3.0 default is a legacy value production overrides.

- max_train:

  Integer cap on the labelled pool used per ESCO group (default 50000).
  Groups above it are subsampled by a deterministic stride over the
  existing row order, so results are reproducible and the RNG is
  untouched. The cap does fire on the current 24-month window — ESCO
  group 5223 carries about 52,700 labelled rows — so raise it to use
  those groups whole, at the cost of a dense `test x train` block that
  grows with it.

- dense_budget:

  Numeric cap on the number of elements in the dense `test x train`
  similarity block (default 2e8). Each batch holds three such matrices
  at once, so peak memory is roughly 24 bytes per budgeted element – the
  default costs about 4.8 GB, which OOM-kills an 8 GB container on the
  24-month window. Lowering it only chunks the work into more batches;
  every test row is scored independently, so results are unchanged.

- rescue_no_match:

  Logical: when TRUE, announcements that would be `no_match` for want of
  an `idesco_level_4` but that do carry skills are classified by an
  unrestricted k-NN over the whole labeled pool, and returned with
  `method = "knn_global"`. Defaults to FALSE, which reproduces the
  previous behaviour exactly. See Details for measured accuracy: these
  predictions are markedly less accurate than the ESCO-restricted ones,
  so `confidence` should be used to filter them.

- rescue_k:

  Integer number of neighbors for the rescue pass (default 10). Only
  used when `rescue_no_match = TRUE`.

- rescue_max_train:

  Integer cap on the labeled pool used by the rescue pass (default
  200000). The pool is subsampled by a deterministic stride over
  `general_id`, so results are reproducible and the RNG is untouched.
  Raising it improves accuracy at roughly linear cost in time.

- verbose:

  Logical: print progress messages (default TRUE).

## Value

A data.table with columns:

- general_id:

  Announcement identifier.

- cp2021_id_level_5:

  Predicted CP2021 level-5 code.

- confidence:

  Weighted vote share of the winning class (0–1).

- method:

  One of `"knn"`, `"frequency"`, `"single_candidate"`, `"no_match"`, or
  `"knn_global"` when `rescue_no_match = TRUE`.

## Details

Validated on the 24-month production window
(`skillviz_workflow/run_cp5_knn.R`, 5 stratified contemporaneous splits
of 50,000 test rows, holdout stratified on CP5). **CP5 accuracy 85.9% on
k-NN-decided rows at k=7 and `sector_boost = 5.0`**, against 86.2% for
[`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
on the same rows: the finer level costs about 0.25 pp. Over all test
rows, including the fallback cascade, CP5 is 79.9%.

Level 5 is barely a harder restricted problem than level 4, which is why
the cost is so small. Row-weighted over the labeled window, an ESCO
level-4 group offers 65.8 CP5 candidates against 61.7 CP4 ones
(modal-share accuracy 61.9% vs 62.7%). By contrast CP3 -\> CP4 costs 5.7
pp of modal share. The hierarchy is also narrow: of 813 CP5 codes in
`staging.dim_cp2021_5`, 510 CP4 parents carry a mean 1.60 children, and
340 have exactly one, so 67.2% of labeled rows sit under a CP4 whose CP5
is determined by the classification alone.

Two axes were closed by that harness. **Do not re-test them.**

First, **predicting CP5 directly beats refining the CP4 winner.**
Mapping the
[`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
argmax to the modal CP5 within its (ESCO group, CP4) cell loses 0.877 pp
at k=7, on 5 of 5 splits (sd 0.039), changing 1.30% of predictions. This
looked like the favourite going in: modal CP5 within the group and the
*true* CP4 is 98.66% accurate over 1.60 candidates, which inverts the
break-even arithmetic that closed the CP3 -\> CP4 second stage in
[`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md).
The mechanism it missed is that conditioning on the *predicted* CP4
inherits every CP4 error, while a direct CP5 vote can land on the right
code under a CP4 the level-4 argmax got wrong.

Second, **truncating the CP5 winner reproduces the CP4 winner.** Taking
`substring(pred, 1L, 7L)` instead of running
[`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
moves CP4 accuracy by +0.004 pp at k=7 (\|delta\| \<= 0.010 across k in
5, 7, 10), changing 0.07% of predictions – two orders of magnitude under
this harness's 0.244 pp tie-break noise floor. One model therefore
serves both levels, and the neighbour search need not be run twice: it
is 98.6% of runtime, the vote 1.4%.

`k = 7` and `sector_boost = 5.0` remain optimal at this cardinality
(swept over k in 5, 7, 10 and boost in 1, 3, 5; boost 1 loses 2.3 pp).
The Jaccard kernel is inherited unchanged and was closed at level 4.

Confidence is monotone in accuracy across all ten deciles and is the
intended filter: the top 10% of k-NN rows by confidence is 97.1%
accurate, the top 50% 96.9%, against 85.9% overall. It is mildly
overconfident at the top, where a mean confidence of 100% scores 96.7%.
**CP5 thresholds must be derived from this table rather than inherited
from level 4**: the same k votes spread over more classes, so the same
confidence value means something different here.

**ESCO level 5 is a better restrictor than level 4** and is available
via `restrictor = "idesco_level_5"`, but is not yet the default. It
scores +3.2 pp at CP5 (89.1% vs 85.9%) and +3.1 pp at CP4, halving the
candidate space (23.9 vs 65.8) for a coverage loss of 0.018 pp, and wins
in every training-pool-size band above 10 labeled rows — losing only in
the thinnest (66.0% vs 73.1% at 1-10 rows). It also shifts
`single_candidate` from 0.01% to 0.54% of rows and puts 3.2% of rows in
groups with fewer than 50 neighbors.

**ESCO level 5 is the default restrictor.** It was adopted after the
walk-forward gate in `skillviz_workflow/run_cp5_restrictor_temporal.R`,
which trains on every labeled month before each of the last 6 and scores
that month alone. The restrictor did better under temporal shift than
contemporaneously, not worse:

- CP5 **+3.95 pp** paired on k-NN-decided rows, positive in **6 of 6**
  months (range +3.61 to +4.54), against a +1.0 pp bar;

- CP4 +3.81 pp, and CP4 recovered by truncating the level-5 argmax is
  within 0.02 pp of that, so one pass would serve both levels;

- coverage cost +0.042 pp; positive in **every** training-pool band,
  including 1-10 rows (+5.8 pp), which reverses the contemporaneous
  finding;

- absolute levels fall as expected under walk-forward: ESCO4 CP4 80.3%
  here against 86.2% contemporaneously.

The first run of that gate failed a secondary criterion — confidence
monotone across deciles in at least 5 of 6 months, of which level 5
managed 4 — and the restrictor was **not** adopted on that run. The
criterion was then found to be defective rather than merely
inconvenient: it rejected the *incumbent* level-4 arm harder still (3 of
6), because deciles 5 through 10 all sit at exactly 100% confidence, one
tied block covering ~60% of rows where the decile split is arbitrary and
there is no ordering to test.

It was replaced, **before** re-running, by two comparative criteria:
expected calibration error relative to level 4 (margin +1.0 pp), and
monotonicity restricted to the deciles where confidence actually varies.
On the corrected gate level 5 passes everything, and on a proper
calibration metric it is **better** calibrated than the incumbent, not
worse: ECE -0.81 pp on average and lower in all 6 months, with
monotonicity 6 of 6.

Two caveats survive regardless: every figure above is
labeled-on-labeled, unlabeled postings are 1.55x longer, and labelling
is strongly non-random across ESCO groups and sources (see
[`build_esco_cp_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_esco_cp_crosswalk.md)).
The gain is *not* established on the population the coder is actually
applied to.

One measurement from that run matters to the container even though the
restrictor was not adopted: an ESCO5-restricted level-5 prediction and
an ESCO4-restricted level-4 prediction **disagree on 12.2% of rows**.
Running the two levels under different restrictors is therefore not
viable — the container drops a level-5 code whose parent contradicts the
level-4 column, so a split configuration would silently discard those
rows. Both levels move together or neither does.

A **hierarchical backoff** — use level 5 where its labeled pool is thick
enough, else fall back to the level-4 parent — was measured for pool
thresholds 10, 30, 50 and 100, in sample and held out in time, and
**loses** 0.3 to 3.0 pp while never improving coverage: it only moves
rows between branches, and just 0.15% of held-out rows have no level-5
group in training. Pure level 5 with a level-4 null-guard for the
residual is the right shape. Do not re-test this axis. (Established on
modal-share accuracy, a proxy for k-NN accuracy, so it is strong
evidence rather than a direct measurement.)

Rows carrying `idesco_level_5 = "Unclassifiable"` have no ESCO code at
any level and so get `method = "no_match"` (5.6% of rows) unless
`rescue_no_match = TRUE`. The container's vtreat + xgboost path, which
serves those rows at level 3, cannot be pushed to level 5.

## See also

[`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
for the level-4 model and the full record of closed experiments at that
level.

## Examples

``` r
postings <- data.table::data.table(
  general_id = as.character(1:10),
  idesco_level_5 = rep(c("1000.1", "2000.1"), each = 5),
  cp2021_id_level_5 = c("1.1.1.1.1", "1.1.1.1.2", "1.1.1.1.1", NA, NA,
                         "2.2.2.1.0", "2.2.2.1.0", "2.2.2.2.0", NA, NA),
  idsector = rep(c("C", "F"), each = 5)
)
skills <- data.table::data.table(
  general_id = as.character(c(1,1,2,2,3,3,4,4,5,5,
                               6,6,7,7,8,8,9,9,10,10)),
  escoskill_level_3 = c("s1","s2","s2","s3","s1","s2","s1","s3","s2","s3",
                         "s4","s5","s4","s5","s5","s6","s4","s6","s5","s6")
)
result <- predict_cp5_knn(postings, skills, k = 3L, sector_boost = 1.0)
#> predict_cp5_knn: 6 labeled, 4 unlabeled
#> predict_cp5_knn: 4 predictions (knn=4)
```
