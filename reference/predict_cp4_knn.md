# Predict CP2021 level-4 codes via sector-boosted Jaccard k-NN

Assigns CP2021 level-4 profession codes to unlabeled job announcements
using a two-step approach: (1) restrict candidates to CP4 codes observed
in labeled data for the same ESCO level-4 code (de facto crosswalk), (2)
disambiguate via Jaccard k-NN on binary skill vectors, with optional
sector boosting that gives higher weight to same-sector neighbors.

## Usage

``` r
predict_cp4_knn(postings, skills, k = 7L, sector_boost = 3, verbose = TRUE)
```

## Arguments

- postings:

  A data.table with columns: `general_id` (character), `idesco_level_4`
  (integer or character), `cp2021_id_level_4` (character, NA for
  unlabeled rows). Optionally includes `idsector` (character) for sector
  boosting. Only `NA` disables the boost for a row: itaposts stores an
  unknown sector as the empty string, so two announcements of unknown
  sector count as same-sector. That is deliberate, see Details.

- skills:

  A data.table with columns: `general_id` (character),
  `escoskill_level_3` (character).

- k:

  Integer number of nearest neighbors (default 7).

- sector_boost:

  Numeric multiplier for same-sector neighbors in the weighted vote. Set
  to 1.0 to disable sector boosting (default 3.0).

- verbose:

  Logical: print progress messages (default TRUE).

## Value

A data.table with columns:

- general_id:

  Announcement identifier.

- cp2021_id_level_4:

  Predicted CP2021 level-4 code.

- confidence:

  Weighted vote share of the winning class (0–1).

- method:

  One of `"knn"`, `"frequency"`, `"single_candidate"`, `"no_match"`.

## Details

The function splits `postings` into labeled (non-NA `cp2021_id_level_4`)
and unlabeled rows. Labeled data serves as the training set. For each
unlabeled announcement:

1.  The ESCO level-4 code restricts the CP4 candidate space to codes
    observed in labeled data (de facto crosswalk).

2.  If only one candidate exists, assign it directly
    (`method = "single_candidate"`).

3.  Otherwise, compute Jaccard similarity between the announcement's
    binary skill vector and all labeled announcements in the same ESCO
    group. Select the k nearest neighbors and apply a weighted vote,
    where same-sector neighbors receive a `sector_boost` multiplier.

4.  If no skills are available, fall back to the modal CP4 for that ESCO
    code (`method = "frequency"`).

5.  If the ESCO code is not present in labeled data, return NA
    (`method = "no_match"`).

Validated on 2025 OJA data (80/20 stratified split): CP4 accuracy 83.0%
with k=7 and sector_boost=3.0, vs 62.6% frequency baseline. Re-swept on
the 24-month production window (7x7 grid over k and sector_boost, 5
stratified splits, `skillviz_workflow/run_cp4_hyperparameter_sweep.R`):
CP4 accuracy 80.29% with k=7 and sector_boost=5.0, vs 59.1% frequency
baseline. The single-year figure is the easier setting; prefer the
windowed one when comparing against pipeline output.

Both figures above come from splits that draw train and test from the
same window at random, so near-duplicate postings can land on both
sides.

The pipeline calls this function **once on the whole 24-month window**
(`skillviz_workflow/_targets.R`), so an unlabeled posting draws
neighbours from every month, including later ones: production is
*contemporaneous*, not walk-forward. A walk-forward check
(`skillviz_workflow/run_cp4_temporal_validation.R`) trains on every
labeled month before each of the last 6 months and scores that month
alone; CP4 accuracy falls to 75.9% and every grid cell loses 3.3-4.4 pp,
stable across the 6 months (75.0-76.6%, no drift). That is a lower bound
for a *future-deployment* scenario, not the regime the pipeline runs in.

Accuracy quoted over all test rows conflates three populations.
Decomposed on a contemporaneous stratified holdout at
k=7/sector_boost=5.0 (`skillviz_workflow/run_cp4_kernel_sweep.R`): the
k-NN vote decides 82.6% of production rows and is 86.1% accurate on
them, against 62.0% for the modal fallback on the same rows, so the vote
is worth +24.1 pp. The frequency fallback covers 4.4% at 69.9%, and
13.1% of unlabeled rows get `no_match` because they carry no
`idesco_level_4` at all – only 5.6% of *labeled* rows do, so a labeled
holdout cannot reproduce production coverage. Weighted together that
gives **~74% expected production CP4 accuracy**, and that assumes k-NN
accuracy transfers unchanged from labeled to unlabeled rows, which the
covariate shift below makes optimistic.

Unlabeled postings are systematically longer than labeled ones: 12.7
skills against 8.2 on k-NN-eligible rows, a 1.55x gap stable across all
25 months and 78% within-source. Six alternative similarity kernels were
tested for length robustness (`run_cp4_kernel_sweep.R`, contemporaneous
holdout, 3 splits), parameterised as Tversky
`I / (I + alpha*(A-I) + beta*(B-I))`: **Jaccard `(1,1)` wins at k=7 and
nothing displaces it.** The best challenger `(1,0.5)` is -0.013 pp, 19x
smaller than the 0.244 pp swing produced by flipping an arbitrary
tie-break; Dice `(0.5,0.5)`, which is rank-equivalent to Jaccard and
therefore selects identical neighbours, still moves -0.33 pp through
vote weights alone, so anything under ~0.3 pp here is noise. The most
length-robust kernel, containment `I/|B|` `(0,1)`, loses **18.1 pp**:
Jaccard's union denominator is load-bearing, because `I/|B|` rewards
short neighbours and lets a 2-skill posting contained in a 28-skill
query score 1.0 while carrying almost no information. Do not re-test
this axis.

The length effect is real but is not a kernel problem. Long queries are
twin-poor: top-1 similarity 0.45 with 0.2% exact twins at 21+ skills,
against 0.76 and 48% at 1-4 skills. No kernel invents a neighbour that
does not exist.

An unknown `idsector` arrives from itaposts as the empty string, never
as NA, so unknown-sector announcements boost each other. Normalising the
empty string to NA was measured and **reduces** accuracy: -1.51 pp on
the 2.8% of announcements it affects (78.09% -\> 76.59%) and -0.04 pp
overall, on 5 of 5 splits. Missing sector is itself predictive of the
occupation, so the behaviour is kept on purpose – do not "fix" it
without re-measuring.

## See also

[`classify_esco_to_cpi()`](https://gmontaletti.github.io/skillviz/reference/classify_esco_to_cpi.md)
for Naive Bayes classification of ESCO-to-CPI3 mapping.

## Examples

``` r
postings <- data.table::data.table(
  general_id = as.character(1:10),
  idesco_level_4 = rep(c(1000L, 2000L), each = 5),
  cp2021_id_level_4 = c("1.1.1.1", "1.1.1.2", "1.1.1.1", NA, NA,
                         "2.2.2.1", "2.2.2.1", "2.2.2.2", NA, NA),
  idsector = rep(c("C", "F"), each = 5)
)
skills <- data.table::data.table(
  general_id = as.character(c(1,1,2,2,3,3,4,4,5,5,
                               6,6,7,7,8,8,9,9,10,10)),
  escoskill_level_3 = c("s1","s2","s2","s3","s1","s2","s1","s3","s2","s3",
                         "s4","s5","s4","s5","s5","s6","s4","s6","s5","s6")
)
result <- predict_cp4_knn(postings, skills, k = 3L, sector_boost = 1.0)
#> predict_cp4_knn: 6 labeled, 4 unlabeled
#> predict_cp4_knn: 4 predictions (knn=4)
```
