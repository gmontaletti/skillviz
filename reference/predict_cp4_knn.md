# Predict CP2021 level-4 codes via sector-boosted Jaccard k-NN

Assigns CP2021 level-4 profession codes to unlabeled job announcements
using a two-step approach: (1) restrict candidates to CP4 codes observed
in labeled data for the same ESCO level-4 code (de facto crosswalk), (2)
disambiguate via Jaccard k-NN on binary skill vectors, with optional
sector boosting that gives higher weight to same-sector neighbors.

## Usage

``` r
predict_cp4_knn(
  postings,
  skills,
  k = 7L,
  sector_boost = 3,
  rescue_no_match = FALSE,
  rescue_k = 10L,
  rescue_max_train = 200000L,
  verbose = TRUE
)
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

- cp2021_id_level_4:

  Predicted CP2021 level-4 code.

- confidence:

  Weighted vote share of the winning class (0–1).

- method:

  One of `"knn"`, `"frequency"`, `"single_candidate"`, `"no_match"`, or
  `"knn_global"` when `rescue_no_match = TRUE`.

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

The occupation hierarchy was also tested and closed
(`skillviz_workflow/run_cp3_vote.R`). **ESCO level 3 is a worse
restrictor than level 4, not a better one**: weighted by row volume over
761,521 doubly-labelled postings, ESCO4 -\> CP3 has 37.4 mean candidates
and 68.4% modal-share accuracy, against 52.3 and 59.8% for ESCO3 -\>
CP3. Coarsening the *target* gains 5.7 pp; coarsening the *predictor*
loses 8.6 pp, because ESCO L3 pools unit groups that map to different
CP3 codes. ESCO L3 also cannot improve coverage: postings without an
`idesco_level_4` carry `idesco_level_5 = "Unclassifiable"`, so no ESCO
code exists at any level (and CP-unlabelled rows carry
`cp2021_id_level_5 = ""`).

Predicting CP3 by pooling the k-NN vote across sibling CP4 codes, rather
than truncating the CP4 winner as `build_annunci_cp4()` does, is
likewise **not worth it**: +0.063 pp CP3 on k-NN rows (88.54% -\> 88.60%
at k=7), changing only 0.31% of predictions, and when the two rules
disagree pooling is right 60% of the time – barely above chance, and far
under this harness's 0.244 pp tie-break noise floor. The predicted
mechanism is refuted: the gain is flat across ESCO-group ambiguity (0.00
pp at 2 candidates, +0.05 pp at 21+) instead of growing with it.
Truncating the CP4 argmax is very nearly optimal for CP3. A second stage
picking the best CP4 sibling inside the winning CP3 also loses (86.011%
vs 86.018%), as its break-even arithmetic predicted: it needs 93.5-97.8%
conditional accuracy while modal-CP4-within-CP3 is 68.5% over 4.6
candidates. Do not re-test this axis.

`rescue_no_match = TRUE` addresses a different population: the 13.1% of
unlabeled rows that carry no `idesco_level_4`, so there is no candidate
set to restrict to. 94.4% of them do have skills and all have an
`idsector`. Validation used the population analogue rather than a
simulation – 45,029 *labeled* rows also lack an ESCO code, so they carry
ground truth in the target's shape
(`skillviz_workflow/run_cp4_no_match_rescue.R`). Unrestricted k-NN at
`rescue_k = 10` with `sector_boost = 5` scores 68.8% CP4 / 74.0% CP3
there, against 36.6% for CP4-centroid cosine, 19.4% for sector-modal and
3.8% for global-modal; reweighted to the target's length distribution,
which is longer than the analogue's, the expected figure is **73.8% CP4
/ 77.8% CP3**. That is well below the 86.1% of the ESCO-restricted path,
which is why the argument defaults to FALSE and why `confidence` matters
here.

Confidence is well calibrated on this population and is the intended
filter: the top 10% of rescued rows by confidence is 99.1% accurate, the
top 30% 96.8%, the top 50% 91.1%. Accuracy also rises steeply with
posting length, from 44.3% at 1-4 skills to 91.9% at 21+. The
high-confidence slice is not an artefact of near-duplicate retrieval:
exact skill-set twins are 61.3% of this population but score *worse*
than non-twins (66.4% vs 71.8%), because a short skill set has many
twins without determining the occupation.

The rescue pass reads accuracy from a pool capped at `rescue_max_train`.
Accuracy was still climbing with pool size when measured (+2.0 pp from
200k to 400k), so the default 200000 trades some accuracy for runtime.

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
