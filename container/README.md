# Containerised CP4 imputation

Standalone job: reads OJA postings from the Postgres database, fills the missing
CP2021 codes at levels 5 and 4, and writes the result to the `staging` schema.

One `skillviz::predict_cp5_knn()` pass does the work — voting at level 5 with the
candidate space restricted on `idesco_level_5`, with the level-4 code read off by
truncation. Announcements carrying no ESCO code at any level cannot be restricted
and go to a one-hot + xgboost model instead.

Unlike the `skillviz_workflow` targets pipeline — which reads the local DuckDB
store — this container talks to Postgres directly and is meant to run from cron.

Setting it up: [INSTALLAZIONE.md](INSTALLAZIONE.md) (in Italian). The image is
**not published to any registry** — it is built from source.

**Before using the imputed codes in an analysis, read [LIMITI.md](LIMITI.md)**
(in Italian): what these procedures do not guarantee, with the measurements that
quantify each limit. The most important one is that no validation covers the
population the coder is actually applied to.

## What it writes

`staging.gm_cp4_imputed`, one row per deduplicated posting:

| column | type | meaning |
|---|---|---|
| `general_id` | bigint, primary key | posting identifier |
| `year_grab_date`, `month_grab_date` | int | month key, indexed |
| `cp2021_id_level_4` | text | the vendor's level-4 code where it exists, the imputed one otherwise |
| `cp4_imputed` | smallint | `0` vendor-supplied, `1` algorithmic, `NULL` still unknown |
| `cp2021_id_level_5` | text, nullable | level-5 code (unità professionale) |
| `cp5_imputed` | smallint, nullable | `0` vendor-supplied, `1` algorithmic, `NULL` unavailable |

`cp4_imputed` is `NULL` exactly when `cp2021_id_level_4` is `NULL`, and the same
holds for the level-5 pair. `substring(cp2021_id_level_5, 1, 7)` always equals
`cp2021_id_level_4` where both are present — asserted before every write.

### One k-NN pass at level 5, level 4 by truncation

The coder votes once, at level 5, with the candidate space restricted on
`idesco_level_5`. The level-4 code is then read off as
`substring(pred, 1, 7)` rather than voted separately.

Two measurements license that. Truncating the level-5 argmax reproduces the
level-4 argmax to **+0.004 pp** contemporaneously and **+0.02 pp** walk-forward,
changing 0.07% of predictions — two orders of magnitude under the 0.244 pp
tie-break noise floor. And the neighbour search is ~98.6% of the runtime against
the vote's 1.4%, so a second pass would nearly double the cost to re-derive the
same answer.

It also removes a failure mode rather than only saving time. When the two levels
were voted independently they could disagree, and a level-5 code whose parent
contradicted the level-4 column had to be discarded — 0.07% of rows then, and
**12.2%** had the two levels also used different restrictors. Under truncation
the hierarchy is a structural invariant, so a violation now aborts the run
instead of silently dropping codes.

### Why the restrictor is ESCO level 5

Validated walk-forward over the last 6 months
(`skillviz_workflow/run_cp5_restrictor_temporal.R`), training on every labelled
month before each test month:

| | ESCO level 5 | ESCO level 4 |
|---|---|---|
| level-5 accuracy, k-NN rows | **84.1%** | 80.0% |
| level-4 accuracy, k-NN rows | **84.6%** | 80.3% |
| paired gain | **+3.95 pp**, 6 months of 6 | — |
| coverage cost | +0.04 pp | — |
| calibration error (ECE) | **5.5 pp** | 6.3 pp |
| wall-clock / peak RSS | **541s / 5.64 GB** | 772s / 6.41 GB |

Level 5 is not a trade: it is more accurate, better calibrated, faster and
lighter. The gain is larger under walk-forward than contemporaneously (+3.95
against +3.2), and it wins in every training-pool band including the thinnest.
2,714 small groups do less dense similarity work than 399 large ones, which is
why the finer restrictor is also the cheaper one.

One caveat travels with all of it: every figure is measured labelled-on-labelled,
and labelling is not random (see `?skillviz::build_esco_cp_crosswalk`). The gain
is not established on the uncoded population the coder is applied to.

### Confidence is on a new scale

The level-5 vote spreads the same `k` neighbours over more classes than the old
level-4 one, so a given confidence value does not mean what it used to. **Any
downstream threshold must be re-derived, not inherited.** Calibration is reported
per decile in `cp5_restrictor_temporal_results.rds`; expected calibration error
is 5.5 pp, better than the 6.3 pp of the restrictor it replaced.

### No padding, at either level

Earlier versions of this job carried level-3-precise codes in the level-4 column,
padded to `<cp3>.0`, because the ESCO-less segment could only be modelled at
level 3. **That convention is gone.** Every model now predicts at level 5 and
every level-4 code is the truncation of a level-5 one, so a level-4 code in this
table is always a genuine level-4 code. The job asserts it: a value ending in
`.0` in `cp2021_id_level_4` aborts the run, since none of the 510 real level-4
codes ends that way.

The same test must never be applied to `cp2021_id_level_5`, where **340 of the
813 real codes do end in `.0`** — CP2021 writes an unsubdivided level-4 category
as `<cp4>.0`.

### Which model handles which rows

Each segment gets the model that wins on it:

| rows | model | level-5 accuracy | level-3 accuracy |
|---|---|---|---|
| **with** an ESCO code | k-NN (`predict_cp5_knn`, restricted on `idesco_level_5`) | **84.1%** | — |
| **without** one (~13%) | one-hot + xgboost | **76.4%** | 80.8% |

The k-NN figure is walk-forward over 6 months; the xgboost figure is a 3-month
temporal holdout on the 42,530 labelled ESCO-less rows.

Where an ESCO code exists, restricting candidates to the codes observed for that
exact occupation beats anything the covariates can do. Where none exists that
restriction is unavailable, and the covariates the k-NN ignores — city, sector,
source, contract, education, salary — win instead. The xgboost path is also the
only one that can classify a posting with **no skills at all** (81.5% vs 69.7%).

Neither model keeps an artefact: both are refitted from scratch on each full run.
Level-5 codes with fewer than `IMPUTE_XGB_RARE_MIN` labelled examples are folded
into an `other` bucket, and rows predicted into it are left unimputed rather than
guessed into a common class.

A vendor code is never overwritten. The job asserts this before committing and
aborts if it ever finds otherwise.

## Run modes

Decided by comparing the months in the source against those already in the target:

| condition | mode |
|---|---|
| target absent, or every month missing, or more than `IMPUTE_MAX_INCREMENTAL` missing | **full** — rebuild everything |
| 1 to `IMPUTE_MAX_INCREMENTAL` months missing | **incremental** — impute only those months |
| nothing missing | **no-op** — exit 0 |

Incremental runs still train on every labelled row in the window; only the
*prediction* is restricted to the new months.

Both writes are transactional and idempotent. Full builds into a `_new` table and
swaps it in, so readers never see a half-built table; incremental deletes the
target months before appending, so a re-run replaces rather than duplicates.

## Exit codes

`0` success · `1` error · `2` skillviz missing from the image · `3` another run holds the lock

## Build

Build from the **package root**, not this directory:

```sh
cd /path/to/skillviz
docker build -f container/Dockerfile -t skillviz-impute:latest .
```

The build context is the package root, so the package-root `.dockerignore`
applies — it excludes `container/.Renviron` and every other credential file.

## Test

`assemble()` and `reconcile()` decide what reaches the live table, so they have
an offline test that needs no database:

```sh
Rscript container/test_impute_cp4.R
```

It loads the script's definitions without running `main()` and drives synthetic
postings through every row class: vendor-coded, k-NN-imputed at both levels,
k-NN levels disagreeing, xgboost `.0`-padded, and unresolved. It also asserts
that a hierarchy break and a malformed level-5 code are both fatal.

## Run

```sh
docker run --rm \
  --name skillviz-impute \
  --user "$(id -u):$(id -g)" \
  --env-file /etc/skillviz/postgres.env \
  -v /var/skillviz/run:/var/skillviz/run \
  skillviz-impute:latest
```

Dry run — reads, decides the mode, imputes, reconciles, writes nothing:

```sh
docker run --rm --env-file /etc/skillviz/postgres.env \
  -e IMPUTE_DRY_RUN=1 skillviz-impute:latest
```

Diagnostics, using the split entrypoint:

```sh
docker run --rm skillviz-impute:latest -e 'packageVersion("skillviz")'
```

## Configuration

See `postgres.env.example` for the full list. Credentials use `PG_*` (canonical
across the ecosystem) with `POSTGRES_*` accepted as an alias. Passwords are never
logged.

Create the host env file readable only by the account that runs the job:

```sh
sudo install -m 0600 -o monty -g monty postgres.env.example /etc/skillviz/postgres.env
sudoedit /etc/skillviz/postgres.env
```

## Cron

```cron
# CP4 imputation, daily at 05:40
40 5 * * * monty docker run --rm --name skillviz-impute --user "$(id -u):$(id -g)" \
  --env-file /etc/skillviz/postgres.env -v /var/skillviz/run:/var/skillviz/run \
  skillviz-impute:latest >> /var/log/skillviz/impute.log 2>&1
```

Concurrency is guarded twice: `--name` makes a second `docker run` fail
immediately, and the in-container lock file detects a stale PID with `ps`.

## Two constraints worth knowing

**The training cap fires, and the job tells you.** `predict_cp4_knn()` caps each
ESCO group's training pool at `IMPUTE_MAX_TRAIN` (default 50,000). That cap is
reached on the 24-month window — ESCO group 5223 carries about 52,700 labelled
rows. Groups above it are subsampled by a *deterministic stride*, so the result
is reproducible; raising `IMPUTE_MAX_TRAIN` uses them whole at proportionally
higher memory, since the dense `test x train` block grows with it. The job logs
how many groups are affected on every run.

This was originally an unseeded `sample.int()`, which meant two pipeline runs on
identical input returned different predictions for that group. Building this
container is what surfaced it; the fix is in `predict_cp4_knn()` itself.

**`general_id` is not unique in the source** — 4.17M rows against 3.27M distinct
ids, though no id spans two months. The read deduplicates on the latest grab
date, matching `fetch_annunci_24m_sql()` in the workflow. Without it
`predict_cp4_knn()` errors, because `factor(levels = )` rejects duplicated levels.

## Imputation quality

Imputed rows are not equally good, and `cp4_imputed` alone does not say which is
which — the value's shape does:

- **k-NN, ESCO rows**, level-4 precise: **~86% CP4 / ~88% CP3**
- **xgboost, ESCO-less rows**, `.0`-padded, level-3 only: **~77.5% CP3**
- **k-NN, ESCO rows, level 5** (`IMPUTE_CP5=1`): **~86% CP5**, on the ~87% of
  rows that get one at all

Filter on `cp2021_id_level_4 NOT LIKE '%.0'` for the level-4-precise subset. Rows
whose predicted CP3 falls in the folded `other` bucket are left `NULL` rather
than guessed, so coverage is slightly lower than the old rescue path but nothing
is invented.

`predict_cp4_knn()` also returns a well-calibrated `confidence`, and xgboost a
class probability; neither is currently written. That is the obvious follow-up if
consumers need to filter the noisier rows more finely.

## Image size

`skillviz` lists `itaposts` under `Imports:`, so installing it pulls itaposts and
duckdb, dplyr, dbplyr and processx. None are used at runtime here —
`predict_cp4_knn()` touches only data.table, Matrix and stats, and the single
live `itaposts::` call is inside `read_oja_itaposts()`. Moving `itaposts` to
`Suggests:` behind the package's existing `check_suggests()` helper would drop
all of that and remove the need for GitHub access at build time. Left alone so
this container introduces no package change.
