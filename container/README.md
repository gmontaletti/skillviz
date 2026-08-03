# Containerised CP4 imputation

Standalone job: reads OJA postings from the Postgres database, fills the missing
CP2021 level-4 codes with `skillviz::predict_cp4_knn()`, and writes the result to
the `staging` schema.

Unlike the `skillviz_workflow` targets pipeline — which reads the local DuckDB
store — this container talks to Postgres directly and is meant to run from cron.

## What it writes

`staging.gm_cp4_imputed`, one row per deduplicated posting:

| column | type | meaning |
|---|---|---|
| `general_id` | bigint, primary key | posting identifier |
| `year_grab_date`, `month_grab_date` | int | month key, indexed |
| `cp2021_id_level_4` | text | the vendor's level-4 code where it exists, the imputed one otherwise |
| `cp4_imputed` | smallint | `0` vendor-supplied, `1` algorithmic, `NULL` still unknown |

`cp4_imputed` is `NULL` exactly when `cp2021_id_level_4` is `NULL`.

### One column, two precisions

A single column carries every code, but imputed values come from two models and
are not equally precise:

| value shape | source | precision |
|---|---|---|
| `5.1.2.2` with `cp4_imputed = 0` | vendor | level 4 |
| `5.1.2.2` with `cp4_imputed = 1` | k-NN, rows **with** an ESCO code | level 4 |
| `5.1.2.0` with `cp4_imputed = 1` | xgboost, rows **without** an ESCO code | **level 3 only** |

**A trailing `.0` means the code is only accurate to CP3.** The padding is
unambiguous: of 510 real CP4 codes none ends in `.0`, and no padded CP3 collides
with a real CP4, so `substring(code, 1, 5)` is always a valid CP3 and
`code NOT LIKE '%.0'` selects the level-4-precise rows.

### Which model handles which rows

Each segment gets the model that wins on it, measured on 60,002 held-out rows at
CP3 level:

| rows | model | accuracy | incumbent |
|---|---|---|---|
| **with** `idesco_level_4` | k-NN (`predict_cp4_knn`) | **88.3%** | xgboost 85.0% |
| **without** `idesco_level_4` | vtreat + xgboost | **77.5%** | k-NN 66.8% |

Where an ESCO code exists, restricting candidates to the CP4 codes observed for
that exact ESCO L4 beats impact-coded ESCO. Where none exists, that restriction
is unavailable and the covariates the k-NN ignores — city, sector, source,
contract, education, salary — win instead. The xgboost path is also the only one
that can classify a posting with **no skills at all** (81.5% vs 69.7%).

The model is refitted from scratch on each full run; there is no artefact to
manage. CP3 codes with fewer than `IMPUTE_XGB_RARE_MIN` labelled examples are
folded into an `other` bucket and those rows are left unimputed rather than
guessed.

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
