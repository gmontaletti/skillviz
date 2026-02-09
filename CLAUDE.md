# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

**skillviz** — R package for skill co-occurrence analysis and
visualization from Online Job Advertising (OJA) data. Provides functions
for Balassa index computation, TF-IDF skill diffusion, co-occurrence
networks, source stability filtering, salary analysis, professional
clustering, and temporal skill ranking. Built on ESCO and CP2021
taxonomies.

**Author/Maintainer:** Giampaolo Montaletti
(<giampaolo.montaletti@gmail.com>)

## Build and Test

``` r
devtools::document()    # generate NAMESPACE and man/ from roxygen2
devtools::check()       # R CMD check
devtools::test()        # run testthat suite
devtools::install()     # install locally
```

## Git Operations section

When asked to commit and push, do it quickly and directly. Do not
over-analyze repo state, explore untracked files extensively, or ask
unnecessary clarifying questions. If no git repo exists, create one and
set up the remote without asking.

## Debugging & Bug Fixes

When investigating bugs or inconsistencies, always deeply investigate
the root cause FIRST. Do not apply superficial quick fixes. Trace the
issue through the full code path before proposing changes.

## General Behavior

Do NOT enter plan-only mode unless explicitly asked to plan. When asked
to fix something, apply the fix directly. When asked to implement
something, write the code — don’t just write a plan document.

## Tech Stack & Conventions

This project uses R extensively. Key tools: targets (pipeline), testthat
(testing), pkgdown (docs), flexdashboard/Shiny (dashboards),
tsibble/fable/fabletools (forecasting). Always run `devtools::test()`
after modifying R package code. When working with PostgreSQL, always
check the correct schema (usually ‘public’) before running queries.

## R-Specific Gotchas

When running R scripts or targets pipelines, watch for silent error
swallowing via tryCatch. If a script completes with no output or empty
results, immediately check error handling before re-running.

## Module Organization

| File                   | Domain                 | Key Functions                                                                                                                                                                                                                                                                                                                                                                                                                  |
|------------------------|------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `R/read_data.R`        | Data loading           | [`read_esco_mapping()`](https://gmontaletti.github.io/skillviz/reference/read_esco_mapping.md), [`read_isco_groups()`](https://gmontaletti.github.io/skillviz/reference/read_isco_groups.md), [`deduplicate_annunci()`](https://gmontaletti.github.io/skillviz/reference/deduplicate_annunci.md)                                                                                                                               |
| `R/crosswalk.R`        | Classification mapping | [`build_cpi_esco_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_cpi_esco_crosswalk.md), [`prepare_annunci_esco()`](https://gmontaletti.github.io/skillviz/reference/prepare_annunci_esco.md), [`prepare_annunci_geography()`](https://gmontaletti.github.io/skillviz/reference/prepare_annunci_geography.md)                                                                                             |
| `R/relevance.R`        | Skill analysis         | [`compute_balassa_index()`](https://gmontaletti.github.io/skillviz/reference/compute_balassa_index.md), [`compute_skill_diffusion()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_diffusion.md), [`build_skillist()`](https://gmontaletti.github.io/skillviz/reference/build_skillist.md), [`compute_idf_classification()`](https://gmontaletti.github.io/skillviz/reference/compute_idf_classification.md) |
| `R/cooccurrence.R`     | Co-occurrence networks | [`cooc_all_professions()`](https://gmontaletti.github.io/skillviz/reference/cooc_all_professions.md), [`compute_cooc_matrix()`](https://gmontaletti.github.io/skillviz/reference/compute_cooc_matrix.md), [`filter_relative_risk()`](https://gmontaletti.github.io/skillviz/reference/filter_relative_risk.md)                                                                                                                 |
| `R/salary.R`           | Salary analysis        | [`extract_salary_data()`](https://gmontaletti.github.io/skillviz/reference/extract_salary_data.md), [`compute_salary_by_period()`](https://gmontaletti.github.io/skillviz/reference/compute_salary_by_period.md), [`compute_salary_by_skill()`](https://gmontaletti.github.io/skillviz/reference/compute_salary_by_skill.md)                                                                                                   |
| `R/source_stability.R` | Source filtering       | [`prepare_source_tsibble()`](https://gmontaletti.github.io/skillviz/reference/prepare_source_tsibble.md), [`compute_source_features()`](https://gmontaletti.github.io/skillviz/reference/compute_source_features.md), [`filter_stable_sources()`](https://gmontaletti.github.io/skillviz/reference/filter_stable_sources.md)                                                                                                   |
| `R/communities.R`      | Community detection    | [`detect_skill_communities()`](https://gmontaletti.github.io/skillviz/reference/detect_skill_communities.md)                                                                                                                                                                                                                                                                                                                   |
| `R/distance.R`         | Professional distance  | [`compute_profession_distance()`](https://gmontaletti.github.io/skillviz/reference/compute_profession_distance.md), [`cluster_professions()`](https://gmontaletti.github.io/skillviz/reference/cluster_professions.md)                                                                                                                                                                                                         |
| `R/extract.R`          | Skill extraction       | [`extract_specific_skills_cpi3()`](https://gmontaletti.github.io/skillviz/reference/extract_specific_skills_cpi3.md), [`build_profession_skill_profile()`](https://gmontaletti.github.io/skillviz/reference/build_profession_skill_profile.md)                                                                                                                                                                                 |
| `R/temporal.R`         | Temporal analysis      | [`compute_skill_ranking_series()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_ranking_series.md), [`compute_skill_variation()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_variation.md)                                                                                                                                                                                               |
| `R/plot.R`             | Visualization          | [`plot_cooc_graph()`](https://gmontaletti.github.io/skillviz/reference/plot_cooc_graph.md), [`plot_skill_ranking()`](https://gmontaletti.github.io/skillviz/reference/plot_skill_ranking.md)                                                                                                                                                                                                                                   |
| `R/zzz.R`              | Package startup        | `globalVariables`, `.onLoad` (setDTthreads 75%)                                                                                                                                                                                                                                                                                                                                                                                |
| `R/utils.R`            | Internal helpers       | [`check_columns()`](https://gmontaletti.github.io/skillviz/reference/check_columns.md), [`check_suggests()`](https://gmontaletti.github.io/skillviz/reference/check_suggests.md), [`parse_ymd_columns()`](https://gmontaletti.github.io/skillviz/reference/parse_ymd_columns.md)                                                                                                                                               |
| `R/data.R`             | Data documentation     | roxygen2 docs for `isco_gruppi`, `skillist`, `cooccorrenza`                                                                                                                                                                                                                                                                                                                                                                    |

## Dependencies

- **Imports:** data.table, fst, igraph, Matrix, collapse
- **Suggests:** ggplot2, ggraph, tidygraph, tsibble, feasts, fable,
  fabletools, readxl, RPostgres, DBI, lubridate, testthat

Functions using Suggests packages call
[`check_suggests()`](https://gmontaletti.github.io/skillviz/reference/check_suggests.md)
before use.

## Coding Conventions

- data.table for all data manipulation, no tidyverse in core functions
- Section comments: `# 1. section name -----`
- Internal functions prefixed with `.` (e.g., `.cooc_single`)
- All exported functions have roxygen2 docs with `@param`, `@return`,
  `@export`
- No hardcoded file paths — all data passed as function parameters

## Related Projects

- **n_annunci** (`../n_annunci/`) — original exploratory scripts
- **skillviz_workflow** (`../skillviz_workflow/`) — targets pipeline
  using this package
