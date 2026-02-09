# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**skillviz** — R package for skill co-occurrence analysis and visualization from Online Job Advertising (OJA) data. Provides functions for Balassa index computation, TF-IDF skill diffusion, co-occurrence networks, source stability filtering, salary analysis, professional clustering, and temporal skill ranking. Built on ESCO and CP2021 taxonomies.

**Author/Maintainer:** Giampaolo Montaletti (giampaolo.montaletti@gmail.com)

## Build and Test

```r
devtools::document()    # generate NAMESPACE and man/ from roxygen2
devtools::check()       # R CMD check
devtools::test()        # run testthat suite
devtools::install()     # install locally
```

## Git Operations section 

When asked to commit and push, do it quickly and directly. Do not over-analyze repo state, explore untracked files extensively, or ask unnecessary clarifying questions. If no git repo exists, create one and set up the remote without asking.


## Debugging & Bug Fixes 

When investigating bugs or inconsistencies, always deeply investigate the root cause FIRST. Do not apply superficial quick fixes. Trace the issue through the full code path before proposing changes.


## General Behavior 

Do NOT enter plan-only mode unless explicitly asked to plan. When asked to fix something, apply the fix directly. When asked to implement something, write the code — don't just write a plan document.

## Tech Stack & Conventions 

This project uses R extensively. Key tools: targets (pipeline), testthat (testing), pkgdown (docs), flexdashboard/Shiny (dashboards), tsibble/fable/fabletools (forecasting). 
Always run `devtools::test()` after modifying R package code. 
When working with PostgreSQL, always check the correct schema (usually 'public') before running queries.

## R-Specific Gotchas 

When running R scripts or targets pipelines, watch for silent error swallowing via tryCatch. If a script completes with no output or empty results, immediately check error handling before re-running.


## Module Organization

| File | Domain | Key Functions |
|------|--------|---------------|
| `R/read_data.R` | Data loading | `read_esco_mapping()`, `read_isco_groups()`, `deduplicate_annunci()` |
| `R/crosswalk.R` | Classification mapping | `build_cpi_esco_crosswalk()`, `prepare_annunci_esco()`, `prepare_annunci_geography()` |
| `R/relevance.R` | Skill analysis | `compute_balassa_index()`, `compute_skill_diffusion()`, `build_skillist()`, `compute_idf_classification()` |
| `R/cooccurrence.R` | Co-occurrence networks | `cooc_all_professions()`, `compute_cooc_matrix()`, `filter_relative_risk()` |
| `R/salary.R` | Salary analysis | `extract_salary_data()`, `compute_salary_by_period()`, `compute_salary_by_skill()` |
| `R/source_stability.R` | Source filtering | `prepare_source_tsibble()`, `compute_source_features()`, `filter_stable_sources()` |
| `R/communities.R` | Community detection | `detect_skill_communities()` |
| `R/distance.R` | Professional distance | `compute_profession_distance()`, `cluster_professions()` |
| `R/extract.R` | Skill extraction | `extract_specific_skills_cpi3()`, `build_profession_skill_profile()` |
| `R/temporal.R` | Temporal analysis | `compute_skill_ranking_series()`, `compute_skill_variation()` |
| `R/plot.R` | Visualization | `plot_cooc_graph()`, `plot_skill_ranking()` |
| `R/zzz.R` | Package startup | `globalVariables`, `.onLoad` (setDTthreads 75%) |
| `R/utils.R` | Internal helpers | `check_columns()`, `check_suggests()`, `parse_ymd_columns()` |
| `R/data.R` | Data documentation | roxygen2 docs for `isco_gruppi`, `skillist`, `cooccorrenza` |

## Dependencies

- **Imports:** data.table, fst, igraph, Matrix, collapse
- **Suggests:** ggplot2, ggraph, tidygraph, tsibble, feasts, fable, fabletools, readxl, RPostgres, DBI, lubridate, testthat

Functions using Suggests packages call `check_suggests()` before use.

## Coding Conventions

- data.table for all data manipulation, no tidyverse in core functions
- Section comments: `# 1. section name -----`
- Internal functions prefixed with `.` (e.g., `.cooc_single`)
- All exported functions have roxygen2 docs with `@param`, `@return`, `@export`
- No hardcoded file paths — all data passed as function parameters

## Related Projects

- **n_annunci** (`../n_annunci/`) — original exploratory scripts
- **skillviz_workflow** (`../skillviz_workflow/`) — targets pipeline using this package
