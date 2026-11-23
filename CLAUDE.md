# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**skillviz** — R package for skill co-occurrence analysis and visualization from Online Job Advertising (OJA) data. Provides functions for Balassa index computation, TF-IDF skill diffusion, co-occurrence networks, source stability filtering, salary analysis, professional clustering, and temporal skill ranking. Built on ESCO and CP2021 taxonomies.

**Author/Maintainer:** Giampaolo Montaletti (giampaolo.montaletti@gmail.com)

## Build and Test

```r
devtools::document()    # generate NAMESPACE and man/ from roxygen2
devtools::test()        # run testthat suite
devtools::install()     # install locally

# R CMD check, with the SAME arguments as CI. A plain devtools::check() is
# weaker than the workflow: r-lib/actions/check-r-package@v2 defaults to
# rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning"), so
# those flags are NOT in R-CMD-check.yaml -- they come from the action.
devtools::check(args = c("--no-manual", "--as-cran"))
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


## Dependencies

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
