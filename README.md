
# skillviz <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/gmontaletti/skillviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gmontaletti/skillviz/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

**skillviz** is an R package for skill co-occurrence analysis and visualization
from Online Job Advertising (OJA) data. It provides tools for computing skill
specialization indices, co-occurrence networks, source stability filtering,
salary analysis, professional distance clustering, and temporal skill ranking.
The package is built on [ESCO](https://esco.ec.europa.eu/) and CP2021 (Italian
professional classification) taxonomies.

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("gmontaletti/skillviz")
```

## Main features

| Domain | Key functions |
|--------|---------------|
| Data loading | `read_esco_mapping()`, `read_isco_groups()`, `deduplicate_annunci()` |
| Classification crosswalks | `build_cpi_esco_crosswalk()`, `prepare_annunci_esco()`, `prepare_annunci_geography()` |
| Skill relevance | `compute_balassa_index()`, `compute_skill_diffusion()`, `build_skillist()` |
| Co-occurrence networks | `compute_cooc_matrix()`, `filter_relative_risk()`, `cooc_all_professions()` |
| Community detection | `detect_skill_communities()` |
| Professional distance | `compute_profession_distance()`, `cluster_professions()` |
| Salary analysis | `extract_salary_data()`, `compute_salary_by_period()`, `compute_salary_by_skill()` |
| Source stability | `prepare_source_tsibble()`, `compute_source_features()`, `filter_stable_sources()` |
| Temporal analysis | `compute_skill_ranking_series()`, `compute_skill_variation()` |
| Visualization | `plot_cooc_graph()`, `plot_skill_ranking()` |

## Included datasets

The package ships with three pre-computed datasets:

- **`isco_gruppi`** -- ESCO ISCO group classification mapping
- **`skillist`** -- Master skills list with metadata (frequency, reuse type, domain flags)
- **`cooccorrenza`** -- Pre-computed global skill co-occurrence network

## Quick example

```r
library(skillviz)
library(data.table)

# 1. Load included data -----
data(cooccorrenza)
data(skillist)

# 2. Build co-occurrence graph -----
graph <- plot_cooc_graph(cooccorrenza, top_n = 50)
```

## Citation

To cite skillviz in publications, use:

```
Montaletti G (2025). skillviz: Skill Co-Occurrence Analysis and Visualization
from Online Job Advertisements. R package version 0.1.0.
https://github.com/gmontaletti/skillviz
```

A BibTeX entry:

```bibtex
@Manual{skillviz,
  title = {skillviz: Skill Co-Occurrence Analysis and Visualization from Online Job Advertisements},
  author = {Giampaolo Montaletti},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/gmontaletti/skillviz},
}
```

## License

MIT
