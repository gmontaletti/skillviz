# skillviz ![](reference/figures/logo.png)

## Overview

**skillviz** is an R package for skill co-occurrence analysis and
visualization from Online Job Advertising (OJA) data. It provides tools
for computing skill specialization indices, co-occurrence networks,
source stability filtering, salary analysis, professional distance
clustering, and temporal skill ranking. The package is built on
[ESCO](https://esco.ec.europa.eu/) and CP2021 (Italian professional
classification) taxonomies.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("gmontaletti/skillviz")
```

## Main features

| Domain                    | Key functions                                                                                                                                                                                                                                                                                                                      |
|---------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Data loading              | [`read_esco_mapping()`](https://gmontaletti.github.io/skillviz/reference/read_esco_mapping.md), [`read_isco_groups()`](https://gmontaletti.github.io/skillviz/reference/read_isco_groups.md), [`deduplicate_annunci()`](https://gmontaletti.github.io/skillviz/reference/deduplicate_annunci.md)                                   |
| Classification crosswalks | [`build_cpi_esco_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_cpi_esco_crosswalk.md), [`prepare_annunci_esco()`](https://gmontaletti.github.io/skillviz/reference/prepare_annunci_esco.md), [`prepare_annunci_geography()`](https://gmontaletti.github.io/skillviz/reference/prepare_annunci_geography.md) |
| Skill relevance           | [`compute_balassa_index()`](https://gmontaletti.github.io/skillviz/reference/compute_balassa_index.md), [`compute_skill_diffusion()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_diffusion.md), [`build_skillist()`](https://gmontaletti.github.io/skillviz/reference/build_skillist.md)                       |
| Co-occurrence networks    | [`compute_cooc_matrix()`](https://gmontaletti.github.io/skillviz/reference/compute_cooc_matrix.md), [`filter_relative_risk()`](https://gmontaletti.github.io/skillviz/reference/filter_relative_risk.md), [`cooc_all_professions()`](https://gmontaletti.github.io/skillviz/reference/cooc_all_professions.md)                     |
| Community detection       | [`detect_skill_communities()`](https://gmontaletti.github.io/skillviz/reference/detect_skill_communities.md)                                                                                                                                                                                                                       |
| Professional distance     | [`compute_profession_distance()`](https://gmontaletti.github.io/skillviz/reference/compute_profession_distance.md), [`cluster_professions()`](https://gmontaletti.github.io/skillviz/reference/cluster_professions.md)                                                                                                             |
| Salary analysis           | [`extract_salary_data()`](https://gmontaletti.github.io/skillviz/reference/extract_salary_data.md), [`compute_salary_by_period()`](https://gmontaletti.github.io/skillviz/reference/compute_salary_by_period.md), [`compute_salary_by_skill()`](https://gmontaletti.github.io/skillviz/reference/compute_salary_by_skill.md)       |
| Source stability          | [`prepare_source_tsibble()`](https://gmontaletti.github.io/skillviz/reference/prepare_source_tsibble.md), [`compute_source_features()`](https://gmontaletti.github.io/skillviz/reference/compute_source_features.md), [`filter_stable_sources()`](https://gmontaletti.github.io/skillviz/reference/filter_stable_sources.md)       |
| Temporal analysis         | [`compute_skill_ranking_series()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_ranking_series.md), [`compute_skill_variation()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_variation.md)                                                                                                   |
| Visualization             | [`plot_cooc_graph()`](https://gmontaletti.github.io/skillviz/reference/plot_cooc_graph.md), [`plot_skill_ranking()`](https://gmontaletti.github.io/skillviz/reference/plot_skill_ranking.md)                                                                                                                                       |

## Included datasets

The package ships with three pre-computed datasets:

- **`isco_gruppi`** – ESCO ISCO group classification mapping
- **`skillist`** – Master skills list with metadata (frequency, reuse
  type, domain flags)
- **`cooccorrenza`** – Pre-computed global skill co-occurrence network

## Quick example

``` r
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

    Montaletti G (2025). skillviz: Skill Co-Occurrence Analysis and Visualization
    from Online Job Advertisements. R package version 0.1.0.
    https://github.com/gmontaletti/skillviz

A BibTeX entry:

``` bibtex
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
