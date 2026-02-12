# Package index

## Data Loading and Deduplication

Functions for reading OJA data, ESCO mappings, and deduplicating
announcements.

- [`read_esco_mapping()`](https://gmontaletti.github.io/skillviz/reference/read_esco_mapping.md)
  : Read CPI-ESCO mapping table
- [`read_isco_groups()`](https://gmontaletti.github.io/skillviz/reference/read_isco_groups.md)
  : Read ESCO ISCO classification from CSV directory
- [`read_ojv_zip()`](https://gmontaletti.github.io/skillviz/reference/read_ojv_zip.md)
  : Read OJV data from ZIP files
- [`normalize_ojv()`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md)
  : Normalize OJV data from ZIP files
- [`deduplicate_annunci()`](https://gmontaletti.github.io/skillviz/reference/deduplicate_annunci.md)
  : Deduplicate job announcements

## Classification Crosswalks

Build crosswalk tables between CPI, ESCO, and CP2021 classifications.

- [`build_cpi_esco_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_cpi_esco_crosswalk.md)
  : Build ESCO-to-CP2021 level 3 crosswalk
- [`prepare_annunci_esco()`](https://gmontaletti.github.io/skillviz/reference/prepare_annunci_esco.md)
  : Merge announcements with ESCO mapping and parse dates
- [`prepare_annunci_geography()`](https://gmontaletti.github.io/skillviz/reference/prepare_annunci_geography.md)
  : Prepare announcements with CPI geographic dimension
- [`classify_esco_to_cpi()`](https://gmontaletti.github.io/skillviz/reference/classify_esco_to_cpi.md)
  : Classify unmapped ESCO L4 codes to CPI groups via Naive Bayes

## Skill Relevance Analysis

Compute specialization indices, TF-IDF diffusion scores, and skill
classification.

- [`compute_balassa_index()`](https://gmontaletti.github.io/skillviz/reference/compute_balassa_index.md)
  : Compute the Balassa index (Revealed Comparative Advantage) for
  skill-profession pairs
- [`compute_skill_diffusion()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_diffusion.md)
  : Compute skill diffusion using TF-IDF on aggregated skill frequencies
- [`build_skillist()`](https://gmontaletti.github.io/skillviz/reference/build_skillist.md)
  : Build a master skills list with metadata and diffusion
  classification
- [`compute_idf_classification()`](https://gmontaletti.github.io/skillviz/reference/compute_idf_classification.md)
  : Compute IDF classification at the document level

## Co-occurrence Networks

Build and filter skill co-occurrence matrices and networks.

- [`compute_cooc_matrix()`](https://gmontaletti.github.io/skillviz/reference/compute_cooc_matrix.md)
  : Build a raw co-occurrence sparse matrix
- [`filter_relative_risk()`](https://gmontaletti.github.io/skillviz/reference/filter_relative_risk.md)
  : Filter a co-occurrence matrix by relative risk
- [`cooc_all_professions()`](https://gmontaletti.github.io/skillviz/reference/cooc_all_professions.md)
  : Compute skill co-occurrence networks for multiple professions

## Community Detection

Identify skill communities within co-occurrence networks.

- [`detect_skill_communities()`](https://gmontaletti.github.io/skillviz/reference/detect_skill_communities.md)
  : Detect skill communities from a co-occurrence edge list

## Professional Distance and Clustering

Compute pairwise profession distances and hierarchical clustering.

- [`compute_profession_distance()`](https://gmontaletti.github.io/skillviz/reference/compute_profession_distance.md)
  : Compute pairwise distances between professions based on skill
  profiles
- [`cluster_professions()`](https://gmontaletti.github.io/skillviz/reference/cluster_professions.md)
  : Hierarchical clustering of professions

## Salary Analysis

Extract and aggregate salary data by period and skill.

- [`extract_salary_data()`](https://gmontaletti.github.io/skillviz/reference/extract_salary_data.md)
  : Extract announcements with salary information
- [`compute_salary_by_period()`](https://gmontaletti.github.io/skillviz/reference/compute_salary_by_period.md)
  : Compute salary statistics by grouping period
- [`compute_salary_by_skill()`](https://gmontaletti.github.io/skillviz/reference/compute_salary_by_skill.md)
  : Compute median salary per skill

## Source Stability

Assess and filter OJA data sources by time-series stability.

- [`prepare_source_tsibble()`](https://gmontaletti.github.io/skillviz/reference/prepare_source_tsibble.md)
  : Prepare a tsibble of announcement counts by source and month
- [`compute_source_features()`](https://gmontaletti.github.io/skillviz/reference/compute_source_features.md)
  : Compute time-series features per source
- [`filter_stable_sources()`](https://gmontaletti.github.io/skillviz/reference/filter_stable_sources.md)
  : Filter sources by stability criteria
- [`filter_annunci_by_source()`](https://gmontaletti.github.io/skillviz/reference/filter_annunci_by_source.md)
  : Filter announcements to stable sources

## Temporal Analysis

Track skill ranking evolution and rank variation over time.

- [`compute_skill_ranking_series()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_ranking_series.md)
  : Compute skill ranking per profession over time periods
- [`compute_skill_variation()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_variation.md)
  : Compute year-over-year variation in skill rankings

## Skill Extraction

Extract profession-specific skills and build skill profiles.

- [`extract_specific_skills_cpi3()`](https://gmontaletti.github.io/skillviz/reference/extract_specific_skills_cpi3.md)
  : Extract non-diffuse skills at the CPI3 profession level
- [`build_profession_skill_profile()`](https://gmontaletti.github.io/skillviz/reference/build_profession_skill_profile.md)
  : Build a comprehensive profession-skill profile table

## Visualization

Plot co-occurrence networks and skill ranking series.

- [`plot_cooc_graph()`](https://gmontaletti.github.io/skillviz/reference/plot_cooc_graph.md)
  : Plot a skill co-occurrence network graph
- [`plot_skill_ranking()`](https://gmontaletti.github.io/skillviz/reference/plot_skill_ranking.md)
  : Plot temporal skill ranking for a profession

## Datasets

Pre-computed datasets included with the package.

- [`isco_gruppi`](https://gmontaletti.github.io/skillviz/reference/isco_gruppi.md)
  : ESCO ISCO group classification
- [`skillist`](https://gmontaletti.github.io/skillviz/reference/skillist.md)
  : Master skills list
- [`cooccorrenza`](https://gmontaletti.github.io/skillviz/reference/cooccorrenza.md)
  : Skill co-occurrence network
