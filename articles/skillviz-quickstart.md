# Quick Start Guide

## Overview

`skillviz` provides tools for analysing skill requirements extracted
from Online Job Advertising (OJA) data. The package covers
specialisation indices, co-occurrence networks, community detection,
professional distance, and temporal ranking – all built on ESCO and
CP2021 taxonomies.

This vignette walks through the core workflow using synthetic data.

## Synthetic data

The code below creates minimal `data.table`s that mirror the column
conventions used throughout the package. In production these tables come
from ESCO-classified job advertisements.

``` r
library(skillviz)
library(data.table)

# 1. Skill occurrences per announcement -----
set.seed(42)
professions <- c("Data analyst", "Software developer", "Project manager")
skills <- c("Python", "SQL", "R", "Agile", "Communication",
            "Java", "Docker", "Budgeting", "Teamwork", "Excel")

n_rows <- 500
skills_dt <- data.table(
  general_id        = sample(seq_len(100), n_rows, replace = TRUE),
  preferredLabel    = sample(professions, n_rows, replace = TRUE),
  escoskill_level_3 = sample(skills, n_rows, replace = TRUE)
)

# 2. Aggregated profession-skill counts -----
skills_agg <- skills_dt[, .(N = .N), .(escoskill_level_3, preferredLabel)]
```

## Balassa index

The Balassa index (Revealed Comparative Advantage) measures how
concentrated a skill is within a profession relative to all professions.
Values \>= 1 indicate that the skill is specific to the profession.

``` r
# 3. Compute Balassa index -----
balassa <- compute_balassa_index(skills_dt)
head(balassa)
```

## Skill diffusion (TF-IDF)

[`compute_skill_diffusion()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_diffusion.md)
applies a TF-IDF measure to classify skills into three diffusion bands:
*alta* (widespread), *centrale* (mid-range), and *minima* (rare).

``` r
# 4. Compute skill diffusion -----
diffusion <- compute_skill_diffusion(skills_agg)
head(diffusion)
```

## Co-occurrence matrix and relative-risk filtering

The co-occurrence matrix records how often pairs of skills appear
together in the same job advertisement.
[`filter_relative_risk()`](https://gmontaletti.github.io/skillviz/reference/filter_relative_risk.md)
retains only pairs whose observed co-occurrence exceeds the independence
baseline.

``` r
# 5. Build co-occurrence matrix -----
cooc_mat <- compute_cooc_matrix(skills_dt)

# 6. Filter by relative risk -----
rr_edges <- filter_relative_risk(cooc_mat)
head(rr_edges)
```

## Community detection

[`detect_skill_communities()`](https://gmontaletti.github.io/skillviz/reference/detect_skill_communities.md)
applies a graph-based community detection algorithm and returns the
graph, community object, and membership vector.

``` r
# 7. Detect skill communities -----
comm <- detect_skill_communities(rr_edges, method = "walktrap")
comm$membership
```

## Co-occurrence graph

[`plot_cooc_graph()`](https://gmontaletti.github.io/skillviz/reference/plot_cooc_graph.md)
renders the co-occurrence network using `ggraph`. It requires the
optional packages `ggraph`, `tidygraph`, and `ggplot2`.

``` r
# 8. Plot the co-occurrence network -----
plot_cooc_graph(rr_edges, profession = "Data analyst")
```

## Profession distance and clustering

[`compute_profession_distance()`](https://gmontaletti.github.io/skillviz/reference/compute_profession_distance.md)
builds a profession-by-skill incidence matrix, applies an optional
Balassa filter, and computes pairwise distances. The input requires a
`cod_3` column (profession code) instead of `preferredLabel`.

``` r
# 9. Prepare distance input -----
distance_dt <- data.table(
  cod_3             = sample(professions, n_rows, replace = TRUE),
  escoskill_level_3 = sample(skills, n_rows, replace = TRUE)
)

# 10. Compute distance and cluster -----
prof_dist <- compute_profession_distance(distance_dt, method = "binary")
hc <- cluster_professions(prof_dist, method = "ward.D2")
plot(hc)
```

## Temporal skill ranking

[`compute_skill_ranking_series()`](https://gmontaletti.github.io/skillviz/reference/compute_skill_ranking_series.md)
ranks skills within each profession at each time period. It requires
columns `nome_3`, `escoskill_level_3`, and `gdate`.

``` r
# 11. Prepare temporal input -----
dates <- seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by = "month")
temporal_dt <- data.table(
  general_id        = sample(seq_len(200), 1000, replace = TRUE),
  nome_3            = sample(professions, 1000, replace = TRUE),
  escoskill_level_3 = sample(skills, 1000, replace = TRUE),
  gdate             = sample(dates, 1000, replace = TRUE)
)

# 12. Compute ranking and plot -----
ranking <- compute_skill_ranking_series(temporal_dt, time_unit = "quarter")
plot_skill_ranking(ranking, profession = "Data analyst")
```

## Bundled datasets

The package ships three pre-computed datasets:

- `isco_gruppi` – ESCO occupation code to profession name mapping
- `skillist` – master skill list with metadata and diffusion flags
- `cooccorrenza` – skill co-occurrence edge list (relative-risk
  filtered)

``` r
# 13. Load bundled data -----
data(cooccorrenza)
head(cooccorrenza)
```

## Further reading

See the function-level help pages (e.g.,
[`?compute_balassa_index`](https://gmontaletti.github.io/skillviz/reference/compute_balassa_index.md))
and the package README for additional details.
