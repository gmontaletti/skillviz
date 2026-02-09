# Plot temporal skill ranking for a profession

Draws a line chart showing how skill ranks evolve over time for a single
profession. Each line represents a skill; the y-axis is the rank
position and the x-axis is the time period.

## Usage

``` r
plot_skill_ranking(serie, profession, skills = NULL)
```

## Arguments

- serie:

  A `data.table` with columns `mese` (date), `skill` (character),
  `rango` (integer rank), `professione` (character), and `N` (count).
  Typically produced by a ranking computation step.

- profession:

  Character, the profession label to filter on.

- skills:

  Optional character vector of skill labels to include. When `NULL` (the
  default), the top 10 skills by total count are used.

## Value

A `ggplot` object.

## Details

When `skills` is `NULL` the function selects the 10 most frequently
occurring skills (summing counts across all time periods).
