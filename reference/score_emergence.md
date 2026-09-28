# Composite emergence score and skill state

Combines several emergence indicators into a robust composite score,
ranks the series and assigns a state label.

## Usage

``` r
score_emergence(
  indicators,
  key_cols,
  components = "pendenza",
  directions = NULL,
  weights = "equal",
  slope_col = "pendenza",
  p_col = "p_adj",
  alpha = 0.05,
  q_star = 0.9,
  age_col = NULL,
  min_age = 12,
  flag_col = NULL
)
```

## Arguments

- indicators:

  A data.frame with one row per series and the component columns.

- key_cols:

  Series key columns.

- components:

  Character vector of component columns. Default `"pendenza"`.

- directions:

  Numeric vector of +1/-1, one per component (e.g. -1 for an age, so
  that younger skills score higher). Default all +1.

- weights:

  `"equal"`, `"pc1"`, `"slope"` or a numeric vector.

- slope_col:

  Slope column used for significance and the `"slope"` weights. Default
  `"pendenza"`.

- p_col:

  Adjusted p-value column. Default `"p_adj"`.

- alpha:

  Significance level. Default `0.05`.

- q_star:

  Score quantile for the `"Emergente"` state. Default `0.9`.

- age_col:

  Optional column with the age of the series in periods (e.g. `eta_mesi`
  from
  [`detect_skill_onset()`](https://gmontaletti.github.io/skillviz/reference/detect_skill_onset.md)).
  Default `NULL`.

- min_age:

  Age below which a series is `"Nuova non consolidata"`. Default `12`.

- flag_col:

  Optional logical column marking taxonomy artefacts. Default `NULL`.

## Value

A copy of `indicators` with `z_<component>` columns, `punteggio`,
`rango` (1 = highest score) and `stato`; the weights used are stored in
the attribute `"pesi"`.

## Details

Each component \\c\\ is multiplied by its direction (+1 or -1) and
converted to a robust z-score \\(v - \mathrm{median})/\mathrm{MAD}\\.
Missing z-scores count as 0 (the median). The score is \\\sum_c w_c
z_c\\ with weights:

- `"equal"`: \\w_c = 1/C\\;

- `"pc1"`: loadings of the first principal component of the z-scores
  (complete rows), oriented so that the slope loading is positive and
  scaled to unit absolute sum;

- `"slope"`: weight 1 on `slope_col` and 0 elsewhere;

- a numeric vector with one weight per component.

States are assigned in order of precedence:

1.  `"Artefatto di tassonomia"`: `flag_col` is `TRUE`;

2.  `"Nuova non consolidata"`: `age_col` below `min_age` (recent onset,
    too short a history);

3.  `"Emergente"`: significant positive slope (`p_col < alpha`) and
    score at least the `q_star` quantile of the scores;

4.  `"In crescita"`: significant positive slope;

5.  `"In calo"`: significant negative slope;

6.  `"Stabile"`: otherwise.

## Examples

``` r
ind <- data.table::data.table(
  skill_id = letters[1:6],
  pendenza = c(0.10, 0.05, 0.00, -0.04, 0.02, 0.08),
  p_adj = c(0.001, 0.01, 0.9, 0.01, 0.5, 0.001),
  eta_mesi = c(30, 30, 30, 30, 30, 5)
)
score_emergence(ind, "skill_id", age_col = "eta_mesi", q_star = 0.8)
#>    skill_id pendenza p_adj eta_mesi z_pendenza  punteggio rango
#>      <char>    <num> <num>    <num>      <num>      <num> <int>
#> 1:        a     0.10 0.001       30  1.0960475  1.0960475     1
#> 2:        b     0.05 0.010       30  0.2529340  0.2529340     3
#> 3:        c     0.00 0.900       30 -0.5901794 -0.5901794     5
#> 4:        d    -0.04 0.010       30 -1.2646702 -1.2646702     6
#> 5:        e     0.02 0.500       30 -0.2529340 -0.2529340     4
#> 6:        f     0.08 0.001        5  0.7588021  0.7588021     2
#>                    stato
#>                   <char>
#> 1:             Emergente
#> 2:           In crescita
#> 3:               Stabile
#> 4:               In calo
#> 5:               Stabile
#> 6: Nuova non consolidata
```
