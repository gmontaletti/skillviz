# First month of consolidated presence of each series

The onset is the first period in which the rolling sum of counts over
`roll` consecutive periods reaches `min_count`.

## Usage

``` r
detect_skill_onset(
  panel,
  key_cols,
  time_col = "mese_idx",
  x_col = "x",
  min_count = 10,
  roll = 3L,
  censor_months = 3L,
  end = NULL
)
```

## Arguments

- panel:

  A share panel with one row per key and period, e.g. from
  [`compute_share_panel()`](https://gmontaletti.github.io/skillviz/reference/compute_share_panel.md).
  Periods with `n = 0` are ignored.

- key_cols:

  Character vector of columns identifying a series (e.g. `"skill_id"` or
  `c("cp4", "skill_id")`).

- time_col:

  Numeric period index column. Default `"mese_idx"`.

- x_col:

  Count column. Default `"x"`.

- min_count:

  Minimum rolling count defining the onset. Default `10`.

- roll:

  Rolling window length in periods. Default `3`.

- censor_months:

  Number of initial periods in which an onset is flagged as
  left-censored. Default `3`.

- end:

  Last period of the window. Default: the maximum period in `panel`.

## Value

A data.table with `key_cols`, `prima_osservazione` (first period with
`x > 0`), `prima_comparsa` (onset period, `NA` if never reached),
`eta_mesi` and `censura_sx`.

## Details

Periods missing from the panel count as zero. Before the first period of
the panel nothing is observed, so the rolling sum of the first
`roll - 1` periods covers fewer months. An onset within the first
`censor_months` periods of the panel is left-censored
(`censura_sx = TRUE`): the skill may have been present before the
observation window. `eta_mesi = end - prima_comparsa`.

## Examples

``` r
panel <- data.table::data.table(
  skill_id = "a", mese_idx = 1:8, x = c(0, 0, 1, 2, 5, 6, 8, 9)
)
detect_skill_onset(panel, key_cols = "skill_id", min_count = 10)
#> Key: <skill_id>
#>    skill_id prima_osservazione prima_comparsa eta_mesi censura_sx
#>      <char>              <num>          <int>    <int>     <lgcl>
#> 1:        a                  3              6        2      FALSE
```
