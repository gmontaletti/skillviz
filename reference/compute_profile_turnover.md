# Turnover of profession skill profiles

Measures how much the skill profile of each profession changed between
the base and the target window, net of sampling noise.

## Usage

``` r
compute_profile_turnover(pair)
```

## Arguments

- pair:

  Output of
  [`build_profile_pair()`](https://gmontaletti.github.io/skillviz/reference/build_profile_pair.md).

## Value

A data.table with one row per profession: the profession column,
`n_base`, `n_target`, `n_skill`, `turnover_coseno`, `d2`, `d2_atteso`,
`turnover_netto`.

## Details

With \\a\\ and \\b\\ the base and target profiles of a profession:

- `turnover_coseno` \\= 1 - a \cdot b / (\\a\\ \\b\\)\\;

- `d2` \\= \sum_k (a_k - b_k)^2\\;

- `d2_atteso` \\= \sum_k s_k (1 - s_k) (1/n_a + 1/n_b)\\, the expected
  value of `d2` when both windows share the pooled profile \\s = (n_a
  a + n_b b)/(n_a + n_b)\\ (binomial sampling noise);

- `turnover_netto` \\= \max(0, d2 - E\[d2\]) / \sum_k \bar s_k^2\\, with
  \\\bar s = (a + b)/2\\, a scale-free change net of noise.

## Examples

``` r
panel <- data.table::data.table(
  cp4 = "p1", skill_id = rep(c("a", "b", "c"), 2),
  mese_idx = rep(1:2, each = 3), x = c(50, 50, 0, 50, 0, 50), n = 100
)
pair <- build_profile_pair(panel, "cp4", base = c(1, 1), target = c(2, 2))
compute_profile_turnover(pair)
#>       cp4 n_base n_target n_skill turnover_coseno    d2 d2_atteso
#>    <char>  <num>    <num>   <int>           <num> <num>     <num>
#> 1:     p1    100      100       3             0.5   0.5    0.0125
#>    turnover_netto
#>             <num>
#> 1:            1.3
```
