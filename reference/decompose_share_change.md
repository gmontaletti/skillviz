# Decompose the change of a share into within-source and composition

Applies the dynamic Olley-Pakes decomposition of Melitz and Polanec
(2015) to the aggregate share of each key between two periods (or two
windows of periods), with sources in the role of firms.

## Usage

``` r
decompose_share_change(
  counts,
  key_cols,
  source_col = "source",
  time_col,
  t0,
  t1,
  x_col = "x",
  n_col = "n",
  source_totals = NULL
)
```

## Arguments

- counts:

  data.frame or data.table with one row per key, source and period,
  holding the count `x_col` and, when `source_totals` is `NULL`, the
  source denominator `n_col`.

- key_cols:

  Character vector of key columns.

- source_col:

  Name of the source column (default `"source"`).

- time_col:

  Name of the period column.

- t0, t1:

  Values of `time_col` of the base and the comparison period; vectors of
  several values define windows, over which counts and denominators are
  summed by source. They must not overlap.

- x_col, n_col:

  Names of the count and denominator columns (defaults `"x"`, `"n"`).

- source_totals:

  Optional data.frame with one row per source and period and columns
  `source_col`, `time_col`, `n_col` (postings of the source in the
  period, also where the key has no posting). Without it the
  denominators are read from `counts` and must be the same for all keys
  of a source and period.

## Value

A data.table with one row per key of `counts` and columns `key_cols`,
`p0`, `p1`, `delta`, `entro`, `fra`, `ingressi`, `uscite`,
`composizione`. The attribute `fonti` lists the sources with their
denominators `n0`, `n1` and class `classe` (`"C"` continuing, `"E"`
entering, `"X"` exiting).

## Details

The aggregate share is `P = sum_s w_s p_s` with source weights
`w_s = n_s / sum(n)` and source shares `p_s = x_s / n_s`. Sources are
continuing (C: `n > 0` in both periods), entering (E: only in `t1`) or
exiting (X: only in `t0`). With `W_G` the weight of group `G` and `P_G`
its aggregate share, Melitz and Polanec (2015) write

`P_1 - P_0 = (P_C1 - P_C0) + W_E1 (P_E1 - P_C1) + W_X0 (P_C0 - P_X0)`

and split the change among continuing sources as in Olley and Pakes
(1996), with weights renormalised within C (`omega_s = n_s / sum_C n`)
and averages between the two periods (bars):

- `entro = sum_C mean(omega_s) (p_s1 - p_s0)`: change of the shares
  within sources;

- `fra = sum_C mean(p_s) (omega_s1 - omega_s0)`: reallocation of
  postings among continuing sources;

- `ingressi = W_E1 (P_E1 - P_C1)`, `uscite = W_X0 (P_C0 - P_X0)`.

The four components sum to `delta = p1 - p0` up to rounding error;
`composizione = fra + ingressi + uscite`. Groups without sources
contribute 0; when there is no continuing source `P_C` is set to 0, so
that `ingressi = p1` and `uscite = -p0`.

## References

Melitz, M. J. and Polanec, S. (2015). Dynamic Olley-Pakes productivity
decomposition with entry and exit. *RAND Journal of Economics*, 46(2),
362-375.

Olley, G. S. and Pakes, A. (1996). The dynamics of productivity in the
telecommunications equipment industry. *Econometrica*, 64(6), 1263-1297.

## Examples

``` r
counts <- data.frame(
  code = "a",
  source = c("s1", "s2", "s1", "s2", "s3"),
  mese = c(1, 1, 2, 2, 2),
  x = c(10, 5, 12, 20, 30)
)
totals <- data.frame(
  source = c("s1", "s2", "s1", "s2", "s3"),
  mese = c(1, 1, 2, 2, 2),
  n = c(100, 100, 100, 200, 100)
)
decompose_share_change(counts, "code", time_col = "mese", t0 = 1,
                       t1 = 2, source_totals = totals)
#>      code    p0    p1 delta  entro          fra   ingressi uscite composizione
#>    <char> <num> <num> <num>  <num>        <num>      <num>  <num>        <num>
#> 1:      a 0.075 0.155  0.08 0.0375 -0.005833333 0.04833333      0       0.0425
```
