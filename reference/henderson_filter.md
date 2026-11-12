# Henderson trend filter with Musgrave end weights

Applies a Henderson moving average to a complete series, with the
symmetric weights in the interior and the Musgrave asymmetric weights at
the first and last `p` points. The length can be chosen among candidates
by generalised cross-validation.

## Usage

``` r
henderson_filter(x, length = NULL, lengths = c(13L, 23L, 39L), ic_ratio = NULL)
```

## Arguments

- x:

  Numeric vector, complete (no `NA`) and finite.

- length:

  `NULL` (choose by GCV among `lengths`) or a single odd integer \>= 3
  not longer than `x`.

- lengths:

  Candidate odd lengths when `length = NULL`.

- ic_ratio:

  `NULL` (estimate) or a non-negative I/C ratio for the Musgrave end
  weights.

## Value

A list with `trend` (numeric, same length as `x`), `length` (chosen
length), `ic_ratio` (used), `edf` (trace of the smoother matrix), `gcv`
(GCV of the chosen length) and `path` (data.table with `length`, `edf`
and `gcv` for every candidate evaluated).

## Details

**Interior and ends.** For a length `H = 2p + 1`, points `p + 1` to
`n - p` get the symmetric weights of
[`henderson_weights()`](https://gmontaletti.github.io/skillviz/reference/henderson_weights.md).
At the end point with `q < p` future observations, the Musgrave weights
on the `M = p + 1 + q` available observations are (Doherty 2001) \$\$u_i
= w_i + \frac{1}{M}\sum\_{j=M+1}^{N} w_j + \frac{(i -
\frac{M+1}{2})\\D}{1 + \frac{M(M-1)(M+1)}{12}D}
\sum\_{j=M+1}^{N}\left(j - \frac{M+1}{2}\right) w_j,\$\$ with \\D = 4 /
(\pi R^2)\\ and `R` the I/C ratio (mean absolute weekly change of the
irregular over that of the trend). The weights sum to one. They minimise
the expected revision under a locally linear trend with noise, so they
reproduce a straight line exactly only in the limit `R -> 0`; for
`R > 0` the slope enters as a penalty, not a constraint. The first `p`
points use the mirrored weights.

**I/C ratio.** When `ic_ratio = NULL`, `R` is estimated from a
preliminary 13-term Henderson pass with `R = 3.5` (the X-11 value for
that length): `R = mean(|diff(x - C)|) / mean(|diff(C)|)`, `C` the
preliminary trend. Series shorter than 13 use the longest odd length
that fits. The same `R` is used for every candidate length.

**Length.** When `length = NULL`, each candidate of `lengths` not longer
than the series is applied and the one with the smallest \\GCV = n\\RSS
/ (n - \mathrm{tr}\\S)^2\\ is kept, where `S` is the linear smoother
matrix (symmetric rows in the interior, Musgrave rows at the ends) and
\\\mathrm{tr}\\S\\ its trace (the equivalent degrees of freedom). GCV
assumes independent errors; with positively autocorrelated noise it
tends to choose short filters.

**Missing values.** The filter needs a complete series: `x` with `NA`
raises an error. Fill missing values first (e.g. linear interpolation,
or the fitted values of a previous pass as in X-11).

## References

Doherty, M. (2001). The surrogate Henderson filters in X-11. *Australian
and New Zealand Journal of Statistics*, 43(4), 385-392.

Musgrave, J. C. (1964). A set of end weights to end all end weights.
Working paper, U.S. Bureau of the Census.

## Examples

``` r
set.seed(1)
x <- sin(seq(0, 4 * pi, length.out = 120)) + rnorm(120, 0, 0.2)
h <- henderson_filter(x)
h$length
#> [1] 23
h$path
#>    length      edf        gcv
#>     <int>    <num>      <num>
#> 1:     13 29.26902 0.03831919
#> 2:     23 17.79198 0.03671435
#> 3:     39 11.10826 0.03729414
h13 <- henderson_filter(x, length = 13L, ic_ratio = 3.5)
head(h13$trend)
#> [1] 0.005961282 0.104230080 0.220273681 0.339850516 0.454927050 0.557178899
```
