# Whittaker-Henderson smoother with GCV

Penalised least-squares smoother: solves \\(W + \lambda D'D) z = W y\\,
with `D` the difference matrix of a given order, and chooses \\\lambda\\
by generalised cross-validation.

## Usage

``` r
whittaker_smooth(
  y,
  lambda = NULL,
  weights = NULL,
  order = 2L,
  log10_grid = seq(0, 9, 0.1)
)
```

## Arguments

- y:

  Numeric vector; `NA` allowed (weight zero).

- lambda:

  `NULL` (choose by GCV) or a single positive smoothing parameter.

- weights:

  `NULL` (all ones) or non-negative weights of the same length as `y`.

- order:

  Order of the differences in the penalty (positive integer, default 2).

- log10_grid:

  Grid of \\\log\_{10}\lambda\\ values searched when `lambda = NULL`.

## Value

A list with `trend` (numeric, same length as `y`), `lambda` (used),
`edf` (equivalent degrees of freedom), `gcv` (GCV at `lambda`) and
`path` (data.table with `lambda`, `edf` and `gcv` over the grid; one row
when `lambda` is given).

## Details

**Missing values and weights.** `NA` values of `y` get weight zero: the
smoother interpolates them (a polynomial spline of degree
`2 * order - 1` between observed points). A weight of zero and an `NA`
give the same result.

**Limits.** As \\\lambda \to 0\\ the solution tends to `y` (at points
with positive weight); as \\\lambda \to \infty\\ it tends to the
weighted least-squares polynomial of degree `order - 1` (a straight line
for `order = 2`, which is the Hodrick-Prescott filter). For weekly
series the Ravn-Uhlig rule gives \\\lambda = 1600 \cdot 13^4\\, about
\\10^{7.66}\\, for comparison with the GCV choice.

**GCV.** For each \\\lambda = 10^g\\, `g` in `log10_grid`, \\GCV =
m\\RSS_w / (m - \mathrm{edf})^2\\, with `m` the number of positive
weights, \\RSS_w = \sum w_i (y_i - z_i)^2\\ and \\\mathrm{edf} =
\mathrm{tr}\[(W + \lambda D'D)^{-1} W\]\\. The system is solved with
sparse matrices (`Matrix`); the trace is exact, from the dense inverse
of the Cholesky factor, which costs \\O(n^3)\\ per grid value: fine for
a few hundred weeks, slow for many thousands of points. GCV assumes
independent errors and tends to undersmooth under positive
autocorrelation.

## References

Eilers, P. H. C. (2003). A perfect smoother. *Analytical Chemistry*,
75(14), 3631-3636.

## Examples

``` r
set.seed(1)
y <- cumsum(rnorm(150, 0, 0.1)) + rnorm(150, 0, 0.3)
y[c(20, 21, 90)] <- NA
wh <- whittaker_smooth(y)
log10(wh$lambda)
#> [1] 3
wh$edf
#> [1] 10.41281
# Ravn-Uhlig value for weekly data
whittaker_smooth(y, lambda = 1600 * 13^4)$edf
#> [1] 2.02552
```
