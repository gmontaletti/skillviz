# Robust trend smoothing with post-smoothing bands and re-smoothing

Extracts a trend from each weekly series (one per key) with one of eight
smoothers, places a band around the fitted values (trend + seasonal),
flags the observed weeks outside the band, sets them to missing and
smooths again. No cleaning is applied before the first smoothing.

## Usage

``` r
robust_smooth(
  dt,
  key_cols,
  time_col,
  value_col,
  method = c("henderson", "loess", "gam_ar1", "whittaker", "level", "llt", "smooth",
    "smooth_t"),
  transform = c("log1p", "log", "none"),
  period = 365.25/7,
  harmonics = NULL,
  max_harmonics = 6L,
  n_backfit = 3L,
  band = c("model", "mad"),
  z = 3.5,
  k = 3.5,
  max_iter = 2L,
  run_min = 4L,
  nu = 4,
  level = 0.95,
  flag_na = c("all", "isolated"),
  q_select = c("gaussian", "profile_t"),
  log10_q_grid = seq(-6, 1, by = 0.25),
  damping = c("none", "profile"),
  phi_grid = c(seq(0.8, 0.98, by = 0.02), 0.99, 1)
)
```

## Arguments

- dt:

  A data.frame or data.table with one row per key x week.

- key_cols:

  Character vector of key columns identifying each series.

- time_col:

  Name of the time column (class Date or IDate, weekly).

- value_col:

  Name of the value column (counts for `log1p`, positive for `log`);
  `NA` allowed.

- method:

  Smoother: `"henderson"`, `"loess"`, `"gam_ar1"`, `"whittaker"`,
  `"level"`, `"llt"`, `"smooth"` or `"smooth_t"`.

- transform:

  Scale of the model: `"log1p"` (default), `"log"` or `"none"`.

- period:

  Seasonal period in weeks (default 365.25 / 7).

- harmonics:

  Fixed number of Fourier harmonics, or `NULL` to choose it by AICc of
  the smooth model.

- max_harmonics:

  Largest number of harmonics considered when `harmonics = NULL`.

- n_backfit:

  Number of backfitting iterations.

- band:

  `"model"` (auxiliary-residual band for Kalman methods, MAD band
  otherwise) or `"mad"` (MAD band for every method).

- z:

  Half-width multiplier of the model band (in standard deviations).

- k:

  Half-width multiplier of the MAD band (in robust standard deviations,
  `1.4826 * MAD`).

- max_iter:

  Maximum number of re-smoothings after pass 1 (non-negative integer; 0
  = pass 1 only).

- run_min:

  Minimum length in weeks of a run of flags that is not `isolated`.

- nu:

  Degrees of freedom of the Student-t weights (`smooth_t`).

- level:

  Confidence level of `trend_lo`, `trend_hi`.

- flag_na:

  `"all"` (every flagged week is set to missing in the next pass) or
  `"isolated"` (only the weeks of `isolated` runs).

- q_select:

  Choice of `q` for `smooth_t`: `"gaussian"` (default; the ML value of
  the Gaussian smooth trend model) or `"profile_t"` (maximum of the
  profile objective under the Student-t model, see Details). Other
  methods accept only the default.

- log10_q_grid:

  Grid of \\\log\_{10} q\\ values searched when
  `q_select = "profile_t"`.

- damping:

  `"none"` (default, \\\phi = 1\\) or `"profile"` (damped slope,
  \\\phi\\ profiled jointly with `q` on `phi_grid`; requires
  `q_select = "profile_t"`, method `smooth_t` only).

- phi_grid:

  Grid of damping factors in (0, 1\] searched when
  `damping = "profile"`.

## Value

A data.table ordered by key and time with columns `key_cols`, `time_col`
(IDate), `observed` (transformed scale, `NA` for missing weeks),
`seasonal`, `trend`, `trend_se` (`NA` for `henderson`, `loess`,
`whittaker`), `trend_lo`, `trend_hi`, `remainder`
(`observed - seasonal - trend`), `trend_pass1`, `band_lo`, `band_hi`,
`excluded`, `flag_1`, `flag_2` (`NA` when pass 2 was not run), `flag`,
`flag_type` (`NA` for unflagged weeks), `pass` (final pass of the key),
`method`, `weight` (final Student-t weight, `smooth_t` only) and
`flag_weight` (`weight < (nu + 1) / (nu + z^2)`). Attributes: `fits`
(one row per key x pass with `pass`, `K`, `aicc_K`, `parameter`,
`value`, `criterion`, `criterion_value`, `edf`, `rho`, `ic_ratio`,
`var_level`, `var_slope`, `var_obs`, `band`, `band_defined`,
`n_excluded`, `n_flag`, `iterations` (IRLS, `smooth_t`),
`irls_converged`, `q_select`, `phi`, `loglik_profile`, `q_at_edge`,
`phi_at_edge` (`smooth_t`, see Details), `final`, `converged`, `error`),
`flags` (one row per run of `flag` with `start`, `end`, `n_weeks`,
`flag_type`, `shift`, `half_width`), `transform`, `observed_transform`,
`key_cols`, `time_col`, `period`, `method` and `band`.

## Details

**Scale.** The series is modelled on the scale of `transform`. `log1p`
keeps zero weeks; in the output attributes it is recorded as
`transform = "log"` (and `observed_transform = "log1p"`), so that
[`trend_index()`](https://gmontaletti.github.io/skillviz/reference/trend_index.md)
with `transform = "log"` and
[`trend_diagnostics()`](https://gmontaletti.github.io/skillviz/reference/trend_diagnostics.md)
treat the log1p trend as a log trend: the index is `exp` of differences
of `log(1 + n)`, close to differences of `log(n)` when counts are large.

**Seasonality.** The seasonal component is a Fourier regression with
period `period` and `t` the number of weeks since 1970-01-01 (as in
[`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)).
The number of harmonics `K` is chosen once per key and pass by the AICc
of the `smooth` Kalman model
([`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md)
with `method = "smooth"`, on the series with the excluded weeks set to
`NA`), or fixed by `harmonics`, and is shared by every method, so that
methods differ only in the trend filter. The seasonal coefficients are
estimated by backfitting (`n_backfit` iterations, as in
[`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md))
with the trend filter of the method, except `gam_ar1`, which has the
harmonics in its formula.

**Methods** (each smoothing parameter is chosen from the data):

- `henderson`:
  [`henderson_filter()`](https://gmontaletti.github.io/skillviz/reference/henderson_filter.md)
  with the length chosen by GCV among 13, 23 and 39 (lengths longer than
  the series are skipped) and the Musgrave end weights with the I/C
  ratio of a preliminary 13-term pass. Missing weeks of the input are
  linearly interpolated; excluded weeks are replaced by the fitted
  values (trend + seasonal) of the previous pass (X-11 style).

- `loess`: `stats::loess(degree = 1, family = "symmetric")` on time,
  with the span chosen on the grid `0.06, 0.08, ..., 0.50` by the GCV of
  the gaussian fit, `n RSS / (n - trace.hat)^2`; `surface = "direct"` so
  that the trend extends to missing weeks at the ends.

- `gam_ar1`:
  `mgcv::bam(y ~ s(t, bs = "cr", k = 40) + Fourier, rho, AR.start, method = "fREML")`
  (`k` reduced on short series), with `rho` on the grid
  `0, 0.05, ..., 0.9` chosen at the minimum fREML score; `AR.start`
  marks the first week after each missing stretch. The trend is the
  intercept plus the spline, `trend_se` its standard error from the
  Bayesian covariance matrix. Requires mgcv.

- `whittaker`:
  [`whittaker_smooth()`](https://gmontaletti.github.io/skillviz/reference/whittaker_smooth.md)
  of order 2 with \\\lambda\\ by GCV; missing weeks get weight zero.

- `level`: Kalman local level, `stats::StructTS(type = "level")` and
  [`stats::KalmanSmooth()`](https://rdrr.io/r/stats/KalmanLike.html), by
  maximum likelihood.

- `llt`, `smooth`: the local linear trend and the smooth trend model
  (level variance 0) of
  [`extract_trend()`](https://gmontaletti.github.io/skillviz/reference/extract_trend.md),
  by maximum likelihood.

- `smooth_t`: robust Kalman smoother. It starts from the `smooth` ML fit
  (seasonal component, observation variance \\\hat h\\, signal-to-noise
  ratio `q = var_slope / var_obs`), then iterates (at most 20 times) a
  Kalman filter and Rauch-Tung-Striebel smoother of the same model with
  time-varying observation variance \\h_t = \hat h / w_t\\, with
  Student-t weights \\w_t = (\nu + 1) / (\nu + s_t^2)\\, \\s_t^2 =
  (e_t^2 + P\_{t\|n}) / \hat h\\ (EM weights; \\e_t\\ the auxiliary
  residual, \\P\_{t\|n}\\ the smoothed level variance), and \\\hat h\\
  re-estimated as the weighted mean of \\e_t^2 + P\_{t\|n}\\. With
  `q_select = "gaussian"` (default) `q` is kept fixed, so the smoothness
  of the ML fit is kept and only the weights change. The seasonal
  coefficients are re-estimated by weighted least squares in the
  backfitting. The final weights are returned in `weight`.

**`smooth_t`: q under the t model and damped slope.** With
`q_select = "profile_t"`, `q` is chosen on the grid \\10^g\\, `g` in
`log10_q_grid`, by maximising \$\$L(q, \phi) = \ell_G(q, \phi; h_t =
\hat h / w_t) + \sum_t \log \mathrm{Gamma}(w_t; \nu/2, \nu/2),\$\$
where, for each grid point, the IRLS is run to convergence from \\w_t =
1\\ and the \\\hat h\\ of the Gaussian fit, \\\ell_G\\ is the Gaussian
log-likelihood of the seasonally adjusted series by prediction-error
decomposition under observation variance \\h_t\\ and slope variance \\q
\hat h\\, with the first two prediction errors (the diffuse elements of
the initial state) left out for every grid point, and the sum runs over
the observed weeks (Gamma density with shape and rate \\\nu/2\\). To
keep one IRLS per grid point, the seasonal component is estimated once
by the backfitting at the Gaussian `q` and held fixed during the grid
search; the full backfitting is then run at the selected point. With
`damping = "profile"` (requires `q_select = "profile_t"`) the slope is
damped, \\\beta\_{t+1} = \phi \beta_t + \eta_t\\, \\\eta_t \sim N(0, q
\hat h)\\, level variance 0, with both states diffuse at the start for
every \\\phi\\, and \\(q, \phi)\\ is profiled jointly on `log10_q_grid`
x `phi_grid`. `fits` reports `q_select`, the selected `q` (`value`),
`phi`, `loglik_profile` (the objective at the selected point, with the
seasonal of the grid search), `q_at_edge` (selected `q` at the minimum
or maximum of the grid), `phi_at_edge` (selected \\\phi \< 1\\ at the
minimum of `phi_grid`; \\\phi = 1\\ is not an edge), and the IRLS
`iterations` and `irls_converged` of the final fit. The grid search
multiplies the cost of a `smooth_t` pass by about the number of grid
points.

**Bands.** With `band = "model"`, the Kalman methods (`level`, `llt`,
`smooth`, `smooth_t`) use the auxiliary-residual band
`trend + seasonal +/- z * sqrt(v_t)`. For an observed week used in the
fit, \\v_t = \hat h - P\_{t\|n}\\ (variance of the auxiliary residual,
\\P\_{t\|n}\\ the smoothed trend variance). In general \\v_t = (1 -
k_t)^2 \hat h + (1 - k_t) P\_{t\|n}\\ with \\k_t = P\_{t\|n} / h_t\\ the
weight of the week in its own smoothed trend: it equals \\\hat h -
P\_{t\|n}\\ when \\h_t = \hat h\\, and \\\hat h + P\_{t\|n}\\
(prediction variance) for weeks excluded from the fit (\\k_t = 0\\). For
`smooth_t`, \\h_t = \hat h / w_t\\: a down-weighted week is judged
against the scale \\\hat h\\, not against its inflated \\h_t\\. The
seasonal coefficients are treated as known. Negative values of \\v_t\\
from rounding are set to 0. When \\\hat h\\ is not finite or not above
`1e-8` times the variance of the series of the pass (the ML fit
interpolates the data, as happens for `level` and `llt` when the
observation variance is estimated at 0), the band is undefined:
`band_lo`, `band_hi` and the flags of the key are `NA`, no further pass
is run, `band_defined` is `FALSE` in `fits`, `converged` is `NA`, and a
single warning lists the keys concerned. Non-Kalman methods with
`band = "model"`, and every method with `band = "mad"`, use
`trend + seasonal +/- k * 1.4826 * MAD(remainder)`, the MAD computed on
the weeks used in the fit of that pass.

**Weights.** For `smooth_t`, `weight` holds the final Student-t weight
\\w_t\\ of the weeks used in the final fit and `flag_weight` is \\w_t \<
(\nu + 1) / (\nu + z^2)\\, i.e. \\s_t \> z\\: the week is down-weighted
as much as a standardised residual beyond the band multiplier `z`. Both
are `NA` for other methods and for weeks not used in the final fit.

**Passes.** Pass 1 smooths the series as observed and flags the observed
weeks outside the band (`flag_1`). Pass 2 sets the flagged weeks to `NA`
(with `flag_na = "isolated"`, only the weeks of `isolated` runs) and
smooths again, re-choosing `K` and the smoothing parameters; the flags
are recomputed on all observed weeks against the pass-2 fit (`flag_2`),
so a week can be unflagged. If the set of weeks to exclude changes, a
further pass is run with the new set, up to `max_iter` re-smoothings
(pass `max_iter + 1`). When the set of flags of pass 1 is empty, pass 1
is final. `flag` holds the flags against the final fit (consistent with
`band_lo`, `band_hi`) and `excluded` the weeks set to missing in the
final fit; they coincide when the procedure converged (column
`converged` of `fits`).

**Flag types** (per run of consecutive flagged weeks of `flag`): `final`
when the run touches the last 4 weeks of the series; otherwise
`isolated` when it is shorter than `run_min` weeks; otherwise
`level_shift` when the median of the 8 unflagged observed weeks after
the run minus the median of the 8 before it (on the seasonally adjusted
scale, `observed - seasonal`) is at least the median band half-width
over the run in absolute value, and `interruption` when it is smaller or
cannot be computed (run at the start of the series).

**Cost.** Each pass chooses `K` with
`(max_harmonics + 1) * (n_backfit + 1)` fits of the smooth Kalman model,
then fits the method (`n_backfit + 1` trend fits; `gam_ar1`: one `bam`
per value of `rho`).

## Examples

``` r
set.seed(1)
weeks <- data.table::as.IDate("2023-01-02") + 7L * (0:119)
tw <- as.numeric(weeks) / 7
mu <- 6 + 0.002 * seq_along(weeks) + 0.2 * sin(2 * pi * tw / (365.25 / 7))
n <- stats::rpois(120, exp(mu))
n[c(40, 80)] <- n[c(40, 80)] * 15
d <- data.table::data.table(source = "a", week = weeks, n = n)
rs <- robust_smooth(d, "source", "week", "n", method = "whittaker",
  harmonics = 1L)
rs[flag == TRUE, list(week, flag_type)]
#>          week flag_type
#>        <IDat>    <char>
#> 1: 2023-10-02  isolated
#> 2: 2024-07-08  isolated
attr(rs, "fits")
#>    source    method  pass     K    aicc_K parameter        value criterion
#>    <char>    <char> <int> <int>     <num>    <char>        <num>    <char>
#> 1:      a whittaker     1     1  112.4410    lambda 1.000000e+09       gcv
#> 2:      a whittaker     2     1 -384.6391    lambda 6.309573e+04       gcv
#>    criterion_value      edf   rho ic_ratio var_level var_slope var_obs   band
#>              <num>    <num> <num>    <num>     <num>     <num>   <num> <char>
#> 1:     0.119262266 2.000494    NA       NA        NA        NA      NA    mad
#> 2:     0.001646131 3.666668    NA       NA        NA        NA      NA    mad
#>    band_defined n_excluded n_flag iterations irls_converged q_select   phi
#>          <lgcl>      <int>  <int>      <int>         <lgcl>   <char> <num>
#> 1:         TRUE          0      2         NA             NA     <NA>    NA
#> 2:         TRUE          2      2         NA             NA     <NA>    NA
#>    loglik_profile q_at_edge phi_at_edge  final converged  error
#>             <num>    <lgcl>      <lgcl> <lgcl>    <lgcl> <char>
#> 1:             NA        NA          NA  FALSE      TRUE   <NA>
#> 2:             NA        NA          NA   TRUE      TRUE   <NA>
idx <- trend_index(rs, "source", "week",
  base_period = c("2023-01-02", "2023-12-31"), transform = "log")
```
