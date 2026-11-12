# Trend extraction with Fourier seasonality for weekly series

Splits each weekly series (one per key) into a deterministic annual
seasonal component, a trend and a remainder, on a transformed scale (log
for totals, logit for shares). The seasonal component is a Fourier
regression whose number of harmonics is chosen by AICc and estimated
jointly with the trend by backfitting.

## Usage

``` r
extract_trend(
  dt,
  key_cols,
  time_col,
  value_col,
  transform = c("log", "logit", "none"),
  method = c("llt", "smooth", "ets", "stl"),
  period = 365.25/7,
  harmonics = NULL,
  max_harmonics = 6L,
  n_backfit = 3L,
  level = 0.95
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

  Name of the value column (positive for `log`, in (0, 1) for `logit`);
  `NA` allowed.

- transform:

  Scale of the model: `"log"`, `"logit"` or `"none"`.

- method:

  Trend method: `"llt"` (default), `"smooth"` (local linear trend with
  level variance fixed at zero), `"ets"` or `"stl"`.

- period:

  Seasonal period in weeks (default 365.25 / 7).

- harmonics:

  Fixed number of Fourier harmonics, or `NULL` to choose it by AICc.
  Ignored by `stl`.

- max_harmonics:

  Largest number of harmonics considered when `harmonics = NULL`.

- n_backfit:

  Number of backfitting iterations (non-negative integer).

- level:

  Confidence level of `trend_lo` and `trend_hi` (`llt` and `smooth`
  only).

## Value

A data.table with columns `key_cols`, `time_col` (IDate), `observed`
(transformed scale, `NA` for missing weeks), `seasonal`, `trend`
(smoothed for `llt`), `trend_filtered`, `trend_se`, `trend_lo`,
`trend_hi`, `remainder` (`observed - seasonal - trend`), `imputed`
(logical, `TRUE` where the input was missing) and `method`, ordered by
key and time. Attribute `fits`: one row per key with `method`, `K`,
`aicc`, `var_level`, `var_slope`, `var_obs`, `convergence`,
`zero_variance`, `fixed_variance`, `boundary`, `retried`, `start`,
`degenerate`, `interpolated`, `n_obs`, `n_na` and `error`. Attributes
`transform`, `key_cols`, `time_col` and `period` record the call.

## Details

**Grid and missing weeks.** Within each key, times must be on a weekly
grid; missing weeks (absent rows or `NA` values, e.g. outlier weeks set
to `NA` upstream) are kept as `NA` in `observed` and flagged in
`imputed`. The trend at those weeks is the model prediction (`llt`,
`smooth`) or comes from a linear interpolation of the input used for
fitting (`ets`, `stl`); the per-key flag `interpolated` in the `fits`
attribute marks the latter.

**Seasonality.** For `llt`, `smooth` and `ets`, the seasonal component
is \\s_t = \sum\_{k=1}^K a_k \sin(2\pi k t/P) + b_k \cos(2\pi k t/P)\\,
with `t` the number of weeks since 1970-01-01 (so the phase is common to
all keys) and `P = period`. When `harmonics = NULL`, `K` ranges over
`0..max_harmonics` and is chosen by \\AICc = -2\ell + 2p +
2p(p+1)/(n-p-1)\\, where \\\ell\\ is the log-likelihood of the trend
model on the seasonally adjusted series and `p` counts the trend
parameters plus `2K` seasonal coefficients. The backfitting starts from
an OLS fit of a quadratic time trend plus the harmonics, then
`n_backfit` times fits the trend on `y - s` and regresses `y - trend` on
the harmonics; a final trend fit on the last adjusted series gives the
output.

**Methods.**

- `llt` (reference): local linear trend,
  `stats::StructTS(type = "trend")` on the adjusted series. `trend` is
  the smoothed (two-sided) level from
  [`stats::KalmanSmooth()`](https://rdrr.io/r/stats/KalmanLike.html),
  `trend_se` its smoothed standard deviation, `trend_filtered` the
  filtered (one-sided) level. `NA` weeks are handled by the Kalman
  filter.

- `smooth`: smooth trend model, the local linear trend of `llt` with the
  level disturbance variance fixed at zero
  (`stats::StructTS(type = "trend", fixed = c(0, NA, NA))`, parameters
  in the order level, slope, epsilon). The level then has no shocks of
  its own and the trend is an integrated random walk: only the slope
  changes from week to week. Its smoothed level is a cubic-spline-type
  smoother whose smoothness is set by the signal-to-noise ratio
  `q = var_slope / var_obs`, estimated by maximum likelihood (no
  smoothing parameter is chosen by hand). Smoothing, `trend_se`,
  optimiser starts and treatment of `NA` weeks are those of `llt`;
  `var_level` is reported as `0` and `fixed_variance` as `"level"`.

- `ets`: `fable::ETS(error("A") + trend(c("A", "Ad")) + season("N"))` on
  the adjusted series, with `NA` weeks linearly interpolated (ETS does
  not accept missing values). `trend` and `trend_filtered` are both the
  filtered level; `trend_se` is `NA`. Requires tsibble, fable and
  fabletools.

- `stl`: `stats::stl(s.window = "periodic", robust = TRUE)` with
  frequency 52 on the interpolated series. It has its own periodic
  seasonal component (no Fourier terms, `K = NA`) and a 52-week period
  instead of 52.18: a descriptive benchmark only.

**Boundary estimates and failed fits.** A variance estimated at zero is
a legitimate boundary maximum-likelihood estimate, not a failure: a zero
slope variance means a deterministic drift, a zero level variance an
integrated random walk (smooth) trend, a zero observation variance a
series without measurement noise (e.g. a moving sum). For `llt` and
`smooth`, `zero_variance` lists the estimated variances at zero (below
`1e-8` times the variance of the first differences) and `boundary` is
`TRUE` when there is at least one; the level variance of `smooth` is
fixed by the model, not estimated, so it is listed in `fixed_variance`
and never in `zero_variance` or `boundary`; for `ets`, `zero_variance`
lists the smoothing parameters (`alpha`, `beta`) at their lower bound
`1e-4` and `boundary` flags them. The `llt` (and `smooth`) likelihood is
maximised from two starts: the StructTS default and the series rescaled
to unit standard deviation of its first differences, with initial
variances (level 0.5, slope 0.01, observation 0.5) on that scale and an
optimiser `parscale` of 0.01 for the slope variance; the variances are
back-transformed. The default start alone often stops at a spurious zero
slope variance with a lower likelihood. Among the converged starts
(`convergence == 0`, finite log-likelihood) the one with the highest
log-likelihood is kept (`start`); `retried` is `TRUE` when the default
start did not converge. When neither start converges and the best one
has variances at zero, the model is refitted with those variances fixed
at zero (`start = "boundary"`); the refit is kept when it converges
without loss of likelihood, which confirms a boundary maximum.
`degenerate` marks failed fits only: for `llt`, when no start converges
(the best finite one is then returned). Leading missing weeks are
excluded from the estimation (StructTS needs an observed first value)
and filled by the Kalman smoother. For `ets`, `degenerate` marks a
non-finite log-likelihood. No fallback method is used: when a fit errors
(e.g. on a deterministic series), the key keeps `NA` components,
`degenerate` is `TRUE`, the message is stored in `error` and a warning
is raised.

**Uncertainty.** `trend_se` (and `trend_lo`, `trend_hi`) is the
model-based standard error of the smoothed level, conditional on the
estimated variances and seasonal coefficients. It is valid only when the
observed series is not itself a moving sum: for stock-type series
(moving sums of inflow, such as the stock of open postings), the
innovations are moving averages by construction and `trend_se`
understates the uncertainty. Use it on inflow-type series only.

**Cost.** With `harmonics = NULL`, each key requires
`(max_harmonics + 1) * (n_backfit + 1)` trend fits (each with two
optimiser starts for `llt` and `smooth`). The AICc counts 3 trend
parameters for `llt` and 2 for `smooth`.

## Examples

``` r
set.seed(1)
weeks <- data.table::as.IDate("2023-05-01") + 7L * (0:155)
tw <- as.numeric(weeks) / 7
d <- data.table::data.table(
  source = "a",
  week = weeks,
  n = exp(5 + 0.003 * seq_along(weeks) +
    0.2 * sin(2 * pi * tw / (365.25 / 7)) + rnorm(156, 0, 0.03))
)
tr <- extract_trend(d, "source", "week", "n", max_harmonics = 2L)
head(tr)
#>    source       week observed   seasonal    trend trend_filtered    trend_se
#>    <char>     <IDat>    <num>      <num>    <num>          <num>       <num>
#> 1:      a 2023-05-01 5.160749 0.18052902 4.987748       4.980220 0.001636927
#> 2:      a 2023-05-08 5.175483 0.16846276 4.991509       4.998774 0.000780673
#> 3:      a 2023-05-15 5.132962 0.15395670 4.995264       4.990640 0.001014808
#> 4:      a 2023-05-22 5.191788 0.13722092 4.999014       5.021635 0.001235456
#> 5:      a 2023-05-29 5.137803 0.11849782 5.002755       5.025673 0.001443171
#> 6:      a 2023-06-05 5.085656 0.09805854 5.006486       5.017155 0.001638417
#>    trend_lo trend_hi    remainder imputed method
#>       <num>    <num>        <num>  <lgcl> <char>
#> 1: 4.984540 4.990956 -0.007528133   FALSE    llt
#> 2: 4.989979 4.993039  0.015512034   FALSE    llt
#> 3: 4.993276 4.997253 -0.016258887   FALSE    llt
#> 4: 4.996592 5.001435  0.055553513   FALSE    llt
#> 5: 4.999926 5.005583  0.016550324   FALSE    llt
#> 6: 5.003275 5.009698 -0.018888662   FALSE    llt
attr(tr, "fits")
#>    source method     K      aicc var_level    var_slope      var_obs
#>    <char> <char> <int>     <num>     <num>        <num>        <num>
#> 1:      a    llt     1 -648.7773         0 6.098826e-09 0.0007024439
#>    convergence zero_variance fixed_variance boundary retried    start
#>          <int>        <char>         <char>   <lgcl>  <lgcl>   <char>
#> 1:           0         level                    TRUE   FALSE rescaled
#>    degenerate interpolated n_obs  n_na  error
#>        <lgcl>       <lgcl> <int> <int> <char>
#> 1:      FALSE        FALSE   156     0   <NA>
# smooth trend model: level variance fixed at 0
ts_ <- extract_trend(d, "source", "week", "n", method = "smooth",
  harmonics = 1L)
attr(ts_, "fits")[, c("var_level", "var_slope", "var_obs", "fixed_variance")]
#>    var_level    var_slope      var_obs fixed_variance
#>        <num>        <num>        <num>         <char>
#> 1:         0 6.063695e-09 0.0007042898          level
```
