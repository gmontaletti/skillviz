# Symmetric Henderson weights

Weights of the symmetric Henderson moving average of a given odd length,
from the closed formula.

## Usage

``` r
henderson_weights(length)
```

## Arguments

- length:

  Odd integer \>= 3, number of terms.

## Value

A numeric vector of `length` weights, for offsets `-p..p`.

## Details

With `length = 2p + 1` and `m = p + 2`, the weight at offset \\j = -p,
\dots, p\\ is \$\$w_j = \frac{315\\\[(m-1)^2 - j^2\]\[m^2 -
j^2\]\[(m+1)^2 - j^2\] \[3m^2 - 16 -
11j^2\]}{8m(m^2-1)(4m^2-1)(4m^2-9)(4m^2-25)}.\$\$ The weights sum to
one, are symmetric, reproduce polynomials of degree up to three and
minimise the sum of squares of the third differences of the weights
(Henderson 1916; Kenny and Durbin 1982). For `length = 3` the filter is
the identity `(0, 1, 0)`.

## Examples

``` r
w <- henderson_weights(13)
round(w, 5)
#>  [1] -0.01935 -0.02786  0.00000  0.06549  0.14736  0.21434  0.24006  0.21434
#>  [9]  0.14736  0.06549  0.00000 -0.02786 -0.01935
sum(w)
#> [1] 1
```
