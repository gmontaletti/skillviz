# Skill co-occurrence network

Pre-computed global skill co-occurrence edges with relative-risk
filtering applied. Each row represents a pair of skills that co-occur in
job announcements above the expected baseline.

## Usage

``` r
cooccorrenza
```

## Format

A data.table with columns:

- from:

  First skill in the pair

- to:

  Second skill in the pair

- weight:

  Co-occurrence count (filtered by relative risk \> 1)

## Source

Computed from OJA data via crossprod on term-document matrix
