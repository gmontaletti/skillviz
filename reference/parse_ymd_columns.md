# Parse year/month/day columns into IDate

Parse year/month/day columns into IDate

## Usage

``` r
parse_ymd_columns(dt, prefix, col_name)
```

## Arguments

- dt:

  data.table with year, month, day integer columns

- prefix:

  Column prefix (e.g. "grab_date" or "expire_date")

- col_name:

  Name for the output IDate column

## Value

data.table with added IDate column (modified by reference)
