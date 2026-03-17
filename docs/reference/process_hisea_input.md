# Process HISEA Input Data

Process HISEA Input Data

## Usage

``` r
process_hisea_input(
  input,
  type = c("std", "mix"),
  nv = NULL,
  stock_col = NULL,
  var_cols_mix = NULL,
  var_cols_std = NULL,
  stocks_names = NULL
)
```

## Arguments

- input:

  Input data (file path or data frame)

- type:

  Type of input ("std" or "mix")

- nv:

  Number of variables

- stock_col:

  Stock column name

- var_cols_mix:

  Variables for mixture data

- var_cols_std:

  Variables for standard data

- stocks_names:

  Stock names

## Value

Processed data

## Examples

``` r
# Essential: load the data first
data(baseline)

# Preparing baseline data for HISEA logic
processed <- process_hisea_input(
  input = baseline,
  type = "std",
  nv = 2,
  stock_col = "population",
  var_cols_std = c("d13c", "d18o")
)
```
