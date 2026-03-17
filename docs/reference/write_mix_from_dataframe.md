# Write Mixture Data to File

Write Mixture Data to File

## Usage

``` r
write_mix_from_dataframe(df, var_cols_mix, file_path = "hisea.mix")
```

## Arguments

- df:

  Data frame

- var_cols_mix:

  Variables to write

- file_path:

  Output file path

## Value

None

## Examples

``` r
# Load mixture data
data(mixture)

# Define a temporary path
tmp_mix <- tempfile(fileext = ".mix")

# Export to HISEA format
write_mix_from_dataframe(
  mixture,
  var_cols_mix = c("d13c_ukn", "d18o_ukn"),
  file_path = tmp_mix
)

# Check if file was created
file.exists(tmp_mix)
#> [1] TRUE
```
