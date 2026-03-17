# Read HISEA Baseline File

Reads a HISEA-formatted baseline (standard) data file. The file is
expected to contain data for multiple stocks/populations, with data for
each stock separated by a delimiter string (e.g., "NEXT STOCK"). Blank
lines and lines starting with '#' (comments) are ignored.

## Usage

``` r
read_baseline(filepath, nv)
```

## Arguments

- filepath:

  Character string, the path to the baseline \`.std\` file.

- nv:

  Integer, the number of variables (columns) expected for each
  observation in the baseline data.

## Value

A list of numeric matrices. Each matrix in the list corresponds to a
stock/population, with rows being observations and columns being
variables. Returns an empty list if no valid data is found or errors
occur during parsing.

## Examples

``` r
# Create a dummy HISEA standard file
tmp_std <- tempfile(fileext = ".std")
writeLines(c(
  "# Stock A",
  "1.2  3.4",
  "5.6  7.8",
  "NEXT STOCK",
  "# Stock B",
  "9.0  1.1"
), tmp_std)

# Read the baseline (2 variables expected)
baseline_data <- read_baseline(tmp_std, nv = 2)

# Check the results
print(baseline_data)
#> [[1]]
#>      [,1] [,2]
#> [1,]  1.2  3.4
#> [2,]  5.6  7.8
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    9  1.1
#> 
length(baseline_data) # Should be 2
#> [1] 2
```
