# Read HISEA Mixture File

Reads a HISEA-formatted mixture data file. It attempts to extract lines
containing only numeric data (typical for HISEA mixture files) and
converts them into a numeric matrix.

## Usage

``` r
read_mixture(filepath = "hisea.mix", nv)
```

## Arguments

- filepath:

  Character string, the path to the mixture \`.mix\` file.

- nv:

  Integer, the number of variables (columns) expected for each
  observation in the mixture data.

## Value

A numeric matrix where rows are observations and columns are variables.
Returns an empty matrix (with \`nv\` columns) if no valid data is found
or errors occur.

## Examples

``` r
# Create a dummy HISEA mixture file
tmp_mix <- tempfile(fileext = ".mix")
writeLines(c(
  "1.1  2.2",
  "3.3  4.4",
  "5.5  6.6"
), tmp_mix)

# Read the mixture (2 variables expected)
mixture_matrix <- read_mixture(tmp_mix, nv = 2)

# Check the results
print(mixture_matrix)
#>      [,1] [,2]
#> [1,]  1.1  2.2
#> [2,]  3.3  4.4
#> [3,]  5.5  6.6
nrow(mixture_matrix) # Should be 3
#> [1] 3
```
