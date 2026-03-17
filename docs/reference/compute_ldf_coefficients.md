# Compute Linear Discriminant Function (LDF) Coefficients

Computes the coefficients for Fisher's Linear Discriminant Function for
each group (population/stock). This implementation assumes a common
(pooled) covariance matrix across all groups, a standard assumption for
LDA. The LDF is of the form: LDF_j(x) = C_j0 + C_j1\*x1 + ... +
C_jp\*xp.

## Usage

``` r
compute_ldf_coefficients(baseline)
```

## Arguments

- baseline:

  A list of numeric matrices, where each matrix represents the baseline
  (training) data for one population/stock. Each matrix should have
  observations as rows and variables as columns.

## Value

A numeric matrix of LDF coefficients. Rows correspond to populations,
and columns correspond to variables followed by the constant term. The
dimensions are (np x (nv + 1)).

## Examples

``` r
# Create dummy baseline data for 2 stocks with 2 variables (isotopes)
# Stock A
s1 <- matrix(c(-10, 2, -11, 2.1, -10.5, 2.2), ncol = 2)
colnames(s1) <- c("d13c", "d18o")
# Stock B
s2 <- matrix(c(-15, 5, -16, 5.1, -15.5, 5.2), ncol = 2)
colnames(s2) <- c("d13c", "d18o")

baseline_list <- list(StockA = s1, StockB = s2)

# Compute coefficients
ldf_coeffs <- compute_ldf_coefficients(baseline_list)
print(ldf_coeffs)
#>             d13c      d18o  Constant
#> StockA -41.23654 -40.91595 -172.8622
#> StockB -51.08841 -50.68277 -265.3082
```
