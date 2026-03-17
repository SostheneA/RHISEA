# Classify Samples Using LDF Coefficients

Assigns each sample (observation) to a population/stock based on the
highest Linear Discriminant Function (LDF) score. It also computes
posterior probabilities (or values proportional to them if priors are
not explicitly used).

## Usage

``` r
classify_samples(samples, coefs, freq = NULL, type = "S")
```

## Arguments

- samples:

  A numeric matrix of samples to be classified. Rows are observations,
  columns are variables.

- coefs:

  A numeric matrix of LDF coefficients, as returned by
  \`compute_ldf_coefficients\`. Rows are populations, columns are
  coefficients for variables followed by the constant term.

- freq:

  Optional numeric vector of frequency weights for the samples. Defaults
  to 1 for each sample if NULL. (Used mainly for HISEA compatibility).

- type:

  Character, indicates the type of run (e.g., "S" for simulation). This
  parameter is from HISEA Fortran and affects a condition for
  processing, though its impact here is minor if \`freq\` are positive.
  Default "S".

## Value

A list containing:

- class:

  An integer vector of predicted class labels (1 to NP) for each sample.

- likelihood:

  A numeric matrix where rows are samples and columns are populations.
  Contains posterior probabilities of class membership (assuming equal
  priors, rows sum to 1).

## Examples

``` r
# 1. Prepare coefficients
s1 <- matrix(rnorm(20, mean = -10), ncol = 2)
s2 <- matrix(rnorm(20, mean = -15), ncol = 2)
coefs <- compute_ldf_coefficients(list(StockA = s1, StockB = s2))

# 2. Prepare unknown samples (mixture)
unknown_samples <- matrix(rnorm(10, mean = -12), ncol = 2)
colnames(unknown_samples) <- c("Var1", "Var2")

# 3. Classify
results <- classify_samples(samples = unknown_samples, coefs = coefs)

# Check predictions and probabilities
print(results$class)
#> [1] 1 2 1 1 1
print(results$likelihood)
#>            StockA       StockB
#> [1,] 9.999962e-01 3.846787e-06
#> [2,] 5.018485e-05 9.999498e-01
#> [3,] 1.000000e+00 3.250438e-11
#> [4,] 8.953844e-01 1.046156e-01
#> [5,] 8.083537e-01 1.916463e-01
```
