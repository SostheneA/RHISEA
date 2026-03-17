# Estimate Millar Theta4 Parameter

This function estimates the theta4 parameter using the Millar method.

## Usage

``` r
estimate_millar_theta4(
  class_predictions_mixed_sample,
  PHI_matrix,
  np,
  use_accelerated_em = TRUE,
  ...
)
```

## Arguments

- class_predictions_mixed_sample:

  Numeric vector of predicted classes for the mixed sample

- PHI_matrix:

  Confusion matrix used for correction

- np:

  Integer number of populations

- use_accelerated_em:

  Logical; whether to use the accelerated EM version

- ...:

  Additional arguments passed to internal functions

## Value

Numeric value of the estimated theta4 parameter

## Examples

``` r
preds <- c(1, 1, 2, 1, 2)
phi_mat <- matrix(c(0.9, 0.1, 0.1, 0.9), 2, 2)

theta4 <- estimate_millar_theta4(preds, phi_mat, np = 2)
print(theta4)
#> [1] 0.6249996 0.3750004
```
