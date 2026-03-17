# Run HISEA Estimations Only

Run HISEA Estimations Only

## Usage

``` r
run_hisea_estimates(
  pseudo_classes,
  likelihoods,
  phi_matrix,
  np,
  type = "ANALYSIS",
  nsamps = 1000,
  stocks_names = NULL,
  export_csv = FALSE,
  output_dir = ".",
  verbose = FALSE
)
```

## Arguments

- pseudo_classes:

  Vector of predicted classes (integers)

- likelihoods:

  Matrix of prediction probabilities

- phi_matrix:

  Confusion matrix (Phi)

- np:

  Number of populations

- type:

  "ANALYSIS", "SIMULATION" or "BOOTSTRAP"

- nsamps:

  Number of samples (default: 1000)

- stocks_names:

  Names of stocks/populations

- export_csv:

  Export results to CSV

- output_dir:

  Output directory

- verbose:

  Print progress messages

## Value

List containing estimates and metrics

## Examples

``` r
# Advanced estimation using probability matrices
dummy_probs <- matrix(runif(150), ncol = 3)
dummy_probs <- dummy_probs / rowSums(dummy_probs)
dummy_classes <- max.col(dummy_probs)

results <- run_hisea_estimates(
  pseudo_classes = dummy_classes,
  likelihoods = dummy_probs,
  phi_matrix = diag(3),
  np = 3, type = "ANALYSIS"
)
print(results$mean_estimates)
#>          RAW COOK COOKC   EM        ML
#> Stock_1 0.48 0.48  0.48 0.48 0.6686370
#> Stock_2 0.28 0.28  0.28 0.28 0.2008567
#> Stock_3 0.24 0.24  0.24 0.24 0.1305062
```
