# Run HISEA Mixed Stock Analysis

This is the main wrapper function and core set of utilities for running
the HISEA mixed-stock analysis framework, allowing simulation, analysis,
or bootstrap estimation of stock composition from mixture samples.

Supported operation modes: - \*\*SIMULATION\*\*: Simulate mixtures based
on known proportions and evaluate performance of classification and
estimators. - \*\*ANALYSIS\*\*: Apply trained classifier to real mixture
data to estimate stock proportions. - \*\*BOOTSTRAP\*\*: Resample real
mixture to evaluate variability of estimates.

Supported classifiers: LDA, QDA, Random Forest, SVM, k-NN, ANN, XGBoost,
Naive Bayes, Mclust, MLR. Supported estimators: RAW, Cook, Constrained
Cook, EM (Millar), Maximum Likelihood.

Includes integrated 10-fold cross-validation and model quality
evaluation (accuracy, kappa, F1, etc.).

## Usage

``` r
run_hisea_all(
  type = "ANALYSIS",
  np,
  nv,
  seed_val = 123456,
  var_cols_std = NULL,
  var_cols_mix = NULL,
  stock_col = NULL,
  nsamps = 1000,
  Nmix = 100,
  actual = NULL,
  baseline_input = NULL,
  mix_input = NULL,
  export_csv = FALSE,
  output_dir = ".",
  verbose = FALSE,
  method_class = "LDA",
  stocks_names = NULL,
  resample_baseline = FALSE,
  resampled_baseline_sizes = NULL,
  phi_method = c("standard", "cv"),
  mclust_model_names = NULL,
  mclust_perform_cv = TRUE,
  ...
)
```

## Arguments

- type:

  Character. "SIMULATION", "ANALYSIS" or "BOOTSTRAP".

- np:

  Integer. Number of populations (stocks).

- nv:

  Integer. Number of variables.

- seed_val:

  Integer. Random seed for reproducibility.

- var_cols_std:

  Character vector of column names for baseline variables.

- var_cols_mix:

  Character vector of column names for mixture variables.

- stock_col:

  Character name of stock column in baseline data.

- nsamps:

  Integer. Number of replicates.

- Nmix:

  Integer. Sample size of the simulated mixture (for SIMULATION only).

- actual:

  Numeric vector. True proportions used in simulation.

- baseline_input:

  Data frame or file path for baseline data.

- mix_input:

  Data frame or file path for mixture data.

- export_csv:

  Logical. Whether to export summary and confusion matrix to CSV.

- output_dir:

  Character. Output directory.

- verbose:

  Logical. Print progress messages.

- method_class:

  Character. Classification method (e.g., "LDA", "RF", "SVM", etc.).

- stocks_names:

  Character vector. Optional vector of stock names.

- resample_baseline:

  Logical. Resample the baseline for each replicate.

- resampled_baseline_sizes:

  Integer vector. Sizes of resamples per stock.

- phi_method:

  Character. "standard" or "cv" (cross-validation-based confusion
  matrix).

- mclust_model_names:

  Character vector. Models to test with Mclust.

- mclust_perform_cv:

  Logical. Whether to cross-validate Mclust.

- ...:

  Additional arguments passed to the underlying classification models
  (e.g., ntree for Random Forest, cost for SVM).

## Value

A list of length 8 containing the statistical summary of the estimation
(the same as the \`estimation_summary\` element in the saved file):

- mean_estimates:

  Matrix \[np x nsamps\] of mean estimated proportions.

- sd_estimates:

  Standard deviations of the estimates.

- mse_estimates:

  Mean Squared Error (if applicable).

- var_emp:

  Empirical variance of the estimates.

- covar_ml:

  Maximum Likelihood covariance matrix.

- cor_ml:

  Correlation matrix.

- covar_inv_ml:

  Inverse of the covariance matrix.

- det_covar_ml:

  Determinant of the covariance matrix (checks for singularity).

## Saved Results Structure

The function automatically saves a \`.rda\` file in \`output_dir\`
containing a master list named \`out\`. This list includes:

- estimation_summary:

  The list of 8 statistical metrics described above.

- classification_model:

  The trained classifier object (e.g., LDA, RF).

- baseline_classification_quality:

  Accuracy, Kappa, and F1 scores.

- phi_matrix:

  The confusion matrix used for bias correction.

- mixture_classification_details:

  Predicted classes and posterior probabilities.

## Examples

``` r
data(baseline)
data(mixture)

res <- run_hisea_all(
  baseline_input = baseline,
  mix_input      = mixture,
  stock_col      = "population",
  var_cols_std   = c("d13c", "d18o"),
  var_cols_mix   = c("d13c_ukn", "d18o_ukn"),
  output_dir     = tempdir(),
  np = 2, nv = 2, nsamps = 5, Nmix = 50, method_class = "LDA"
)
print(res$mean_estimates)
#>               RAW      COOK     COOKC        EM        ML
#> Stock_1 0.3771111 0.3376263 0.3376263 0.3376264 0.3216781
#> Stock_2 0.6228889 0.6623737 0.6623737 0.6623736 0.6783219
```
