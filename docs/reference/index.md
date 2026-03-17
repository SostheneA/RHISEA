# Package index

## Main Analysis Functions

Core functions to run HISEA analyses

- [`run_hisea_all()`](https://sosthenea.github.io/RHISEA/reference/run_hisea_all.md)
  : Run HISEA Mixed Stock Analysis
- [`run_hisea_estimates()`](https://sosthenea.github.io/RHISEA/reference/run_hisea_estimates.md)
  : Run HISEA Estimations Only
- [`simulate_mixture()`](https://sosthenea.github.io/RHISEA/reference/simulate_mixture.md)
  : Simulate a Mixed Fishery Sample from Known Stock Proportions
- [`create_hisea_summary_report()`](https://sosthenea.github.io/RHISEA/reference/create_hisea_summary_report.md)
  : Create HISEA Summary Report

## Data Input and Output

Tools to prepare and process data

- [`read_baseline()`](https://sosthenea.github.io/RHISEA/reference/read_baseline.md)
  : Read HISEA Baseline File
- [`read_mixture()`](https://sosthenea.github.io/RHISEA/reference/read_mixture.md)
  : Read HISEA Mixture File
- [`write_mix_from_dataframe()`](https://sosthenea.github.io/RHISEA/reference/write_mix_from_dataframe.md)
  : Write Mixture Data to File
- [`write_std_from_dataframe()`](https://sosthenea.github.io/RHISEA/reference/write_std_from_dataframe.md)
  : Write Standard Data to File
- [`process_hisea_input()`](https://sosthenea.github.io/RHISEA/reference/process_hisea_input.md)
  : Process HISEA Input Data
- [`baseline`](https://sosthenea.github.io/RHISEA/reference/baseline.md)
  : Baseline Data for RHISEA
- [`mixture`](https://sosthenea.github.io/RHISEA/reference/mixture.md) :
  Mixture dataset for RHISEA examples

## Estimators and Algorithms

Statistical implementations

- [`accel_em_algorithm()`](https://sosthenea.github.io/RHISEA/reference/accel_em_algorithm.md)
  : Accelerated Expectation-Maximization (EM) Algorithm
- [`em_algorithm()`](https://sosthenea.github.io/RHISEA/reference/em_algorithm.md)
  : Standard Expectation-Maximization (EM) Algorithm for Stock
  Proportions
- [`compute_cook_estimators()`](https://sosthenea.github.io/RHISEA/reference/compute_cook_estimators.md)
  : Compute Cook Estimators for Stock Proportions
- [`estimate_millar_theta4()`](https://sosthenea.github.io/RHISEA/reference/estimate_millar_theta4.md)
  : Estimate Millar Theta4 Parameter
- [`estimate_ml_theta5()`](https://sosthenea.github.io/RHISEA/reference/estimate_ml_theta5.md)
  : Estimate Stock Composition Using Direct Maximum Likelihood (Theta5)
- [`get_phi()`](https://sosthenea.github.io/RHISEA/reference/get_phi.md)
  : Get Phi Matrix

## Classification & Visualization

- [`classify_samples()`](https://sosthenea.github.io/RHISEA/reference/classify_samples.md)
  : Classify Samples Using LDF Coefficients
- [`compute_ldf_coefficients()`](https://sosthenea.github.io/RHISEA/reference/compute_ldf_coefficients.md)
  : Compute Linear Discriminant Function (LDF) Coefficients
- [`ordvec()`](https://sosthenea.github.io/RHISEA/reference/ordvec.md) :
  Generate Ordered Random Values (Order Statistics based)
- [`plot_hisea_theta()`](https://sosthenea.github.io/RHISEA/reference/plot_hisea_theta.md)
  : Plot HISEA theta estimates with error bars
  utils::globalVariables(c("Stock", "Mean", "Estimator", "SD",
  "ActualValue"))
- [`print_hisea_summary()`](https://sosthenea.github.io/RHISEA/reference/print_hisea_summary.md)
  : Print HISEA Summary Report to Console and/or File

## Internal Helpers

- [`.resample_baseline_data_helper()`](https://sosthenea.github.io/RHISEA/reference/dot-resample_baseline_data_helper.md)
  : Helper: resample baseline data per stock
