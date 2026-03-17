# Create HISEA Summary Report

Generates a comprehensive summary from HISEA estimation results,
including means, standard deviations, MSE (for simulations), and
covariance/correlation for ML estimates (often for bootstrap).

## Usage

``` r
create_hisea_summary_report(
  all_estimates_array,
  actual_proportions = NULL,
  run_type
)
```

## Arguments

- all_estimates_array:

  An array of dimension (nsamps, np, n_estimators) containing all
  estimation results. The 3rd dimension should have estimator names.

- actual_proportions:

  Optional numeric vector of true stock proportions. Required if MSE is
  to be calculated (typically for "SIMULATION" run_type).

- run_type:

  Character string, e.g., "SIMULATION", "ANALYSIS", "BOOTSTRAP". Used to
  decide if actual_proportions are expected for MSE.

## Value

A list containing the summary statistics.

## Examples

``` r
# 1. Create a dummy estimation array (10 samples, 2 stocks, 3 estimators)
nsamps <- 10; np <- 2; nest <- 3
est_array <- array(runif(nsamps * np * nest), dim = c(nsamps, np, nest))
dimnames(est_array) <- list(
  NULL,
  c("StockA", "StockB"),
  c("RAW", "COOK", "ML")
)
# Ensure proportions sum to 1
for(i in 1:nsamps) for(k in 1:nest) est_array[i,,k] <- est_array[i,,k]/sum(est_array[i,,k])

# 2. Generate report
summary_rep <- create_hisea_summary_report(
  all_estimates_array = est_array,
  actual_proportions = c(0.6, 0.4),
  run_type = "SIMULATION"
)
print(summary_rep$mean_estimates)
#>              RAW      COOK        ML
#> StockA 0.5460221 0.4127984 0.5806911
#> StockB 0.4539779 0.5872016 0.4193089
```
