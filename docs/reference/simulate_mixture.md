# Simulate a Mixed Fishery Sample from Known Stock Proportions

Creates a synthetic fishery mixture by sampling individuals from
different baseline stocks according to specified true proportions.
Sampling from each stock is done with replacement.

## Usage

``` r
simulate_mixture(baseline_data_list, actual_proportions, N_mixture_size)
```

## Arguments

- baseline_data_list:

  A list of numeric matrices. Each matrix represents the baseline
  (standard) data for one stock/population, with observations as rows
  and variables as columns.

- actual_proportions:

  A numeric vector of true proportions for each stock in the mixture.
  Must sum to 1 and have the same length as \`baseline_data_list\`.

- N_mixture_size:

  Integer, the total number of individuals to draw for the synthetic
  mixture.

## Value

A numeric matrix representing the simulated mixture sample. Rows are
individuals, and columns are variables. Returns an error if the mixture
cannot be formed (e.g., due to empty baseline stocks needed for
sampling).

## Examples

``` r
# 1. Prepare dummy baseline data for 2 stocks
stock1 <- matrix(rnorm(40, mean = 0, sd = 1), ncol = 2)
stock2 <- matrix(rnorm(60, mean = 2, sd = 1), ncol = 2)
baseline_list <- list(StockA = stock1, StockB = stock2)

# 2. Set true proportions and mixture size
true_props <- c(0.6, 0.4)
n_mix <- 100

# 3. Generate the synthetic mixture
simulated_sample <- simulate_mixture(baseline_list, true_props, n_mix)

# 4. Verify output
head(simulated_sample)
#>            [,1]         [,2]
#> [1,]  1.6007598  1.201167182
#> [2,]  0.4326778 -0.001062756
#> [3,] -0.4968519 -0.866606890
#> [4,] -0.2724399  1.508721847
#> [5,] -1.5738221 -1.206355025
#> [6,]  3.1837559  2.258213780
nrow(simulated_sample) # Should be 100
#> [1] 100
```
