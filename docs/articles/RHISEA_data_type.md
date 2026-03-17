# Two input paths in RHISEA: run_hisea_all with HISEA-style files or Dataframes

## Introduction

This vignette demonstrates two equivalent ways to run
[`run_hisea_all()`](https://sosthenea.github.io/RHISEA/reference/run_hisea_all.md)
from the **RHISEA** package:

1.  **Data frame inputs** (R objects `baseline` and `mixture` already
    loaded), and
2.  **HISEA file inputs** (`hisea.std` and `hisea.mix` files).

We run the same analysis with LDA classifier and compare the results
(means, bootstrap distributions, etc.). The vignette explains the
differences, shows reproducible code, and provides interpretation
guidance.

## Required packages

``` r
library(RHISEA)
library(tidyverse)
library(data.table)
library(mclust) # used by some RHISEA internals or for post-analysis
```

## Load the data

``` r
# Option A: load .rda saved objects (development)
baseline_file <- system.file("extdata", "baseline.rda", package = "RHISEA")
mixture_file <- system.file("extdata", "mixture.rda", package = "RHISEA")

load(baseline_file) # loads `baseline` data.frame
load(mixture_file) # loads `mixture` data.frame

# Quick checks
stopifnot(is.data.frame(baseline) || is.data.table(baseline))
stopifnot(is.data.frame(mixture) || is.data.table(mixture))

str(baseline)
#> Classes 'data.table' and 'data.frame':   300 obs. of  3 variables:
#>  $ d13c      : num  -7.99 -8.23 -7.99 -7.32 -9.01 -8.19 -8.48 -8.42 -8.02 -7.9 ...
#>  $ d18o      : num  -0.46 -0.58 -0.89 -1.06 -0.89 -1.25 -0.27 -0.72 -0.74 -0.51 ...
#>  $ population: Factor w/ 2 levels "East","West": 1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, ".internal.selfref")=<externalptr>
str(mixture)
#> tibble [4,500 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ d13c_ukn: chr [1:4500] "-8.6" "-8.8362287144933909" "-8.48" "-8.6199999999999992" ...
#>  $ d18o_ukn: chr [1:4500] "-1.1599999999999999" "-1.3548853367293201" "-0.93" "-1.91" ...

# Ensure variable names expected by run_hisea_all
# baseline should include: d13c, d18o, population
# mixture should include: d13c_ukn, d18o_ukn
```

## Write HISEA formatted files (optional)

If you want to compare with HISEA text format, write files from the data
frames:

``` r
write_std_from_dataframe(df = baseline, stock_col = "population", var_cols = c("d13c", "d18o"))
write_mix_from_dataframe(df = mixture, var_cols = c("d13c_ukn", "d18o_ukn"))

# files created in working directory: "hisea.std", "hisea.mix"
list.files(pattern = "hisea\\.(std|mix)$")
#> [1] "hisea.mix" "hisea.std"
```

## Analysis parameters

``` r
np <- 2
nv <- 2
Nsamps <- 1000 # number of bootstrap/resampling iterations inside run_hisea_all (depends on function)
Nmix <- 100
baseline_file <- "hisea.std"
mixture_file <- "hisea.mix"
stock_labels <- c("East", "West")
stocks_names <- c("East", "West")
resample_baseline <- TRUE
resampled_baseline_sizes <- c(50, 50)

# set seed for reproducibility
seed_val <- 123456
```

## 1) Run with Data frame inputs

``` r
DataFrame_option <- run_hisea_all(
  type = "ANALYSIS",
  np = np,
  phi_method = "cv", # chosen in your script: cv or standard; we use "cv" here
  nv = nv,
  resample_baseline = resample_baseline,
  resampled_baseline_sizes = resampled_baseline_sizes,
  seed_val = seed_val,
  nsamps = Nsamps,
  Nmix = Nmix,
  baseline_input = baseline,
  mix_input = mixture,
  method_class = "LDA",
  stocks_names = stock_labels,
  stock_col = "population",
  var_cols_std = c("d13c", "d18o"),
  var_cols_mix = c("d13c_ukn", "d18o_ukn")
)
#> Warning in run_hisea_all(type = "ANALYSIS", np = np, phi_method = "cv", : resample_baseline only for
#> SIMULATION/BOOTSTRAP; disabling.

# Print summary statistics
print("Data frame input — mean estimates:")
#> [1] "Data frame input — mean estimates:"
print(round(DataFrame_option$mean, 4))
#>         RAW   COOK  COOKC     EM     ML
#> East 0.3771 0.3402 0.3402 0.3402 0.3217
#> West 0.6229 0.6598 0.6598 0.6598 0.6783

# If the object contains bootstrap results, show a small summary
if (!is.null(DataFrame_option$boot)) {
  cat("\nBootstrap summary (data frame input):\n")
  print(summary(DataFrame_option$boot))
}
```

**What this block does**

- Runs the analysis using the in-memory `baseline` and `mixture`
  objects.
- `DataFrame_option$mean` should contain the estimated mean mixture
  composition (e.g., estimated proportions for East and West) averaged
  across resamples.
- `DataFrame_option$boot` (if present) might contain bootstrap
  distribution of estimates — used to compute CI, sd, etc.

## 2) Run with HISEA-formatted files

``` r
HiseaFormat_option <- run_hisea_all(
  type = "ANALYSIS",
  np = np,
  phi_method = "cv",
  nv = nv,
  resample_baseline = resample_baseline,
  resampled_baseline_sizes = resampled_baseline_sizes,
  seed_val = seed_val,
  nsamps = Nsamps,
  Nmix = Nmix,
  baseline_input = baseline_file,
  mix_input = mixture_file,
  method_class = "LDA",
  stocks_names = stock_labels
)
#> Warning in run_hisea_all(type = "ANALYSIS", np = np, phi_method = "cv", : resample_baseline only for
#> SIMULATION/BOOTSTRAP; disabling.

print("HISEA file input — mean estimates:")
#> [1] "HISEA file input — mean estimates:"
print(round(HiseaFormat_option$mean, 4))
#>         RAW   COOK  COOKC     EM     ML
#> East 0.3771 0.3402 0.3402 0.3402 0.3217
#> West 0.6229 0.6598 0.6598 0.6598 0.6783

if (!is.null(HiseaFormat_option$boot)) {
  cat("\nBootstrap summary (HISEA file input):\n")
  print(summary(HiseaFormat_option$boot))
}
```

**What to expect**

- If
  [`write_std_from_dataframe()`](https://sosthenea.github.io/RHISEA/reference/write_std_from_dataframe.md)
  /
  [`write_mix_from_dataframe()`](https://sosthenea.github.io/RHISEA/reference/write_mix_from_dataframe.md)
  produced identical formatted files to the in-memory data, results
  **should be extremely similar**. Small differences can arise from
  formatting, row order, or numeric rounding.
- If results differ substantially, see *Troubleshooting* below.

## Compare results: table + plot

This section produces a comparison table and simple plot of the mean
estimates and their differences.

``` r
# collect means
df_compare <- tibble(
  mean_df = as.numeric(DataFrame_option$mean),
  mean_file = as.numeric(HiseaFormat_option$mean)
) %>%
  mutate(diff = mean_df - mean_file)

kable(df_compare, digits = 4, caption = "Comparison of estimates: Data frame vs HISEA file")
```

| mean_df | mean_file | diff |
|--------:|----------:|-----:|
|  0.3771 |    0.3771 |    0 |
|  0.6229 |    0.6229 |    0 |
|  0.3402 |    0.3402 |    0 |
|  0.6598 |    0.6598 |    0 |
|  0.3402 |    0.3402 |    0 |
|  0.6598 |    0.6598 |    0 |
|  0.3402 |    0.3402 |    0 |
|  0.6598 |    0.6598 |    0 |
|  0.3217 |    0.3217 |    0 |
|  0.6783 |    0.6783 |    0 |

Comparison of estimates: Data frame vs HISEA file

``` r

# Plot side-by-side bars
df_plot <- df_compare %>% pivot_longer(cols = c(mean_df, mean_file), names_to = "input_type", values_to = "estimate")
```

## Visualize bootstrap distributions (if available)

If the objects returned contain bootstrap distributions (common when
`nsamps` used), plot histograms/density for each stock and input type:

``` r
DataFrame_option_boot <- run_hisea_all(
  type = "BOOTSTRAP",
  np = np,
  phi_method = "cv", # chosen in your script: cv or standard; we use "cv" here
  nv = nv,
  resample_baseline = resample_baseline,
  resampled_baseline_sizes = resampled_baseline_sizes,
  seed_val = seed_val,
  nsamps = Nsamps,
  Nmix = Nmix,
  baseline_input = baseline,
  mix_input = mixture,
  method_class = "LDA",
  stocks_names = stock_labels,
  stock_col = "population",
  var_cols_std = c("d13c", "d18o"),
  var_cols_mix = c("d13c_ukn", "d18o_ukn")
)

HiseaFormat_option_boot <- run_hisea_all(
  type = "BOOTSTRAP",
  np = np,
  phi_method = "cv",
  nv = nv,
  resample_baseline = resample_baseline,
  resampled_baseline_sizes = resampled_baseline_sizes,
  seed_val = seed_val,
  nsamps = Nsamps,
  Nmix = Nmix,
  baseline_input = baseline_file,
  mix_input = mixture_file,
  method_class = "LDA",
  stocks_names = stock_labels
)

make_boot_df <- function(boot_obj, label) {
  if (is.null(boot_obj)) {
    return(NULL)
  }
  # adapt depending on structure
  boot_mat <- as.matrix(boot_obj) # try to coerce
  df <- as.data.frame(boot_mat)
  names(df) <- stock_labels[1:ncol(df)]
  df_long <- pivot_longer(df, cols = everything(), names_to = "stock", values_to = "estimate")
  df_long$input <- label
  df_long
}

boot_df1 <- make_boot_df(DataFrame_option_boot$boot, "data_frame")
boot_df2 <- make_boot_df(HiseaFormat_option_boot$boot, "hisea_file")
boot_all <- bind_rows(boot_df1, boot_df2)

if (!is.null(boot_all) && nrow(boot_all) > 0) {
  ggplot(boot_all, aes(x = estimate)) +
    geom_density() +
    facet_grid(stock ~ input, scales = "free") +
    labs(title = "Bootstrap distributions by stock and input type")
}
```

## Interpretation & Reasoning (how to read results)

1.  **Mean estimates (`$mean`)**: these are the estimated proportions of
    each stock in the mixture averaged across resamples. Example:
    `mean = c(0.87, 0.13)` would indicate 87% East, 13% West.

2.  **Bootstrap distributions**: provide variability — use to compute
    95% CI or sd. If bootstrap distributions between `DataFrame_option`
    and `HiseaFormat_option` overlap strongly and mean differences are
    tiny, the two input modes are effectively equivalent.
