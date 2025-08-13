## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)
library(knitr)

## ----libs, message=TRUE-------------------------------------------------------
library(Rhisea)
library(tidyverse)
library(data.table)
library(mclust)     # used by some Rhisea internals or for post-analysis

## ----load-data----------------------------------------------------------------
# Option A: load .rda saved objects (development)
baseline_file <- system.file("extdata", "baseline.rda", package = "Rhisea")
mixture_file  <- system.file("extdata", "mixture.rda",  package = "Rhisea")

load(baseline_file)  # loads `baseline` data.frame
load(mixture_file)   # loads `mixture` data.frame

# Quick checks
stopifnot(is.data.frame(baseline) || is.data.table(baseline))
stopifnot(is.data.frame(mixture) || is.data.table(mixture))

str(baseline)
str(mixture)

# Ensure variable names expected by run_hisea_all
# baseline should include: d13c, d18o, population
# mixture should include: d13c_ukn, d18o_ukn

## ----write-hisea-files--------------------------------------------------------
write_std_from_dataframe(df = baseline, stock_col = "population", var_cols = c("d13c", "d18o"))
write_mix_from_dataframe(df = mixture, var_cols = c("d13c_ukn", "d18o_ukn"))

# files created in working directory: "hisea.std", "hisea.mix"
list.files(pattern = "hisea\\.(std|mix)$")

## ----params-------------------------------------------------------------------
np <- 2
nv <- 2
Nsamps <- 1000     # number of bootstrap/resampling iterations inside run_hisea_all (depends on function)
Nmix <- 100
baseline_file <- "hisea.std"
mixture_file <- "hisea.mix"
stock_labels <- c("East", "West")
stocks_names <- c("East", "West")
resample_baseline <- TRUE
resampled_baseline_sizes <- c(50, 50)

# set seed for reproducibility
seed_val <- 123456

## ----run-dataframe, message=TRUE, warning=TRUE--------------------------------
DataFrame_option <- run_hisea_all(
  type = "ANALYSIS",
  np = np,
  phi_method = "cv",         # chosen in your script: cv or standard; we use "cv" here
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

# Print summary statistics
print("Data frame input — mean estimates:")
print(round(DataFrame_option$mean, 4))

# If the object contains bootstrap results, show a small summary
if (!is.null(DataFrame_option$boot) ) {
  cat("\nBootstrap summary (data frame input):\n")
  print(summary(DataFrame_option$boot))
}

## ----run-file-format, message=TRUE, warning=TRUE------------------------------
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
  mix_input  = mixture_file,
  method_class = "LDA",
  stocks_names = stock_labels
)

print("HISEA file input — mean estimates:")
print(round(HiseaFormat_option$mean, 4))

if (!is.null(HiseaFormat_option$boot) ) {
  cat("\nBootstrap summary (HISEA file input):\n")
  print(summary(HiseaFormat_option$boot))
}

## ----compare-results, warning=FALSE-------------------------------------------
# collect means
df_compare <- tibble(
  mean_df = as.numeric(DataFrame_option$mean),
  mean_file = as.numeric(HiseaFormat_option$mean)
) %>%
  mutate(diff = mean_df - mean_file)

kable(df_compare, digits = 4, caption = "Comparison of estimates: Data frame vs HISEA file")

# Plot side-by-side bars
df_plot <- df_compare %>% pivot_longer(cols = c(mean_df, mean_file), names_to = "input_type", values_to = "estimate")


## ----plot-boot, warning=FALSE, message=FALSE----------------------------------

DataFrame_option_boot <- run_hisea_all(
  type = "BOOTSTRAP",
  np = np,
  phi_method = "cv",         # chosen in your script: cv or standard; we use "cv" here
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
  mix_input  = mixture_file,
  method_class = "LDA",
  stocks_names = stock_labels
)

make_boot_df <- function(boot_obj, label){
  if (is.null(boot_obj)) return(NULL)
  # adapt depending on structure
  boot_mat <- as.matrix(boot_obj)  # try to coerce
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

