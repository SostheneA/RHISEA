library(testthat)
library(RHISEA)
library(randomForest)
library(caret)
library(dplyr)

test_that("RHISEA handles the complete workflow: LDA, RF and External Tuning", {

  # --- 1. DATA SIMULATION (2 STOCKS) ---
  set.seed(123)
  np <- 2
  nv <- 2
  Nsamps <- 10  # Reduced for fast unit testing
  Nmix <- 50
  stock_labels <- c("East", "West")

  # Simulate Baseline data (e.g., Stable Isotopes)
  baseline_df <- data.frame(
    population = factor(rep(stock_labels, each = Nsamps)),
    d13c = c(rnorm(Nsamps, -20, 1), rnorm(Nsamps, -15, 1)),
    d18o = c(rnorm(Nsamps, 10, 0.5), rnorm(Nsamps, 12, 0.5))
  )

  # Simulate Mixture data
  mixture_df <- data.frame(
    d13c_ukn = rnorm(Nmix, -17.5, 2),
    d18o_ukn = rnorm(Nmix, 11, 1)
  )

  # --- 2. TEST: LDA WITH STANDARD PHI ---
  # Ensuring run_hisea_all executes without error
  expect_error(
    {
      LDA_results <- run_hisea_all(
        type = "ANALYSIS",
        np = np,
        phi_method = "standard",
        nv = nv,
        resample_baseline = FALSE,
        seed_val = 123,
        nsamps = 1, # Minimal replicates for testing
        Nmix = Nmix,
        baseline_input = baseline_df,
        mix_input = mixture_df,
        method_class = "LDA",
        stocks_names = stock_labels,
        stock_col = "population",
        var_cols_std = c("d13c", "d18o"),
        var_cols_mix = c("d13c_ukn", "d18o_ukn"),
        verbose = FALSE
      )
    },
    NA
  )

  # Fix: Accessing matrix columns using [ , "name"] instead of $
  # to prevent "atomic vector" errors.
  raw_estimates <- LDA_results$mean_estimates[, "RAW"]

  expect_equal(length(raw_estimates), np)
  expect_equal(sum(raw_estimates), 1, tolerance = 1e-6)

  # --- 3. TEST: RANDOM FOREST WITH CV PHI ---
  expect_error(
    {
      RF_results <- run_hisea_all(
        type = "ANALYSIS",
        np = np,
        phi_method = "cv",
        nv = nv,
        resample_baseline = FALSE,
        seed_val = 123,
        nsamps = 1,
        Nmix = Nmix,
        baseline_input = baseline_df,
        mix_input = mixture_df,
        method_class = "RF",
        stocks_names = stock_labels,
        stock_col = "population",
        var_cols_std = c("d13c", "d18o"),
        var_cols_mix = c("d13c_ukn", "d18o_ukn"),
        ntree = 50, # Reduced for speed
        verbose = FALSE
      )
    },
    NA
  )

  expect_true(!is.null(RF_results$mean_estimates))

  # --- 4. TEST: EXTERNAL ESTIMATION (RF TUNING) ---
  # Simulating an external model training
  ext_rf_model <- randomForest::randomForest(population ~ d13c + d18o, data = baseline_df, ntree = 20)

  # Preparing mixture data for prediction (matching feature names)
  mix_prepared <- mixture_df %>%
    rename(d13c = d13c_ukn, d18o = d18o_ukn)

  pred_probs <- predict(ext_rf_model, newdata = mix_prepared, type = "prob")
  pred_classes <- as.integer(predict(ext_rf_model, newdata = mix_prepared))

  # Validation of prediction outputs
  expect_true(is.matrix(pred_probs))
  expect_equal(ncol(pred_probs), np)
  expect_equal(length(pred_classes), Nmix)
})
