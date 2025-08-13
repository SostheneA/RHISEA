## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ----lda----------------------------------------------------------------------
# Load required packages
library(MASS)
library(caret)
library(reshape2)
library(ggplot2)
library(Rhisea)

# Load baseline and mixture data
baseline_file <- system.file("extdata", "baseline.rda", package = "Rhisea")
mixture_file  <- system.file("extdata", "mixture.rda",  package = "Rhisea")

load(baseline_file)  # loads `baseline` data.frame
load(mixture_file)   # loads `mixture` data.frame

# Prepare baseline data
baseline$population <- as.factor(baseline$population)
stocks_names <- levels(baseline$population)
np <- length(stocks_names)

# Define formula for classification
formula <- population ~ d13c + d18o

# Function to perform stratified k-fold CV and compute phi matrix for LDA
get_cv_results_lda <- function(data, formula, k = 10) {
  set.seed(123)
  folds <- createFolds(data$population, k = k, list = TRUE)
  
  all_predictions <- factor(rep(NA, nrow(data)), levels = levels(data$population))
  all_probabilities <- matrix(NA, nrow = nrow(data), ncol = length(levels(data$population)),
                              dimnames = list(NULL, levels(data$population)))
  
  for(i in seq_along(folds)) {
    test_idx <- folds[[i]]
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]
    
    model <- lda(formula, data = train_data)
    pred <- predict(model, test_data)
    all_predictions[test_idx] <- pred$class
    all_probabilities[test_idx, ] <- pred$posterior
  }
  
  conf_matrix <- table(Predicted = all_predictions, Actual = data$population)
  phi_matrix <- prop.table(conf_matrix, margin = 2)
  
  list(confusion_matrix = conf_matrix,
       phi_matrix = phi_matrix,
       predictions = all_predictions,
       probabilities = all_probabilities)
}

# Run CV and get phi matrix
lda_cv <- get_cv_results_lda(baseline, formula)

# Train full LDA model on baseline
lda_model <- lda(formula, data = baseline)

# Prepare mixture data for prediction
mix_data_prepared <- data.frame(
  d13c = as.numeric(as.character(mixture$d13c_ukn)),
  d18o = as.numeric(as.character(mixture$d18o_ukn))
)

# Predict classes and posterior probabilities for mixture
lda_pred <- predict(lda_model, mix_data_prepared)
lda_classes <- as.integer(lda_pred$class)
lda_probs <- lda_pred$posterior

# Convert phi matrix to numeric matrix if needed
phi_matrix_numeric <- as.matrix(lda_cv$phi_matrix)
phi_matrix_numeric <- matrix(as.numeric(phi_matrix_numeric), nrow = nrow(phi_matrix_numeric), ncol = ncol(phi_matrix_numeric))

# Run HISEA estimates with LDA results
lda_results <- run_hisea_estimates(
  pseudo_classes = lda_classes,
  likelihoods = lda_probs,
  phi_matrix = phi_matrix_numeric,
  np = np,
  type = "BOOTSTRAP",
  stocks_names = stocks_names,
  export_csv = TRUE,
  output_dir = "results_lda",
  verbose = FALSE
)

# Display results
cat("\nLDA Results - Mean Estimates:\n")
print(lda_results$mean_estimates)

cat("\nLDA Results - Standard Deviations:\n")
print(lda_results$sd_estimates)

# Visualization of results
results_long <- melt(lda_results$mean_estimates)
colnames(results_long) <- c("Stock", "Method", "Proportion")

ggplot(results_long, aes(x = Method, y = Proportion, fill = Stock)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "LDA Stock Proportion Estimates",
       y = "Estimated Proportion",
       x = "Estimation Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ----rf-----------------------------------------------------------------------
library(randomForest)

# Function to perform stratified k-fold CV and compute phi matrix for RF
get_cv_results_rf <- function(data, formula, k = 10, ntree = 500) {
  set.seed(123)
  folds <- createFolds(data$population, k = k, list = TRUE)
  
  all_predictions <- factor(rep(NA, nrow(data)), levels = levels(data$population))
  all_probabilities <- matrix(NA, nrow = nrow(data), ncol = length(levels(data$population)),
                              dimnames = list(NULL, levels(data$population)))
  
  for(i in seq_along(folds)) {
    test_idx <- folds[[i]]
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]
    
    model <- randomForest(formula, data = train_data, ntree = ntree)
    all_predictions[test_idx] <- predict(model, test_data)
    all_probabilities[test_idx, ] <- predict(model, test_data, type = "prob")
  }
  
  conf_matrix <- table(Predicted = all_predictions, Actual = data$population)
  phi_matrix <- prop.table(conf_matrix, margin = 2)
  
  list(confusion_matrix = conf_matrix,
       phi_matrix = phi_matrix,
       predictions = all_predictions,
       probabilities = all_probabilities)
}

# Run CV and get phi matrix
rf_cv <- get_cv_results_rf(baseline, formula, ntree = 500)

# Train full RF model on baseline
rf_model <- randomForest(formula, data = baseline, ntree = 500)

# Predict classes and posterior probabilities for mixture
rf_probs <- predict(rf_model, mix_data_prepared, type = "prob")
rf_classes <- as.integer(predict(rf_model, mix_data_prepared))

# Run HISEA estimates with RF results
rf_results <- run_hisea_estimates(
  pseudo_classes = rf_classes,
  likelihoods = rf_probs,
  phi_matrix = rf_cv$phi_matrix,
  np = np,
  type = "BOOTSTRAP",
  stocks_names = stocks_names,
  export_csv = TRUE,
  output_dir = "results_rf",
  verbose = FALSE
)

# Display results
cat("\nRandom Forest Results - Mean Estimates:\n")
print(rf_results$mean_estimates)

cat("\nRandom Forest Results - Standard Deviations:\n")
print(rf_results$sd_estimates)

# Visualization of results
results_long <- melt(rf_results$mean_estimates)
colnames(results_long) <- c("Stock", "Method", "Proportion")

ggplot(results_long, aes(x = Method, y = Proportion, fill = Stock)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Random Forest Stock Proportion Estimates",
       y = "Estimated Proportion",
       x = "Estimation Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ----ctree--------------------------------------------------------------------
library(party)

# Function to perform stratified k-fold CV and compute phi matrix for CTREE
get_cv_results_ctree <- function(data, formula, k = 10) {
  set.seed(123)
  folds <- createFolds(data$population, k = k, list = TRUE)
  
  all_predictions <- factor(rep(NA, nrow(data)), levels = levels(data$population))
  all_probabilities <- matrix(NA, nrow = nrow(data), ncol = length(levels(data$population)),
                              dimnames = list(NULL, levels(data$population)))
  
  for(i in seq_along(folds)) {
    test_idx <- folds[[i]]
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]
    
    model <- ctree(formula, data = train_data)
    pred_probs <- predict(model, test_data, type = "prob")
    pred_probs_matrix <- do.call(rbind, pred_probs)
    all_predictions[test_idx] <- predict(model, test_data)
    all_probabilities[test_idx, ] <- pred_probs_matrix
  }
  
  conf_matrix <- table(Predicted = all_predictions, Actual = data$population)
  phi_matrix <- prop.table(conf_matrix, margin = 2)
  
  list(confusion_matrix = conf_matrix,
       phi_matrix = phi_matrix,
       predictions = all_predictions,
       probabilities = all_probabilities)
}

# Run CV and get phi matrix
ctree_cv <- get_cv_results_ctree(baseline, formula)

# Train full CTREE model on baseline
ctree_model <- ctree(formula, data = baseline,
                     controls = ctree_control(mincriterion = 0.95,
                                              minsplit = 20,
                                              minbucket = 7))

# Predict classes and posterior probabilities for mixture
ctree_probs <- predict(ctree_model, mix_data_prepared, type = "prob")
ctree_probs_matrix <- do.call(rbind, ctree_probs)
ctree_classes <- as.integer(predict(ctree_model, mix_data_prepared))

# Run HISEA estimates with CTREE results
ctree_results <- run_hisea_estimates(
  pseudo_classes = ctree_classes,
  likelihoods = ctree_probs_matrix,
  phi_matrix = ctree_cv$phi_matrix,
  np = np,
  type = "BOOTSTRAP",
  stocks_names = stocks_names,
  export_csv = TRUE,
  output_dir = "results_ctree",
  verbose = FALSE
)

# Display results
cat("\nCTREE Results - Mean Estimates:\n")
print(ctree_results$mean_estimates)

cat("\nCTREE Results - Standard Deviations:\n")
print(ctree_results$sd_estimates)

# Visualize tree and results
plot(ctree_model)

results_long <- melt(ctree_results$mean_estimates)
colnames(results_long) <- c("Stock", "Method", "Proportion")

ggplot(results_long, aes(x = Method, y = Proportion, fill = Stock)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "CTREE Stock Proportion Estimates",
       y = "Estimated Proportion",
       x = "Estimation Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

