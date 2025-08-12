# ldf.R — Functions related to Linear Discriminant Function (LDF) analysis:
#           - compute_ldf_coefficients: Calculates LDF coefficients.
#           - classify_samples: Classifies samples using LDF coefficients.
#           - get_phi: Computes the misclassification matrix (Phi).

#' Compute Linear Discriminant Function (LDF) Coefficients
#'
#' Computes the coefficients for Fisher's Linear Discriminant Function for each
#' group (population/stock). This implementation assumes a common (pooled)
#' covariance matrix across all groups, a standard assumption for LDA.
#' The LDF is of the form: LDF_j(x) = C_j0 + C_j1*x1 + ... + C_jp*xp.
#'
#' @param baseline A list of numeric matrices, where each matrix represents the
#'                 baseline (training) data for one population/stock.
#'                 Each matrix should have observations as rows and variables as columns.
#'
#' @return A numeric matrix of LDF coefficients. Rows correspond to populations,
#'         and columns correspond to variables followed by the constant term.
#'         The dimensions are (np x (nv + 1)).
#' @export
#' @examples
#' # Create dummy baseline data for 2 stocks with 2 variables
#' stock1_data <- matrix(rnorm(20, mean = 0), ncol = 2)
#' stock2_data <- matrix(rnorm(20, mean = 1), ncol = 2)
#' baseline_list <- list(StockA = stock1_data, StockB = stock2_data) # Named list
#' ldf_coeffs <- compute_ldf_coefficients(baseline_list)
#' print(ldf_coeffs)
compute_ldf_coefficients <- function(baseline) {
  # --- Input Validation and Basic Setup ---
  if (!is.list(baseline) || length(baseline) < 2) {
    stop("Argument 'baseline' must be a list of at least two matrices (one for each population).")
  }
  if (!all(sapply(baseline, is.matrix)) || !all(sapply(baseline, is.numeric))) {
    stop("Each element in 'baseline' must be a numeric matrix.")
  }

  num_vars_per_stock <- sapply(baseline, ncol)
  if (length(unique(num_vars_per_stock)) != 1) {
    stop("All baseline matrices must have the same number of columns (variables).")
  }
  NV <- unique(num_vars_per_stock) # Number of variables
  if (NV == 0 && length(baseline[[1]]) > 0) { # Handles case where ncol might return 0 for single column matrix if not careful
    if (is.matrix(baseline[[1]]) && NROW(baseline[[1]]) > 0) NV <- 1
  }
  if (NV == 0) stop("Number of variables (NV) is zero. Check baseline data structure.")

  NP <- length(baseline)   # Number of populations (stocks)

  # Combine all baseline data into a single matrix
  VARBLE <- do.call(rbind, baseline)
  # Get the size (number of samples) for each population
  gsize <- sapply(baseline, nrow)

  if (any(gsize <= NV)) {
    warning("One or more groups have a sample size less than or equal to the number of variables. Pooled covariance matrix might be singular or unstable.", call. = FALSE)
  }
  if (sum(gsize) <= NP) {
    stop("Total number of samples across all groups must be greater than the number of groups for pooled covariance calculation.")
  }
  if (sum(gsize) <= NV && NP > 1) { # Denominator N-NP could be <=0 or too small if N also small
    warning("Total number of samples (N) is less than or equal to the number of variables (NV), and N-NP for covariance denominator might be too small or non-positive if N also small.", call. = FALSE)
  }


  # FREQ is assumed to be 1 for each observation in this context for standard LDA.
  FREQ <- rep(1, nrow(VARBLE)) # Frequency/weight of each observation

  N <- sum(gsize) # Total number of observations across all populations

  # Calculate split points to easily access data for each population from VARBLE
  SPLIT <- cumsum(c(0, gsize)) # Cumulative sums of group sizes for indexing

  # --- Calculate Group Means ---
  MEANS <- matrix(0, nrow = NP, ncol = NV) # Matrix to store means for each group

  # Assign rownames to MEANS using base R logic
  r_names_baseline <- names(baseline)
  if (is.null(r_names_baseline) || !all(nzchar(r_names_baseline)) || length(r_names_baseline) != NP) {
    rownames(MEANS) <- paste0("Stock", 1:NP)
  } else {
    rownames(MEANS) <- r_names_baseline
  }

  # Assign colnames to MEANS using base R logic, from the first baseline matrix
  if (!is.null(baseline[[1]]) && !is.null(colnames(baseline[[1]]))) {
    c_names_baseline1 <- colnames(baseline[[1]])
    if (length(c_names_baseline1) == NV && all(nzchar(c_names_baseline1))) {
      colnames(MEANS) <- c_names_baseline1
    } else {
      colnames(MEANS) <- paste0("Var", 1:NV)
    }
  } else {
    colnames(MEANS) <- paste0("Var", 1:NV)
  }

  # Calculate means for each group
  for (j in 1:NP) {
    group_indices <- (SPLIT[j] + 1):SPLIT[j + 1]
    # Using colMeans for matrices is direct if FREQ is always 1 (as assumed)
    MEANS[j, ] <- colMeans(VARBLE[group_indices, , drop = FALSE])
    # The original HISEA Fortran code included FREQ in mean calculation,
    # but if FREQ is always 1, colMeans is equivalent and simpler.
    # If FREQ could vary and represent weights:
    # for (l in 1:NV) {
    #   MEANS[j, l] <- sum(VARBLE[group_indices, l] * FREQ[group_indices]) / sum(FREQ[group_indices])
    # }
  }

  # --- Calculate Pooled Covariance Matrix (SIGMA) ---
  # SIGMA = sum_over_groups [ sum_over_samples_in_group (x_ij - mean_j)(x_ij - mean_j)' ] / (N - NP)
  SIGMA <- matrix(0, nrow = NV, ncol = NV) # Initialize pooled covariance matrix
  colnames(SIGMA) <- rownames(SIGMA) <- colnames(MEANS) # Assign names

  # HISEA Fortran calculation method for SIGMA:
  # SIGMA(L,LL) = (SUM_G(VARBLE(G,L)*VARBLE(G,LL)*FREQ(G)) - SUM_J(GSIZE(J)*MEANS(J,L)*MEANS(J,LL))) / (N-NP)
  # This is sum(x_il * x_ill) - sum(n_j * mean_jl * mean_jll), scaled by (N-NP)
  if ((N - NP) <= 0) {
    stop(paste0("Denominator for pooled covariance matrix (N-NP = ", N, "-", NP, ") is not positive. Cannot compute SIGMA."))
  }

  for (l in 1:NV) {
    for (ll in 1:l) { # Iterate only for the lower triangle and diagonal
      term1_sum_cross_products_raw <- sum(VARBLE[, l] * VARBLE[, ll] * FREQ)
      term2_sum_cross_products_means <- sum(gsize * MEANS[, l] * MEANS[, ll])

      SIGMA[l, ll] <- (term1_sum_cross_products_raw - term2_sum_cross_products_means) / (N - NP)
      if (l != ll) {
        SIGMA[ll, l] <- SIGMA[l, ll] # Ensure matrix is symmetric
      }
    }
  }

  # --- Calculate LDF Coefficients ---
  # Check for singularity of the pooled covariance matrix before inversion
  eigen_values_sigma <- eigen(SIGMA, symmetric = TRUE, only.values = TRUE)$values
  if (any(eigen_values_sigma < .Machine$double.eps * 100 * max(eigen_values_sigma))) {
    # Using condition number or relative eigenvalue check is more robust than just det()
    warning("Pooled covariance matrix (SIGMA) is singular or near-singular. LDF coefficients may be unstable or invalid.", call. = FALSE)
    # Attempting generalized inverse as a fallback, though HISEA Fortran would likely stop.
    SIGMA_INV <- tryCatch({
      MASS::ginv(SIGMA) # Use generalized inverse from MASS package
    }, error = function(e_ginv) {
      stop(paste("Error inverting SIGMA with solve() and ginv():", conditionMessage(e_ginv),
                 "Consider checking for collinear variables or small group sizes relative to Nvar."))
    })
    warning("Used generalized inverse for SIGMA due to singularity.", call. = FALSE)
  } else {
    SIGMA_INV <- tryCatch({
      solve(SIGMA)
    }, error = function(e_solve) {
      stop(paste("Error inverting pooled covariance matrix SIGMA with solve():", conditionMessage(e_solve)))
    })
  }

  # Intermediate calculation: MEANS %*% SIGMA_INV
  WORK <- MEANS %*% SIGMA_INV # Dimensions: NP x NV

  # LDF coefficients matrix: NP rows, (NV + 1) columns (NV variables + 1 constant term)
  coefs <- matrix(0, nrow = NP, ncol = NV + 1)
  rownames(coefs) <- rownames(MEANS) # Assign stock names
  colnames(coefs) <- c(colnames(MEANS), "Constant") # Assign variable names + "Constant"

  # Calculate coefficients for each population's discriminant function
  # LDF_j(x) = (mean_j^T * SIGMA_INV * x) - 0.5 * (mean_j^T * SIGMA_INV * mean_j)
  # (Assuming equal priors, log(prior_j) term is omitted or absorbed into constant later if needed)
  for (j in 1:NP) {
    coefs[j, 1:NV] <- WORK[j, ]                     # Coefficients for variables x1, ..., xNV
    coefs[j, NV + 1] <- -0.5 * sum(WORK[j, ] * MEANS[j, ]) # Constant term C_j0
  }

  return(coefs)
}


#' Classify Samples Using LDF Coefficients
#'
#' Assigns each sample (observation) to a population/stock based on the highest
#' Linear Discriminant Function (LDF) score. It also computes posterior
#' probabilities (or values proportional to them if priors are not explicitly used).
#'
#' @param samples A numeric matrix of samples to be classified.
#'                Rows are observations, columns are variables.
#' @param coefs A numeric matrix of LDF coefficients, as returned by
#'              `compute_ldf_coefficients`. Rows are populations, columns are
#'              coefficients for variables followed by the constant term.
#' @param freq Optional numeric vector of frequency weights for the samples.
#'             Defaults to 1 for each sample if NULL. (Used mainly for HISEA compatibility).
#' @param type Character, indicates the type of run (e.g., "S" for simulation).
#'             This parameter is from HISEA Fortran and affects a condition for processing,
#'             though its impact here is minor if `freq` are positive. Default "S".
#'
#' @return A list containing:
#'         \item{class}{An integer vector of predicted class labels (1 to NP) for each sample.}
#'         \item{likelihood}{A numeric matrix where rows are samples and columns are
#'                           populations. Contains posterior probabilities of class membership
#'                           (assuming equal priors, rows sum to 1).}
#' @export
classify_samples <- function(samples, coefs, freq = NULL, type = "S") {
  # --- Input Validation and Basic Setup ---
  if (!is.matrix(samples)) {
    if (is.numeric(samples) && is.vector(samples)) {
      # Attempt to convert vector to a single-row or single-column matrix
      # This depends on whether it's a single sample with multiple vars, or multiple samples with one var.
      num_vars_in_coefs <- ncol(coefs) - 1
      if (length(samples) == num_vars_in_coefs) { # Assume single sample
        samples <- matrix(samples, nrow = 1)
      } else if (num_vars_in_coefs == 1) { # Assume multiple samples of a single variable
        samples <- matrix(samples, ncol = 1)
      } else {
        stop("'samples' vector could not be unambiguously converted to a matrix. Provide a matrix.")
      }
    } else {
      stop("'samples' must be a numeric matrix.")
    }
  }
  if (!is.numeric(samples)) stop("'samples' matrix must be numeric.") # After potential conversion

  if (!is.matrix(coefs) || !is.numeric(coefs)) {
    stop("'coefs' must be a numeric matrix of LDF coefficients.")
  }

  NV_samples <- ncol(samples)  # Number of variables in the samples
  NV_coefs <- ncol(coefs) - 1  # Number of variables expected by LDF coefficients

  if (NV_samples != NV_coefs) {
    stop(paste("Number of variables in 'samples' (", NV_samples,
               ") does not match number of variable coefficients in 'coefs' (", NV_coefs, ")."))
  }

  NP <- nrow(coefs)           # Number of populations
  N_samples <- nrow(samples)  # Number of samples to classify

  if (N_samples == 0) { # Handle empty samples input
    return(list(
      class = integer(0),
      likelihood = matrix(numeric(0), ncol = NP, nrow = 0)
    ))
  }

  # Initialize frequency weights if not provided (default to 1 for each sample)
  if (is.null(freq)) {
    freq <- rep(1, N_samples)
  } else if (!is.numeric(freq) || length(freq) != N_samples) {
    stop("Length of 'freq' vector must match the number of rows in 'samples', and it must be numeric.")
  }

  # --- LDF Score Calculation & Classification ---
  # Augment samples matrix with a column of 1s for the constant term in LDF
  samples_augmented <- cbind(samples, 1)

  # Calculate LDF scores: samples_augmented %*% t(coefs)
  # Result is an N_samples x NP matrix, where each element [i, j] is the LDF score
  # of sample i for population j.
  raw_ldf_scores <- tcrossprod(samples_augmented, coefs) # More direct: samples_augmented %*% t(coefs)

  # Classification: assign each sample to the class with the highest LDF score
  classifications <- apply(raw_ldf_scores, 1, which.max) # Returns index of max (1 to NP)

  # --- Posterior Probability Calculation (Likelihoods) ---
  # Convert LDF scores to posterior probabilities P(Stock_j | Data_i) assuming equal priors.
  # Uses softmax for numerical stability: P_ij = exp(score_ij) / sum_k(exp(score_ik))
  # Subtracting the max score per row before exponentiating prevents overflow/underflow.
  max_scores_per_row <- apply(raw_ldf_scores, 1, max, na.rm = TRUE) # Max score for each sample
  # Sweep out the max score from each row for stability
  stable_scores <- sweep(raw_ldf_scores, 1, max_scores_per_row, FUN = "-")
  exp_stable_scores <- exp(stable_scores)
  sum_exp_scores_per_row <- rowSums(exp_stable_scores, na.rm = TRUE)

  # Avoid division by zero if all exp_stable_scores for a row are 0 (after subtracting max, some could be very negative)
  sum_exp_scores_per_row[sum_exp_scores_per_row < .Machine$double.eps] <- 1 # Avoid 0/0 => NaN; effectively assigns equal small prob if all exp are ~0

  likelihoods <- exp_stable_scores / sum_exp_scores_per_row # Normalize to get probabilities

  # Handle HISEA Fortran logic for specific cases (freq <= 0 and type != "S")
  # This implies such samples are effectively ignored or their results zeroed out.
  if (any(freq <= 0) && toupper(substr(type, 1, 1)) != "S") {
    ignored_indices <- which(freq <= 0)
    classifications[ignored_indices] <- 0 # HISEA Fortran convention for ignored/unclassified
    likelihoods[ignored_indices, ] <- 0   # Zero out likelihoods for these samples
    warning(paste(length(ignored_indices),
                  "samples had freq <= 0 and type was not 'S'; their classifications set to 0 and likelihoods to zero."),
            call. = FALSE)
  }

  return(list(
    class = classifications,
    likelihood = likelihoods
  ))
}


#' Get Phi Matrix
#'
#' Calculate the phi matrix for stock composition analysis.
#'
#' @param predicted_class Integer vector of predictions
#' @param true_labels Factor of true labels
#' @param np Number of populations (levels)
#' @return Matrix of phi values
#' @export
#' @examples
#' \dontrun{
#' pred <- c(1, 2, 1, 2)
#' true <- c(1, 2, 1, 1)
#' phi_matrix <- get_phi(pred, true, np = 2)
#' }
get_phi <- function(predicted_class, true_labels, np) {
  # --- Input Validation ---
  if (length(predicted_class) != length(true_labels)) {
    stop("Arguments 'predicted_class' and 'true_labels' must have the same length.")
  }
  if (!is.numeric(np) || length(np) != 1 || np < 1 || floor(np) != np) { # np can be 1 if only one stock
    stop("Argument 'np' must be a single positive integer (number of populations).")
  }
  if (length(predicted_class) == 0) { # Handle empty input
    warning("'predicted_class' and 'true_labels' are empty. Returning an empty Phi matrix if np > 0, or error if np=0.", call. = FALSE)
    if (np > 0) return(matrix(0, nrow = np, ncol = np)) else return(matrix(numeric(0),0,0))
  }
  if (any(!is.finite(predicted_class)) || any(!is.finite(true_labels))) {
    stop("Predicted classes and true labels must contain only finite numeric values.")
  }

  # Check if class labels are within the expected range [1, np]
  # Allow for 0 if it's from the classify_samples ignored_indices logic
  valid_preds <- predicted_class[predicted_class != 0]
  valid_trues <- true_labels # true_labels should not contain 0 unless it's an error.

  if (length(valid_preds) > 0 && (max(valid_preds, na.rm = TRUE) > np || min(valid_preds, na.rm = TRUE) < 1)) {
    stop(paste0("Some 'predicted_class' labels are outside the expected range [1, np] or are 0 incorrectly. Max: ",
                max(predicted_class, na.rm=T), ", Min: ", min(predicted_class, na.rm=T)))
  }
  if (length(valid_trues) > 0 && (max(valid_trues, na.rm = TRUE) > np || min(valid_trues, na.rm = TRUE) < 1)) {
    stop(paste0("Some 'true_labels' are outside the expected range [1, np]. Max: ",
                max(true_labels, na.rm=T), ", Min: ", min(true_labels, na.rm=T)))
  }

  # Use table for efficient cross-tabulation, ensuring all levels 1:np are present
  # This directly gives counts of (predicted_class, true_labels)
  # Rows of contingency_table are predicted_class, columns are true_labels
  # This matches HISEA's Phi definition where PHI(Assigned, True)
  contingency_table <- table(factor(predicted_class, levels = 1:np),
                             factor(true_labels, levels = 1:np))

  # Convert counts to proportions by dividing each column by its sum (total in that true class)
  phi <- sweep(contingency_table, MARGIN = 2, STATS = colSums(contingency_table), FUN = "/")

  # Handle cases where a true class might have had zero samples (colSums would be 0, leading to NaN)
  # In such cases, the column proportions are undefined. HISEA might error or set to 0.
  # Setting to 0 if colSum was 0.
  phi[is.nan(phi)] <- 0

  # Verification: each column should sum to 1 (if colSums(contingency_table) > 0 for that column)
  # This is inherently handled by the sweep if colSums > 0.
  # If a colSum was 0, that column in phi will be all 0s.

  # Original HISEA loop-based construction (kept for reference, table() is more idiomatic R):
  # phi_loop <- matrix(0, nrow = np, ncol = np)
  # for (j in 1:np) { # Iterate over true classes (columns of Phi)
  #   indices_true_j <- which(true_labels == j)
  #   num_samples_true_j <- length(indices_true_j)
  #
  #   if (num_samples_true_j == 0) {
  #     warning(paste("No individuals found for true population", j, "in 'true_labels'. Column", j, "of Phi will be zeros."), call. = FALSE)
  #     phi_loop[, j] <- 0 # Or NA
  #     next
  #   }
  #
  #   for (i in 1:np) { # Iterate over predicted classes (rows of Phi)
  #     phi_loop[i, j] <- sum(predicted_class[indices_true_j] == i)
  #   }
  #   phi_loop[, j] <- phi_loop[, j] / num_samples_true_j
  # }

  # Assign row and column names for clarity if not already set by table() with factor levels
  # The table() function with factors as input should already produce named dimensions if factors have labels.
  # If true_labels and predicted_class were just numbers, dimnames would be those numbers.
  dim_names_default <- paste0("Stock", 1:np)
  stock_names_for_phi <- tryCatch(levels(factor(true_labels, levels = 1:np, labels=dim_names_default)), error=function(e) dim_names_default)

  rownames(phi) <- paste0("Assigned_", stock_names_for_phi)
  colnames(phi) <- paste0("True_", stock_names_for_phi)

  return(as.matrix(phi)) # Ensure it's a matrix, not a table object
}
