# estimate.R — Functions for estimating stock proportions:
# - compute_cook_estimators: Calculates Cook's raw, corrected, and constrained estimators.
# - em_algorithm: Standard Expectation-Maximization algorithm for ML estimation.
# - accel_em_algorithm: Accelerated Expectation-Maximization algorithm.
# - estimate_millar (as estimate_millar_theta4): Millar's constrained classification likelihood estimator (Theta4).
# - estimate_ml (as estimate_ml_theta5): Direct Maximum Likelihood estimator (Theta5).

#' Compute Cook Estimators for Stock Proportions
#'
#' Computes three types of estimators based on classification results:
#' 1. Raw proportions: Simple proportion of samples classified to each stock.
#' 2. Cook's corrected estimator: Raw proportions corrected by the inverse of the
#'    misclassification matrix (Phi_inv). This can result in negative estimates.
#' 3. Cook's constrained estimator: An iterative adjustment of Cook's corrected
#'    estimator to ensure proportions are non-negative and sum to 1.
#'
#' @param class_predictions A numeric vector of predicted class (stock) labels for the mixed sample.
#'                 Values should be integers from 1 to `np`.
#' @param PHIinv The inverse of the misclassification matrix (Phi).
#'               Dimensions should be np x np.
#' @param np Integer, the number of populations (stocks).
#'
#' @return A list containing three numeric vectors:
#'         \item{raw}{Raw proportions of classification.}
#'         \item{cook}{Cook's corrected estimates.}
#'         \item{cook_constrained}{Cook's constrained estimates (non-negative, sum to 1).}
#' @export
compute_cook_estimators <- function(class_predictions, PHIinv, np) {
  # --- Input Validation ---
  if (!is.numeric(np) || length(np) != 1 || np < 1 || floor(np) != np) {
    stop("'np' must be a single positive integer.")
  }
  # Allow class_predictions to be empty, handle downstream.
  if (!is.numeric(class_predictions) || !all(floor(class_predictions) == class_predictions)) {
    if (length(class_predictions) > 0) { # Only error if not empty and invalid
      stop("'class_predictions' must be a vector of integers.")
    }
  }
  if (length(class_predictions) > 0 && (any(class_predictions < 1, na.rm=TRUE) || any(class_predictions > np, na.rm=TRUE))) {
    stop(paste0("'class_predictions' contain values outside the range [1, np]. Min: ",
                min(class_predictions, na.rm=T), ", Max: ", max(class_predictions, na.rm=T)))
  }
  if (!is.matrix(PHIinv) || !is.numeric(PHIinv) || nrow(PHIinv) != np || ncol(PHIinv) != np) {
    stop("'PHIinv' must be a numeric matrix of dimensions np x np.")
  }

  num_mixed_samples <- length(class_predictions)

  if (num_mixed_samples == 0) {
    warning("Empty 'class_predictions' (mixed sample) provided. Returning zero or 1/np estimates.", call. = FALSE)
    default_prop <- if (np > 0) rep(1 / np, np) else numeric(0)
    return(list(
      raw = default_prop,
      cook = default_prop,
      cook_constrained = default_prop
    ))
  }

  # Calculate Y: count of samples classified to each stock
  # Using table() with factor ensures all levels 1:np are present, even if count is 0.
  Y_table <- table(factor(class_predictions, levels = 1:np))
  Y <- as.numeric(Y_table) # Converts table to a named numeric vector, then to unnamed

  # 1. Raw proportions of classification
  raw_proportions <- Y / num_mixed_samples

  # 2. Cook's corrected estimator: PHIinv %*% raw_proportions
  cook_corrected_estimates <- as.numeric(PHIinv %*% raw_proportions)

  # 3. Cook's constrained estimator
  #    Iteratively adjust negative estimates and re-normalize.
  #    Based on HISEA Fortran logic (subroutine COOKLRD, part for THETA(*,3)).
  current_constrained_estimates <- cook_corrected_estimates

  # Max NP iterations, as in HISEA Fortran code (DO 10 ITERS = 1, NP in COOKLRD)
  for (iter in 1:np) {
    # Check if all current estimates are non-negative (within a small tolerance)
    if (all(current_constrained_estimates >= -1e-9)) {
      current_constrained_estimates[current_constrained_estimates < 0] <- 0 # Ensure strictly non-negative
      break # Exit loop if all are non-negative
    }

    active_set_A <- numeric(np) # Vector A in HISEA, marks stocks with >0 proportion
    sum_A_indicator <- 0        # ATOT in HISEA: count of stocks in active set
    sum_theta_in_A <- 0         # ATHTOT in HISEA: sum of proportions for stocks in active set (after zeroing negatives)

    for (j in 1:np) {
      if (current_constrained_estimates[j] > 1e-9) { # Consider positive if > small epsilon
        active_set_A[j] <- 1.0
      } else {
        active_set_A[j] <- 0.0
        current_constrained_estimates[j] <- 0.0 # Set negative (or near-zero) estimates to zero
      }
      sum_A_indicator <- sum_A_indicator + active_set_A[j]
      # sum_theta_in_A accumulates estimates that are currently positive or have been set to zero
      sum_theta_in_A <- sum_theta_in_A + current_constrained_estimates[j]
    }

    if (sum_A_indicator > 0) { # If there's at least one stock in the active set
      # Distribute the deficit/surplus (sum_theta_in_A - 1.0) among the active stocks
      correction_per_active_stock <- (sum_theta_in_A - 1.0) / sum_A_indicator
      for (j in 1:np) {
        current_constrained_estimates[j] <- current_constrained_estimates[j] - active_set_A[j] * correction_per_active_stock
      }
      # After correction, some estimates might have become negative again if correction_per_active_stock was large
      current_constrained_estimates[current_constrained_estimates < 0] <- 0
    } else {
      # All estimates became zero (sum_A_indicator is 0).
      # This implies sum_theta_in_A was also 0.
      # If Np > 0, this is problematic. HISEA doesn't explicitly detail this, implies it shouldn't happen or method fails.
      # Fallback to equal proportions.
      warning("Constrained Cook: all estimates zeroed out during an iteration. Results may be unstable. Falling back to 1/np.", call. = FALSE)
      if (np > 0) current_constrained_estimates <- rep(1/np, np) else current_constrained_estimates <- numeric(0)
      break # Exit loop as no further progress can be made
    }
  } # End of iteration loop for constraining

  # Final normalization to ensure sum to 1, due to potential floating point residuals
  # or if the loop finished due to max_iter without perfect convergence to sum=1.
  sum_final_estimates <- sum(current_constrained_estimates)
  if (sum_final_estimates > 1e-9) { # Avoid division by zero if all are zero
    cook_constrained_final <- current_constrained_estimates / sum_final_estimates
  } else if (np > 0) {
    warning("Constrained Cook estimator resulted in all zero proportions; defaulting to 1/np.", call. = FALSE)
    cook_constrained_final <- rep(1/np, np)
  } else {
    cook_constrained_final <- numeric(0) # Case np = 0
  }

  # Ensure strict non-negativity for final output due to potential floating point issues
  cook_constrained_final[cook_constrained_final < 0] <- 0

  return(list(
    raw = raw_proportions,
    cook = cook_corrected_estimates,
    cook_constrained = cook_constrained_final
  ))
}

#' Standard Expectation-Maximization (EM) Algorithm for Stock Proportions
#'
#' Estimates mixing proportions (theta) from a likelihood matrix using the EM algorithm.
#'
#' @param likelihood A numeric matrix (N_rows x N_populations) where `likelihood[i, j]`
#'                 is $P(Data_i | Stock_j)$ or a value proportional to it.
#'                 N_rows can be N_samples (for Theta5) or N_populations (for Theta4).
#' @param np Integer, the number of populations (stocks). Should match `ncol(likelihood)`.
#' @param freq Optional numeric vector of frequencies/weights for each "observation"
#'             (row in `likelihood`). Length must match `nrow(likelihood)`.
#'             Defaults to `rep(1, nrow(likelihood))`.
#' @param max_iter Integer, the maximum number of iterations for the EM algorithm.
#' @param tol Numeric, the convergence tolerance. The algorithm stops if the maximum
#'            absolute change in any `theta` component is less than `tol`.
#' @param verbose Logical, if TRUE, prints iteration number and current theta estimates.
#' @param save_theta_path Optional character string. If provided, the history of theta
#'                        estimates at each iteration is saved to a CSV file at this path.
#'
#' @return A numeric vector of estimated stock proportions (theta), length `np`.
#' @export
em_algorithm <- function(likelihood, np, freq = NULL,
                         max_iter = 100, tol = 1e-6,
                         verbose = FALSE, save_theta_path = NULL) {
  # --- Input Validation and Setup ---
  if (!is.matrix(likelihood) || !is.numeric(likelihood)) {
    stop("'likelihood' must be a numeric matrix.")
  }
  if (ncol(likelihood) != np) {
    stop("Number of columns in 'likelihood' matrix must be equal to 'np'.")
  }
  N_rows_likelihood <- nrow(likelihood) # Number of "observation types" or actual observations

  if (N_rows_likelihood == 0) {
    if (np > 0) {
      warning("EM algorithm: 'likelihood' matrix has 0 rows. Returning initial equal proportions.", call. = FALSE)
      return(rep(1 / np, np))
    } else { # np is also 0
      return(numeric(0))
    }
  }

  if (is.null(freq)) {
    freq <- rep(1, N_rows_likelihood)
  } else if (!is.numeric(freq) || length(freq) != N_rows_likelihood) {
    stop("Length of 'freq' must match the number of rows in 'likelihood', and it must be numeric.")
  }
  if(any(is.na(likelihood)) || any(!is.finite(likelihood))){
    stop("EM algorithm: 'likelihood' matrix contains NA or non-finite values.")
  }
  if(any(is.na(freq)) || any(!is.finite(freq)) || any(freq < 0)){
    stop("EM algorithm: 'freq' vector contains NA, non-finite, or negative values.")
  }

  # Total "effective" number of samples (sum of frequencies)
  n_effective_sum_freq <- sum(freq)
  if (n_effective_sum_freq <= 0 && N_rows_likelihood > 0) { # Only warn if there were rows but freqs summed to 0
    warning("EM algorithm: Sum of frequencies is zero or negative. Returning initial equal proportions.", call. = FALSE)
    if (np > 0) return(rep(1 / np, np)) else return(numeric(0))
  }
  # If N_rows_likelihood is 0, n_effective_sum_freq will be 0 too, handled above.


  # Initial guess for theta: equal proportions
  theta <- if (np > 0) rep(1 / np, np) else numeric(0)
  if (np == 0) return(theta) # No proportions to estimate if np=0

  theta_history <- list() # To store theta at each iteration if save_theta_path is provided

  # --- EM Iterations ---
  for (iter in 1:max_iter) {
    prev_theta_for_diff_check <- theta # Store theta from start of this iteration for convergence check

    # E-step: Calculate expected membership (posterior probabilities Z_ij or responsibilities)
    # Z_ik = P(sample_i from stock_k | Data_i, current_theta)
    #      = (Likelihood_ik * theta_k) / sum_j(Likelihood_ij * theta_j)

    # Numerator: likelihood[i,k] * theta[k]
    # sweep applies theta[k] to each column k of likelihood matrix
    weighted_likelihoods <- sweep(likelihood, MARGIN = 2, STATS = theta, FUN = `*`)

    # Denominator: sum_j (Likelihood_ij * theta_j) for each sample i (row of likelihood)
    # This is the marginal likelihood P(Data_i | current_theta) for each observation type/sample
    marginal_likelihood_per_row <- rowSums(weighted_likelihoods)

    # Avoid division by zero if a row's marginal likelihood is zero.
    # This can happen if likelihoods are extremely small or some thetas are effectively zero.
    # Replace near-zero sums with a small epsilon to prevent NaN/Inf in posterior_Z.
    marginal_likelihood_per_row[marginal_likelihood_per_row < .Machine$double.eps] <- .Machine$double.eps

    posterior_Z <- weighted_likelihoods / marginal_likelihood_per_row # N_rows_likelihood x NP matrix

    # M-step: Update theta
    # theta_k_new = sum_i (posterior_Z_ik * freq_i) / sum_all_freqs
    # where sum_all_freqs is n_effective_sum_freq.

    # Sum of (posterior_Z_ik * freq_i) over all "observations" i, for each stock k
    # This is colSums(posterior_Z * freq), where freq is broadcasted or applied row-wise.
    sum_weighted_Z_per_stock <- colSums(posterior_Z * freq) # Results in a vector of length NP

    theta_new <- sum_weighted_Z_per_stock / n_effective_sum_freq

    # Numerical stability: ensure theta_new sums to 1 and components are non-negative.
    if (sum(theta_new) > .Machine$double.eps) { # Avoid division by zero if all are zero
      theta_new <- theta_new / sum(theta_new)
    } else { # All components of theta_new are effectively zero
      warning(paste("EM iteration", iter, ": All components of theta_new are near zero. Resetting to 1/np."), call. = FALSE)
      theta_new <- rep(1 / np, np) # Reset to avoid NaN/Inf propagation
    }
    theta_new[theta_new < 0] <- 0 # Ensure non-negativity

    # --- Convergence Check and Verbose Output ---
    max_abs_change <- max(abs(theta_new - prev_theta_for_diff_check))

    if (verbose) {
      cat("\nEM Iteration:", iter, "\n")
      cat("Theta_new: ", paste(sapply(theta_new, function(x) sprintf("%.6f", x)), collapse = ", "), "\n")
      cat("Max abs diff from prev_theta:", signif(max_abs_change, 6), "\n")
    }

    if (save_theta_path != FALSE && !is.null(save_theta_path)) {
      theta_history[[iter]] <- theta_new
    }

    theta <- theta_new # Update theta for the next iteration

    if (max_abs_change < tol) {
      if (verbose) cat("\nEM converged at iteration", iter, "to tolerance", tol, "\n")
      break
    }

    if (iter == max_iter && verbose) {
      warning("EM algorithm reached maximum iterations (", max_iter,
              ") without full convergence to tolerance (", tol,
              "). Max change was: ", signif(max_abs_change, 6), call. = FALSE)
    }
  } # End of EM iterations

  # Save theta history if path provided
  if (save_theta_path != FALSE && !is.null(save_theta_path) && length(theta_history) > 0) {
    tryCatch({
      theta_df <- do.call(rbind, theta_history)
      colnames(theta_df) <- paste0("Stock", 1:np)
      theta_df <- data.frame(Iteration = seq_along(theta_history), theta_df)
      write.csv(theta_df, file = save_theta_path, row.names = FALSE)
      if(verbose) cat("Theta history saved to:", save_theta_path, "\n")
    }, error = function(e){
      warning(paste("Could not save theta history to CSV:", conditionMessage(e)), call. = FALSE)
    })
  }

  return(theta)
}


#' Accelerated Expectation-Maximization (EM) Algorithm
#'
#' Estimates mixing proportions (theta) using an EM algorithm with a simple acceleration step.
#' The acceleration is attempted based on changes in theta over iterations and log-likelihood improvement.
#' This is a simplified interpretation of HISEA's ACCEL subroutine.
#'
#' @inheritParams em_algorithm
#'
#' @return A numeric vector of estimated stock proportions (theta), length `np`.
#' @export
accel_em_algorithm <- function(likelihood, np, freq = NULL,
                               max_iter = 100, tol = 1e-6,
                               verbose = FALSE, save_theta_path = NULL) {
  # --- Input Validation and Setup --- (Identical to em_algorithm)
  if (!is.matrix(likelihood) || !is.numeric(likelihood)) {
    stop("'likelihood' must be a numeric matrix.")
  }
  if (ncol(likelihood) != np) {
    stop("Number of columns in 'likelihood' matrix must be equal to 'np'.")
  }
  N_rows_likelihood <- nrow(likelihood)
  if (N_rows_likelihood == 0) {
    if (np > 0) {
      warning("Accelerated EM: 'likelihood' matrix has 0 rows. Returning initial equal proportions.", call. = FALSE)
      return(rep(1 / np, np))
    } else { return(numeric(0)) }
  }
  if (is.null(freq)) {
    freq <- rep(1, N_rows_likelihood)
  } else if (!is.numeric(freq) || length(freq) != N_rows_likelihood) {
    stop("Length of 'freq' must match 'nrow(likelihood)', and it must be numeric.")
  }
  if(any(is.na(likelihood)) || any(!is.finite(likelihood))){
    stop("Accelerated EM: 'likelihood' matrix contains NA or non-finite values.")
  }
  if(any(is.na(freq)) || any(!is.finite(freq)) || any(freq < 0)){
    stop("Accelerated EM: 'freq' vector contains NA, non-finite, or negative values.")
  }
  n_effective_sum_freq <- sum(freq)
  if (n_effective_sum_freq <= 0 && N_rows_likelihood > 0) {
    warning("Accelerated EM: Sum of frequencies is zero or negative. Returning initial equal proportions.", call. = FALSE)
    if (np > 0) return(rep(1 / np, np)) else return(numeric(0))
  }

  theta <- if (np > 0) rep(1 / np, np) else numeric(0)
  if (np == 0) return(theta)

  # Variables for acceleration
  diff1_current_step <- numeric(np)
  diff2_prev_step <- numeric(np)

  log_likelihood_previous_accepted <- -Inf # Log-likelihood from the previously accepted theta

  theta_history <- list()

  # --- Accelerated EM Iterations ---
  for (iter in 1:max_iter) {
    prev_theta_for_diff_check <- theta # Theta at the start of this iteration

    # --- Standard EM Step (E-step and M-step) ---
    weighted_likelihoods_em <- sweep(likelihood, MARGIN = 2, STATS = theta, FUN = `*`)
    marginal_likelihood_per_row_em <- rowSums(weighted_likelihoods_em)

    # Safe marginal likelihood for log and division
    marginal_l_safe_em <- marginal_likelihood_per_row_em
    marginal_l_safe_em[marginal_l_safe_em < .Machine$double.eps] <- .Machine$double.eps

    # Calculate log-likelihood for current theta (before M-step)
    log_likelihood_at_iter_start <- sum(freq * log(marginal_l_safe_em))

    posterior_Z_em <- weighted_likelihoods_em / marginal_l_safe_em
    sum_weighted_Z_per_stock_em <- colSums(posterior_Z_em * freq)
    theta_after_em_step <- sum_weighted_Z_per_stock_em / n_effective_sum_freq

    if (sum(theta_after_em_step) > .Machine$double.eps) {
      theta_after_em_step <- theta_after_em_step / sum(theta_after_em_step)
    } else {
      warning(paste("AccelEM iteration", iter, ": All EM step thetas near zero. Resetting."), call. = FALSE)
      theta_after_em_step <- rep(1/np, np)
    }
    theta_after_em_step[theta_after_em_step < 0] <- 0

    # Update differences for acceleration logic
    diff2_prev_step <- diff1_current_step       # Store previous step's difference
    diff1_current_step <- theta_after_em_step - theta # Current EM step's difference

    max_abs_change_em_step <- max(abs(diff1_current_step))

    if (verbose) {
      cat("\nAccelEM Iteration:", iter, "(EM Step Output)\n")
      cat("LogLik (from theta at iter start):", signif(log_likelihood_at_iter_start, 7), "\n")
      cat("Theta (after EM step): ", paste(sapply(theta_after_em_step, function(x) sprintf("%.6f",x)), collapse = ", "), "\n")
      cat("Max abs diff (EM step from theta at iter start):", signif(max_abs_change_em_step, 6), "\n")
    }

    if (save_theta_path != FALSE && !is.null(save_theta_path)) {
      theta_history[[paste0(iter, "_em")]] <- theta_after_em_step
    }

    # Check for convergence based on the standard EM step before attempting acceleration
    if (max_abs_change_em_step < tol) {
      if (verbose) cat("\nAccelerated EM converged at iteration", iter, "(based on EM step difference to tol)\n")
      theta <- theta_after_em_step
      break
    }

    # --- Acceleration Step Attempt ---
    theta_next_candidate <- theta_after_em_step # Default is the standard EM update

    # Attempt acceleration after a few initial EM iterations (e.g., iter >= 3)
    # and if the previous step (diff2_prev_step) was non-trivial.
    if (iter >= 3 && all(abs(diff2_prev_step) > (tol / 100))) { # Avoid division by zero or unstable ratios

      # Estimate convergence rate 'r' (Aitken's delta-squared process idea)
      # r_k = (theta_{k+1} - theta_k) / (theta_k - theta_{k-1}) = diff1_current_step_k / diff2_prev_step_k
      # We need to handle component-wise ratios carefully.
      r_estimates_k <- diff1_current_step / diff2_prev_step
      r_estimates_k[!is.finite(r_estimates_k)] <- 1 # Default if diff2_k was zero, to avoid large alpha

      # Use a robust measure, e.g., median, and ensure it's within reasonable bounds for stability
      r_median <- stats::median(r_estimates_k[abs(diff2_prev_step) > (tol/100)], na.rm = TRUE)
      if (is.na(r_median)) r_median <- 1 # Fallback if all diff2 small

      alpha_aitken <- 1.0 # Default: no acceleration (alpha_aitken-1 = 0)
      if (r_median < (1.0 - sqrt(.Machine$double.eps)) && r_median > -1.0) { # Check for stable r_median
        alpha_aitken = 1.0 / (1.0 - r_median)
      } else { # r_median indicates instability or very fast convergence/divergence
        if (verbose) cat("Acceleration skipped: r_median (", signif(r_median,4) ,") out of stable range.\n")
      }

      # Aitken extrapolation: theta_k - ( (delta_theta_k)^2 / (delta_theta_k - delta_theta_{k-1}) )
      # Or: theta_accelerated = theta_current_EM_step - (alpha_aitken * diff1_current_step) if r_median is rate
      # A common form: x_{k+1} = x_k - step * grad_f(x_k)
      # EM is x_{k+1} = M(x_k). Accelerated: x_{k+1} = M(x_k) - C * (M(x_k) - x_k) where C involves (1-r)^-1
      # Let's use a common squared EM step idea (SQUAREM line search)
      # step = -r/(1-r) if using vector r, or more simply, find optimal step.
      # HISEA Fortran ACCEL: theta_candidate = theta_old + ExtrapolationFactor * diff1_from_theta_old
      # Here, diff1_current_step = theta_after_em_step - theta (theta_old for this EM output)

      # Try a simple Aitken-like step from the previous theta
      # Extrapolated theta: theta_prev + (factor) * (theta_em_step - theta_prev)
      # factor = 1/(1-r_median)
      if (alpha_aitken > 0 && alpha_aitken < 20) { # Bound the extrapolation factor
        theta_accelerated_try <- theta + alpha_aitken * diff1_current_step

        # Ensure non-negativity and sum to 1 for the accelerated candidate
        theta_accelerated_try[theta_accelerated_try < 0] <- 0
        if (sum(theta_accelerated_try) > .Machine$double.eps) {
          theta_accelerated_try <- theta_accelerated_try / sum(theta_accelerated_try)
        } else {
          theta_accelerated_try <- rep(1/np, np) # Fallback
        }

        # Check if the accelerated step increases log-likelihood compared to *theta at start of iter*
        weighted_l_accel <- sweep(likelihood, MARGIN = 2, STATS = theta_accelerated_try, FUN = `*`)
        marginal_l_accel_safe <- rowSums(weighted_l_accel)
        marginal_l_accel_safe[marginal_l_accel_safe < .Machine$double.eps] <- .Machine$double.eps
        log_likelihood_accel_candidate <- sum(freq * log(marginal_l_accel_safe))

        if (log_likelihood_accel_candidate > log_likelihood_at_iter_start) { # Compare to LL before this iter's EM step
          theta_next_candidate <- theta_accelerated_try # Accept accelerated step
          if (verbose) {
            cat("--- Accepted Accelerated Step ---\n")
            cat("LogLik (Accel candidate):", signif(log_likelihood_accel_candidate, 7), "\n")
            cat("Theta (Accel candidate): ", paste(sapply(theta_next_candidate,function(x)sprintf("%.6f",x)), collapse = ", "), "\n")
          }
          if (save_theta_path != FALSE && !is.null(save_theta_path)) {
            theta_history[[paste0(iter, "_accel")]] <- theta_next_candidate
          }
          # Update diff1 based on accepted accelerated step for next iteration's diff2 calculation
          # This diff1 is (theta_accelerated - theta_at_start_of_iter)
          diff1_current_step = theta_next_candidate - theta
        } else {
          if (verbose) cat("--- Rejected Accelerated Step (LogLik did not improve sufficiently vs start of iter) ---\n")
          # If rejected, diff1_current_step remains (theta_after_em_step - theta)
        }
      } else {
        if (verbose) cat("--- Acceleration factor (alpha_aitken=", signif(alpha_aitken,4) ,") out of bounds, using EM step. ---\n")
      }
    } # End of acceleration attempt block

    # Update theta for the next iteration using the chosen candidate
    theta <- theta_next_candidate

    # Final convergence check for this iteration, based on change from prev_theta_for_diff_check to current theta
    if (max(abs(theta - prev_theta_for_diff_check)) < tol) {
      if (verbose) cat("\nAccelerated EM converged at iteration", iter, " (based on accepted theta to tol)\n")
      break
    }

    if (iter == max_iter && verbose) {
      warning("Accelerated EM reached maximum iterations (", max_iter,
              ") without full convergence to tolerance (", tol, "). Max change was: ",
              signif(max(abs(theta - prev_theta_for_diff_check)), 6), call. = FALSE)
    }
  } # End of iterations

  # Save theta history if path provided
  if (save_theta_path != FALSE && !is.null(save_theta_path) && length(theta_history) > 0) {
    tryCatch({
      # Need to handle the _em and _accel tags if just collating
      # For simplicity, just rbind what was stored.
      final_theta_history_df <- data.frame()
      iter_count = 0
      for(name in names(theta_history)){
        iter_count = iter_count + 1
        temp_df = data.frame(Iteration_Step = name, t(theta_history[[name]]))
        colnames(temp_df)[-1] <- paste0("Stock",1:np)
        final_theta_history_df <- rbind(final_theta_history_df, temp_df)
      }
      write.csv(final_theta_history_df, file = save_theta_path, row.names = FALSE)
      if(verbose) cat("Theta history (incl. EM/Accel steps) saved to:", save_theta_path, "\n")
    }, error = function(e){
      warning(paste("Could not save theta history to CSV:", conditionMessage(e)), call. = FALSE)
    })
  }

  return(theta)
}


#' Estimate Millar Theta4 Parameter
#'
#' This function estimates the theta4 parameter using the Millar method.
#'
#' @param class_predictions_mixed_sample Numeric vector of predicted classes for the mixed sample
#' @param PHI_matrix Confusion matrix used for correction
#' @param np Integer number of populations
#' @param use_accelerated_em Logical; whether to use the accelerated EM version
#' @param ... Additional arguments passed to internal functions
#' @return Numeric value of the estimated theta4 parameter
#' @export
#' @examples
#' \dontrun{
#' pred_classes <- c(1, 2, 1, 2)
#' phi_mat <- matrix(c(0.8, 0.2, 0.2, 0.8), 2, 2)
#' theta4 <- estimate_millar_theta4(pred_classes, phi_mat, np = 2)
#' }
estimate_millar_theta4 <- function(class_predictions_mixed_sample, PHI_matrix, np,
                                   use_accelerated_em = TRUE, ...) {
  # --- Input Validation ---
  if (!is.numeric(np) || length(np) != 1 || np < 1 || floor(np) != np ) {
    stop("'np' must be a single positive integer.")
  }
  if (length(class_predictions_mixed_sample) == 0) {
    warning("Estimate Millar (Theta4): 'class_predictions_mixed_sample' is empty. Returning equal proportions if np > 0.", call. = FALSE)
    return(if (np > 0) rep(1/np, np) else numeric(0))
  }
  if (!is.numeric(class_predictions_mixed_sample) || !all(floor(class_predictions_mixed_sample) == class_predictions_mixed_sample) ||
      any(class_predictions_mixed_sample < 1) || any(class_predictions_mixed_sample > np)) {
    stop("'class_predictions_mixed_sample' must be a vector of integers between 1 and np.")
  }
  if (!is.matrix(PHI_matrix) || !is.numeric(PHI_matrix) || nrow(PHI_matrix) != np || ncol(PHI_matrix) != np) {
    stop("'PHI_matrix' must be an np x np numeric matrix.")
  }

  # Y_k: Counts of fish *classified* into each stock k from the mixed sample.
  # These are the "frequencies" for the rows of PHI_matrix when passed to the EM algorithm.
  observed_classification_counts_Yk <- as.numeric(table(factor(class_predictions_mixed_sample, levels = 1:np)))

  # Choose the EM algorithm
  em_function_to_use <- if (use_accelerated_em) accel_em_algorithm else em_algorithm

  # Call the EM algorithm:
  # - The `likelihood` argument for EM is PHI_matrix. Each row 'k' of PHI_matrix represents
  #   P(classified as k | stock 1), P(classified as k | stock 2), ..., P(classified as k | stock np).
  # - The `freq` argument for EM is `observed_classification_counts_Yk`. This vector indicates
  #   how many times each "classification outcome k" (row of PHI_matrix) was observed.
  theta_millar <- em_function_to_use(
    likelihood = PHI_matrix,
    np = np,
    freq = observed_classification_counts_Yk,
    ... # Pass other args like max_iter, tol, verbose, save_theta_path
  )

  return(theta_millar)
}


#' Estimate Stock Composition Using Direct Maximum Likelihood (Theta5)
#'
#' Applies an EM algorithm to obtain the direct Maximum Likelihood Estimate (MLE)
#' of stock proportions (Theta_5 in HISEA). This uses the likelihood of each
#' individual mixed fishery sample fish belonging to each stock.
#' The likelihood maximized is:
#'
#' @param individual_likelihoods A numeric matrix (N_mixed_samples x NP) where
#'        `individual_likelihoods[i,j]` is $P(Data_i | Stock_j)$, the likelihood
#'        of observing the data for mixed sample fish `i` given it came from stock `j`.
#' @param np Integer, the number of populations (stocks).
#' @param freq Optional numeric vector of frequencies/weights for each sample (row) in
#'             `individual_likelihoods`. Defaults to `rep(1, nrow(individual_likelihoods))`.
#' @param use_accelerated_em Logical, if TRUE, uses `accel_em_algorithm`, otherwise `em_algorithm`. Default TRUE.
#' @param ... Additional arguments passed to the chosen EM algorithm
#'            (e.g., `max_iter`, `tol`, `verbose`, `save_theta_path`).
#'
#' @return A numeric vector of estimated stock proportions (Theta5).
#' @export
estimate_ml_theta5 <- function(individual_likelihoods, np, freq = NULL, use_accelerated_em = TRUE, ...) {
  # --- Input Validation ---
  if (!is.numeric(np) || length(np) != 1 || np < 1 || floor(np) != np ) {
    stop("'np' must be a single positive integer.")
  }
  if (!is.matrix(individual_likelihoods) || !is.numeric(individual_likelihoods)) {
    stop("'individual_likelihoods' must be a numeric matrix.")
  }
  if (ncol(individual_likelihoods) != np) {
    stop("Number of columns in 'individual_likelihoods' must be equal to 'np'.")
  }
  if (nrow(individual_likelihoods) == 0) {
    warning("Estimate ML (Theta5): 'individual_likelihoods' matrix has 0 rows. Returning equal proportions if np > 0.", call. = FALSE)
    return(if (np > 0) rep(1/np, np) else numeric(0))
  }

  if (is.null(freq)) {
    freq <- rep(1, nrow(individual_likelihoods)) # Default: each fish is one observation
  } else if (!is.numeric(freq) || length(freq) != nrow(individual_likelihoods)) {
    stop("Length of 'freq' must match nrow(individual_likelihoods) and be numeric.")
  }

  # Choose the EM algorithm
  em_function_to_use <- if (use_accelerated_em) accel_em_algorithm else em_algorithm

  theta_ml <- em_function_to_use(
    likelihood = individual_likelihoods, # P(Data_i | Stock_j)
    np = np,
    freq = freq, # Weights for each fish/row in individual_likelihoods
    ... # Pass other args like max_iter, tol, verbose, save_theta_path
  )
  return(theta_ml)
}

# --- Assigning implementations to generic names for use by run_hisea_final ---
# This assumes run_hisea_final will call 'estimate_millar' for Theta4
# and 'estimate_ml' for Theta5.

estimate_millar <- estimate_millar_theta4
estimate_ml <- estimate_ml_theta5
