# results.R
utils::globalVariables(c("Stock", "Mean", "Estimator", "SD", "ActualValue"))
# Ensure utils::globalVariables is at the top level if this were a package,
# or ensure necessary packages are loaded by the main script.
# utils::globalVariables(c("Estimator", "Mean", "Stock", "SD"))

#' Create HISEA Summary Report
#'
#' Generates a comprehensive summary from HISEA estimation results, including
#' means, standard deviations, MSE (for simulations), and covariance/correlation
#' for ML estimates (often for bootstrap).
#'
#' @param all_estimates_array An array of dimension (nsamps, np, n_estimators)
#'                            containing all estimation results. The 3rd dimension
#'                            should have estimator names.
#' @param actual_proportions Optional numeric vector of true stock proportions.
#'                           Required if MSE is to be calculated (typically for "SIMULATION" run_type).
#' @param run_type Character string, e.g., "SIMULATION", "ANALYSIS", "BOOTSTRAP".
#'                 Used to decide if actual_proportions are expected for MSE.
#' @return A list containing the summary statistics.
#' @importFrom stats cov sd var cov2cor complete.cases
create_hisea_summary_report <- function(all_estimates_array, actual_proportions = NULL, run_type) {
  # --- Input Validation ---
  if (!is.array(all_estimates_array) || length(dim(all_estimates_array)) != 3) {
    stop("'all_estimates_array' must be a 3-dimensional array (nsamps, np, n_estimators).")
  }
  nsamps <- dim(all_estimates_array)[1]
  num_populations <- dim(all_estimates_array)[2]
  num_estimators <- dim(all_estimates_array)[3]

  estimator_names <- dimnames(all_estimates_array)[[3]]
  stock_dimnames <- dimnames(all_estimates_array)[[2]]

  if (is.null(estimator_names) || length(estimator_names) != num_estimators) {
    # Suppressed: warning("Estimator names (3rd dim of all_estimates_array) are missing or length mismatch. Using default Est1, Est2,...")
    if (num_estimators > 0) estimator_names <- paste0("Estimator", 1:num_estimators) else estimator_names <- character(0)
  }
  if (is.null(stock_dimnames) || length(stock_dimnames) != num_populations) {
    if (num_populations > 0) {
      # Suppressed: warning("Stock names (2nd dim of all_estimates_array) are missing or length mismatch. Using default Pop1, Pop2,...")
      stock_dimnames <- paste0("Pop", 1:num_populations)
    } else {
      stock_dimnames <- character(0)
    }
  }

  # --- Basic Statistics (Mean, SD, Empirical Variance) ---
  mean_estimates <- apply(all_estimates_array, c(2, 3), mean, na.rm = TRUE)
  sd_estimates <- apply(all_estimates_array, c(2, 3), sd, na.rm = TRUE)
  var_emp <- apply(all_estimates_array, c(2, 3), function(x) {
    if (all(is.na(x))) return(NA_real_)
    mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2
  })

  if (num_populations > 0 && num_estimators > 0) {
    dimnames(mean_estimates) <- list(stock_dimnames, estimator_names)
    dimnames(sd_estimates) <- list(stock_dimnames, estimator_names)
    dimnames(var_emp) <- list(stock_dimnames, estimator_names)
  } else {
    mean_estimates <- matrix(numeric(0), nrow=num_populations, ncol=num_estimators, dimnames=list(stock_dimnames, estimator_names))
    sd_estimates <- matrix(numeric(0), nrow=num_populations, ncol=num_estimators, dimnames=list(stock_dimnames, estimator_names))
    var_emp <- matrix(numeric(0), nrow=num_populations, ncol=num_estimators, dimnames=list(stock_dimnames, estimator_names))
  }

  # --- MSE Calculation ---
  mse_estimates <- NULL
  if (!is.null(actual_proportions)) {
    if (length(actual_proportions) != num_populations) {
      stop("Length of 'actual_proportions' (", length(actual_proportions),
           ") must match the number of populations (", num_populations, ").")
    }
    if (num_populations > 0 && num_estimators > 0) {
      mse_estimates <- matrix(NA_real_, nrow = num_populations, ncol = num_estimators,
                              dimnames = list(stock_dimnames, estimator_names))
      for (j in 1:num_populations) {
        for (k in 1:num_estimators) {
          if (!is.na(mean_estimates[j,k]) && !is.na(sd_estimates[j,k]) && !is.na(actual_proportions[j])) {
            bias_jk <- mean_estimates[j, k] - actual_proportions[j]
            var_jk <- sd_estimates[j, k]^2
            mse_estimates[j, k] <- sqrt(var_jk + bias_jk^2)
          }
        }
      }
    } else {
      mse_estimates <- matrix(numeric(0), nrow=num_populations, ncol=num_estimators, dimnames=list(stock_dimnames, estimator_names))
    }
  }

  # --- ML Specific Calculations ---
  na_matrix_ml <- matrix(NA_real_, nrow = num_populations, ncol = num_populations,
                         dimnames = list(stock_dimnames, stock_dimnames))
  covar_ml <- na_matrix_ml
  cor_ml <- na_matrix_ml
  covar_inv_ml <- na_matrix_ml
  det_covar_ml <- NA_real_

  if ("ML" %in% estimator_names && num_populations > 0) {
    ml_estimates_for_cov <- all_estimates_array[, , "ML", drop = TRUE]

    if (nsamps < 2) {
      # Suppressed: warning("ML Covariance/Correlation cannot be computed with nsamps < 2. ML cov/cor metrics will be NA.")
    } else {
      if (is.vector(ml_estimates_for_cov)) {
        ml_estimates_for_cov <- matrix(ml_estimates_for_cov, ncol = 1, dimnames = list(NULL, stock_dimnames[1]))
      }

      valid_ml_rows <- stats::complete.cases(ml_estimates_for_cov)
      if(sum(valid_ml_rows) < 2) {
        # Suppressed: warning("Not enough complete cases (nsamps >= 2) in ML estimates to compute covariance after NA removal.")
      } else {
        ml_estimates_for_cov_complete <- ml_estimates_for_cov[valid_ml_rows, , drop = FALSE]

        covar_ml_calculated <- tryCatch({
          stats::cov(ml_estimates_for_cov_complete)
        }, error = function(e) {
          # Suppressed: warning("stats::cov() failed for ML estimates: ", e$message)
          return(na_matrix_ml)
        })

        if(!all(is.na(covar_ml_calculated))) {
          covar_ml <- covar_ml_calculated
          if (nrow(covar_ml) == num_populations && ncol(covar_ml) == num_populations) {
            dimnames(covar_ml) <- list(stock_dimnames, stock_dimnames)
          }

          cov2rho <- function(cv_mat) {
            na_mat_local <- matrix(NA_real_, nrow=nrow(cv_mat), ncol=ncol(cv_mat), dimnames=dimnames(cv_mat))
            if (anyNA(cv_mat)) return(na_mat_local)
            if (is.numeric(cv_mat) && length(cv_mat) == 1 && is.matrix(cv_mat) && all(dim(cv_mat) == c(1,1))) {
              return(matrix(1,1,1, dimnames=dimnames(cv_mat)))
            }
            if (!is.matrix(cv_mat) || nrow(cv_mat) != ncol(cv_mat)) {
              # This is a stop condition, not a warning, so keep it or handle appropriately
              stop("Input to cov2rho must be a square matrix.")
            }
            cor_m <- suppressWarnings(tryCatch(stats::cov2cor(cv_mat),
                                               error = function(e) {
                                                 # Suppressed: warning("stats::cov2cor failed: ", e$message)
                                                 return(na_mat_local)
                                               }))
            if(!is.null(cor_m) && !anyNA(cor_m)) diag(cor_m) <- 1.0
            return(cor_m)
          }
          cor_ml <- cov2rho(covar_ml)

          mxint <- function(sigma_matrix) {
            inv_mat_na_local <- matrix(NA_real_, nrow=nrow(sigma_matrix), ncol=ncol(sigma_matrix), dimnames=dimnames(sigma_matrix))
            if (anyNA(sigma_matrix)) return(list(inv = inv_mat_na_local, det = NA_real_))

            if (is.numeric(sigma_matrix) && length(sigma_matrix) == 1 && is.matrix(sigma_matrix) && all(dim(sigma_matrix) == c(1,1))) {
              s_val <- sigma_matrix[1,1]
              if (abs(s_val) < .Machine$double.eps^0.5) {
                # Suppressed: warning("ML estimate variance is singular or near-zero for single stock (mxint).")
                return(list(inv = matrix(Inf,1,1, dimnames=dimnames(sigma_matrix)), det = 0))
              }
              return(list(inv = matrix(1/s_val,1,1, dimnames=dimnames(sigma_matrix)), det = s_val))
            }
            if (!is.matrix(sigma_matrix) || nrow(sigma_matrix) != ncol(sigma_matrix)) {
              stop("sigma_matrix for mxint must be a square matrix.")
            }

            det_val <- suppressWarnings(tryCatch(det(sigma_matrix), error = function(e) {
              # Suppressed: warning("Determinant calculation failed for ML covariance matrix (mxint): ", e$message)
              return(NA_real_)
            }))
            if(is.na(det_val)) return(list(inv = inv_mat_na_local, det = NA_real_))

            inv_mat <- inv_mat_na_local
            if (abs(det_val) < .Machine$double.eps * max(1, nrow(sigma_matrix)) * max(abs(sigma_matrix), na.rm=TRUE)) {
              if(requireNamespace("MASS", quietly = TRUE)){
                inv_mat_try <- suppressWarnings(tryCatch(MASS::ginv(sigma_matrix), error = function(e_ginv){ inv_mat_na_local}))
                if(!is.null(inv_mat_try)) inv_mat <- inv_mat_try
              } else {
                # Suppressed: warning("MASS package not available for ginv() in mxint. Inverse will be NA.")
              }
            } else {
              inv_mat_try <- suppressWarnings(tryCatch(solve(sigma_matrix), error = function(e_solve){
                if(requireNamespace("MASS", quietly = TRUE)){
                  return(suppressWarnings(tryCatch(MASS::ginv(sigma_matrix), error = function(e_ginv2){inv_mat_na_local})))
                } else {
                  return(inv_mat_na_local)
                }
              }))
              if(!is.null(inv_mat_try)) inv_mat <- inv_mat_try
            }
            return(list(inv = inv_mat, det = det_val))
          }
          inv_info_ml <- mxint(covar_ml)
          covar_inv_ml <- inv_info_ml$inv
          det_covar_ml <- inv_info_ml$det
        }
      }
    }
  } else if (!("ML" %in% estimator_names)) {
    # Suppressed: warning("Estimator 'ML' not found in all_estimates_array. ML-specific metrics will be NA.")
  } else if (num_populations == 0) {
    # Suppressed: warning("Number of populations is 0. ML-specific metrics will be NA.")
  }

  return(list(
    mean_estimates = mean_estimates,
    sd_estimates   = sd_estimates,
    mse_estimates  = mse_estimates,
    var_emp        = var_emp,
    covar_ml       = covar_ml,
    cor_ml         = cor_ml,
    covar_inv_ml   = covar_inv_ml,
    det_covar_ml   = det_covar_ml
  ))
}

#' Plot HISEA theta estimates with error bars
#' utils::globalVariables(c("Stock", "Mean", "Estimator", "SD", "ActualValue"))
#' @param summary_report Output from code{create_hisea_summary_report}.
#' @param stock_names Optional character vector for stock labels.
#' @param actual_proportions Optional numeric vector of true proportions.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_bar geom_errorbar theme_minimal labs scale_fill_brewer position_dodge geom_hline facet_wrap element_text coord_fixed
#' @importFrom tidyr pivot_longer
plot_hisea_theta <- function(summary_report, stock_names = NULL, actual_proportions = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'ggplot2' and 'tidyr' are required for plot_hisea_theta. Please install them.")
  }

  theta_mean <- summary_report$mean_estimates
  theta_sd <- summary_report$sd_estimates

  if(is.null(theta_mean) || is.null(theta_sd) || nrow(theta_mean) == 0 || ncol(theta_mean) == 0) {
    # Suppressed: warning("Mean or SD estimates are missing or empty. Cannot generate plot.")
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data available for plotting estimates."))
  }

  num_populations <- nrow(theta_mean)
  estimator_names <- colnames(theta_mean)
  if(is.null(estimator_names)) estimator_names <- paste0("Estimator", 1:ncol(theta_mean))

  if (is.null(stock_names)) {
    stock_names <- rownames(theta_mean)
    if (is.null(stock_names) || length(stock_names) != num_populations) {
      # Suppressed: warning("Using default stock names (Pop1, Pop2,...) for plot as names were not provided or mismatched.")
      stock_names <- paste0("Pop", 1:num_populations)
    }
  } else if (length(stock_names) != num_populations) {
    # Suppressed: warning("Length of provided 'stock_names' does not match number of populations. Using default names for plot.")
    stock_names <- paste0("Pop", 1:num_populations)
  }

  rownames(theta_mean) <- stock_names
  rownames(theta_sd) <- stock_names

  mean_df <- as.data.frame(theta_mean)
  mean_df$Stock <- factor(stock_names, levels = stock_names)
  sd_df <- as.data.frame(theta_sd)
  sd_df$Stock <- factor(stock_names, levels = stock_names)

  plot_df_mean <- tidyr::pivot_longer(mean_df, cols = -"Stock", names_to = "Estimator", values_to = "Mean")
  plot_df_sd <- tidyr::pivot_longer(sd_df, cols = -"Stock", names_to = "Estimator", values_to = "SD")

  plot_df <- merge(plot_df_mean, plot_df_sd, by = c("Stock", "Estimator"))
  plot_df$Estimator <- factor(plot_df$Estimator, levels = estimator_names)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Stock, y = Mean, fill = Estimator)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Mean - SD, ymax = Mean + SD),
                           position = ggplot2::position_dodge(width = 0.9), width = 0.25) +
    ggplot2::facet_wrap(~Stock, scales = "free_x", ncol = num_populations) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(title = "Estimated Mixing Proportions",
                  y = "Proportion Estimate", x = "Estimator") +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size=8),
                   strip.text = ggplot2::element_text(face = "bold"),
                   legend.position = "top")

  if (!is.null(actual_proportions)) {
    if (length(actual_proportions) == num_populations) {
      actual_df_for_plot <- data.frame(Stock = factor(stock_names, levels = stock_names),
                                       ActualValue = actual_proportions)
      p <- p + ggplot2::geom_hline(data = actual_df_for_plot,
                                   ggplot2::aes(yintercept = ActualValue),
                                   linetype = "dashed", color = "blue", size=0.8)
    } else {
      # Suppressed: warning("'actual_proportions' length mismatch with num_populations, not plotting reference lines.")
    }
  }
  return(p)
}


#' Print HISEA Summary Report to Console and/or File
#'
#' @param summary_report Output from code{create_hisea_summary_report}.
#' @param nsamps Number of simulation/bootstrap replicates.
#' @param actual_proportions True composition vector.
#' @param means_vars Matrix of mean variable values.
#' @param sds_vars Matrix of SD of variable values.
#' @param standard_sizes Vector of baseline sample sizes.
#' @param stocks_names Character vector of stock names.
#' @param output_file Path to the output text file.
#' @param header Optional custom header line.
#' @param run_type Character string of run type.
#' @param nv_used Integer, number of variables.
#' @param seed_val Integer, seed value.
#' @param nmix_simulated Integer, size of simulated mixture.
#' @export
print_hisea_summary <- function(summary_report,
                                nsamps,
                                actual_proportions,
                                means_vars = NULL,
                                sds_vars = NULL,
                                standard_sizes = NULL,
                                stocks_names,
                                output_file = "hisea_summary_report.txt",
                                header = NULL,
                                run_type = "SIMULATION",
                                nv_used = NULL,
                                seed_val = NULL,
                                nmix_simulated = NULL) {

  if (is.null(summary_report) || !is.list(summary_report)) {
    stop("'summary_report' must be a list.")
  }
  req_elements <- c("mean_estimates", "sd_estimates")
  if (!all(req_elements %in% names(summary_report))) {
    stop("Missing one or more required elements in 'summary_report': ", paste(req_elements[!req_elements %in% names(summary_report)], collapse=", "))
  }

  mean_est <- summary_report$mean_estimates
  sd_est <- summary_report$sd_estimates
  mse_est_sqrt <- summary_report$mse_estimates
  covar_ml_est <- summary_report$covar_ml
  cor_ml_est <- summary_report$cor_ml
  det_covar_ml_est <- summary_report$det_covar_ml

  num_populations <- if(!is.null(mean_est)) nrow(mean_est) else 0


  if (is.null(stocks_names) || (num_populations > 0 && length(stocks_names) != num_populations)) {
    # Suppressed: warning("stocks_names not provided or length mismatch for print_hisea_summary. Using defaults.")
    stocks_names_report <- if(num_populations > 0) paste0("Stock", 1:num_populations) else character(0)
  } else {
    stocks_names_report <- stocks_names
  }

  estimator_labels_report <- c("RAW", "COOK & LORD", "COOK CONSTR.", "MILLAR C.", "MAX.LIKE.")
  current_estimator_names_from_data <- if(!is.null(mean_est)) colnames(mean_est) else NULL
  if(!is.null(current_estimator_names_from_data) && length(estimator_labels_report) != ncol(mean_est)){
    # Suppressed: warning("Mismatch between hardcoded estimator labels for report and actual estimators in data. Using data colnames.")
    estimator_labels_report <- current_estimator_names_from_data
  } else if (is.null(current_estimator_names_from_data) && ncol(mean_est) > 0) {
    estimator_labels_report <- paste0("Estimator", 1:ncol(mean_est))
  }


  file_opened <- FALSE
  file_con <- NULL
  if (!is.null(output_file) && nzchar(output_file)) {
    file_con <- suppressWarnings(tryCatch(file(output_file, open = "wt"), error = function(e){ NULL }))
    if(!is.null(file_con)) {
      file_opened <- TRUE
      on.exit(close(file_con), add = TRUE)
    } else {
      # Suppressed: warning("Could not open output_file '", output_file, "': ", e$message, ". Report will print to console only.")
    }
  }

  write_line_output <- function(...) {
    line <- paste0(..., "\n")
    cat(line)
    if (file_opened && !is.null(file_con)) {
      cat(line, file = file_con)
    }
  }

  now_datetime <- Sys.time()
  hisea_date_header <- paste0(toupper(format(now_datetime, "%d-%b-%y")),
                              "       ", format(now_datetime, "%H:%M:%S"))

  write_line_output("PROGRAM HISEA (R Version)...EXECUTION DATE: ", hisea_date_header)
  if (!is.null(header) && nzchar(header)) {
    write_line_output(header)
  }
  write_line_output("FUNCTION OF THIS RUN IS.........", toupper(run_type))
  write_line_output("#STOCKS IN THE MODEL............", num_populations)
  if (num_populations > 0) {
    write_line_output("THE STOCKS ARE..................", paste(format(stocks_names_report, width = 12), collapse = ""))
  }
  if(!is.null(nv_used)) {
    write_line_output("#VARIABLES USED.................", nv_used)
  }
  if(!is.null(standard_sizes) && toupper(run_type) == "SIMULATION"){
    write_line_output("STANDARD BEING RESAMPLED?.......Y")
    write_line_output("RESAMPLED STANDARD SIZES........", paste(standard_sizes, collapse = " "))
    write_line_output("MIXTURE BEING SIMULATED?........Y")
  }
  if(!is.null(seed_val)){
    write_line_output("RANDOM NUMBER GENERATOR SEED.... ", seed_val)
  }
  write_line_output("NUMBER OF RUNS ANALYZED.......", nsamps)
  if(!is.null(nmix_simulated) && toupper(run_type) %in% c("SIMULATION", "ANALYSIS")){
    write_line_output("SIZE OF MIXTURE SAMPLE(S)...... ", nmix_simulated)
  }
  if (!is.null(actual_proportions) && num_populations > 0) {
    write_line_output("ACTUAL COMPOSITION IS........... ", paste(sprintf("% .3f", actual_proportions), collapse = " "))
  }
  write_line_output("=======================================================================")

  if (!is.null(means_vars) && !is.null(sds_vars) && nrow(means_vars) > 0 && ncol(means_vars) == num_populations) {
    write_line_output("                              MEAN AND STD DEV SUMMARY OF VARIABLES")
    header_stocks_vars <- paste(sapply(stocks_names_report, function(sn) sprintf("%14s", sn)), collapse = "")
    write_line_output("VAR          ", header_stocks_vars)
    for (i in 1:nrow(means_vars)) {
      write_line_output(sprintf("%-13d", i), paste(sprintf("%14.4f", means_vars[i, ]), collapse = ""))
      sd_line_parts <- sapply(sds_vars[i, ], function(sd_val) sprintf("(%5.2f)", sd_val))
      write_line_output("             ", paste(format(sd_line_parts, width = 14, justify = "right"), collapse=""))
    }
    write_line_output("=======================================================================")
    if(!is.null(standard_sizes)){
      write_line_output("THE SIZES OF THE STANDARDS ARE ", paste(standard_sizes, collapse = " "), "\n")
    }
  }

  if (num_populations > 0 && !is.null(mean_est) && ncol(mean_est) > 0 && length(estimator_labels_report) == ncol(mean_est)) {
    estimator_values_format_string <- paste0("%-14s", paste(rep("%-15.5f", length(estimator_labels_report)), collapse = ""))
    estimator_header_format_string <- paste0("%-14s", paste(rep("%-15s", length(estimator_labels_report)), collapse = ""))

    print_estimator_block_report <- function(title, matrix_vals_report) {
      write_line_output(title)
      write_line_output(do.call(sprintf, c(list(estimator_header_format_string),
                                           c(list("STOCK"), as.list(estimator_labels_report)))))
      for (k_pop in 1:num_populations) {
        write_line_output(do.call(sprintf, c(list(estimator_values_format_string),
                                             c(list(stocks_names_report[k_pop]), as.list(matrix_vals_report[k_pop, ])))))
      }
      write_line_output("")
    }

    write_line_output("=======================================================================")
    print_estimator_block_report(
      paste("TABLE OF COMPOSITION ESTIMATE MEANS.  NUMBER OF RUNS = ", sprintf("%4d", nsamps)),
      mean_est
    )
    write_line_output("=======================================================================")
    print_estimator_block_report(
      paste("TABLE OF COMPOSITION ESTIMATE STANDARD DEVIATIONS OVER THE ", sprintf("%4d", nsamps), "  RUNS"),
      sd_est
    )
    write_line_output("=======================================================================")

    if(!is.null(mse_est_sqrt) && !all(is.na(mse_est_sqrt))){
      print_estimator_block_report("TABLE OF SQRT OF MEAN SQUARED ERRORS", mse_est_sqrt)
      write_line_output("=======================================================================")
    }
  }

  if (!is.null(covar_ml_est) && !all(is.na(covar_ml_est)) && num_populations > 0) {
    write_line_output("THE COVARIANCE MATRIX OF THE  ", nsamps, " MAXIMUM LIKELIHOOD COMPOSITION ESTIMATES IS:")
    write_line_output("(POPULATIONS 1-", num_populations, " * 1-", num_populations, ")")
    row_labels_cov <- rownames(covar_ml_est)
    if (is.null(row_labels_cov) || length(row_labels_cov) != num_populations) row_labels_cov <- stocks_names_report
    for (i in 1:num_populations) {
      line <- sprintf("%-13s", row_labels_cov[i])
      for (j in 1:i) { line <- paste0(line, sprintf("%12.4e", covar_ml_est[i, j])) }
      write_line_output(line)
    }
    write_line_output("")
  }

  if (!is.null(cor_ml_est) && !all(is.na(cor_ml_est)) && num_populations > 0) {
    write_line_output("THE CORRESPONDING CORRELATION MATRIX IS:")
    write_line_output("(POPULATIONS 1-", num_populations, " * 1-", num_populations, ")")
    row_labels_cor <- rownames(cor_ml_est)
    if (is.null(row_labels_cor) || length(row_labels_cor) != num_populations) row_labels_cor <- stocks_names_report
    for (i in 1:num_populations) {
      line <- sprintf("%-13s", row_labels_cor[i])
      for (j in 1:i) { line <- paste0(line, sprintf("%12.5f", cor_ml_est[i, j])) }
      write_line_output(line)
    }
    write_line_output("")
  }

  if (!is.null(det_covar_ml_est) && !is.na(det_covar_ml_est)) {
    write_line_output(sprintf("DETERMINANT OF ML COVARIANCE MATRIX: %12.5e", det_covar_ml_est))
    write_line_output("")
  }

  write_line_output("FORTRAN STOP")
  write_line_output("R HISEA       job terminated at ", hisea_date_header)

  if (file_opened && !is.null(file_con)) {
    cat("HISEA formatted summary written to:", output_file, "\n")
  } else if (is.null(output_file) || !nzchar(output_file)) {
    cat("HISEA formatted summary printed to console.\n")
  } else {
    # This case means output_file was specified but couldn't be opened.
    # The original warning suppression for file opening should have caught this.
    cat(" HISEA formatted summary printed to console. File output to '", output_file, "' was not possible.\n")
  }
}
