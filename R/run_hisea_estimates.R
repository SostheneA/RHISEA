#' Run HISEA Estimations Only
#'
#' @param pseudo_classes Vector of predicted classes (integers)
#' @param likelihoods Matrix of prediction probabilities
#' @param phi_matrix Confusion matrix (Phi)
#' @param np Number of populations
#' @param type "ANALYSIS", "SIMULATION" or "BOOTSTRAP"
#' @param nsamps Number of samples (default: 1000)
#' @param stocks_names Names of stocks/populations
#' @param export_csv Export results to CSV
#' @param output_dir Output directory
#' @param verbose Print progress messages
#'
#' @return List containing estimates and metrics
#' @export
run_hisea_estimates <- function(pseudo_classes,
                                likelihoods,
                                phi_matrix,
                                np,
                                type = "ANALYSIS",
                                nsamps = 1000,
                                stocks_names = NULL,
                                export_csv = FALSE,
                                output_dir = ".",
                                verbose = FALSE) {

  # Input validation
  type <- toupper(type)
  if (!type %in% c("ANALYSIS", "SIMULATION", "BOOTSTRAP")) {
    stop("type must be 'ANALYSIS', 'SIMULATION' or 'BOOTSTRAP'")
  }

  if (is.null(stocks_names)) {
    stocks_names <- paste0("Stock_", seq_len(np))
  }

  # Verify inputs
  if (length(pseudo_classes) != nrow(likelihoods)) {
    stop("Number of pseudo_classes must match number of rows in likelihoods")
  }

  if (ncol(likelihoods) != np) {
    stop("Number of columns in likelihoods must match np")
  }

  if (!all(dim(phi_matrix) == c(np, np))) {
    stop("phi_matrix must be a square matrix of dimension np x np")
  }

  # Calculate Phi inverse
  phi_det <- tryCatch({
    det(phi_matrix)
  }, error = function(e) {
    warning("Could not compute det(phi_matrix): ", e$message)
    0
  })

  if (abs(phi_det) < 1e-12) {
    inv_Phi <- MASS::ginv(phi_matrix)
  } else {
    inv_Phi <- solve(phi_matrix)
  }

  # Prepare results array
  est_names <- c("RAW", "COOK", "COOKC", "EM", "ML")
  all_res <- array(NA_real_,
                   dim = c(nsamps, np, length(est_names)),
                   dimnames = list(NULL, stocks_names, est_names))

  # Main loop
  if (verbose) message("Running ", nsamps, " x ", type)

  for (i in seq_len(nsamps)) {
    if (verbose && (i==1 || i==nsamps || i%%max(1,floor(nsamps/10))==0)) {
      message(" Iter ", i, "/", nsamps)
    }

    # Bootstrap sampling if needed
    if (type == "BOOTSTRAP") {
      idx <- sample(seq_len(length(pseudo_classes)), replace = TRUE)
      pc_iter <- pseudo_classes[idx]
      like_iter <- likelihoods[idx, , drop = FALSE]
    } else {
      pc_iter <- pseudo_classes
      like_iter <- likelihoods
    }

    # Calculate HISEA estimators
    raw   <- prop.table(tabulate(pc_iter, nbins = np))
    ck    <- compute_cook_estimators(pc_iter, inv_Phi, np)
    emv   <- estimate_millar(pc_iter, phi_matrix, np, verbose = FALSE)
    mlp   <- estimate_ml(like_iter, np, verbose = FALSE)

    # Store results
    all_res[i, , "RAW"]   <- raw
    all_res[i, , "COOK"]  <- ck$cook
    all_res[i, , "COOKC"] <- ck$cook_constrained
    all_res[i, , "EM"]    <- emv
    all_res[i, , "ML"]    <- mlp
  }

  # Create summary
  summary_l <- list(
    mean_estimates = apply(all_res, c(2,3), mean, na.rm = TRUE),
    sd_estimates = apply(all_res, c(2,3), sd, na.rm = TRUE),
    estimates = all_res
  )

  # Export results if requested
  if (export_csv) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    write.csv(as.data.frame(summary_l$mean_estimates),
              file.path(output_dir, paste0("mean_estimates_", timestamp, ".csv")),
              row.names = TRUE)
    write.csv(as.data.frame(summary_l$sd_estimates),
              file.path(output_dir, paste0("sd_estimates_", timestamp, ".csv")),
              row.names = TRUE)
  }

  if (verbose) message("run_hisea_estimates() done.")
  return(summary_l)
}
