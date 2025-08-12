# simulate.R — Functions for simulating fishery mixture samples and utility for ordered random numbers.

# --- Utility Function (previously in utils.R) ---

#' Generate Ordered Random Values (Order Statistics based)
#'
#' Produces an ordered vector of `num` random values, scaled to be approximately
#' between 0 and `upper`. This function can be used as a basis for certain types
#' of random sampling schemes. The logic is similar to generating order statistics
#' from an exponential distribution.
#'
#' @param num Integer, the number of ordered values to generate.
#' @param upper Numeric, the maximum value for the scaled output. The generated values
#'              will be in the approximate range (0, `upper`].
#'
#' @return A numeric vector of length `num`, containing ordered values scaled
#'         approximately between 0 and `upper`.
#' @export
#' @examples
#' ordvec(5, 100) # Generates 5 ordered numbers, max value around 100
#' ordvec(10, 10) # Generates 10 ordered numbers, max value around 10
ordvec <- function(num, upper) {
  # --- Input Validation ---
  if (!is.numeric(num) || length(num) != 1 || num <= 0 || floor(num) != num) {
    stop("Argument 'num' must be a single positive integer.")
  }
  if (!is.numeric(upper) || length(upper) != 1 || upper <= 0) {
    stop("Argument 'upper' must be a single positive number.")
  }

  # Generate 'num + 1' random variates from a standard exponential distribution
  # by taking -log(U) where U ~ Uniform(0,1).
  random_exponential_variates <- -log(stats::runif(num + 1))

  # Calculate cumulative sums to get order statistics.
  # These sums represent the arrival times in a Poisson process.
  ordered_cumulative_sums <- cumsum(random_exponential_variates)

  # Determine the scaling factor to ensure the largest value (the (num+1)-th sum)
  # would scale to 'upper'.
  scale_factor <- upper / ordered_cumulative_sums[num + 1]

  # Apply the scaling factor and return the first 'num' ordered, scaled values.
  scaled_ordered_values <- (ordered_cumulative_sums * scale_factor)[1:num]

  return(scaled_ordered_values)
}

# --- Mixture Simulation Function ---

#' Simulate a Mixed Fishery Sample from Known Stock Proportions
#'
#' Creates a synthetic fishery mixture by sampling individuals from different baseline
#' stocks according to specified true proportions. Sampling from each stock is done
#' with replacement.
#'
#' @param baseline_data_list A list of numeric matrices. Each matrix represents the
#'                     baseline (standard) data for one stock/population, with
#'                     observations as rows and variables as columns.
#' @param actual_proportions A numeric vector of true proportions for each stock in the mixture.
#'                     Must sum to 1 and have the same length as `baseline_data_list`.
#' @param N_mixture_size Integer, the total number of individuals to draw for the synthetic mixture.
#'
#' @return A numeric matrix representing the simulated mixture sample. Rows are
#'         individuals, and columns are variables. Returns an error if the mixture
#'         cannot be formed (e.g., due to empty baseline stocks needed for sampling).
#' @export
#' @examples
#' # Dummy baseline data
#' stock1 <- matrix(rnorm(20 * 2, mean = 0, sd = 1), ncol = 2) # Assuming 2 variables
#' stock2 <- matrix(rnorm(30 * 2, mean = 2, sd = 1), ncol = 2) # Assuming 2 variables
#' baseline_list_ex <- list(stock1, stock2)
#' true_props_ex <- c(0.6, 0.4) # 60% from stock1, 40% from stock2
#' mixture_N_ex <- 100
#' # simulated_sample_ex <- simulate_mixture(baseline_list_ex, true_props_ex, mixture_N_ex)
#' # print(head(simulated_sample_ex))
#' # print(nrow(simulated_sample_ex)) # Should be close to 100
simulate_mixture <- function(baseline_data_list, actual_proportions, N_mixture_size) {
  # --- Input Validation ---
  if (!is.list(baseline_data_list) || length(baseline_data_list) == 0) {
    stop("'baseline_data_list' must be a non-empty list of matrices.")
  }
  if (!all(sapply(baseline_data_list, function(x) is.matrix(x) && is.numeric(x)))) {
    stop("Each element in 'baseline_data_list' must be a numeric matrix.")
  }
  num_populations <- length(baseline_data_list)
  if (!is.numeric(actual_proportions) || length(actual_proportions) != num_populations) {
    stop("'actual_proportions' must be a numeric vector of the same length as 'baseline_data_list'.")
  }
  if (abs(sum(actual_proportions) - 1.0) > 1e-9) {
    warning("'actual_proportions' do not sum to 1. They will be normalized for sampling counts.", call. = FALSE)
    if (sum(actual_proportions) > 0) {
      actual_proportions <- actual_proportions / sum(actual_proportions)
    } else {
      stop("Sum of 'actual_proportions' is zero or negative, cannot proceed.")
    }
  }
  if (any(actual_proportions < 0)) {
    stop("'actual_proportions' must be non-negative.")
  }
  if (!is.numeric(N_mixture_size) || length(N_mixture_size) != 1 || N_mixture_size <= 0 || floor(N_mixture_size) != N_mixture_size) {
    stop("'N_mixture_size' must be a single positive integer.")
  }
  num_vars_check <- sapply(baseline_data_list, ncol)
  if(length(unique(num_vars_check[sapply(baseline_data_list, function(x) !is.null(x) && nrow(x) > 0)])) > 1){
    stop("All non-empty baseline stock matrices must have the same number of columns (variables).")
  }


  # List to store sampled rows from each stock
  mixture_components <- vector("list", num_populations)

  # Calculate the number of individuals to sample from each stock
  # Using multinomial sampling ensures that the sum of n_j_counts is exactly N_mixture_size
  # and respects proportions on average over many N_mixture_size selections.
  if (N_mixture_size > 0 && sum(actual_proportions) > 0) { # Ensure prob is not all zero
    n_j_counts <- as.vector(stats::rmultinom(1, size = N_mixture_size, prob = actual_proportions))
  } else {
    n_j_counts <- rep(0, num_populations)
  }


  # --- Sample from each stock ---
  for (j in seq_len(num_populations)) {
    num_to_sample_from_stock_j <- n_j_counts[j]

    current_stock_data <- baseline_data_list[[j]]
    # Default number of columns for empty matrices, taken from first non-empty stock if possible
    default_ncol <- unique(num_vars_check[sapply(baseline_data_list, function(x) !is.null(x) && nrow(x) > 0)])[1]
    if(is.na(default_ncol) && length(baseline_data_list)>0) default_ncol <- ncol(baseline_data_list[[1]]) # fallback if all empty

    # Skip if no samples are to be drawn from this stock
    if (num_to_sample_from_stock_j == 0) {
      # Store an empty matrix with correct number of columns if possible
      n_cols_for_empty <- if(!is.null(ncol(current_stock_data))) ncol(current_stock_data) else if(!is.na(default_ncol)) default_ncol else 0
      mixture_components[[j]] <- matrix(numeric(0), ncol = n_cols_for_empty, nrow = 0)
      next
    }

    num_available_in_stock_j <- if(is.null(current_stock_data)) 0 else nrow(current_stock_data)

    if (num_available_in_stock_j == 0) {
      # This condition means we need to sample from an empty stock. This is an error.
      stop(paste("Cannot sample", num_to_sample_from_stock_j, "individuals from stock", j,
                 "as it has no available data in 'baseline_data_list'."))
    }

    # Sample 'num_to_sample_from_stock_j' individuals from 'current_stock_data'
    # Sampling WITH REPLACEMENT is standard for this type of simulation.
    sampled_indices <- sample(seq_len(num_available_in_stock_j),
                              size = num_to_sample_from_stock_j,
                              replace = TRUE)
    mixture_components[[j]] <- current_stock_data[sampled_indices, , drop = FALSE]
  }

  # Combine the sampled individuals from all stocks into a single mixture matrix
  # Filter out NULLs or zero-row matrices before rbind to prevent errors
  valid_components <- Filter(function(x) !is.null(x) && nrow(x) > 0, mixture_components)

  if (length(valid_components) > 0) {
    final_mixture_sample <- do.call(rbind, valid_components)
  } else if (N_mixture_size > 0) {
    # This case implies N_mixture_size > 0 but no samples were actually drawn (e.g., all n_j_counts became 0).
    stop("Simulate_mixture resulted in an empty mixture despite N_mixture_size > 0. Check actual_proportions and baseline data availability.")
  } else { # N_mixture_size was 0
    # Determine number of columns from first available baseline or default to 0
    n_cols_final <- if(!is.null(default_ncol) && !is.na(default_ncol)) default_ncol else 0
    final_mixture_sample <- matrix(numeric(0), ncol = n_cols_final, nrow = 0)
  }

  # Shuffle the rows of the final mixture sample so that individuals
  # from different stocks are intermingled, not grouped by original stock.
  if (nrow(final_mixture_sample) > 1) {
    final_mixture_sample <- final_mixture_sample[sample(nrow(final_mixture_sample)), , drop = FALSE]
  }

  return(final_mixture_sample)
}
