# io.R — Functions for input/output operations, specifically for reading HISEA-formatted data files.

#' Read HISEA Baseline File
#'
#' Reads a HISEA-formatted baseline (standard) data file. The file is expected to
#' contain data for multiple stocks/populations, with data for each stock
#' separated by a delimiter string (e.g., "NEXT STOCK").
#' Blank lines and lines starting with '#' (comments) are ignored.
#'
#' @param filepath Character string, the path to the baseline `.std` file.
#' @param nv Integer, the number of variables (columns) expected for each observation
#'           in the baseline data.
#'
#' @return A list of numeric matrices. Each matrix in the list corresponds to a
#'         stock/population, with rows being observations and columns being variables.
#'         Returns an empty list if no valid data is found or errors occur during parsing.
#' @export
read_baseline <- function(filepath, nv) {
  # --- Input Validation ---
  if (!is.character(filepath) || length(filepath) != 1) {
    stop("Argument 'filepath' must be a single character string.")
  }
  if (!file.exists(filepath)) {
    stop(paste("File not found:", filepath))
  }
  if (!is.numeric(nv) || length(nv) != 1 || nv <= 0 || floor(nv) != nv) {
    stop("Argument 'nv' must be a single positive integer (number of variables).")
  }

  # Read all lines from the file
  raw_lines <- tryCatch({
    readLines(filepath, warn = FALSE)
  }, error = function(e) {
    stop(paste("Error reading file:", filepath, "-", conditionMessage(e)))
  })

  # --- Pre-processing Lines ---
  # 1. Remove leading/trailing whitespace
  processed_lines <- trimws(raw_lines)
  # 2. Remove empty lines
  processed_lines <- processed_lines[processed_lines != ""]
  # 3. Remove comment lines (starting with '#')
  processed_lines <- processed_lines[!grepl("^\\s*#", processed_lines)]
  # 4. Remove specific HISEA end-of-file markers (case-insensitive)
  #    These markers might appear at the very end of the data for a stock or the file.
  end_markers <- c("end of baseline data", "end of file", "end of stock data") # Added common variants
  processed_lines <- processed_lines[!tolower(processed_lines) %in% end_markers]

  # --- Identify Stock Blocks ---
  # Delimiter pattern for "NEXT STOCK" (case-insensitive, allowing surrounding whitespace)
  delimiter_pattern <- "^\\s*next\\s+stock\\s*$"
  split_indices <- grep(delimiter_pattern, processed_lines, ignore.case = TRUE)

  # Define data blocks based on delimiter positions
  # Add implicit split points at the beginning and end of the processed lines
  all_split_points <- c(0, split_indices, length(processed_lines) + 1)

  data_list <- list() # Initialize list to store matrices for each stock

  # --- Process Each Stock Block ---
  for (i in 1:(length(all_split_points) - 1)) {
    start_line_idx <- all_split_points[i] + 1
    end_line_idx <- all_split_points[i+1] - 1

    # Skip if the block is effectively empty (e.g., consecutive delimiters or delimiter at EOF)
    if (start_line_idx > end_line_idx) {
      next
    }

    stock_lines_block <- processed_lines[start_line_idx:end_line_idx]

    # Ensure no delimiter lines are accidentally included within a data block
    # (e.g., if a delimiter was not removed by the global split index logic due to file structure)
    stock_data_lines <- stock_lines_block[!grepl(delimiter_pattern, stock_lines_block, ignore.case = TRUE)]

    if (length(stock_data_lines) == 0) {
      # This means the block between delimiters (or start/end and a delimiter) contained no valid data lines.
      next
    }

    # Attempt to scan numeric data from the lines of the current stock block
    scanned_values <- tryCatch({
      # Concatenate lines into a single string for scan, helps handle numbers split across lines if any (though not typical for HISEA)
      # However, HISEA usually has one observation per line.
      scan(text = stock_data_lines, what = numeric(), quiet = TRUE, comment.char="#", multi.line = TRUE)
    }, warning = function(w){
      warning(paste("Warning while scanning numeric data for a stock block (lines approx.", start_line_idx, "to", end_line_idx,
                    "in processed file contents):", conditionMessage(w)), call. = FALSE)
      numeric(0) # Return empty numeric on warning (e.g., non-numeric data found)
    }, error = function(e) {
      warning(paste("Error scanning numeric data for a stock block (lines approx.", start_line_idx, "to", end_line_idx,
                    "in processed file contents):", conditionMessage(e)), call. = FALSE)
      numeric(0) # Return empty numeric on error
    })

    # Validate scanned values for the current stock
    if (length(scanned_values) == 0) {
      warning(paste("No numeric values were successfully scanned for a stock block (lines approx.", start_line_idx, "to", end_line_idx,
                    "). Skipping this stock block."), call. = FALSE)
      next
    }
    if (length(scanned_values) %% nv != 0) {
      warning(paste("Number of values read for a stock (", length(scanned_values),
                    ") is not a multiple of the number of variables 'nv' (", nv,
                    "). Lines approx.", start_line_idx, "to", end_line_idx, ". Skipping this stock block."), call. = FALSE)
      next
    }

    # Convert the scanned values into a matrix with 'nv' columns
    stock_matrix <- matrix(scanned_values, ncol = nv, byrow = TRUE)
    data_list[[length(data_list) + 1]] <- stock_matrix
  } # End loop over stock blocks

  if(length(data_list) == 0) {
    warning("No valid stock data successfully read from the baseline file: ", filepath,
            ". Check file format, delimiters, and 'nv'.", call. = FALSE)
  }

  return(data_list)
}


#' Read HISEA Mixture File
#'
#' Reads a HISEA-formatted mixture data file. It attempts to extract lines
#' containing only numeric data (typical for HISEA mixture files) and
#' converts them into a numeric matrix.
#'
#' @param filepath Character string, the path to the mixture `.mix` file.
#' @param nv Integer, the number of variables (columns) expected for each observation
#'           in the mixture data.
#'
#' @return A numeric matrix where rows are observations and columns are variables.
#'         Returns an empty matrix (with `nv` columns) if no valid data is found or errors occur.
#' @export
read_mixture <- function(filepath = "hisea.mix", nv) {
  # --- Input Validation ---
  if (!is.character(filepath) || length(filepath) != 1) {
    stop("Argument 'filepath' must be a single character string.")
  }
  if (!file.exists(filepath)) {
    stop(paste("File not found:", filepath))
  }
  if (!is.numeric(nv) || length(nv) != 1 || nv <= 0 || floor(nv) != nv) {
    stop("Argument 'nv' must be a single positive integer (number of variables).")
  }

  # Read all lines from the file
  all_lines <- tryCatch({
    readLines(filepath, warn = FALSE)
  }, error = function(e) {
    stop(paste("Error reading file:", filepath, "-", conditionMessage(e)))
  })

  # Pre-processing: Remove comments and trim whitespace
  processed_lines <- trimws(all_lines)
  processed_lines <- processed_lines[!grepl("^\\s*#", processed_lines)] # Remove comment lines
  processed_lines <- processed_lines[processed_lines != ""]              # Remove empty lines

  # Filter for lines that appear to contain only space-separated numbers.
  # This regex is more robust for numbers including decimals, scientific notation, and negative signs.
  # It expects numbers to be separated by one or more spaces.
  numeric_pattern <- "^\\s*(-?\\d*\\.?\\d+([eE][-+]?\\d+)?(\\s+(-?\\d*\\.?\\d+([eE][-+]?\\d+)?))*)\\s*$"
  data_lines <- processed_lines[grepl(numeric_pattern, processed_lines)]

  if (length(data_lines) == 0) {
    warning(paste("No lines with expected numeric data format found in mixture file:", filepath), call. = FALSE)
    return(matrix(numeric(0), ncol = nv, nrow = 0)) # Return empty matrix with correct column count
  }

  # Scan the numeric values from the filtered data lines
  scanned_values <- tryCatch({
    scan(text = data_lines, what = numeric(), quiet = TRUE, comment.char="#", multi.line = TRUE)
  }, warning = function(w){
    warning(paste("Warning while scanning numeric data in mixture file", filepath, ":", conditionMessage(w)), call. = FALSE)
    numeric(0) # Return empty numeric on warning
  }, error = function(e) {
    warning(paste("Error scanning numeric data in mixture file", filepath, ":", conditionMessage(e)), call. = FALSE)
    numeric(0) # Return empty numeric on error
  })

  if (length(scanned_values) == 0) {
    warning(paste("No numeric values were successfully scanned from mixture file:", filepath), call. = FALSE)
    return(matrix(numeric(0), ncol = nv, nrow = 0))
  }

  # Check if the number of scanned values is a multiple of 'nv'
  if (length(scanned_values) %% nv != 0) {
    warning(paste("Total number of numeric values scanned in mixture file (", length(scanned_values),
                  ") is not a multiple of the number of variables 'nv' (", nv,
                  "). File may be corrupt or 'nv' incorrect. Returning empty matrix."), call. = FALSE)
    return(matrix(numeric(0), ncol = nv, nrow = 0))
  }

  # Convert the scanned values into a matrix
  mixture_matrix <- matrix(scanned_values, ncol = nv, byrow = TRUE)

  return(mixture_matrix)
}
