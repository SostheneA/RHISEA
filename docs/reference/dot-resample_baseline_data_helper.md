# Helper: resample baseline data per stock

Helper: resample baseline data per stock

## Usage

``` r
.resample_baseline_data_helper(
  original_baseline_list,
  resampled_sizes,
  stock_names_for_error,
  nv_fallback
)
```

## Arguments

- original_baseline_list:

  List of baseline matrices.

- resampled_sizes:

  Integer vector of sizes for resampling.

- stock_names_for_error:

  Character vector of stock names for error messages.

- nv_fallback:

  Integer. Number of variables to use if a matrix is empty.

## Value

A list of resampled matrices.
