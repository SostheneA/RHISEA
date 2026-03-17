# Get Phi Matrix

Calculate the phi matrix for stock composition analysis.

## Usage

``` r
get_phi(predicted_class, true_labels, np)
```

## Arguments

- predicted_class:

  Integer vector of predictions

- true_labels:

  Factor of true labels

- np:

  Number of populations (levels)

## Value

Matrix of phi values

## Examples

``` r
# Simulated classification results
# 4 individuals: 2 from Stock 1, 2 from Stock 2
true_labels <- c(1, 1, 2, 2)
predicted_class <- c(1, 2, 2, 2) # One error: individual 2 misclassified

# Compute Phi matrix (2 stocks)
phi_matrix <- get_phi(
  predicted_class = predicted_class,
  true_labels = true_labels,
  np = 2
)

# The result shows the classification accuracy per stock
print(phi_matrix)
#>                  
#>                   True_Stock1 True_Stock2
#>   Assigned_Stock1         0.5         0.0
#>   Assigned_Stock2         0.5         1.0
```
