# Compute Cook Estimators for Stock Proportions

Computes three types of estimators based on classification results: 1.
Raw proportions: Simple proportion of samples classified to each stock.
2. Cook's corrected estimator: Raw proportions corrected by the inverse
of the misclassification matrix (Phi_inv). This can result in negative
estimates. 3. Cook's constrained estimator: An iterative adjustment of
Cook's corrected estimator to ensure proportions are non-negative and
sum to 1.

## Usage

``` r
compute_cook_estimators(class_predictions, PHIinv, np)
```

## Arguments

- class_predictions:

  A numeric vector of predicted class (stock) labels for the mixed
  sample. Values should be integers from 1 to \`np\`.

- PHIinv:

  The inverse of the misclassification matrix (Phi). Dimensions should
  be np x np.

- np:

  Integer, the number of populations (stocks).

## Value

A list containing three numeric vectors:

- raw:

  Raw proportions of classification.

- cook:

  Cook's corrected estimates.

- cook_constrained:

  Cook's constrained estimates (non-negative, sum to 1).

## Examples

``` r
# 2 stocks, 10 samples classified
preds <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1) # Mostly stock 1
# Dummy Phi matrix (80% accuracy)
phi <- matrix(c(0.8, 0.2, 0.2, 0.8), 2, 2)
phi_inv <- solve(phi)

cooks <- compute_cook_estimators(preds, phi_inv, np = 2)
print(cooks$cook_constrained)
#> [1] 0.8333333 0.1666667
```
