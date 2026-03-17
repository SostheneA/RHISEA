# Standard Expectation-Maximization (EM) Algorithm for Stock Proportions

Estimates mixing proportions (theta) from a likelihood matrix using the
EM algorithm.

## Usage

``` r
em_algorithm(
  likelihood,
  np,
  freq = NULL,
  max_iter = 100,
  tol = 1e-06,
  verbose = FALSE,
  save_theta_path = NULL
)
```

## Arguments

- likelihood:

  A numeric matrix (N_rows x N_populations) where \`likelihood\[i, j\]\`
  is \$P(Data_i \| Stock_j)\$ or a value proportional to it. N_rows can
  be N_samples (for Theta5) or N_populations (for Theta4).

- np:

  Integer, the number of populations (stocks). Should match
  \`ncol(likelihood)\`.

- freq:

  Optional numeric vector of frequencies/weights for each "observation"
  (row in \`likelihood\`). Length must match \`nrow(likelihood)\`.
  Defaults to \`rep(1, nrow(likelihood))\`.

- max_iter:

  Integer, the maximum number of iterations for the EM algorithm.

- tol:

  Numeric, the convergence tolerance. The algorithm stops if the maximum
  absolute change in any \`theta\` component is less than \`tol\`.

- verbose:

  Logical, if TRUE, prints iteration number and current theta estimates.

- save_theta_path:

  Optional character string. If provided, the history of theta estimates
  at each iteration is saved to a CSV file at this path.

## Value

A numeric vector of estimated stock proportions (theta), length \`np\`.

## Examples

``` r
# Dummy likelihood matrix (10 samples, 3 stocks)
lik <- matrix(runif(30), 10, 3)
lik <- lik / rowSums(lik) # Normalize rows

theta_em <- em_algorithm(lik, np = 3, max_iter = 20)
print(theta_em)
#> [1] 4.840631e-01 6.137913e-05 5.158755e-01
```
