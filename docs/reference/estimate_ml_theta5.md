# Estimate Stock Composition Using Direct Maximum Likelihood (Theta5)

Applies an EM algorithm to obtain the direct Maximum Likelihood Estimate
(MLE) of stock proportions (Theta_5 in HISEA). This uses the likelihood
of each individual mixed fishery sample fish belonging to each stock.
The likelihood maximized is:

## Usage

``` r
estimate_ml_theta5(
  individual_likelihoods,
  np,
  freq = NULL,
  use_accelerated_em = TRUE,
  ...
)
```

## Arguments

- individual_likelihoods:

  A numeric matrix (N_mixed_samples x NP) where
  \`individual_likelihoods\[i,j\]\` is \$P(Data_i \| Stock_j)\$, the
  likelihood of observing the data for mixed sample fish \`i\` given it
  came from stock \`j\`.

- np:

  Integer, the number of populations (stocks).

- freq:

  Optional numeric vector of frequencies/weights for each sample (row)
  in \`individual_likelihoods\`. Defaults to \`rep(1,
  nrow(individual_likelihoods))\`.

- use_accelerated_em:

  Logical, if TRUE, uses \`accel_em_algorithm\`, otherwise
  \`em_algorithm\`. Default TRUE.

- ...:

  Additional arguments passed to the chosen EM algorithm (e.g.,
  \`max_iter\`, \`tol\`, \`verbose\`, \`save_theta_path\`).

## Value

A numeric vector of estimated stock proportions (Theta5).

## Examples

``` r
# Direct ML estimation from individual likelihoods
lik <- matrix(runif(50), 25, 2)
lik <- lik / rowSums(lik)

theta5 <- estimate_ml_theta5(lik, np = 2)
print(theta5)
#> [1] 0.4768657 0.5231343
```
