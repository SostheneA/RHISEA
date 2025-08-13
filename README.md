# Rhisea

> R Package for Mixed-Stock Analysis Extending HISEA with Flexible and Robust Classification Frameworks

Rhisea modernizes and extends HISEA with flexible, robust classification frameworks to estimate source contributions and assign individuals to their stock of origin, with rigorous uncertainty quantification.

## Key features:

-   Drop-in compatibility with classic HISEA (FORTRAN) workflows, plus modern extensions
-   Multiple classification approaches (parametric and nonparametric), or use your own model and apply HISEA estimators
-   Estimation of stock proportions and individual assignment with uncertainty via bootstrap, cross-validation, and simulations
-   Built-in diagnostics for transparent interpretation Designed for fisheries biologists, ecologists, managers, and analysts seeking robust, reproducible inferences to support decision-making

## Table of contents

-   [Installation](#installation)
-   [Citation](#citation)

## Installation {#installation}

Rhisea can be installed from GitHub:

``` r
library(devtools)
install_github("SostheneA/Rhisea", dependencies = TRUE)
```

## Citation {#citation}

To cite Rhisea in publications use:

``` r
citation("Rhisea")
```
