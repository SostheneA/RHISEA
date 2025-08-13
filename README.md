# Rhisea

> ***"R Package for Mixed-Stock Analysis Extending HISEA with Flexible and Robust Classification Frameworks"***

Rhisea modernizes and extends HISEA with flexible, robust classification frameworks to estimate source contributions and assign individuals to their stock of origin, with rigorous uncertainty quantification.

## Key features:

-   Drop-in compatibility with classic HISEA (FORTRAN) workflows, plus modern extensions
-   Multiple classification approaches (parametric and nonparametric), or use your own model and apply HISEA estimators
-   Estimation of stock proportions and individual assignment with uncertainty via bootstrap, cross-validation, and simulations
-   Built-in diagnostics for transparent interpretation Designed for fisheries biologists, ecologists, managers, and analysts seeking robust, reproducible inferences to support decision-making

## Table of contents

-   [Installation](#installation)
-   [Citation](#citation)
-   [From HISEA FORTRAN to Rhisea: Modern Mixed-Stock Analysis in R](#from-hisea-fortran-to-rhisea-modern-mixed-stock-analysis-in-r)
    -   [However, the classical HISEA had important limitations:](#however-the-classical-hisea-had-important-limitations)
    -   [The Modern Workflow: Rhisea in R](#the-modern-workflow-rhisea-in-r)
        -   [Key Innovations](#key-innovations)
            -   [1- Flexible Classifier Choice](#1--flexible-classifier-choice)
            -   [2- Modern Data Handling](#2--modern-data-handling)
            -   [3- Expanded Outputs & Reproducibility](#3--expanded-outputs--reproducibility)
            -   [4- Intuitive, Modular Workflow](#4--intuitive-modular-workflow)
        -   [Step-by-Step Overview (see diagram)](#step-by-step-overview-see-diagram)
-   [Conclusion: The Rhisea Advantage](#conclusion-the-rhisea-advantage)

## Installation

Rhisea can be installed from GitHub:

``` r
library(devtools)
install_github("SostheneA/Rhisea", dependencies = TRUE)
```

## Citation

To cite Rhisea in publications use:

``` r
citation("Rhisea")
```

# From HISEA FORTRAN to Rhisea: Modern Mixed-Stock Analysis in R

Mixed-stock analysis is a fundamental tool for fisheries science, allowing estimation of the contributions of source populations to mixture samples (e.g., fisheries catches). The original HISEA program, implemented in FORTRAN, pioneered this approach with a robust statistical framework and introduced several estimators (Θ₁ to Θ₅) for stock proportion inference.

### However, the classical HISEA had important limitations:

-   Only a single built-in classifier (LDA/discriminant analysis), requiring the variables to be approximately multivariate normal with constant covariance matrices.
-   Limited flexibility for integrating modern statistical learning approaches.
-   Rigid input/output based on text files (*.std,* .mix, \*.ctl), and no option to save intermediate or diagnostic results for downstream analysis.

## The Modern Workflow: Rhisea in R

Rhisea builds on the foundation of HISEA and brings mixed-stock analysis into the present:

### Key Innovations

#### 1- Flexible Classifier Choice

With Rhisea, you are not limited to LDA. Built-in support now exists for a wide range of modern supervised learning classifiers:

-   Linear/Quadratic Discriminant Analysis (LDA, QDA, MASS)

-   Mixture models (MCLUST)

-   Decision Trees (CTREE, CART)

-   Linear/Multinomial Regression (MLR)

-   k-Nearest Neighbors (KNN, kknn)

-   Support Vector Machines (SVM)

-   Random Forests (RF, randomForest)

-   Extreme Gradient Boosting (XGB)

-   Artificial Neural Networks (ANN, nnet)

-   Naive Bayes (NB, e1071)

    ...and you can even supply your own trained model and prediction function.

#### 2- Modern Data Handling

You can work directly with R data frames---no need for fixed-format text files. However, full compatibility is retained: you may still use HISEA-formatted files for reproducibility or validation against legacy outputs.

#### 3- Expanded Outputs & Reproducibility

-   Rhisea logs not only the final report but can save all intermediate objects (classification probabilities, confusion matrices, etc.) as .rda files for downstream diagnostics and reproducibility.
-   Results are summarized with confidence intervals via bootstrap, and automated visualizations are provided.
-   All run parameters, models, and derived files can be archived for full transparency and reproducibility.

#### 4- Intuitive, Modular Workflow

Each step of the process is transparent: from baseline/mixture data ingestion → classifier choice and fitting → estimation with all classical HISEA estimators (Θ₁: Raw, Θ₂: Cook & Lord, Θ₃: Cook Constrained, Θ₄: EM MillarC, Θ₅: EM ML) → summary and report. You control every aspect or automate it all.

## Step-by-Step Overview (see diagram)

1.  Start: Specify your inputs---choose between R data frames or HISEA .std/.mix control files.
2.  Classifier Selection: Easily select or define your choice of classification model for stock assignment.
3.  Model Training & Validation: Fit the classifier to the baseline data, estimate the classification matrix (Φ), and validate model accuracy/confusion.
4.  Application to Mixture: Predict class probabilities for the mixture, obtain individual assignments and posterior probabilities.
5.  Estimation: Apply the suite of HISEA estimators to convert classification outputs to stock proportion estimates (Θ₁--Θ₅).
6.  Summary & Reporting: Save a comprehensive summary report, with options to log all intermediate files (.rda).
7.  End: Ensure results are reproducible, documented, and ready for interpretation or downstream analysis.

![](images/HISEA%20_%20Mermaid.png)

## Conclusion: The Rhisea Advantage 

With Rhisea, you retain all the statistical rigor of original HISEA - including compatibility and reproducibility with legacy workflows - while gaining the flexibility, transparency, and analytic power of modern R. Advanced users can plug in custom classifiers, automate cross-validation, and leverage the full R ecosystem for further statistical or graphical analysis. All intermediate steps are traceable and archivable for robust, audit-friendly science.

> ***"Reference: Millar, R.B. (1990),"A Versatile Computer Program for Mixed Stock Fishery Composition Estimation", Can. Tech. Rep. Fish. Aquat. Sci. 1753: iii + 29 p."***
