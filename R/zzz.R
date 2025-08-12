#' @importFrom utils install.packages
#' @keywords internal
.required_pkgs <- function() {
  # Vérifier et configurer le miroir CRAN correctement
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  pkgs <- c("caret", "MASS", "nnet", "e1071", "randomForest",
            "xgboost", "mclust", "class",  "kknn")

  missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs)
  }

  invisible(sapply(pkgs, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Configuration du miroir CRAN au chargement
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  tryCatch({
    .required_pkgs()
    packageStartupMessage("Rhisea loaded successfully")
  }, error = function(e) {
    warning("Error loading required packages: ", e$message)
  })
}
