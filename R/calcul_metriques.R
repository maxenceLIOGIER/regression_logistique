# source("R/predict_proba.R")

#' @title Function to Calculate Log-Likelihood
#' @description This function calculates the log-likelihood of the model
#'              based on the predicted probabilities for each observation.
#'
#' @param X Feature matrix (n x p),
#'          n = number of individuals and p = number of variables
#' @param y Label vector (n x 1)
#' @param theta Matrix of regression coefficients (p x K),
#'              p = number of features and K = number of classes.
#'
#' @return The log-likelihood of the model
calcul_log_likelihood <- function(X, y, theta) {
  proba <- predict_proba(X, theta)
  # calculating the probabilities on the training sample allows
  # to measure how well the model fits the data

  log_likelihood <- 0
  for (i in seq_len(nrow(X))) {
    classe <- y[i]
    index_classe <- which(levels(y) == classe)
    log_likelihood <- log_likelihood + log(proba[i, index_classe])
  }
  return(log_likelihood)
}


#' @title Function to Calculate AIC (Akaike Information Criterion)
#' @description This function calculates the Akaike Information Criterion (AIC).
#'              The AIC allows comparing several models with each other.
#'              It takes into account the quality of the fit and the number of parameters used.
#'
#' @param X Feature matrix
#' @param y Label vector (n x 1)
#' @param ll Log-likelihood of the model
#' @param theta Matrix of regression coefficients (p x K),
#'              p = number of features and K = number of classes.
#'
#' @return The AIC of the model
calcul_aic <- function(X, y, ll, theta) {
  k <- length(theta)
  aic <- 2*k - 2*ll
  return(aic)
}
