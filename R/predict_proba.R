# source("R/prepare_x.R")

#' @title Predicts class membership probabilities
#' @description Predicts the probabilities of individuals belonging to classes.
#'              Uses the scores obtained by multiplying X with theta,
#'              then applies the softmax function to obtain the probabilities
#'
#' @param X matrix or data.frame of individuals' features
#'          row = an individual, column = a feature
#' @param theta matrix of learned coefficients
#'              column = a class, row = a feature
#'
#' @return matrix of class membership probabilities for each individual
#'         each row corresponds to an individual and each column to a class
predict_proba <- function(X, theta) {
  if (is.null(theta)) {
    stop("The model is not yet trained")
  }

  # Prepare the matrix X
  X_new <- as.matrix(prepare_x(X))

  # Calculate the scores for each class
  scores <- X_new %*% theta

  # Apply the softmax function to obtain the probabilities
  softmax <- function(x) {
    exp(x) / rowSums(exp(x))
  }
  proba <- softmax(scores)

  return(proba)
}