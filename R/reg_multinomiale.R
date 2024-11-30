#' @title Multinomial logistic regression
#' @description Performs multinomial logistic regression to find optimal parameters.
#'
#' @param X (data.frame) Predictor variables (features) for the model.
#' @param y (vector) Target variable (labels) for training the model.
#' @param nb_iters (integer) Number of iterations for the gradient descent.
#' @param alpha (numeric) Learning rate for the gradient descent.
#' @param penalty (character) Type of regularization: l1=lasso, l2=ridge, elasticnet.
#' @param lambda (numeric) Regularization parameter.
#' @param l1_ratio (numeric) The ElasticNet mixing parameter, with 0 <= l1_ratio <= 1. Only used if penalty is 'elasticnet'.
#' @return (matrix) Optimized parameters for each class.
reg_multinomiale <- function(X, y, nb_iters, alpha,
                             penalty, lambda, l1_ratio) {
  X_new <- as.matrix(prepare_x(X))
  classes_uniques <- unique(y)
  K <- length(classes_uniques)
  n <- ncol(X_new)
  theta <- matrix(0, nrow = n, ncol = K)
  colnames(theta) <- classes_uniques
  rownames(theta) <- colnames(X_new)

  for (k in 1:K) {
    y_k <- ifelse(y == classes_uniques[k], 1, 0)
    result <- descente_gradient(X_new, y_k, theta[, k, drop = FALSE],
                                nb_iters = nb_iters,
                                alpha = alpha,
                                penalty = penalty,
                                lambda = lambda,
                                l1_ratio = l1_ratio)
    theta[, k] <- result$theta
  }

  return(theta)
}