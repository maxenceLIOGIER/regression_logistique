#' @title Multinomial logistic regression
#' @description Performs multinomial logistic regression to find optimal parameters.
#'
#' @param X (data.frame) Predictor variables (features) for the model.
#' @param y (vector) Target variable (labels) for training the model.
#' @return (matrix) Optimized parameters for each class.
#' @method LogisticRegression multinomial_logistic_regression
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