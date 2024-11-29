#' @title Gradient Descent Function for Logistic Regression
#' @description applies the gradient descent to optimize the `theta` parameters
#'              with the possibility to add L1, L2, or ElasticNet regularization
#'
#' @param X : feature matrix
#' @param y : label vector
#' @param theta : parameter vector
#' @param nb_iters : number of iterations to perform
#' @param alpha : learning rate
#' @param penalty : type of regularization: l1=lasso, l2=ridge, elasticnet
#' @param lambda : regularization parameter
#' @param l1_ratio : l1 regularization ratio
#'
#' @return list of optimized parameters
descente_gradient <- function(X, y, theta, nb_iters = 1000, alpha = 0.01,
                              penalty = NULL, lambda = 0, l1_ratio = 0) {
  # Check penalty
  if (!is.null(penalty) && !penalty %in% c("l1", "l2", "elasticnet")) {
    stop("Error: 'penalty' must be 'l1', 'l2', 'elasticnet' or NULL.")
  }

  # Check l1_ratio
  if (penalty == "elasticnet" && (l1_ratio < 0 || l1_ratio > 1)) {
    stop("Error: 'l1_ratio' must be between 0 and 1.")
  }
  # Check lambda
  if (lambda < 0) {
    stop("Error: 'lambda' must be positive.")
  }
  # Check alpha
  if (alpha <= 0) {
    stop("Error: 'alpha' must be positive.")
  }
  # Check nb_iters
  if (nb_iters <= 0) {
    stop("Error: 'nb_iters' must be positive.")
  }

  m <- nrow(X)

  # sigmoid function
  sigmoid <- function(z) {
    return(1 / (1 + exp(-z)))
  }

  for (i in 1:nb_iters) {
    h <- sigmoid(X %*% theta)
    gradient <- t(X) %*% (h - y) / m

    if (!is.null(penalty)) {
      if (penalty == "l2") { # Ridge
        gradient <- gradient + lambda * theta / m
        # obtained by deriving \lambda \sum_{j=1}^{p} \beta_j^2

      } else if (penalty == "l1") { # Lasso
        gradient <- gradient + lambda * sign(theta) / m
        # obtained by deriving \lambda \sum_{j=1}^{p} |\beta_j|

      } else if (penalty == "elasticnet") {
        ridge_component <- (1 - l1_ratio) * theta
        lasso_component <- l1_ratio * sign(theta)
        gradient <- gradient + lambda * (ridge_component + lasso_component) / m
      }
    }
    theta <- theta - alpha * gradient
  }

  return(list(theta = theta))
}