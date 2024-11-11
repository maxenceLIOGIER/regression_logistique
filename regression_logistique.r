# Fonction sigmoïde
sigmoide <- function(z) {
  #' @param z : vecteur ou matrice
  #' @return sigmoïde de z (valeur comprise entre 0 et 1)
  return(1 / (1 + exp(-z)))
}

# Fonction de coût
cost_function <- function(X, y, theta) {
  #' @param X : matrice des caractéristiques
  #' @param y : vecteur des étiquettes
  #' @param theta : vecteur des paramètres
  #' @return coût de la régression logistique
  m <- length(y)
  h <- sigmoide(X %*% theta) # %*% = produit matriciel
  cost <- (-1 / m) * sum(y * log(h) + (1 - y) * log(1 - h))
  return(cost)
}

# Fonction de descente de gradient
gradient_descent <- function(X, y, theta, alpha, num_iters) {
  #' @param X : matrice des caractéristiques
  #' @param y : vecteur des étiquettes
  #' @param theta : vecteur des paramètres
  #' @param alpha : taux d'apprentissage
  #' @param num_iters : nombre d'itérations
  #' @return liste contenant les paramètres optimisés et l'historique des coûts
  m <- length(y)
  cost_history <- numeric(num_iters)
  
  for (i in 1:num_iters) {
    h <- sigmoide(X %*% theta)
    gradient <- (1 / m) * (t(X) %*% (h - y))
    theta <- theta - alpha * gradient
    cost_history[i] <- cost_function(X, y, theta)
  }
  
  return(list(theta = theta, cost_history = cost_history))
}

# Régression logistique multinomiale
multinomial_logistic_regression <- function(X, y, alpha = 0.01, num_iters = 1000) {
  #' @param X Matrice des caractéristiques
  #' @param y Vecteur des étiquettes
  #' @param alpha Taux d'apprentissage
  #' @param num_iters Nombre d'itérations
  #' @return Liste contenant les paramètres optimisés et l'historique des coûts
  K <- length(unique(y))
  n <- ncol(X)
  theta <- matrix(0, nrow = n, ncol = K)
  
  for (k in 1:K) {
    y_k <- ifelse(y == k, 1, 0)
    result <- gradient_descent(X, y_k, theta[, k], alpha, num_iters)
    theta[, k] <- result$theta
  }
  
  return(theta)
}

# Exemple d'utilisation
set.seed(123)
X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(0, 0, 1, 1, 1)
theta <- matrix(c(0, 0), nrow = 2)
alpha <- 0.01
num_iters <- 1000

result <- gradient_descent(X, y, theta, alpha, num_iters)
print(result$theta)
plot(result$cost_history, type = "l", main = "Convergence du coût", xlab = "Itérations", ylab = "Coût")