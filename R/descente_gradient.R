# Fonction sigmoïde
sigmoid <- function(z) {
  #' @param z : vecteur ou matrice
  #' @return sigmoïde de z (valeur comprise entre 0 et 1)
  return(1 / (1 + exp(-z)))
}

# Fonction de descente de gradient
descente_gradient <- function(X, y, theta, nb_iters = 1000, alpha = 0.01) {
  #' @param X : matrice des caractéristiques
  #' @param y : vecteur des étiquettes
  #' @param theta : vecteur des paramètres
  #' @param nb_iters : nombre d'itérations à effectuer
  #' @param alpha : taux d'apprentissage
  #' @return liste des paramètres optimisés
  #' @description implémente l'algorithme de descente de gradient
  m <- nrow(X)
  for (i in 1:nb_iters) {
    h <- sigmoid(X %*% theta)
    gradient <- t(X) %*% (h - y) / m
    theta <- theta - alpha * gradient
  }
  return(list(theta = theta))
}