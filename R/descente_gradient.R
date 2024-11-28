#' @title Fonction sigmoïde
#' @description Cette fonction calcule la sigmoïde d'un vecteur ou d'une matrice.
#' @param z : vecteur ou matrice
#' @return sigmoïde de z (valeur comprise entre 0 et 1)
sigmoid <- function(z) {
  return(1 / (1 + exp(-z)))
}


#' @title Fonction de descente de gradient pour régression logistique
#' @description Cette fonction applique l'algorithme de descente de gradient pour optimiser les paramètres `theta`
#'              d'un modèle de régression logistique, avec possibilité d'ajouter une régularisation L1, L2 ou ElasticNet.
#'
#' @param X : matrice des caractéristiques
#' @param y : vecteur des étiquettes
#' @param theta : vecteur des paramètres
#' @param nb_iters : nombre d'itérations à effectuer
#' @param alpha : taux d'apprentissage
#' @param penalty : type de régularisation : l1=lasso, l2=ridge, elasticnet
#' @param lambda : paramètre de régularisation
#' @param l1_ratio : ratio de régularisation l1
#'
#' @return liste des paramètres optimisés
descente_gradient <- function(X, y, theta, nb_iters = 1000, alpha = 0.01,
                              penalty = NULL, lambda = 0, l1_ratio = 0) {
  # Vérification de penalty
  if (!is.null(penalty) && !penalty %in% c("l1", "l2", "elasticnet")) {
    stop("Erreur : 'penalty' doit être 'l1', 'l2', 'elasticnet' ou NULL.")
  }

  # Vérification de l1_ratio
  if (penalty == "elasticnet" && (l1_ratio < 0 || l1_ratio > 1)) {
    stop("Erreur : 'l1_ratio' doit être compris entre 0 et 1.")
  }
  # Vérification de lambda
  if (lambda < 0) {
    stop("Erreur : 'lambda' doit être positif.")
  }
  # Vérification de alpha
  if (alpha <= 0) {
    stop("Erreur : 'alpha' doit être positif.")
  }
  # Vérification de nb_iters
  if (nb_iters <= 0) {
    stop("Erreur : 'nb_iters' doit être positif.")
  }

  m <- nrow(X)

  for (i in 1:nb_iters) {
    h <- sigmoid(X %*% theta)
    gradient <- t(X) %*% (h - y) / m

    if (!is.null(penalty)) {
      if (penalty == "l2") { # Ridge
        gradient <- gradient + lambda * theta / m
        # obtenu en dérivant \lambda \sum_{j=1}^{p} \beta_j^2

      } else if (penalty == "l1") { # Lasso
        gradient <- gradient + lambda * sign(theta) / m
        # obtenu en dérivant \lambda \sum_{j=1}^{p} |\beta_j|

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