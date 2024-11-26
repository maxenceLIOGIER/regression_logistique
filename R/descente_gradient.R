#' @title Fonction sigmoïde
#' @description Calcule la fonction sigmoïde de l'entrée `z`. La sigmoïde est une fonction qui transforme
#'              les valeurs d'entrée en un résultat compris entre 0 et 1.
#'
#' @param z Un vecteur ou une matrice de valeurs numériques.
#'
#' @return Un vecteur ou une matrice de même taille que `z`, où chaque élément correspond à la valeur de la fonction sigmoïde appliquée à l'élément correspondant de `z`.
#' @export
#' @examples
#' print("hi")
#' @export

# Fonction sigmoïde
sigmoid <- function(z) {
  #' @param z : vecteur ou matrice
  #' @return sigmoïde de z (valeur comprise entre 0 et 1)
  return(1 / (1 + exp(-z)))
}


#' @title Fonction de descente de gradient pour régression logistique
#' @description Cette fonction applique l'algorithme de descente de gradient pour optimiser les paramètres `theta`
#'              d'un modèle de régression logistique, avec possibilité d'ajouter une régularisation L1, L2 ou ElasticNet.
#'
#' @param X Matrice des caractéristiques (n x p), où n est le nombre d'individus et p est le nombre de caractéristiques.
#' @param y Vecteur des étiquettes (n x 1), où chaque élément correspond à la classe d'un individu.
#' @param theta Vecteur des paramètres du modèle (p x 1), où p est le nombre de caractéristiques.
#' @param nb_iters Nombre d'itérations de la descente de gradient (par défaut 1000).
#' @param alpha Taux d'apprentissage (par défaut 0.01).
#' @param penalite Type de régularisation : "l1" pour Lasso, "l2" pour Ridge, "elasticnet" pour ElasticNet, ou NULL pour aucune régularisation.
#' @param lambda Paramètre de régularisation (par défaut 0).
#' @param l1_ratio Ratio de régularisation L1 pour ElasticNet, doit être compris entre 0 et 1 (par défaut 0).
#'
#' @return Liste contenant les paramètres optimisés `theta`.
#'
#' @examples
#' print("hi")
#' @export

# Fonction de descente de gradient pour régression logistique
# avec régularisation l1, l2 ou elasticnet
descente_gradient <- function(X, y, theta, nb_iters = 1000, alpha = 0.01,
                              penalite = NULL, lambda = 0, l1_ratio = 0) {
  #' @param X : matrice des caractéristiques
  #' @param y : vecteur des étiquettes
  #' @param theta : vecteur des paramètres
  #' @param nb_iters : nombre d'itérations à effectuer
  #' @param alpha : taux d'apprentissage
  #' @param penalite : type de régularisation : l1=lasso, l2=ridge, elasticnet
  #' @param lambda : paramètre de régularisation
  #' @param l1_ratio : ratio de régularisation l1

  #' @return liste des paramètres optimisés

  # Vérification de penalite
  if (!is.null(penalite) && !penalite %in% c("l1", "l2", "elasticnet")) {
    stop("Erreur : 'penalite' doit être 'l1', 'l2', 'elasticnet' ou NULL.")
  }

  # Vérification de l1_ratio
  if (penalite == "elasticnet" && (l1_ratio < 0 || l1_ratio > 1)) {
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

    if (!is.null(penalite)) {
      if (penalite == "l2") { # Ridge
        gradient <- gradient + lambda * theta / m
        # obtenu en dérivant \lambda \sum_{j=1}^{p} \beta_j^2

      } else if (penalite == "l1") { # Lasso
        gradient <- gradient + lambda * sign(theta) / m
        # obtenu en dérivant \lambda \sum_{j=1}^{p} |\beta_j|

      } else if (penalite == "elasticnet") {
        ridge_component <- (1 - l1_ratio) * theta
        lasso_component <- l1_ratio * sign(theta)
        gradient <- gradient + lambda * (ridge_component + lasso_component) / m
      }
    }
    theta <- theta - alpha * gradient
  }

  return(list(theta = theta))
}
