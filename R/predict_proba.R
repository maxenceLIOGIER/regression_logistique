#' @title Prédire les probabilités d'appartenance aux classes
#' @description Prédire les probabilités d'appartenance des individus aux classes.
#' Cette fonction utilise les scores obtenus par la multiplication de la matrice
#' des caractéristiques X avec les coefficients theta, puis applique la fonction softmax
#' pour obtenir les probabilités d'appartenance aux différentes classes.
#'
#' @param X matrice ou data.frame des caractéristiques des individus
#'          ligne = un individu, colonne = une caractéristique
#' @param theta matrice des coefficients appris
#'              colonne = une classe, ligne = une caractéristique
#'
#' @return matrice des probabilités d'appartenance aux classes pour chaque individu
#'         chaque ligne correspond à un individu et chaque colonne à une classe
predict_proba <- function(X, theta) {
  if (is.null(theta)) {
    stop("Le modèle n'est pas encore entraîné")
  }

  # Préparation de la matrice X
  X_new <- as.matrix(prepare_x(X))

  # Calculer les scores pour chaque classe
  scores <- X_new %*% theta

  # Appliquer la fonction softmax pour obtenir les probabilités
  softmax <- function(x) {
    exp(x) / rowSums(exp(x))
  }
  proba <- softmax(scores)

  return(proba)
}