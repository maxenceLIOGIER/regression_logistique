source("R/prepare_x.R")

predict_proba <- function(X, theta) {
  #' @description prédire probas d'appartenance des individus aux classes

  if (is.null(theta)) {
    stop("Le modèle n'est pas encore entraîné")
  }

  # Préparation de la matrice X
  X_new <- prepare_x(X)

  # Calculer les scores pour chaque classe
  scores <- X_new %*% theta

  # Appliquer la fonction softmax pour obtenir les probabilités
  softmax <- function(x) {
    exp(x) / rowSums(exp(x))
  }
  proba <- softmax(scores)

  return(proba)
}