source("R/predict_proba.R")

# Fonction pour calculer la log-vraisemblance
calcul_log_likelihood <- function(X, y, theta) {
  #' @param X : matrice des caractéristiques
  #' @param y : vecteur des étiquettes
  #' @description calculer la log-vraisemblance du modèle

  probabilities <- predict_proba(X, theta)
  # calculer les proba sur l'échantillon d'apprentissage permet
  # de mesurer à quel point le modèle s'ajuste aux données

  log_likelihood <- 0
  for (i in seq_len(nrow(X))) {
    log_likelihood <- log_likelihood + log(probabilities[i, y[i] + 1])
    # hypothèse : les classes sont numérotées de 0 à K-1
  }
  return(log_likelihood)
}

# Fonction pour calculer l'AIC (Akaike Information Criterion)
calcul_aic <- function(X, y, ll, theta) {
  #' @param ll : log-likelyhood du modèle
  #' @param theta : paramètres du modèle
  #' @description calculer l'AIC du modèle
  #' l'AIC sert à comparer des modèles, en pénalisant le nb de paramètres

  k <- length(theta) # nombre de paramètres
  aic <- 2*k - 2*ll
  return(aic)
}

# Fonction pour calculer le pseudo R² de McFadden
calcul_pseudo_r2 <- function(X, y, ll) {
  #' @param ll : log-likelihood du modèle
  #' @description calculer le pseudo R² de McFadden
  #' @return valeur du pseudo R²

  # Calcul de la log-vraisemblance du modèle nul pour multiclasse
  classes <- unique(y)
  null_ll <- 0
  for (class in classes) {
    p_class <- mean(y == class)
    null_ll <- null_ll + sum((y == class) * log(p_class))
  }

  null_deviance <- -2 * null_ll
  residual_deviance <- -2 * ll
  pseudo_r2 <- 1 - (residual_deviance / null_deviance)

  return(pseudo_r2)
}