# source("R/predict_proba.R")

#' @title Fonction pour calculer la log-vraisemblance
#' @description Cette fonction calcule la log-vraisemblance du modèle
#'              en fonction des probabilités prédites pour chaque observation.
#'
#' @param X Matrice des caractéristiques (n x p),
#'          n e= nombre d'individus et p = nombre de variables
#' @param y Vecteur des étiquettes (n x 1)
#' @param theta Matrice des coeffs de la régression (p x K),
#'              p = nombre de caractéristiques et K = nombre de classes.
#'
#' @return La log-vraisemblance du modèle
calcul_log_likelihood <- function(X, y, theta) {
  proba <- predict_proba(X, theta)
  # calculer les proba sur l'échantillon d'apprentissage permet
  # de mesurer à quel point le modèle s'ajuste aux données

  log_likelihood <- 0
  for (i in seq_len(nrow(X))) {
    classe <- y[i]
    index_classe <- which(levels(y) == classe)
    log_likelihood <- log_likelihood + log(proba[i, index_classe])
  }
  return(log_likelihood)
}


#' @title Fonction pour calculer l'AIC (Akaike Information Criterion)
#' @description Cette fonction calcule l'Akaike Information Criterion (AIC).
#'              L'AIC permet de comparer plusieurs modèles entre eux.
#'              Il prend en compte la qualité de l'ajustement et le nombre de paramètres utilisés.
#'
#' @param X : matrice des caractéristiques
#' @param y : vecteur des étiquettes (n x 1)
#' @param ll Log-vraisemblance du modèle
#' @param theta Matrice des coeffs de la régression (p x K),
#'              p = nombre de caractéristiques et K = nombre de classes.
#'
#' @return L'AIC du modèle
calcul_aic <- function(X, y, ll, theta) {
  k <- length(theta)
  aic <- 2*k - 2*ll
  return(aic)
}
