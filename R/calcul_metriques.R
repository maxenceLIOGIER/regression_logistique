# source("R/predict_proba.R")

#' @title Fonction pour calculer la log-vraisemblance
#' @description Cette fonction calcule la log-vraisemblance du modèle de régression logistique en fonction
#'              des probabilités prédites pour chaque observation.
#'
#' @param X Matrice des caractéristiques (n x p), où n est le nombre d'individus et p est le nombre de caractéristiques.
#' @param y Vecteur des étiquettes (n x 1), où chaque élément correspond à la classe d'un individu.
#' @param theta Matrice des paramètres (p x K), où p est le nombre de caractéristiques et K est le nombre de classes.
#'
#' @return La log-vraisemblance du modèle, un nombre réel qui mesure la qualité de l'ajustement du modèle aux données.
#'
#' @examples
#' print("hi")
#' @export

# Fonction pour calculer la log-vraisemblance
calcul_log_likelihood <- function(X, y, theta) {
  #' @param X : matrice des caractéristiques
  #' @param y : vecteur des étiquettes
  #' @description calculer la log-vraisemblance du modèle

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
#' @description Cette fonction calcule l'Akaike Information Criterion (AIC) du modèle de régression logistique.
#'              L'AIC permet de comparer plusieurs modèles en tenant compte de la qualité de l'ajustement et du nombre de paramètres utilisés.
#'
#' @param ll Log-vraisemblance du modèle, un nombre réel représentant la log-vraisemblance calculée.
#' @param theta Matrice des paramètres (p x K), où p est le nombre de caractéristiques et K est le nombre de classes.
#'
#' @return L'AIC du modèle, un nombre réel qui pénalise les modèles avec un grand nombre de paramètres.
#'
#' @examples
#' print("hi")
#' @export

# Fonction pour calculer l'AIC (Akaike Information Criterion)
calcul_aic <- function(X, y, ll, theta) {
  #' @param X : matrice des caractéristiques
  #' @param y : vecteur des étiquettes
  #' @param ll : log-likelyhood du modèle
  #' @param theta : paramètres du modèle
  #' @description calculer l'AIC du modèle
  #' l'AIC sert à comparer des modèles, en pénalisant le nb de paramètres

  k <- length(theta) # nombre de paramètres
  aic <- 2*k - 2*ll
  return(aic)
}
