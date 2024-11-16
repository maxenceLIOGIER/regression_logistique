# Fonction pour déterminer les variables qualitatives et quantitatives
type_variable <- function(df) {
  #' @param df : data.frame contenant les données
  #' @return liste contenant les variables qualitatives et quantitatives

  quali <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  quanti <- names(df)[sapply(df, is.numeric) | sapply(df, is.integer)]

  return(list(qualitatives = quali, quantitatives = quanti))
}

# Fonction pour préparer la matrice X à la prise en compte des variables mixtes
prepare_x <- function(X) {
  #' @param X : data.frame contenant les données
  #' @return X encodé et normalisé
  #' @description encode variables quali et normalise les quanti + intercept

  # Déterminer les variables qualitatives et quantitatives
  types_variables <- type_variable(X)
  quali <- types_variables$qualitatives
  quanti <- types_variables$quantitatives

  # Encodage one-hot des variables qualitatives
  if (length(quali) > 0) {
    quali_data <- X[, quali, drop = FALSE]
    quali_encoded <- model.matrix(~ . - 1, data = quali_data)
  } else {
    quali_encoded <- matrix(0, nrow = nrow(X), ncol = 0)
  }

  # Normalisation des variables quantitatives
  if (length(quanti) > 0) {
    quanti_data <- X[, quanti, drop = FALSE]
    quanti_normalized <- scale(quanti_data)
  } else {
    quanti_normalized <- matrix(0, nrow = nrow(X), ncol = 0)
  }

  # Combinaison des variables encodées et normalisées
  X <- cbind(quali_encoded, quanti_normalized)

  # Ajout d'une colonne d'intercept, si n'est pas déjà présente
  if (sum(colnames(X) == "intercept") == 0) {
  X <- cbind(1, X)
  colnames(X)[1] <- "intercept"
  }

  return(X)
}