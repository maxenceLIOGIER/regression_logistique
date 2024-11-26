#' @title Déterminer les variables qualitatives et quantitatives
#' @description Cette fonction permet de différencier les variables qualitatives (facteurs et caractères)
#'              et quantitatives (numériques et entières) dans un data.frame.
#'
#' @param df : data.frame contenant les données à analyser.
#'
#' @return Une liste contenant deux éléments :
#'         - `qualitatives` : un vecteur des noms des variables qualitatives
#'         - `quantitatives` : un vecteur des noms des variables quantitatives
#'
#' @examples
#' print("Hi")
#' @export

# Fonction pour déterminer les variables qualitatives et quantitatives
type_variable <- function(df) {
  #' @param df : data.frame contenant les données
  #' @return liste contenant les variables qualitatives et quantitatives

  quali <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  quanti <- names(df)[sapply(df, is.numeric) | sapply(df, is.integer)]

  return(list(qualitatives = quali, quantitatives = quanti))
}

#' @title Préparer la matrice X à la prise en compte des variables mixtes
#' @description Cette fonction prépare la matrice X en encodant les variables qualitatives (s'il y a plus de 2 modalités),
#'              en normalisant les variables quantitatives et en ajoutant une colonne d'intercept (si nécessaire).
#'
#' @param X : data.frame contenant les données d'entrée avec des variables qualitatives et quantitatives.
#'
#' @return Une matrice `X` préparée, avec les variables qualitatives encodées, les variables quantitatives normalisées,
#'         et une colonne d'intercept ajoutée (si ce n'est pas déjà présent).
#'
#' @examples
#' print("Hi")
#' @export

# Fonction pour préparer la matrice X à la prise en compte des variables mixtes
prepare_x <- function(X) {
  #' @param X : data.frame contenant les données
  #' @return X encodé et normalisé
  #' @description encode variables quali et normalise les quanti + intercept

  # Déterminer les variables qualitatives et quantitatives
  types_variables <- type_variable(X)
  quali <- types_variables$qualitatives
  quanti <- types_variables$quantitatives

  # Encodage one-hot des variables qualitatives s'il y a plus de 2 modalités
  if (length(quali) > 0) {
    quali_data <- X[, quali, drop = FALSE]

    # Filtrer les colonnes avec plus de 2 niveaux
    quali_to_encode <- quali[sapply(quali_data, function(x) length(levels(x)) > 2)]
    quali_to_keep <- quali[sapply(quali_data, function(x) length(levels(x)) == 2)]
    quali_kept <- X[, quali_to_keep, drop = FALSE]

    # Encodage one-hot
    quali_encoded_list <- lapply(quali_to_encode, function(col) {
      model.matrix(~ . , data = X[, col, drop = FALSE])[ , -1]
    })
    quali_encoded <- do.call(cbind, quali_encoded_list)

  } else {
    quali_encoded <- matrix(0, nrow = nrow(X), ncol = 0)
    quali_kept <- matrix(0, nrow = nrow(X), ncol = 0)
    # matrices vides pour éviter les erreurs
  }

  # Normalisation des variables quantitatives
  if (length(quanti) > 0) {
    quanti_data <- X[, quanti, drop = FALSE]
    quanti_normalized <- scale(quanti_data)
  } else {
    quanti_normalized <- matrix(0, nrow = nrow(X), ncol = 0)
  }

  # Combinaison des variables encodées et normalisées
  X <- cbind(quali_kept, quali_encoded, quanti_normalized)

  # Ajout d'une colonne d'intercept, si n'est pas déjà présente
  if (sum(colnames(X) == "intercept") == 0) {
  X <- cbind(1, X)
  colnames(X)[1] <- "intercept"
  }

  # Conversion de toutes les colonnes en numérique
  X[] <- sapply(X, as.numeric)
  X <- as.data.frame(X)
  return(X)
}
