#' @title Calcul de la matrice hessienne
#' @description Calcul de la matrice hessienne
#' @param X : matrice des données taille n x p
#' @param theta : matrice des paramètres taille p x K
#' @return matrice hessienne taille p x p
hessienne <- function(X, theta) {

  # préparation de X
  X <- as.matrix(prepare_x(X)) # taille n x p

  # Initialisation de H
  H <- matrix(0, ncol(X), ncol(X))

  # Calcul de H pour chaque classe
  for (k in seq_len(ncol(theta))) {
    p_k <- 1 / (1 + exp(-X %*% theta[, k]))
    D_k <- diag(as.vector(p_k * (1 - p_k)))
    H <- H + t(X) %*% D_k %*% X
  }
  return(H)
}


#' @title Calcul des p-values des coefficients de la régression
#' @description Calcul des p-values des coefficients de la régression
#'
#' @param X matrice des données d'entrée
#' @param theta matrice des coefficients de régression
#'
#' @return liste de dataframes contenant :
#'         coeffs, erreurs std, z-scores et p-values
#'         pour chaque classe de la régression
calcul_p_values <- function(X, theta) {
  H <- hessienne(X, theta)

  # calcul des se, z-scores et p-values
  se <- sqrt(diag(solve(H)))
  z_scores <- theta / se
  p_values <- 2 * (1 - pnorm(abs(z_scores)))

  # Formattage des p-values pour une meilleure lisibilité
  # Seules les p-values < 0.001 sont affichées en notation scientifique
  f_p_values <- apply(p_values, 2, function(p) {
    ifelse(p < 0.001, format(p, scientific = TRUE, digits = 3),
           sprintf("%.5f", p))
  })

  # Création du dictionnaire de coefficients
  dict_coeff <- list()
  noms_classes <- colnames(theta)

  for (i in seq_len(ncol(theta))) {
    significance <- ifelse(p_values[, i] < 0.01, "***",
                           ifelse(p_values[, i] < 0.05, "**",
                                  ifelse(p_values[, i] < 0.1, "*", "")))

    dict_coeff[[noms_classes[i]]] <- data.frame(
    Coefficients = round(theta[, i], 5),
    Std_Errors = round(se, 5),
    Z_Scores = round(z_scores[, i], 5),
    P_Values = f_p_values[, i],
    Signif = significance
    )
  }

  return(dict_coeff)
}


#' @title Afficher un résumé des coefficients
#' @description afficher les coefficients de la régression
#' @param dict_coeff : liste de dataframes contenant
#'                     coeffs, erreurs std, z-scores et p-values
#' @return coefficients de la régression
print_coeffs <- function(dict_coeff) {
  cat("\n Coefficients de la régression : \n")

  if (length(dict_coeff) == 2) {
    # Afficher uniquement la classe 1 si binaire
    print(dict_coeff[[2]])

  } else {
    # Afficher toutes les classes si multiclasse
    for (k in seq_len(length(dict_coeff))) {
      cat("\nClasse", names(dict_coeff[k]), ":\n")
      print(dict_coeff[[k]])
    }
  }

  cat("______________________________")
  cat("\n\n")
}