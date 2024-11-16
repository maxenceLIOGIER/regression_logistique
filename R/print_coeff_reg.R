# Fonction pour afficher un résumé clair des coefficients
print_coeff_reg <- function(theta) {
  #' @description afficher les coefficients de la régression
  #' @param theta : coefficients de la régression
  #' @return coefficients de la régression

  cat("\n Coefficients de la régression : \n\n")

  if (ncol(theta) == 2) {
    # Afficher uniquement la classe 1 si binaire
    for (j in seq_len(nrow(theta))) {
      cat(rownames(theta)[j], ":", theta[j, 2], "\n")
    }

  } else {
    # Afficher toutes les classes si multiclasse
    for (k in seq_len(ncol(theta))) {
      cat("\nClasse", colnames(theta)[k], ":\n")
      for (j in seq_len(nrow(theta))) {
        cat(rownames(theta)[j], ":", theta[j, k], "\n")
      }
    }
  }

  cat("______________________________")
  cat("\n\n")
}