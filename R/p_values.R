# source("R/prepare_x.R")

#' @importFrom stats pnorm

#' @title hessian matrix
#' @description Computes the hessian matrix
#' @param X : data matrix (n x p)
#' @param theta : matrix of regression coefficients (p x K)
#' @return hessian matrix (p x p)
hessienne <- function(X, theta) {

  # preparation de X
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


#' @title Calculate p-values of regression coefficients
#' @description Calculate p-values of regression coefficients
#'
#' @param X matrix of input data
#' @param theta matrix of regression coefficients
#'
#' @return list of dataframes containing:
#'         coefficients, standard errors, z-scores, and p-values
#'         for each class of the regression
calcul_p_values <- function(X, theta) {
  H <- hessienne(X, theta)

  # calculate standard errors, z-scores, and p-values
  se <- sqrt(diag(solve(H)))
  z_scores <- theta / se
  p_values <- 2 * (1 - pnorm(abs(z_scores)))

  # Formatting p-values for better readability
  # Only p-values < 0.001 are displayed in scientific notation
  f_p_values <- apply(p_values, 2, function(p) {
    ifelse(p < 0.001, format(p, scientific = TRUE, digits = 3),
           sprintf("%.5f", p))
  })

  # Creating the dictionary of coefficients
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


#' @title Display a summary of coefficients
#' @description Display the regression coefficients
#' @param dict_coeff : list of dataframes containing
#'                     coefficients, standard errors, z-scores, and p-values
#' @return regression coefficients
print_coeffs <- function(dict_coeff) {
  cat("\n Regression Coefficients : \n")

  if (length(dict_coeff) == 2) {
    # Display only class 1 if binary
    print(dict_coeff[[2]])

  } else {
    # Display all classes if multiclass
    for (k in seq_len(length(dict_coeff))) {
      cat("\nClass", names(dict_coeff[k]), ":\n")
      print(dict_coeff[[k]])
    }
  }

  cat("significance codes:  *** '0.001' '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  cat("______________________________")
  cat("\n\n")
}