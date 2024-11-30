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