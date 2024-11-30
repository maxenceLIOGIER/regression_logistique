#' @title Determines qualitative and quantitative variables
#' @description differentiates between qualitative variables (factors and characters)
#'              and quantitative variables (numeric and integer) in a data.frame
#'              WARNING : if the variables are not correctly encoded this won't work
#' @param df : data.frame containing the data
#' @return list containing qualitative and quantitative variables
type_variable <- function(df) {
  quali <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  quanti <- names(df)[sapply(df, is.numeric) | sapply(df, is.integer)]

  return(list(qualitatives = quali, quantitatives = quanti))
}


#' @title Prepare the matrix X to account for mixed variables
#' @description Prepares the matrix X by encoding qualitative variables (if more than 2 modalities),
#'              normalizing quantitative variables, and adding an intercept column.
#' @param X : data.frame containing the data
#' @return encoded and normalized X
prepare_x <- function(X) {
  # Determine qualitative and quantitative variables
  types_variables <- type_variable(X)
  quali <- types_variables$qualitatives
  quanti <- types_variables$quantitatives

  # One-hot encoding of qualitative variables if more than 2 modalities
  if (length(quali) > 0) {
    quali_data <- X[, quali, drop = FALSE]

    # Check if columns are factors and have levels
    quali_data <- lapply(quali_data, function(x) {
      if (!is.factor(x)) {
        x <- as.factor(x)
      }
      return(x)
    })
    quali_data <- as.data.frame(quali_data)

    # Filter columns with more than 2 levels
    quali_to_encode <- quali[sapply(quali_data, function(x) length(levels(x)) > 2)]
    quali_to_keep <- quali[sapply(quali_data, function(x) length(levels(x)) == 2)]
    quali_kept <- X[, quali_to_keep, drop = FALSE]

    # One-hot encoding
    quali_encoded_list <- lapply(quali_to_encode, function(col) {
      model.matrix(~ ., data = X[, col, drop = FALSE])[ , -1]
    })
    quali_encoded <- do.call(cbind, quali_encoded_list)

  } else {
    quali_encoded <- matrix(0, nrow = nrow(X), ncol = 0)
    quali_kept <- matrix(0, nrow = nrow(X), ncol = 0)
    # matrices vides pour éviter les erreurs
  }

  # Normalization of quantitative variables
  if (length(quanti) > 0) {
    quanti_data <- X[, quanti, drop = FALSE]
    quanti_normalized <- scale(quanti_data)
  } else {
    quanti_normalized <- matrix(0, nrow = nrow(X), ncol = 0)
  }

  # Combination of encoded and normalized variables
  X <- cbind(quali_kept, quali_encoded, quanti_normalized)

  # Adding an intercept column, if not already present
  if (sum(colnames(X) == "intercept") == 0) {
    X <- cbind(1, X)
    colnames(X)[1] <- "intercept"
  }

  # Conversion of all columns to numeric
  X[] <- sapply(X, as.numeric)
  X <- as.data.frame(X)

  return(X)
}