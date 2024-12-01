# library("R6")
# library("ggplot2")

# source("R/prepare_x.R")
# source("R/predict_proba.R")
# source("R/calcul_metriques.R")
# source("R/p_values.R")
# source("R/descente_gradient.R")
# source("R/reg_multinomiale.R")

#' LogisticRegression Class
#'
#' A class for performing multinomial logistic regression using gradient descent.
#'
#' @include reg_multinomiale.R
#' @include prepare_x.R
#' @include calcul_metriques.R
#' @include predict_proba.R
#' @include p_values.R
#' @include descente_gradient.R
#'
#' @importFrom R6 R6Class
#' @import ggplot2
#' @import XML
#' 
#' @export
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    #' @field theta (matrix) Model coefficients, including the intercept for each class.
    theta = NULL,

    #' @field dict_coeff (data.frame) A data frame of p-values for the model coefficients.
    dict_coeff = NULL,

    #' @field nb_iters (integer) Number of iterations for gradient descent. Default is 500.
    nb_iters = NULL,

    #' @field alpha (numeric) Learning rate for gradient descent. Default is 0.01.
    alpha = NULL,

    #' @field penalty (character) Regularization type. Can be "l1" (lasso), "l2" (ridge), or "elasticnet".
    penalty = NULL,

    #' @field lambda (numeric) Regularization parameter.
    lambda = NULL,

    #' @field l1_ratio (numeric) The ratio of L1 regularization in elasticnet. Default is 0.
    l1_ratio = NULL,

    #' @field summary_values (vector) A vector containing the log-likelihood and AIC values.
    summary_values = c(ll = NULL, aic = NULL),

    #' @field is_too_big (logical) Whether the dataset is too large to calculate p-values
    is_too_big = FALSE,


    #' @description Initializes the Logistic Regression model with specified parameters.
    #'
    #' @param nb_iters (integer) Number of iterations for gradient descent. Default is 500.
    #' @param alpha (numeric) Learning rate for gradient descent. Default is 0.01.
    #' @param penalty (character) Regularization method: "l1" for lasso, "l2" for ridge, or "elasticnet". Default is NULL.
    #' @param lambda (numeric) Regularization parameter. Default is 0.
    #' @param l1_ratio (numeric) The ratio of L1 regularization in elasticnet. Default is 0.
    #' @return A LogisticRegression object.
    #' @method LogisticRegression initialize
    initialize = function(nb_iters = 500, alpha = 0.01, penalty = NULL,
                          lambda = 0, l1_ratio = 0) {
      self$nb_iters <- nb_iters
      self$alpha <- alpha
      self$penalty <- penalty
      self$lambda <- lambda
      self$l1_ratio <- l1_ratio
    },


    #' @description Trains the logistic regression model using provided data.
    #'
    #' @param X (data.frame) Training predictor variables (features).
    #' @param y (vector) Training target variable (labels).
    #' @return (LogisticRegression) A trained LogisticRegression model.
    #' @method LogisticRegression fit
    #' @export
    fit = function(X, y) {
      # new object
      new_model <- self$clone()

      print("Training the model...")
      # Train the model
      theta <- reg_multinomiale(X, y, self$nb_iters, self$alpha,
                                self$penalty, self$lambda, self$l1_ratio)
      print("Model trained successfully")

      # Calculate p-values
      # If the dataset is too large, p-values are not calculated
      if (nrow(X) > 10000) {
        new_model$is_too_big <- TRUE
        cat("The dataset is too large to calculate p-values (>10 000 rows)\n")
      } else {
        dict_coeff <- calcul_p_values(X, theta)
        new_model$dict_coeff <- dict_coeff
      }

      # Calculate log-likelihood and AIC
      ll <- calcul_log_likelihood(X, y, theta)
      aic <- calcul_aic(X, y, ll, theta)

      # Assign values to the new model
      new_model$theta <- theta
      new_model$summary_values["ll"] <- ll
      new_model$summary_values["aic"] <- aic

      return(new_model)
    },


    #' @description Predicts the probabilities of individuals belonging to classes
    #'              Uses the scores obtained by multiplying X with theta,
    #'              then applies the softmax function to obtain the probabilities
    #'
    #' @param X (data.frame) New data (features) for which to predict probabilities.
    #' @param theta (matrix) Model parameters.
    #' @return (matrix) A matrix of predicted probabilities for each class.
    #' @method LogisticRegression predict_proba
    predict_proba = function(X, theta) {
      proba <- predict_proba(X, theta)
      return(proba)
    },


    #' @description Predicts the class for each observation in the dataset.
    #'
    #' @param X (data.frame) New data (features) for which to predict the class.
    #' @return (vector) Predicted classes based on highest probabilities.
    #' @method LogisticRegression  predict
    predict = function(X) {
      if (is.null(self$theta)) {
        stop("The model is not trained yet")
      }

      proba <- predict_proba(X, self$theta)
      class_indices <- apply(proba, 1, which.max)
      class_names <- colnames(proba)
      pred <- class_names[class_indices]
      return(pred)
    },


    #' @description Evaluates the model performance using various metrics :
    #'              accuracy, precision, recall, f1 score, and confusion matrix
    #'
    #' @param y_true (vector) True labels.
    #' @param y_pred (vector) Predicted labels.
    #' @param confusion_matrix (logical) Whether to display the confusion matrix
    #' @return (list) accuracy, precision, recall, f1 score, and confusion matrix if requested.
    test = function(y_true, y_pred, confusion_matrix = FALSE) {
      if (is.null(self$theta)) {
        stop("The model is not trained yet")
      }

      accuracy <- sum(y_true == y_pred) / length(y_true)

      classes <- unique(y_true)
      precision_list <- c()
      rappel_list <- c()

      for (classe in classes) {
        VP <- sum(y_true == classe & y_pred == classe)
        FP <- sum(y_true != classe & y_pred == classe)
        FN <- sum(y_true == classe & y_pred != classe)

        precision <- VP / (VP + FP)
        rappel <- VP / (VP + FN)

        precision_list <- c(precision_list, precision)
        rappel_list <- c(rappel_list, rappel)
      }

      precision <- mean(precision_list, na.rm = TRUE)
      rappel <- mean(rappel_list, na.rm = TRUE)
      f1_score <- 2 * precision * rappel / (precision + rappel)

      if (confusion_matrix) {
        confusion_matrix <- table(y_pred, y_true)
        print(confusion_matrix)
      }

      return(list(accuracy = accuracy, precision = precision,
                  rappel = rappel, f1_score = f1_score))
    },


    #' @description Prints the coefficients of the trained model.
    #'
    #' @return (list) A list of coefficients for each class.
    #' @method LogisticRegression print
    print = function() {
      if (is.null(self$theta)) {
        stop("The model is not trained yet")
      }

      coeffs <- list()
      noms_classes <- colnames(self$theta)

      for (i in seq_len(ncol(self$theta))) {
        coeffs[[noms_classes[i]]] <- data.frame(
          Coefficients = round(self$theta[, i], 5)
        )
      }

      return(coeffs)
    },


    #' @description Displays a summary of model metrics and coefficients.
    #'
    #' @return (void) Print summary to the console.
    #' @method LogisticRegression summary
    summary = function() {
      if (is.null(self$theta)) {
        stop("The model is not trained yet")
      }

      # Display coefficients
      # Display p-values if the dataset is not too large
      if (!self$is_too_big) {
        print_coeffs(self$dict_coeff)
      } else {
        cat("The dataset is too large to display p-values (>10 000 rows)\n")
        coeffs <- self$print()
        print(coeffs)
        cat("\n")
      }

      cat("Log-likelihood:", self$summary_values["ll"], "\n")
      cat("AIC:", self$summary_values["aic"], "\n")
    },


    #' @description Computes the importance of the variables based on the trained model.
    #'
    #' @param graph (logical) Whether to display the variable importance graph. Default is TRUE.
    #' @return (vector) A vector of the relative importance of each variable.
    #'         The sum is equal to 100%
    #' @method LogisticRegression var_importance
    var_importance = function(graph = TRUE) {
      if (is.null(self$theta)) {
        stop("The model is not trained yet")
      }
      theta2 <- self$theta[-1, , drop = FALSE]
      importance <- rowMeans(abs(theta2))

      # importances en %
      importance <- importance / sum(importance) * 100

      # Creer un data frame avec les noms des variables et leurs importances
      imp_df <- data.frame(
        Variable = rownames(theta2),
        Importance = importance
      )

      # Trier les variables par importance decroissante
      imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]

      # Creer un barplot horizontal
      if (graph) {
        p <- ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          xlab("Variables") +
          ylab("Importance (%)") +
          ggtitle("Importance of variables")
        print(p)
      }

      return(importance)
    },
    #' @description Exports the Logistic Regression model to PMML format.
    #' @param file_path (character) Path to save the PMML file. Default is "model.pmml".
    #' @param target_name (character) The name of the target variable. Default is NULL, which assumes the last column is the target.
    #' @return Saves the PMML file at the specified location.
    #' @method LogisticRegression export_pmml
    export_pmml = function(file_path = "model.pmml", target_name) {
      if (is.null(self$theta)) {
        stop("The model is not trained yet. Please train the model before exporting.")
      }
      
      # Create PMML structure
      pmml_doc <- XML::newXMLNode("PMML", namespaceDefinitions = c("xmlns" = "http://www.dmg.org/PMML-4_2"))
      XML::newXMLNode("Header", parent = pmml_doc, 
                      .children = list(XML::newXMLNode("Application", attrs = c(name = "Custom Logistic Regression in R", version = "1.0"))))
      
      # RegressionModel node
      reg_model <- XML::newXMLNode("RegressionModel", parent = pmml_doc, 
                                   attrs = c(functionName = "classification", modelName = "LogisticRegression", algorithmName = "multinomialLogisticRegression"))
      
      # MiningSchema
      mining_schema <- XML::newXMLNode("MiningSchema", parent = reg_model)
      for (feature in rownames(self$theta)) {
        XML::newXMLNode("MiningField", attrs = c(name = feature, usageType = "active"), parent = mining_schema)
      }
      
      # Dynamically use the provided target name
      XML::newXMLNode("MiningField", attrs = c(name = target_name, usageType = "target"), parent = mining_schema)
      
      # RegressionTable for each class
      for (k in seq_len(ncol(self$theta))) {
        class_name <- colnames(self$theta)[k]
        intercept <- self$theta[1, k]  # Intercept term
        regression_table <- XML::newXMLNode("RegressionTable", parent = reg_model, attrs = c(intercept = intercept, targetCategory = class_name))
        
        for (i in 2:nrow(self$theta)) {  # Skip intercept (row 1)
          feature <- rownames(self$theta)[i]
          coefficient <- self$theta[i, k]
          XML::newXMLNode("NumericPredictor", parent = regression_table, attrs = c(name = feature, coefficient = coefficient))
        }
      }
      
      # Save the PMML to file
      XML::saveXML(pmml_doc, file = file_path)
      message("PMML model exported to: ", file_path)
    }
    
    
    
  )
)


# Exemple d'utilisation
set.seed(123)
data(iris)

# Séparation des données en train et test
X <- iris[, -c(5)]
y <- iris$Species

index <- sample(seq_len(nrow(iris)), nrow(iris) * 0.7)
X_train <- X[index, ] # n x p
y_train <- y[index] # nF x 1
X_test <- X[-index, ]
y_test <- y[-index]

# Entraînement du modèle
model <- LogisticRegression$new(penalty = NULL, lambda = 0,
                                l1_ratio = 0.5)
model <- model$fit(X_train, y_train)
model$summary()
pmml_content <- model$export_pmml(target_name = "Species")
print(pmml_content)
