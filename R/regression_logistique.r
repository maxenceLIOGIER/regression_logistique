#' LogisticRegression Class
#'
#' A class for performing multinomial logistic regression using gradient descent.
#'
#' @import R6
#' @import ggplot2

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

    #' @field penalite (character) Regularization type. Can be "l1" (lasso), "l2" (ridge), or "elasticnet".
    penalite = NULL,

    #' @field lambda (numeric) Regularization parameter.
    lambda = NULL,

    #' @field l1_ratio (numeric) The ratio of L1 regularization in elasticnet. Default is 0.
    l1_ratio = NULL,

    #' @field summary_values (vector) A vector containing the log-likelihood and AIC values.
    summary_values = c(ll = NULL, aic = NULL),

    #' @description Initialize the LogisticRegression model with specified parameters.
    #'
    #' @param nb_iters (integer) Number of iterations for gradient descent. Default is 500.
    #' @param alpha (numeric) Learning rate for gradient descent. Default is 0.01.
    #' @param penalite (character) Regularization method: "l1" for lasso, "l2" for ridge, or "elasticnet". Default is NULL.
    #' @param lambda (numeric) Regularization parameter. Default is 0.
    #' @param l1_ratio (numeric) The ratio of L1 regularization in elasticnet. Default is 0.
    #' @return A LogisticRegression object.
    #' @method LogisticRegression initialize
    initialize = function(nb_iters = 500, alpha = 0.01, penalite = NULL,
                          lambda = 0, l1_ratio = 0) {
      self$nb_iters <- nb_iters
      self$alpha <- alpha
      self$penalite <- penalite
      self$lambda <- lambda
      self$l1_ratio <- l1_ratio
    },

    #' @description Perform multinomial logistic regression to find optimal parameters.
    #'
    #' @param X (data.frame) Predictor variables (features) for the model.
    #' @param y (vector) Target variable (labels) for training the model.
    #' @return (matrix) Optimized parameters for each class.
    #' @method LogisticRegression multinomial_logistic_regression
    multinomial_logistic_regression = function(X, y) {
      X_new <- as.matrix(prepare_x(X))
      classes_uniques <- unique(y)
      K <- length(classes_uniques)
      n <- ncol(X_new)
      theta <- matrix(0, nrow = n, ncol = K)
      colnames(theta) <- classes_uniques
      rownames(theta) <- colnames(X_new)

      for (k in 1:K) {
        y_k <- ifelse(y == classes_uniques[k], 1, 0)
        result <- descente_gradient(X_new, y_k, theta[, k, drop = FALSE],
                                    nb_iters = self$nb_iters,
                                    alpha = self$alpha,
                                    penalite = self$penalite,
                                    lambda = self$lambda,
                                    l1_ratio = self$l1_ratio)
        theta[, k] <- result$theta
      }

      return(theta)
    },

    #' @description Train the logistic regression model using provided data.
    #'
    #' @param X (data.frame) Training predictor variables (features).
    #' @param y (vector) Training target variable (labels).
    #' @return (LogisticRegression) A trained LogisticRegression model.
    #' @method LogisticRegression fit
    #' @export
    fit = function(X, y) {
      new_model <- self$clone()
      theta <- self$multinomial_logistic_regression(X, y)
      dict_coeff <- calcul_p_values(X, theta)

      ll <- calcul_log_likelihood(X, y, theta)
      aic <- calcul_aic(X, y, ll, theta)

      new_model$theta <- theta
      new_model$summary_values["ll"] <- ll
      new_model$summary_values["aic"] <- aic
      new_model$dict_coeff <- dict_coeff

      return(new_model)
    },

    #' @description Predict the probabilities of each class for new data.
    #'
    #' @param X (data.frame) New data (features) for which to predict probabilities.
    #' @param theta (matrix) Model parameters.
    #' @return (matrix) A matrix of predicted probabilities for each class.
    #' @method LogisticRegression predict_proba
    predict_proba = function(X, theta) {
      proba <- predict_proba(X, theta)
      return(proba)
    },

    #' @description Predict the class for each observation in the dataset.
    #'
    #' @param X (data.frame) New data (features) for which to predict the class.
    #' @return (vector) Predicted classes based on highest probabilities.
    #' @method LogisticRegression  predict
    predict = function(X) {
      if (is.null(self$theta)) {
        stop("Le modèle n'est pas encore entraîné")
      }

      proba <- predict_proba(X, self$theta)
      class_indices <- apply(proba, 1, which.max)
      class_names <- colnames(proba)
      pred <- class_names[class_indices]
      return(pred)
    },

    #' @description Evaluate the model performance using various metrics.
    #'
    #' @param y_true (vector) True labels.
    #' @param y_pred (vector) Predicted labels.
    #' @param confusion_matrix (logical) Whether to display the confusion matrix. Default is FALSE.
    #' @return (list) A list containing accuracy, precision, recall, f1 score, and confusion matrix if requested.
    #' @method LogisticRegression test
    test = function(y_true, y_pred, confusion_matrix = FALSE) {
      if (is.null(self$theta)) {
        stop("Le modèle n'est pas encore entraîné")
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

    #' @description Print the coefficients of the trained model.
    #'
    #' @return (list) A list of coefficients for each class.
    #' @method LogisticRegression print
    print = function() {
      if (is.null(self$theta)) {
        stop("Le modèle n'est pas encore entraîné")
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

    #' @description Display a summary of model metrics and coefficients.
    #'
    #' @return (void) Print summary to the console.
    #' @method LogisticRegression summary
    summary = function() {
      if (is.null(self$theta)) {
        stop("Le modèle n'est pas encore entraîné")
      }

      print_coeffs(self$dict_coeff)
      cat("Log-likelihood:", self$summary_values["ll"], "\n")
      cat("AIC:", self$summary_values["aic"], "\n")
    },

    #' @description Compute the importance of the variables based on the trained model.
    #'
    #' @param graph (logical) Whether to display the variable importance graph. Default is TRUE.
    #' @return (vector) A vector of the relative importance of each variable.
    #' @method LogisticRegression var_importance
    var_importance = function(graph = TRUE) {
      if (is.null(self$theta)) {
        stop("Le modèle n'est pas encore entraîné")
      }
      theta2 <- self$theta[-1, , drop = FALSE]
      importance <- rowMeans(abs(theta2))



      # importances en %
      importance <- importance * 100

      # Créer un data frame avec les noms des variables et leurs importances
      imp_df <- data.frame(
        Variable = rownames(theta2),
        Importance = importance
      )

      # Trier les variables par importance décroissante
      imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]

      # Créer un barplot horizontal
      if (graph) {
        ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          xlab("Variable") +
          ylab("Importance (%)") +
          ggtitle("Importance des variables")
      }

      return(importance)
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
model <- LogisticRegression$new(penalite = NULL, lambda = 0,
                                l1_ratio = 0.5)
model <- model$fit(X_train, y_train)
model$summary()
# model$print()

# Importance des variables
model$var_importance()

# Prédiction sur les données test
y_pred <- model$predict(X_test)
print(model$test(y_test, y_pred, confusion_matrix = TRUE))


# # Exemple d'utilisation
# set.seed(123)
# setwd("C:/Users/maxen/Documents/_SISE/Prog Stat sous R/Projet")
# data <- read.csv("framingham.csv")
# # head(data)

# # supprimer lignes manquantes
# data <- na.omit(data)

# # il faut s'assurer que les variables quali sont bien encodées
# data$male <- as.factor(data$male)
# data$education <- as.factor(data$education)
# data$currentSmoker <- as.factor(data$currentSmoker)
# data$BPMeds <- as.factor(data$BPMeds)
# data$prevalentStroke <- as.factor(data$prevalentStroke)
# data$prevalentHyp <- as.factor(data$prevalentHyp)
# data$diabetes <- as.factor(data$diabetes)
# data$TenYearCHD <- as.factor(data$TenYearCHD)

# # X y
# X <- data[, -c(16)]
# y <- data$TenYearCHD

# # Séparation des données en train et test
# index <- sample(1:nrow(data), nrow(data) * 0.7)
# X_train <- X[index, ]
# y_train <- y[index]
# X_test <- X[-index, ]
# y_test <- y[-index]

# model <- LogisticRegression$new()
# model <- model$fit(X_train, y_train)
# predictions <- model$predict(X_test)

# print(model$test(y_test, predictions, confusion_matrix = TRUE))

# model$summary()


# # glm
# data2 <- data.frame(prepare_x(data))
# glm_model <- glm(TenYearCHD ~ . - 1, data = data[index, ], family = binomial)
# summary(glm_model)
