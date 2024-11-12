library(R6)
library(devtools)

# Définition de la classe LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    initialize = function(X, y, alpha = 0.01, num_iters = 1000) {
      #' @param X : matrice des caractéristiques
      #' @param y : vecteur des étiquettes
      #' @param alpha : taux d'apprentissage
      #' @param num_iters : nombre d'itérations
      self$X <- X
      self$y <- y
      self$alpha <- alpha
      self$num_iters <- num_iters
      self$theta <- NULL
      self$cost_history <- NULL
    },

    # Fonction sigmoïde
    sigmoid = function(z) {
      #' @param z : vecteur ou matrice
      #' @return sigmoïde de z (valeur comprise entre 0 et 1)
      return(1 / (1 + exp(-z)))
    },

    # Fonction de coût
    fct_cout = function(theta) {
      #' @param theta : vecteur des paramètres
      #' @return coût de la régression logistique
      m <- length(self$y)
      h <- self$sigmoid(self$X %*% theta)
      cost <- (-1 / m) * sum(self$y * log(h) + (1 - self$y) * log(1 - h))
      return(cost)
    },

    # Fonction de descente de gradient
    gradient_descent = function(X, y, theta, alpha, num_iters) {
      #' @param X : matrice des caractéristiques
      #' @param y : vecteur des étiquettes
      #' @param theta : vecteur des paramètres
      #' @param alpha : taux d'apprentissage
      #' @param num_iters : nombre d'itérations
      #' @return liste des paramètres optimisés et de l'historique des coûts
      m <- length(y)
      cost_history <- numeric(num_iters)

      for (i in 1:num_iters) {
        h <- self$sigmoid(X %*% theta)
        gradient <- (1 / m) * (t(X) %*% (h - y))
        theta <- theta - alpha * gradient
        cost_history[i] <- self$fct_cout(theta)
      }

      return(list(theta = theta, cost_history = cost_history))
    },

    # Fonction pour déterminer les variables qualitatives et quantitatives
    type_variable = function(df) {
      #' @param df : data.frame contenant les données
      #' @return liste contenant les variables qualitatives et quantitatives

      quali <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
      quanti <- names(df)[sapply(df, is.numeric) | sapply(df, is.integer)]

      return(list(qualitatives = quali, quantitatives = quanti))
    },

    # Fonction pour préparer la matrice X
    prepare_X = function(df) {
      #' @param df : data.frame contenant les données
      #' @return matrice X des caractéristiques

      # Déterminer les variables qualitatives et quantitatives
      types_variables <- self$type_variable(df)
      quali <- types_variables$qualitatives
      quanti <- types_variables$quantitatives

      # Encodage one-hot des variables qualitatives
      quali_data <- df[, quali, drop = FALSE]
      quali_encoded <- model.matrix(~ . - 1, data = quali_data)

      # Normalisation des variables quantitatives
      quanti_data <- df[, quanti, drop = FALSE]
      quanti_normalized <- scale(quanti_data)

      # Combinaison des variables encodées et normalisées
      X <- cbind(quali_encoded, quanti_normalized)

      return(X)
    },

    # Régression logistique multinomiale
    multinomial_logistic_regression = function(X, y, num_iters = 1000) {
      #' @param X data.frame des caractéristiques
      #'          peut contenir des variables qualitatives et quantitatives
      #' @param y Vecteur des étiquettes = variable cible
      #' @param num_iters Nombre d'itérations
      #' @return Liste des paramètres optimisés et de l'historique des coûts

      # Initialisation des paramètres
      classes_uniques <- unique(y)
      K <- length(classes_uniques)
      n <- ncol(X)
      theta <- matrix(0, nrow = n, ncol = K)
      colnames(theta) <- classes_uniques
      rownames(theta) <- colnames(X)

      # Taux d'apprentissage
      alpha <- 0.01

      X_new <- self$prepare_X(X)

      # Optimisation des paramètres pour chaque classe
      for (k in 1:K) {
        y_k <- ifelse(y == classes_uniques[k], 1, 0) # Encodage one-hot
        result <- self$gradient_descent(X_new, y_k, theta[,k], alpha, num_iters)
        theta[, k] <- result$theta
      }

      return(theta)
    }
  )
)



## Exemple d'utilisation
set.seed(123)
X <- data.frame(
  color = c("red", "blue", "green", "blue", "red"),
  height = c(150, 160, 170, 180, 190),
  weight = c(65, 70, 75, 80, 85),
  stringsAsFactors = TRUE
)
y <- c(0, 0, 1, 1, 2)
alpha <- 0.01
num_iters <- 1000

model <- LogisticRegression$new()
theta <- model$multinomial_logistic_regression(X, y, alpha, num_iters)
print(theta)