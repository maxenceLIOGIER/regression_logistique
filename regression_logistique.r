library(R6)
# library(devtools)

# Définition de la classe LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    theta = NULL,
    nb_iters = NULL,
    alpha = NULL,

    # Initialisation de la classe
    initialize = function(nb_iters = 1000, alpha = 0.01) {
      #' @param nb_iters : nombre d'itérations
      #' @param alpha : taux d'apprentissage
      self$nb_iters <- nb_iters
      self$alpha <- alpha
    },

    # Fonction sigmoïde
    sigmoid = function(z) {
      #' @param z : vecteur ou matrice
      #' @return sigmoïde de z (valeur comprise entre 0 et 1)
      return(1 / (1 + exp(-z)))
    },

    # Fonction de coût
    fct_cout = function(X, y, theta) {
      #' @param theta : vecteur des paramètres
      #' @return coût de la régression logistique
      m <- length(y)
      h <- self$sigmoid(X %*% theta)
      cost <- (-1 / m) * sum(y * log(h) + (1 - y) * log(1 - h))
      return(cost)
    },

    # Fonction de descente de gradient
    descente_gradient = function(X, y, theta) {
      #' @param X : matrice des caractéristiques
      #' @param y : vecteur des étiquettes
      #' @param theta : vecteur des paramètres
      #' @return liste des paramètres optimisés et de l'historique des coûts
      m <- nrow(X)
      for (i in 1:self$nb_iters) {
        h <- self$sigmoid(X %*% theta)
        gradient <- t(X) %*% (h - y) / m
        theta <- theta - self$alpha * gradient
      }
      return(list(theta = theta))
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

      # Ajout d'une colonne d'intercept
      X <- cbind(1, X)
      colnames(X)[1] <- "intercept"

      return(X)
    },

    # Modèle de régression logistique multinomiale
    multinomial_logistic_regression = function(X, y) {
      #' @param X data.frame des caractéristiques
      #'          peut contenir des variables qualitatives et quantitatives
      #' @param y Vecteur des étiquettes = variable cible
      #' @return Liste des paramètres optimisés et de l'historique des coûts

      # Préparation de la matrice X
      X_new <- self$prepare_X(X)

      # Initialisation des paramètres
      classes_uniques <- unique(y)
      K <- length(classes_uniques)
      n <- ncol(X_new)
      theta <- matrix(0, nrow = n, ncol = K)
      colnames(theta) <- classes_uniques
      rownames(theta) <- colnames(X_new)

      # Optimisation des paramètres pour chaque classe
      for (k in 1:K) {
        y_k <- ifelse(y == classes_uniques[k], 1, 0) # Encodage one-hot
        result <- self$descente_gradient(X_new, y_k, theta[, k])
        theta[, k] <- result$theta
      }

      return(theta)
    },

    # Fonction d'apprentissage
    fit = function(X, y) {
      self$theta <- self$multinomial_logistic_regression(X, y)
    },

    # Fonction de prédiction
    predict = function(X) {
      # Préparation de la matrice X
      X_new <- self$prepare_X(X)

      # Calculer les scores pour chaque classe
      scores <- X_new %*% self$theta

      # Appliquer la fonction softmax pour obtenir les probabilités
      softmax <- function(x) {
        exp(x) / rowSums(exp(x))
      }
      probabilities <- softmax(scores)

      # Prédire la classe avec la probabilité la plus élevée
      predictions <- apply(probabilities, 1, which.max)

      return(predictions)
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

model <- LogisticRegression$new()
model$fit(X, y)
# TODO : il faudrait que $fit() ne modifie pas model
# ligne à avoir : model_fitted <- model$fit(X, y)

# print(model$theta)

X_pred <- data.frame(
  color = c("red", "blue", "green"),
  height = c(155, 175, 185),
  weight = c(68, 78, 88),
  stringsAsFactors = TRUE
)
predictions <- model$predict(X_pred)
print(predictions)