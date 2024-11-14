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

    # Modèle de régression logistique multinomiale
    multinomial_logistic_regression = function(X, y) {
      #' @param X data.frame des caractéristiques
      #'          peut contenir des variables qualitatives et quantitatives
      #' @param y Vecteur des étiquettes = variable cible
      #' @description trouver paramètres optimaux de la régression logistique
      #' @return matrice des paramètres optimisés

      # Préparation de la matrice X
      X_new <- private$prepare_X(X)

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
        result <- private$descente_gradient(X_new, y_k, theta[, k])
        theta[, k] <- result$theta
      }

      return(theta)
    },

    # Fonction d'apprentissage
    fit = function(X, y) {
      #' @description entraîner le modèle de régression logistique
      #' @return nouveau modèle, entraîné sur les données X et y
      new_model <- self$clone()
      new_model$theta <- self$multinomial_logistic_regression(X, y)
      return(new_model)
    },

    predict_proba = function(X) {
      #' @description prédire probas d'appartenance des individus aux classes

      # Préparation de la matrice X
      X_new <- private$prepare_X(X)

      # Calculer les scores pour chaque classe
      scores <- X_new %*% self$theta

      # Appliquer la fonction softmax pour obtenir les probabilités
      softmax <- function(x) {
        exp(x) / rowSums(exp(x))
      }
      probabilities <- softmax(scores)

      return(probabilities)
    },

    # Fonction de prédiction
    predict = function(X) {
      #' @description prédire les classes des individus

      # Calcul des probabilités d'appartenance aux classes
      probabilities <- self$predict_proba(X)

      # Prédire la classe avec la probabilité la plus élevée
      predictions <- apply(probabilities, 1, which.max)
      return(predictions)
    }
  ),

  private = list(
    # Fonction sigmoïde
    sigmoid = function(z) {
      #' @param z : vecteur ou matrice
      #' @return sigmoïde de z (valeur comprise entre 0 et 1)
      return(1 / (1 + exp(-z)))
    },

    # Fonction de descente de gradient
    descente_gradient = function(X, y, theta) {
      #' @param X : matrice des caractéristiques
      #' @param y : vecteur des étiquettes
      #' @param theta : vecteur des paramètres
      #' @return liste des paramètres optimisés
      #' @description implémente l'algorithme de descente de gradient
      m <- nrow(X)
      for (i in 1:self$nb_iters) {
        h <- private$sigmoid(X %*% theta)
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
      #' @description encode variables quali et normalise les quanti + intercept

      # Déterminer les variables qualitatives et quantitatives
      types_variables <- private$type_variable(df)
      quali <- types_variables$qualitatives
      quanti <- types_variables$quantitatives

      # Encodage one-hot des variables qualitatives
      if (length(quali) > 0) {
        quali <- df[, quali, drop = FALSE]
        quali_encoded <- model.matrix(~ . - 1, data = quali_data)
      } else {
        quali_encoded <- matrix(0, nrow = nrow(df), ncol = 0)
      }

      # Normalisation des variables quantitatives
      if (length(quanti) > 0) {
        quanti_data <- df[, quanti, drop = FALSE]
        quanti_normalized <- scale(quanti_data)
      } else {
        quanti_normalized <- matrix(0, nrow = nrow(df), ncol = 0)
      }

      # Combinaison des variables encodées et normalisées
      X <- cbind(quali_encoded, quanti_normalized)

      # Ajout d'une colonne d'intercept
      X <- cbind(1, X)
      colnames(X)[1] <- "intercept"

      return(X)
    }
  )
)


# Exemple d'utilisation
set.seed(123)
setwd("C:/Users/maxen/Documents/_SISE/Prog Stat sous R/Projet")
data <- read.csv("framingham.csv")
head(data)

# supprimer lignes manquantes
data <- na.omit(data)

X <- data[, -c(16)]
y <- data$TenYearCHD

index <- sample(1:nrow(data), nrow(data) * 0.7)
X_train <- X[index, ]
y_train <- y[index]
X_test <- X[-index, ]
y_test <- y[-index]

model <- LogisticRegression$new()
model <- model$fit(X_train, y_train)
predictions <- model$predict(X_test)

confusion_matrix <- table(predictions, y_test)
confusion_matrix

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)