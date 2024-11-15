library(R6)

# Définition de la classe LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    theta = NULL,
    nb_iters = NULL,
    alpha = NULL,
    summary_values = c(ll = 0, aic = 0, pseudo_r2 = 0),

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

      new_model$summary_values["ll"] <- new_model$calcul_log_likelihood(X, y)
      new_model$summary_values["aic"] <- new_model$Calcul_AIC(X, y)
      new_model$summary_values["pseudo_r2"] <- new_model$calcul_pseudo_r2(X, y)

      # Pour ces 3 métriques, j'ai été obligé de passer les fonctions en public.
      # Je n'arrive pas à faire marcher le code lorsque je combine new_model et private
      # normalement il faudrait ceci : new_model$private$calcul_log_likelihood(X, y)

      # reste à calculer les p-values et les coefficients de la régression

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
    },

    # Fonction pour évaluer le modèle
    test = function(y_true, y_pred, confusion_matrix = FALSE) {
      #' @param y_true : vecteur des vraies étiquettes
      #' @param y_pred : vecteur des étiquettes prédites
      #' @param confusion_matrix : booléen pour afficher la matrice de confusion
      #' @return liste des métriques et matrice de confusion si demandée

      # metriques
      accuracy <- sum(y_true == y_pred) / length(y_true)
      precision <- sum(y_true == y_pred & y_true == 1) / sum(y_pred == 1)
      recall <- sum(y_true == y_pred & y_true == 1) / sum(y_true == 1)
      f1_score <- 2 * precision * recall / (precision + recall)

      # Matrice de confusion
      if (confusion_matrix) {
        confusion_matrix <- table(y_pred, y_true)
        print(confusion_matrix)
      }

      return(list(accuracy = accuracy, precision = precision, recall = recall, f1_score = f1_score))
    },

    summary = function() {
      #' @description afficher un résumé des métriques du modèle
      #' @return résumé des métriques

      # Il faudrait intégrer les coefficients de la régression et les p-values

      cat("Log-likelihood:", self$summary_values["ll"], "\n")
      cat("AIC:", self$summary_values["aic"], "\n")
      cat("Pseudo R² de McFadden:", self$summary_values["pseudo_r2"], "\n")
    },

    # Fonction pour calculer la log-vraisemblance
    calcul_log_likelihood = function(X, y) {
      #' @param X : matrice des caractéristiques
      #' @param y : vecteur des étiquettes
      #' @description calculer la log-vraisemblance du modèle

      probabilities <- self$predict_proba(X)
      # calculer les proba sur l'échantillon d'apprentissage permet
      # de mesurer à quel point le modèle s'ajuste aux données

      log_likelihood <- 0
      for (i in 1:nrow(X)) {
        log_likelihood <- log_likelihood + log(probabilities[i, y[i] + 1])
        # hypothèse : les classes sont numérotées de 0 à K-1
      }
      return(log_likelihood)
    },

    # Fonction pour calculer l'AIC (Akaike Information Criterion)
    Calcul_AIC = function(X, y) {
      #' @description calculer l'AIC du modèle
      #' l'AIC sert à comparer des modèles, en pénalisant le nb de paramètres

      log_likelihood <- self$summary_values["ll"]
      k <- length(self$theta) # nombre de paramètres
      aic <- 2*k - 2*log_likelihood
      return(aic)
    },

    # Fonction pour calculer le pseudo R² de McFadden
    calcul_pseudo_r2 = function(X, y) {
      #' @description calculer le pseudo R² de McFadden
      #' @return valeur du pseudo R²

      null_deviance <- -2 * sum(y * log(mean(y))) # vérifier formule
      residual_deviance <- -2 * self$summary_values["ll"]
      pseudo_r2 <- 1 - (residual_deviance / null_deviance)
      return(pseudo_r2)
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

      # Ajout d'une colonne d'intercept, si n'est pas déjà présente
      if (sum(colnames(X) == "intercept") == 0) {
        X <- cbind(1, X)
        colnames(X)[1] <- "intercept"
      }

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

print(model$test(y_test, predictions, confusion_matrix = TRUE))

model$summary()

# glm_model <- glm(TenYearCHD ~ ., data = data[index, ], family = binomial)
# summary(glm_model)