library(R6)

source("R/calcul_metriques.R")
source("R/prepare_x.R")
source("R/descente_gradient.R")
source("R/predict_proba.R")
source("R/print_coeff_reg.R")


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
      X_new <- prepare_x(X)

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
        result <- descente_gradient(X_new, y_k, theta[, k], self$nb_iters, self$alpha)
        theta[, k] <- result$theta
      }

      return(theta)
    },

    # Fonction d'apprentissage
    fit = function(X, y) {
      #' @description entraîner le modèle de régression logistique
      #' @return nouveau modèle, entraîné sur les données X et y
      new_model <- self$clone()

      # calcul de theta et des metriques
      theta <- self$multinomial_logistic_regression(X, y)
      ll <- calcul_log_likelihood(X, y, theta)
      aic <- calcul_aic(X, y, ll, theta)
      pseudo_r2 <- calcul_pseudo_r2(X, y, ll)

      # Mise à jour des paramètres du modèle
      new_model$theta <- theta
      new_model$summary_values["ll"] <- ll
      new_model$summary_values["aic"] <- aic
      new_model$summary_values["pseudo_r2"] <- pseudo_r2

      # reste à calculer les p-values et les coefficients de la régression

      return(new_model)
    },

    # Fonction de prédiction
    predict = function(X) {
      #' @description prédire les classes des individus

      # Calcul des probabilités d'appartenance aux classes
      probabilities <- predict_proba(X, self$theta)

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

      # Affichage des coefficients de la régression
      print_coeff_reg(self$theta)

      # Affichage des métriques
      cat("Log-likelihood:", self$summary_values["ll"], "\n")
      cat("AIC:", self$summary_values["aic"], "\n")
      cat("Pseudo R² de McFadden:", round(self$summary_values["pseudo_r2"], 4), "\n")
    }
  )
)


# Exemple d'utilisation
set.seed(123)
setwd("C:/Users/maxen/Documents/_SISE/Prog Stat sous R/Projet")
data <- read.csv("framingham.csv")
# head(data)

# supprimer lignes manquantes
data <- na.omit(data)

# Séparation des données en train et test
X <- data[, -c(16)]
y <- data$TenYearCHD
index <- sample(seq_len(nrow(data)), nrow(data) * 0.7)
X_train <- X[index, ]
y_train <- y[index]
X_test <- X[-index, ]
y_test <- y[-index]

# Entraînement du modèle
model <- LogisticRegression$new()
model <- model$fit(X_train, y_train)
model$summary()

# Prédiction sur les données test
predictions <- model$predict(X_test)
print(model$test(y_test, predictions, confusion_matrix = TRUE))

# data2 <- data[index, ]
# glm_model <- glm(TenYearCHD ~ ., data = data2, family = binomial)
# summary(glm_model)