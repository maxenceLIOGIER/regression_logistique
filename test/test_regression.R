devtools::install_github("maxenceLIOGIER/regression_logistique")

library(LogisticRegression)

# Exemple d'utilisation
set.seed(123)
setwd("C:/Users/maxen/Documents/_SISE/Prog Stat sous R/Projet")
df <- read.csv("data_69.csv", sep = "|")
# head(df)

# Mise en forme des données
del_col <- c("Nom commune", "Date réception DPE", "Latitude", "Longitude",
             "Date_réception_DPE_graph", "Adresse_.BAN.", "X_geopoint", "Nom__commune_.BAN.")
df <- df[, !(names(df) %in% del_col)]
df <- na.omit(df) # Suppression des lignes contenant des NA

# Définition des variables explicatives et de la variable cible
y <- df$"Etiquette_DPE"
X <- df[, !(names(df) %in% c("Etiquette_DPE"))]

y <- as.factor(y)
X$Code_postal_.BAN. <- as.factor(X$Code_postal_.BAN.)

# Separation des donnees en train et test
index <- sample(seq_len(nrow(df)), nrow(df) * 0.7)
X_train <- X[index, ]
y_train <- y[index]
X_test <- X[-index, ]
y_test <- y[-index]

model <- LogisticRegression$new()
fit_time <- system.time({
  model <- model$fit(X_train, y_train)
})
print(fit_time)

model$summary()
# model$print()

# # Importance des variables
# model$var_importance(graph=TRUE)

# # Prediction sur les donnees test
# y_pred <- model$predict(X_test)
# print(model$test(y_test, y_pred, confusion_matrix = TRUE))