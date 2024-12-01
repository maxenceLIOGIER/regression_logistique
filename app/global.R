# Bibliothèques pour l'interface utilisateur Shiny
library(shiny)             # Interface principale
library(shinydashboard)    # Mise en page avancée
library(shinyWidgets)      # Widgets stylisés supplémentaires


# Bibliothèques pour la gestion des données
library(readr)             # Lecture des fichiers CSV
library(readxl)            # Lecture des fichiers Excel
library(stringr)           # Manipulation de chaînes de caractères



if (!requireNamespace("LogisticRegression", quietly = TRUE)) {
  devtools::install_github("maxenceLIOGIER/regression_logistique")
}

library(LogisticRegression)# Notre package

# Bibliothèques pour les visualisations
library(ggplot2)           # Graphiques de qualité
library(caret)             # Partition des données, métriques de performance


# Bibliothèques pour les tableaux et les téléchargements
library(DT)                # Tableaux interactifs
library(data.table)       
library(writexl)           # Exportation des fichiers Excel

# Bibliothèques pour les fonctionnalités supplémentaires
library(shinyjs)           # Interactions JavaScript avec Shiny
library(shinyalert)        # Notifications et alertes stylisées
library(shinycssloaders)
