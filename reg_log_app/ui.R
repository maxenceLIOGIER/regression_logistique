fluidPage(
  # Titre de l'application, affiché en haut
  includeCSS("www/style.css"),
  titlePanel("Application de Régression Logistique Multinomiale"),
  
  # Disposition en deux parties : sidebar (barre latérale) et main (contenu principal)
  sidebarLayout(
    
    # Barre latérale pour les contrôles de l'utilisateur
    sidebarPanel(
      # Import d'un fichier de données (CSV ou XLSX)
      fileInput("datafile", "Charger un fichier de données (CSV ou XLSX)"),
      
      # Sélection de la variable cible pour la régression (affiché dynamiquement)
      uiOutput("var_target"),  
      
      # Sélection des variables explicatives (affiché dynamiquement)
      uiOutput("var_predictors"),
      
      # Bouton pour lancer la modélisation
      actionButton("run_model", "Lancer la Modélisation")
    ),
    
    # Contenu principal, affiché en onglets pour organiser les résultats
    mainPanel(
      tabsetPanel(
        
        # Onglet pour afficher un aperçu des données chargées
        tabPanel("Aperçu des Données", 
                 # Affichage d'une table de données interactive avec un "spinner" de chargement
                 DTOutput("data_preview") %>% withSpinner()),
        
        # Onglet pour afficher les résultats de la modélisation
        tabPanel("Résultats du Modèle", 
                 h4("Coefficients du modèle :"),  # Titre pour les résultats de régression
                 tableOutput("model_coefficients")),  # Tableau pour les coefficients
        
        # Onglet pour afficher une visualisation de l'importance des variables
        tabPanel("Visualisation", plotOutput("importance_plot"))  # Graphique d'importance
      )
    )
  )
)
