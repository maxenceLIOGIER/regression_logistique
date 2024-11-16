
ui <- dashboardPage(
  dashboardHeader(title = "Régression Logistique Multinomiale"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Lecture des données", tabName = "menu1", icon = icon("file-upload")),
      menuItem("Statistiques et Visualisation", tabName = "menu2", icon = icon("chart-line")),
      menuItem("Modélisation et Prédiction", tabName = "menu3", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # Page d'accueil
      tabItem(tabName = "accueil",
              fluidRow(
                box(
                  title = "Bienvenue", status = "primary", solidHeader = TRUE, width = 12,
                  h3("Application de Régression Logistique Multinomiale"),
                  p("Cette application a été conçue pour démontrer les fonctionnalités d’un package 
                    R dédié à la régression logistique multinomiale."),
                  p("Voici les fonctionnalités principales :"),
                  tags$ul(
                    tags$li("Chargement et exploration des données"),
                    tags$li("Statistiques descriptives et visualisation"),
                    tags$li("Modélisation par régression logistique multinomiale"),
                    tags$li("Prédiction et évaluation du modèle")
                  ),
                  p("Utilisez le menu à gauche pour naviguer à travers l'application.")
                )
              )
      ),
      
      # Menu 1 : Lecture des données
      tabItem(tabName = "menu1",
              tabsetPanel(
                tabPanel("Charger les données", 
                         fluidRow(
                           column(6, 
                                  box(title = "Charger un fichier CSV ou Excel", status = "primary", solidHeader = TRUE, width = NULL,
                                      fileInput("file", "Importer un fichier CSV ou Excel", 
                                                accept = c(".csv", ".xls", ".xlsx")),
                                      checkboxInput("header", "Entête", TRUE),
                                      selectInput("sep", "Séparateur", choices = c("," = ",", ";" = ";", "Tabulation" = "\t")),
                                      conditionalPanel(
                                        condition = "output.showSheetSelector",
                                        selectInput("sheet_select", "Sélectionner une feuille", choices = NULL)
                                      ),
                                      actionButton("load_data", "Charger")
                                  )
                           ),
                           column(6, 
                                  box(title = "Actions sur les données", status = "warning", solidHeader = TRUE, width = NULL,
                                      selectInput("factor_column", "Colonnes à convertir en facteur :",
                                                  choices = NULL, selected = NULL),
                                      actionButton("convert_to_factor", "Convertir en Facteur"),
                                      br(),
                                      selectInput("num_column", "Colonnes à convertir en numérique :",
                                                  choices = NULL, selected = NULL),
                                      actionButton("convert_to_numeric", "Convertir en Numérique")
                                  )
                           )
                         ),
                         fluidRow(
                           column(12, 
                                  box(title = "Aperçu des données", status = "info", solidHeader = TRUE, width = NULL,
                                      DTOutput("data_preview"), style = "overflow-x: auto;"  # Ajout du style pour activer le slider
                                  )
                           )
                         )
                ),
                
                
                tabPanel("Structure des données",
                         verbatimTextOutput("data_structure")
                ),
                tabPanel("Résumé statistique",
                         verbatimTextOutput("data_summary")
                )
              )
      ),
      
      # Menu 2 : Statistiques et visualisation
      # Menu 2 : Statistiques et visualisation
      tabItem(tabName = "menu2",
              tabsetPanel(
                # Répartition des variables
                tabPanel("Répartition des variables",
                         fluidRow(
                           column(4,
                                  selectInput("variable", "Choisir une variable", choices = NULL)  # Choix de la variable
                           ),
                           column(8,
                                  plotOutput("variable_distribution")  # Affichage du graphique
                           )
                         )
                ),
                
                # Boxplot des variables numériques
                tabPanel("Boxplot des variables numériques",
                         fluidRow(
                           column(4,
                                  selectInput("numeric_var", "Choisir une variable numérique", choices = NULL),
                                  selectInput("cat_var", "Choisir une variable catégorielle", choices = NULL, selected = NULL)
                           ),
                           column(8,
                                  plotOutput("boxplot_numeric")
                           )
                         )
                ),
                
                # Exploration des variables catégorielles
                tabPanel("Exploration des variables catégorielles",
                         fluidRow(
                           column(4,
                                  selectInput("cat_var1", "Choisir la première variable catégorielle", choices = NULL),
                                  selectInput("cat_var2", "Choisir la deuxième variable catégorielle", choices = NULL)
                           ),
                           column(8,
                                  plotOutput("cat_cat_relation")
                           )
                         )
                )
              )
      ),
      
      
      # Menu 3 : Modélisation et prédiction
      tabItem(tabName = "menu3",
              tabsetPanel(
                tabPanel("Préparation des données",
                         selectInput("target", "Variable cible", choices = NULL),
                         uiOutput("features_ui"),
                         actionButton("prepare_data", "Préparer les données")
                ),
                tabPanel("Lancer la modélisation",
                         verbatimTextOutput("model_summary"),
                         actionButton("run_model", "Lancer la régression logistique")
                ),
                tabPanel("Prédiction",
                         fileInput("new_data", "Importer des données pour la prédiction"),
                         verbatimTextOutput("prediction_results")
                )
              )
      )
    )
  )
)
