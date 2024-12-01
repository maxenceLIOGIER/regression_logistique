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
    # Inclusion du fichier CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    
    tabItems(
      # Page d'accueil
      tabItem(tabName = "accueil",
              fluidRow(
                column(8,  # Partie texte (60%)
                       box(
                         title = "Application de Régression Logistique Multinomiale", status = "primary", solidHeader = TRUE, width = 12,
                         p("Cette application a été conçue pour démontrer les fonctionnalités de notre package 
                   R dédié à la régression logistique multinomiale."),
                         br(),
                         p("Voici les fonctionnalités principales :"),
                         tags$ul(
                           tags$li(icon("database", style = "color: #3498db; font-size: 18px;"), " Chargement et Prétraitement des données"),
                           tags$li(icon("chart-bar", style = "color: #2ecc71; font-size: 18px;"), " Statistiques descriptives et visualisation"),
                           tags$li(icon("project-diagram", style = "color: #f1c40f; font-size: 18px;"), " Modélisation par régression logistique multinomiale"),
                           tags$li(icon("tasks", style = "color: #e74c3c; font-size: 18px;"), " Prédiction et évaluation du modèle")
                         ),
                         br(),
                         p("Utilisez le menu à gauche pour naviguer à travers l'application.")
                       )
                ),
                column(4,  # Partie image (40%)
                       div(
                         style = "text-align: center;",
                         img(src = "image.jpg", 
                             style = "max-width: 100%; height: auto; border-radius: 10px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);")
                       )
                )
              ),
              
              # Section des cartes horizontales
              fluidRow(
                column(4,
                       box(
                         status = "primary", solidHeader = TRUE, width = 12, class = "profile-box",
                         div(
                           icon("smile", class = "profile-icon"),
                           div(
                             class = "profile-text",
                             span("Souraya Ahmed", class = "profile-name"),
                             p(icon("envelope", class = "email-icon"), "sourayatahmed18@gmail.com")
                           )
                         )
                       )
                ),
                column(4,
                       box(
                         status = "primary", solidHeader = TRUE, width = 12, class = "profile-box",
                         div(
                           icon("smile", class = "profile-icon"),
                           div(
                             class = "profile-text",
                             span("Maxence", class = "profile-name"),
                             p(icon("envelope", class = "email-icon"), "maxencen@mail.com")
                           )
                         )
                       )
                ),
                column(4,
                       box(
                         status = "primary", solidHeader = TRUE, width = 12, class = "profile-box",
                         div(
                           icon("smile", class = "profile-icon"),
                           div(
                             class = "profile-text",
                             span("Yacine Ayachi", class = "profile-name"),
                             p(icon("envelope", class = "email-icon"), "yacine@mail.com")
                           )
                         )
                       )
                )
              )
      ),
      
      
      
      # Menu 1 : Lecture et prétraitement des données
      tabItem(tabName = "menu1",
              tabsetPanel(
                tabPanel("Charger et Prétraiter les données", 
                         fluidRow(
                           column(6, 
                                  box(title = "Charger un fichier CSV ou Excel", status = "primary", solidHeader = TRUE, width = NULL,
                                      fileInput("file", "Importer un fichier CSV ou Excel", 
                                                accept = c(".csv", ".xls", ".xlsx")),
                                      checkboxInput("header", "Utiliser la première ligne comme entête", TRUE),
                                      selectInput("sep", "Séparateur", choices = c("," = ",", ";" = ";", "Tabulation" = "\t"), selected = ","),
                                      actionButton("load_data", "Charger",class = "btn btn-primary")
                                  )
                           ),
                           column(6, 
                                  box(title = "Actions sur les données", status = "warning", solidHeader = TRUE, width = NULL,
                                      selectizeInput("factor_columns", "Colonnes à convertir en facteur :",
                                                     choices = NULL, multiple = TRUE),
                                      actionButton("convert_to_factor", "Convertir en Facteur",class = "btn btn-primary"),
                                      br(),
                                      selectizeInput("num_columns", "Colonnes à convertir en numérique :",
                                                     choices = NULL, multiple = TRUE),
                                      actionButton("convert_to_numeric", "Convertir en Numérique",class = "btn btn-primary"),
                                      br(),
                                      selectInput("na_method", "Méthode d'imputation pour les NA :",
                                                  choices = c("Moyenne" = "mean", 
                                                              "Médiane" = "median", 
                                                              "Mode" = "mode", 
                                                              "Valeur fixe" = "fixed"),
                                                  selected = "mean"),
                                      conditionalPanel(
                                        condition = "input.na_method == 'fixed'",
                                        numericInput("fixed_value", "Valeur fixe pour l'imputation :", value = 0)
                                      ),
                                      actionButton("handle_na", "Traiter les NA",class = "btn btn-primary")
                                  )
                           )
                         ),
                         fluidRow(
                           column(12, 
                                  box(title = "Aperçu des données", status = "info", solidHeader = TRUE, width = NULL,
                                      DTOutput("data_preview"), style = "overflow-x: auto;"
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
      tabItem(tabName = "menu2",
              tabsetPanel(
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
                )
              )
      ),
      
      # Menu 3 : Modélisation et prédiction
      tabItem(tabName = "menu3",
              tabsetPanel(
                # Tab 1: Choix des variables et Split Train/Test
                tabPanel("1. Choix des variables et Split Train/Test",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Variable cible", status = "primary", solidHeader = TRUE, width = 12,
                                    selectInput("target", label = NULL, choices = NULL)
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Variables explicatives", status = "primary", solidHeader = TRUE, width = 12,
                                    checkboxGroupInput("features", label = NULL, choices = NULL, inline = TRUE)
                                  )
                           )
                         ),
                         fluidRow(
                           column(12,
                                  box(
                                    title = "Configuration du Split Train/Test", status = "primary", solidHeader = TRUE, width = 12,
                                    numericInput("split_ratio", "Proportion des données d'entraînement (%)", value = 70, min = 50, max = 90),
                                    actionButton("prepare_data", "Préparer les données", class = "btn btn-primary"),
                                    br(),
                                    verbatimTextOutput("split_summary"),
                                    br(),
                                    downloadButton("download_test_data", "Télécharger les données de test", class = "btn btn-info")
                                  )
                           )
                         )
                ),
                
                # Tab 2: Entraîner le modèle
                tabPanel("2. Entraîner le modèle",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Hyperparamètres", status = "primary", solidHeader = TRUE, width = 12,
                                    numericInput("nb_iters", "Nombre d'itérations :", value = 500, min = 1),
                                    numericInput("alpha", "Taux d'apprentissage (alpha) :", value = 0.01, min = 0.0001),
                                    selectInput("penalty", "Type de régularisation :", choices = c("L1 (Lasso)" = "l1", "L2 (Ridge)" = "l2")),
                                    numericInput("lambda", "Valeur de régularisation (lambda) :", value = 0.05, min = 0)
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Lancer l'entraînement", status = "success", solidHeader = TRUE, width = 12,
                                    actionButton("run_model", "Lancer l'entraînement", class = "btn btn-primary"),
                                    br(),
                                    br(),
                                    verbatimTextOutput("model_summary")
                                  )
                           )
                         )
                ),
                
                # Tab 3: Validation et Prédictions
                tabPanel("3. Validation et Prédictions",
                         fluidRow(
                           column(12,
                                  box(
                                    title = "Faire des prédictions", status = "info", solidHeader = TRUE, width = 12,
                                    fileInput("new_data", "Importer des données pour la prédiction"),
                                    actionButton("run_prediction", "Faire les prédictions", class = "btn btn-primary"),
                                    br(),
                                    verbatimTextOutput("prediction_results")
                                  )
                           )
                         )
                ),
                
                # Tab 4: Importance des variables
                tabPanel("4. Importance des variables",
                         fluidRow(
                           column(12,
                                  box(
                                    title = "Graphique d'importance des variables", status = "warning", solidHeader = TRUE, width = 12,
                                    plotOutput("variable_importance_plot")
                                  )
                           )
                         )
                )
              )
      )
      
    )
  )
)
