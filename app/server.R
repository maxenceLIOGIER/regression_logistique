server <- function(input, output, session) {
  # Variables réactives
  
  data <- reactiveVal(NULL)
  sheets <- reactiveVal(NULL) 
  reactiveTrainData <- reactiveVal()
  reactiveTestData <- reactiveVal()
  reactiveModel <- reactiveVal()
  # Charger les données
  observeEvent(input$load_data, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    # Si fichier CSV
    if (ext == "csv") {
      data(read.csv(input$file$datapath, header = input$header, sep = input$sep))
    } 
    
    # Si fichier Excel
    else if (ext %in% c("xls", "xlsx")) {
      data(readxl::read_excel(input$file$datapath, col_names = input$header))
    }
    
    # Mettre à jour l'aperçu des données
    updateSelectizeInput(session, "factor_columns", choices = names(data()), server = TRUE)
    updateSelectizeInput(session, "num_columns", choices = names(data()), server = TRUE)
  })
  
  # Aperçu des données
  output$data_preview <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE))
  })
  
    # Résumé statistique
    output$data_summary <- renderPrint({
      req(data())
      summary(data())
    })
  

    # Structure des données
    output$data_structure <- renderPrint({
      req(data())
      str(data())
    })
    
  # Conversion en facteur
  observeEvent(input$convert_to_factor, {
    req(data())
    cols <- input$factor_columns
    df <- data()
    df[cols] <- lapply(df[cols], as.factor)
    data(df)
  })
  
  # Conversion en numérique
  observeEvent(input$convert_to_numeric, {
    req(data())
    cols <- input$num_columns
    df <- data()
    df[cols] <- lapply(df[cols], as.numeric)
    data(df)
  })
  
  # Traitement des valeurs manquantes
  observeEvent(input$handle_na, {
    req(data())
    method <- input$na_method
    df <- data()
    
    for (col in names(df)) {
      if (any(is.na(df[[col]]))) {
        if (method == "mean" && is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        } else if (method == "median" && is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        } else if (method == "mode") {
          mode_value <- as.numeric(names(sort(table(df[[col]]), decreasing = TRUE)[1]))
          df[[col]][is.na(df[[col]])] <- mode_value
        } else if (method == "fixed") {
          df[[col]][is.na(df[[col]])] <- input$fixed_value
        }
      }
    }
    data(df)
  })
  
  
  #page 2 statistique et visualisation 
  
  # Mise à jour dynamique des colonnes pour les sélections
  observe({
    req(data())
    col_names <- names(data())
    
    # Colonnes numériques et catégorielles
    num_cols <- col_names[sapply(data(), is.numeric)]
    cat_cols <- col_names[sapply(data(), is.factor) | sapply(data(), is.character)]
    
    # Mettre à jour les choix
    updateSelectInput(session, "variable", choices = col_names, selected = NULL)         # Toutes les colonnes
    updateSelectInput(session, "numeric_var", choices = num_cols, selected = NULL)      # Numériques
    updateSelectInput(session, "cat_var", choices = cat_cols, selected = NULL)          # Catégorielles
    updateSelectInput(session, "cat_var1", choices = cat_cols, selected = NULL)         # Catégorielles
    updateSelectInput(session, "cat_var2", choices = cat_cols, selected = NULL)         # Catégorielles
  })
  
  # Répartition des variables
  output$variable_distribution <- renderPlot({
    req(data(), input$variable)
    var_data <- data()[[input$variable]]
    
    if (is.numeric(var_data)) {
      ggplot(data(), aes_string(x = input$variable)) +
        geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Répartition de la variable", input$variable), x = input$variable, y = "Fréquence")
    } else {
      ggplot(data(), aes_string(x = input$variable)) +
        geom_bar(fill = "lightgreen", color = "black", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Répartition de la variable", input$variable), x = input$variable, y = "Effectif")
    }
  })
  
  # Boxplot des variables numériques
  output$boxplot_numeric <- renderPlot({
    req(data(), input$numeric_var, input$cat_var)
    
    ggplot(data(), aes_string(x = input$cat_var, y = input$numeric_var)) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Boxplot de", input$numeric_var, "par rapport à", input$cat_var), 
           x = input$cat_var, y = input$numeric_var)
  })
  

  
  
  #page 3 modelisation prediction
  # Sélection automatique des variables explicatives basées sur l'importance
  observe({
    req(data())
    col_names <- names(data())
    
    # Mettre à jour les choix pour la variable cible
    updateSelectInput(session, "target", choices = col_names, selected = input$target)
    
    # Mise à jour automatique des variables explicatives en excluant la cible
    selected_target <- input$target
    if (!is.null(selected_target)) {
      feature_candidates <- setdiff(col_names, selected_target)
      updateCheckboxGroupInput(session, "features", 
                               choices = feature_candidates, 
                               selected = feature_candidates)
    }
  })
  
  # Préparation des données
  observeEvent(input$prepare_data, {
    req(data(), input$target, input$features)
    
    target_var <- input$target
    features <- input$features
    
    # Validation des sélections
    if (is.null(features) || length(features) == 0) {
      showNotification("Veuillez sélectionner au moins une variable explicative.", type = "error")
      return()
    }
    if (target_var %in% features) {
      showNotification("La variable cible ne peut pas être une variable explicative.", type = "error")
      return()
    }
    
    # Filtrer les données
    selected_data <- data()[, c(features, target_var), drop = FALSE]
    
    # Split des données
    split <- caret::createDataPartition(selected_data[[target_var]], p = input$split_ratio / 100, list = FALSE)
    train_data <- selected_data[split, ]
    test_data <- selected_data[-split, ]
    
    reactiveTrainData(train_data)
    reactiveTestData(test_data)
    
    output$split_summary <- renderPrint({
      list(
        "Données d'entraînement" = dim(train_data),
        "Données de test" = dim(test_data)
      )
    })
  })
  
  # Fonctionnalité de téléchargement des données de test
  output$download_test_data <- downloadHandler(
    filename = function() {
      paste("test_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(reactiveTestData())  # Vérifie que les données de test sont prêtes
      write.csv(reactiveTestData(), file, row.names = FALSE)
    }
  )
  
  #Entraîner le modèle 
  observeEvent(input$run_model, {
    req(reactiveTrainData()) 
    
    # Activer le spinner
    showNotification("Entraînement en cours...", type = "warning", duration = NULL, id = "training")
    
    # Récupérer les données et les hyperparamètres
    train_data <- reactiveTrainData()
    target_var <- input$target
    features <- input$features
    
    # Préparer les données
    train_data[[target_var]] <- factor(train_data[[target_var]])  # Convertir la variable cible en facteur
    X <- train_data[, features]
    y <- train_data[[target_var]]
    
    # Initialiser le modèle avec les hyperparamètres personnalisés
    model <- LogisticRegression$new(
      nb_iters = input$nb_iters,      # Nombre d'itérations
      alpha = input$alpha,            # Taux d'apprentissage
      penalty = input$penalty,        # Type de régularisation
      lambda = input$lambda           # Valeur de régularisation
    )
    
    # Entraîner le modèle
    model <- model$fit(X, y)
    
    # Stocker le modèle dans une variable réactive
    reactiveModel(model)
    
    # Désactiver le spinner
    removeNotification(id = "training")
    
    # Résumé du modèle
    output$model_summary <- renderPrint({
      req(reactiveModel())  # Vérifie que le modèle est prêt
      model <- reactiveModel()
      summary_output <- capture.output(model$summary())
      cat(paste(summary_output, collapse = "\n"))
    })
    
    # Notification de succès
    showNotification("Entraînement terminé avec succès !", type = "message")
  })
  

  
  
  # Prédictions

  observeEvent(input$run_prediction, {
    req(reactiveModel(), reactiveTestData())
    
    model <- reactiveModel()
    test_data <- reactiveTestData()
    
    # Préparation des prédicteurs X (tout sauf la variable cible)
    X <- test_data[, setdiff(names(test_data), input$target), drop = FALSE]
    
    # Prédictions des classes
    pred_classes <- model$predict(X)
    
    # Affichage de la matrice de confusion
    output$confusion_matrix <- renderPrint({
      confusion <- model$test(as.factor(pred_classes), test_data[[input$target]], confusion_matrix = TRUE)
      cat("Matrice de Confusion:\n")
      print(confusion)  # Afficher la confusion
    })
    
    # Affichage des probabilités des classes
    output$probabilities <- renderPrint({
      proba <- model$predict_proba(X, model$theta)  # Calcul des probabilités
      prob_df <- as.data.frame(proba)
      
      # Afficher uniquement les 5 premières lignes des probabilités
      prob_head <- head(prob_df, 5)
      cat("Probabilités des classes pour les 5 premiers individus:\n")
      print(prob_head)  # Afficher les probabilités des 5 premiers individus
    })
  })
  
  
  # Affichage de l'importance des variables
  output$variable_importance_plot <- renderPlot({
    req(reactiveModel())  # Vérifie que le modèle est bien chargé
    
    model <- reactiveModel()  # Récupérer le modèle réactif
    
    # Appeler la méthode pour récupérer l'importance des variables et générer le graphique
    model$var_importance(graph = TRUE) 
  })
  
  
}
  
  
  

