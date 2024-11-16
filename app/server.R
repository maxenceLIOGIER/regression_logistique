server <- function(input, output, session) {
  # Réactive pour stocker les données
  data <- reactiveVal(NULL)
  sheets <- reactiveVal(NULL)  # Pour les feuilles d'un fichier Excel
  
  # Charger le fichier
  observeEvent(input$file, {
    req(input$file)  # S'assurer qu'un fichier est sélectionné
    
    # Obtenir le chemin du fichier
    file_path <- input$file$datapath
    file_ext <- tools::file_ext(file_path)  # Extension du fichier
    
    # Charger selon le type de fichier
    if (file_ext == "csv") {
      file_data <- read.csv(
        file = file_path,
        header = input$header,
        sep = input$sep,
        na.strings = c("", "NA")  # Gestion des valeurs manquantes
      )
      data(file_data)  # Stocker les données
      sheets(NULL)  # Pas de feuilles pour un fichier CSV
    } else if (file_ext %in% c("xls", "xlsx")) {
      file_sheets <- excel_sheets(file_path)  # Obtenir les feuilles
      sheets(file_sheets)  # Stocker les noms des feuilles
      data(NULL)  # Réinitialiser les données
    } else {
      showNotification("Format de fichier non pris en charge.", type = "error")
    }
  })
  
  # Mise à jour de l'interface si un fichier Excel est chargé
  observe({
    req(sheets())
    updateSelectInput(
      session, "sheet_select",
      choices = sheets(),
      selected = sheets()[1]
    )
  })
  
  # Lecture de la feuille sélectionnée
  observeEvent(input$sheet_select, {
    req(input$file, sheets(), input$sheet_select)
    file_path <- input$file$datapath
    selected_data <- read_excel(file_path, sheet = input$sheet_select)
    data(selected_data)  # Stocker les données
  })
  
  # Aperçu interactif des données avec DT
  output$data_preview <- DT::renderDT({
    req(data())
    datatable(
      data(),
      options = list(
        pageLength = 10,    # Limiter à 10 lignes par page
        scrollX = TRUE,     # Activer le défilement horizontal
        searchHighlight = TRUE
      ),
      filter = "top",  # Barre de recherche en haut des colonnes
      rownames = FALSE
    )
  })
  
  # Mise à jour des listes de colonnes
  observe({
    req(data())
    col_names <- names(data())
    
    # Mettre à jour les choix pour les conversions
    updateSelectInput(session, "factor_column", choices = col_names, selected = NULL)
    updateSelectInput(session, "num_column", choices = col_names, selected = NULL)
  })
  
  # Convertir une colonne en facteur
  observeEvent(input$convert_to_factor, {
    req(data(), input$factor_column)
    updated_data <- data()
    updated_data[[input$factor_column]] <- as.factor(updated_data[[input$factor_column]])
    data(updated_data)
    showNotification(paste("La colonne", input$factor_column, "a été convertie en facteur."), type = "message")
  })
  
  # Convertir une colonne en numérique
  observeEvent(input$convert_to_numeric, {
    req(data(), input$num_column)
    updated_data <- data()
    updated_data[[input$num_column]] <- as.numeric(updated_data[[input$num_column]])
    data(updated_data)
    showNotification(paste("La colonne", input$num_column, "a été convertie en numérique."), type = "message")
  })
  
  
  
  # Structure des données
  output$data_structure <- renderPrint({
    req(data())
    str(data())
  })
  
  # Résumé statistique
  output$data_summary <- renderPrint({
    req(data())
    summary(data())
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
  
  # Relation entre variables catégorielles
  output$cat_cat_relation <- renderPlot({
    req(data(), input$cat_var1, input$cat_var2)
    
    ggplot(data(), aes_string(x = input$cat_var1, fill = input$cat_var2)) +
      geom_bar(position = "fill", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Relation entre", input$cat_var1, "et", input$cat_var2), y = "Proportion")
  })
}
  
  
  

