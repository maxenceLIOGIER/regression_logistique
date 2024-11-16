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
}
