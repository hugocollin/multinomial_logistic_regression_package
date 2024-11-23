# Importation des fonctions du modèle
source("model.R")

# Importation des bibliothèques nécessaires
library(DT)
library(shiny)
library(shinyjs)
library(readxl)

# Définition de la taille maximale des fichiers à 1Go
options(shiny.maxRequestSize = 1 * 1024^3)

# Interface utilisateur
ui <- fluidPage(
  useShinyjs(),

  tags$h1("Multinomial Logistic Regression", style = "text-align: center;"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Data",
          p(tags$b("Upload and configuration of the data"), style = "text-align: center;"),
          p("Choose a CSV or Excel file (max 1Go)"),
          fileInput("file", label = NULL,
                    accept = c(".csv", ".xlsx")),
          p("Delimiter (required only for CSV files)"),
          disabled(selectInput(
            "delimiter",
            label = NULL,
            choices = c(",", ";", ":", "-", "|", "/"),
            selected = ","
          )),
          p("Target variable (required)"),
          disabled(selectInput(
            "target",
            label = NULL,
            choices = NULL
          )),
          p("Columns to remove (optional)"),
          disabled(selectInput(
            "remove_cols",
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            selected = NULL
          )),
          disabled(actionButton("param_data", "Validate data parameters", width = "100%")),
          p(tags$b("Handle missing values (optional)"), style = "text-align: center;"),
          p("Handle missing method"),
          disabled(selectInput(
            "handle_missing_method",
            label = NULL,
            choices = c("Mean" = "mean", "Median" = "median", "Mode" = "mode", "Remove" = "remove"),
            selected = "mean"
          )),
          disabled(actionButton("handle_missing", "Handle missing values", width = "100%"))
        ),
        
        tabPanel(
          "Modeling",
          p(tags$b("Step 1 : Prepare data"), style = "text-align: center;"),
          p("Test size (%)"),
          disabled(sliderInput(
            "test_size",
            label = NULL,
            min = 0,
            max = 1,
            value = 0.3,
            step = 0.01
          )),
          disabled(actionButton("prepare_data", "Prepare data", width = "100%")),
          p(tags$b("Step 2 : Fit the model"), style = "text-align: center;"),
          p("Learning rate"),
          disabled(numericInput(
            "learning_rate",
            label = NULL,
            value = 0.01,
            min = 0,
            step = 0.001
          )),
          p("Maximum number of iterations"),
          disabled(numericInput(
            "max_iter",
            label = NULL,
            value = 1000,
            min = 1,
            step = 1
          )),
          disabled(actionButton("fit_model", "Fit the model", width = "100%")),
          p(tags$b("Step 3 : Predict classes"), style = "text-align: center;"),
          disabled(actionButton("predict", "Predict classes", width = "100%"))
        )
      )
    ),
    
    mainPanel(
      verbatimTextOutput("output"),
      DT::dataTableOutput("data_preview"),
      DT::dataTableOutput("predictions")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(model = NULL, accuracy = NULL, predictions = NULL, temp_files = character(), trigger = 0)

  observe({
    # Réinitialisation de l'interface
    disable("delimiter")
    disable("target")
    disable("remove_cols")
    disable("param_data")
    disable("handle_missing_method")
    disable("handle_missing")
    disable("test_size")
    disable("prepare_data")
    disable("learning_rate")
    disable("max_iter")
    disable("fit_model")
    disable("predict")
    rv$model <- NULL
    rv$predictions <- NULL

    # Vérification de l'existence du fichier
    req(input$file)
    
    # Lecture du fichier en fonction de l'extension
    if (grepl("\\.csv$", input$file$name, ignore.case = TRUE)) {
      df <- read.csv(input$file$datapath, sep = input$delimiter, stringsAsFactors = FALSE)
      output$output <- renderText("[INFO] The file has been successfully uploaded.")
    } else if (grepl("\\.xlsx$", input$file$name, ignore.case = TRUE)) {
      df <- read_excel(input$file$datapath)
      output$output <- renderText("[INFO] The file has been successfully uploaded.")
    } else {
      df <- NULL
      output$output <- renderText("[Warning] Please upload a CSV or Excel file.")
    }
    
    if (!is.null(df)) {
      noms_colonnes <- names(df)
      
      # Mise à jour de la sélection de la variable cible
      updateSelectInput(session, "target", choices = noms_colonnes)
      
      # Mise à jour de la sélection des colonnes à supprimer
      updateSelectInput(session, "remove_cols", choices = noms_colonnes, selected = NULL)

      # Activation de l'interface
      if (grepl("\\.csv$", input$file$name, ignore.case = TRUE)) {
        enable("delimiter")
      }
      enable("target")
      enable("remove_cols")
      enable("param_data")
    } else {
      # Désactivation de l'interface
      disable("delimiter")
      disable("target")
      disable("remove_cols")
      disable("param_data")
      disable("handle_missing_method")
      disable("handle_missing")
      disable("test_size")
      disable("prepare_data")
      disable("learning_rate")
      disable("max_iter")
      disable("fit_model")
      disable("predict")
      rv$model <- NULL
      rv$predictions <- NULL
    }
  })

  # Paramétrage des données
  observeEvent(input$param_data, {
    # Réinitialisation de l'interface
    disable("handle_missing_method")
    disable("handle_missing")
    disable("test_size")
    disable("prepare_data")
    disable("learning_rate")
    disable("max_iter")
    disable("fit_model")
    disable("predict")
    rv$predictions <- NULL

    # Vérification de l'existence du fichier et de la variable cible
    req(input$file, input$target)
    
    # Obtention des colonnes à supprimer
    remove_cols <- if (is.null(input$remove_cols)) {
      character(0)
    } else {
      input$remove_cols
    }

    # Création de l'objet LogisticRegression
    tryCatch({
      rv$model <- LogisticRegression$new(
        file_path = input$file$datapath,
        target = input$target,
        columns_to_remove = remove_cols,
        delimiter = input$delimiter
      )

      output$output <- renderText("[INFO] The data has been successfully configured.")

      # Activation de l'interface
      enable("handle_missing_method")
      enable("handle_missing")
      enable("test_size")
      enable("prepare_data")
    }, error = function(e) {
      rv$model <- NULL
      rv$predictions <- NULL
      output$output <- renderText(paste("[ERROR]", e$message))
      
      # Désactivation de l'interface
      disable("handle_missing_method")
      disable("handle_missing")
      disable("test_size")
      disable("prepare_data")
      disable("learning_rate")
      disable("max_iter")
      disable("fit_model")
      disable("predict")
    })
  })
  
  # Gestion des valeurs manquantes
  observeEvent(input$handle_missing, {
    # Réinitialisation de l'interface
    disable("learning_rate")
    disable("max_iter")
    disable("fit_model")
    disable("predict")

    # Récupération de la méthode sélectionnée par l'utilisateur
    selected_method <- input$handle_missing_method
    
    # Vérification que la méthode est bien sélectionnée
    req(selected_method)
    
    # Gestion des valeurs manquantes avec la méthode sélectionnée
    tryCatch({
      rv$model$handle_missing_values(method = selected_method)
      output$output <- renderText(paste("[INFO] The missing values have been successfully handled using the", selected_method, "method."))
      rv$trigger <- rv$trigger + 1
    }, error = function(e) {
      output$output <- renderText(paste("[ERROR]", e$message))
    })
  })
  
  # Préparation des données
  observeEvent(input$prepare_data, {
    # Réinitialisation de l'interface
    disable("learning_rate")
    disable("max_iter")
    disable("fit_model")
    disable("predict")
    rv$predictions <- NULL

    # Récupération de la taille de l'échantillon de test sélectionnée par l'utilisateur
    test_size <- input$test_size

    # Vérification que la taille de l'échantillon de test est bien sélectionnée
    req(test_size)

    # Préparation des données avec le paramètre fourni
    tryCatch({
      rv$model$prepare_data(test_size = test_size)
      output$output <- renderText("[INFO] The data has been successfully prepared.")

      # Activation de l'interface
      enable("learning_rate")
      enable("max_iter")
      enable("fit_model")
    }, error = function(e) {
      output$output <- renderText(paste("[ERROR]", e$message))

      # Désactivation de l'interface
      disable("learning_rate")
      disable("max_iter")
      disable("fit_model")
      disable("predict")
      rv$predictions <- NULL
    })
  })
  
  # Ajustement du modèle
  observeEvent(input$fit_model, {
    # Réinitialisation de l'interface
    disable("predict")
    rv$predictions <- NULL

    # Récupération des paramètres du modèle
    learning_rate <- input$learning_rate
    max_iter <- input$max_iter

    # Vérification des paramètres du modèle
    req(learning_rate, max_iter)

    # Validation des entrées
    if (is.null(learning_rate) || learning_rate <= 0) {
      output$output <- renderText("[Warning] The learning rate must be a positive number.")
      return(NULL)
    }
    
    if (is.null(max_iter) || max_iter < 1) {
      output$output <- renderText("[Warning] The maximum number of iterations must be a positive integer.")
      return(NULL)
    }

    # Ajustement du modèle avec les paramètres fournis
    tryCatch({
      rv$model$fit(learning_rate = learning_rate, max_iter = max_iter)
      output$output <- renderText("[INFO] The model has been successfully fitted.")
      
      # Activation du bouton prédiction
      enable("predict")
    }, error = function(e) {
      output$output <- renderText(paste("[ERROR]", e$message))

      # Désactivation de l'interface
      disable("predict")
      rv$predictions <- NULL
    })
  })
  
  # Prédiction des classes
  observeEvent(input$predict, {
    # Prédiction des classes
    tryCatch({
      rv$model$predict()
      output$output <- renderText(paste("[INFO] The data has been successfully predicted with an accuracy of", round(rv$model$accuracy * 100, 2), "%."))
      rv$predictions <- rv$model$predicted_targets
    }, error = function(e) {
      output$output <- renderText(paste("[ERROR]", e$message))
      rv$predictions <- NULL
    })
  })

  # Affichage des données
  output$data_preview <- DT::renderDataTable({
    req(rv$model)
    rv$trigger
    rv$model$data
    }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    scrollX = TRUE
  ))

  # Affichage des prédictions
  output$predictions <- DT::renderDataTable({
    req(rv$predictions)
    rv$predictions
  }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    scrollX = TRUE
  ))

  # Suppression des fichiers temporaires à la fermeture du programme
  session$onSessionEnded(function() {
    unlink(isolate(rv$temp_files), recursive = TRUE, force = TRUE)
  })
}

# Lancement de l'application
shinyApp(ui, server, options = list(launch.browser = TRUE))