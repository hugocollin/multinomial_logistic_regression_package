# Liste des packages requis
packages <- c("R6", "DT", "shiny", "shinythemes", "shinyjs", "readr", "readxl", "stringr")

# Installer les packages manquants
installed_packages <- packages %in% rownames(installed.packages())
if(any(!installed_packages)){
    install.packages(packages[!installed_packages])
}

# Importation des fonctions du modèle
source("model.R")

# Importation des bibliothèques nécessaires
library(DT)
library(shiny)
library(shinythemes)
library(shinyjs)
library(readxl)
library(stringr)

# Définition de la taille maximale des fichiers à 1Go
options(shiny.maxRequestSize = 1 * 1024^3)

# Fonction de détection automatique du délimiteur
detect_delimiter <- function(file_path, n_max = 5) {
  lines <- read_lines(file_path, n_max = n_max)
  delimiters <- c(",", ";", ":", "-", "|", "/", "\t")
  
  # Comptage des séparateurs dans les lignes
  delimiter_counts <- sapply(delimiters, function(delim) {
    sum(sapply(lines, function(line) {
      num_sep <- str_count(line, fixed(delim))
      return(num_sep)
    }))
  })
  
  # Détection du séparateur le plus fréquent
  detected_delimiter <- delimiters[[which.max(delimiter_counts)]]
  
  return(detected_delimiter)
}

# Interface utilisateur
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),

  tags$h1("Multinomial Logistic Regression", style = "text-align: center;"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Data",
          p(tags$b("Upload data"), style = "text-align: center;"),
          p("Choose a CSV or Excel file (max 1Go)"),
          fileInput("file", label = NULL,
                    accept = c(".csv", ".xlsx")),
          disabled(checkboxInput("auto_delimiter", "Auto delimiter", value = TRUE)),
          p("Delimiter (required only for CSV files)"),
          disabled(selectInput(
            "delimiter",
            label = NULL,
            choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Colon (:)" = ":", "Hyphen (-)" = "-", "Pipe (|)" = "|", "Slash (/)" = "/", "Tab (\\t)" = "\t"),
            selected = ","
          )),
          disabled(actionButton("upload_data", "Upload data", width = "100%")),
          p(tags$b("Handle missing values (optional)"), style = "text-align: center;"),
          p("Handle missing numeric values method"),
          disabled(selectInput(
            "handle_missing_num_method",
            label = NULL,
            choices = c("None" = "none", "Mean" = "mean", "Median" = "median", "Mode" = "mode", "Remove" = "remove"),
            selected = "none"
          )),
          p("Handle missing categorical values method"),
          disabled(selectInput(
            "handle_missing_cat_method",
            label = NULL,
            choices = c("None" = "none", "Mode" = "mode", "Remove" = "remove"),
            selected = "none"
          )),
          disabled(actionButton("handle_missing", "Handle missing values", width = "100%"))
        ),
        
        tabPanel(
          "Modeling",
          p(tags$b("Step 1 : Prepare data"), style = "text-align: center;"),
          disabled(checkboxInput("auto_target", "Auto target variable", value = TRUE)),
          p("Target variable (required)"),
          disabled(selectInput(
            "target",
            label = NULL,
            choices = NULL
          )),
          disabled(checkboxInput("auto_delete_cols", "Auto delete columns (can be very slow for large datasets)", value = FALSE)),
          p("Columns to remove (optional)"),
          disabled(selectInput(
            "remove_cols",
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            selected = NULL
          )),
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
          p("Batch size"),
          disabled(numericInput(
            "batch_size",
            label = NULL,
            value = 50,
            min = 1,
            step = 1
          )),
          p("Tolerance"),
          disabled(numericInput(
            "tol",
            label = NULL,
            value = 1e-3,
            min = 1e-4,
            step = 0.0001
          )),
          disabled(actionButton("fit_model", "Fit the model", width = "100%")),
          p(tags$b("Step 3 : Predict classes"), style = "text-align: center;"),
          disabled(actionButton("predict", "Predict classes", width = "100%"))
        )
      )
    ),
    
    mainPanel(
      class = "main-panel",
      verbatimTextOutput("output"),
      verbatimTextOutput("missing_info"),
      DT::dataTableOutput("data_preview"),
      DT::dataTableOutput("predictions")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  rv <- reactiveValues(model = NULL, accuracy = NULL, predictions = NULL, temp_files = character(), trigger = 0)

  observe({
    withProgress(message = 'File import > ', value = 0, {
      # Réinitialisation de l'interface
      incProgress(0.2, detail = "Interface updating in progress...")

      disable("auto_delimiter")
      disable("delimiter")
      disable("upload_data")
      disable("handle_missing_num_method")
      disable("handle_missing_cat_method")
      disable("handle_missing")
      disable("auto_target")
      disable("target")
      disable("auto_delete_cols")
      updateCheckboxInput(session, "auto_delete_cols", value = FALSE)
      disable("remove_cols")
      disable("test_size")
      disable("prepare_data")
      disable("learning_rate")
      disable("max_iter")
      disable("batch_size")
      disable("tol")
      disable("fit_model")
      disable("predict")
      output$missing_info <- renderText(NULL)
      rv$model <- NULL
      rv$predictions <- NULL

      # Vérification de l'existence du fichier
      incProgress(0.2, detail = "File verification in progress...")

      req(input$file)

      # Lecture du fichier en fonction de l'extension
      if (grepl("\\.csv$", input$file$name, ignore.case = TRUE)) {
        # Détection automatique du délimiteur
        if (!input$auto_delimiter) {
          enable("delimiter")
          delimiter <- input$delimiter
        } else {
          disable("delimiter")

          # Détection du délimiteur
          delimiter <- detect_delimiter(input$file$datapath)

          incProgress(0.2, detail = "Delimiter detection in progress...")

          # Sélection automatique du délimiteur
          updateSelectInput(session, "delimiter", selected = delimiter)
        }

        incProgress(0.2, detail = "File reading in progress...")
        df <- read_delim(input$file$datapath, delim = delimiter, show_col_types = FALSE)
        output$output <- renderText("[INFO] The file has been successfully uploaded.")
      } else if (grepl("\\.xlsx$", input$file$name, ignore.case = TRUE)) {
        incProgress(0.2, detail = "File reading in progress...")
        df <- read_excel(input$file$datapath)
        output$output <- renderText("[INFO] The file has been successfully uploaded.")
      } else {
        df <- NULL
        output$output <- renderText("[WARNING] Please upload a CSV or Excel file.")
      }
      
      if (!is.null(df)) {
        # Activation de l'interface
        if (grepl("\\.csv$", input$file$name, ignore.case = TRUE)) {
          enable("auto_delimiter")
        }
        enable("upload_data")

        incProgress(1, detail = "Success")
      } else {
        # Désactivation de l'interface
        disable("auto_delimiter")
        disable("delimiter")
        disable("upload_data")
        disable("handle_missing_num_method")
        disable("handle_missing_cat_method")
        disable("handle_missing")
        disable("auto_target")
        disable("target")
        disable("auto_delete_cols")
        disable("remove_cols")
        disable("test_size")
        disable("prepare_data")
        disable("learning_rate")
        disable("max_iter")
        disable("batch_size")
        disable("tol")
        disable("fit_model")
        disable("predict")
        rv$model <- NULL
        rv$predictions <- NULL

        incProgress(1, detail = "Error")

        if (is.null(df)) {
          output$output <- renderText("[WARNING] The file could not be read.")
        }
      }
    })
  })

  # Paramétrage des données
  observeEvent(input$upload_data, {
    withProgress(message = 'Data configuration > ', value = 0, {
      
      # Réinitialisation de l'interface
      incProgress(0.1, detail = "Interface updating in progress...")

      disable("handle_missing_num_method")
      disable("handle_missing_cat_method")
      disable("handle_missing")
      disable("auto_target")
      disable("target")
      disable("auto_delete_cols")
      disable("remove_cols")
      disable("test_size")
      disable("prepare_data")
      disable("learning_rate")
      disable("max_iter")
      disable("batch_size")
      disable("tol")
      disable("fit_model")
      disable("predict")
      rv$predictions <- NULL

      # Vérification de l'existence du fichier
      incProgress(0.1, detail = "Data verification in progress...")

      req(input$file)

      # Création de l'objet LogisticRegression
      incProgress(0.3, detail = "Model creation in progress...")

      tryCatch({
        rv$model <- LogisticRegression$new(
          file_path = input$file$datapath,
          delimiter = input$delimiter
        )

        # Récupération des colonnes catégorielles
        cat_cols_names <- rv$model$cat_cols_names

        # Mise à jour de la sélection de la variable cible
        updateSelectInput(session, "target", choices = cat_cols_names)

        # Calcul de la meilleure variable cible
        best_target <- rv$model$auto_select_target()

        # Sélection automatique de la variable cible
        updateSelectInput(session, "target", selected = best_target)

        # Récupération des colonnes
        cols_names <- rv$model$cols_names

        # Mise à jour de la sélection des colonnes à supprimer
        updateSelectInput(session, "remove_cols", choices = cols_names, selected = NULL)

        output$output <- renderText("[INFO] The data has been successfully configured.")
        output$missing_info <- renderText({
          paste0(
            "[DATA INFO] Number of missing values : ", rv$model$missing_values, "\n",
            "[DATA INFO] Percentage of missing values : ", round(rv$model$missing_values_percent, 2), " %"
          )
        })

        # Activation de l'interface
        enable("handle_missing_num_method")
        enable("handle_missing_cat_method")
        enable("handle_missing")
        enable("auto_target")
        enable("auto_delete_cols")
        enable("test_size")
        enable("prepare_data")
        
        incProgress(1, detail = "Success")
      }, error = function(e) {
        # Désactivation de l'interface
        disable("handle_missing_num_method")
        disable("handle_missing_cat_method")
        disable("handle_missing")
        disable("auto_target")
        disable("target")
        disable("auto_delete_cols")
        disable("remove_cols")
        disable("test_size")
        disable("prepare_data")
        disable("learning_rate")
        disable("max_iter")
        disable("batch_size")
        disable("tol")
        disable("fit_model")
        disable("predict")
        rv$model <- NULL
        rv$predictions <- NULL

        output$output <- renderText(paste("[ERROR]", e$message))
        incProgress(1, detail = "Error")
      })
    })
  })
  
  # Gestion des valeurs manquantes
  observeEvent(input$handle_missing, {
    withProgress(message = 'Missing values handling > ', value = 0, {
      
      # Réinitialisation de l'interface
      incProgress(0.2, detail = "Interface updating in progress...")

      disable("learning_rate")
      disable("max_iter")
      disable("batch_size")
      disable("tol")
      disable("fit_model")
      disable("predict")

      # Récupération des méthodes sélectionnées par l'utilisateur
      incProgress(0.2, detail = "Retrieving selected methods in progress...")

      selected_num_method <- input$handle_missing_num_method
      selected_cat_method <- input$handle_missing_cat_method
      
      # Vérification que les méthodes sont bien sélectionnées
      incProgress(0.2, detail = "Method verification in progress...")

      req(selected_num_method, selected_cat_method)
      
      # Gestion des valeurs manquantes avec la méthode sélectionnée
      incProgress(0.2, detail = "Missing values handling in progress...")

      tryCatch({
        if (!(selected_num_method == "none" && selected_cat_method == "none")) {
          rv$model$handle_missing_values(num_method = selected_num_method, cat_method = selected_cat_method)
          if (selected_num_method != "none" && selected_cat_method != "none") {
            output$output <- renderText(paste("[INFO] The missing values have been successfully handled using the", selected_num_method, "method for numeric values and the", selected_cat_method, "method for categorical values."))
          } else if (selected_num_method != "none") {
            output$output <- renderText(paste("[INFO] The missing values have been successfully handled using the", selected_num_method, "method for numeric values."))
          } else if (selected_cat_method != "none") {
            output$output <- renderText(paste("[INFO] The missing values have been successfully handled using the", selected_cat_method, "method for categorical values."))
          }
          output$missing_info <- renderText({
            paste0(
              "[DATA INFO] Number of missing values : ", rv$model$missing_values, "\n",
              "[DATA INFO] Percentage of missing values : ", round(rv$model$missing_values_percent, 2), " %"
            )
          })
          rv$trigger <- rv$trigger + 1
        } else {
          output$output <- renderText("[WARNING] Please choose at least one method to handle missing values.")
        }

        incProgress(1, detail = "Success")
      }, error = function(e) {
        output$output <- renderText(paste("[ERROR]", e$message))
        incProgress(1, detail = "Error")
      })
    })
  })

  # Sélection automatique de la variable cible
  observeEvent(input$auto_target, {

    # Vérification de l'existence du modèle
    req(rv$model)

    if (input$auto_target) {
      disable("target")
      
      # Calcul de la meilleure variable cible
      best_target <- rv$model$auto_select_target()
      
      # Sélection automatique de la variable cible
      updateSelectInput(session, "target", selected = best_target)
      
      output$output <- renderText(paste("[INFO] The target variable", best_target, "has been automatically selected."))
    } else {
      enable("target")
      
      output$output <- renderText("[INFO] Please select the target variable.")
    }
  })

  # Sélection automatique des colonnes à supprimer
  observeEvent(input$auto_delete_cols, {

      # Vérification de l'existence du modèle
      req(rv$model)

      if (input$auto_delete_cols) {
        withProgress(message = 'Auto column suppression > ', value = 0, {
          disable("remove_cols")
          
          # Calcul des colonnes à supprimer
          incProgress(0.5, detail = "Calculating columns to be removed in progress...")

          cols_to_remove <- rv$model$auto_remove_columns()

          # Sélection automatique des colonnes à supprimer
          incProgress(0.2, detail = "Auto selection of columns to be removed in progress...")

          updateSelectInput(session, "remove_cols", selected = cols_to_remove)

          if(is.null(cols_to_remove)) {
            output$output <- renderText(paste("[INFO] No columns need to be removed."))
          } else {
            output$output <- renderText(paste("[INFO] The columns", paste(cols_to_remove, collapse = ", "), "have been automatically selected for deletion."))
          }

          incProgress(1, detail = "Success")
        })
      } else {
        enable("remove_cols")
        
        output$output <- renderText("[INFO] Please select the columns to remove.")
      }
    
  })
  
  # Préparation des données
  observeEvent(input$prepare_data, {
    withProgress(message = 'Data preparation > ', value = 0, {

      # Réinitialisation de l'interface
      incProgress(0.2, detail = "Interface updating in progress...")

      disable("learning_rate")
      disable("max_iter")
      disable("batch_size")
      disable("tol")
      disable("fit_model")
      disable("predict")
      rv$predictions <- NULL

      # Obtention des colonnes à supprimer
      remove_cols <- if (is.null(input$remove_cols)) {
        character(0)
      } else {
        input$remove_cols
      }

      # Récupération de la taille de l'échantillon de test sélectionnée par l'utilisateur
      incProgress(0.2, detail = "Retrieving selected test size in progress...")

      test_size <- input$test_size

      # Vérification que la taille de l'échantillon de test est bien sélectionnée
      incProgress(0.2, detail = "Test size verification in progress...")

      req(test_size)

      # Préparation des données avec le paramètre fourni
      incProgress(0.2, detail = "Data preparation in progress...")
      tryCatch({
        rv$model$prepare_data(target = input$target, columns_to_remove = remove_cols, test_size = test_size)

        # Activation de l'interface
        enable("learning_rate")
        enable("max_iter")
        enable("batch_size")
        enable("tol")
        enable("fit_model")

        output$output <- renderText("[INFO] The data has been successfully prepared.")
        incProgress(1, detail = "Success")
      }, error = function(e) {
        # Désactivation de l'interface
        disable("learning_rate")
        disable("max_iter")
        disable("batch_size")
        disable("tol")
        disable("fit_model")
        disable("predict")
        rv$predictions <- NULL

        output$output <- renderText(paste("[ERROR]", e$message))
        incProgress(1, detail = "Error")
      })
    })
  })
  
  # Ajustement du modèle
  observeEvent(input$fit_model, {
    withProgress(message = 'Model fitting > ', value = 0, {

      # Réinitialisation de l'interface
      incProgress(0.2, detail = "Interface updating in progress...")

      disable("predict")
      rv$predictions <- NULL

      # Récupération des paramètres du modèle
      incProgress(0.2, detail = "Retrieving model parameters in progress...")

      learning_rate <- input$learning_rate
      max_iter <- input$max_iter
      batch_size <- input$batch_size
      tol <- input$tol

      # Vérification des paramètres du modèle
      incProgress(0.1, detail = "Model parameters verification in progress...")

      req(learning_rate, max_iter, batch_size, tol)

      # Validation des paramètres
      incProgress(0.1, detail = "Model parameters validation in progress...")
      if (is.null(learning_rate) || learning_rate <= 0) {
        output$output <- renderText("[WARNING] The learning rate must be a positive number.")
        return(NULL)
      }
      
      if (is.null(max_iter) || max_iter < 1) {
        output$output <- renderText("[WARNING] The maximum number of iterations must be a positive integer.")
        return(NULL)
      }

      if (is.null(batch_size) || batch_size < 1) {
        output$output <- renderText("[WARNING] The batch size must be a positive integer.")
        return(NULL)
      }
      
      if (is.null(tol) || tol <= 0) {
        output$output <- renderText("[WARNING] The tolerance must be a positive number.")
        return(NULL)
      }

      # Ajustement du modèle avec les paramètres fournis
      incProgress(0.2, detail = "Model fitting in progress...")
      tryCatch({
        rv$model$fit(learning_rate = learning_rate, max_iter = max_iter, batch_size = batch_size, tol = tol)
        
        # Activation du bouton prédiction
        enable("predict")

        output$output <- renderText("[INFO] The model has been successfully fitted.")
        incProgress(1, detail = "Success")
      }, error = function(e) {
        # Désactivation de l'interface
        disable("predict")
        rv$predictions <- NULL

        output$output <- renderText(paste("[ERROR]", e$message))
        incProgress(1, detail = "Error")
      })
    })
  })
  
  # Prédiction des classes
  observeEvent(input$predict, {
    withProgress(message = 'Predicting classes > ', value = 0, {
      incProgress(0.5, detail = "Prediction in progress...")
      tryCatch({
        # Prédiction des classes et récupération de l'accuracy
        accuracy <- rv$model$predict()
        output$output <- renderText(paste("[INFO] The data has been successfully predicted with an accuracy of", round(accuracy * 100, 2), " %."))
        rv$predictions <- rv$model$predicted_targets
        rv$accuracy <- accuracy
        incProgress(1, detail = "Success")
      }, error = function(e) {
        output$output <- renderText(paste("[ERROR]", e$message))
        rv$predictions <- NULL
        rv$accuracy <- NULL
        incProgress(1, detail = "Error")
      })
    })
  })

  # Affichage des données
  output$data_preview <- DT::renderDataTable({
    req(rv$model)
    rv$trigger
    rv$model$data
    }, options = list(
    server = TRUE,
    pageLength = 10,
    autoWidth = TRUE,
    scrollX = TRUE
  ))

  # Affichage des prédictions
  output$predictions <- DT::renderDataTable({
    req(rv$predictions)
    rv$predictions
  }, options = list(
    server = TRUE,
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