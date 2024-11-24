# Importation des bibliothèques nécessaires
library(R6)
library(readr)
library(readxl)

# Définition de la classe LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    data = NULL,
    prepared_data = NULL,
    X = NULL,
    y = NULL,
    n = NULL,
    X_train = NULL,
    y_train = NULL,
    X_test = NULL,
    y_test = NULL,
    predictors = NULL,
    target = NULL,
    missing_values = NULL,
    missing_values_percent = NULL,
    predicted_targets = NULL,
    accuracy = NULL,
    coefficients = NULL,
    levels_map = NULL,
    class_labels = NULL,
    class_frequencies = NULL,

    # Constructeur de la classe
    initialize = function(file_path, target, columns_to_remove, delimiter) {
      # Chargement d'un fichier CSV
      if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        # Lecture du fichier avec le délimiteur spécifié
        tryCatch({
          data <- read_delim(file_path, delim = delimiter, col_types = cols(), progress = FALSE)
        }, error = function(e) {
          stop(paste("[Warning] Failed to load the CSV file with the delimiter '", delimiter, "'.", sep = ""))
        })
        
        # Vérification de la cohérence des colonnes
        lines <- read_lines(file_path)
        lines <- trimws(lines)
        lines <- lines[nchar(lines) > 0]
        split_lines <- strsplit(lines, delimiter, fixed = TRUE)
        num_cols <- length(split_lines[[1]])
        inconsistant_rows <- which(sapply(split_lines, length) != num_cols)

        if (num_cols < 2) {
          stop(paste("[Warning] Failed to load the CSV file with the delimiter '", delimiter, "'.", sep = ""))
        }
        
        if (length(inconsistant_rows) > 0) {
          problematic_lines <- lines[inconsistant_rows]
          final_text <- ifelse(length(inconsistant_rows) == 1, "à la ligne", "aux lignes")
          stop(paste0("[Warning] Discrepancy detected in the number of columns in the CSV file.",
                      final_text, " : ",
                      paste(inconsistant_rows, collapse = ", "), "."))
        }
      
      # Chargement d'un fichier Excel
      } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
        data <- read_excel(file_path)
      }
      
      # Vérification des colonnes à supprimer
      if (!is.null(columns_to_remove)) {
        data <- data[, !(colnames(data) %in% columns_to_remove), drop = FALSE]
      }
      
      # Définition des colonnes de prédiction
      self$predictors <- setdiff(colnames(data), c(target, columns_to_remove))
      
      # Vérification de la cohérence de la suppression des colonnes
      if (length(self$predictors) == 0) {
        stop(paste("[Warning] You cannot remove all columns."))
      }
      if (!(target %in% colnames(data))) {
        stop(paste("[Warning] You cannot remove the target column."))
      }

      # Calcul du nombre de valeurs manquantes et du pourcentage
      self$missing_values <- sum(is.na(data))
      self$missing_values_percent <- self$missing_values / (nrow(data) * ncol(data)) * 100
      
      self$data <- data
      self$target <- target
      self$levels_map <- list()
    },

    # Fonction de gestion des valeurs manquantes
    handle_missing_values = function(num_method = c("none", "mean", "median", "mode", "remove"), cat_method = c("none", "mode", "remove")) {
      num_method <- match.arg(num_method)
      cat_method <- match.arg(cat_method)
      data <- self$data
      
      for (var in colnames(data)) {
        if (any(is.na(data[[var]]))) {
          # Gestion des valeurs manquantes pour les variables numériques
          if (is.numeric(data[[var]])) {
            if (num_method == "none") {
              next
            } else if (num_method == "mean") {
              data[[var]][is.na(data[[var]])] <- mean(data[[var]], na.rm = TRUE)
            } else if (num_method == "median") {
              data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
            } else if (num_method == "mode") {
              mode_value <- as.numeric(names(sort(table(data[[var]]), decreasing = TRUE)[1]))
              data[[var]][is.na(data[[var]])] <- mode_value
            } else if (num_method == "remove") {
              data <- data[complete.cases(data), ]
            }
          # Gestion des valeurs manquantes pour les variables catégorielles
          } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
            if (cat_method == "none") {
              next
            } else if (cat_method == "mode") {
              mode_value <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
              data[[var]][is.na(data[[var]])] <- mode_value
            } else if (cat_method == "remove") {
              data <- data[!is.na(data[[var]]), ]
            }
          }
        }
      }
      
      # Calcul du nombre de valeurs manquantes et du pourcentage
      self$missing_values <- sum(is.na(data))
      self$missing_values_percent <- self$missing_values / (nrow(data) * ncol(data)) * 100
      
      self$data <- data
    },
    
    # Fonction de préparation des données
    prepare_data = function(test_size) {
      data <- self$data
      
      # Encodage de la variable cible (facteur -> indices numériques)
      if (!is.factor(data[[self$target]])) {
        data[[self$target]] <- as.factor(data[[self$target]])
      }
      self$levels_map[[self$target]] <- levels(data[[self$target]])
      

      # Conservation des niveaux avant conversion
      y_factor <- data[[self$target]]
      data[[self$target]] <- as.numeric(y_factor) - 1
      
      # Gestion des variables prédictives
      new_columns <- list()
      for (var in self$predictors) {
        if (is.factor(data[[var]]) || is.character(data[[var]])) {
          # Encodage one-hot pour les variables catégoriques
          levels_var <- unique(data[[var]])
          for (lvl in levels_var) {
            col_name <- paste0(var, "=", lvl)
            new_columns[[col_name]] <- as.numeric(data[[var]] == lvl)
          }
          data[[var]] <- NULL
        }
      }
      
      # Ajout des nouvelles colonnes
      for (col_name in names(new_columns)) {
        data[[col_name]] <- new_columns[[col_name]]
      }
      
      # Séparation de la variable cible et des prédicteurs
      y <- data[[self$target]]
      X <- data[, !colnames(data) %in% self$target, drop = FALSE]
      n <- nrow(X)
      
      # Séparation des données en ensembles d'entraînement et de test
      set.seed(123)
      indices <- sample(1:n, size = floor(n * (1 - test_size)))
      X_train <- X[indices, ]
      y_train <- y[indices]
      X_test <- X[-indices, ]
      y_test <- y[-indices]
      
      self$prepared_data <- data
      self$X <- X
      self$y <- y
      self$n <- n
      self$X_train <- X_train
      self$y_train <- y_train
      self$X_test <- X_test
      self$y_test <- y_test
    },

    # Fonction fit : Ajustement du modèle
    fit = function(learning_rate, max_iter) {
      # Récupération des données préparées
      y <- self$y_train
      X <- self$X_train

      # Initialisation des attributs du modèle
      self$class_labels <- self$levels_map[[self$target]]
      self$class_frequencies <- table(y) / length(y)
      
      # Ajout de l'intercept
      X <- cbind(1, as.matrix(X))
      
      # Initialisation des coefficients
      coefficients <- matrix(0, ncol = 1, nrow = ncol(X))
      
      # Calcul de la descente de gradient
      for (i in 1:max_iter) {
        predictions <- 1 / (1 + exp(-X %*% coefficients))
        gradient <- t(X) %*% (predictions - y)
        coefficients <- coefficients - learning_rate * gradient
      }
      
      self$coefficients <- coefficients
    },

    # Fonction predict : Prédiction des classes
    predict = function() {
      # Récupération des données de test
      X <- self$X_test

      # Ajout de l'intercept
      X <- cbind(1, as.matrix(X))

      # Prédiction des probabilités
      probs <- 1 / (1 + exp(-X %*% self$coefficients))

      # Prédiction des classes
      predicted_targets <- ifelse(probs > 0.5, self$class_labels[2], self$class_labels[1])

      self$predicted_targets <- predicted_targets

      # Calcul de la précision
      self$accuracy <- mean(predicted_targets == self$y_test)

      return(self$accuracy)
    }
    
    # # Méthode predict_proba : Prédiction des probabilités
    # predict_proba = function(X) {
    #   X <- cbind(1, as.matrix(X))  # Ajout de l'intercept
    #   probs <- 1 / (1 + exp(-X %*% self$coefficients))
    #   return(data.frame(Class1 = 1 - probs, Class2 = probs))
    # }
  )
)