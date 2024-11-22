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
    predicted_targets = NULL,
    coefficients = NULL,
    levels_map = NULL,
    class_labels = NULL,
    class_frequencies = NULL,

    # Constructeur de la classe
    initialize = function(file_path, data, target, columns_to_remove, delimiter) {
      # Chargement d'un fichier CSV
      if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        # Lecture du fichier avec le délimiteur spécifié
        tryCatch({
          data <- read_delim(file_path, delim = delimiter, col_types = cols(), progress = FALSE)
        }, error = function(e) {
          stop(paste("[Attention] Échec du chargement du fichier CSV avec le délimiteur '", delimiter, "'.", sep = ""))
        })
        
        # Vérification de la cohérence des colonnes
        lines <- read_lines(file_path)
        lines <- trimws(lines)
        lines <- lines[nchar(lines) > 0]
        split_lines <- strsplit(lines, delimiter, fixed = TRUE)
        num_cols <- length(split_lines[[1]])
        inconsistant_rows <- which(sapply(split_lines, length) != num_cols)

        if (num_cols < 2) {
          stop("[Attention] Échec du chargement du fichier CSV avec le délimiteur '", delimiter, "'.", sep = "")
        }
        
        if (length(inconsistant_rows) > 0) {
          problematic_lines <- lines[inconsistant_rows]
          final_text <- ifelse(length(inconsistant_rows) == 1, "à la ligne", "aux lignes")
          stop(paste0("[Attention] Incohérence détectée dans le nombre de colonnes du fichier CSV ",
                      final_text, " : ",
                      paste(inconsistant_rows, collapse = ", "), "."))
        }
      
      # Chargement d'un fichier Excel
      } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
        data <- read_excel(file_path)
      } else {
        stop("[Attention] Format de fichier non supporté, utilisez un fichier .csv ou .xlsx.")
      }
      
      # Vérification de la colonne cible
      if (!target %in% colnames(data)) {
        stop("[Attention] La variable cible n'existe pas dans le jeu de données.")
      }
      
      # Vérification des colonnes à supprimer
      if (!is.null(columns_to_remove)) {
        missing_cols <- setdiff(columns_to_remove, colnames(data))
        if (length(missing_cols) > 0) {
          stop(paste0("[Attention] Les colonnes suivantes à supprimer n'existent pas dans le jeu de données : ",
                      paste(missing_cols, collapse = ", "), "."))
        }
        data <- data[, !(colnames(data) %in% columns_to_remove), drop = FALSE]
      }
      
      # Définition des colonnes de prédiction
      self$predictors <- setdiff(colnames(data), c(target, columns_to_remove))
      
      # Vérification qu'il y a au moins une colonne prédictive
      if (length(self$predictors) == 0) {
        stop("[Attention] Aucun prédicteur disponible après suppression des colonnes spécifiées.")
      }
      
      self$data <- data
      self$target <- target
      self$levels_map <- list()

      print("[INFO] Les données ont été chargées avec succès.")
    },

    # Fonction de gestion des valeurs manquantes
    handle_missing_values = function(method = c("mean", "median", "mode", "remove")) {
      method <- match.arg(method)
      data <- self$data
      
      for (var in colnames(data)) {
        if (any(is.na(data[[var]]))) {
          # Gestion des valeurs manquantes pour les variables numériques
          if (is.numeric(data[[var]])) {
            if (method == "mean") {
              data[[var]][is.na(data[[var]])] <- mean(data[[var]], na.rm = TRUE)
            } else if (method == "median") {
              data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
            } else if (method == "mode") {
              mode_value <- as.numeric(names(sort(table(data[[var]]), decreasing = TRUE)[1]))
              data[[var]][is.na(data[[var]])] <- mode_value
            } else if (method == "remove") {
              data <- data[complete.cases(data), ]
            }
          # Gestion des valeurs manquantes pour les variables catégorielles
          } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
            if (method == "mode") {
              mode_value <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
              data[[var]][is.na(data[[var]])] <- mode_value
            } else if (method == "remove") {
              data <- data[!is.na(data[[var]]), ]
            }
          }
        }
      }
      
      self$data <- data

      print("[INFO] Les valeurs manquantes ont été gérées avec succès.")
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

      print("[INFO] Les données ont été préparées avec succès.")
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

      print("[INFO] Le modèle a été ajusté avec succès.")
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

      print("[INFO] Les données ont été prédites avec succès.")

      # Calcul de la précision
      accuracy <- mean(predicted_targets == self$y_test)

      return(accuracy)
    }
    
    # # Méthode predict_proba : Prédiction des probabilités
    # predict_proba = function(X) {
    #   X <- cbind(1, as.matrix(X))  # Ajout de l'intercept
    #   probs <- 1 / (1 + exp(-X %*% self$coefficients))
    #   return(data.frame(Class1 = 1 - probs, Class2 = probs))
    # }
  )
)

# Chargement des données
model <- LogisticRegression$new(
  file_path = "test.csv",
  target = "target",
  columns_to_remove = c("var3"),
  delimiter = "|"
)

# Gestion des valeurs manquantes
model$handle_missing_values(method = "mean")

# Préparation des données
model$prepare_data(test_size = 0.3)

# Création et ajustement du modèle
model$fit(learning_rate = 0.01, max_iter = 1000)
print(model$coefficients) # [TEMP]
print(model) # [TEMP]

# Prédiction des classes
accuracy <- model$predict()
print(paste("Précision du modèle :", round(accuracy * 100, 2), "%"))
print(model$predicted_targets) # [TEMP]