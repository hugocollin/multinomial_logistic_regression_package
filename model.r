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
      
      self$data <- data
      self$target <- target
      self$levels_map <- list()
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
    },
    
    # Fonction de préparation des données
    prepare_data = function(test_size, scale=TRUE) {
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
      
      # Normalisation des variables numériques
      if (scale) {
        numeric_vars <- sapply(X, is.numeric)
        X[, numeric_vars] <- scale(X[, numeric_vars])
      }
      
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
    
    
    fit = function(learning_rate = 0.001, max_iter = 1000, batch_size = 68, tol = 1e-6) {
      y <- self$y_train
      X <- self$X_train
      
      # Ajout de l'intercept
      X <- cbind(1, as.matrix(X))  # Dimensions : (n_samples, n_features + 1)
      
      # Initialisation des coefficients
      num_classes <- length(unique(y))  # Nombre de classes
      coefficients <- matrix(rnorm(ncol(X) * num_classes, mean = 0, sd = 0.01), 
                             nrow = ncol(X), ncol = num_classes)
      epsilon <- 1e-15
      
      # Encodage en one-hot
      y_one_hot <- matrix(0, nrow = length(y), ncol = num_classes)
      for (i in 1:length(y)) {
        y_one_hot[i, y[i] + 1] <- 1  # Assurez-vous que les classes commencent à 0
      }
      
      # Variables de convergence
      prev_log_likelihood <- -Inf
      
      for (i in 1:max_iter) {
        indices <- sample(1:nrow(X))
        for (batch_start in seq(1, nrow(X), by = batch_size)) {
          batch_end <- min(batch_start + batch_size - 1, nrow(X))
          batch_indices <- indices[batch_start:batch_end]
          
          X_batch <- X[batch_indices, , drop = FALSE]  # Dimensions : (batch_size, n_features + 1)
          y_batch <- y_one_hot[batch_indices, ]  # Dimensions : (batch_size, num_classes)
          
          # Calcul des scores et softmax
          scores <- X_batch %*% coefficients  # Dimensions : (batch_size, num_classes)
          softmax_probs <- exp(scores - apply(scores, 1, max))
          softmax_probs <- softmax_probs / rowSums(softmax_probs)
          
          # Calcul du gradient
          gradient <- t(X_batch) %*% (softmax_probs - y_batch) / nrow(X_batch)  # Dimensions : (n_features + 1, num_classes)
          
          # Mise à jour des coefficients
          coefficients <- coefficients - learning_rate * gradient
        }
        
        # Log-vraisemblance
        scores <- X %*% coefficients  # Dimensions : (n_samples, num_classes)
        softmax_probs <- exp(scores - apply(scores, 1, max))
        softmax_probs <- softmax_probs / rowSums(softmax_probs)
        log_likelihood <- sum(y_one_hot * log(pmax(softmax_probs, epsilon)))
        
        if (is.na(log_likelihood) || is.infinite(log_likelihood)) {
          cat("Log-Likelihood invalide, arrêt de l'entraînement.\n")
          break
        }
        
        if (abs(log_likelihood - prev_log_likelihood) < tol) {
          cat("Convergence atteinte à l'itération", i, "avec la log-vraisemblance :", log_likelihood, "\n")
          break
        }
        
        prev_log_likelihood <- log_likelihood
        
        if (i %% 100 == 0) {
          cat("Iteration:", i, "Log-Likelihood:", log_likelihood, "\n")
        }
      }
      
      self$coefficients <- coefficients
    },
    
    
    
    # Fonction predict : Prédiction des classes
    predict = function() {
      # Vérification des données de test
      if (is.null(self$X_test) || is.null(self$coefficients)) {
        stop("Les données de test ou les coefficients ne sont pas disponibles.")
      }
      
      # Récupération des données de test
      X <- self$X_test
      
      # Ajout de l'intercept
      X <- cbind(1, as.matrix(X))
      
      # Vérification des dimensions
      if (ncol(X) != nrow(self$coefficients)) {
        stop("Les dimensions de X et des coefficients ne correspondent pas.")
      }
      
      # Prédiction des scores pour chaque classe
      scores <- X %*% self$coefficients  # Matrice de scores pour chaque classe
      
      # Application de Softmax pour obtenir les probabilités
      exp_scores <- exp(scores - apply(scores, 1, max))  # Stabilisation numérique
      softmax_probs <- exp_scores / rowSums(exp_scores)  # Normalisation Softmax
      
      # Prédiction des classes (classe ayant la probabilité maximale)
      predicted_classes <- apply(softmax_probs, 1, which.max) - 1  # Décalage pour correspondre à l'index 0
      
      # Mapping aux labels originaux (si fourni)
      if (!is.null(self$class_labels)) {
        self$predicted_targets <- self$class_labels[predicted_classes + 1]  # Ajuster pour correspondre aux labels
      } else {
        self$predicted_targets <- predicted_classes  # Sans mapping
      }
      
      # Affichage des probabilités pour les premières 10 prédictions
      cat("Premières 10 probabilités de prédiction :\n")
      print(head(softmax_probs, 10))
      
      # Calcul de la précision
      if (!is.null(self$y_test)) {
        accuracy <- mean(predicted_classes == self$y_test)
        cat("Précision : ", accuracy, "\n")
      } else {
        accuracy <- NA
        cat("Aucune donnée d'évaluation fournie pour calculer la précision.\n")
      }
      
      # Retourner les résultats
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