# Importation des bibliothèques nécessaires
library(R6)
library(readr)
library(readxl)

# Définition de la classe LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    data = NULL,
    missing_values = NULL,
    missing_values_percent = NULL,
    cols_names = NULL,
    cat_cols_names = NULL,
    target = NULL,
    predictors = NULL,
    prepared_data = NULL,
    X = NULL,
    y = NULL,
    n = NULL,
    X_train = NULL,
    y_train = NULL,
    X_test = NULL,
    y_test = NULL,
    predicted_targets = NULL,
    accuracy = NULL,
    coefficients = NULL,
    levels_map = NULL,
    class_labels = NULL,
    class_frequencies = NULL,

    # Constructeur de la classe
    initialize = function(file_path, delimiter) {
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
        lines <- ifelse(grepl(paste0(delimiter, "$"), lines), paste0(lines, NaN), lines)
        split_lines <- strsplit(lines, delimiter, fixed = TRUE)
        num_cols <- length(split_lines[[1]])
        if (num_cols < 2) {
          stop(paste("[Warning] Failed to load the CSV file with the delimiter '", delimiter, "'.", sep = ""))
        }
        count_columns <- function(line) {
          return(length(line))
        }
        num_cols_per_line <- sapply(split_lines, count_columns)
        inconsistant_rows <- which(num_cols_per_line != num_cols)
        if (length(inconsistant_rows) > 0) {
          problematic_lines <- inconsistant_rows
          stop(paste0("[Warning] Discrepancy detected in the number of columns in the CSV file at lines : ",
                      paste(problematic_lines, collapse = ", "), "."))
        }
      
      # Chargement d'un fichier Excel
      } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
        data <- read_excel(file_path)
      }

      # Calcul du nombre de valeurs manquantes et du pourcentage
      self$missing_values <- sum(is.na(data))
      self$missing_values_percent <- self$missing_values / (nrow(data) * ncol(data)) * 100

      # Récupération du nom des colonnes
      self$cols_names <- colnames(data)

      # Récupération des colonnes catégorielles
      self$cat_cols_names <- colnames(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
      
      self$data <- data
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

    # Fonction de sélection automatique de la variable cible
    auto_select_target = function(entropy_threshold = 0.5, correlation_threshold = 0.5, weight_entropy = 0.5, weight_correlation = 0.5) {
      data <- self$data
      cat_cols_names <- self$cat_cols_names

      # Filtrage des colonnes avec au moins 2 catégories et moins de 10% de valeurs manquantes
      cat_cols_names <- cat_cols_names[sapply(data[cat_cols_names], function(col) length(unique(col)) > 1)]
      cat_cols_names <- cat_cols_names[sapply(data[cat_cols_names], function(col) mean(is.na(col)) <= 0.1)]

      if (length(cat_cols_names) == 0) {
        stop(paste("[WARNING] No available categorical column found for the target."))
      }

      # Calcul de l'entropie
      class_entropy <- sapply(data[cat_cols_names], compute_entropy)

      # Exclusion des colonnes avec une entropie inférieure au seuil
      cat_cols_names <- cat_cols_names[class_entropy >= entropy_threshold]

      if (length(cat_cols_names) == 0) {
        stop(paste("[WARNING] No categorical column with sufficient entropy found."))
      }

      # Calcul de la corrélation de Cramer de manière vectorisée
      cramers_v_matrix <- outer(cat_cols_names, cat_cols_names, Vectorize(function(x, y) {
        if(x == y) {
          return(1)
        } else {
          return(cramers_v(data[[x]], data[[y]]))
        }
      }))

      diag(cramers_v_matrix) <- 1
      mean_correlations <- rowMeans(cramers_v_matrix, na.rm = TRUE)

      # Normalisation des scores
      norm_entropy <- (class_entropy[cat_cols_names] - min(class_entropy[cat_cols_names])) / 
                    (max(class_entropy[cat_cols_names]) - min(class_entropy[cat_cols_names]))
      norm_correlation <- 1 - (mean_correlations / max(mean_correlations))

      # Combinaison des scores
      combined_score <- weight_entropy * norm_entropy + weight_correlation * norm_correlation

      # Sélection des colonnes avec une corrélation moyenne inférieure au seuil
      selected_cols <- cat_cols_names[mean_correlations < correlation_threshold]

      # Pondération des critères
      if (length(selected_cols) > 0) {
        best_target <- selected_cols[which.max(combined_score[selected_cols])]
      } else {
        best_target <- cat_cols_names[which.max(combined_score)]
      }

      return(best_target)
    },

    # Fonction de suppression automatique des colonnes inutiles
    auto_remove_columns = function(correlation_threshold = 0.9) {
      data <- self$data
      cols_names <- self$cols_names

      # Calcul de la corrélation de Cramer de manière vectorisée
      cramers_v_matrix <- outer(cols_names, cols_names, Vectorize(function(x, y) {
        if(x == y) {
          return(1)
        } else {
          return(cramers_v(data[[x]], data[[y]]))
        }
      }))

      # Ajustement des valeurs diagonales
      diag(cramers_v_matrix) <- 1

      # Moyenne des corrélations pour chaque colonne
      mean_correlations <- rowMeans(cramers_v_matrix, na.rm = TRUE)

      # Identification des colonnes à supprimer
      columns_to_remove <- cols_names[abs(mean_correlations) > correlation_threshold]

      if (length(columns_to_remove) == 0) {
        return(NULL)
      } else {
        return(columns_to_remove)
      }
    },
    
    # Fonction de préparation des données
    prepare_data = function(target, columns_to_remove, test_size) {
      data <- self$data
      self$target <- target

      # Définition des colonnes de prédiction,
      self$predictors <- setdiff(colnames(data), c(target, columns_to_remove))
      
      # Vérification de la cohérence de la suppression des colonnes
      if (length(self$predictors) == 0) {
        stop(paste("[WARNING] You cannot remove all columns."))
      }
      # Vérification si la colonne cible est dans les colonnes à supprimer
      if (target %in% columns_to_remove) {
        stop(paste("[WARNING] The target column cannot be removed."))
      }

      # Vérification des colonnes à supprimer
      if (!is.null(columns_to_remove)) {
        data <- data[, !(colnames(data) %in% columns_to_remove), drop = FALSE]
      }
      
      # Encodage de la variable cible (facteur -> indices numériques)
      if (!is.factor(data[[target]])) {
        data[[target]] <- as.factor(data[[target]])
      }
      self$levels_map[[target]] <- levels(data[[target]])

      # Conservation des niveaux avant conversion
      y_factor <- data[[target]]
      data[[target]] <- as.numeric(y_factor) - 1
      
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
      y <- data[[target]]
      X <- data[, !colnames(data) %in% target, drop = FALSE]
      n <- nrow(X)
      
      # Normalisation des variables numériques
      numeric_vars <- sapply(X, is.numeric)
      X[, numeric_vars] <- scale(X[, numeric_vars])
      
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
    fit = function(learning_rate, max_iter, batch_size, tol) {
      y <- self$y_train
      X <- self$X_train

      # Initialisation des attributs du modèle
      self$class_labels <- self$levels_map[[self$target]]
      self$class_frequencies <- table(y) / length(y)
      
      # Ajout de l'intercept
      X <- cbind(1, as.matrix(X))
      
      # Initialisation des coefficients
      num_classes <- length(unique(y))
      coefficients <- matrix(rnorm(ncol(X) * num_classes, mean = 0, sd = 0.01), 
                             nrow = ncol(X), ncol = num_classes)
      epsilon <- 1e-15
      
      # Encodage en one-hot
      y_factor <- as.factor(y)
      y_one_hot <- model.matrix(~ y_factor - 1)
      
      # Variables de convergence
      prev_log_likelihood <- -Inf
      
      for (i in 1:max_iter) {
        indices <- sample(1:nrow(X))
        for (batch_start in seq(1, nrow(X), by = batch_size)) {
          batch_end <- min(batch_start + batch_size - 1, nrow(X))
          batch_indices <- indices[batch_start:batch_end]
          
          X_batch <- X[batch_indices, , drop = FALSE]
          y_batch <- y_one_hot[batch_indices, ]
          
          # Calcul des scores et softmax
          scores <- X_batch %*% coefficients
          softmax_probs <- exp(scores - apply(scores, 1, max))
          softmax_probs <- softmax_probs / rowSums(softmax_probs)
          
          # Calcul du gradient
          gradient <- t(X_batch) %*% (softmax_probs - y_batch) / nrow(X_batch)
          
          # Mise à jour des coefficients
          coefficients <- coefficients - learning_rate * gradient
        }
        
        # Log-vraisemblance
        scores <- X %*% coefficients
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
      # Récupération des données de test
      X <- self$X_test
      
      # Ajout de l'intercept
      X <- cbind(1, as.matrix(X))
      
      # Prédiction des scores pour chaque classe
      scores <- X %*% self$coefficients
      
      # Application de Softmax pour obtenir les probabilités
      exp_scores <- exp(scores - apply(scores, 1, max))
      softmax_probs <- exp_scores / rowSums(exp_scores)
      
      # Prédiction des classes (classe ayant la probabilité maximale)
      predicted_classes <- apply(softmax_probs, 1, which.max) - 1
      
      
      self$predicted_targets <- predicted_classes
      # Calcul de la performance
      self.accuracy <- mean(predicted_classes == self$y_test)
      
      return(self.accuracy)
    },
    
    predictions_to_labels = function() {

      if (is.null(self$class_labels) || length(self$class_labels) == 0) {
        stop("[Error] class_labels cannot be NULL or empty.")
      }
      
      # Conversion of predicted classes to class labels
      predicted_labels <- data.frame(
        Predicted = self$class_labels[self$predicted_targets + 1],  # Map indices to labels
        stringsAsFactors = FALSE
      )
      
      return(predicted_labels)
    },
    
    
    variable_importance = function() {
      if (is.null(self$coefficients)) {
        stop("[Error] The model has not been fitted yet. Run the `fit` method first.")
      }
      
      # Extraire les coefficients sans l'intercept (1ère colonne)
      coef_matrix <- self$coefficients[-1, ]  # Exclure l'intercept
      
      # Calcul des scores d'importance
      importance_scores <- apply(abs(coef_matrix), 1, sum)  # Somme des valeurs absolues des coefficients
      
      # Normalisation des scores (optionnelle)
      importance_scores <- importance_scores / sum(importance_scores)
      
      # Associer les scores aux noms des variables
      variable_names <- colnames(self$X_train)
      importance <- data.frame(
        Variable = variable_names,
        Importance = importance_scores
      )
      
      # Trier par ordre décroissant d'importance
      importance <- importance[order(-importance$Importance), ]
      
      return(importance)
    },
    
    var_select = function(threshold = 0.05, num_vars = NULL) {
      if (is.null(self$coefficients)) {
        stop("[Error] The model has not been fitted yet. Run the `fit` method first.")
      }
      
      # Calculer l'importance des variables
      importance <- self$variable_importance()
      
      # Filtrer les variables en fonction du seuil
      if (!is.null(num_vars)) {
        # Conserver uniquement les 'num_vars' variables les plus importantes
        selected_variables <- head(importance$Variable, n = num_vars)
      } else {
        # Conserver les variables dont l'importance est >= au seuil
        selected_variables <- importance$Variable[importance$Importance >= threshold]
      }
      
      # Réduire les données aux variables sélectionnées
      self$X_train <- self$X_train[, selected_variables, drop = FALSE]
      self$X_test <- self$X_test[, selected_variables, drop = FALSE]
      
      # Mettre à jour les prédicteurs de la classe
      self$predictors <- selected_variables
      
      cat("[Info] Variables selected based on importance:\n")
      print(selected_variables)
    },
    
    
    generate_confusion_matrix = function() {
      # Convert true labels and predicted labels to vectors
      true_labels <- as.vector(self$y_test)                # Convert y_test to a vector
      predicted_labels <- as.vector(self$predicted_targets)  # Convert predicted_targets to a vector
      
      # Verify consistency of the input lengths
      if (length(true_labels) != length(predicted_labels)) {
        stop("[Error] The length of true labels and predicted labels must be the same.")
      }
      
      # Calculate the confusion matrix
      confusion_matrix <- table(True = true_labels, Predicted = predicted_labels)
      
      # Convert to data frame for easier readability (optional)
      confusion_df <- as.data.frame(as.table(confusion_matrix))
      colnames(confusion_df) <- c("True", "Predicted", "Frequency")
      
      # Calculate performance metrics
      total <- sum(confusion_matrix)                               # Total observations
      accuracy <- sum(diag(confusion_matrix)) / total              # Overall accuracy
      
      # Calculate metrics for each class
      precision <- diag(confusion_matrix) / rowSums(confusion_matrix)  # Precision for each class
      recall <- diag(confusion_matrix) / colSums(confusion_matrix)     # Recall for each class
      f1_score <- 2 * precision * recall / (precision + recall)        # F1-score for each class
      
      # Return results as a list
      results <- list(
        confusion_matrix = confusion_matrix,
        accuracy = accuracy,
        precision = precision,
        recall = recall,
        f1_score = f1_score
      )
      
      return(results)
    },
    
    
    # Méthode predict_proba : Prédiction des probabilités
    predict_proba = function() {
      
      X_input <- self$X_test
     
      # Ajout de l'intercept
      X_input <- cbind(1, as.matrix(X_input))
      
      # Calcul des scores (logits)
      scores <- X_input %*% self$coefficients
      
      # Application du softmax
      exp_scores <- exp(scores - apply(scores, 1, max))  # Eviter overflow numérique
      softmax_probs <- exp_scores / rowSums(exp_scores)
      
      # Retourner les probabilités pour chaque classe
      return(softmax_probs)
    },
    
    summary = function() {
      cat("Logistic Regression Multinomial Model - Summary\n")
      cat("---------------------------------------------------\n")
      cat("Number of Observations (Training): ", nrow(self$X_train), "\n")
      cat("Number of Observations (Testing): ", nrow(self$X_test), "\n")
      cat("Number of Predictors: ", ncol(self$X_train), "\n")
      cat("Number of Classes: ", length(self$class_labels), "\n")
      cat("Class Labels: ", paste(self$class_labels, collapse = ", "), "\n")
      cat("Class Frequencies (Training):\n")
      print(self$class_frequencies)
      cat("\n")
      
      if (!is.null(self$coefficients)) {
        cat("Coefficients (first 5):\n")
        print(head(self$coefficients, 5))
        cat("\n")
      } else {
        cat("Model not yet fitted. Coefficients not available.\n")
      }
      
      if (!is.null(self$predicted_targets)) {
        # Calculer l'accuracy sur les données de test si les prédictions existent
        accuracy <- mean(self$predicted_targets == self$y_test)
        cat("Accuracy on Test Data: ", accuracy, "\n")
      }
    },
    
    print = function() {
      cat("Logistic Regression Multinomial Model\n")
      cat("Number of Classes: ", length(self$class_labels), "\n")
      cat("Number of Predictors: ", ncol(self$X_train), "\n")
      cat("Class Labels: ", paste(self$class_labels, collapse = ", "), "\n")
      
      if (!is.null(self$coefficients)) {
        cat("Coefficients (first 5): \n")
        print(head(self$coefficients, 5))  # Afficher les 5 premiers coefficients
      } else {
        cat("Model not yet fitted.\n")
      }
    }
  )
)


# Fonction de calcul du coefficient de corrélation de Cramer
cramers_v <- function(x, y) {
  tbl <- table(x, y)                                  # Tableau de contingence
  chi2 <- suppressWarnings(chisq.test(tbl))$statistic # Statistique du test du chi2
  n <- sum(tbl)                                       # Taille de l'échantillon
  phi2 <- chi2 / n                                    # Coefficient de contingence
  r <- nrow(tbl)                                      # Nombre de lignes
  k <- ncol(tbl)                                      # Nombre de colonnes
  V <- sqrt(phi2 / min(k - 1, r - 1))                 # Coefficient de corrélation de Cramer
  return(as.numeric(V))
}

# Fonction de calcul de l'entropie des classes
compute_entropy <- function(y) {
  freq <- table(y) / length(y)              # Fréquences des classes
  entropy <- -sum(freq * log(freq + 1e-10)) # Entropie
  return(entropy)
}