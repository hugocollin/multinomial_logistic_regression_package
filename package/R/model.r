# Importation des bibliothèques nécessaires
library(R6)
library(readr)
library(readxl)
library(roxygen2)

#' LogisticRegression Class
#'
#' Implements a multinomial logistic regression model using R6.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(file_path, delimiter)}}{Initializes the model with data from a CSV or Excel file.}
#'   \item{\code{handle_missing_values(num_method, cat_method)}}{Handles missing values in the dataset.}
#'   \item{\code{auto_select_target(entropy_threshold, correlation_threshold, weight_entropy, weight_correlation)}}{Automatically selects the target variable based on entropy and correlation thresholds.}
#'   \item{\code{auto_remove_columns(correlation_threshold)}}{Automatically removes columns with high correlation.}
#'   \item{\code{prepare_data(target, columns_to_remove, test_size)}}{Prepares the data for training by defining predictors and splitting into training and testing sets.}
#'   \item{\code{fit(learning_rate, max_iter, batch_size, tol)}}{Fits the logistic regression model using gradient descent.}
#'   \item{\code{predict()}}{Predicts classes for the test set and returns accuracy.}
#'   \item{\code{predict_proba()}}{Predicts class probabilities for the test set.}
#'   \item{\code{summary()}}{Prints a summary of the model.}
#'   \item{\code{print()}}{Prints basic information about the model.}
#' }
#'
#' @docType class
#' @import R6 readr readxl
#' @export
#'
#' @examples
#' # Example usage
#' # model <- LogisticRegression$new("data.csv", ",")
#'
#' @name LogisticRegression

# Définition de la classe LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    #' @field data Data frame containing the raw data loaded from the specified file.
    data = NULL,
    
    #' @field missing_values List or vector indicating missing values in each column.
    missing_values = NULL,
    
    #' @field missing_values_percent Percentage of missing values per column.
    missing_values_percent = NULL,
    
    #' @field cols_names Character vector containing the names of all columns in the dataset.
    cols_names = NULL,
    
    #' @field cat_cols_names Character vector containing the names of categorical columns.
    cat_cols_names = NULL,
    
    #' @field target Character indicating the name of the target variable for regression.
    target = NULL,
    
    #' @field predictors Character vector containing the names of predictor variables used for the model.
    predictors = NULL,
    
    #' @field X Matrix of predictor variables used for model training.
    X = NULL,
    
    #' @field y Vector of target variable labels corresponding to the observations.
    y = NULL,
    
    #' @field n Total number of observations in the dataset.
    n = NULL,
    
    #' @field X_train Matrix of predictor variables for the training set.
    X_train = NULL,
    
    #' @field y_train Vector of target variable labels for the training set.
    y_train = NULL,
    
    #' @field X_test Matrix of predictor variables for the test set.
    X_test = NULL,
    
    #' @field y_test Vector of target variable labels for the test set.
    y_test = NULL,
    
    #' @field predicted_targets Data frame containing the classes predicted by the model on the test set.
    predicted_targets = NULL,
    
    #' @field accuracy Numeric value indicating the accuracy of the model on the test set.
    accuracy = NULL,
    
    #' @field coefficients Vector or matrix containing the coefficients of the logistic regression model.
    coefficients = NULL,
    
    #' @field levels_map List or vector mapping the levels of the encoded categorical variables.
    levels_map = NULL,
    
    #' @field class_labels Character vector containing the labels of the different classes.
    class_labels = NULL,
    
    #' @field class_frequencies Vector or table indicating the frequency of each class in the training data.
    class_frequencies = NULL,

    #' Initialize Logistic Regression Model
    #'
    #' Initializes the \code{LogisticRegression} class by loading data from a specified file.
    #'
    #' @param file_path Character. The path to the data file (CSV or Excel).
    #' @param delimiter Character. The delimiter used in the CSV file (e.g., "," for comma).
    #'
    #'
    #' @examples
    #' \dontrun{
    #' # Initialize the model with a CSV file and comma delimiter
    #' model <- LogisticRegression$new("data/loan_data.csv", ",")
    #' }

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

    #' Handle missing values in the dataset
    #'
    #' Processes missing values in the dataset using specified methods for numerical and categorical variables.
    #'
    #' @param num_method Character. Method to handle missing numerical values. Options are:
    #'   \itemize{
    #'     \item \code{"none"}: Do not handle missing values.
    #'     \item \code{"mean"}: Replace missing values with the mean of the column.
    #'     \item \code{"median"}: Replace missing values with the median of the column.
    #'     \item \code{"mode"}: Replace missing values with the mode of the column.
    #'     \item \code{"remove"}: Remove rows with missing numerical values.
    #'   }
    #' @param cat_method Character. Method to handle missing categorical values. Options are:
    #'   \itemize{
    #'     \item \code{"none"}: Do not handle missing values.
    #'     \item \code{"mode"}: Replace missing values with the mode of the column.
    #'     \item \code{"remove"}: Remove rows with missing categorical values.
    #'   }
    #'
    #'
    #' @examples
    #' \dontrun{
    #' # Handle missing values by replacing numerical NAs with mean and categorical NAs with mode
    #' model$handle_missing_values(num_method = "mean", cat_method = "mode")
    #' }

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

    #' Automatically select target variable
    #'
    #' Automatically selects the target variable based on entropy and correlation thresholds.
    #'
    #' @param entropy_threshold Numeric. Threshold for entropy to consider a variable as a potential target.
    #' @param correlation_threshold Numeric. Threshold for correlation to filter out highly correlated variables.
    #' @param weight_entropy Numeric. Weight assigned to entropy in the selection criteria.
    #' @param weight_correlation Numeric. Weight assigned to correlation in the selection criteria.
    #'
    #' @return returns the name of the selected target variable.
    #'
    #' @examples
    #' \dontrun{
    #' # Automatically select target with specified thresholds
    #' model$auto_select_target(entropy_threshold = 0.5, correlation_threshold = 0.3, 
    #'                         weight_entropy = 0.7, weight_correlation = 0.3)
    #' }

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

    #' Automatically remove unnecessary columns
    #'
    #' Automatically removes columns with high correlation based on the specified threshold. This function can be very slow for large datasets.
    #'
    #' @param correlation_threshold Numeric. Threshold for average Cramér's V correlation to consider a column for removal (default is 0.9).
    #'
    #' @return A character vector of column names to remove or \code{NULL} if no columns meet the criteria.
    #'
    #' @examples
    #' \dontrun{
    #' # Remove columns with average correlation above 0.9
    #' columns_removed <- model$auto_remove_columns(correlation_threshold = 0.9)
    #' }

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

    #' Prepare data for modeling
    #'
    #' Prepares the dataset by defining the target variable, removing specified columns, encoding categorical variables, normalizing numerical variables, and splitting the data into training and testing sets.
    #'
    #' @param target Character. The name of the target variable.
    #' @param columns_to_remove Character vector. Names of columns to remove from the predictors.
    #' @param test_size Numeric. Proportion of the dataset to include in the test split (e.g., 0.3 for 30%).
    #'
    #'
    #' @examples
    #' \dontrun{
    #' # Prepare the data with a specified target, columns to remove, and test size
    #' model$prepare_data(target = "default", columns_to_remove = c("id", "timestamp"), test_size = 0.3)
    #' }
    
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
      
      self$data <- data
      self$X <- X
      self$y <- y
      self$n <- n
      self$X_train <- X_train
      self$y_train <- y_train
      self$X_test <- X_test
      self$y_test <- y_test
    },

    #' Fit the Logistic Regression Model
    #'
    #' Adjusts the model coefficients using gradient descent.
    #'
    #' @param learning_rate Numeric. The learning rate for gradient descent.
    #' @param max_iter Integer. The maximum number of iterations.
    #' @param batch_size Integer. The size of each mini-batch.
    #' @param tol Numeric. The tolerance for convergence.
    #'
    #' @return Invisibly returns the fitted coefficients.
    #'
    #' @examples
    #' \dontrun{
    #' model <- LogisticRegression$new("data/loan_data.csv", ",")
    #' model$prepare_data(target = "default", columns_to_remove = NULL, test_size = 0.2)
    #' model$fit(learning_rate = 0.01, max_iter = 1000, batch_size = 32, tol = 1e-4)
    #' }
    
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
    
    #' Predict function for Logistic Regression Model
    #'
    #' This function makes predictions on the test data using the fitted logistic regression model. 
    #' It calculates the scores for each class, applies the softmax function to obtain class probabilities, 
    #' and predicts the class with the highest probability. The accuracy of the model is then calculated based on the predictions.
    #'
    #' @return The function returns the accuracy of the model on the test data.
    #'   - `accuracy`: The accuracy of the predictions on the test data.
    #'
    #' @examples
    #' \dontrun{
    #' # Assuming `model` is an instance of the logistic regression model
    #' accuracy <- model$predict()
    #' print(accuracy)  # Prints the accuracy of the model on the test data
    #' }
    #'
    #' @note 
    #' The model must be fitted before calling this function (i.e., the `fit` method should be run first).
    #' This function assumes that the model has a `coefficients` attribute, which contains the learned coefficients 
    #' for the logistic regression model, as well as `X_test` and `y_test` attributes for the test data and true labels, respectively.
    #'
    
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

    #' Convertpredicted classes to class labels
    #'
    #' Maps numeric class indices from the predicted outputs to their corresponding class labels.
    #'
    #' @return A data frame containing the predicted class labels.
    #'
    #' @details
    #' This function takes the predicted class indices (stored in \code{self$predicted_targets}) and maps them to their corresponding class labels (stored in \code{self$class_labels}).
    #' The mapping is achieved by indexing \code{self$class_labels} with the predicted class indices (incremented by 1 to account for R's 1-based indexing).
    #'
    #' @examples
    #' \dontrun{
    #' # Convert predicted class indices to labels
    #' predicted_labels <- model$predictions_to_labels()
    #'
    #' # View the first few predicted labels
    #' print(head(predicted_labels))
    #' }
    
    # Fonction de conversion des prédictions en labels
    predictions_to_labels = function() {
      # Conversion of predicted classes to class labels
      predicted_labels <- data.frame(
        Predicted = self$class_labels[self$predicted_targets + 1],
        stringsAsFactors = FALSE
      )
      
      return(predicted_labels)
    },
    
    #' Calculate variable importance for Logistic Regression Model
    #'
    #' Calculates the importance scores of the variables (predictors) based on the coefficients of a fitted logistic regression model.
    #'
    #' @return A data frame with two columns:
    #' - \code{Variable}: The name of each predictor variable.
    #' - \code{Importance}: The normalized importance score for each variable.
    #'
    #' @details
    #' This function calculates the importance of each predictor variable by taking the absolute values of the coefficients (excluding the intercept). 
    #' The sum of these absolute values for each variable is used as a measure of importance. Optionally, the scores are normalized by dividing by the total sum of importance scores.
    #' The variables are then sorted in descending order of importance.
    #'
    #' @examples
    #' \dontrun{
    #' # Calculate variable importance for a fitted model
    #' importance <- model$variable_importance()
    #'
    #' # View the importance of the top 5 variables
    #' head(importance, 5)
    #' }

    # Fonction de calcul de l'importance des variables
    variable_importance = function() {
      # Extraire les coefficients sans l'intercept (1ère colonne)
      coef_matrix <- self$coefficients[-1, ]
      
      # Calcul des scores d'importance
      importance_scores <- apply(abs(coef_matrix), 1, sum)
      
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
    
    #' Variable selection based on importance or threshold
    #'
    #' Selects variables (predictors) for the logistic regression model based on their importance scores or a specified threshold. 
    #' This helps in reducing the model complexity by keeping only the most important predictors.
    #'
    #' @param threshold Numeric value (default: 0.05). The minimum importance score a variable must have to be selected. 
    #' If `num_vars` is provided, this argument is ignored.
    #' @param num_vars Integer (default: NULL). The exact number of most important variables to select. If provided, 
    #' the function will select the top `num_vars` based on their importance scores.
    #'
    #' @return None. The function modifies the `X_train`, `X_test`, and `predictors` attributes of the model instance.
    #'
    #' @details
    #' This function uses the importance scores of the variables calculated by the `variable_importance` function to filter out 
    #' less important variables. The selection can either be based on a minimum importance threshold or by selecting a fixed 
    #' number of top variables.
    #'
    #' - If `num_vars` is specified, the function selects the top `num_vars` variables based on their importance score.
    #' - If `threshold` is provided, the function selects variables whose importance is greater than or equal to the threshold.
    #'
    #' After selecting the variables, it reduces the `X_train` and `X_test` datasets to the selected variables and updates the 
    #' `predictors` attribute.
    #'
    #' @examples
    #' \dontrun{
    #' # Select variables based on importance threshold
    #' model$var_select(threshold = 0.05)
    #'
    #' # Select the top 10 most important variables
    #' model$var_select(num_vars = 10)
    #' }

    # Fonction de sélection des variables
    var_select = function(threshold = 0.05, num_vars = NULL) {
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
    
    #' Generate Confusion Matrix and Performance Metrics
    #'
    #' Generates a confusion matrix based on actual and predicted labels and calculates key performance metrics such as accuracy, precision, recall, and F1-score for each class.
    #'
    #' @return A list containing:
    #' \describe{
    #'   \item{\code{confusion_matrix}}{Confusion matrix as a table.}
    #'   \item{\code{accuracy}}{Numeric value indicating the model's accuracy.}
    #'   \item{\code{precision}}{Numeric vector indicating precision for each class.}
    #'   \item{\code{recall}}{Numeric vector indicating recall for each class.}
    #'   \item{\code{f1_score}}{Numeric vector indicating F1-score for each class.}
    #' }
    #'
    #' @examples
    #' \dontrun{
    #' results <- model$generate_confusion_matrix()
    #' print(results$confusion_matrix)
    #' print(results$accuracy)
    #' }

    # Fonction de génération de la matrice de confusion et des métriques de performance
    generate_confusion_matrix = function() {
      # Convertion des labels en vecteurs
      true_labels <- as.vector(self$y_test)
      predicted_labels <- as.vector(self$predicted_targets)
      
      # Vérification de la cohérence des labels
      if (length(true_labels) != length(predicted_labels)) {
        stop("[Error] The length of true labels and predicted labels must be the same.")
      }
      
      # Calcul de la matrice de confusion
      confusion_matrix <- table(True = true_labels, Predicted = predicted_labels)
      
      # Conversion de la matrice de confusion en data frame
      confusion_df <- as.data.frame(as.table(confusion_matrix))
      colnames(confusion_df) <- c("True", "Predicted", "Frequency")
      
      # Calcul de l'accuracy
      total <- sum(confusion_matrix)
      accuracy <- sum(diag(confusion_matrix)) / total
      
      # Calcul des métriques de performance
      precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
      recall <- diag(confusion_matrix) / colSums(confusion_matrix)
      f1_score <- 2 * precision * recall / (precision + recall)
      
      # Stockage des résultats
      results <- list(
        confusion_matrix = confusion_matrix,
        accuracy = accuracy,
        precision = precision,
        recall = recall,
        f1_score = f1_score
      )
      
      return(results)
    },
    
    #' Predict class probabilities
    #'
    #' Computes the predicted probabilities for each class using the softmax function applied to the logistic regression model's coefficients and test data.
    #'
    #' @return A matrix of predicted probabilities, where each row corresponds to a sample and each column corresponds to a class.
    #'
    #' @details
    #' The `predict_proba` function calculates the probabilities for each class for the input test data (`X_test`) using the softmax function. It includes the following steps:
    #' - Adds an intercept term to the test data.
    #' - Computes the raw scores (logits) by multiplying the input data with the model's coefficients.
    #' - Applies the softmax function to convert logits into probabilities.
    #'
    #' If the model has not been trained and `self$coefficients` are not available, the function will raise an error.
    #'
    #' @examples
    #' \dontrun{
    #' # Predict probabilities for the test set
    #' probabilities <- model$predict_proba()
    #' print(probabilities)
    #' }
    
    # Fonction de prédiction des probabilités
    predict_proba = function() {
      
      X_input <- self$X_test
     
      # Ajout de l'intercept
      X_input <- cbind(1, as.matrix(X_input))
      
      # Calcul des scores (logits)
      scores <- X_input %*% self$coefficients
      
      # Application du softmax
      exp_scores <- exp(scores - apply(scores, 1, max))
      softmax_probs <- exp_scores / rowSums(exp_scores)
      
      # Retourner les probabilités pour chaque classe
      return(softmax_probs)
    },

    #' Summary of the Logistic Regression Model
    #'
    #' Prints a summary of the logistic regression model, including the number of observations, predictors, classes, class labels, class frequencies, and model coefficients.
    #'
    #' @return None. Prints the summary to the console.
    #'
    #' @examples
    #' \dontrun{
    #' # Print the model summary
    #' model$summary()
    #' }
    
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

    #' Print basics informations about the Logistic Regression Model
    #'
    #' Prints basic information about the logistic regression model, including the number of observations, predictors, classes, class labels, class frequencies, and model coefficients.
    #'
    #' @return None. The method prints information to the console.
    #'
    #' @examples
    #' \dontrun{
    #' # Print the model information
    #' model$print()
    #' }
    
    print = function() {
      cat("Logistic Regression Multinomial Model\n")
      cat("Number of Classes: ", length(self$class_labels), "\n")
      cat("Number of Predictors: ", ncol(self$X_train), "\n")
      cat("Class Labels: ", paste(self$class_labels, collapse = ", "), "\n")
      
      if (!is.null(self$coefficients)) {
        cat("Coefficients (first 5): \n")
        print(head(self$coefficients, 5))
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

#' S3 Method for summary of R6 Objects
#'
#' Provides a summary method for objects of R6 classes.
#'
#' @param object An R6 object to summarize.
#' @param ... Additional arguments passed to the summary method.
#'
#' @export
#' @method summary R6

# Définir la méthode générique summary pour les objets R6
summary.R6 <- function(object, ...) {
  if (!inherits(object, "R6")) {
    stop("L'objet fourni n'est pas une instance de classe R6.")
  }
  if ("summary" %in% names(object)) {
    object$summary(...)
  } else {
    stop("Cet objet ne supporte pas la fonction summary.")
  }
}