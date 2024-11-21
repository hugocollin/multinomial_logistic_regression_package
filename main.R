# Importation des bibliothèques nécessaires
library(R6)
library(readr)
library(readxl)

# Définition de la classe DataPreparer
DataPreparer <- R6Class("DataPreparer",
  public = list(
    data = NULL,          # Jeu de données
    target = NULL,        # Nom de la variable cible
    predictors = NULL,    # Liste des variables prédictives
    prepared_data = NULL, # Données transformées
    levels_map = NULL,    # Mapping des niveaux des variables catégoriques
    
    # Constructeur de la classe
    initialize = function(file_path = NULL, data = NULL, target, predictors) {
        # Chargement du fichier
        if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
          data <- read_csv(file_path)
        } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
          data <- read_excel(file_path)
        } else {
          stop("[Attention] Format de fichier non supporté, utilisez un fichier .csv ou .xlsx.")
        }
      print("[INFO] Les données ont été chargées avec succès.")
      
      # Vérification des colonnes
      if (!target %in% colnames(data)) stop("[Attention] La variable cible n'existe pas dans le jeu de données.")
      if (!all(predictors %in% colnames(data))) stop("[Attention] Certaines variables prédictives sont manquantes")
      
      self$data <- data             # Sauvegarde des données
      self$target <- target         # Sauvegarde de la variable cible
      self$predictors <- predictors # Sauvegarde des variables prédictives
      self$levels_map <- list()     # Initialisation du mapping des niveaux
    },

    # Fonction de gestion des valeurs manquantes
    handle_missing_values = function(method = c("mean", "median", "mode", "remove")) {
      method <- match.arg(method)
      df <- self$data
      
      for (var in colnames(df)) {
        if (any(is.na(df[[var]]))) {
          # Remplacement des valeurs manquantes par la moyenne
          if (method == "mean" && is.numeric(df[[var]])) {
            df[[var]][is.na(df[[var]])] <- mean(df[[var]], na.rm = TRUE)
          # Remplacement des valeurs manquantes par la médiane
          } else if (method == "median" && is.numeric(df[[var]])) {
            df[[var]][is.na(df[[var]])] <- median(df[[var]], na.rm = TRUE)
          # Remplacement des valeurs manquantes par la valeur la plus fréquente
          } else if (method == "mode") {
            mode_value <- as.numeric(names(sort(table(df[[var]]), decreasing = TRUE)[1]))
            df[[var]][is.na(df[[var]])] <- mode_value
          # Suppression des lignes avec des valeurs manquantes
          } else if (method == "remove") {
            df <- df[complete.cases(df), ]
          }
        }
      }
      
      self$data <- df
      print("[INFO] Les valeurs manquantes ont été gérées avec succès.")
    },
    
    # Fonction de préparation des données
    prepare_data = function() {
      df <- self$data
      
      # Encodage de la variable cible (facteur -> indices numériques)
      if (!is.factor(df[[self$target]])) {
        df[[self$target]] <- as.factor(df[[self$target]])
      }
      self$levels_map[[self$target]] <- levels(df[[self$target]])
      df[[self$target]] <- as.numeric(df[[self$target]]) - 1
      
      # Gestion des variables prédictives
      new_columns <- list()
      for (var in self$predictors) {
        if (is.factor(df[[var]]) || is.character(df[[var]])) {
          # Encodage one-hot pour les variables catégoriques
          levels_var <- unique(df[[var]])
          for (lvl in levels_var) {
            col_name <- paste0(var, "=", lvl)
            new_columns[[col_name]] <- as.numeric(df[[var]] == lvl)
          }
          df[[var]] <- NULL
        }
      }
      
      # Ajout des nouvelles colonnes au dataframe
      for (col_name in names(new_columns)) {
        df[[col_name]] <- new_columns[[col_name]]
      }
      
      self$prepared_data <- df # Sauvegarde des données préparées
      invisible(self)
      
      print("[INFO] Les données ont été préparées avec succès.")
    },
    
    # Fonction pour obtenir les données préparées séparément
    get_prepared_data = function() {
      if (is.null(self$prepared_data)) {
        stop("[Attention] Les données n'ont pas encore été préparées. Appelez prepare_data() d'abord.")
      }
      # Séparation de la variable cible et des prédicteurs
      y <- self$prepared_data[[self$target]]
      X <- self$prepared_data[, !colnames(self$prepared_data) %in% self$target, drop = FALSE]
      return(list(X = X, y = y))
    },

    # Fonction pour afficher les données préparées et des statistiques
    display_data = function() {
      if (is.null(self$prepared_data)) {
        stop("[Attention] Les données n'ont pas encore été préparées. Appelez prepare_data() d'abord.")
      }
      print("[INFO] Données préparées :")
      print(self$prepared_data)
      
      print("[INFO] Statistiques des données préparées :")
      print(summary(self$prepared_data))
    }
  )
)

# Exemple d'utilisation avec un fichier CSV
preparer <- DataPreparer$new(
  file_path = "test.csv",
  target = "target",
  predictors = c("var1", "var2")
)
preparer$handle_missing_values(method = "mean")
preparer$prepare_data()
prepared_data <- preparer$get_prepared_data()
preparer$display_data()

# Accès aux données prédictives et à la variable cible
X <- prepared_data$X
y <- prepared_data$y

print("Données prédictives (X) :")
print(X)

print("Variable cible (y) :")
print(y)