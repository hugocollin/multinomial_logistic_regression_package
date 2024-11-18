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
    initialize = function(import_from_file = FALSE, file_path = NULL, data = NULL, target, predictors) {
      if (import_from_file) {
        # Chargement du fichier import_from_file est TRUE
        if (is.null(file_path)) stop("[Attention] Le chemin du fichier doit être fourni si import_from_file est TRUE.")
        if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
          data <- read_csv(file_path)
        } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
          data <- read_excel(file_path)
        } else {
          stop("[Attention] Format de fichier non supporté, utilisez un fichier .csv ou .xlsx.")
        }
      } else {
        # Vérification et convertion des données si import_from_file est FALSE
        if (is.null(data)) stop("[Attention] Les données doivent être fournies si import_from_file est FALSE.")
        if (!inherits(data, "data.frame")) {
          if (is.matrix(data)) {
            data <- as.data.frame(data)
          } else {
            stop("[Attention] Les données doivent être un dataframe ou une matrice.")
          }
        }
      }
      print("[INFO] Les données ont été chargées avec succès.")
      
      # Vérification des colonnes
      if (!target %in% colnames(data)) stop("[Attention] La variable cible n'existe pas dans le jeu de données.")
      if (!all(predictors %in% colnames(data))) stop("[Attention] Certaines variables prédictives sont manquantes.")
      
      self$data <- data             # Sauvegarde des données
      self$target <- target         # Sauvegarde de la variable cible
      self$predictors <- predictors # Sauvegarde des variables prédictives
      self$levels_map <- list()     # Initialisation du mapping des niveaux
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
      new_columns <- list()  # Pour stocker les colonnes encodées
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
    
    # Fonction pour obtenir les données préparées
    get_prepared_data = function() {
      if (is.null(self$prepared_data)) stop("[Attention] Les données n'ont pas encore été préparées. Appelez prepare_data() d'abord.")
      return(self$prepared_data)
    }
  )
)

data <- matrix(
  c("A", 1.2, "X",
    "B", 3.4, "Y",
    "A", 5.6, "X",
    "C", 2.3, "Z"),
  ncol = 3, byrow = TRUE
)
colnames(data) <- c("target", "var1", "var2")

preparer <- DataPreparer$new(
  import_from_file = FALSE,
  data = data,
  target = "target",
  predictors = c("var1", "var2")
)
preparer$prepare_data()
prepared_data <- preparer$get_prepared_data()
print(prepared_data)