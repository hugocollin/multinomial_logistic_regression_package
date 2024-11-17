library(R6)

LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    # Propriétés
    coefficients = NULL,
    fitted = FALSE,
    class_labels = NULL,
    class_frequencies = NULL,
    learning_rate = 0.01, 
    max_iter = 1000,
    
    # Constructeur
    initialize = function(learning_rate = 0.01, max_iter = 1000) {
      self$learning_rate <- learning_rate
      self$max_iter <- max_iter
    },
    
    # Méthode fit : Ajustement du modèle
    fit = function(X, y) {
      if (!is.data.frame(X)) stop("X doit être un data frame")
      if (!is.factor(y) && !is.character(y)) stop("y doit être un facteur ou un vecteur de caractères")
      
      y <- as.factor(y)  # Conversion en facteur si nécessaire
      self$class_labels <- levels(y)
      self$class_frequencies <- table(y) / length(y)
      
      # Préparation des données
      
      non_numeric_vars <- sapply(X, function(x) !is.numeric(x))
      non_numeric_columns <- names(X)[non_numeric_vars]
      print("Variables non numériques :")
      print(non_numeric_columns)
      
      # Encoder les variables non numériques
      encoded_data <- model.matrix(~ . - 1, data = X[, non_numeric_columns])
      
      # Combiner avec les variables numériques
      final_data <- cbind(X[, sapply(X, is.numeric)], encoded_data)
      X <- cbind(1, as.matrix(final_data))  # Ajout de l'intercept
      
      
      y_binary <- as.numeric(y == self$class_labels[2])  # Convertir en binaire
      
      # Initialisation des coefficients
      beta <- matrix(0, ncol = 1, nrow = ncol(X))
      
      # Descente de gradient
      for (i in 1:self$max_iter) {
        predictions <- 1 / (1 + exp(-X %*% beta))
        gradient <- t(X) %*% (predictions - y_binary)
        beta <- beta - self$learning_rate * gradient
      }
      
      self$coefficients <- beta
      self$fitted <- TRUE
    },
    
    # Méthode predict : Prédiction des classes
    predict = function(X) {
      if (!self$fitted) stop("Le modèle n'a pas encore été ajusté. Appelez fit() d'abord.")
      X <- cbind(1, as.matrix(X))  # Ajout de l'intercept
      probs <- 1 / (1 + exp(-X %*% self$coefficients))
      predictions <- ifelse(probs > 0.5, self$class_labels[2], self$class_labels[1])
      return(predictions)
    },
    
    # Méthode predict_proba : Prédiction des probabilités
    predict_proba = function(X) {
      if (!self$fitted) stop("Le modèle n'a pas encore été ajusté. Appelez fit() d'abord.")
      X <- cbind(1, as.matrix(X))  # Ajout de l'intercept
      probs <- 1 / (1 + exp(-X %*% self$coefficients))
      return(data.frame(Class1 = 1 - probs, Class2 = probs))
    },
    
    # Méthode print : Informations succinctes
    print = function() {
      cat("Logistic Regression Model\n")
      if (self$fitted) {
        cat("Coefficients:\n")
        print(self$coefficients)
      } else {
        cat("Le modèle n'est pas encore ajusté.\n")
      }
    },
    
    # Méthode summary : Informations détaillées
    summary = function() {
      cat("Résumé du modèle de régression logistique\n")
      if (self$fitted) {
        cat("Classes :\n")
        print(self$class_labels)
        cat("\nFréquences des classes :\n")
        print(self$class_frequencies)
        cat("\nCoefficients :\n")
        print(self$coefficients)
      } else {
        cat("Le modèle n'a pas encore été ajusté.\n")
      }
    }
  )
)


titanic <- as.data.frame(Titanic)
X <- titanic[, !names(titanic) %in% "Survived"]
y <- titanic$Survived
logit_model = LogisticRegression$new()
logit_model$fit(X, y)

