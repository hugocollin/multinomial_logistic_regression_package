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
      
      self$class_labels <- levels(y)
      self$class_frequencies <- table(y) / length(y)
      
      
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

n <- nrow(titanic)

set.seed(123)  # Assure la reproductibilité
train_indices <- sample(1:n, size = 0.7 * n)  # 70% des données pour le train

X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Vérifier les formats
X_train <- model.matrix(~ . - 1, data = X_train)  # Encodage one-hot
X_test <- model.matrix(~ . - 1, data = X_test)
y_train <- factor(y_train)  # Assurez-vous que y est un facteur
y_test <- factor(y_test)

X_train <- as.data.frame(X_train)
X_test <- as.data.frame(X_test)

logit_model = LogisticRegression$new()
logit_model$fit(X_train, y_train)

print(logit_model)
logit_model$summary()

predictions <- logit_model$predict(X_test)
accuracy <- mean(predictions == y_test)
cat("Accuracy sur l'ensemble de test :", accuracy, "\n")
