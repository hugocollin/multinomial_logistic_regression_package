# README : Multinomial Logistic Regression package

## Table of contents

- [Description](#description)
- [Project structure](#project-structure)
- [Installation of the package](#installation-of-the-package)
- [Installation of the UI package](#installation-of-the-ui-package)
- [Usage of the package](#usage-of-the-package)
- [Usage of the UI package](#usage-of-the-ui-package)
- [Authors](#authors)

## Description

This package is a simple implementation of the Multinomial Logistic Regression algorithm in R. It is a supervised learning algorithm that is used when the target variable is categorical. It is an extension of the logistic regression algorithm that is used when the target variable is binary. The multinomial logistic regression algorithm is used when the target variable has more than two categories. The algorithm is implemented using the maximum likelihood estimation method and used to predict the probability of the target variable belonging to each category.

## Project structure

```bash
├── data/
│   ├── iris_extended.csv
│   ├── iris_original.csv
│   ├── iris_original.xlsx
│   ├── iris_tabulation.csv
│   └── loan_data.csv
├── docs/
│   ├── multinomial_logistic_regression_report.pdf
│   ├── multinomial_logistic_regression_report.tex
│   ├── package.zip
│   └── README.md
├── package/
│   ├── man/
│   │   ├── LogisticRegression.Rd
│   │   └── summary.R6.Rd
│   ├── R/
│   │   └── model.r
│   ├── DESCRIPTION
│   └── NAMESPACE
└── interface.r
```

## Installation of the package

To install the package on your local machine, you can use the following command :

```R
devtools::install_github("hugocollin/multinomial_logistic_regression_package/package")
```

_Before installing the package, make sure you have the following dependencies installed :_

- _devtools_
- _rtools_

_You can install them using the following commands :_

```R
install.packages("devtools")
install.packages("rtools")
```
## Installation of the UI package

To install the UI package on your local machine, you can use the following command :

```bash
git clone https://github.com/hugocollin/multinomial_logistic_regression_package
```

_To run the UI package, make sure you have R installed on your machine._

## Usage of the package

To use the package, you can use the following commands :

```R
library(sisemlr)
```

You can ask for help using the following command :

```R
?LogisticRegression
```

Here is an example of how to use the package :

```R
# Load the package
library(sisemlr)

# Initialization of the model
model <- LogisticRegression$new(file_path = "path/to/your/data.extention", delimiter = "delimiter")

# Handle missing values by replacing numerical NAs with mean and categorical NAs with mode
model$handle_missing_values(num_method = "mean", cat_method = "mode")

# Automatically select target with specified thresholds
model$target_select(entropy_threshold = 0.5, correlation_threshold = 0.3, weight_entropy = 0.7, weight_correlation = 0.3)

# Prepare the data with a specified target, columns to remove, and test size
model$prepare_data(target = "target_column", columns_to_remove = c("feature1", "feature2", "feature3"), test_size = 0.3)

# Fit the model with specified hyperparameters
model$fit(learning_rate = 0.01, max_iter = 1000, batch_size = 50, tol = 0.001)

# Calculate variable importance for a fitted model
importance <- model$var_importance()

# View the importance of the variables
print(importance)

# Make predictions on the test set
accuracy <- model$predict()

# Print the accuracy of the model on the test data
print(accuracy)

# Predict probabilities for the test set
probabilities <- model$predict_proba()

# Print the predicted probabilities
print(probabilities)

# Print the model summary
model$summary()

# Print the model information
model$print()

# Generate a confusion matrix and performance metrics
results <- model$generate_confusion_matrix()
print(results$confusion_matrix)
print(results$accuracy)
print(results$precision)
print(results$recall)
print(results$f1_score)
```

## Usage of the UI package

To use the UI package, you can run _interface.r_ on RStudio. This will open a new window on your browser where you can use the application.

## Authors

This project was developed by 3 students from the Master 2 SISE program at the University of Lyon 2 : KPAMEGAN Falonne, COLLIN Hugo and PERBET Lucile.