# README : Multinomial Logistic Regression package

## Description

This package is a simple implementation of the Multinomial Logistic Regression algorithm in R. It is a supervised learning algorithm that is used when the target variable is categorical. It is an extension of the logistic regression algorithm that is used when the target variable is binary. The multinomial logistic regression algorithm is used when the target variable has more than two categories. The algorithm is implemented using the maximum likelihood estimation method and used to predict the probability of the target variable belonging to each category.

## Table of contents

- [Description](#description)
- [Installation of the package](#installation-of-the-package)
- [Installation of the UI package](#installation-of-the-ui-package)
- [Usage of the package](#usage-of-the-package)
- [Usage of the UI package](#usage-of-the-ui-package)
- [Author](#author)

## Installation of the package

To install the package on your local machine, you can use the following command :

```R
devtools::install_githug("hugocollin/multinomial_logistic_regression_package")
```

_Before installing the package, make sure you have the following dependencies installed :_

- _devtools_
- _rtools_

_You can install them using the following commands :_

```R
install.package("devtools")
install.package("rtools")
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
library(mlr)
```

You can ask for help using the following command :

```R
?LogisticRegression
```

## Usage of the UI package

To use the UI package, you can run _interface.r_ on RStudio. This will open a new window on your browser where you can use the application.

## Author

This project was developed by 3 students from the Master 2 SISE program at the University of Lyon 2 : KPAMEGAN Falonne, COLLIN Hugo and PERBET Lucile.