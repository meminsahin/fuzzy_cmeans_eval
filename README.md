# Regression Analysis with Type-1 Fuzzy Functions and Feature Selection Methods

This repository contains functions developed to optimize the **Fuzzy C-Means clustering algorithm** and **regression models**. These functions allow users to tune the parameters for clustering and regression to achieve the best performance, including support for Lasso and Ridge regression.

## Features

- Fuzzy C-Means clustering with parameter optimization (`c`, `m` values)
- Regression model optimization using:
  - **Linear Regression**
  - **Lasso Regression**
  - **Ridge Regression**
- Customizable test size and dataset inputs
- Automatic selection of the best model parameters based on RMSE (Root Mean Squared Error)

## Installation

To use this project, you will need to install the following R packages:

```r
install.packages(c("e1071", "glmnet"))

## Usage

Here’s an example of how to use the functions with the `mtcars` dataset:

```r
# Load necessary packages
library(e1071)
library(glmnet)

# Example dataset
data(mtcars)
target_col_mtcars <- which(names(mtcars) == "mpg")

# Function call to evaluate Fuzzy C-Means with Linear Regression
result_mtcars_fuzzy <- fuzzy_cmeans_eval(mtcars, target_col = target_col_mtcars)
print(result_mtcars_fuzzy)

# Function call to evaluate Fuzzy C-Means with Lasso Regression
result_mtcars_fuzzy_lasso <- fuzzy_cmeans_eval_lasso(mtcars, target_col = target_col_mtcars)
print(result_mtcars_fuzzy_lasso)

# Function call to evaluate Fuzzy C-Means with Ridge Regression
result_mtcars_fuzzy_ridge <- fuzzy_cmeans_eval_ridge(mtcars, target_col = target_col_mtcars)
print(result_mtcars_fuzzy_ridge)
