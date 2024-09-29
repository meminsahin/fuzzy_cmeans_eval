fuzzy_cmeans_eval_ridge <- function(data, target_col, length_test = 20, c = NULL, m = NULL) {
  required_packages <- c("e1071", "glmnet")
  
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
  
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame.")
  }
  
  if (!is.numeric(target_col) || length(target_col) != 1 || target_col <= 0 || target_col > ncol(data)) {
    stop("Error: 'target_col' must be a valid column index in 'data'. It should be a single numeric value.")
  }
  
  if (!is.numeric(length_test) || length(length_test) != 1 || length_test <= 0 || length_test > 100) {
    stop("Error: 'length_test' must be a single numeric value between 0 and 100.")
  }
  
  if (!is.null(c) && (!is.numeric(c) || any(c <= 0) || any(c %% 1 != 0))) {
    stop("Error: 'c' must be a numeric vector with positive integer values.")
  }
  
  if (!is.null(m) && (!is.numeric(m) || any(m <= 1))) {
    stop("Error: 'm' must be a numeric vector with values greater than 1.")
  }
  
  set.seed(53)
  n <- nrow(data)
  test_size <- round(n * length_test / 100)
  test_indices <- sample(1:n, test_size)
  
  test_data <- data[test_indices, ]
  train_data <- data[-test_indices, ]
  
  y_train <- train_data[, target_col]
  x_train <- train_data[, -target_col, drop = FALSE]
  y_test <- test_data[, target_col]
  x_test <- test_data[, -target_col, drop = FALSE]
  
  best_rmse <- Inf
  best_c <- c
  best_m <- m
  best_lambda <- NULL
  best_predictions <- NULL
  
  if (is.null(c) | is.null(m)) {
    c_values <- 2:5  
    m_values <- seq(1.3, 3, 0.1)
  } else {
    c_values <- c
    m_values <- m
  }
  
  for (ci in c_values) {
    for (mi in m_values) {
      suppressWarnings({
        model <- cmeans(train_data, centers = ci, m = mi)
        mu_train <- model$membership
        train_data_with_membership <- cbind(mu_train, x_train)
        lambdas <- c(0, 10^seq(-1, 3, by = 0.1))
        ridge_fit <- cv.glmnet(as.matrix(train_data_with_membership), y_train, alpha = 0, lambda = lambdas)
        best_lambda <- ridge_fit$lambda.min
        
        fit <- glmnet(as.matrix(train_data_with_membership), y_train, alpha = 0, lambda = best_lambda)
        
        centers <- model$centers[, -target_col, drop = FALSE]
        
        euclidean_distance <- function(x, y) {
          sqrt(sum((x - y)^2))
        }
        
        distances <- matrix(0, nrow = ci, ncol = nrow(test_data))
        for (i in 1:ci) {
          for (j in 1:nrow(test_data)) {
            distances[i, j] <- euclidean_distance(centers[i, ], x_test[j, ])
          }
        }
        
        membership <- matrix(0, nrow = nrow(test_data), ncol = ci)  
        for (k in 1:nrow(test_data)) { 
          denom_sum <- numeric(ci)
          for (i in 1:ci) { 
            for (j in 1:ci) { 
              denom_sum[j] <-  (distances[i, k] / distances[j, k]) ^ (2 / (mi - 1)) 
            }
            membership[k, i] <- 1 / sum(denom_sum) 
          }
        }
        
        test_data_with_membership <- cbind(membership, x_test)
        
        predictions <- predict(fit, newx = as.matrix(test_data_with_membership))
        predictions <- as.vector(predictions)
        
        weighted_predictions <- rowSums(membership * predictions) / rowSums(membership)
        
        rmse <- sqrt(mean((y_test - weighted_predictions)^2))
        
        if (rmse < best_rmse) {
          best_rmse <- rmse
          best_c <- ci
          best_m <- mi
          best_lambda <- best_lambda
          best_predictions <- weighted_predictions
        }
      })
    }
  }
  
  result <- list(
    c = best_c,
    m = best_m,
    lambda = best_lambda,
    rmse = best_rmse,
    predictions = best_predictions
  )
  
  if (is.null(c) | is.null(m)) {
    output <- sprintf(
      "\n%s\n%s\n\n%s\nBest c value: %d\nBest m value: %.1f\nBest lambda value: %.4f\n\n%s\nRMSE value: %.4f\n\n%s\n%s\n",
      "Fuzzy C-Means Evaluation Ridge Results",
      strrep("=", 30),
      "Best Parameters:",
      best_c,
      best_m,
      best_lambda,
      "Model Performance:",
      best_rmse,
      "Predictions:",
      toString(round(best_predictions, 4))
    )
  } else {
    output <- sprintf(
      "\n%s\n%s\n\n%s\nRMSE value: %.4f\n\n%s\n%s\n",
      "Fuzzy C-Means Evaluation Results",
      strrep("=", 30),
      "Model Performance:",
      best_rmse,
      "Predictions:",
      toString(round(best_predictions, 4))
    )
  }
  
  cat(output)
  
  return(result)
}