#' training random forest models
#'
#' @param Y Outcome (vector)
#' @param X Covariates (matrix)
#' @param num.trees Number of trees
#' @param ... additional options for ranger
#'
#' @return A trained random forest model
#'
#' @importFrom ranger ranger
train_rf <- function(Y, X, num.trees = 500, ...) {
  # If binary, force Y to factor and set probability estimation
  if(length(unique(Y)) == 2) {
    mod <- ranger::ranger(
      formula = Y ~ .,
      data = data.frame(Y = as.factor(Y), X),
      num.trees = num.trees,
      probability = TRUE,
      ...
    )
  } else {
    mod <- ranger::ranger(
      formula = Y ~ .,
      data = data.frame(Y, X),
      num.trees = num.trees,
      ...
    )
  }
  return(mod)
}
#' predicitions based on a random forest models
#'
#' @param X Covariates (matrix)
#' @param mod Random forest model
#'
#' @return Predictions
#'
#' @importFrom ranger predict.ranger
pred_rf <- function(mod, X) {
  new_data <- data.frame(X)
  # Check if model is for probability estimation
  if(mod$forest$treetype == "Probability estimation") {
    return(ranger::predict.ranger(mod, data = new_data)$predictions[,"1"])
  } else {
    return(ranger::predict.ranger(mod, data = new_data)$predictions)
  }
}
train_nn <- function(Y, X, size = 5, ...) {
  if(length(unique(Y)) == 2) {
    mod <- nnet::nnet(
      Y ~ .,
      data = data.frame(Y = as.factor(Y), X),
      size = size,
      trace = FALSE,
      linout = FALSE,
      ...
    )
  } else {
    mod <- nnet::nnet(
      Y ~ .,
      data = data.frame(Y, X),
      size = size,
      trace = FALSE,
      linout = TRUE,
      ...
    )
  }
  return(mod)
}
pred_nn <- function(mod, X) {
  new_data <- data.frame(X)
  return(nnet::predict.nnet(mod, newdata = new_data))
}
train_ols <- function(Y, X, ...) {
  df_train <- as.data.frame(X)
  df_train$Y <- Y
  return(stats::lm(Y ~ ., data = df_train, ...))
}
pred_ols <- function(mod, X) {
  newdata <- as.data.frame(X)
  return(stats::predict(mod, newdata = newdata))
}
train_log = function(Y, X) {
  stats::glm(Y ~ X, family = stats::binomial(), data = data.frame(Y, X))
}
pred_log = function(mod, X) {
  stats::predict(mod, data.frame(X), type = "response")
}

learning_function <- function(Y, X, ids_out, ids_in, type = "parametric", crossfit = FALSE, seed = NULL, ...) {
  # Allow external seed control
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Choose training and prediction functions based on model type
  if (type == "nn") {
    train_fun <- train_nn
    pred_fun  <- pred_nn
  } else if (type == "rf") {
    train_fun <- train_rf
    pred_fun  <- pred_rf
  } else if (type == "parametric") {
    if(length(unique(Y)) == 2) {
      train_fun <- train_log
      pred_fun  <- pred_log
    } else {
      train_fun <- train_ols
      pred_fun  <- pred_ols
    }
  } else {
    stop("Unknown type specified.")
  }

  res <- numeric(length(ids_out))

  if (crossfit) {
    n <- length(ids_out)
    # Create a two-fold split indicator
    fold_indicator <- sample(rep(1:2, length.out = n))
    fold1 <- (fold_indicator == 1)
    fold2 <- (fold_indicator == 2)

    # Identify training indices: only use indices that are in ids_in
    train_idx1 <- which(fold1 & (ids_out %in% ids_in))
    train_idx2 <- which(fold2 & (ids_out %in% ids_in))

    # Define test indices for each fold
    test_idx1 <- which(fold2)
    test_idx2 <- which(fold1)

    # Train on fold1 training indices, predict on fold2 observations
    if(length(train_idx1) == 0) stop("No training observations in fold 1.")
    mod1 <- train_fun(Y[train_idx1], X[train_idx1, , drop = FALSE], ...)
    res[test_idx1] <- as.numeric(pred_fun(mod1, X[test_idx1, , drop = FALSE]))

    # Train on fold2 training indices, predict on fold1 observations
    if(length(train_idx2) == 0) stop("No training observations in fold 2.")
    mod2 <- train_fun(Y[train_idx2], X[train_idx2, , drop = FALSE], ...)
    res[test_idx2] <- as.numeric(pred_fun(mod2, X[test_idx2, , drop = FALSE]))
  } else {
    # For the non-crossfitting branch, only use the subset of observations in ids_in for training.
    train_idx <- which(ids_out %in% ids_in)
    mod <- train_fun(Y[train_idx], X[train_idx, , drop = FALSE],...)
    res <- as.numeric(pred_fun(mod, X))
  }
  return(res)
}
