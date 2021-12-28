RMSE <- function(actual, fitted) {
    sqrt(mean((actual - fitted) ^ 2))
}

RSQ <- function(actual, fitted) {
    1 - sum((actual - fitted) ^ 2) / sum((actual - mean(actual)) ^ 2)
}
