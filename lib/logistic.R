confusion_matrix <- function(actual, predicted, threshold, true_label, false_label) {
    table(
        actual = actual,
        prediction = factor(
            ifelse(predicted > threshold, true_label, false_label),
            levels = c(false_label, true_label)
        )
    )
}

cm_summary <- function(threshold, cm) {
    metrics <- tibble(
        threshold = threshold,
        TP = cm[2, 2],
        FP = cm[1, 2],
        TN = cm[1, 1],
        FN = cm[2, 1]
    ) %>% 
    mutate(
        precision = TP / (TP + FP),
        recall = TP / (TP + FN),
        accuracy = (TP + TN) / (TP + FP + TN + FN)
    )
    return(metrics)
}

log_liklihoods <- function(y, py) {
    # Function to return the log likelihoods for each data point.
    # Argument y is the true outcome (as a numeric variable, 0/1);
    # argument py is the predicted probability.
    # Remember that the close to 1, the smaller the negative log value
    # gives the liklihood of having a 0 or 1 (y) given the predicted probability (py)
    values <- y * log(py) + (1-y) * log(1 - py)
    # log(0) is 0
    values[is.nan(values)] <- 0
    return(values)
}

deviance <- function(liklihoods) {
    -2 * sum(liklihoods)
}

logit <- function(p) {
    log(p/(1-p))
}

inv_logit <- function(x) {
    1 / (1 + exp(-x))
}

odds2p <- function(odds) {
    odds / (1 + odds)
}

p2odds <- function(p) {
    p / (1 - p)
}
