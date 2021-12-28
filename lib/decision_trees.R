loglikelihood <- function(y, py) {
    pysmooth <- ifelse(py == 0, 1e-12, ifelse(py == 1, 1 - 1e-12, py))
    sum(y * log(pysmooth) + (1 - y) * log(1 - pysmooth))
}

accuracyMeasures <- function(pred, truth, name = "model") {
    # normalize the deviance by the number of data points
    # so we can compare the deviance across training and test sets
    dev.norm <- -2 * loglikelihood(as.numeric(truth), pred) / length(pred)
    # convert the class probability estimator into a classifier
    # by labeling documents that score greater than 0.5 as spam
    ctable <- table(truth = truth, pred = (pred > 0.5))
    accuracy <- sum(diag(ctable)) / sum(ctable)
    precision <- ctable[2, 2] / sum(ctable[, 2])
    recall <- ctable[2, 2] / sum(ctable[2, ])
    f1 <- 2 * precision * recall / (precision + recall)
    tibble(
        model = name,
        accuracy = accuracy,
        f1 = f1,
        deviance = dev.norm
    )
}


predict_bag <- function(treelist, newdata) {
    # assume that the underlying classifier returns decision probabilities
    # not decisions
    preds <- sapply(
        1:length(treelist),
        FUN = function(iter) { predict(treelist[[iter]], newdata = newdata)[,2] }
    )
    # take the mean of the predictions of all the individual trees
    predsums <- rowSums(preds)
    predsums / length(treelist)
}