# Depends: ROCR
require(ROCR)

# Vector functions --------------------------------------------------------
# squared error
se <- function (predicted, actual)
    (actual - predicted) ^ 2

# absolute error
ae <- function (predicted, actual)
    abs(actual - predicted)

# squared log error
sle <- function (predicted, actual)
    (log(1 + actual) - log(1 + predicted)) ^ 2

# Metric functions --------------------------------------------------------
# mean squared error
mse <- function (predicted, actual)
    mean(se(predicted, actual))

# root mean squared error
rmse <- function (predicted, actual)
    sqrt(mse(predicted, actual))

# mean absolute error
mae <- function (predicted, actual)
    mean(ae(predicted, actual))

# mean squared log error
msle <- function (predicted, actual)
    mean(sle(predicted, actual))

# root mean squared log error
rmsle <- function (predicted, actual)
    sqrt(msle(predicted, actual))

# Relative square error
rse <- function(predicted, actual)
    mean((predicted - actual) ^ 2) / mean((mean(actual) - actual) ^ 2)

# R-Square
rs <- function(predicted, actual)
    1 - rse(predicted, actual)

# Accuracy
acc <- function(predicted, actual, cutoff = 0.5) {
    mean((predicted > cutoff) == (actual > cutoff))
}

# area under the ROC (AUC)
auc <- function(predicted, actual) {
    r <- rank(predicted)
    n_pos <- sum(actual == 1)
    n_neg <- length(actual) - n_pos
    auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
    auc
}

# mean log loss
logLoss <- function(predicted, actual){
    ll <- function(predicted, actual) {
        score <- -(actual * log(predicted) + (1 - actual) * log(1 - predicted))
        score[actual == predicted] <- 0
        score[is.nan(score)] <- Inf
        score
    }
    mean(ll(predicted, actual))
}

# multi-class log loss
ll <- function(predicted, actual, eps = 1e-15) {
    #     ll <- function(predicted, actual, eps = 1e-15) {
    #         predicted[predicted < eps] <- eps
    #         predicted[predicted > 1 - eps] <- 1 - eps
    #         score <- -1 / nrow(actual) * (sum(actual * log(predicted)))
    #         score
    #     }
    
    nr <- nrow(predicted)
    predicted <- matrix(sapply(predicted, function(x) max(eps,x)), nrow = nr)
    predicted <- matrix(sapply(predicted, function(x) min(1 - eps,x)), nrow = nr)
    ll <- sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))
    ll <- ll * -1 / (nrow(actual))
    return(ll)
}

# Regression --------------------------------------------------------------
Eval_Reg <- function(predicted, actual, ...) {
    c(
        N = length(predicted),
        RMSE = round(rmse(predicted, actual), 4),
        RSE = round(rse(predicted, actual), 4),
        RSq = round(rs(predicted, actual), 4),
        Acc = round(acc(predicted, actual, ...),4)
    )
}

# Classification ----------------------------------------------------------
Eval_Class <- function(predicted, actual, showplots = TRUE) {
    actual[actual == 0.5] <- 1
    pred_obj <- prediction(predicted, actual)
    
    if (showplots) {
        plot(performance(pred_obj, "tpr", "fpr"))               ## ROC curve
        plot(performance(pred_obj, "prec", "rec"))              ## precision/recall curve
        plot(performance(pred_obj, "sens", "spec"))             ## sensitivity/specificity curve
        plot(performance(pred_obj, "lift", "rpp"))              ## Lift chart
        plot(performance(pred_obj, "acc"))                      ## Accuracy
    }
    
    acc_perf <- performance(pred_obj, "acc")
    acc_cutoff <- acc_perf@x.values[[1]]
    acc_score <- acc_perf@y.values[[1]]
    
    f1_perf <- performance(pred_obj, 'f')
    f1_cutoff <- f1_perf@x.values[[1]]
    f1_score <- f1_perf@y.values[[1]]
    
    return(c(
        AUC = auc(predicted, actual),
        Acc = acc(predicted, actual),
        F1 = max(f1_score, na.rm = TRUE),
        F1_BestCutOff = f1_cutoff[which.max(f1_score)],
        Acc_Best = max(acc_score, na.rm = TRUE),
        Acc_BestCutOff = acc_cutoff[which.max(acc_score)]
    ))
}