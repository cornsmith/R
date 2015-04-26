# Evaluative Functions ----------------------------------------------------
# Depends: ROCR

# squared error
se <- function (predicted, actual) (actual-predicted)^2

# mean squared error
mse <- function (predicted, actual) mean(se(predicted, actual))

# root mean squared error
rmse <- function (predicted, actual) sqrt(mse(predicted, actual))

# absolute error
ae <- function (predicted, actual) abs(actual-predicted)

# mean absolute error
mae <- function (predicted, actual) mean(ae(predicted, actual))

# squared log error
sle <- function (predicted, actual) (log(1+actual)-log(1+predicted))^2

# mean squared log error
msle <- function (predicted, actual) mean(sle(predicted, actual))

# root mean squared log error
rmsle <- function (predicted, actual) sqrt(msle(predicted, actual))

# Relative square error
rse <- function(predicted, actual){mean((predicted - actual)^2) / mean((mean(actual) - actual)^2)}

# R-Square
rs <- function(predicted, actual){1 - rse(predicted, actual)}


# area under the ROC (AUC)
auc <- function(predicted, actual){
    r <- rank(predicted)
    n_pos <- sum(actual==1)
    n_neg <- length(actual) - n_pos
    auc <- (sum(r[actual==1]) - n_pos*(n_pos+1)/2) / (n_pos*n_neg)
    auc
}

# log loss
ll <- function(predicted, actual){
    score <- -(actual*log(predicted) + (1-actual)*log(1-predicted))
    score[actual==predicted] <- 0
    score[is.nan(score)] <- Inf
    score
}

# multi-class log loss
mcll <- function(predicted, actual, eps=1e-15) {
    predicted[predicted < eps] <- eps
    predicted[predicted > 1 - eps] <- 1 - eps
    score <- -1/nrow(actual)*(sum(actual*log(predicted)))
    score
}

# mean log loss
logLoss <- function(predicted, actual) mean(ll(predicted, actual))


# Regression --------------------------------------------------------------
EvalReg <- function(predicted, labels, cutoff = 0.5){
    # Accuracy
    Acc <- function(predicted, labels, cutoff){mean((predicted > cutoff) == (labels > cutoff))}
    
    return(c(N = length(predicted),
             RMSE = round(rmse(predicted, labels), 4),
             RSE = round(rse(predicted, labels), 4),
             RSq = round(rs(predicted, labels), 4),
             Acc = round(Acc(predicted, labels, cutoff),4))
    )
}


# Classification ----------------------------------------------------------
EvalClass <- function(predicted, labels, showplots = TRUE){
    require(ROCR)
    eval.pred <- prediction(predicted, labels)
    
    if(showplots){
        plot(performance(eval.pred, "tpr", "fpr"))                   ## ROC curve
        plot(performance(eval.pred, "prec", "rec"))                  ## precision/recall curve
        plot(performance(eval.pred, "sens", "spec"))                 ## sensitivity/specificity curve
        plot(performance(eval.pred, "lift", "rpp"))                  ## Lift chart
        plot(performance(eval.pred, "acc"))                          ## Accuracy        
    }
    auc <- performance(eval.pred, "auc")
    auc <- unlist(slot(auc, "y.values"))
    
    
    f1 <- performance(eval.pred, 'f')
    cutoff <- f1@x.values[[1]]
    f1 <- f1@y.values[[1]]
    
    return(c(AUC = auc,
             F1 = max(f1, na.rm = TRUE),
             BestCutOff = cutoff[which.max(f1)]
             )
    )
}