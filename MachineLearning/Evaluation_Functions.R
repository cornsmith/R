# Evaluative Functions

# Regression --------------------------------------------------------------
EvalReg <- function(predicted, labels, cutoff = 0.5){
    
    # Root mean squared error
    RMSE <- function(predicted, labels){sqrt(mean((predicted - labels)^2))}
    
    # Relative square error
    RSE <- function(predicted, labels){mean((predicted - labels)^2) / mean((mean(labels) - labels)^2)}
    
    # R-Square
    RSq <- function(predicted, labels){1 - RSE (predicted, labels)}
    
    # Accuracy
    Acc <- function(predicted, labels, cutoff){mean((predicted > cutoff) == (labels > cutoff))}
    
    return(c(N = length(predicted),
             RMSE = round(RMSE(predicted, labels), 4),
             RSE = round(RSE(predicted, labels), 4),
             RSq = round(RSq(predicted, labels), 4),
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