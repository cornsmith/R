require(randomForest); require(foreach); require(doParallel)

# function for adding NAs indicators to dataframe and replacing NA's with a value
#"cols" is vector of columns to operate on (necessary for randomForest package)
appendNAs <- function(df, cols){
#   append_these <- data.frame(is.na(df[ , cols]) * 1)
  append_these <- data.frame(is.na(df[ , cols]))
  names(append_these) <- paste(names(append_these), "NA", sep = "_")
  for(i in cols){
      if (class(df[ , i]) == "factor"){
          levels(df[ , i]) <- c(levels(df[ , i]), "NA")
          df[ , i][is.na(df[ , i])] <- "NA"
      } else {
          df[ , i][is.na(df[ , i])] <-  -1
      }
  }
  df <- cbind(df, append_these)
  return(df)
}

RForest <- function(df, ntree, sampsize, procs = 2){
    cl <- makePSOCKcluster(procs); registerDoParallel(cl)
    model <- foreach(ntree = rep(ntree / procs, procs), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar%
        randomForest(Pred ~ ., data = df, sampsize = sampsize, importance = TRUE, ntree = ntree)
    stopCluster(cl)
    return(model)
}