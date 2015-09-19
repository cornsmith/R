require(doParallel)
cl <- makeCluster(2)  # Use 2 cores
registerDoParallel(cl) # register these 2 cores with the "foreach" package