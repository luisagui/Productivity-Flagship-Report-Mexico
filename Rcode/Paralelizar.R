libarary(parallel)
numCores <- detectCores()
cl <- makeCluster(numCores)
clusterEvalQ(cl, DT1)
clusterExport(cl, "DT1")

clusterEvalQ(cl, {
  library(ggplot2)
  library(stringr)
})

parSapply(cl, Orange, mean, na.rm = TRUE)
stopCluster(cl)