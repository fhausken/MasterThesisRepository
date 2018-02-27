library(parallel)


no_cores=detectCores()
wd=getwd()

print(paste("Number of cores:",no_cores))
print(paste("Working directory:",wd))