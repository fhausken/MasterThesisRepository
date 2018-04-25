library(parallel,warn.conflicts = FALSE)
library(doParallel,warn.conflicts = FALSE)
library(quantmod,warn.conflicts = FALSE)
library(lattice,warn.conflicts = FALSE)
library(timeSeries,warn.conflicts = FALSE)
library(rugarch,warn.conflicts = FALSE)
library(xts,warn.conflicts = FALSE)
library(tseries,warn.conflicts = FALSE)
library(bigmemory,warn.conflicts = FALSE)
library(R.utils,warn.conflicts = FALSE)
library(sys,warn.conflicts = FALSE)

fit = tryCatch({
  fitGARCH= eval_safe({while (TRUE){print("Ja")}},timeout=3)}, error=function(e) e, warning=function(w) w)

if(is(fit,"error")){
  if(grepl("timeout",fit$message)==TRUE){
    print("Timeout")
  }else{
    print("Other error")
  }
  
  save(fit,file="test.RData")
  
  #cat(paste(Sys.time(), "\t\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Warning i fit!","\n",sep=""), file=URL.logging, append=TRUE)
  
}

