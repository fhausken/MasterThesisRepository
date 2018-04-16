library(bigmemory)
daysLeft=seq(from = 0, to = 100, by = 1)
daysLeft=as.character(daysLeft)
daysLeft[length(daysLeft)+1]="Slutt"

URL.repo=getwd()
URL=paste(URL.repo,"/Data/bigmemory",sep="")

daysLeft=as.big.matrix(daysLeft, type = "char")

daysLeft[1]

fit = tryCatch({
  fitGARCH= withTimeout({while(3>2){print ("A")}},timeout=2,elapsed = 2,onTimeout="error")}, error=function(e) e, warning=function(w) w)

if(is(fit,"warning")){
  
  #cat(paste(Sys.time(), "\t\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Warning i fit!","\n",sep=""), file=URL.logging, append=TRUE)
  
} else if(is(fit,"error")){
  
  
  class(fit)[1]=="TimeoutException"
  
  
} else{
  
}

URL=paste(URL.repo,"/Output/Warning.Rda", sep="")
load(URL)
dayFinished