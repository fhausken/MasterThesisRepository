rm(list=ls()) #Clears environment

library(parallel)
library(doParallel)
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
library(xts)
library(tseries)
library(rugarch)

no_cores=detectCores()
c1=makeCluster(no_cores)
registerDoParallel(c1)


URL.repo=getwd()

URL=paste(URL.repo,"/Data/ARMAGARCHResults_NewParallell.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleSizes_NewParallell.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/garchModels_NewParallell.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocksRemoved.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/distributionFitResults.Rda",sep="")
load(URL)

sampleForecastsDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
#sampleForecastsDataFramesList=foreach(sampleSizesIndex=1:length(sampleSizes)) %dopar%{
  
  library(parallel)
  library(doParallel)
  library(quantmod)
  library(lattice)
  library(timeSeries)
  library(rugarch)
  library(xts)
  library(tseries)
  library(rugarch)

  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    forecastVector=vector()
    for (day in 1:(rollingWindowSize+1)){
      forecastVector[length(forecastVector)+1]=allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[2]]
      #print(allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[3]])
    }
    if (stocksIndex==1){
      stockForecastDataFrame=data.frame(forecastVector)
    }else{
      stockForecastDataFrame=cbind(stockForecastDataFrame,forecastVector)
    }
  }
 
  names(stockForecastDataFrame)=stocks[[1]]
  row.names(stockForecastDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleForecastsDataFramesList[[length(sampleForecastsDataFramesList)+1]]=stockForecastDataFrame
  #return(stockForecastDataFrame)
}

names(sampleForecastsDataFramesList)=sampleSizes