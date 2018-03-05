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
#library(ggplot2)

no_cores=detectCores()
c1=makeCluster(no_cores)
registerDoParallel(c1)


URL.repo=getwd()

URL=paste(URL.repo,"/Data/ARMAGARCHResults.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleSizes.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/garchModels.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocksRemoved.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/distributionFitResults.Rda",sep="")
load(URL)

#Diagnostics
sampleRunTimeDiagnosticsList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  stockRunTimeDiagnosticsList=list()
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    garchModelVector=vector()
    ARLagVector=vector()
    MALagVector=vector()
    for (day in 1:(rollingWindowSize+1)){
      garchModelVector[length(garchModelVector)+1]=allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[3]]
      ARLagVector[length(ARLagVector)+1]=allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[4]]
      MALagVector[length(MALagVector)+1]=allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[5]]
    }
    occurences=table(garchModelVector)
    averageARLag=mean(ARLagVector)
    averageMALag=mean(MALagVector)
    runTimeDiagnosticsList=list(occurences,averageARLag,averageMALag)
    names(runTimeDiagnosticsList)=c("Occurences", "Average AR Lag", "Average MA Lag")
    stockRunTimeDiagnosticsList[[length(stockRunTimeDiagnosticsList)+1]]=runTimeDiagnosticsList
    
  }
  
  names(stockRunTimeDiagnosticsList)=stocks[[1]]
  sampleRunTimeDiagnosticsList[[length(sampleRunTimeDiagnosticsList)+1]]=stockRunTimeDiagnosticsList
}

names(sampleRunTimeDiagnosticsList)=sampleSizes

#Forecats

sampleForecastsDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize

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
}

names(sampleForecastsDataFramesList)=sampleSizes

#Buy and Hold Return

sampleBuyAndHoldReturnDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  buyAndHoldDataFrame=data.frame(colSums(stockReturns[sampleSize:nrow(stockReturns)]))
  names(buyAndHoldDataFrame)="Buy and Hold Return"
  sampleBuyAndHoldReturnDataFramesList[[length(sampleBuyAndHoldReturnDataFramesList)+1]]=buyAndHoldDataFrame
}
names(sampleBuyAndHoldReturnDataFramesList)=sampleSizes

#Short/Sell Long Hit

sampleSignDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  shortLongVector=vector()
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    signVector=vector()
    for (day in 1:(rollingWindowSize+1)){
      forecast=sampleForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      signOfForecast=sign(forecast)
      
      if (signOfForecast==1){
        signVector[length(signVector)+1]=1 #Long
      }else{
        signVector[length(signVector)+1]=0 #Short or Sell
      }
  
    }
    if (stocksIndex==1){
      stockSignDataFrame=data.frame(signVector)
    }else{
      stockSignDataFrame=cbind(stockSignDataFrame,signVector)
    }
  }
  
  names(stockSignDataFrame)=stocks[[1]]
  row.names(stockSignDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleSignDataFramesList[[length(sampleSignDataFramesList)+1]]=stockSignDataFrame
  
}
names(sampleSignDataFramesList)=sampleSizes

#Short/Sell Long Hit and Return

sampleHitDataFramesList=list()
sampleAccumulatedShortLongReturnDataFramesList=list()
sampeErrorDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    hitVector=vector()
    accumulatedShortLongReturnVector=c(0)
    accumulatedShortLongReturn=0
    errorVector=vector()
     
    for (day in 1:(rollingWindowSize)){
      forecast=sampleForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      nextDayReturn=drop(coredata(stockReturns[sampleSize+day,stocksIndex]))
      
      errorVector[length(errorVector)+1]=nextDayReturn-forecast #realized - forecast
      
      if (sign(nextDayReturn)==sign(forecast)){
        hitVector[length(hitVector)+1]=1 #Hit
        accumulatedShortLongReturn=accumulatedShortLongReturn+abs(nextDayReturn)
        
      }else{
        hitVector[length(hitVector)+1]=0 #Miss
        accumulatedShortLongReturn=accumulatedShortLongReturn-abs(nextDayReturn)
      }
      
      accumulatedShortLongReturnVector[length(accumulatedShortLongReturnVector)+1]=accumulatedShortLongReturn
    }
    
    if (stocksIndex==1){
      stockHitDataFrame=data.frame(hitVector)
      accumulatedShortLongReturnDataFrame=data.frame(accumulatedShortLongReturnVector)
      errorDataFrame=data.frame(errorVector)
    }else{
      stockHitDataFrame=cbind(stockHitDataFrame,hitVector)
      accumulatedShortLongReturnDataFrame=cbind(accumulatedShortLongReturnDataFrame,accumulatedShortLongReturnVector)
      errorDataFrame=cbind(errorDataFrame, errorVector)
    }
  }
  
  names(stockHitDataFrame)=stocks[[1]]
  row.names(stockHitDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleHitDataFramesList[[length(sampleHitDataFramesList)+1]]=stockHitDataFrame
  
  names(errorDataFrame)=stocks[[1]]
  row.names(errorDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampeErrorDataFramesList[[length(sampeErrorDataFramesList)+1]]=errorDataFrame
  
  names(accumulatedShortLongReturnDataFrame)=stocks[[1]]
  row.names(accumulatedShortLongReturnDataFrame)=index(stockReturns)[(sampleSize):nrow(stockReturns)]
  sampleAccumulatedShortLongReturnDataFramesList[[length(sampleAccumulatedShortLongReturnDataFramesList)+1]]=accumulatedShortLongReturnDataFrame
  
}
names(sampleHitDataFramesList)=sampleSizes
names(sampeErrorDataFramesList)=sampleSizes
names(sampleAccumulatedShortLongReturnDataFramesList)=sampleSizes

#Short/Sell Long Hit Ratio

sampleHitRatioDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleHitRatioDataFramesList[[length(sampleHitRatioDataFramesList)+1]]=data.frame((colSums(sampleHitDataFramesList[[sampleSizesIndex]]))/nrow(sampleHitDataFramesList[[sampleSizesIndex]]))
}
names(sampleHitRatioDataFramesList)=sampleSizes

# STATISTICAL METRICS


