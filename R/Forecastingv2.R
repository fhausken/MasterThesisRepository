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
library(ggplot2)
library(plotly)
library(webshot)


no_cores=detectCores()
c1=makeCluster(no_cores)
registerDoParallel(c1)


URL.repo=getwd()

if (grepl("Fredrik", URL.repo)){
  URL.drop="C:/Users/Fredrik Hausken/Dropbox/Apper/ShareLaTeX/Master thesis"
}else if (grepl("andersronold", URL.repo)){
  URL.drop="/Users/andersronold/Dropbox/Apper/ShareLaTeX/Master\ thesis" #Anders ma fylle inn
}else{
  URL.drop="Does not find"
}

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

#Short/Sell Long Sign

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
sampleAccumulatedBuyAndHoldReturnDataFramesList=list()
sampleAccumulatedAlphaReturnDataFramesList=list()
sampleErrorDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    hitVector=vector()
    accumulatedShortLongReturnVector=c(0)
    accumulatedShortLongReturn=0
    accumulatedBuyAndHoldReturnVector=c(0)
    accumulatedBuyAndHoldReturn=0
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
      accumulatedBuyAndHoldReturn=accumulatedBuyAndHoldReturn+nextDayReturn
      accumulatedBuyAndHoldReturnVector[length(accumulatedBuyAndHoldReturnVector)+1]=accumulatedBuyAndHoldReturn
    }
    
    if (stocksIndex==1){
      stockHitDataFrame=data.frame(hitVector)
      accumulatedShortLongReturnDataFrame=data.frame(accumulatedShortLongReturnVector)
      accumulatedBuyAndHoldReturnDataFrame=data.frame(accumulatedBuyAndHoldReturnVector)
      accumulatedAlphaReturnDataFrame=data.frame((accumulatedShortLongReturnVector-accumulatedBuyAndHoldReturnVector))
      errorDataFrame=data.frame(errorVector)
    }else{
      stockHitDataFrame=cbind(stockHitDataFrame,hitVector)
      accumulatedShortLongReturnDataFrame=cbind(accumulatedShortLongReturnDataFrame,accumulatedShortLongReturnVector)
      errorDataFrame=cbind(errorDataFrame, errorVector)
      accumulatedBuyAndHoldReturnDataFrame=cbind(accumulatedBuyAndHoldReturnDataFrame,accumulatedBuyAndHoldReturnVector)
      accumulatedAlphaReturnDataFrame=cbind(accumulatedAlphaReturnDataFrame,(accumulatedShortLongReturnVector-accumulatedBuyAndHoldReturnVector))
    }
  }
  
  names(stockHitDataFrame)=stocks[[1]]
  row.names(stockHitDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleHitDataFramesList[[length(sampleHitDataFramesList)+1]]=stockHitDataFrame
  
  names(errorDataFrame)=stocks[[1]]
  row.names(errorDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleErrorDataFramesList[[length(sampleErrorDataFramesList)+1]]=errorDataFrame
  
  names(accumulatedShortLongReturnDataFrame)=stocks[[1]]
  row.names(accumulatedShortLongReturnDataFrame)=index(stockReturns)[(sampleSize):nrow(stockReturns)]
  sampleAccumulatedShortLongReturnDataFramesList[[length(sampleAccumulatedShortLongReturnDataFramesList)+1]]=accumulatedShortLongReturnDataFrame
  
  names(accumulatedBuyAndHoldReturnDataFrame)=stocks[[1]]
  row.names(accumulatedBuyAndHoldReturnDataFrame)=index(stockReturns)[(sampleSize):nrow(stockReturns)]
  sampleAccumulatedBuyAndHoldReturnDataFramesList[[length(sampleAccumulatedBuyAndHoldReturnDataFramesList)+1]]=accumulatedBuyAndHoldReturnDataFrame
  
  names(accumulatedAlphaReturnDataFrame)=stocks[[1]]
  row.names(accumulatedAlphaReturnDataFrame)=index(stockReturns)[(sampleSize):nrow(stockReturns)]
  sampleAccumulatedAlphaReturnDataFramesList[[length(sampleAccumulatedAlphaReturnDataFramesList)+1]]=accumulatedAlphaReturnDataFrame
  
  
}
names(sampleHitDataFramesList)=sampleSizes
names(sampleErrorDataFramesList)=sampleSizes
names(sampleAccumulatedShortLongReturnDataFramesList)=sampleSizes
names(sampleAccumulatedBuyAndHoldReturnDataFramesList)=sampleSizes

#Short/Sell Long Hit Ratio

sampleHitRatioDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleHitRatioDataFramesList[[length(sampleHitRatioDataFramesList)+1]]=data.frame((colSums(sampleHitDataFramesList[[sampleSizesIndex]]))/nrow(sampleHitDataFramesList[[sampleSizesIndex]]))
}
names(sampleHitRatioDataFramesList)=sampleSizes

# STATISTICAL METRICS

#Plotting

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    
    accumulatedShortLongReturnVector=drop(sampleAccumulatedShortLongReturnDataFramesList[[sampleSizesIndex]][,stocksIndex])
    accumulatedBuyAndHoldReturnVector=drop(sampleAccumulatedBuyAndHoldReturnDataFramesList[[sampleSizesIndex]][,stocksIndex])
    accumulatedAlphaReturnVector=drop(sampleAccumulatedAlphaReturnDataFramesList[[sampleSizesIndex]][,stocksIndex])
    rowNamesAccumulatedShortLongReturnVector=as.Date(row.names(sampleAccumulatedShortLongReturnDataFramesList[[sampleSizesIndex]]))
    plotDataFrame=data.frame(dates=rowNamesAccumulatedShortLongReturnVector,buyAndHold=accumulatedBuyAndHoldReturnVector, shortLong=accumulatedShortLongReturnVector, alpha=accumulatedAlphaReturnVector)
    
    subplotOne=plot_ly(plotDataFrame, x=~dates) %>%
      add_trace(y = ~buyAndHold, name = 'Buy and Hold Strategy',type='scatter',mode = 'lines') %>%
      add_trace(y = ~shortLong, name = 'Short Long Strategy',type='scatter', mode = 'lines')%>%
      layout(legend = list(x = 100, y = 0.5), yaxis=list(title="Return"))
    
    subplotTwo=plot_ly(plotDataFrame, x=~dates) %>%
      add_trace(y = ~alpha, name = 'Alpha',type='scatter',mode = 'lines')%>%
      layout(legend = list(x = 100, y = 0.5),yaxis=list(title="Return"), xaxis=list(title="Date"))
    
    
    fullPlot=subplot(nrows=2,subplotOne,subplotTwo, shareX = TRUE, heights = c(0.75,0.25), titleX = TRUE, titleY = TRUE)
    
    URL=paste(URL.drop,"/Plot/",stockName,"_",sampleSize,".jpeg",sep="")
    export(fullPlot, file = URL)
  }
}