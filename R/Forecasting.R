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


#DIAGNOSTICS
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

#FORECASTS
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
notAccumulatedSampleBuyAndHoldReturnDataFramesList = list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  buyAndHoldDataFrame=data.frame(colSums(stockReturns[sampleSize:nrow(stockReturns)]))
  names(buyAndHoldDataFrame)="Buy and Hold Return"
  sampleBuyAndHoldReturnDataFramesList[[length(sampleBuyAndHoldReturnDataFramesList)+1]]=buyAndHoldDataFrame
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    returnVector = c()
    for (day in 1:(rollingWindowSize)){
      nextDayReturn=drop(coredata(stockReturns[sampleSize+day,stocksIndex]))
      returnVector[length(returnVector)+1] = nextDayReturn
    }
    
    if (stocksIndex==1){
      stockReturnDataFrame=data.frame(returnVector)
    }else{
      stockReturnDataFrame=cbind(stockReturnDataFrame,returnVector)
    }
    
  }
  colnames(stockReturnDataFrame) = stocks[[1]]
  row.names(stockReturnDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  notAccumulatedSampleBuyAndHoldReturnDataFramesList[[length(notAccumulatedSampleBuyAndHoldReturnDataFramesList)+1]] = stockReturnDataFrame
}
names(sampleBuyAndHoldReturnDataFramesList)=sampleSizes
names(notAccumulatedSampleBuyAndHoldReturnDataFramesList) = sampleSizes

#CALCULATE VARIANCE BUY-AND-HOLD
varianceBuyAndHold = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  varianceDataFrame = data.frame(colVars(notAccumulatedSampleBuyAndHoldReturnDataFramesList[[sampleSizesIndex]]))
  colnames(varianceDataFrame) = stocks$Ticker
  varianceBuyAndHold[[length(varianceBuyAndHold)+1]] = varianceDataFrame
}

names(varianceBuyAndHold) = sampleSizes

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
sampleShortLongReturnDataFramesList=list()
sampleAccumulatedShortLongReturnDataFramesList=list()
sampleErrorDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    hitVector=vector()
    shortLongReturnVector = c()
    accumulatedShortLongReturnVector=c(0)
    accumulatedShortLongReturn=0
    longShortReturn = 0
    errorVector=vector()
     
    for (day in 1:(rollingWindowSize)){
      forecast=sampleForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      nextDayReturn=drop(coredata(stockReturns[sampleSize+day,stocksIndex]))
    
      errorVector[length(errorVector)+1]=nextDayReturn-forecast #realized - forecast
      
      if (sign(nextDayReturn)==sign(forecast)){
        hitVector[length(hitVector)+1]=1 #Hit
        accumulatedShortLongReturn=accumulatedShortLongReturn+abs(nextDayReturn)
        shortLongReturnVector[length(shortLongReturnVector)+1] = abs(nextDayReturn)
        
      }else{
        hitVector[length(hitVector)+1]=0 #Miss
        accumulatedShortLongReturn=accumulatedShortLongReturn-abs(nextDayReturn)
        shortLongReturnVector[length(shortLongReturnVector)+1] = -abs(nextDayReturn)
      }
      
      accumulatedShortLongReturnVector[length(accumulatedShortLongReturnVector)+1]=accumulatedShortLongReturn
    }
    
    if (stocksIndex==1){
      stockHitDataFrame=data.frame(hitVector)
      longShortReturnDataFrame = data.frame(shortLongReturnVector)
      accumulatedShortLongReturnDataFrame=data.frame(accumulatedShortLongReturnVector)
      errorDataFrame=data.frame(errorVector)
    }else{
      stockHitDataFrame=cbind(stockHitDataFrame,hitVector)
      longShortReturnDataFrame = cbind(longShortReturnDataFrame, shortLongReturnVector)
      accumulatedShortLongReturnDataFrame=cbind(accumulatedShortLongReturnDataFrame,accumulatedShortLongReturnVector)
      errorDataFrame=cbind(errorDataFrame, errorVector)
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
  
  names(longShortReturnDataFrame)=stocks[[1]]
  row.names(longShortReturnDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleShortLongReturnDataFramesList[[length(sampleShortLongReturnDataFramesList)+1]]=longShortReturnDataFrame
  
}
names(sampleHitDataFramesList)=sampleSizes
names(sampleErrorDataFramesList)=sampleSizes
names(sampleAccumulatedShortLongReturnDataFramesList)=sampleSizes
names(sampleShortLongReturnDataFramesList)=sampleSizes

#CALCULATE VARIANCE SHORT-LONG RETURN
varianceLongShort = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  varianceDataFrame = data.frame(colVars(sampleShortLongReturnDataFramesList[[sampleSizesIndex]]))
  colnames(varianceDataFrame) = stocks$Ticker
  varianceLongShort[[length(varianceLongShort)+1]] = varianceDataFrame
}
names(varianceLongShort) = sampleSizes

#Short/Sell Long Hit Ratio

sampleHitRatioDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleHitRatioDataFramesList[[length(sampleHitRatioDataFramesList)+1]]=data.frame((colSums(sampleHitDataFramesList[[sampleSizesIndex]]))/nrow(sampleHitDataFramesList[[sampleSizesIndex]]))
}
names(sampleHitRatioDataFramesList)=sampleSizes

# STATISTICAL METRICS

# RMSE function
RMSE <- function(errorListStock) {
  return(sqrt(colMeans(errorListStock^2)))
}

# MAE function
MAE <- function(errorListStock) {
  return(colMeans(abs(errorListStock)))
}


sampleRMSEDataFrameList=list()
sampleMAEDataFrameList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleRMSEDataFrameList[[length(sampleRMSEDataFrameList)+1]] = RMSE(sampleErrorDataFramesList[[sampleSizesIndex]])
  sampleMAEDataFrameList[[length(sampleMAEDataFrameList)+1]] = MAE(sampleErrorDataFramesList[[sampleSizesIndex]])
}

# CREATE RMSE, MAE DATA FRAME FOR LATEX
sampleRMSE.MAE.dataFrame <- data.frame(matrix(c(unlist(sampleRMSEDataFrameList),unlist(sampleMAEDataFrameList)), nrow=1, byrow=T),stringsAsFactors=FALSE)

# ADD RMSE COL NAME
sampleSizeNameList = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSizeNameList[[length(sampleSizeNameList)+1]] = paste("RMSE with ","sample size ",sampleSizes[[sampleSizesIndex]]," days")
}

# ADD MAE COL NAME
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSizeNameList[[length(sampleSizeNameList)+1]] = paste("MAE with ","sample size ",sampleSizes[[sampleSizesIndex]]," days")
}

# ADD STOCK TICKER NAMES
stockNameList = list()
for (stocksIndex in 1:nrow(stocks)){
  stockNameList[[length(stockNameList)+1]]=stocks[stocksIndex,1]
}

# ASSIGN NAMES TO ROWS AND COLS
colnames(sampleRMSE.MAE.dataFrame) = sampleSizeNameList
row.names(sampleRMSE.MAE.dataFrame) = unlist(stockNameList)



# TABLES-TO-LATEX

# STATISTICAL METRICS
x = sampleRMSE.MAE.dataFrame
# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/statisticalMetrics.txt",sep="")
print(xtable(sampleRMSE.MAE.dataFrame, auto=FALSE, digits=c(1,3,3,3,3), align = c('l','c','c','c','c'), type = "latex", caption = "Statistical metrics "), hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
