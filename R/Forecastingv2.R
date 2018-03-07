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

sampleBuyAndHoldTotalReturnDataFramesList=list()
sampleBuyAndHoldReturnDataFramesList = list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  buyAndHoldDataFrame=data.frame(colSums(stockReturns[sampleSize:nrow(stockReturns)]))
  names(buyAndHoldDataFrame)="Buy and Hold Return"
  sampleBuyAndHoldTotalReturnDataFramesList[[length(sampleBuyAndHoldTotalReturnDataFramesList)+1]]=buyAndHoldDataFrame

  
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
  sampleBuyAndHoldReturnDataFramesList[[length(sampleBuyAndHoldReturnDataFramesList)+1]] = stockReturnDataFrame
}
names(sampleBuyAndHoldTotalReturnDataFramesList)=sampleSizes
names(sampleBuyAndHoldReturnDataFramesList) = sampleSizes

#CALCULATE MEAN & VARIANCE BUY-AND-HOLD
varianceBuyAndHold = list()
meanBuyAndHold = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  varianceDataFrame = data.frame(colVars(sampleBuyAndHoldReturnDataFramesList[[sampleSizesIndex]]))
  meanDataFrame = data.frame(colMeans(sampleBuyAndHoldReturnDataFramesList[[sampleSizesIndex]]))
  
  colnames(varianceDataFrame) = stocks$Ticker
  colnames(meanDataFrame) = stocks$Ticker
  
  varianceBuyAndHold[[length(varianceBuyAndHold)+1]] = varianceDataFrame
  meanBuyAndHold[[length(meanBuyAndHold)+1]] = meanDataFrame
}

names(varianceBuyAndHold) = sampleSizes
names(meanBuyAndHold) = sampleSizes

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
sampleAccumulatedBuyAndHoldReturnDataFramesList=list()
sampleAccumulatedAlphaReturnDataFramesList=list()
sampleErrorDataFramesList=list()
sampleShortLongReturnDataFramesList=list()

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
    shortLongReturnVector = c()
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
      accumulatedBuyAndHoldReturn=accumulatedBuyAndHoldReturn+nextDayReturn
      accumulatedBuyAndHoldReturnVector[length(accumulatedBuyAndHoldReturnVector)+1]=accumulatedBuyAndHoldReturn

    }
    
    if (stocksIndex==1){
      stockHitDataFrame=data.frame(hitVector)

      accumulatedShortLongReturnDataFrame=data.frame(accumulatedShortLongReturnVector)
      accumulatedBuyAndHoldReturnDataFrame=data.frame(accumulatedBuyAndHoldReturnVector)
      accumulatedAlphaReturnDataFrame=data.frame((accumulatedShortLongReturnVector-accumulatedBuyAndHoldReturnVector))
      errorDataFrame=data.frame(errorVector)
      longShortReturnDataFrame = data.frame(shortLongReturnVector)
    }else{
      stockHitDataFrame=cbind(stockHitDataFrame,hitVector)
      accumulatedShortLongReturnDataFrame=cbind(accumulatedShortLongReturnDataFrame,accumulatedShortLongReturnVector)
      errorDataFrame=cbind(errorDataFrame, errorVector)
      accumulatedBuyAndHoldReturnDataFrame=cbind(accumulatedBuyAndHoldReturnDataFrame,accumulatedBuyAndHoldReturnVector)
      accumulatedAlphaReturnDataFrame=cbind(accumulatedAlphaReturnDataFrame,(accumulatedShortLongReturnVector-accumulatedBuyAndHoldReturnVector))
      longShortReturnDataFrame = cbind(longShortReturnDataFrame, shortLongReturnVector)

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
  

  names(longShortReturnDataFrame)=stocks[[1]]
  row.names(longShortReturnDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleShortLongReturnDataFramesList[[length(sampleShortLongReturnDataFramesList)+1]]=longShortReturnDataFrame

  
}
names(sampleHitDataFramesList)=sampleSizes
names(sampleErrorDataFramesList)=sampleSizes
names(sampleAccumulatedShortLongReturnDataFramesList)=sampleSizes
names(sampleAccumulatedBuyAndHoldReturnDataFramesList)=sampleSizes
names(sampleShortLongReturnDataFramesList)=sampleSizes

#CALCULATE MEAN & VARIANCE SHORT-LONG RETURN
varianceLongShort = list()
meanLongShort = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  meanLongShortDataFrame = data.frame(colMeans(sampleShortLongReturnDataFramesList[[sampleSizesIndex]]))
  varianceDataFrame = data.frame(colVars(sampleShortLongReturnDataFramesList[[sampleSizesIndex]]))
  
  colnames(meanLongShortDataFrame) = stocks$Stock
  colnames(varianceDataFrame) = stocks$Stock
  
  varianceLongShort[[length(varianceLongShort)+1]] = varianceDataFrame
  meanLongShort[[length(meanLongShort)+1]] = meanLongShortDataFrame
}

names(varianceLongShort) = sampleSizes
names(meanLongShort) = sampleSizes


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
  RMSEDataFrame = data.frame(RMSE(sampleErrorDataFramesList[[sampleSizesIndex]]))
  MAEDataFrame = data.frame(MAE(sampleErrorDataFramesList[[sampleSizesIndex]]))
  
  colnames(RMSEDataFrame) = stocks$Stock
  colnames(MAEDataFrame) = stocks$Stock
  
  sampleRMSEDataFrameList[[length(sampleRMSEDataFrameList)+1]] = RMSEDataFrame
  sampleMAEDataFrameList[[length(sampleMAEDataFrameList)+1]] = MAEDataFrame
}

names(sampleRMSEDataFrameList) = sampleSizes
names(sampleMAEDataFrameList) = sampleSizes

# CREATE RMSE, MAE DATAFRAME FOR LATEX
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


# CREATE INFORMATION METRIC TABLE (Stock, mean_buy-and-hold, std.dev_buy-and-hold, r_buy-and-hold, Sign Ratio, mean_short-long, std.dev_short-long, return_short-long, alpha, SR_buy-and-hold, SR_short-long)
informationDataFrameList = list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  informationDataFrame = data.frame(stocks[[1]], meanBuyAndHold[[sampleSizesIndex]], varianceBuyAndHold[[sampleSizesIndex]], sampleBuyAndHoldTotalReturnDataFramesList[[sampleSizesIndex]], meanLongShort[[sampleSizesIndex]], varianceLongShort[[sampleSizesIndex]]) #, sampleShortLongReturnDataFramesList[[sampleSizesIndex]])
  
  colnames(informationDataFrame) = c("Stock","Buy-and-hold mean", "Buy-and-hold std.dev","Buy-and-hold return", "Short-long mean", "Short-long std.dev")#, "Short-long return") #, "Alpha", "Buy-and-hold SR", "Long-short SR")
  
  informationDataFrameList[[length(informationDataFrameList)+1]] = informationDataFrame
  
}

names(informationDataFrameList) = sampleSizes

# # TABLES-TO-LATEX
# 
# # STATISTICAL METRICS
# x = sampleRMSE.MAE.dataFrame
# # GENERAL LONG-TABLE COMMAND
# add.to.row <- list(pos = list(0), command = NULL)
# command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
# add.to.row$command <- command
# 
# URL=paste(URL.drop,"/Tables/statisticalMetrics.txt",sep="")
# print(xtable(sampleRMSE.MAE.dataFrame, auto=FALSE, digits=c(1,3,3,3,3), align = c('l','c','c','c','c'), type = "latex", caption = "Statistical metrics "), hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
# 
# 
# # RETURN, VARIANCE, SIGN RATIO AND ALPHA METRICS FOR ALL STOCKS
# x = informationDataFrame
# # GENERAL LONG-TABLE COMMAND
# add.to.row <- list(pos = list(0), command = NULL)
# command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
# add.to.row$command <- command
# 
# URL=paste(URL.drop,"/Tables/statisticalMetrics.txt",sep="")
# print(xtable(sampleRMSE.MAE.dataFrame, auto=FALSE, digits=c(1,3,3,3,3), align = c('l','c','c','c','c'), type = "latex", caption = "Statistical metrics "), hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

