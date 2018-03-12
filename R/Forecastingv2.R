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


URL.repo=getwd()

if (grepl("Fredrik", URL.repo)){
  URL.drop="C:/Users/Fredrik Hausken/Dropbox/Apper/ShareLaTeX/Master thesis"
}else if (grepl("andersronold", URL.repo)){
  URL.drop="/Users/andersronold/Dropbox/Apper/ShareLaTeX/Master\ thesis" #Anders ma fylle inn
}else{
  URL.drop="Does not find"
}

#INPUT

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
  rollingWindowSize = nrow(stockReturns) - max(sampleSizes)
  
  stocksRunTimeDiagnosticsList=list()
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
    for (garchModelsIndex in 1:length(garchModels)){
      garchModel=garchModels[garchModelsIndex]
      occurence=length(which(garchModelVector == garchModel))
      stocksRunTimeDiagnosticsList[[length(stocksRunTimeDiagnosticsList)+1]]=occurence       
    }
    
    stocksRunTimeDiagnosticsList[[length(stocksRunTimeDiagnosticsList)+1]]=mean(ARLagVector)
    stocksRunTimeDiagnosticsList[[length(stocksRunTimeDiagnosticsList)+1]]=mean(MALagVector)
  }
  
  stocksRunTimeDiagnosticsDataFrame=data.frame(stocks[,1],matrix(stocksRunTimeDiagnosticsList, ncol=(length(garchModels)+2), byrow=TRUE),stringsAsFactors=FALSE)
  names(stocksRunTimeDiagnosticsDataFrame)=c("Stock", garchModels, "Mean AR-LAG","Mean MA-Lag")
  sampleRunTimeDiagnosticsList[[length(sampleRunTimeDiagnosticsList)+1]]=stocksRunTimeDiagnosticsDataFrame
}

names(sampleRunTimeDiagnosticsList)=sampleSizes


#FORECASTS
sampleMeanForecastsDataFramesList=list()
sampleVolatilityForecastsDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = max(sampleSizes)
  rollingWindowSize = nrow(stockReturns) - max(sampleSizes)
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    meanForecastVector=vector()
    volatilityForecastVector=vector()
    for (day in 1:(rollingWindowSize+1)){
      meanForecastVector[length(meanForecastVector)+1]=allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[2]]
      volatilityForecastVector[length(volatilityForecastVector)+1]=allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[9]]
      #print(allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[3]])
    }
    if (stocksIndex==1){
      stockMeanForecastDataFrame=data.frame(meanForecastVector)
      stockVolatilityForecastDataFrame=data.frame(volatilityForecastVector)
    }else{
      stockMeanForecastDataFrame=cbind(stockMeanForecastDataFrame,meanForecastVector)
      stockVolatilityForecastDataFrame=cbind(stockVolatilityForecastDataFrame,volatilityForecastVector)
    }
  }
  
  names(stockMeanForecastDataFrame)=stocks[[1]]
  row.names(stockMeanForecastDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleMeanForecastsDataFramesList[[length(sampleMeanForecastsDataFramesList)+1]]=stockMeanForecastDataFrame
  
  names(stockVolatilityForecastDataFrame)=stocks[[1]]
  row.names(stockVolatilityForecastDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleVolatilityForecastsDataFramesList[[length(sampleVolatilityForecastsDataFramesList)+1]]=stockVolatilityForecastDataFrame
}

names(sampleMeanForecastsDataFramesList)=sampleSizes
names(sampleVolatilityForecastsDataFramesList)=sampleSizes

URL=paste(URL.repo,"/Data/sampleMeanForecastsDataFramesList.Rda",sep="")
save(sampleMeanForecastsDataFramesList,file=URL)
URL=paste(URL.repo,"/Data/sampleVolatilityForecastsDataFramesList.Rda",sep="")
save(sampleVolatilityForecastsDataFramesList,file=URL)

#Buy and Hold Return

sampleBuyAndHoldTotalReturnDataFramesList=list()
sampleBuyAndHoldReturnDataFramesList = list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = max(sampleSizes)
  rollingWindowSize = nrow(stockReturns) - max(sampleSizes)
  
  buyAndHoldDataFrame=data.frame(colSums(stockReturns[(sampleSize+1):nrow(stockReturns)]))
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
standardDevBuyAndHold = list()
meanBuyAndHold = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  varianceDataFrame = data.frame(colVars(sampleBuyAndHoldReturnDataFramesList[[sampleSizesIndex]]))
  standardDeviationDataFrame = data.frame(colSds(sampleBuyAndHoldReturnDataFramesList[[sampleSizesIndex]]))
  meanDataFrame = data.frame(colMeans(sampleBuyAndHoldReturnDataFramesList[[sampleSizesIndex]]))

  colnames(varianceDataFrame) = "Variance buy and hold"
  colnames(standardDeviationDataFrame) = "Standard deviation buy and hold"
  colnames(meanDataFrame) = "Mean buy and hold"
  
  varianceBuyAndHold[[length(varianceBuyAndHold)+1]] = varianceDataFrame
  standardDevBuyAndHold[[length(standardDevBuyAndHold)+1]] = standardDeviationDataFrame
  meanBuyAndHold[[length(meanBuyAndHold)+1]] = meanDataFrame
}

names(varianceBuyAndHold) = sampleSizes
names(standardDevBuyAndHold) = sampleSizes
names(meanBuyAndHold) = sampleSizes

#Error

sampleErrorDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = max(sampleSizes)
  rollingWindowSize = nrow(stockReturns) - max(sampleSizes)
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]

    errorVector=vector()
    
    for (day in 1:(rollingWindowSize)){
      forecast=sampleMeanForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      nextDayReturn=drop(coredata(stockReturns[sampleSize+day,stocksIndex]))
      
      errorVector[length(errorVector)+1]=nextDayReturn-forecast #realized - forecast
      
    }
    
    if (stocksIndex==1){
      errorDataFrame=data.frame(errorVector)
    }else{
      errorDataFrame=cbind(errorDataFrame, errorVector)
      
      
    }
  }
  
 
  names(errorDataFrame)=stocks[[1]]
  row.names(errorDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleErrorDataFramesList[[length(sampleErrorDataFramesList)+1]]=errorDataFrame
  
  
}

names(sampleErrorDataFramesList)=sampleSizes




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
  RMSEDataFrame = data.frame(RMSE(sampleErrorDataFramesList[[sampleSizesIndex]]))
  MAEDataFrame = data.frame(MAE(sampleErrorDataFramesList[[sampleSizesIndex]]))
  
  colnames(RMSEDataFrame) = "RMSE"
  colnames(MAEDataFrame) = "MAE"
  
  sampleRMSEDataFrameList[[length(sampleRMSEDataFrameList)+1]] = RMSEDataFrame
  sampleMAEDataFrameList[[length(sampleMAEDataFrameList)+1]] = MAEDataFrame
}

names(sampleRMSEDataFrameList) = sampleSizes
names(sampleMAEDataFrameList) = sampleSizes

# CREATE RMSE, MAE DATAFRAME FOR LATEX
sampleRMSE.MAE.dataFrame <- data.frame(matrix(c(unlist(sampleRMSEDataFrameList),unlist(sampleMAEDataFrameList)), ncol=4, byrow=F),stringsAsFactors=FALSE)

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
bold <- function(x){
  paste0('{\\bfseries ', x, '}')
}

# STATISTICAL METRICS
 x = sampleRMSE.MAE.dataFrame
# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/statisticalMetrics.txt",sep="")
print(xtable(sampleRMSE.MAE.dataFrame, auto=FALSE, digits=c(1,3,3,3,3), align = c('l','c','c','c','c'), type = "latex", caption = "Statistical metrics "), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)



# SAVE LISTS TO Rda-files

URL=paste(URL.repo,"/Data/meanBuyAndHold.Rda",sep="")
save(meanBuyAndHold,file=URL)

URL=paste(URL.repo,"/Data/standardDevBuyAndHold.Rda",sep="")
save(standardDevBuyAndHold,file=URL)

URL=paste(URL.repo,"/Data/sampleBuyAndHoldTotalReturnDataFramesList.Rda",sep="")
save(sampleBuyAndHoldTotalReturnDataFramesList,file=URL)





# Diagnostics Metrics (Antar tre forskjellige GARCHer)
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  # STATISTICAL METRICS
  x = sampleRunTimeDiagnosticsList[[sampleSizesIndex]]
  # GENERAL LONG-TABLE COMMAND
  add.to.row <- list(pos = list(0), command = NULL)
  command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
  add.to.row$command <- command
  
  URL=paste(URL.drop,"/Tables/modelDiagnostics_",sampleSize,".txt",sep="")
  print(xtable(sampleRunTimeDiagnosticsList[[sampleSizesIndex]], auto=FALSE, digits=c(1,1,0,0,0,2,2), align = c('l','c','c','c','c','c','c'), type = "latex", caption = "Model Diagnostics"), hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
}
