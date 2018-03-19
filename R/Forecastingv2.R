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
library(xtable)


URL.repo=getwd()

if (grepl("Fredrik", URL.repo)){
  URL.drop="C:/Users/Fredrik Hausken/Dropbox/Apper/ShareLaTeX/Master thesis"
}else if (grepl("andersronold", URL.repo)){
  URL.drop="/Users/andersronold/Dropbox/Apper/ShareLaTeX/Master\ thesis" #Anders ma fylle inn
}else{
  URL.drop="Does not find"
}

#INPUT

PLOTTING=TRUE

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
sampleARLagDataFrameList=list()
sampleMALagDataFrameList=list()
sampleGARCHModelDataFrameList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = max(sampleSizes)
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
      stocksRunTimeDiagnosticsList[[length(stocksRunTimeDiagnosticsList)+1]]=occurence/(rollingWindowSize+1)       
    }
    
    stocksRunTimeDiagnosticsList[[length(stocksRunTimeDiagnosticsList)+1]]=mean(ARLagVector)
    stocksRunTimeDiagnosticsList[[length(stocksRunTimeDiagnosticsList)+1]]=mean(MALagVector)
    
    if (stocksIndex==1){
      ARLagDataFrame=data.frame(ARLagVector)
      MALagDataFrame=data.frame(MALagVector)
      GARCHModelDataFrame=data.frame(garchModelVector)
    }else{
      ARLagDataFrame=cbind(ARLagDataFrame,ARLagVector)
      MALagDataFrame=cbind(MALagDataFrame,MALagVector)
      GARCHModelDataFrame=cbind(GARCHModelDataFrame,garchModelVector)
    }
  }
  
  stocksRunTimeDiagnosticsDataFrame=data.frame(stocks[,1],matrix(stocksRunTimeDiagnosticsList, ncol=(length(garchModels)+2), byrow=TRUE),stringsAsFactors=FALSE)
  names(stocksRunTimeDiagnosticsDataFrame)=c("Stock", garchModels, "Mean AR-LAG","Mean MA-Lag")
  sampleRunTimeDiagnosticsList[[length(sampleRunTimeDiagnosticsList)+1]]=stocksRunTimeDiagnosticsDataFrame
  
  ARLagDataFrame=cbind(ARLagDataFrame,(rowMeans(ARLagDataFrame)))
  names(ARLagDataFrame)=c(stocks[[1]], "Row Mean")
  row.names(ARLagDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleARLagDataFrameList[[length(sampleARLagDataFrameList)+1]]=ARLagDataFrame
  
  MALagDataFrame=cbind(MALagDataFrame,(rowMeans(MALagDataFrame)))
  names(MALagDataFrame)=c(stocks[[1]], "Row Mean")
  row.names(MALagDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleMALagDataFrameList[[length(sampleMALagDataFrameList)+1]]=MALagDataFrame
 
  
  for (garchModelsIndex in 1:length(garchModels)){
    garchModelCountVector=c()
    for (day in 1:(rollingWindowSize+1)){
      garchModel=garchModels[garchModelsIndex]
      occurence=length(which(GARCHModelDataFrame[day,] == garchModel))
      garchModelCountVector[length(garchModelCountVector)+1]=occurence
    }
    GARCHModelDataFrame=cbind(GARCHModelDataFrame,garchModelCountVector)

  }
  names(GARCHModelDataFrame)=c(stocks[[1]], garchModels)
  row.names(GARCHModelDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleGARCHModelDataFrameList[[length(sampleGARCHModelDataFrameList)+1]]=GARCHModelDataFrame

}

names(sampleRunTimeDiagnosticsList)=sampleSizes
names(sampleARLagDataFrameList)=sampleSizes
names(sampleMALagDataFrameList)=sampleSizes
names(sampleGARCHModelDataFrameList)=sampleSizes

# CALCULATE MEAN FUNCTION
getColMeans <- function(dataFrame) {
  endVector = c()
  for (i in 1:ncol(dataFrame)) {
    if (is.numeric(dataFrame[1,i])) {
      endVector = cbind(endVector,mean(dataFrame[,i]))
    }
    
  }
  return(data.frame(endVector))
}

# CALCULATE DIAGNOSTIC MEAN FUNCTION
getDiagColMeans <- function(dataFrame) {
  endVector = c()
  for (i in 1:ncol(dataFrame)) {
    if (is.numeric(dataFrame[1,i][[1]])) {
      print(dataFrame[,i])
      endVector = cbind(endVector,mean(unlist(dataFrame[,i]))) 
    }
    
  }
  return(data.frame(endVector))
}


# CREATE DIAGNOSTIC AVERAGE LINE LIST
sampleRunTimeDiagnosticsList.average = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleRunTimeDiagnosticsList.average.dataFrame = getDiagColMeans(sampleRunTimeDiagnosticsList[[sampleSizesIndex]])
  names(sampleRunTimeDiagnosticsList.average.dataFrame) = colnames(sampleRunTimeDiagnosticsList[[sampleSizesIndex]][-c(1)])
  
  sampleRunTimeDiagnosticsList.average[[length(sampleRunTimeDiagnosticsList.average)+1]] = sampleRunTimeDiagnosticsList.average.dataFrame
}

names(sampleRunTimeDiagnosticsList.average)=sampleSizes

  
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
      # print(paste(stockName,": ",day,"/",(rollingWindowSize+1),sep=""))
      # print(allStocksResults[[stocksIndex]][[sampleSizesIndex]][[day]][[8]])
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

#Average MAE and RMSE for plotting
sampleAverageMAEPlotDataFrameList=list()
sampleAverageRMSEPlotDataFrameList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = max(sampleSizes)
  rollingWindowSize = nrow(stockReturns) - max(sampleSizes)
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    
    MAEVector=vector()
    RMSEVector=vector()
    
    for (day in 1:(rollingWindowSize)){
      error=sampleErrorDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      MAEVector[length(MAEVector)+1]=abs(error)
      RMSEVector[length(RMSEVector)+1]=sqrt(error^2)
      
      
      # print(paste(stockName,": ",day,"/",rollingWindowSize,sep=""))
      # print(abs(error))
      # print(sqrt(error^2))
      
    }
    
    if (stocksIndex==1){
      MAEDataFrame=data.frame(MAEVector)
      RMSEDataFrame=data.frame(RMSEVector)
    }else{
      MAEDataFrame=cbind(MAEDataFrame, MAEVector)
      RMSEDataFrame=cbind(RMSEDataFrame, RMSEVector)
    }
  }
  names(MAEDataFrame)=stocks[[1]]
  names(RMSEDataFrame)=stocks[[1]]
  row.names(MAEDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  row.names(RMSEDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  
  MAEDates=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  RMSEDates=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  MAEPlotDataFrame=data.frame(dates=MAEDates,MAEMean=rowMeans(MAEDataFrame))
  RMSEPlotDataFrame=data.frame(dates=RMSEDates,RMSEMean=rowMeans(RMSEDataFrame))
  

  sampleAverageMAEPlotDataFrameList[[length(sampleAverageMAEPlotDataFrameList)+1]]=MAEPlotDataFrame
  sampleAverageRMSEPlotDataFrameList[[length(sampleAverageRMSEPlotDataFrameList)+1]]=RMSEPlotDataFrame

}

names(sampleAverageMAEPlotDataFrameList)=sampleSizes
names(sampleAverageRMSEPlotDataFrameList)=sampleSizes

# VARIANCE OF ABS ERROR function
RMSE <- function(errorListStock) {
  return(colSds(abs(errorListStock)))
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
  
  names(RMSEDataFrame) = "RMSE"
  names(MAEDataFrame) = "MAE"
  
  sampleRMSEDataFrameList[[length(sampleRMSEDataFrameList)+1]] = RMSEDataFrame
  sampleMAEDataFrameList[[length(sampleMAEDataFrameList)+1]] = MAEDataFrame
}

names(sampleRMSEDataFrameList) = sampleSizes
names(sampleMAEDataFrameList) = sampleSizes


# CREATE RMSE, MAE DATAFRAME FOR LATEX
sampleRMSE.MAE.dataFrame <- data.frame(matrix(c(unlist(sampleRMSEDataFrameList),unlist(sampleMAEDataFrameList)), ncol=(2*length(sampleSizes)), byrow=F),stringsAsFactors=FALSE)

# ADD RMSE COL NAME
sampleSizeNameList = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSizeNameList[[length(sampleSizeNameList)+1]] = paste("$\\boldsymbol{\\sigma_{",sampleSizes[[sampleSizesIndex]],"}}$")
}

# ADD MAE COL NAME
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSizeNameList[[length(sampleSizeNameList)+1]] = paste("$\\boldsymbol{MAE_{",sampleSizes[[sampleSizesIndex]],"}}$")
}

# ADD STOCK TICKER NAMES
stockNameList = list()
for (stocksIndex in 1:nrow(stocks)){
  stockNameList[[length(stockNameList)+1]]=stocks[stocksIndex,1]
}

sampleRMSE.MAE.dataFrame = cbind(unlist(stockNameList),sampleRMSE.MAE.dataFrame)

# ASSIGN NAMES TO ROWS AND COLS
colnames(sampleRMSE.MAE.dataFrame) = c("Stock",unlist(sampleSizeNameList))
row.names(sampleRMSE.MAE.dataFrame) = NULL


sampleRMSE.MAE.dataFrame.average = getColMeans(sampleRMSE.MAE.dataFrame)

names(sampleRMSE.MAE.dataFrame.average) = c(unlist(sampleSizeNameList))


# TABLES-TO-LATEX
bold <- function(x){
  paste0('{\\bfseries ', x, '}')
}

createAverageLine <- function(x, digits,totCols) {
  resultString = ""
  for(i in 1:length(x)) {
    if (i == 1) {
      resultString = paste0(resultString," \\hline & ","{ \\bfseries ", "Average ","} &", "{ \\bfseries ",round(x[i], digits = digits),"}")
    }
    else {
      resultString = paste0(resultString, " & ","{ \\bfseries ", round(x[i], digits = digits),"}")
    }
  }
  diffCols = totCols - length(x) - 1
  if (diffCols == 0) {
    return(resultString)
  }
  else{
    for (i in 1:diffCols) {
      resultString = paste0(resultString, " & ")
    }
    return(resultString)
  }
}


#Plotting

#Model Plots
if(PLOTTING == TRUE) {
  for (sampleSizesIndex in 1:length(sampleSizes)){
    sampleSize = sampleSizes[sampleSizesIndex]
    
    stockName=stocks[stocksIndex,1]
    
    ARLagDataFrame=sampleARLagDataFrameList[[sampleSizesIndex]]
    averageARLagVector=drop(ARLagDataFrame[,ncol(ARLagDataFrame)])
    MALagDataFrame=sampleMALagDataFrameList[[sampleSizesIndex]]
    averageMALagVector=drop(MALagDataFrame[,ncol(MALagDataFrame)])
    
    
    if (sampleSizesIndex==1){

      rowNamesARLagDataFrame=as.Date(row.names(ARLagDataFrame))
      ARPlotDataFrame=data.frame(rowNamesARLagDataFrame,averageARLagVector)
      rowNamesMALagDataFrame=as.Date(row.names(MALagDataFrame))
      MAPlotDataFrame=data.frame(rowNamesMALagDataFrame,averageMALagVector)
    }else{
      ARPlotDataFrame=cbind(ARPlotDataFrame,averageARLagVector)
      MAPlotDataFrame=cbind(MAPlotDataFrame,averageMALagVector)    
    }
    
    
    GARCHModelDataFrame=sampleGARCHModelDataFrameList[[sampleSizesIndex]]
    GARCHModelDataFrameVector=c()
    for (day in 1:nrow(GARCHModelDataFrame)){
      previous=0
      for (column in (ncol(GARCHModelDataFrame)-length(garchModels)+1):ncol(GARCHModelDataFrame)){
        GARCHModelDataFrameVector[length(GARCHModelDataFrameVector)+1]=previous+(GARCHModelDataFrame[day,column]/nrow(stocks))
        previous=previous+(GARCHModelDataFrame[day,column]/nrow(stocks))
      }
    }
    
    rowNamesGARCHModelPlotDataFrame=as.Date(row.names(GARCHModelDataFrame))
    GARCHModelPlotDataFrame=data.frame(rowNamesGARCHModelPlotDataFrame, matrix(GARCHModelDataFrameVector, ncol=length(garchModels), byrow=TRUE))
    names(GARCHModelPlotDataFrame)=c("dates",garchModels)

    GARCHPlot=plot_ly(data=GARCHModelPlotDataFrame, x=~dates)
    for (garchModelsIndex in 1:length(garchModels)){
      garchModel = garchModels[garchModelsIndex]
      GARCHPlot=add_trace(GARCHPlot, y = GARCHModelPlotDataFrame[,(1+garchModelsIndex)], name = garchModel,type='scatter',mode = 'lines', fill='tonexty')
    }
    GARCHPlot=layout(GARCHPlot,legend = list(x = 100, y = 0.5), yaxis=list(title="Percentage of Stocks"), xaxis=list(title="Date"))

    print(GARCHPlot) #Printer plottet

    URL=paste(URL.drop,"/Plot/GARCHPlot_",sampleSize,".jpeg",sep="")
    export(GARCHPlot, file = URL)

  }
  names(ARPlotDataFrame)=c("dates",sampleSizes)
  names(MAPlotDataFrame)=c("dates",sampleSizes)
  
  ARPlot=plot_ly(data=ARPlotDataFrame, x=~dates)
  MAPlot=plot_ly(data=MAPlotDataFrame, x=~dates)
  for (sampleSizesIndex in 1:length(sampleSizes)){
    sampleSize = sampleSizes[sampleSizesIndex]
    ARPlot=add_trace(ARPlot, y = ARPlotDataFrame[,(1+sampleSizesIndex)], name = sampleSize,type='scatter',mode = 'lines')
    MAPlot=add_trace(MAPlot, y = MAPlotDataFrame[,(1+sampleSizesIndex)], name = sampleSize,type='scatter',mode = 'lines')
  }
  ARPlot=layout(ARPlot,legend = list(x = 100, y = 0.5), yaxis=list(title="Mean AR Lag"), xaxis=list(title="Date"))
  MAPlot=layout(MAPlot,legend = list(x = 100, y = 0.5), yaxis=list(title="Mean MA Lag"), xaxis=list(title="Date"))

  print(ARPlot) #Printer plottet
  print(MAPlot)
  
  URL=paste(URL.drop,"/Plot/averageARLags.jpeg",sep="")
  export(ARPlot, file = URL)
  URL=paste(URL.drop,"/Plot/averageMALags.jpeg",sep="")
  export(MAPlot, file = URL)

}
  
#Statistical Plots
if(PLOTTING == TRUE) {
  
  MAEPlot=plot_ly(data=sampleAverageMAEPlotDataFrameList[[1]], x=~dates)
  RMSEPlot=plot_ly(data=sampleAverageRMSEPlotDataFrameList[[1]], x=~dates)
  for (sampleSizesIndex in 1:length(sampleSizes)){
    sampleSize = sampleSizes[sampleSizesIndex]
    MAEPlot=add_trace(MAEPlot, y = sampleAverageMAEPlotDataFrameList[[sampleSizesIndex]][,2], name = sampleSize,type='scatter',mode = 'lines')
    RMSEPlot=add_trace(RMSEPlot, y = sampleAverageRMSEPlotDataFrameList[[sampleSizesIndex]][,2], name = sampleSize,type='scatter',mode = 'lines')
  }
  MAEPlot=layout(MAEPlot,legend = list(x = 100, y = 0.5), yaxis=list(title="Mean MAE"), xaxis=list(title="Date"))
  RMSEPlot=layout(RMSEPlot,legend = list(x = 100, y = 0.5), yaxis=list(title="Mean RMSE"), xaxis=list(title="Date"))
  
  print(MAEPlot)
  print(RMSEPlot)
    
  stockName=stocks[stocksIndex,1]
  URL=paste(URL.drop,"/Plot/averageMAE.jpeg",sep="")
  export(MAEPlot, file = URL)
  URL=paste(URL.drop,"/Plot/averageRMSELags.jpeg",sep="")
  export(RMSEPlot, file = URL) 
  
}
# SAVE LISTS TO Rda-files

URL=paste(URL.repo,"/Data/meanBuyAndHold.Rda",sep="")
save(meanBuyAndHold,file=URL)

URL=paste(URL.repo,"/Data/standardDevBuyAndHold.Rda",sep="")
save(standardDevBuyAndHold,file=URL)

URL=paste(URL.repo,"/Data/sampleBuyAndHoldTotalReturnDataFramesList.Rda",sep="")
save(sampleBuyAndHoldTotalReturnDataFramesList,file=URL)

URL=paste(URL.repo,"/Data/rollingWindowSize.Rda",sep="")
save(rollingWindowSize,file=URL)

# TO LATEX
createDigitsandAlignVectors <- function(dataFrame,digits) {
  colDim = dim(dataFrame)[2] - 1
  
  alignVector = c('c','l')
  for (i in 1:colDim) {
    alignVector = c(alignVector,'c')
  }
  
  digitsVector = c(1,1)
  for (i in 1:colDim) {
    digitsVector = c(digitsVector,digits)
  }
  
  return(list(alignVector,digitsVector))
}

# Diagnostics Metrics (Antar tre forskjellige GARCHer)

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  
  # DIAGNOSTIC METRICS
  x = sampleRunTimeDiagnosticsList[[sampleSizesIndex]]
  digits = 2
  alignAndDigitsVectors = createDigitsandAlignVectors(x,digits)
  # GENERAL LONG-TABLE COMMAND
  command <- c(paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLine(sampleRunTimeDiagnosticsList.average[[sampleSizesIndex]],digits,(ncol(x))))

  add.to.row <- list(pos = list(0,0), command = command)
  add.to.row$pos[[1]] = 1
  add.to.row$pos[[2]] = nrow(x)

  add.to.row$command <- command

  URL=paste(URL.drop,"/Tables/modelDiagnostics_",sampleSize,".txt",sep="")
  print(xtable(sampleRunTimeDiagnosticsList[[sampleSizesIndex]], auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Model Diagnostics"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
}

# STATISTICAL METRICS
x = sampleRMSE.MAE.dataFrame
digits = 3
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLine(sampleRMSE.MAE.dataFrame.average,digits,(ncol(x))))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 1
add.to.row$pos[[2]] = nrow(x)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/statisticalMetrics.txt",sep="")
print(xtable(sampleRMSE.MAE.dataFrame, auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "MAE metric and standard deviation of mean error"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
