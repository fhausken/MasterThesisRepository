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

#INPUT

transactionCost.variable=0.0001
PLOTTING = F


URL=paste(URL.repo,"/Data/sampleSizes.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleVolatilityForecastsDataFramesList.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleMeanForecastsDataFramesList.Rda",sep="")
load(URL)


#Short/Sell Long Hit and Return

sampleHitDataFramesList=list()
sampleAccumulatedShortLongReturnDataFramesList=list()
sampleAccumulatedBuyAndHoldReturnDataFramesList=list()
sampleAccumulatedAlphaReturnDataFramesList=list()
sampleErrorDataFramesList=list()
sampleShortLongReturnDataFramesList=list()
sampleShortLongNumberOfTransactionsDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = max(sampleSizes)
  rollingWindowSize = nrow(stockReturns) - max(sampleSizes)
  
  numberOfTransactionsVector=c()
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    hitVector=vector()
    
    accumulatedShortLongReturnVector=c(0)
    accumulatedShortLongReturn=0
    accumulatedBuyAndHoldReturnVector=c(0)
    accumulatedBuyAndHoldReturn=0
    shortLongReturnVector = c()
    shortLongReturn = 0
    errorVector=vector()
    
    numberOfTransactions=0
    position=0
    for (day in 1:(rollingWindowSize)){
      forecast=sampleMeanForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      nextDayReturn=drop(coredata(stockReturns[sampleSize+day,stocksIndex]))
      
      errorVector[length(errorVector)+1]=nextDayReturn-forecast #realized - forecast
      
      if (sign(nextDayReturn)==sign(forecast)){
        if (position==sign(forecast)){
          hitVector[length(hitVector)+1]=1 #Hit
          accumulatedShortLongReturn=accumulatedShortLongReturn+abs(nextDayReturn)
          shortLongReturnVector[length(shortLongReturnVector)+1] = abs(nextDayReturn)
        }else{
          hitVector[length(hitVector)+1]=1 #Hit
          accumulatedShortLongReturn=accumulatedShortLongReturn+abs(nextDayReturn)-transactionCost.variable
          shortLongReturnVector[length(shortLongReturnVector)+1] = abs(nextDayReturn)-transactionCost.variable
          numberOfTransactions=numberOfTransactions+1
        }
    
        
      }else{
        if (position==sign(forecast)){
          hitVector[length(hitVector)+1]=0 #Miss
          accumulatedShortLongReturn=accumulatedShortLongReturn-abs(nextDayReturn)
          shortLongReturnVector[length(shortLongReturnVector)+1] = -abs(nextDayReturn)
        }else{
          hitVector[length(hitVector)+1]=0 #Miss
          accumulatedShortLongReturn=accumulatedShortLongReturn-abs(nextDayReturn)-transactionCost.variable
          shortLongReturnVector[length(shortLongReturnVector)+1] = -abs(nextDayReturn)-transactionCost.variable
          numberOfTransactions=numberOfTransactions+1
        }
        
      }
      position=sign(forecast)
      
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
      shortLongReturnDataFrame = data.frame(shortLongReturnVector)
    }else{
      stockHitDataFrame=cbind(stockHitDataFrame,hitVector)
      accumulatedShortLongReturnDataFrame=cbind(accumulatedShortLongReturnDataFrame,accumulatedShortLongReturnVector)
      errorDataFrame=cbind(errorDataFrame, errorVector)
      accumulatedBuyAndHoldReturnDataFrame=cbind(accumulatedBuyAndHoldReturnDataFrame,accumulatedBuyAndHoldReturnVector)
      accumulatedAlphaReturnDataFrame=cbind(accumulatedAlphaReturnDataFrame,(accumulatedShortLongReturnVector-accumulatedBuyAndHoldReturnVector))
      shortLongReturnDataFrame = cbind(shortLongReturnDataFrame, shortLongReturnVector)
      
    }
    
    numberOfTransactionsVector[length(numberOfTransactionsVector)+1]=numberOfTransactions
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
  
  names(shortLongReturnDataFrame)=stocks[[1]]
  row.names(shortLongReturnDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleShortLongReturnDataFramesList[[length(sampleShortLongReturnDataFramesList)+1]]=shortLongReturnDataFrame
  
  numberOfTransactionsDataFrame=data.frame(numberOfTransactionsVector)
  names(numberOfTransactionsDataFrame)=c("Number of Transactions")
  row.names(numberOfTransactionsDataFrame)=stocks[[1]]
  sampleShortLongNumberOfTransactionsDataFramesList[length(sampleShortLongNumberOfTransactionsDataFramesList)+1]=numberOfTransactionsDataFrame
}
names(sampleHitDataFramesList)=sampleSizes
names(sampleErrorDataFramesList)=sampleSizes
names(sampleAccumulatedShortLongReturnDataFramesList)=sampleSizes
names(sampleAccumulatedBuyAndHoldReturnDataFramesList)=sampleSizes
names(sampleShortLongReturnDataFramesList)=sampleSizes
names(sampleShortLongNumberOfTransactionsDataFramesList)=sampleSizes

#Short/Sell Long Hit Ratio

sampleHitRatioDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleHitRatioDataFramesList[[length(sampleHitRatioDataFramesList)+1]]=data.frame((colSums(sampleHitDataFramesList[[sampleSizesIndex]]))/nrow(sampleHitDataFramesList[[sampleSizesIndex]]))
}
names(sampleHitRatioDataFramesList)=sampleSizes


# CALCULATE TOTAL LONG-SHORT STRATEGY RETURN
sampleShortLongTotalReturnDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  totReturnDataFrame = data.frame(colSums(sampleShortLongReturnDataFramesList[[sampleSizesIndex]]))
  
  colnames(totReturnDataFrame) = "Total short long strategy return"
  
  sampleShortLongTotalReturnDataFramesList[[length(sampleShortLongTotalReturnDataFramesList)+1]] = totReturnDataFrame
}

names(sampleShortLongTotalReturnDataFramesList) = sampleSizes

# CALCULATE TOTAL ALPHA RETURN
sampleAlphaDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  totAlphaDataFrame = data.frame(unlist(sampleAccumulatedAlphaReturnDataFramesList[[sampleSizesIndex]][nrow(sampleAccumulatedAlphaReturnDataFramesList[[sampleSizesIndex]]),]))
  
  colnames(totAlphaDataFrame) = 'Alpha'
  
  sampleAlphaDataFramesList[[length(sampleAlphaDataFramesList)+1]] = totAlphaDataFrame
}

names(sampleAlphaDataFramesList) = sampleSizes

#CALCULATE MEAN & VARIANCE SHORT-LONG RETURN
varianceLongShort = list()
standardDevShortLong = list()
meanLongShort = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  meanLongShortDataFrame = data.frame(colMeans(sampleShortLongReturnDataFramesList[[sampleSizesIndex]]))
  standardDeviationDataFrame = data.frame(colSds(sampleShortLongReturnDataFramesList[[sampleSizesIndex]]))
  varianceDataFrame = data.frame(colVars(sampleShortLongReturnDataFramesList[[sampleSizesIndex]]))
  
  colnames(meanLongShortDataFrame) = "Mean short long"
  colnames(standardDeviationDataFrame) = "Standard deviation short long"
  colnames(varianceDataFrame) = "Variance short long"
  
  varianceLongShort[[length(varianceLongShort)+1]] = varianceDataFrame
  standardDevShortLong[[length(standardDevShortLong)+1]] = standardDeviationDataFrame
  meanLongShort[[length(meanLongShort)+1]] = meanLongShortDataFrame
}

names(varianceLongShort) = sampleSizes
names(standardDevShortLong) = sampleSizes
names(meanLongShort) = sampleSizes

#Plotting

if(PLOTTING == TRUE) {
  for (sampleSizesIndex in 1:length(sampleSizes)){
    sampleSize = sampleSizes[sampleSizesIndex]
    
    individualStockPlotting=foreach(stocksIndex=1:nrow(stocks)) %dopar%{
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
      print(fullPlot) #Printer plottet
      
      URL=paste(URL.drop,"/Plot/",stockName,"_",sampleSize,"_ShortLongStrategy",".jpeg",sep="")
      export(fullPlot, file = URL)
      return(fullPlot)
    }
    for (plotIndex in 1:length(individualStockPlotting)){
      print(individualStockPlotting[[plotIndex]]) #plotter
    }
  }
  

}

stopCluster(c1)

# SAVE RDA-files FOR INFORMATION-TABLE

# HIT RATIO
URL=paste(URL.repo,"/Data/sampleHitRatioDataFramesList.Rda",sep="")
save(sampleHitRatioDataFramesList,file=URL)

# MEAN
URL=paste(URL.repo,"/Data/meanLongShort.Rda",sep="")
save(meanLongShort,file=URL)

# STD_DEV
URL=paste(URL.repo,"/Data/standardDevShortLong.Rda",sep="")
save(standardDevShortLong,file=URL)

# RETURN
URL=paste(URL.repo,"/Data/sampleShortLongTotalReturnDataFramesList.Rda",sep="")
save(sampleShortLongTotalReturnDataFramesList,file=URL)

# ALPHA
URL=paste(URL.repo,"/Data/sampleAlphaDataFramesList.Rda",sep="")
save(sampleAlphaDataFramesList,file=URL)

# Number of transactions
URL=paste(URL.repo,"/Data/sampleShortLongNumberOfTransactionsDataFramesList.Rda",sep="")
save(sampleShortLongNumberOfTransactionsDataFramesList,file=URL)
