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

tradingBound=0.2 #Number of times the standard deviation

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

#Bound Trading Hit and Return

sampleHitDataFramesList=list()
sampleAccumulatedBoundReturnDataFramesList=list()
sampleAccumulatedBuyAndHoldReturnDataFramesList=list()
sampleAccumulatedAlphaReturnDataFramesList=list()
sampleBoundReturnDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    hitVector=vector()
    
    accumulatedBoundReturnVector=c(0)
    accumulatedBoundReturn=0
    accumulatedBuyAndHoldReturnVector=c(0)
    accumulatedBuyAndHoldReturn=0
    boundReturnVector = c()
    longShortReturn = 0
  
    long=0
    for (day in 1:(rollingWindowSize)){
      meanForecast=sampleMeanForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      volatilityForecast=sampleVolatilityForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      nextDayReturn=drop(coredata(stockReturns[sampleSize+day,stocksIndex]))
      
      if(abs(meanForecast)>(volatilityForecast*tradingBound)){
        if(sign(meanForecast==1)){
          long=1
        }
        else{
          long=-1
        }
      }
      
      if (sign(nextDayReturn)==long){ #Tatt riktig posisjon
        hitVector[length(hitVector)+1]=1 #Hit
        accumulatedBoundReturn=accumulatedBoundReturn+abs(nextDayReturn)
        boundReturnVector[length(boundReturnVector)+1] = abs(nextDayReturn)
        
        
      }else{
        if(long==0){#Har ikke tatt noen posisjon
          hitVector[length(hitVector)+1]=0 #Miss
          accumulatedBoundReturn=accumulatedBoundReturn+0
          boundReturnVector[length(boundReturnVector)+1] = 0
        }else{#Tatt feil posisjon
          hitVector[length(hitVector)+1]=0 #Miss
          accumulatedBoundReturn=accumulatedBoundReturn-abs(nextDayReturn)
          boundReturnVector[length(boundReturnVector)+1] = -abs(nextDayReturn)
        }
      }
      accumulatedBoundReturnVector[length(accumulatedBoundReturnVector)+1]=accumulatedBoundReturn
      accumulatedBuyAndHoldReturn=accumulatedBuyAndHoldReturn+nextDayReturn
      accumulatedBuyAndHoldReturnVector[length(accumulatedBuyAndHoldReturnVector)+1]=accumulatedBuyAndHoldReturn
      
    }
    
    if (stocksIndex==1){
      stockHitDataFrame=data.frame(hitVector)
      
      accumulatedBoundReturnDataFrame=data.frame(accumulatedBoundReturnVector)
      accumulatedBuyAndHoldReturnDataFrame=data.frame(accumulatedBuyAndHoldReturnVector)
      accumulatedAlphaReturnDataFrame=data.frame((accumulatedBoundReturnVector-accumulatedBuyAndHoldReturnVector))
      boundReturnDataFrame = data.frame(boundReturnVector)
    }else{
      stockHitDataFrame=cbind(stockHitDataFrame,hitVector)
      accumulatedBoundReturnDataFrame=cbind(accumulatedBoundReturnDataFrame,accumulatedBoundReturnVector)
      accumulatedBuyAndHoldReturnDataFrame=cbind(accumulatedBuyAndHoldReturnDataFrame,accumulatedBuyAndHoldReturnVector)
      accumulatedAlphaReturnDataFrame=cbind(accumulatedAlphaReturnDataFrame,(accumulatedBoundReturnVector-accumulatedBuyAndHoldReturnVector))
      boundReturnDataFrame = cbind(boundReturnDataFrame, boundReturnVector)
    }
  }
  
  names(stockHitDataFrame)=stocks[[1]]
  row.names(stockHitDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleHitDataFramesList[[length(sampleHitDataFramesList)+1]]=stockHitDataFrame
  
  names(accumulatedBoundReturnDataFrame)=stocks[[1]]
  row.names(accumulatedBoundReturnDataFrame)=index(stockReturns)[(sampleSize):nrow(stockReturns)]
  sampleAccumulatedBoundReturnDataFramesList[[length(sampleAccumulatedBoundReturnDataFramesList)+1]]=accumulatedBoundReturnDataFrame
  
  names(accumulatedBuyAndHoldReturnDataFrame)=stocks[[1]]
  row.names(accumulatedBuyAndHoldReturnDataFrame)=index(stockReturns)[(sampleSize):nrow(stockReturns)]
  sampleAccumulatedBuyAndHoldReturnDataFramesList[[length(sampleAccumulatedBuyAndHoldReturnDataFramesList)+1]]=accumulatedBuyAndHoldReturnDataFrame
  
  names(accumulatedAlphaReturnDataFrame)=stocks[[1]]
  row.names(accumulatedAlphaReturnDataFrame)=index(stockReturns)[(sampleSize):nrow(stockReturns)]
  sampleAccumulatedAlphaReturnDataFramesList[[length(sampleAccumulatedAlphaReturnDataFramesList)+1]]=accumulatedAlphaReturnDataFrame
  
  names(boundReturnDataFrame)=stocks[[1]]
  row.names(boundReturnDataFrame)=index(stockReturns)[(sampleSize+1):nrow(stockReturns)]
  sampleBoundReturnDataFramesList[[length(sampleBoundReturnDataFramesList)+1]]=boundReturnDataFrame
}
names(sampleHitDataFramesList)=sampleSizes
names(sampleAccumulatedBoundReturnDataFramesList)=sampleSizes
names(sampleAccumulatedBuyAndHoldReturnDataFramesList)=sampleSizes
names(sampleBoundReturnDataFramesList)=sampleSizes

# CALCULATE TOTAL LONG-SHORT STRATEGY RETURN
sampleBoundTotalReturnDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  totReturnDataFrame = data.frame(colSums(sampleBoundReturnDataFramesList[[sampleSizesIndex]]))
  
  colnames(totReturnDataFrame) = "Total bound strategy return"
  
  sampleBoundTotalReturnDataFramesList[[length(sampleBoundTotalReturnDataFramesList)+1]] = totReturnDataFrame
}

names(sampleBoundTotalReturnDataFramesList) = sampleSizes

# CALCULATE TOTAL ALPHA RETURN
sampleBoundAlphaDataFramesList=list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  totAlphaDataFrame = data.frame(unlist(sampleAccumulatedAlphaReturnDataFramesList[[sampleSizesIndex]][nrow(sampleAccumulatedAlphaReturnDataFramesList[[sampleSizesIndex]]),]))
  
  colnames(totAlphaDataFrame) = 'Alpha'
  
  sampleBoundAlphaDataFramesList[[length(sampleBoundAlphaDataFramesList)+1]] = totAlphaDataFrame
}

names(sampleBoundAlphaDataFramesList) = sampleSizes

#CALCULATE MEAN & VARIANCE BOUND TRADING RETURN
varianceBound = list()
standardDevBound = list()
meanBound = list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  meanBoundDataFrame = data.frame(colMeans(sampleBoundReturnDataFramesList[[sampleSizesIndex]]))
  standardDeviationDataFrame = data.frame(colSds(sampleBoundReturnDataFramesList[[sampleSizesIndex]]))
  varianceDataFrame = data.frame(colVars(sampleBoundReturnDataFramesList[[sampleSizesIndex]]))
  
  colnames(meanBoundDataFrame) = "Mean bound"
  colnames(standardDeviationDataFrame) = "Standard deviation bound"
  colnames(varianceDataFrame) = "Variance bound"
  
  varianceBound[[length(varianceBound)+1]] = varianceDataFrame
  standardDevBound[[length(standardDevBound)+1]] = standardDeviationDataFrame
  meanBound[[length(meanBound)+1]] = meanBoundDataFrame
}

names(varianceBound) = sampleSizes
names(standardDevBound) = sampleSizes
names(meanBound) = sampleSizes

#Bound trading Hit Ratio

sampleBoundHitRatioDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleBoundHitRatioDataFramesList[[length(sampleBoundHitRatioDataFramesList)+1]]=data.frame((colSums(sampleHitDataFramesList[[sampleSizesIndex]]))/nrow(sampleHitDataFramesList[[sampleSizesIndex]]))
}
names(sampleBoundHitRatioDataFramesList)=sampleSizes

#Plotting
PLOTTING = FALSE

if(PLOTTING == TRUE) {
  for (sampleSizesIndex in 1:length(sampleSizes)){
    sampleSize = sampleSizes[sampleSizesIndex]
    
    for (stocksIndex in 1:nrow(stocks)){
      stockName=stocks[stocksIndex,1]
      
      accumulatedBoundReturnVector=drop(sampleAccumulatedBoundReturnDataFramesList[[sampleSizesIndex]][,stocksIndex])
      accumulatedBuyAndHoldReturnVector=drop(sampleAccumulatedBuyAndHoldReturnDataFramesList[[sampleSizesIndex]][,stocksIndex])
      accumulatedAlphaReturnVector=drop(sampleAccumulatedAlphaReturnDataFramesList[[sampleSizesIndex]][,stocksIndex])
      rowNamesAccumulatedBoundReturnVector=as.Date(row.names(sampleAccumulatedBoundReturnDataFramesList[[sampleSizesIndex]]))
      plotDataFrame=data.frame(dates=rowNamesAccumulatedBoundReturnVector,buyAndHold=accumulatedBuyAndHoldReturnVector, bound=accumulatedBoundReturnVector, alpha=accumulatedAlphaReturnVector)
      
      subplotOne=plot_ly(plotDataFrame, x=~dates) %>%
        add_trace(y = ~buyAndHold, name = 'Buy and Hold Strategy',type='scatter',mode = 'lines') %>%
        add_trace(y = ~bound, name = 'Bound Strategy',type='scatter', mode = 'lines')%>%
        layout(legend = list(x = 100, y = 0.5), yaxis=list(title="Return"))
      
      subplotTwo=plot_ly(plotDataFrame, x=~dates) %>%
        add_trace(y = ~alpha, name = 'Alpha',type='scatter',mode = 'lines')%>%
        layout(legend = list(x = 100, y = 0.5),yaxis=list(title="Return"), xaxis=list(title="Date"))
      
      
      fullPlot=subplot(nrows=2,subplotOne,subplotTwo, shareX = TRUE, heights = c(0.75,0.25), titleX = TRUE, titleY = TRUE)
      
      URL=paste(URL.drop,"/Plot/",stockName,"_",sampleSize,"_Bound Strategy",".jpeg",sep="")
      export(fullPlot, file = URL)
    }
  }
}


# SAVE RDA-files FOR INFORMATION-TABLE
#BOUND
URL=paste(URL.repo,"/Data/tradingBound.Rda",sep="")
save(tradingBound,file=URL)

# HIT RATIO
URL=paste(URL.repo,"/Data/sampleBoundHitRatioDataFramesList.Rda",sep="")
save(sampleBoundHitRatioDataFramesList,file=URL)

# MEAN
URL=paste(URL.repo,"/Data/meanBound.Rda",sep="")
save(meanBound,file=URL)

# STD_DEV
URL=paste(URL.repo,"/Data/standardDevBound.Rda",sep="")
save(standardDevBound,file=URL)

# RETURN
URL=paste(URL.repo,"/Data/sampleBoundTotalReturnDataFramesList.Rda",sep="")
save(sampleBoundTotalReturnDataFramesList,file=URL)

# ALPHA
URL=paste(URL.repo,"/Data/sampleBoundAlphaDataFramesList.Rda",sep="")
save(sampleBoundAlphaDataFramesList,file=URL)
