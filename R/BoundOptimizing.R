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
transactionCost.variable=0.001
boundIterator=seq(from=0,to=1.5,by=0.025)
PLOTTING=T

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

optimizationVectorAlpha=data.frame(boundIterator)
optimizationVectorNumberOfTransactions=data.frame(boundIterator)
optimizationVectorHitRatio=data.frame(boundIterator)
for (sampleIndex2 in 1:length(sampleSizes)){
  
  bestAlpha=-100
  currentAlphaVector=c()
  currentNumberOfTransactionVector=c()
  currentHitRatioVector=c()
  
  for (boundIndex in 1:length(boundIterator)){
    tradingBound=boundIterator[boundIndex]
  
    #Bound Trading Hit and Return
    
    sampleHitDataFramesList=list()
    sampleAccumulatedBoundReturnDataFramesList=list()
    sampleAccumulatedBuyAndHoldReturnDataFramesList=list()
    sampleAccumulatedAlphaReturnDataFramesList=list()
    sampleBoundReturnDataFramesList=list()
    sampleBoundNumberOfTransactionsDataFramesList=list()
    
    for (sampleSizesIndex in 1:length(sampleSizes)){
      sampleSize = max(sampleSizes)
      rollingWindowSize = nrow(stockReturns) - max(sampleSizes)
      
      numberOfTransactionsVector=c()
      for (stocksIndex in 1:nrow(stocks)){
        stockName=stocks[stocksIndex,1]
        hitVector=vector()
        
        accumulatedBoundReturnVector=c(0)
        accumulatedBoundReturn=0
        accumulatedBuyAndHoldReturnVector=c(0)
        accumulatedBuyAndHoldReturn=0
        boundReturnVector = c()
        longShortReturn = 0
        
        numberOfTransactions=0
        long=0
        position=0
        for (day in 1:(rollingWindowSize)){
          meanForecast=sampleMeanForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
          volatilityForecast=sampleVolatilityForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
          nextDayReturn=drop(coredata(stockReturns[sampleSize+day,stocksIndex]))
          
          if(abs(meanForecast)>=(volatilityForecast*tradingBound)){
            long=sign(meanForecast)
          }else{
            long=0
          }
          
          if (sign(nextDayReturn)==long){ #Bound trade, predikerer riktig posisjon 
            if (position==long){#Hadde predikert posisjon fra f?r av, slipper transaksjon
              hitVector[length(hitVector)+1]=1 #Hit
              accumulatedBoundReturn=accumulatedBoundReturn+abs(nextDayReturn)
              boundReturnVector[length(boundReturnVector)+1] = abs(nextDayReturn)
            }else{#Hadde ikke predikert posisjon fra f?r av, m? gj?re en transaksjon
              if(position==0){
                hitVector[length(hitVector)+1]=1 #Hit
                accumulatedBoundReturn=accumulatedBoundReturn+abs(nextDayReturn)-1*transactionCost.variable
                boundReturnVector[length(boundReturnVector)+1] = abs(nextDayReturn)-1*transactionCost.variable
                numberOfTransactions=numberOfTransactions+1
              }else{ #Dobbelt transaksjonsgebyr. Kj??pe og selge
                hitVector[length(hitVector)+1]=1 #Hit
                accumulatedBoundReturn=accumulatedBoundReturn+abs(nextDayReturn)-2*transactionCost.variable
                boundReturnVector[length(boundReturnVector)+1] = abs(nextDayReturn)-2*transactionCost.variable
                numberOfTransactions=numberOfTransactions+2
              }
            }
            
            
          }else{ 
            if(long==0){ #Ikke bound trade, predikerer selg posisjon
              if(position==0){ #Hadde ikke noen posisjon fra f?r, slipper ? gj?re noe
                #hitVector[length(hitVector)+1]=0 #Miss
                hitVector[length(hitVector)+1]=NA
                accumulatedBoundReturn=accumulatedBoundReturn+0
                boundReturnVector[length(boundReturnVector)+1] = 0
              }else{ #Hadde posisjon fra f?r
                if(position==sign(meanForecast)){ #Har posisjon fra f?r som samsvarer med prediksjon, hold posisjon
                  if(position==sign(nextDayReturn)){ #Holder riktig posisjon
                    hitVector[length(hitVector)+1]=1 #Hit
                    accumulatedBoundReturn=accumulatedBoundReturn+abs(nextDayReturn)
                    boundReturnVector[length(boundReturnVector)+1] = abs(nextDayReturn)
                  }else{ #Holder feil posisjon
                    hitVector[length(hitVector)+1]=0 #Miss
                    accumulatedBoundReturn=accumulatedBoundReturn-abs(nextDayReturn)
                    boundReturnVector[length(boundReturnVector)+1] = -abs(nextDayReturn)
                  }
                  long=position
                }else{ #Har posisjon fra f?r som ikke samsvarer med prediksjon, selg posisjon
                  hitVector[length(hitVector)+1]=NA
                  accumulatedBoundReturn=accumulatedBoundReturn-transactionCost.variable
                  boundReturnVector[length(boundReturnVector)+1] = -transactionCost.variable
                  numberOfTransactions=numberOfTransactions+1
                }
                
              }
              
            }else{# Bound trade, predikerer feil posisjon
              if (position==long){ #Hadde predikert posisjon fra f?r av, slipper transaksjon
                hitVector[length(hitVector)+1]=0 #Miss
                accumulatedBoundReturn=accumulatedBoundReturn-abs(nextDayReturn)
                boundReturnVector[length(boundReturnVector)+1] = -abs(nextDayReturn)
              }else{#Hadde ikke predikert posisjon fra f?r av, m? gj?re en transaksjon
                if(position==0){
                  hitVector[length(hitVector)+1]=0 #Miss 
                  accumulatedBoundReturn=accumulatedBoundReturn-abs(nextDayReturn)-1*transactionCost.variable
                  boundReturnVector[length(boundReturnVector)+1] = -abs(nextDayReturn)-1*transactionCost.variable
                  numberOfTransactions=numberOfTransactions+1
                }else{#Dobbelt transaksjonsgebyr. Kj??pe og selge
                  hitVector[length(hitVector)+1]=0 #Miss 
                  accumulatedBoundReturn=accumulatedBoundReturn-abs(nextDayReturn)-2*transactionCost.variable
                  boundReturnVector[length(boundReturnVector)+1] = -abs(nextDayReturn)-2*transactionCost.variable
                  numberOfTransactions=numberOfTransactions+2
                }
              }
            }
          }
          
          accumulatedBoundReturnVector[length(accumulatedBoundReturnVector)+1]=accumulatedBoundReturn
          accumulatedBuyAndHoldReturn=accumulatedBuyAndHoldReturn+nextDayReturn
          accumulatedBuyAndHoldReturnVector[length(accumulatedBuyAndHoldReturnVector)+1]=accumulatedBuyAndHoldReturn
          position=long
          
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
        
        numberOfTransactionsVector[length(numberOfTransactionsVector)+1]=numberOfTransactions
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
      
      numberOfTransactionsDataFrame=data.frame(numberOfTransactionsVector)
      names(numberOfTransactionsDataFrame)=c("Number of Transactions")
      row.names(numberOfTransactionsDataFrame)=stocks[[1]]
      sampleBoundNumberOfTransactionsDataFramesList[length(sampleBoundNumberOfTransactionsDataFramesList)+1]=numberOfTransactionsDataFrame
      
      
    }
    names(sampleHitDataFramesList)=sampleSizes
    names(sampleAccumulatedBoundReturnDataFramesList)=sampleSizes
    names(sampleAccumulatedBuyAndHoldReturnDataFramesList)=sampleSizes
    names(sampleBoundReturnDataFramesList)=sampleSizes
    names(sampleBoundNumberOfTransactionsDataFramesList)=sampleSizes
    
    # CALCULATE TOTAL BOUND STRATEGY RETURN
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
    
    
    #Bound trading Hit Ratio
    
    sampleBoundHitRatioDataFramesList=list()
    for (sampleSizesIndex in 1:length(sampleSizes)){
      for (stocksIndex in 1:nrow(stocks)){
        stockName=stocks[stocksIndex,1]
        hitVector=sampleHitDataFramesList[[sampleSizesIndex]][,stocksIndex]
        hitVector=na.omit(hitVector)
        if (length(hitVector)>=1){
          hitRatio=sum(hitVector)/length(hitVector)
        }else{
          hitRatio=1
        }
        if (stocksIndex==1){
          stockHitRatioDataFrame=data.frame(hitRatio)
        }else{
          stockHitRatioDataFrame=cbind(stockHitRatioDataFrame,hitRatio)
        }
      }
      names(stockHitRatioDataFrame)=stocks[[1]]  
      sampleBoundHitRatioDataFramesList[[length(sampleBoundHitRatioDataFramesList)+1]]=stockHitRatioDataFrame
    }
    names(sampleBoundHitRatioDataFramesList)=sampleSizes
    
    currentAlpha=mean(sampleBoundAlphaDataFramesList[[sampleIndex2]][,1]) #Er tydeligvis en liste og ikke en dataframe
    currentNumberOfTransactions=mean(sampleBoundNumberOfTransactionsDataFramesList[[sampleIndex2]])
    currentHitRatio=mean(sampleBoundHitRatioDataFramesList[[sampleIndex2]][,1])
    currentAlphaVector[length(currentAlphaVector)+1]=currentAlpha
    currentNumberOfTransactionVector[length(currentNumberOfTransactionVector)+1]=currentNumberOfTransactions
    currentHitRatioVector[length(currentHitRatioVector)+1]=currentHitRatio
    
    if (currentAlpha>=bestAlpha){
      bestAlpha=currentAlpha
    }
  
  }
  
  optimizationVectorAlpha=cbind(optimizationVectorAlpha,currentAlphaVector)
  optimizationVectorNumberOfTransactions=cbind(optimizationVectorNumberOfTransactions,currentNumberOfTransactionVector)
  optimizationVectorHitRatio=cbind(optimizationVectorHitRatio,currentHitRatioVector)
  
}


if (PLOTTING==T){
  
  for (sampleSizeIndex in 1:length(sampleSizes)){
    sampleSize=sampleSizes[sampleSizeIndex]
  
    plotOne=plot_ly(x=optimizationVectorAlpha[,1])%>%
      add_lines(y = optimizationVectorAlpha[,sampleSizeIndex+1], name = "Average Alpha",type='scatter',mode = 'lines')%>%
      layout(legend = list(x = 100, y = 0.5), xaxis=list(title="Bound"),yaxis=list(title="Average Alpha"))
    print(plotOne)
    
    URL=paste(URL.drop,"/Plot/BoundTrading/",sampleSize,"_BoundOptimization_AverageAlpha",".jpeg",sep="")
    export(plotOne, file = URL)
    
    plotTwo=plot_ly(x=optimizationVectorNumberOfTransactions[,1])%>%
      add_lines(y = optimizationVectorNumberOfTransactions[,sampleSizeIndex+1], name = "Average Number Of Transactions",type='scatter',mode = 'lines')%>%
      layout(legend = list(x = 100, y = 0.5), xaxis=list(title="Bound"),yaxis=list(title="Average Number Of Transactions"))
    print(plotTwo)
    
    URL=paste(URL.drop,"/Plot/BoundTrading/",sampleSize,"_BoundOptimization_AverageNumberOfTransactions",".jpeg",sep="")
    export(plotTwo, file = URL)
    
    plotThree=plot_ly(x=optimizationVectorHitRatio[,1])%>%
      add_lines(y = optimizationVectorHitRatio[,sampleSizeIndex+1], name = "Average Hit Ratio",type='scatter',mode = 'lines')%>%
      layout(legend = list(x = 100, y = 0.5), xaxis=list(title="Bound"),yaxis=list(title="Average Hit Ratio"))
    print(plotThree)
    
    URL=paste(URL.drop,"/Plot/BoundTrading/",sampleSize,"_BoundOptimization_AverageHitRatio",".jpeg",sep="")
    export(plotThree, file = URL)
    
  }
}

# optimizationVectorAlpha[which.max(optimizationVectorAlpha[,2]),1]
# optimizationVectorAlpha[which.max(optimizationVectorAlpha[,2]),2]
  

