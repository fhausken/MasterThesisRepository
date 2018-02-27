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

URL.logging=paste(URL.repo,"/Output/ParallellLog.txt", sep="")
cat("Output:\n\n", file=URL.logging, append=FALSE) #Clears log

URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocksRemoved.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/distributionFitResults.Rda",sep="")
load(URL)

sampleSizes=c(100,250)#,283,750,1000)
garchModels=c('sGARCH','gjrGARCH','eGARCH')
ARLag.max=3
MALag.max=3
GARCHLagOne.max=2
GARCHLagTwo.max=2


start_time <- Sys.time()
#allStocksResults=list()
allStocksResults=foreach(stocksIndex=1:nrow(stocks)) %dopar%{
#for (stocksIndex in 1:nrow(stocks)){
  
  library(parallel)
  library(doParallel)
  library(quantmod)
  library(lattice)
  library(timeSeries)
  library(rugarch)
  library(xts)
  library(tseries)
  library(rugarch)
  
  
  cat(paste(Sys.time(), "\t","Starting iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,"\n",sep=""), file=URL.logging, append=TRUE)
  
  individualStockRetun=stockReturns[,stocksIndex]
  stockDistribution=distributionsFitResults[stocksIndex,12]
  stockDistribution.fullname=distributionsFitResults[stocksIndex,11]
  
  sampleSizeResults=list()
  for (sampleSizesIndex in 1:length(sampleSizes)){
    sampleSize = sampleSizes[sampleSizesIndex]
    rollingWindowSize = length(individualStockRetun) - sampleSize
    
    
    individualStockResults=list()
    for (day in 0:rollingWindowSize){
      individualStockReturnOffset = individualStockRetun[(1+day):(sampleSize+day)]
    
      AIC.final=1000000 # tilsvarer + infinity
      
      
      for (garchModelsIndex in 1:length(garchModels)){
        garchModel=garchModels[garchModelsIndex]
        
        for (ARLag in 0:ARLag.max){
          for (MALag in 0:MALag.max){
            for (GARCHLagOne in 0:GARCHLagOne.max){
              for (GARCHLagTwo in 0:GARCHLagTwo.max){
                spec = ugarchspec(
                  variance.model=list(model=garchModel,garchOrder=c(GARCHLagOne,GARCHLagTwo)),
                  mean.model=list(armaOrder=c(ARLag, MALag), include.mean=T),
                  distribution.model=stockDistribution
                )
                fit = tryCatch(
                  ugarchfit(spec, individualStockReturnOffset, solver = 'hybrid'), error=function(e) e, warning=function(w) w
                )
                
                if(is(fit,"warning")){
                  
                  AIC=1000000
                  #cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ," Day: ",day,"/" , rollingWindowSize," Warning!","\n",sep=""), file=URL.logging, append=TRUE)
                  
                } else if(is(fit," error")){
                  
                  AIC=1000000
                  #cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ," Day: ",day,"/" , rollingWindowSize," Error!","\n",sep=""), file=URL.logging, append=TRUE)
                  
                }else{
                  
                  AIC=infocriteria(fit)[1]
                  forecast=ugarchforecast(fit,n.ahead=1)
                  forecastOneDayAhead=drop(forecast@forecast$seriesFor) #Drop fjerner kolonne og radnavn
                  
                  
                  
                }
                
                if (AIC<AIC.final){
                  AIC.final=AIC
                  forecastOneDayAhead.final=forecastOneDayAhead
                  ARLag.final=ARLag
                  MALag.final=MALag
                  GARCHLagOne.final=GARCHLagOne
                  GARCHLagTwo.final=GARCHLagTwo
                  
                  
                }
                
              } 
              
            }
            
          }
          
        }
        

      }
      if (AIC.final==1000000){
        cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ," Day: ",day,"/" , rollingWindowSize," Did Not Converge!","\n",sep=""), file=URL.logging, append=TRUE)
        AIC=NULL
        forecastOneDayAhead.final=NULL
        garchModel=NULL
        ARLag.final=NULL
        MALag.final=NULL
        GARCHLagOne.final=NULL
        GARCHLagTwo.final=NULL
        stockDistribution.fullname=NULL
      }else{
        if(GARCHLagOne.final==0){
          if(GARCHLagTwo.final==0){
            garchModel="Plain Vanilla ARMA" #Hvis vi bare har en ARMA prosess
          }
        }
      }
      
      results=list(AIC.final, forecastOneDayAhead.final, garchModel,ARLag.final, MALag.final, GARCHLagOne.final, GARCHLagTwo.final, stockDistribution.fullname) # Merk at man må bruke to brackets for å legge til en liste inni en liste
      names(results)=c("AIC", "One-Day-Ahead Forecast",  "Garch Model","AR Lag","MA Lag", "GARCH Lag 1","GARCH Lag 2","Stock Distribution" )
      individualStockResults[[length(individualStockResults)+1]]=results
      
      
    }
    names(individualStockResults)=index(individualStockRetun)[sampleSize:nrow(individualStockRetun)]
    sampleSizeResults[[length(sampleSizeResults)+1]]=individualStockResults
    
    
  }
  
  names(sampleSizeResults)=sampleSizes
  #allStocksResults[[length(allStocksResults)+1]]=sampleSizeResults
  return(sampleSizeResults)
}

names(allStocksResults)=stocks[[1]]

URL=paste(URL.repo,"/Data/ARMAGARCHResults.Rda",sep="")
save(allStocksResults,file=URL)

URL=paste(URL.repo,"/Data/sampleSizes.Rda",sep="")
save(sampleSizes,file=URL)

URL=paste(URL.repo,"/Data/GARCHModels.Rda",sep="")
save(garchModels,file=URL)

end_time <- Sys.time()
run_time=end_time-start_time
cat(paste("\nKjøretid:",run_time, "\n"), file=URL.logging, append=TRUE)

stopCluster(c1)
