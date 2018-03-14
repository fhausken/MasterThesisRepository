rm(list=ls()) #Clears environment

library(parallel)
library(doParallel)
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
library(xts)
library(tseries)


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

URL=paste(URL.repo,"/Data/distributions.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/distributionsFullname.Rda",sep="")
load(URL)

sampleSizes=c(1290)#,500,750,1000)

garchModels=c('sGARCH','gjrGARCH','eGARCH')
ARLag.max=1
MALag.max=1

GARCHLagOne.max=1
GARCHLagTwo.max=1


start_time <- Sys.time()
allStocksResults=list()
#allStocksResults=foreach(stocksIndex=1:nrow(stocks)) %dopar%{
for (stocksIndex in 1:nrow(stocks)){
  
  individualStockRetun=stockReturns[,stocksIndex]
  individualStockRetunTotalDays=length(individualStockRetun)
  
  sampleSizeResults=list()
  for (sampleSizesIndex in 1:length(sampleSizes)){
    sampleSize = sampleSizes[sampleSizesIndex]
    rollingWindowSize = individualStockRetunTotalDays - max(sampleSizes)
    
    cat(paste(Sys.time(), "\t","Starting iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize," \n",sep=""), file=URL.logging, append=TRUE)
    print(paste(Sys.time(), "\t","Starting iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize," \n",sep=""))
    #individualStockResults=list()
    #for (day in 0:rollingWindowSize){
    individualStockResults=foreach(day=0:rollingWindowSize) %dopar%{ #
      library(parallel)
      library(doParallel)
      library(quantmod)
      library(lattice)
      library(timeSeries)
      library(rugarch)
      library(xts)
      library(tseries)
      
      equalStartingPointdjustment=(max(sampleSizes)-sampleSize)
      individualStockReturnOffset = individualStockRetun[(1+equalStartingPointdjustment+day):(sampleSize+equalStartingPointdjustment+day)]
      
      AIC.final.distribution=1000000 #tilsvarer + infinity
      bestDistributionFit.fullname="Normal Distribution"
      bestDistributionFit="norm"
      for (distributions.index in 1:length(distributions)){
        
        vectorizedReturn=drop(coredata(individualStockReturnOffset))
        fit.distribution <- fitdist(distribution = distributions[distributions.index], vectorizedReturn, control = list())
        
        k=length(fit.distribution$pars)
        maxLikelihood=min(fit.distribution$values)
        AIC=2*k+2*maxLikelihood 
        
        if (AIC.final.distribution>=AIC){
          AIC.final.distribution=AIC
          bestDistributionFit.fullname=distributions.fullname[distributions.index]
          bestDistributionFit=distributions[distributions.index]
        }
      }
        
      
      
      
      AIC.final=1000000 # tilsvarer + infinity
      
      
      for (garchModelsIndex in 1:length(garchModels)){
        garchModel=garchModels[garchModelsIndex]
        
        runARCHInMean=FALSE
        if (garchModel!= "sGARCH"){
          runARCHInMean=TRUE
        }
        
        for (ARLag in 0:ARLag.max){
          for (MALag in 0:MALag.max){
            for (GARCHLagOne in 1:GARCHLagOne.max){
              for (GARCHLagTwo in 1:GARCHLagTwo.max){
                spec = ugarchspec(
                  variance.model=list(model=garchModel,garchOrder=c(GARCHLagOne,GARCHLagTwo)),
                  mean.model=list(armaOrder=c(ARLag, MALag), include.mean=T,archm=runARCHInMean, archpow=1),
                  distribution.model=bestDistributionFit
                )
                fit = tryCatch(
                  ugarchfit(spec, individualStockReturnOffset, solver = 'hybrid'), error=function(e) e, warning=function(w) w
                )
                
                if(is(fit,"warning")){
                  
                  AIC=1000000
                  #cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Model: ",garchModel,ARLag,MALag,GARCHLagOne,GARCHLagTwo,". Warning i fit!","\n",sep=""), file=URL.logging, append=TRUE)
                  
                } else if(is(fit," error")){
                  
                  AIC=1000000
                  cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Error i fit!","\n",sep=""), file=URL.logging, append=TRUE)
                  
                }else{
                  
                  tryCatch({
                    AIC=infocriteria(fit)[1]
                    forecast=ugarchforecast(fit,n.ahead=1)
                    forecastOneDayAhead.mean=drop(forecast@forecast$seriesFor) #Drop fjerner kolonne og radnavn}
                    forecastOneDayAhead.volatility=drop(forecast@forecast$sigmaFor) #Drop fjerner kolonne og radnavn}
                    }, error = function(e) { 

                      cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Error in infocriteria!","\n",sep=""), file=URL.logging, append=TRUE) #Skjønner ikke hvorfor denne feilen ikke blir fanget over...

                      URL=paste(URL.repo,"/Data/ErroriFit.Rda",sep="")
                      save(fit,file=URL)
                      URL=paste(URL.repo,"/Data/ErroriSpec.Rda",sep="")
                      save(spec,file=URL)
                      
                      AIC=1000000
                    })

                }
                
                if (AIC<AIC.final){
                  AIC.final=AIC
                  forecastOneDayAhead.mean.final=forecastOneDayAhead.mean
                  forecastOneDayAhead.volatility.final=forecastOneDayAhead.volatility
                  garchModel.final=garchModel
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
      
      #print(fit)
      
      if (AIC.final==1000000){
        cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1],". Sample size: ",sampleSize ,". Day: ",day,"/" , rollingWindowSize,". Did Not Converge!"," \n",sep=""), file=URL.logging, append=TRUE)
        AIC.final=1000000
        forecastOneDayAhead.mean.final=0
        forecastOneDayAhead.volatility.final=0
        garchModel.final="Ikke konvergert"
        ARLag.final=0
        MALag.final=0
        GARCHLagOne.final=0
        GARCHLagTwo.final=0
        stockDistribution.fullname=bestDistributionFit.fullname
      }else{
        if(GARCHLagOne.final==0){
          if(GARCHLagTwo.final==0){
            garchModel.final="Plain Vanilla ARMA" #Hvis vi bare har en ARMA prosess
          }
        }
      }
      

      cat(paste(Sys.time(), "\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Model: " ,garchModel.final,"(",ARLag.final,MALag.final,GARCHLagOne.final,GARCHLagTwo.final,"). Iterasjon fullført!","\n",sep=""), file=URL.logging, append=TRUE) 
      
      results=list(AIC.final, forecastOneDayAhead.mean.final, garchModel.final,ARLag.final, MALag.final, GARCHLagOne.final, GARCHLagTwo.final, bestDistributionFit.fullname, forecastOneDayAhead.volatility.final) # Merk at man må bruke to brackets for å legge til en liste inni en liste

      names(results)=c("AIC", "One-Day-Ahead Mean Forecast",  "Garch Model","AR Lag","MA Lag", "GARCH Lag 1","GARCH Lag 2","Stock Distribution","One-Day-Ahead VOlatility Forecast" )
      #individualStockResults[[length(individualStockResults)+1]]=results
      return(results)
      
    }
    names(individualStockResults)=index(individualStockRetun)[max(sampleSizes):nrow(individualStockRetun)]
    sampleSizeResults[[length(sampleSizeResults)+1]]=individualStockResults
    
    
  }
  
  names(sampleSizeResults)=sampleSizes
  allStocksResults[[length(allStocksResults)+1]]=sampleSizeResults
  #return(sampleSizeResults)
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
cat(paste("\nKjoreretid:",run_time, "\n"), file=URL.logging, append=TRUE)

stopCluster(c1)