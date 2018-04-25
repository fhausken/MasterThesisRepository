rm(list=ls()) #Clears environment

library(parallel,warn.conflicts = FALSE)
library(doParallel,warn.conflicts = FALSE)
library(quantmod,warn.conflicts = FALSE)
library(lattice,warn.conflicts = FALSE)
library(timeSeries,warn.conflicts = FALSE)
library(rugarch,warn.conflicts = FALSE)
library(xts,warn.conflicts = FALSE)
library(tseries,warn.conflicts = FALSE)
library(bigmemory,warn.conflicts = FALSE)
library(R.utils,warn.conflicts = FALSE)

no_cores=detectCores() -2 #Beholder to logisk kjerne til operativsystem operasjoner
c1=makeCluster(no_cores,type = "PSOCK")
registerDoParallel(c1)

URL.repo=getwd()

URL.kritisk=paste(URL.repo,"/Output/KritiskFeil.txt", sep="")
cat("Output:\n\n", file=URL.kritisk, append=FALSE) #Clears log

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

# gc()
# URL=paste(URL.repo,"/Data/data.bin",sep="")
# remove=file.remove(URL)
# URL=paste(URL.repo,"/Data/data.desc",sep="")
# remove=file.remove(URL)

debugging=F

sampleSizes=c(500,1000)

garchModels=c('sGARCH','gjrGARCH','eGARCH') #'gjrGARCH'

distributions=c("norm","std","snorm") #,"sged","sstd","ghyp","nig","ghst")
distributions.fullname=c("Normal Distribution","Student Distribution","Skewed Normal Distribution") #,"Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution")

ARLag.max=6
MALag.max=6

GARCHLagOne.max=1
GARCHLagTwo.max=1

runARCHInMean.switch=T
archpow.switch=1

timeOutCounter=15
forecastTimeOut=2
distributionFitTimOut=2
dayTimeOutCounter=(timeOutCounter*(ARLag.max+1)*(MALag.max+1)*length(garchModels)*2)

start_time <- Sys.time()
allStocksResults=list()
#allStocksResults=foreach(stocksIndex=1:nrow(stocks)) %dopar%{
for (stocksIndex in 1:nrow(stocks)){
  
  URL.logging=paste(URL.repo,"/Output/ParallellLog.txt", sep="")
  cat("Output:\n\n", file=URL.logging, append=FALSE) #Clears log
  
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
    
    
    daysLeft=seq(from = 0, to = rollingWindowSize, by = 1)
    URL.progress=paste(URL.repo,"/Output/Progress.txt", sep="")
    cat("Output:\n\n", file=URL.progress, append=FALSE) 
    cat(paste(Sys.time(), "\n\n"), file=URL.progress, append=TRUE) 
    cat(daysLeft, file=URL.progress, append=TRUE)
    tryCatch({
      daysLeft=as.big.matrix(daysLeft, backingfile="data.bin",descriptorfile="data.desc",backingpath = paste(URL.repo,"/Data",sep=""),shared=TRUE)
    }, error=function(e){ 
      URL=paste(URL.repo,"/Data/data.desc",sep="")
      daysLeft=attach.big.matrix(URL)
      for (i in 0:(length(daysLeft)-1)){
        daysLeft[i+1]=i
      }})

    individualStockResults=foreach(day=0:rollingWindowSize) %dopar%{
      library(R.utils)
      finished=FALSE
      while (finished==FALSE) {
        dayFinished=tryCatch(withTimeout({
          errorInDay=FALSE
          library(parallel)
          library(doParallel)
          library(quantmod)
          library(lattice)
          library(timeSeries)
          library(rugarch)
          library(xts)
          library(tseries)
          library(bigmemory)
          library(R.utils)
          
          
          URL=paste(URL.repo,"/Data/data.desc",sep="")
          daysLeft=attach.big.matrix(URL)
          
          
          equalStartingPointdjustment=(max(sampleSizes)-sampleSize)
          individualStockReturnOffset = individualStockRetun[(1+equalStartingPointdjustment+day):(sampleSize+equalStartingPointdjustment+day)]
          
          AIC.final.distribution=1000000 #tilsvarer + infinity
          bestDistributionFit.fullname="Normal Distribution"
          bestDistributionFit="norm"
          for (distributions.index in 1:length(distributions)){
            currentDistribution = distributions[distributions.index]
            if (debugging==TRUE){
              URL=paste(URL.repo,"/Debugging/",day,"_1.RData",sep="")
              save(currentDistribution, bestDistributionFit.fullname,file=URL)
           }
            AIC=1000000
            vectorizedReturn=drop(coredata(individualStockReturnOffset))
            fit.distribution=tryCatch({
              fitDistribution=withTimeout({fitdist(distribution = distributions[distributions.index], vectorizedReturn, control = list())},timeout = distributionFitTimOut,elapsed=distributionFitTimOut,onTimeout = "error")}, error=function(e) e, warning=function(w) w)

            if(is(fit.distribution,"warning")){


            } else if(is(fit.distribution,"error")){
              URL=paste(URL.repo,"/Data/ErrorInDistributionFitting.Rda", sep="")
              save(fit.distribution,file=URL)

              if (class(fit.distribution)[1]=="TimeoutException"){
                writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Timeout i distribution fit!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)

              } else{
                writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Error i distribution fit!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)

              }

            }else{

              k=length(fit.distribution$pars)
              maxLikelihood=min(fit.distribution$values)
              AIC=2*k+2*maxLikelihood
            }

            if (AIC.final.distribution>AIC){
              AIC.final.distribution=AIC
              bestDistributionFit.fullname=distributions.fullname[distributions.index]
              bestDistributionFit=distributions[distributions.index]
            }
          }

          if (debugging==TRUE){
            URL=paste(URL.repo,"/Debugging/",day,"_2.RData",sep="")
            save(bestDistributionFit.fullname,fit.distribution,file=URL)
          }

          AIC.final=1000000 # tilsvarer + infinity
          
          
          for (garchModelsIndex in 1:length(garchModels)){
            garchModel=garchModels[garchModelsIndex]
            
            runARCHInMean=FALSE
            if (garchModel!= "sGARCH"){
              runARCHInMean=runARCHInMean.switch
            }
            
            for (ARLag in 0:ARLag.max){
              for (MALag in 0:MALag.max){
                for (GARCHLagOne in 1:GARCHLagOne.max){
                  for (GARCHLagTwo in 1:GARCHLagTwo.max){
                    spec = ugarchspec(
                      variance.model=list(model=garchModel,garchOrder=c(GARCHLagOne,GARCHLagTwo)),
                      mean.model=list(armaOrder=c(ARLag, MALag), include.mean=T,archm=runARCHInMean, archpow=archpow.switch),
                      distribution.model=bestDistributionFit
                    )
                    if (debugging==TRUE){
                      URL=paste(URL.repo,"/Debugging/",day,"_3.RData",sep="")
                      save(garchModel,ARLag,MALag,file=URL)
                    }
                    AIC=1000000
                    fit = tryCatch({
                      fitGARCH= withTimeout({ugarchfit(spec, individualStockReturnOffset, solver = 'solnp')},timeout=timeOutCounter,onTimeout="error")}, error=function(e) e, warning=function(w) w)
                    
                    if(is(fit,"warning")){
                      
                      #cat(paste(Sys.time(), "\t\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Warning i fit!","\n",sep=""), file=URL.logging, append=TRUE)
                      
                    } else if(is(fit,"error")){
                      
                      
                      if (class(fit)[1]=="TimeoutException"){
                        writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Timeout i ARMA GARCH fit!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
                        
                      }else{
                        writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Error i ARMA GARCH fit!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
                        
                      }
                      
                      
                    } else{
                      forecastFitting=tryCatch({
                      withTimeout({forecast = ugarchforecast(fit, n.ahead = 1)},timeout = forecastTimeOut,elapsed=forecastTimeOut,onTimeout = "error")
                      AIC = infocriteria(fit)[1]
                      forecastOneDayAhead.mean = drop(fitted(forecast))
                      forecastOneDayAhead.volatility = drop(sigma(forecast))},warning=function(w) w,error=function(e) e)
                      
                      if(is(forecastFitting,"error")){
                        URL=paste(URL.repo,"/Data/ErrorInForecastFitting.Rda", sep="")
                        save(forecastFitting,file=URL)
                        if (class(forecastFitting)[1]=="TimeoutException"){
                          writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Timeout i forecast fitting!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
                          writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Timeout i forecast fitting!","\n",sep=""), file=URL.kritisk, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
                        }else{
                          writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Error i forecast fitting!!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
                          writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: ",garchModel,"(",ARLag,MALag,GARCHLagOne,GARCHLagTwo,"). Error i forecast fitting!!","\n",sep=""), file=URL.kritisk, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
                        }
                        
                      }
                      
                      
                    }
                    
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
                    if (debugging==TRUE){
                      URL=paste(URL.repo,"/Debugging/",day,"_4.RData",sep="")
                      save(garchModel,ARLag,MALag,fit, AIC,file=URL)
                    
                    } 
                  
                }
                
                if (debugging==TRUE){
                  URL=paste(URL.repo,"/Debugging/",day,"_5.RData",sep="")
                  save(garchModel,ARLag,MALag,fit, AIC,file=URL)
                  
                }
                
              }
              
              if (debugging==TRUE){
                URL=paste(URL.repo,"/Debugging/",day,"_6.RData",sep="")
                save(garchModel,ARLag,MALag,fit, AIC,file=URL)
                
              }
              
            }
            
            if (debugging==TRUE){
              URL=paste(URL.repo,"/Debugging/",day,"_7.RData",sep="")
              save(garchModel,ARLag,MALag,fit, AIC,file=URL)
              
            }
            
          }
          
          if (debugging==TRUE){
            URL=paste(URL.repo,"/Debugging/",day,"_8.RData",sep="")
            save(garchModel,ARLag,MALag,fit, AIC,file=URL)
            
          }
          
          #print(fit)
          
          if (AIC.final==1000000){
            writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1],". Sample size: ",sampleSize ,". Day: ",day,"/" , rollingWindowSize, ". Did Not Converge!"," \n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
            writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1],". Sample size: ",sampleSize ,". Day: ",day,"/" , rollingWindowSize, ". Did Not Converge!"," \n",sep=""), file=URL.kritisk, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
            AIC.final=1000000
            forecastOneDayAhead.mean.final=0
            forecastOneDayAhead.volatility.final=0
            garchModel.final="Ikke Konvergert"
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
          
          writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Distribution: ",bestDistributionFit.fullname, ". Model: " ,garchModel.final,"(",ARLag.final,MALag.final,GARCHLagOne.final,GARCHLagTwo.final,"). Iterasjon fullf??rt!","\n",sep=""), file=URL.logging, append=TRUE) },timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
    
          results=list(AIC.final, forecastOneDayAhead.mean.final, garchModel.final,ARLag.final, MALag.final, GARCHLagOne.final, GARCHLagTwo.final, bestDistributionFit.fullname, forecastOneDayAhead.volatility.final) # Merk at man m?? bruke to brackets for ?? legge til en liste inni en liste
          
          names(results)=c("AIC", "One-Day-Ahead Mean Forecast",  "Garch Model","AR Lag","MA Lag", "GARCH Lag 1","GARCH Lag 2","Stock Distribution","One-Day-Ahead VOlatility Forecast" )
          #individualStockResults[[length(individualStockResults)+1]]=results
          
          daysLeft[day+1]=-1
          daysLeftAsVector=as.vector(daysLeft[1:length(daysLeft)])
          URL.progress=paste(URL.repo,"/Output/Progress.txt", sep="")
          writeFile=tryCatch({withTimeout({cat("Output:\n\n", file=URL.progress, append=FALSE) },timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
          writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\n\n"), file=URL.progress, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
          writeFile=tryCatch({withTimeout({cat(daysLeftAsVector, file=URL.progress, append=TRUE) },timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
          
          
          if (debugging==TRUE){
            URL=paste(URL.repo,"/Debugging/",day,"_9.RData",sep="")
            save(results,file=URL)
          }
          
        },timeout = dayTimeOutCounter,elapsed=dayTimeOutCounter,onTimeout = "error"), error=function(e) e)
          
        if(is(dayFinished,"error")){
          errorInDay=TRUE
          URL=paste(URL.repo,"/Data/ErrorInDay.Rda", sep="")
          save(dayFinished,file=URL)
          if (class(dayFinished)[1]=="TimeoutException"){
            writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Timeout day!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
            writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Timeout day!","\n",sep=""), file=URL.kritisk, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
          } else{
            writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Error day!","\n",sep=""), file=URL.logging, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
            writeFile=tryCatch({withTimeout({cat(paste(Sys.time(), "\t\t","Iteration: ",stocksIndex,"/" , nrow(stocks),". Stock: ",stocks[stocksIndex,1] ,". Sample size: ",sampleSize,". Day: ",day,"/" , rollingWindowSize,". Error day!","\n",sep=""), file=URL.kritisk, append=TRUE)},timeout = 1,elapsed=1,onTimeout = "error")}, error=function(e) e, warning=function(w) w)
          }
        }
        
        if (errorInDay==FALSE){
          finished=TRUE
        }
        
        if (debugging==TRUE){
          URL=paste(URL.repo,"/Debugging/",day,"_10.RData",sep="")
          save(dayFinished,finished,file=URL)
        }
        
      }
      
      if (debugging==TRUE){
        URL=paste(URL.repo,"/Debugging/",day,"_11.RData",sep="")
        save(dayFinished,results,file=URL)
      }
      
      return(results)
    }
    
    names(individualStockResults)=index(individualStockRetun)[max(sampleSizes):nrow(individualStockRetun)]
    sampleSizeResults[[length(sampleSizeResults)+1]]=individualStockResults
    
  }
  
  names(sampleSizeResults)=sampleSizes
  allStocksResults[[length(allStocksResults)+1]]=sampleSizeResults
  #return(sampleSizeResults)
}

stopCluster(c1)

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