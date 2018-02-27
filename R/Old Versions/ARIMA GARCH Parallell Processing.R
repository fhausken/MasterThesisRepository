#install.packages("quantmod")
#install.packages("lattice")
#install.packages("timeSeries")




setwd("C:/Users/Fredrik Hausken/OneDrive/Skole/NTNU/Høst 2017/Prosjekt/Progging/projectFinanceMaster/RStudio/ARIMA GARCH")

# Import the necessary libraries
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
library(readr)
library(xts)
library(parallel)
library(doParallel)


no_cores=detectCores()-1
c1=makeCluster(no_cores,outfile="")
registerDoParallel(c1)

#Read CSV with tickers

ticker_companies <- read_csv("C:/Users/Fredrik Hausken/OneDrive/Skole/NTNU/Høst 2017/Prosjekt/Progging/projectFinanceMaster/Python/CSV-files/Returns Segmented/ticker_companies.csv", 
                             col_names = FALSE)

tickers=ticker_companies$X1

status=list()

status[length(status)+1]=0
statusOutput=data.frame(status)

destination="C:/Users/Fredrik Hausken/OneDrive/Skole/NTNU/Høst 2017/Prosjekt/Progging/projectFinanceMaster/Python/CSV-files/ARIMA GARCH Parameters/"
write.csv(statusOutput, file=paste(destination,"status.csv",sep=""),col.names = FALSE, row.names = FALSE,append = FALSE)


forloop=foreach(companyNumber=1:length(tickers)) %dopar%{
  library(quantmod)
  library(lattice)
  library(timeSeries)
  library(rugarch)
  library(readr)
  library(xts)
  library(parallel)
  library(doParallel)
  
  outfile="test"
  ticker=tickers[companyNumber]
  
  #ticker="COV-NO" #debugging
  
  URL=paste("C:/Users/Fredrik Hausken/OneDrive/Skole/NTNU/Høst 2017/Prosjekt/Progging/projectFinanceMaster/Python/CSV-files/Returns Segmented/",
            ticker,"_SR_data.csv",sep="")
  
  companyReturn <- read_csv(URL,col_names = FALSE, col_types = cols(X1 = col_date(format = "%m/%d/%Y")))
  
  
  stockLogReturns<-xts(companyReturn[, -1], order.by=(companyReturn$X1))
  
  
  # Create the forecasts vector to store the predictions
  windowLength = 250
  foreLength = length(stockLogReturns) - windowLength
  
  # Lager en lister for output
  ARIMAparameters <- vector() 
  ARIMAorder<- vector()
  GARCHparameters <- vector()
  epsilons=vector()
  variances=vector()

  
  for (d in 0:foreLength) {
    kukk=0
    whileTest=101
    while (whileTest>100) {
      # Obtain the S&P500 rolling window for this day
      #d=1 #debugging
      stockLogReturnsOffset = stockLogReturns[(1+d):(windowLength+d)] #Her slicer man returnsene. Merk at R er 1 indeksert
      
      # Fit the ARIMA model
      final.aic <- Inf
      final.order <- c(0,0,0)
      for (p in 0:3) for (q in 0:3) {
        if ( p == 0 && q == 0) {
          next
        }
        
        arimaFit = tryCatch( arima(stockLogReturnsOffset, order=c(p, 0, q), include.mean = TRUE),
                             error=function( err ) FALSE,
                             warning=function( err ) FALSE )
        
        if( !is.logical( arimaFit ) ) {
          current.aic <- AIC(arimaFit)
          if (current.aic < final.aic) {
            final.aic <- current.aic
            final.order <- c(p, 0, q)
            final.arima <- arimaFit
          }
        } else {
          next
        }
      }
      
      # Specify and fit the GARCH model
      spec = ugarchspec(
        variance.model=list(garchOrder=c(1,1)),
        mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
        distribution.model="norm"
      )
      fit = tryCatch(
        ugarchfit(spec, stockLogReturnsOffset, solver = 'hybrid')
        , error=function(e) e, warning=function(w) w
      )
      
      if(is(fit,"warning")){
        message("ARMA: (w)")
        message(companyNumber,"/",length(tickers),"  ",ticker, "  Day: ", windowLength+d, "  d: ", d, "  for-length: ", foreLength)     
        print(final.arima$coef)
        #message("While: ",kukk)
        #print(logLik(final.arima))
        #print(final.arima$residuals)
        
        #message(final.arima$residuals[windowLength])
        
        whileTest=abs(final.arima$residuals[windowLength])
        if (abs(final.arima$residuals[windowLength])>100){
          kukk=kukk+1
          next
        }
        
        
        GARCHparameters[length(GARCHparameters)+1]=0
        GARCHparameters[length(GARCHparameters)+1]=0
        GARCHparameters[length(GARCHparameters)+1]=0
        
        for (i in 1:length(final.arima$coef)){
          ARIMAparameters[length(ARIMAparameters)+1]=unname(final.arima$coef[i],force=FALSE)
        }
        
        for (i in 1:length(final.order)){
          ARIMAorder[length(ARIMAorder)+1]=final.order[i]
        }
        
        epsilons=c(epsilons,unname(final.arima$residuals,force=FALSE))
        
      }
      else if(is(fit," error")){
        message("ARMA: (e) ")
        message(companyNumber,"/",length(tickers),"  ",ticker, "  Day: ", windowLength+d, "  d: ", d, "  for-length: ", foreLength)   
        print(final.arima$coef)
        #message("While: ",kukk)
        #print(logLik(final.arima))
        #print(final.arima$residuals)
        
        #message(final.arima$residuals[windowLength])
        
        whileTest=abs(final.arima$residuals[windowLength])
        if (abs(final.arima$residuals[windowLength])>100){
          kukk=kukk+1
          next
        }
        
        GARCHparameters[length(GARCHparameters)+1]=0
        GARCHparameters[length(GARCHparameters)+1]=0
        GARCHparameters[length(GARCHparameters)+1]=0
        
        for (i in 1:length(final.arima$coef)){
          ARIMAparameters[length(ARIMAparameters)+1]=unname(final.arima$coef[i],force=FALSE)
        }
        
        for (i in 1:length(final.order)){
          ARIMAorder[length(ARIMAorder)+1]=final.order[i]
        }
        
        epsilons=c(epsilons,unname(final.arima$residuals,force=FALSE))
        
        
      }
      else{
        message("ARMA-GARCH: ")
        message(companyNumber,"/",length(tickers),"  ",ticker, "  Day: ", windowLength+d, "  d: ", d, "  for-length: ", foreLength) 
        print(fit@fit$coef)
        message("While: ",kukk)
        #print(logLik(final.arima))
        #print(fit@fit$residuals)
        
        message(fit@fit$residuals[windowLength])
        
        whileTest=abs(fit@fit$residuals[windowLength])
        if (abs(fit@fit$residuals[windowLength])>100){
          kukk=kukk+1
          next
        }
        
        lengthLoop=length(fit@fit$coef)-3
        for (i in 2:lengthLoop){
          ARIMAparameters[length(ARIMAparameters)+1]=unname(fit@fit$coef[i],force=FALSE)
        }
        
        ARIMAparameters[length(ARIMAparameters)+1]=unname(fit@fit$coef[1],force=FALSE)
        
        for (i in 1:length(final.order)){
          ARIMAorder[length(ARIMAorder)+1]=final.order[i]
        }
        
        epsilons=c(epsilons,unname(fit@fit$residuals,force=FALSE))
        
        GARCHparameters[length(GARCHparameters)+1]=unname(fit@fit$coef[final.order[1]+final.order[3]+2],force=FALSE)
        GARCHparameters[length(GARCHparameters)+1]=unname(fit@fit$coef[final.order[1]+final.order[3]+3],force=FALSE)
        GARCHparameters[length(GARCHparameters)+1]=unname(fit@fit$coef[final.order[1]+final.order[3]+4],force=FALSE)
        
        variances=c(variances,unname(fit@fit$var,force=FALSE))
        
      }
      print("")
      print("------------------------------------------------------")
      print("")
    }
    
    

  }
  
  
  
  # Output the CSV file to "forecasts.csv"
  destination="C:/Users/Fredrik Hausken/OneDrive/Skole/NTNU/Høst 2017/Prosjekt/Progging/projectFinanceMaster/Python/CSV-files/ARIMA GARCH Parameters/"
  
  output1=data.frame(ARIMAparameters)
  write.csv(output1, file=paste(destination,ticker,"_","ARIMAParameters.csv",sep=""), row.names=FALSE)
  output2=data.frame(ARIMAorder)
  write.csv(output2, file=paste(destination,ticker,"_","ARIMAOrder.csv",sep=""), row.names=FALSE)
  #output3=data.frame(GARCHparameters)
  #write.csv(output3, file=paste(destination,ticker,"_","GARCHParameters.csv",sep=""), row.names=FALSE)
  output4=data.frame(epsilons)
  write.csv(output4, file=paste(destination,ticker,"_","Epsilons.csv",sep=""), row.names=FALSE)
  #output5=data.frame(variances)
  #write.csv(output5, file=paste(destination,ticker,"_","Variances.csv",sep=""), row.names=FALSE)
  
  status[length(status)+1]=ticker
  statusOutput=data.frame(status)
  write.csv(statusOutput, file=paste(destination,"status.csv",sep=""),col.names = FALSE, row.names = FALSE,append = TRUE)
  
}
stopCluster(c1)
