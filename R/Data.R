#install.packages("psych")

library(tseries)
library(rugarch)
library(psych)
library(quantmod)
library(xtable)
library(plotly)
library(parallel)
library(doParallel)
library(stats)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


rm(list=ls()) #Clears environment

URL.repo=getwd()

if (grepl("Fredrik", URL.repo)){
  URL.drop="C:/Users/Fredrik Hausken/Dropbox/Apper/ShareLaTeX/Master thesis"
}else if (grepl("andersronold", URL.repo)){
  URL.drop="/Users/andersronold/Dropbox/Apper/ShareLaTeX/Master\ thesis" #Anders ma fylle inn
}else{
  URL.drop="Does not find"
}

#Input
biggestSampleSize=1000
PLOTTING=F

# RETRIEVE DATA SETS 
URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocksRemoved.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/OBXReturn.Rda",sep="")
load(URL)


#Slice Rows in Stock Returns

stockReturns=stockReturns[(biggestSampleSize+1):nrow(stockReturns),1:ncol(stockReturns)]
OBX.close.return=OBX.close.return[(biggestSampleSize+1):nrow(OBX.close.return)]

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

# CALCULATE DISTRIBUTION MEAN FUNCTION
getDistributionColMeans <- function(dataFrame) {
  dataFrame = dataFrame[-c(ncol(dataFrame))]
  endNumVector = c()
  mostCommonDist = c()
  for (i in 1:ncol(dataFrame)) {
    if (is.numeric(as.numeric(levels(distributionsFitResults[2,i])))) {
      if (is.na(mean(as.numeric(levels(distributionsFitResults[,i]))))){
        mostCommonDist = cbind(mostCommonDist,names(which.max(table(levels(distributionsFitResults[,i])))))
      }
      else {
        endNumVector = cbind(endNumVector,mean(as.numeric(levels(distributionsFitResults[,i]))))
      }
    }
    
  }
  return(data.frame(endNumVector,c(mostCommonDist)))
}
 
#DESCRIPTIVE STATISTICS
results=vector()
numberOfStocks=nrow(stocks)

for (stock in 1:numberOfStocks){
  vectorizedReturn=drop(coredata(stockReturns[,stock]))
  descriptiveStatistics=summary(vectorizedReturn)
  results[length(results)+1]=round(length(vectorizedReturn),digits = 0)
  results[length(results)+1]=min(vectorizedReturn)
  results[length(results)+1]=descriptiveStatistics[2] # 1. kvantil
  results[length(results)+1]=median(vectorizedReturn)
  results[length(results)+1]=mean(vectorizedReturn)
  results[length(results)+1]=sd(vectorizedReturn) #Bruker sample (n-1)
  results[length(results)+1]=var(vectorizedReturn) #Bruker sample (n-1)
  results[length(results)+1]=descriptiveStatistics[5] # 3. kvantil
  results[length(results)+1]=max(vectorizedReturn)
  results[length(results)+1]=skew(vectorizedReturn) #Bruker sample (n-1)
  results[length(results)+1]=kurtosi(vectorizedReturn) #Bruker sample (n-1) -------> Burde vel bruke excess kurtosis?

}

descriptiveStatisticsResults=data.frame(stocks[,1],matrix(results, ncol=11, byrow=TRUE))

# SORT BY VOL
#descriptiveStatisticsResults[order(descriptiveStatisticsResults$X6),]

names(descriptiveStatisticsResults)=c("Stocks","n","Min","1. Quant.", "Median", "$\\boldsymbol{\\mu}$", "$\\boldsymbol{\\sigma}$", "$\\boldsymbol{\\sigma^2}$", "3. Quant.", "Max", "Skew", "Kurtosis" )

# CALCULATE MEAN FOR STOCKS AND OBX
descriptiveStatisticsResults.average = as.numeric(as.vector(getColMeans(descriptiveStatisticsResults)[1,]))

vectorizedReturnOBX=drop(coredata(OBX.close.return[,1]))
descriptiveStatisticsOBX=summary(vectorizedReturnOBX)

OBX.average = list()
OBX.average[length(OBX.average)+1]=round(length(vectorizedReturnOBX),digits = 0)
OBX.average[length(OBX.average)+1]=min(vectorizedReturnOBX)
OBX.average[length(OBX.average)+1]=descriptiveStatisticsOBX[2] # 1. kvantil
OBX.average[length(OBX.average)+1]=median(vectorizedReturnOBX)
OBX.average[length(OBX.average)+1]=mean(vectorizedReturnOBX)
OBX.average[length(OBX.average)+1]=sd(vectorizedReturnOBX) #Bruker sample (n-1)
OBX.average[length(OBX.average)+1]=var(vectorizedReturnOBX) #Bruker sample (n-1)
OBX.average[length(OBX.average)+1]=descriptiveStatisticsOBX[5] # 3. kvantil
OBX.average[length(OBX.average)+1]=max(vectorizedReturnOBX)
OBX.average[length(OBX.average)+1]=skew(vectorizedReturnOBX) #Bruker sample (n-1)
OBX.average[length(OBX.average)+1]=kurtosi(vectorizedReturnOBX) #Bruker sample (n-1) -------> Burde vel bruke excess kurtosis?

OBX.average = as.numeric(as.vector(OBX.average))

URL=paste(URL.repo,"/Data/descriptiveStatisticsResults.Rda",sep="")
save(descriptiveStatisticsResults,file=URL)

#PHILLIP PERRON TEST
testStatistic=vector()
pValue=vector()
testConclusion=vector()

for (stock in 1:numberOfStocks){
  
  phillip.perron.test = pp.test(stockReturns[,stock])
  
  testStatistic[length(testStatistic)+1]=phillip.perron.test$statistic
  pValue[length(pValue)+1]=phillip.perron.test$p.value
  
  if (pValue <= 0.01){
    testConclusion[length(testConclusion)+1]="Stationary"
  }
  else{
    testConclusion[length(testConclusion)+1]="Not Stationary"
  }
}

phillipPerronResults=data.frame(stocks[,1],testStatistic, pValue, testConclusion)
names(phillipPerronResults)=c("Stock", "Test Statistic", "P-Value", "Test Conlcusion")

# CALCULATE MEAN
phillipPerronResults.average = as.numeric(as.vector(getColMeans(phillipPerronResults)[1,]))

URL=paste(URL.repo,"/Data/phillipPerronResults.Rda",sep="")
save(phillipPerronResults,file=URL)

#DISTRIBUTION FITTING
distributions=c("norm","ged","std","snorm","sged","sstd","ghyp","nig","ghst")
URL=paste(URL.repo,"/Data/distributions.Rda",sep="")
save(distributions,file=URL)

distributions.fullname=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution")
URL=paste(URL.repo,"/Data/distributionsFullname.Rda",sep="")
save(distributions.fullname,file=URL)

distributionsFitResults = data.frame(stringsAsFactors = FALSE)
for (stock in 1:numberOfStocks){
  minAIC=100000000 #tilsvarer + infinity
  bestDistributionFit=""
  resultsForStock=c()
  for (distributions.index in 1:length(distributions)){
    
    vectorizedReturn=drop(coredata(stockReturns[,stock]))
    fit.distribution <- fitdist(distribution = distributions[distributions.index], vectorizedReturn, control = list())
    
    k=length(fit.distribution$pars)
    maxLikelihood=min(fit.distribution$values)
    AIC=2*k+2*maxLikelihood #Er dette riktig!?
    
    if (minAIC>=AIC){
      minAIC=AIC
      bestDistributionFit.fullname=distributions.fullname[distributions.index]
      bestDistributionFit=distributions[distributions.index]
    }
    
    resultsForStock=cbind(resultsForStock,round(AIC,digits = 0))
  }
  resultsForStock=cbind(resultsForStock,toupper(bestDistributionFit))
  resultsForStock=cbind(resultsForStock,toupper(bestDistributionFit))
  
  distributionsFitResults = rbind(distributionsFitResults,resultsForStock)
}

names(distributionsFitResults)=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution", "Best Fit Fullname", "Best Fit Shortname")

distributionsFitResults.average = getDistributionColMeans(distributionsFitResults)

distributionsFitResults = cbind(stocks[,1],distributionsFitResults)
names(distributionsFitResults)=c("Stock","Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution", "Best Fit", "Best Fit")
names(distributionsFitResults.average)=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution", "Best Fit")

URL=paste(URL.repo,"/Data/distributionFitResults.Rda",sep="")
save(distributionsFitResults,file=URL)


#Plotting


if (PLOTTING==T){
  
  no_cores=detectCores() -2 #Beholder to logisk kjerne til operativsystem operasjoner
  c1=makeCluster(no_cores)
  registerDoParallel(c1)
  
  individualStockReturnACF=vector()
  individualStockReturnPACF=vector()
  individualStockSquaredReturnACF=vector()
  individualStockSquaredReturnPACF=vector()
  
  individualStockPlot=foreach(stock=1:numberOfStocks) %dopar%{
    library(tseries)
    library(rugarch)
    library(psych)
    library(quantmod)
    library(xtable)
    library(plotly)
    library(stats)
  
    returnPlot=plot_ly(x=index(stockReturns),y=drop(coredata(stockReturns[,stock])),type="scatter",mode="lines")
    returnPlot=layout(returnPlot,yaxis=list(title="Return"), xaxis=list(title="Date"))
    
    stockTicker=stocks[stock,2]
    URL=paste(URL.drop,"/Plot/ConstituentsReturn/",stockTicker,"_ReturnPlot.jpeg",sep="")
    export(returnPlot, file = URL)
    
    print(returnPlot)
    
    squaredReturnPlot=plot_ly(x=index(stockReturns),y=(drop(coredata(stockReturns[,stock])))^2,type="scatter",mode="lines")
    squaredReturnPlot=layout(squaredReturnPlot,yaxis=list(title="Squared Return"), xaxis=list(title="Date"))
    
    stockTicker=stocks[stock,2]
    URL=paste(URL.drop,"/Plot/ConstituentsReturn/",stockTicker,"_SquaredReturnPlot.jpeg",sep="")
    export(squaredReturnPlot, file = URL)
    
    print(squaredReturnPlot)
    
    
    ACF=acf(drop(coredata(stockReturns[,stock])),lag.max = 30,plot = F)
    confidenceInterval=1.96/sqrt(length(drop(coredata(stockReturns[,stock]))))
    plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(ACF$lag)-1)
    negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(ACF$lag)-1)
    
    
    ACF=plot_ly(x=drop(ACF$lag)[-1]) %>%
      add_trace(y =drop(ACF$acf)[-1],type="bar")%>%
      add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      layout(yaxis=list(title="ACF"),xaxis=list(title="Lag"),showlegend=F)
    
    URL=paste(URL.drop,"/Plot/ACF/",stockTicker,"_ACF.jpeg",sep="")
    export(ACF, file = URL)
    
    print(ACF)
    
    ACF=acf(drop(coredata(stockReturns[,stock]))^2,lag.max = 30, plot = F)
    confidenceInterval=1.96/sqrt(length(drop(coredata(stockReturns[,stock]))))
    plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(ACF$lag)-1)
    negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(ACF$lag)-1)
    
    
    squaredACF=plot_ly(x=drop(ACF$lag)[-1]) %>%
      add_trace(y =drop(ACF$acf)[-1],type="bar")%>%
      add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      layout(yaxis=list(title="ACF"),xaxis=list(title="Lag"),showlegend=F)
    
    URL=paste(URL.drop,"/Plot/ACF/",stockTicker,"_SquaredACF.jpeg",sep="")
    export(squaredACF, file = URL)
    
    print(squaredACF)
    
    PACF=pacf(drop(coredata(stockReturns[,stock])),lag.max = 30,plot = F)
    confidenceInterval=1.96/sqrt(length(drop(coredata(stockReturns[,stock]))))
    plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(PACF$lag)-1)
    negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(PACF$lag)-1)
    
    
    PACF=plot_ly(x=drop(PACF$lag)[-1]) %>%
      add_trace(y =drop(PACF$acf)[-1],type="bar")%>%
      add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      layout(yaxis=list(title="Partial ACF"),xaxis=list(title="Lag"),showlegend=F)
    
    URL=paste(URL.drop,"/Plot/PACF/",stockTicker,"_PACF.jpeg",sep="")
    export(PACF, file = URL)
    
    print(PACF)
    
    PACF=pacf(drop(coredata(stockReturns[,stock]))^2,lag.max = 30, plot = F)
    confidenceInterval=1.96/sqrt(length(drop(coredata(stockReturns[,stock]))))
    plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(PACF$lag)-1)
    negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(PACF$lag)-1)
    
    
    squaredPACF=plot_ly(x=drop(PACF$lag)[-1]) %>%
      add_trace(y =drop(PACF$acf)[-1],type="bar")%>%
      add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
      layout(yaxis=list(title="Partial ACF"),xaxis=list(title="Lag"),showlegend=F)
    
    URL=paste(URL.drop,"/Plot/PACF/",stockTicker,"_SquaredPACF.jpeg",sep="")
    export(squaredPACF, file = URL)
    
    print(squaredPACF)
    
    
  }
  
  stopCluster(c1)
  
  returnPlot=plot_ly(x=index(OBX.close.return),y=drop(coredata(OBX.close.return)),type="scatter",mode="lines")
  returnPlot=layout(returnPlot,yaxis=list(title="OBX Total Return Index Return"), xaxis=list(title="Date"))

  URL=paste(URL.drop,"/Plot/ConstituentsReturn/OBXTotalReturnIndex_ReturnPlot.jpeg",sep="")
  export(returnPlot, file = URL)
  
  print(returnPlot)
  
  returnPlot=plot_ly(x=index(OBX.close.return),y=(drop(coredata(OBX.close.return)))^2,type="scatter",mode="lines")
  returnPlot=layout(returnPlot,yaxis=list(title="OBX Total Return Index Squared Return"), xaxis=list(title="Date"))
  
  URL=paste(URL.drop,"/Plot/ConstituentsReturn/OBXTotalReturnIndex_SquaredReturnPlot.jpeg",sep="")
  export(returnPlot, file = URL)
  
  print(returnPlot)
  
  ACF=acf((drop(coredata(OBX.close.return))),lag.max = 50, plot = F)
  confidenceInterval=1.96/sqrt(length(drop(coredata(OBX.close.return))))
  plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(ACF$lag)-1)
  negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(ACF$lag)-1)
  
  
  ACF=plot_ly(x=drop(ACF$lag)[-1]) %>%
    add_trace(y =drop(ACF$acf)[-1],type="bar")%>%
    add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    layout(yaxis=list(title="ACF"),xaxis=list(title="Lag"),showlegend=F)
  
  URL=paste(URL.drop,"/Plot/ACF/OBXTotalReturnIndex_ACF.jpeg",sep="")
  export(ACF, file = URL)
  
  print(ACF)
  
  ACF=acf((drop(coredata(OBX.close.return)))^2,lag.max = 50, plot = F)
  confidenceInterval=1.96/sqrt(length(drop(coredata(OBX.close.return))))
  plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(ACF$lag)-1)
  negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(ACF$lag)-1)
  
  
  squaredACF=plot_ly(x=drop(ACF$lag)[-1]) %>%
    add_trace(y =drop(ACF$acf)[-1],type="bar")%>%
    add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    layout(yaxis=list(title="ACF"),xaxis=list(title="Lag"),showlegend=F)
  
  URL=paste(URL.drop,"/Plot/ACF/OBXTotalReturnIndex_SquaredACF.jpeg",sep="")
  export(squaredACF, file = URL)
  
  print(squaredACF)
  
  PACF=pacf((drop(coredata(OBX.close.return))),lag.max = 50, plot = F)
  confidenceInterval=1.96/sqrt(length(drop(coredata(OBX.close.return))))
  plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(PACF$lag)-1)
  negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(PACF$lag)-1)
  
  
  PACF=plot_ly(x=drop(PACF$lag)[-1]) %>%
    add_trace(y =drop(PACF$acf)[-1],type="bar")%>%
    add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    layout(yaxis=list(title="Partial ACF"),xaxis=list(title="Lag"),showlegend=F)
  
  URL=paste(URL.drop,"/Plot/PACF/OBXTotalReturnIndex_PACF.jpeg",sep="")
  export(PACF, file = URL)
  
  print(PACF)
  
  PACF=pacf((drop(coredata(OBX.close.return)))^2,lag.max = 50, plot = F)
  confidenceInterval=1.96/sqrt(length(drop(coredata(OBX.close.return))))
  plussConfindenceVector=seq(from = confidenceInterval, to = confidenceInterval, length.out = length(PACF$lag)-1)
  negativeConfindenceVector=seq(from = (-1*confidenceInterval), to = (-1*confidenceInterval), length.out = length(PACF$lag)-1)
  
  
  squaredPACF=plot_ly(x=drop(PACF$lag)[-1]) %>%
    add_trace(y =drop(PACF$acf)[-1],type="bar")%>%
    add_trace(y = plussConfindenceVector, type='scatter', mode = 'lines', line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    add_trace(y = negativeConfindenceVector, type='scatter', mode = 'lines',line = list(color = 'rgb(255,127,80)',dash = 'dash'))%>%
    layout(yaxis=list(title="Partial ACF"),xaxis=list(title="Lag"),showlegend=F)
  
  URL=paste(URL.drop,"/Plot/PACF/OBXTotalReturnIndex_SquaredPACF.jpeg",sep="")
  export(squaredPACF, file = URL)
  
  print(squaredPACF)
  
  
}

#ACF and PACF Tables
boldTable=function(vector,confidenceLevel){
  roundedVector=c()
  newVector=c()
  for (index in 1:length(vector)){
    if (abs(vector[index])>=confidenceLevel){
      roundedVector[length(roundedVector)+1]=format(round(vector[index], 3), nsmall = 3)
      newVector[length(newVector)+1]=paste("\\textbf{",roundedVector[index],"}",sep="")
    }else{
      roundedVector[length(roundedVector)+1]=format(round(vector[index], 3), nsmall = 3)
      newVector[length(newVector)+1]=paste("",roundedVector[index],"",sep="")
    }
  }
  return(newVector)
  
}

confidenceLevel=1.96/sqrt(length(drop(coredata(OBX.close.return))))
for (stock in 1:nrow(stocks)){
  
  returnACF=acf(drop(coredata(stockReturns[,stock])),lag.max = 12,plot = F)$acf[-1]
  returnACF=boldTable(returnACF,confidenceLevel)
  returnPACF=pacf(drop(coredata(stockReturns[,stock])),lag.max = 12,plot = F)$acf
  returnPACF=boldTable(returnPACF,confidenceLevel)
  squaredReturnACF=acf(drop(coredata(stockReturns[,stock]))^2,lag.max = 12,plot = F)$acf[-1]
  squaredReturnACF=boldTable(squaredReturnACF,confidenceLevel)
  squaredReturnPACF=pacf(drop(coredata(stockReturns[,stock]))^2,lag.max = 12,plot = F)$acf
  squaredReturnPACF=boldTable(squaredReturnPACF,confidenceLevel)

  if (stock==1){
    stockReturnACF=data.frame(returnACF)
    stockReturnPACF=data.frame(returnPACF)
    
    stockSquaredReturnACF=data.frame(squaredReturnACF)
    stockSquaredReturnPACF=data.frame(squaredReturnPACF)
  }else{
    stockReturnACF=cbind(stockReturnACF,returnACF)
    stockReturnPACF=cbind(stockReturnPACF,returnPACF)
    
    stockSquaredReturnACF=cbind(stockSquaredReturnACF,squaredReturnACF)
    stockSquaredReturnPACF=cbind(stockSquaredReturnPACF,squaredReturnPACF)
  }
}
stockReturnACF=data.frame(t(stockReturnACF))
names(stockReturnACF)=seq(from =1, to = 12, by = 1)

stockReturnPACF=data.frame(t(stockReturnPACF))
names(stockReturnPACF)=seq(from =1, to = 12, by = 1)

stockSquaredReturnACF=data.frame(t(stockSquaredReturnACF))
names(stockSquaredReturnACF)=seq(from =1, to = 12, by = 1)

stockSquaredReturnPACF=data.frame(t(stockSquaredReturnPACF))
names(stockSquaredReturnPACF)=seq(from =1, to = 12, by = 1)


stockNameColumn=data.frame(stocks[[1]])
names(stockNameColumn)=c("Stock")

stockReturnACF=cbind(stockNameColumn,stockReturnACF)
stockReturnPACF=cbind(stockNameColumn,stockReturnPACF)
stockSquaredReturnACF=cbind(stockNameColumn,stockSquaredReturnACF)
stockSquaredReturnPACF=cbind(stockNameColumn,stockSquaredReturnPACF)

# TABLES-TO-LATEX
bold <- function(x){
  paste0('{\\bfseries ', x, '}')
}

createAverageLineOBX <- function(x, digits,totCols) {
  resultString = ""
  for(i in 1:length(x)) {
    if (i == 1) {
      resultString = paste0(resultString," \\hline & ","{ \\bfseries ", "OBX ","} &", "{ \\bfseries ",round(x[i], digits = digits),"}")
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

createAverageLineDistribution <- function(x, digits,totCols) {
  
  resultString = ""
  for(i in 1:length(x)) {
    if (i == 1) {
      resultString = paste0(resultString," \\hline & ","{ \\bfseries ", "Average ","} &", "{ \\bfseries ",round(x[i], digits = digits),"}")
    }
    else {
      if (i==length(x)) {
        resultString = paste0(resultString, " & ","{ \\bfseries ", levels(x[i][1,1]),"}")
      }
      else {
        resultString = paste0(resultString, " & ","{ \\bfseries ", round(x[i], digits = digits),"}")
      }
    }
  }
  return(resultString)
}

createDigitsandAlignVectors <- function(dataFrame,digits,descStat) {
  colDim = dim(dataFrame)[2] - 1
  
  alignVector = c('c','l')
  for (i in 1:colDim) {
    alignVector = c(alignVector,'c')
  }
  
  digitsVector = c(1,1)
  for (i in 1:colDim) {
    if(descStat == TRUE) {
      digitsVector = c(digitsVector,0)
      descStat = FALSE
    }
    else {
      digitsVector = c(digitsVector,digits)  
    }
  }

  return(list(alignVector,digitsVector))
}


# DESCRIPTIVE STATISTICS
x = descriptiveStatisticsResults
digits = 4
descStat = TRUE
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,descStat)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),paste0(createAverageLine(descriptiveStatisticsResults.average,digits,ncol(x)),"\n \\",createAverageLineOBX(OBX.average,digits,ncol(x))))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 0
add.to.row$pos[[2]] = nrow(descriptiveStatisticsResults)


add.to.row$command <- command

URL=paste(URL.drop,"/Tables/descriptiveStatisticsResults.txt",sep="")
print(xtable(descriptiveStatisticsResults, auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Descriptive Statistics for the OBX Constituents",label="DescriptiveStats"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row, tabular.environment = "longtable",file = URL)

# PHILLIP PERRON TEST
x = phillipPerronResults
digits = 3
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),paste0(createAverageLine(phillipPerronResults.average,digits,ncol(x)),"{ \\bfseries Stationary } &"))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 0
add.to.row$pos[[2]] = nrow(phillipPerronResults)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/ppResults.txt",sep="")
print(xtable(phillipPerronResults, auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Phillips-Perron Test for the OBX Constituents", label="StationarityTest"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# DISTRIBUTION FIT
x = distributionsFitResults[-c(ncol(distributionsFitResults))]
digits = 0
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 0
add.to.row$pos[[2]] = nrow(x)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/distributionFitResults.txt", sep="")
names(x)=c("Stock", "NORM","GED","STD","SNORM","SGED","SSTD","GHYP","NIG","GHST","Best Fit")
print(xtable(x, auto=TRUE, display = c('d','s','f','f','f','f','f','f','f','f','f','s'),digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "AIC Results for Various Distributions for the OBX Constituents", label="DistributionFits"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# STOCKS
x = stocks
digits = 0
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),"")#,createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] <- 0
add.to.row$pos[[2]] <- nrow(x)
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/stocks.txt", sep="")
names(x)=c("Stock","Ticker")
print(xtable(x, auto=TRUE,digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "The OBX Constituents Used in This Analysis", label="Stocks"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row, tabular.environment = "longtable", file = URL) 


# STOCKS REMOVED
x = stocksRemoved
digits = 0
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),"")#,createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] <- 0
add.to.row$pos[[2]] <- nrow(x)
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/stocksRemoved.txt", sep="")

names(x)=c("Stock","Ticker", "Reason for removal")
print(xtable(x, auto=TRUE,digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "The Removed OBX Constituents Not Used in This Analysis", label="StocksRemoved"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)




# STOCK RETURN ACF
x = stockReturnACF
digits = 3
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),"")#,createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 0
add.to.row$pos[[2]] = nrow(x)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/stockReturnsACF.txt", sep="")
print(xtable(x, auto=TRUE,digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Stock Return ACF Results", label="ACF"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# STOCK RETURN PACF
x = stockReturnPACF
digits = 3
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),"")#,createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 0
add.to.row$pos[[2]] = nrow(x)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/stockReturnsPACF.txt", sep="")
print(xtable(x, auto=TRUE,digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Stock Return PACF Results", label="PACF"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)


# STOCK SQUARED RETURN ACF
x = stockSquaredReturnACF
digits = 3
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),"")#,createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 0
add.to.row$pos[[2]] = nrow(x)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/stockSquaredReturnsACF.txt", sep="")
print(xtable(x, auto=TRUE,digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Stock Squared Return ACF Results", label="SquaredACF"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# STOCK SQUARED RETURN PACF
x = stockSquaredReturnPACF
digits = 3
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits,FALSE)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),"")#,createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 0
add.to.row$pos[[2]] = nrow(x)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/stockSquaredReturnsPACF.txt", sep="")
print(xtable(x, auto=TRUE,digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Stock Squared Return PACF Results", label="SquaredPACF"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

