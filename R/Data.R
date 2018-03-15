#install.packages("psych")

library(tseries)
library(rugarch)
library(psych)
library(quantmod)
library(xtable)
library(het.test)
library(lmtest)
library(vars)

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

# RETRIEVE DATA SETS 
URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocksRemoved.Rda",sep="")
load(URL)

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

descriptiveStatisticsResults=data.frame(stocks[,1],matrix(results, ncol=10, byrow=TRUE))
names(descriptiveStatisticsResults)=c("Stocks","Min","1st Quantile", "Median", "$\\boldsymbol{\\mu}$", "$\\boldsymbol{\\sigma}$", "$\\boldsymbol{\\sigma^2}$", "3rd Quantile", "Max", "Skew", "Kurtosis" )

# CALCULATE MEAN
descriptiveStatisticsResults.average = as.numeric(as.vector(getColMeans(descriptiveStatisticsResults)[1,]))

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
    
    resultsForStock=cbind(resultsForStock,round(AIC,digits = 1))
  }
  resultsForStock=cbind(resultsForStock,bestDistributionFit.fullname)
  resultsForStock=cbind(resultsForStock,bestDistributionFit)
  
  distributionsFitResults = rbind(distributionsFitResults,resultsForStock)
}

names(distributionsFitResults)=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution", "Best Fit Fullname", "Best Fit Shortname")


distributionsFitResults.average = getDistributionColMeans(distributionsFitResults)

distributionsFitResults = cbind(stocks[,1],distributionsFitResults)
names(distributionsFitResults)=c("Stock","Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution", "Best Fit Fullname", "Best Fit Shortname")
names(distributionsFitResults.average)=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution", "Best Fit Fullname")

URL=paste(URL.repo,"/Data/distributionFitResults.Rda",sep="")
save(distributionsFitResults,file=URL)

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
        resultString = paste0(resultString, " & ","{ \\bfseries ", round(x[i], digits = digits), digits = digits,"}")
      }
    }
  }
  
  return(resultString)
}

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


# DESCRIPTIVE STATISTICS
x = descriptiveStatisticsResults
digits = 5
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLine(descriptiveStatisticsResults.average,digits,(ncol(x))))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 1
add.to.row$pos[[2]] = nrow(descriptiveStatisticsResults)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/descriptiveStatisticsResults.txt",sep="")
print(xtable(descriptiveStatisticsResults, auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Descriptive statistics for OBX stocks"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row, tabular.environment = "longtable",file = URL)

# PHILLIP PERRON TEST
x = phillipPerronResults
digits = 3
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLine(phillipPerronResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 1
add.to.row$pos[[2]] = nrow(phillipPerronResults)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/ppResults.txt",sep="")
print(xtable(phillipPerronResults, auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "Phillip Perron test for OBX stocks"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# DISTRIBUTION FIT
x = distributionsFitResults[-c(ncol(distributionsFitResults))]
digits = 1
alignAndDigitsVectors = createDigitsandAlignVectors(x,digits)
# GENERAL LONG-TABLE COMMAND
command <- c(paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLineDistribution(distributionsFitResults.average,digits,ncol(x)))

add.to.row <- list(pos = list(0,0), command = command)
add.to.row$pos[[1]] = 1
add.to.row$pos[[2]] = nrow(x)

add.to.row$command <- command

URL=paste(URL.drop,"/Tables/distributionFitResults.txt", sep="")
names(x)=c("Stock", "NORM","GED","STD","SNORM","SGED","SSTD","GHYP","NIG","GHST","Best Fit")
print(xtable(x, auto=TRUE, display = c('d','s','f','f','f','f','f','f','f','f','f','s'),digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = "AIC results for various distributions for OBX stocks"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
