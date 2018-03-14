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
names(descriptiveStatisticsResults)=c("Stocks","Min","1st Quantile", "Median", "Mean", "Standard Deviation", "Variance", "3rd Quantile", "Max", "Skew", "Kurtosis" )

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

URL=paste(URL.repo,"/Data/phillipPerronResults.Rda",sep="")
save(phillipPerronResults,file=URL)

#DISTRIBUTION FITTING
distributions=c("norm","ged","std","snorm","sged","sstd","ghyp","nig","ghst")
URL=paste(URL.repo,"/Data/distributions.Rda",sep="")
save(distributions,file=URL)

distributions.fullname=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution")
URL=paste(URL.repo,"/Data/distributionsFullname.Rda",sep="")
save(distributions.fullname,file=URL)

results=list()
for (stock in 1:numberOfStocks){
  minAIC=100000000 #tilsvarer + infinity
  bestDistributionFit=""
  resultsForStock=list()
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
    
    results[length(results)+1]=AIC
  }
  results[length(results)+1]=bestDistributionFit.fullname
  results[length(results)+1]=bestDistributionFit
}

distributionsFitResults=data.frame(stocks[,1],matrix(results, ncol=length(distributions)+2, byrow=TRUE),stringsAsFactors=FALSE) #Dette er ikke en dataframe. Da m? man legge til unlist(results) rundt results.

names(distributionsFitResults)=c("Stock","Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution", "Best Fit Fullname", "Best Fit Shortname")


URL=paste(URL.repo,"/Data/distributionFitResults.Rda",sep="")
save(distributionsFitResults,file=URL)


# TABLES-TO-LATEX
bold <- function(x){
  paste0('{\\bfseries ', x, '}')
}
# DESCRIPTIVE STATISTICS
x = descriptiveStatisticsResults
# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/descriptiveStatisticsResults.txt",sep="")
print(xtable(descriptiveStatisticsResults, auto=FALSE, digits=c(1,1,4,4,5,5,4,5,4,4,4,4), align = c('l','l','c','c','c','c','c','c','c','c','c','c'), type = "latex", caption = "Descriptive statistics for OSEAX stocks"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# PHILLIP PERRON TEST
x = phillipPerronResults
# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/ppResults.txt",sep="")
print(xtable(phillipPerronResults, auto=FALSE, digits=c(1,1,3,3,1), align = c('c','l','c','c','c'), type = "latex", caption = "Phillip Perron test for OSEAX stocks"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# DISTRIBUTION FIT
x = distributionsFitResults[-c(ncol(distributionsFitResults))]

# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/distributionFitResults.txt", sep="")
names(x)=c("Stock", "NORM","GED","STD","SNORM","SGED","SSTD","GHYP","NIG","GHST","Best Fit")
print(xtable(x, auto=TRUE, display = c('d','s','f','f','f','f','f','f','f','f','f','s'),digits=c(1,1,1,1,1,1,1,1,1,1,1,1), align = c('l','l','c','c','c','c','c','c','c','c','c','c'), type = "latex", caption = "AIC results for various distributions for OSEAX stocks"), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
