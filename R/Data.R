#install.packages("psych")

library(tseries)
library(rugarch)
library(psych)
library(quantmod)
library(xtable)
library(het.test)
library(lmtest)


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
  
  if (pValue<=0.01){
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

# WHITE-TEST
lmValue.wt = c()
testConclusion.wt = c()

for (stock in 1:numberOfStocks){
  
  # BRUKER 2 LAGS I REGRESJONEN
  y = stockReturns[3:nrow(stockReturns),stock]
  x1 = stockReturns[2:nrow(stockReturns),stock]
  x2 = stockReturns[1:nrow(stockReturns),stock]
  
  x1 = x1[-c(nrow(x1)),]
  x2 = x2[-c(nrow(x2)),]
  x2 = x2[-c(nrow(x2)),]
  
  # KJORER REGRESJONEN - RETRIEVES RESIDUALS
  reg.model <- lm(y ~ x1 + x2)
  
  # RUN THE WHITE TEST - RESIDUALS ON LHS AND LOG RETURNS ON RHS
  residuals.reg = reg.model$residuals
  
  reg.model2 <- lm(residuals.reg^2 ~ x1 + x2 + x1^2 + x2^2 + x1*x2)
  LM.statistic = nrow(stockReturns)*summary(reg.model2)$r.squared
  
  # LM.statistic is chi-squared distributed with 5 DF --> cutoff value: 11.07 for significance of 0.05
  lmValue.wt[length(lmValue.wt)+1] = LM.statistic
  
  if (LM.statistic >= 11.07){
    testConclusion.wt[length(testConclusion.wt)+1]="Heteroscedasticity"
  }
  else{
    testConclusion.wt[length(testConclusion.wt)+1]="Homoscedasticity"
  }
  
}

wtResults=data.frame(stocks[,1],lmValue.wt, testConclusion.wt)
names(wtResults)=c("Stock", "Lagrange Multiplier Value", "Test Conlcusion")

URL=paste(URL.repo,"/Data/wtResults.Rda",sep="")
save(wtResults,file=URL)

#DISTRIBUTION FITTING
distributions=c("norm","ged","std","snorm","sged","sstd","ghyp","nig","ghst")
distributions.fullname=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution")

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

# DESCRIPTIVE STATISTICS
x = descriptiveStatisticsResults
# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/descriptiveStatisticsResults.txt",sep="")
print(xtable(descriptiveStatisticsResults, auto=FALSE, digits=c(1,1,4,4,5,5,4,5,4,4,4,4), align = c('l','l','c','c','c','c','c','c','c','c','c','c'), type = "latex", caption = "Descriptive statistics for OSEAX stocks"), hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# PHILLIP PERRON TEST
x = phillipPerronResults
# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/ppResults.txt",sep="")
print(xtable(phillipPerronResults, auto=FALSE, digits=c(1,1,3,3,1), align = c('c','l','c','c','c'), type = "latex", caption = "Phillip Perron test for OSEAX stocks"),hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)


# WHITE TEST
x = wtResults
# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/wtResults.txt",sep="")
print(xtable(wtResults, auto=FALSE, digits=c(1,1,2,1), align = c('c','l','c','c'), type = "latex", caption = "White test for OSEAX stocks"),hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

# DISTRIBUTION FIT
x = distributionsFitResults[-c(ncol(distributionsFitResults))]

# GENERAL LONG-TABLE COMMAND
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/distributionFitResults.txt", sep="")
names(x)=c("Stock", "NORM","GED","STD","SNORM","SGED","SSTD","GHYP","NIG","GHST","Best Fit")
print(xtable(x, auto=TRUE, display = c('d','s','f','f','f','f','f','f','f','f','f','s'),digits=c(1,1,1,1,1,1,1,1,1,1,1,1), align = c('l','l','c','c','c','c','c','c','c','c','c','c'), type = "latex", caption = "AIC results for various distributions for OSEAX stocks"), hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
