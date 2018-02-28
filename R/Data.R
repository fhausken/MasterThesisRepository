#install.packages("psych")

library(tseries)
library(rugarch)
library(psych)
library(quantmod)
library(xtable)


options(xtable.floating = FALSE)
options(xtable.timestamp = "")


rm(list=ls()) #Clears environment

URL.repo=getwd()

if (grepl("Fredrik", URL.repo)){
  URL.drop="C:/Users/Fredrik Hausken/Dropbox/Apper/ShareLaTeX/Master thesis"
}else if (grepl("Anders", URL.repo)){
  URL.drop="C:/Users/Fredrik Hausken/Dropbox/Apper/ShareLaTeX/Master thesis" #Anders må fylle inn
}else{
  URL.drop="Does not find"
}

URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocksRemoved.Rda",sep="")
load(URL)

numberOfStocks=nrow(stocks)

#Descriptive Statistics

results=vector()

for (stock in 1:numberOfStocks){
  vectorizedReturn=drop(coredata(stockReturns[,stock]))
  descriptiveStatistics=summary(vectorizedReturn)
  results[length(results)+1]=min(vectorizedReturn)
  results[length(results)+1]=descriptiveStatistics[2]
  results[length(results)+1]=median(vectorizedReturn)
  results[length(results)+1]=mean(vectorizedReturn)
  results[length(results)+1]=sd(vectorizedReturn) #Bruker sample (n-1)
  results[length(results)+1]=var(vectorizedReturn) #Bruker sample (n-1)
  results[length(results)+1]=descriptiveStatistics[5]
  results[length(results)+1]=max(vectorizedReturn)
  results[length(results)+1]=skew(vectorizedReturn) #Bruker sample (n-1)
  results[length(results)+1]=kurtosi(vectorizedReturn) #Bruker sample (n-1)
  
  
}

descriptiveStatisticsResults=data.frame(stocks[,1],matrix(results, ncol=10, byrow=TRUE))
names(descriptiveStatisticsResults)=c("Stocks","Min","1st Quantile", "Median", "Mean", "Standard Deviation", "Variance", "3rd Quantile", "Max", "Skew", "Kurtosi" )

URL=paste(URL.repo,"/Data/descriptiveStatisticsResults.Rda",sep="")
save(descriptiveStatisticsResults,file=URL)

#Augmented Dicky Fuller

testStatistic=vector()
pValue=vector()
testConclusion=vector()

for (stock in 1:numberOfStocks){
  dickyFuller=adf.test(stockReturns[,stock])
  
  testStatistic[length(testStatistic)+1]=dickyFuller$statistic
  pValue[length(pValue)+1]=dickyFuller$p.value
  
  if (pValue<=0.01){
    testConclusion[length(testConclusion)+1]="Stationary"
  }
  else{
    testConclusion[length(testConclusion)+1]="Not Stationary"
  }
  
  
}

dickyFullerResults=data.frame(stocks[,1],testStatistic, pValue, testConclusion)
names(dickyFullerResults)=c("Stock", "Test Statistic", "P-Value", "Test Conlcusion")

URL=paste(URL.repo,"/Data/dickyFullerResults.Rda",sep="")
save(dickyFullerResults,file=URL)

#Distribution Fitting

distributions=c("norm","ged","std","snorm","sged","sstd","ghyp","nig","ghst")
distributions.fullname=c("Normal Distribution","Generalized Error Distribution","Student Distribution","Skewed Normal Distribution","Skewed Generalized Error Distribution","Skewed Student Distribution","Generalized Hyperbolic Function Distribution","Normal Inverse Gaussian Distribution","Generalized Hyperbolic Skew Student Distribution")

results=list()
for (stock in 1:numberOfStocks){
  minAIC=100000000 #tilsvarer +infinity
  bestDistributionFit=""
  resultsForStock=list()
  for (distributions.index in 1:length(distributions)){
    
    vectorizedReturn=drop(coredata(stockReturns[,stock]))
    fit.distribution<- fitdist(distribution = distributions[distributions.index], vectorizedReturn, control = list())
    
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

distributionsFitResults=data.frame(stocks[,1],matrix(results, ncol=length(distributions)+2, byrow=TRUE),stringsAsFactors=FALSE)
names(distributionsFitResults)=c("Stock","AIC Normal Distribution","AIC Generalized Error Distribution","AIC Student Distribution","AIC Skewed Normal Distribution","AIC Skewed Generalized Error Distribution","AIC Skewed Student Distribution","AIC Generalized Hyperbolic Function Distribution","AIC Normal Inverse Gaussian Distribution","AIC Generalized Hyperbolic Skew Student Distribution", "Best Fit Fullname", "Best Fit Shortname")

URL=paste(URL.repo,"/Data/distributionFitResults.Rda",sep="")
save(distributionsFitResults,file=URL)

# LATEX

x=descriptiveStatisticsResults

add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n","\\hline\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

URL=paste(URL.drop,"/Tables/descriptiveStatisticsResults.txt",sep="")
print(xtable(x, auto=TRUE, type = "latex", caption = "Descriptive Statitics"), hline.after=c(-1), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)

test=print(xtable(x, auto=TRUE, type = "latex", caption = "Descriptive Statitics"),math.style.exponents=TRUE)

