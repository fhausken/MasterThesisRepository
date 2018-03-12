
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
library(xtable)

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

tradingBound=0.2 #Number of times the standard deviation

# GENERAL
URL=paste(URL.repo,"/Data/sampleSizes.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

# BUY-HOLD
URL=paste(URL.repo,"/Data/meanBuyAndHold.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/standardDevBuyAndHold.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleBuyAndHoldTotalReturnDataFramesList.Rda",sep="")
load(URL)


# SHORT LONG
URL=paste(URL.repo,"/Data/tradingBound.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleHitRatioDataFramesList.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/meanLongShort.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/standardDevShortLong.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleShortLongTotalReturnDataFramesList.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleAlphaDataFramesList.Rda",sep="")
load(URL)

# BOUND TRADING
URL=paste(URL.repo,"/Data/sampleBoundHitRatioDataFramesList.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/meanBound.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/standardDevBound.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleBoundTotalReturnDataFramesList.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleBoundAlphaDataFramesList.Rda",sep="")
load(URL)


# SHORT LONG: CREATE INFORMATION METRIC TABLE (Stock, mean_buy-and-hold, std.dev_buy-and-hold, r_buy-and-hold, Sign Ratio, mean_short-long, std.dev_short-long, return_short-long, alpha, SR_buy-and-hold, SR_short-long)
informationShortLongDataFrameList = list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  stock.names = stocks[[1]]
  mean.bh = unlist(meanBuyAndHold[[sampleSizesIndex]])
  stDev.bh = unlist(standardDevBuyAndHold[[sampleSizesIndex]])
  return.bh = unlist(sampleBuyAndHoldTotalReturnDataFramesList[[sampleSizesIndex]])
  hitratio = unlist(sampleHitRatioDataFramesList[[sampleSizesIndex]])
  mean.ls = unlist(meanLongShort[[sampleSizesIndex]])
  stDev.ls = unlist(standardDevShortLong[[sampleSizesIndex]])
  return.ls = unlist(sampleShortLongTotalReturnDataFramesList[[sampleSizesIndex]]) #Use short-long returns to calculate total accumulated return
  alpha = unlist(sampleAlphaDataFramesList[[sampleSizesIndex]])
  sr.bh = return.bh/stDev.bh
  sr.ls = return.ls/stDev.ls
  
  # CREATE DATAFRAME
  informationShortLongDataFrame = data.frame(stock.names, mean.bh, stDev.bh, return.bh, hitratio, mean.ls, stDev.ls, return.ls, alpha, sr.bh, sr.ls)
  
  # RESET ROWNAMES
  rownames(informationShortLongDataFrame) <- NULL 
  
  # SET COLNAMES
  colnames(informationShortLongDataFrame) = c("Stock","Buy-and-hold mean", "Buy-and-hold std.dev","Buy-and-hold return", "Hit ratio","Short-long mean", "Short-long std.dev", "Short-long return", "Alpha", "Buy-and-hold SR", "Long-short SR")
  
  informationShortLongDataFrameList[[length(informationShortLongDataFrameList)+1]] = informationShortLongDataFrame
  
}

names(informationShortLongDataFrameList) = sampleSizes


# BOUND: CREATE INFORMATION METRIC TABLE (Stock, mean_buy-and-hold, std.dev_buy-and-hold, r_buy-and-hold, Sign Ratio, mean_short-long, std.dev_short-long, return_short-long, alpha, SR_buy-and-hold, SR_short-long)
informationBoundDataFrameList = list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  stock.names = stocks[[1]]
  mean.bh = unlist(meanBuyAndHold[[sampleSizesIndex]])
  stDev.bh = unlist(standardDevBuyAndHold[[sampleSizesIndex]])
  return.bh = unlist(sampleBuyAndHoldTotalReturnDataFramesList[[sampleSizesIndex]])
  hitratio.b = unlist(sampleBoundHitRatioDataFramesList[[sampleSizesIndex]])
  mean.b = unlist(meanBound[[sampleSizesIndex]])
  stDev.b = unlist(standardDevBound[[sampleSizesIndex]])
  return.b = unlist(sampleBoundTotalReturnDataFramesList[[sampleSizesIndex]]) #Use short-long returns to calculate total accumulated return
  alpha.b = unlist(sampleBoundAlphaDataFramesList[[sampleSizesIndex]])
  sr.bh = return.bh/stDev.bh
  sr.b = return.b/stDev.b
  
  # CREATE DATAFRAME
  informationBoundDataFrame = data.frame(stock.names, mean.bh, stDev.bh, return.bh, hitratio.b, mean.b, stDev.b, return.b, alpha.b, sr.bh, sr.b)
  
  # RESET ROWNAMES
  rownames(informationBoundDataFrame) <- NULL 
  
  # SET COLNAMES
  colnames(informationBoundDataFrame) = c("Stock","$\sigma_{BH}$", "Buy-and-hold std.dev","Buy-and-hold return", "Hit ratio","Bound mean", "Bound std.dev", "Bound return", "Alpha", "Buy-and-hold SR", "Bound SR")
  
  informationBoundDataFrameList[[length(informationBoundDataFrameList)+1]] = informationBoundDataFrame
  
}

names(informationBoundDataFrameList) = sampleSizes

# TABLES-TO-LATEX
bold <- function(x){
  paste0('{\\bfseries ', x, '}')
}

# SHORT-LONG: RETURN, VARIANCE, SIGN RATIO AND ALPHA METRICS FOR ALL STOCKS
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize=sampleSizes[sampleSizesIndex]
  x = informationShortLongDataFrameList[[sampleSizesIndex]]
  
  # GENERAL LONG-TABLE COMMAND
  add.to.row <- list(pos = list(0), command = NULL)
  command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
  add.to.row$command <- command
  
  URL=paste(URL.drop,"/Tables/informationTable_",sampleSize,".txt",sep="")
  print(xtable(informationShortLongDataFrameList[[sampleSizesIndex]], auto=FALSE, digits=c(1,1,3,3,3,3,3,3,3,3,3,3), align = c('l','c','c','c','c','c','c','c','c','c','c','c'), type = "latex", caption = paste("Stock metrics short long trading strategy and sample size",sampleSize)), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
}

# BOUND: RETURN, VARIANCE, SIGN RATIO AND ALPHA METRICS FOR ALL STOCKS
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize=sampleSizes[sampleSizesIndex]
  x = informationBoundDataFrameList[[sampleSizesIndex]]
  
  # GENERAL LONG-TABLE COMMAND
  add.to.row <- list(pos = list(0), command = NULL)
  command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
  add.to.row$command <- command
  
  URL=paste(URL.drop,"/Tables/informationTable_",sampleSize,"tradingBound",tradingBound,".txt",sep="")
  print(xtable(informationBoundDataFrameList[[sampleSizesIndex]], auto=FALSE, digits=c(1,1,3,3,3,3,3,3,3,3,3,3), align = c('l','c','c','c','c','c','c','c','c','c','c','c'), type = "latex", caption = paste("Stock metrics with trading bound ",tradingBound,"and sample size",sampleSize)), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row, tabular.environment = "longtable",file = URL)
        
}


