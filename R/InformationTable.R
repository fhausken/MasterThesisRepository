
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

URL.repo=getwd()

if (grepl("Fredrik", URL.repo)){
  URL.drop="C:/Users/Fredrik Hausken/Dropbox/Apper/ShareLaTeX/Master thesis"
}else if (grepl("andersronold", URL.repo)){
  URL.drop="/Users/andersronold/Dropbox/Apper/ShareLaTeX/Master\ thesis" #Anders ma fylle inn
}else{
  URL.drop="Does not find"
}

#INPUT

#tradingBound=0.2 #Number of times the standard deviation

# GENERAL
URL=paste(URL.repo,"/Data/sampleSizes.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/rollingWindowSize.Rda",sep="")
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

URL=paste(URL.repo,"/Data/sampleShortLongNumberOfTransactionsDataFramesList.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleShortLongtransactionCostvariable.Rda",sep="")
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

URL=paste(URL.repo,"/Data/sampleBoundNumberOfTransactionsDataFramesList.Rda",sep="")
load(URL)

URL=paste(URL.repo,"/Data/sampleBoundtransactionCostvariable.Rda",sep="")
load(URL)


TRANSACTION.COST.SHORTLONG = FALSE  
TRANSACTION.COST.BOUND = FALSE  
# SET TRANSACTION COST BOOLEAN
if (sampleBoundtransactionCostvariable > 0) {
  TRANSACTION.COST.BOUND = TRUE  
}

if (sampleShortLongtransactionCostvariable > 0) {
  TRANSACTION.COST.SHORTLONG = TRUE  
}




# SHORT LONG: CREATE INFORMATION METRIC TABLE (Stock, mean_buy-and-hold, std.dev_buy-and-hold, r_buy-and-hold, Sign Ratio, mean_short-long, std.dev_short-long, return_short-long, alpha, SR_buy-and-hold, SR_short-long)
informationShortLongDataFrameList = list()
TRADING.DAYS.PER.YEAR = 250
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
  nTrans.ls = unlist(sampleShortLongNumberOfTransactionsDataFramesList[[sampleSizesIndex]])
  sr.bh = sqrt(TRADING.DAYS.PER.YEAR) * mean.bh/stDev.bh
  sr.ls = sqrt(TRADING.DAYS.PER.YEAR) * mean.ls/stDev.ls
  
  # ANNUALIZE RETURNS
  
  # CREATE DATAFRAME
  informationShortLongDataFrame = data.frame(stock.names, mean.bh, stDev.bh, return.bh, hitratio, mean.ls, stDev.ls, return.ls, alpha, nTrans.ls, sr.bh, sr.ls)
  
  # RESET ROWNAMES
  rownames(informationShortLongDataFrame) <- NULL 
  
  # SET COLNAMES
  colnames(informationShortLongDataFrame) = c("Stock","BH mean", "Buy-and-hold std.dev","Buy-and-hold return", "Hit ratio","Short-long mean", "Short-long std.dev", "Short-long return", "Alpha", "Trades","Buy-and-hold SR", "Short-long SR")
  
  # CALCULATE AVERAGE OF COLUMNS
  average.mean.bh = mean(informationShortLongDataFrame$`BH mean`)
  average.stDev.bh = mean(informationShortLongDataFrame$`Buy-and-hold std.dev`)
  average.return.bh = mean(informationShortLongDataFrame$`Buy-and-hold return`)
  
  average.hitratio.ls = mean(informationShortLongDataFrame$`Hit ratio`)
  average.mean.ls = mean(informationShortLongDataFrame$`Short-long mean`)
  average.stDev.ls = mean(informationShortLongDataFrame$`Short-long std.dev`)
  average.return.ls = mean(informationShortLongDataFrame$`Short-long return`)
  average.alpha.ls = mean(informationShortLongDataFrame$Alpha)
  average.nTrans.ls = round(mean(informationShortLongDataFrame$Trades),digits = 0)
  
  average.sr.bh = mean(informationShortLongDataFrame$`Buy-and-hold SR`)
  average.sr.ls = mean(informationShortLongDataFrame$`Short-long SR`)
  
  end.line.ls.infoTable = c(average.mean.bh, average.stDev.bh, average.return.bh, average.hitratio.ls, average.mean.ls, average.stDev.ls, average.return.ls, average.alpha.ls, average.nTrans.ls, average.sr.bh, average.sr.ls)
  
  # SET LATEX COL NAMES
  colnames(informationShortLongDataFrame) = c("Stock","$\\boldsymbol{\\mu_{BH}}$", "$\\boldsymbol{\\sigma_{BH}}$","$\\boldsymbol{r_{BH}}$", "Hit Ratio","$\\boldsymbol{\\mu_{SL}}$", "$\\boldsymbol{\\sigma_{SL}}$", "$\\boldsymbol{r_{SL}}$", "$\\boldsymbol{\\alpha}$", "Trades","$\\boldsymbol{SR_{BH}}$", "$\\boldsymbol{SR_{SL}}$")
  
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
  nTrans.b = unlist(sampleBoundNumberOfTransactionsDataFramesList[[sampleSizesIndex]])
  sr.bh = sqrt(TRADING.DAYS.PER.YEAR) * mean.bh/stDev.bh
  sr.b = sqrt(TRADING.DAYS.PER.YEAR) * mean.b/stDev.b
  
  
  # CREATE DATAFRAME
  informationBoundDataFrame = data.frame(stock.names, mean.bh, stDev.bh, return.bh, hitratio.b, mean.b, stDev.b, return.b, alpha.b, nTrans.b, sr.bh, sr.b)
  
  # RESET ROWNAMES
  rownames(informationBoundDataFrame) <- NULL 
  
  # SET COLNAMES
  colnames(informationBoundDataFrame) = c("Stock","BH mean", "Buy-and-hold std.dev","Buy-and-hold return", "Hit ratio","Bound mean", "Bound std.dev", "Bound return", "Alpha", "Trades", "Buy-and-hold SR", "Bound SR")
  
  # CALCULATE AVERAGE OF COLUMNS
  average.mean.bh = mean(informationBoundDataFrame$`BH mean`)
  average.stDev.bh = mean(informationBoundDataFrame$`Buy-and-hold std.dev`)
  average.return.bh = mean(informationBoundDataFrame$`Buy-and-hold return`)
  
  average.hitratio.b = mean(informationBoundDataFrame$`Hit ratio`)
  average.mean.b = mean(informationBoundDataFrame$`Bound mean`)
  average.stDev.b = mean(informationBoundDataFrame$`Bound std.dev`)
  average.return.b = mean(informationBoundDataFrame$`Bound return`)
  average.alpha.b = mean(informationBoundDataFrame$Alpha)
  average.nTrans.b = round(mean(informationBoundDataFrame$Trades), digits = 0)
  
  average.sr.bh = mean(informationBoundDataFrame$`Buy-and-hold SR`)
  average.sr.b = mean(informationBoundDataFrame$`Bound SR`)
  
  end.line.bound.infoTable = c(average.mean.bh, average.stDev.bh, average.return.bh, average.hitratio.b, average.mean.b, average.stDev.b, average.return.b, average.alpha.b, average.nTrans.b, average.sr.bh, average.sr.b)
  
  # SET LATEX COL NAMES
  colnames(informationBoundDataFrame) = c("Stock","$\\boldsymbol{\\mu_{BH}}$", "$\\boldsymbol{\\sigma_{BH}}$","$\\boldsymbol{r_{BH}}$", "Hit Ratio","$\\boldsymbol{\\mu_{Bound}}$", "$\\boldsymbol{\\sigma_{Bound}}$", "$\\boldsymbol{r_{Bound}}$", "$\\boldsymbol{\\alpha}$", "Trades","$\\boldsymbol{SR_{BH}}$", "$\\boldsymbol{SR_{Bound}}$")
  
  informationBoundDataFrameList[[length(informationBoundDataFrameList)+1]] = informationBoundDataFrame
  
}

names(informationBoundDataFrameList) = sampleSizes

# TABLES-TO-LATEX
bold <- function(x){
  paste0('{\\bfseries ', x, '}')
}

createAverageLine <- function(x, digits) {
  resultString = ""
  for(i in 1:length(x)) {
    if (i == 1) {
      resultString = paste0(resultString," \\hline & ","{ \\bfseries ", "Average ","} &", "{ \\bfseries ",round(x[i], digits = digits),"}")
    }
    else {
      resultString = paste0(resultString, " & ","{ \\bfseries ", round(x[i], digits = digits),"}")
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
    if(i == 9) {
      # IN ORDER TO SET 0 DIGITS FOR TRANSACTIONS
      digitsVector = c(digitsVector,0)
    }
    else {
      digitsVector = c(digitsVector,digits)
    }
  }
  
  return(list(alignVector,digitsVector))
}

# SHORT-LONG: RETURN, VARIANCE, SIGN RATIO AND ALPHA METRICS FOR ALL STOCKS
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize=sampleSizes[sampleSizesIndex]
  x = informationShortLongDataFrameList[[sampleSizesIndex]]
  digits = 4
  alignAndDigitsVectors = createDigitsandAlignVectors(x,digits)

  # GENERAL LONG-TABLE COMMAND
  command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLine(end.line.ls.infoTable, digits))

  add.to.row <- list(pos = list(0,0), command = command)
  add.to.row$pos[[1]] = 0
  add.to.row$pos[[2]] = nrow(informationShortLongDataFrameList[[sampleSizesIndex]])

  add.to.row$command <- command

  if(TRANSACTION.COST.SHORTLONG == TRUE) {
    URL=paste(URL.drop,"/Tables/informationTable_","shortLong","sampleSize",sampleSizesIndex,".txt",sep="")  
  }
  else {
    URL=paste(URL.drop,"/Tables/informationTable_","TshortLong","sampleSize",sampleSizesIndex,".txt",sep="")  
  }
  
  print(xtable(informationShortLongDataFrameList[[sampleSizesIndex]], auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = paste("Stock Metrics Short Long Trading Strategy and Sample Size",sampleSize)), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row, tabular.environment = "longtable",file = URL)
}

# BOUND: RETURN, VARIANCE, SIGN RATIO AND ALPHA METRICS FOR ALL STOCKS
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize=sampleSizes[sampleSizesIndex]
  x = informationBoundDataFrameList[[sampleSizesIndex]]
  digits = 4
  alignAndDigitsVectors = createDigitsandAlignVectors(x,digits)

  # GENERAL LONG-TABLE COMMAND
  command <- c(paste0(" \\hline ","\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n"),createAverageLine(end.line.bound.infoTable, digits))

  add.to.row <- list(pos = list(0,0), command = command)
  add.to.row$pos[[1]] = 0
  add.to.row$pos[[2]] = nrow(informationBoundDataFrameList[[sampleSizesIndex]])

  add.to.row$command <- command

  if(TRANSACTION.COST.BOUND == TRUE) {
    URL=paste(URL.drop,"/Tables/informationTable_","tradingBound",tradingBound,"sampleSize",sampleSizesIndex,".txt",sep="")
    print(xtable(informationBoundDataFrameList[[sampleSizesIndex]], auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = paste("Stock Metrics with Trading Bound ",tradingBound,"and Sample Size",sampleSize)), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row, tabular.environment = "longtable",file = URL)
  }
  else {
    URL=paste(URL.drop,"/Tables/informationTable_","TtradingBound",tradingBound,"sampleSize",sampleSizesIndex,".txt",sep="")
    print(xtable(informationBoundDataFrameList[[sampleSizesIndex]], auto=FALSE, digits=alignAndDigitsVectors[[2]], align = alignAndDigitsVectors[[1]], type = "latex", caption = paste("Stock Metrics with Transaction costs with Trading Bound ",tradingBound,"and Sample Size",sampleSize)), sanitize.text.function = function(x) {x}, sanitize.colnames.function = bold, hline.after=c(-1,0), add.to.row = add.to.row, tabular.environment = "longtable",file = URL)
  }
  
  

}


