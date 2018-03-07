
# CREATE INFORMATION METRIC TABLE (Stock, mean_buy-and-hold, std.dev_buy-and-hold, r_buy-and-hold, Sign Ratio, mean_short-long, std.dev_short-long, return_short-long, alpha, SR_buy-and-hold, SR_short-long)
informationDataFrameList = list()

for (sampleSizesIndex in 1:length(sampleSizes)){
  
  stock.names = stocks[[1]]
  mean.bh = unlist(meanBuyAndHold[[sampleSizesIndex]])
  stDev.bh = unlist(standardDevBuyAndHold[[sampleSizesIndex]])
  return.bh = unlist(sampleBuyAndHoldTotalReturnDataFramesList[[sampleSizesIndex]])
  hitratio = unlist(sampleHitRatioDataFramesList[[sampleSizesIndex]])
  mean.ls = unlist(meanLongShort[[sampleSizesIndex]])
  stDev.ls = unlist(standardDevShortLong[[sampleSizesIndex]])
  return.ls = unlist(getTotalReturn(sampleShortLongReturnDataFramesList)[[sampleSizesIndex]]) #Use short-long returns to calculate total accumulated return
  alpha = unlist(getTotalReturn(sampleAccumulatedAlphaReturnDataFramesList)[[sampleSizesIndex]])
  sr.bh = return.bh/stDev.bh
  sr.ls = return.ls/stDev.ls
  
  # CREATE DATAFRAME
  informationDataFrame = data.frame(stock.names, mean.bh, stDev.bh, return.bh, hitratio, mean.ls, stDev.ls, return.ls, alpha, sr.bh, sr.ls)
  
  # RESET ROWNAMES
  rownames(informationDataFrame) <- NULL 
  
  # SET COLNAMES
  colnames(informationDataFrame) = c("Stock","Buy-and-hold mean", "Buy-and-hold std.dev","Buy-and-hold return", "Hit ratio","Short-long mean", "Short-long std.dev", "Short-long return", "Alpha", "Buy-and-hold SR", "Long-short SR")
  
  informationDataFrameList[[length(informationDataFrameList)+1]] = informationDataFrame
  
}

names(informationDataFrameList) = sampleSizes

# TABLES-TO-LATEX

# RETURN, VARIANCE, SIGN RATIO AND ALPHA METRICS FOR ALL STOCKS
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize=sampleSizes[sampleSizesIndex]
  x = informationDataFrameList[[sampleSizesIndex]]
  
  # GENERAL LONG-TABLE COMMAND
  add.to.row <- list(pos = list(0), command = NULL)
  command <- paste0("\\endhead\n","\n","\\multicolumn{", dim(x)[2] + 1, "}{l}","{\\footnotesize Continued on next page}\n","\\endfoot\n","\\endlastfoot\n")
  add.to.row$command <- command
  
  URL=paste(URL.drop,"/Tables/informationTable_",sampleSize,".txt",sep="")
  print(xtable(informationDataFrameList[[sampleSizesIndex]], auto=FALSE, digits=c(1,1,3,3,3,3,3,3,3,3,3,3), align = c('l','c','c','c','c','c','c','c','c','c','c','c'), type = "latex", caption = "Stock metrics"), hline.after=c(-1,0), add.to.row = add.to.row,tabular.environment = "longtable",file = URL)
  
  
}
