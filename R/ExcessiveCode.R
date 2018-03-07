#Short/Sell Long Sign

sampleSignDataFramesList=list()
for (sampleSizesIndex in 1:length(sampleSizes)){
  sampleSize = sampleSizes[sampleSizesIndex]
  rollingWindowSize = nrow(stockReturns) - sampleSize
  shortLongVector=vector()
  
  for (stocksIndex in 1:nrow(stocks)){
    stockName=stocks[stocksIndex,1]
    signVector=vector()
    for (day in 1:(rollingWindowSize+1)){
      forecast=sampleForecastsDataFramesList[[sampleSizesIndex]][day,stocksIndex]
      signOfForecast=sign(forecast)
      
      if (signOfForecast==1){
        signVector[length(signVector)+1]=1 #Long
      }else{
        signVector[length(signVector)+1]=0 #Short or Sell
      }
      
    }
    if (stocksIndex==1){
      stockSignDataFrame=data.frame(signVector)
    }else{
      stockSignDataFrame=cbind(stockSignDataFrame,signVector)
    }
  }
  
  names(stockSignDataFrame)=stocks[[1]]
  row.names(stockSignDataFrame)=index(stockReturns)[sampleSize:nrow(stockReturns)]
  sampleSignDataFramesList[[length(sampleSignDataFramesList)+1]]=stockSignDataFrame
  
}
names(sampleSignDataFramesList)=sampleSizes
