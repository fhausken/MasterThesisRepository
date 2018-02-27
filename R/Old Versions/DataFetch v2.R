library(readxl)
library(quantmod)
library(qmao)

wd=getwd()
URL=paste(wd,"/Data/Stocks.xlsx",sep="")
stocks <- read_excel(URL,sheet = "Sheet1")

from.date <- as.Date("01/04/10", format="%m/%d/%y") 
consecutiveZerosCap=5

stocks.nrow=nrow(stocks)
  
stocksRemoved.index=vector()
stocksRemoved.reason=vector()


stockData=new.env()

for (row in 1:stocks.nrow) {
  
  tryCatch({
    
    fetchName=paste(stocks$Ticker[row],".OL",sep="")
    stock.data=getSymbols(fetchName,from=from.date,auto.assign = FALSE)
    
    if(index(stock.data)[1]==(from.date)){
      
      vectorized=drop(coredata(stock.data[,4]))
      vectorized[is.na(vectorized)] <- 0
      occurences = rle(vectorized)
      consecutiveZerosVector=occurences$lengths[occurences$values == 0]
      if(length(consecutiveZerosVector>0)){
        
        maxOfconsecutiveZerosVector=max(consecutiveZerosVector)
        #print(fetchName)
        #print(maxOfconsecutiveZerosVector)
        #print("")
      }
      else{
        maxOfconsecutiveZerosVector=0
      }
      
      if(maxOfconsecutiveZerosVector<=consecutiveZerosCap){
        getSymbols(fetchName,from=from.date,auto.assign =TRUE,env=stockData)
      }
      else{
        stocksRemoved.index[length(stocksRemoved.index)+1]=row
        stocksRemoved.reason[length(stocksRemoved.reason)+1]="Data Missing/Not Continously Listed"
      }
    }
    else{
      stocksRemoved.index[length(stocksRemoved.index)+1]=row
      stocksRemoved.reason[length(stocksRemoved.reason)+1]="Listed Later"
    }

  },error = function(e) {
    stocksRemoved.index[length(stocksRemoved.index)+1]<<-row #Merk dobbel pil som betyr at man aksesserer en globalvariabel
    stocksRemoved.reason[length(stocksRemoved.reason)+1]<<-"Not Found"
    })

}
stocksRemoved=stocks[stocksRemoved.index,]
stocksRemoved$Reason=stocksRemoved.reason
stocks=stocks[-stocksRemoved.index,]


#Prices

stockPricesList <- eapply(stockData, Cl)
stockPrices <- do.call(merge, stockPricesList)

#raderFor=nrow(stockPrices)
#stockPrices=na.omit(stockPrices) #Tar bort rader med na
#raderEtter=nrow(stockPrices)

stockPrices[is.na(stockPrices)] <- 0 # Setter NA elementer til 0.
names(stockPrices)=stocks$Ticker #Endrer navn på kolonner

#Volume

stockVolumesList <- eapply(stockData, Vo)
stockVolumes <- do.call(merge, stockVolumesList)

#raderFor=nrow(stockPrices)
#stockPrices=na.omit(stockPrices) #Tar bort rader med na
#raderEtter=nrow(stockPrices)

#stockVolumes[is.na(stockVolumes)] <- 0 # Setter NA elementer til 0.
names(stockVolumes)=stocks$Ticker #Endrer navn på kolonner

#Lagring

URL=paste(wd,"/Data/stockPrices.Rda",sep="")
save(stockPrices,file=URL)
URL=paste(wd,"/Data/stockVolumes.Rda",sep="")
save(stockVolumes,file=URL)
URL=paste(wd,"/Data/stocks.Rda",sep="")
save(stocks,file=URL)
URL=paste(wd,"/Data/stocksRemoved.Rda",sep="")
save(stocksRemoved,file=URL)
