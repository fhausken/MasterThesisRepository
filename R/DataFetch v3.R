library(readxl)
library(quantmod)
library(xts)

rm(list=ls()) #Clears environment

URL.repo=getwd()
URL=paste(URL.repo,"/Data/Stocks.xlsx",sep="")
stocks <- read_excel(URL,sheet = "Sheet1")


stocks=stocks[c(114,125,128),] #For testing. Et utvalg av aksjer.
#stocks=stocks[c(10,114),]

# SET FROM DATE
from.date <- as.Date("01/02/13", format="%m/%d/%y")

consecutiveZerosCapClose=10

# VOLUME CONSTRAINTS
XCapVolume=0
consecutiveXCapVolume=10


# INIT LISTS AND LENGTH OF DESIRED OSEAX STOCKS
stocks.nrow=nrow(stocks)
  
stocksRemoved.index=vector()
stocksRemoved.reason=vector()

stockData=new.env()

for (row in 1:stocks.nrow) {
  
  tryCatch({
    
    fetchName=paste(stocks$Ticker[row],".OL",sep="")
    stock.data=getSymbols(fetchName,from=from.date,auto.assign = FALSE)
    
    if(index(stock.data)[1]==(from.date)){
      
      vectorizedClose=drop(coredata(stock.data[,4]))
      vectorizedClose[is.na(vectorizedClose)] <- 1000000000
      occurences = rle(vectorizedClose)
      consecutiveZerosVector.close=occurences$lengths[occurences$values == 1000000000]
      if(length(consecutiveZerosVector.close>0)){
        
        maxOfconsecutiveZerosVectorClose=max(consecutiveZerosVector.close)
        #print(fetchName)
        #print(maxOfconsecutiveZerosVectorClose)
        #print("")
      }
      else{
        maxOfconsecutiveZerosVectorClose=0
      }
      
    
      vectorizedVolume=drop(coredata(stock.data[,5]))
      vectorizedVolume[is.na(vectorizedVolume)] <- 10000000000000 #Skummelt ? sette denne til null. Da det ikke er sikkert at volumet er 0, og NA i volum ikke p?virker utregninger som GARCH
      occurences = rle(vectorizedVolume)
      consecutiveZerosVector.volume=occurences$lengths[occurences$values <= XCapVolume]
      if(length(consecutiveZerosVector.volume>0)){
        
        maxOfconsecutiveZerosVectorVolume=max(consecutiveZerosVector.volume)
        #print(fetchName)
        #print(maxOfconsecutiveZerosVectorVolume)
        #print("")
      }
      else{
        maxOfconsecutiveZerosVectorVolume=0
      }
      
      
      if(maxOfconsecutiveZerosVectorClose<=consecutiveZerosCapClose){
        if(maxOfconsecutiveZerosVectorVolume<=consecutiveXCapVolume){
          getSymbols(fetchName,from=from.date,auto.assign =TRUE,env=stockData)
        }
        else{
          stocksRemoved.index[length(stocksRemoved.index)+1]=row
          stocksRemoved.reason[length(stocksRemoved.reason)+1]="Illiquid"
        }
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
    #message(e)
    stocksRemoved.index[length(stocksRemoved.index)+1]<<-row #Merk dobbel pil som betyr at man aksesserer en globalvariabel
    stocksRemoved.reason[length(stocksRemoved.reason)+1]<<-"Not Found"
    })

}

stocksRemoved=NULL #Hvis if-testen p? neste linje er false, m? objektet ha verdi for at det ikke skal bli feil. Dette fordi objektet lagres tilslutt
if (length(stocksRemoved.index)>0){
  
  stocksRemoved=stocks[stocksRemoved.index,]
  stocksRemoved$Reason=stocksRemoved.reason
  stocks=stocks[-stocksRemoved.index,]
}
  

#Prices and Returns

# RETRIEVE CLOSING PRICES AND MERGE CLOSE FOR ALL STOCKS INTO DATAFRAME
stockPricesList <- eapply(stockData, Cl)
stockPrices <- do.call(merge, stockPricesList)

#raderFor=nrow(stockPrices)
#stockPrices=na.omit(stockPrices) #Tar bort rader med na
#raderEtter=nrow(stockPrices)

stockReturns=diff(log(stockPrices))
stockReturns=stockReturns[-c(1),] #Fjerner f?rste rad siden den er NA

stockPrices[is.na(stockPrices)] <- 0 # Setter NA elementer til 0.
stockReturns[is.na(stockReturns)] <- 0 # Setter NA elementer til 0.

names(stockPrices)=stocks$Stock #Endrer navn p? kolonner
names(stockReturns)=stocks$Stock

#Volume

stockVolumesList <- eapply(stockData, Vo)
stockVolumes <- do.call(merge, stockVolumesList)

#raderFor=nrow(stockPrices)
#stockPrices=na.omit(stockPrices) #Tar bort rader med na
#raderEtter=nrow(stockPrices)

#stockVolumes[is.na(stockVolumes)] <- 0 # Setter NA elementer til 0.
names(stockVolumes)=stocks$Ticker #Endrer navn p? kolonner

#Lagring
URL=paste(URL.repo,"/Data/stockPrices.Rda",sep="")
save(stockPrices,file=URL)
URL=paste(URL.repo,"/Data/stockReturns.Rda",sep="")
save(stockReturns,file=URL)
URL=paste(URL.repo,"/Data/stockVolumes.Rda",sep="")
save(stockVolumes,file=URL)
URL=paste(URL.repo,"/Data/stocks.Rda",sep="")
save(stocks,file=URL)
URL=paste(URL.repo,"/Data/stocksRemoved.Rda",sep="")
save(stocksRemoved,file=URL)
