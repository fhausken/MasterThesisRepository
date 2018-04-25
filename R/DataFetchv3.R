library(readxl)
library(quantmod)
library(xts)


rm(list=ls()) #Clears environment

URL.repo=getwd()
#URL=paste(URL.repo,"/Data/Stocks.xlsx",sep="")
URL=paste(URL.repo,"/Data/StocksOBX.xlsx",sep="")
stocks <- read_excel(URL,sheet = "Sheet1")


#stocks=stocks[c(5,7,8),] #For testing. Et utvalg av aksjer.
#stocks=stocks[c(10,114),]

#DISCARDED STOCKS

discardedStocksVector=c()

# SET FROM DATE
from.date <- as.Date("01/04/10", format="%m/%d/%y")


# SET TO DATE
to.date <- as.Date("03/31/18", format="%m/%d/%y")

consecutiveZerosCapClose=20

# VOLUME CONSTRAINTS
XCapVolume=0
consecutiveXCapVolume=20


# INIT LISTS AND LENGTH OF DESIRED OBX STOCKS
stocks.nrow=nrow(stocks)
  
stocksRemoved.index=vector()
stocksRemoved.reason=vector()

stockData=new.env()

for (row in 1:stocks.nrow) {
  if (length(discardedStocksVector)>0){
    discard=FALSE  
    for (discardedStocksIndex in 1:length(discardedStocksVector)){
      if(stocks[row,1]==discardedStocksVector[discardedStocksIndex]){
        discard=TRUE
      }
    }
    if (discard==TRUE){
      stocksRemoved.index[length(stocksRemoved.index)+1]=row
      stocksRemoved.reason[length(stocksRemoved.reason)+1]="Data Missing/Not Continously Listed"
      next
    }
  }
  
  tryCatch({
    
    fetchName=paste(stocks$Ticker[row],".OL",sep="")
    stock.data=getSymbols(fetchName,from=from.date,to=to.date,auto.assign = FALSE)
    
    if(index(stock.data)[1]==(from.date)){
      
      vectorizedClose=drop(coredata(stock.data[,4]))
      vectorizedClose[is.na(vectorizedClose)] <- 1000000000
      occurences = rle(vectorizedClose)
      consecutiveZerosVector.close=occurences$lengths[occurences$values == 1000000000]
      if(length(consecutiveZerosVector.close>0)){
        
        maxOfconsecutiveZerosVectorClose=max(consecutiveZerosVector.close)
        # print(fetchName)
        # print(maxOfconsecutiveZerosVectorClose)
        # print(occurences$lengths)
        # print("")
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
          getSymbols(fetchName,from=from.date,to=to.date,auto.assign =TRUE,env=stockData)
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

stocks=stocks[order(stocks$Ticker),] #Sorterer stocks removed etter index
  

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

stockPrices=stockPrices[,order(names(stockPrices))] #Sorter kolonner alfabetisk etter ticker
stockReturns=stockReturns[,order(names(stockReturns))] #Sorter kolonner alfabetisk etter ticker

names(stockPrices)=stocks$Stock #Endrer navn p? kolonner
names(stockReturns)=stocks$Stock

#Volume

stockVolumesList <- eapply(stockData, Vo)
stockVolumes <- do.call(merge, stockVolumesList)

stockVolumes=stockVolumes[,order(names(stockVolumes))]
#raderFor=nrow(stockPrices)
#stockPrices=na.omit(stockPrices) #Tar bort rader med na
#raderEtter=nrow(stockPrices)

#stockVolumes[is.na(stockVolumes)] <- 0 # Setter NA elementer til 0.
names(stockVolumes)=stocks$Stock #Endrer navn p? kolonner

#Index fetch
OBX.close.price=getSymbols("OSEBX.OL",from=from.date,to=to.date,auto.assign =FALSE)[,4] #Bruker OBX Price Index som bare er Cap gain
OBX.close.price=do.call(merge,list(OBX.close.price,stockPrices))[,1] #For ? s?rge for at OBX har like mange datapunkter som aksjene.

OBX.close.return=diff(log(OBX.close.price))
OBX.close.return=OBX.close.return[-c(1),]

OBX.close.price[is.na(OBX.close.price)] = 0 #NA fylles inn der den forrige linjen gir NA. Det vil si n?r OBX har f?rre datapunkter enn aksjene. 
OBX.close.return[is.na(OBX.close.return)] = 0

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
URL=paste(URL.repo,"/Data/OBXReturn.Rda",sep="")
save(OBX.close.return,file=URL)
