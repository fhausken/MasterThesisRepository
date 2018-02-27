library(readxl)
library(quantmod)
library(qmao)

companies <- read_excel("C:/Users/Fredrik Hausken/OneDrive/Skole/NTNU/Master/Master-Thesis-Repo/Data/Companies.xlsx", 
                        sheet = "Sheet1")
from.date <- as.Date("01/04/10", format="%m/%d/%y") 
consecutiveZerosCap=5

companies.nrow=nrow(companies)
  
companiesRemoved.index=vector()
companiesRemoved.reason=vector()


stockData=new.env()

for (row in 1:companies.nrow) {
  
  tryCatch({
    
    fetchName=paste(companies$Ticker[row],".OL",sep="")
    company.data=getSymbols(fetchName,from=from.date,auto.assign = FALSE)
    
    if(index(company.data)[1]==(from.date)){
      
      vectorized=drop(coredata(company.data[,4]))
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
        companiesRemoved.index[length(companiesRemoved.index)+1]=row
        companiesRemoved.reason[length(companiesRemoved.reason)+1]="Illiquid/Not continously listed"
      }
    }
    else{
      companiesRemoved.index[length(companiesRemoved.index)+1]=row
      companiesRemoved.reason[length(companiesRemoved.reason)+1]="Listed later"
    }

  },error = function(e) {
    companiesRemoved.index[length(companiesRemoved.index)+1]<<-row #Merk dobbel pil som betyr at man aksesserer en globalvariabel
    companiesRemoved.reason[length(companiesRemoved.reason)+1]<<-"Not found"
    })

}
companiesRemoved=companies[companiesRemoved.index,]
companiesRemoved$Reason=companiesRemoved.reason
companies=companies[-companiesRemoved.index,]

stockPricesList <- eapply(stockData, Cl)
stockPrices <- do.call(merge, stockPricesList)



#raderFor=nrow(stockPrices)
#stockPrices=na.omit(stockPrices) #Tar bort rader med na
#raderEtter=nrow(stockPrices)

stockPrices[is.na(stockPrices)] <- 0 # Setter NA elementer til 0.

names(stockPrices)=companies$Ticker #Endrer navn på kolonner

