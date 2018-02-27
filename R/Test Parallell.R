rm(list=ls()) #Clears environment


library(parallel)
library(doParallel)

no_cores=detectCores()-2
c1=makeCluster(no_cores)
registerDoParallel(c1)


cat("Output:\n\n", file="Output/ParallellLog.txt", append=FALSE) #Clears log

start_time <- Sys.time()
forloop=foreach(i=1:1000, .combine=rbind) %dopar%{ #rbind står får row bind. kan også skrive cbind som betyr column bind. Eller bare c, da blir alt listet etter hverandre i kolonner. 
  cat(paste(Sys.time(), "Starting iteration",i, "\n"), file="/Output/ParallellLog.txt", append=TRUE)
  
  return((list(list(i,i+i),"ooh")))
 
}
end_time <- Sys.time()
run_time=end_time-start_time
cat(paste("\nKjøretid:",run_time, "\n"), file="Output/ParallellLog.txt", append=TRUE)


#Merk at ved bruk av rbind og cbind, dukker foreLoop opp i "Data" slik at man kan se objektets innhold. Veldig smart.

stopCluster(c1)
