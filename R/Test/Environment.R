library(parallel,warn.conflicts = FALSE)
library(doParallel,warn.conflicts = FALSE)
library(quantmod,warn.conflicts = FALSE)
library(lattice,warn.conflicts = FALSE)
library(timeSeries,warn.conflicts = FALSE)
library(rugarch,warn.conflicts = FALSE)
library(xts,warn.conflicts = FALSE)
library(tseries,warn.conflicts = FALSE)
library(bigmemory,warn.conflicts = FALSE)
library(R.utils,warn.conflicts = FALSE)

rm(list=ls()) #Clears environment

day=120
number=4

URL.repo=getwd()
URL=paste(URL.repo,"/Debugging/",day,"_",number,".RData", sep="")
load(URL)

rm(list=ls()) #Clears environment
URL.repo=getwd()
URL=paste(URL.repo,"/From Solstorm/130_5.Rdata", sep="")
load(URL)

URL.repo=getwd()
URL=paste(URL.repo,"/Data/ErrorInDay.Rda", sep="")
load(URL)
