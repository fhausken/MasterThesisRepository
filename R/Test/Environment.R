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

day=809
number=5

URL.repo=getwd()
URL=paste(URL.repo,"/Debugging/",day,"_",number,".RData", sep="")
load(URL)


URL=paste(URL.repo,"/Data/ErrorInForecastFitting.Rda", sep="")
load(URL)
