library(fields)
library(ff)
setOldClass("ff_array")

setRelativeDirectory(".")
stations<-read.table("stations_meta_data.csv",sep=";",header=T,stringsAsFactors=F)
setRelativeDirectory("code/copa")
source("000_classes.R")
source("000_functions.R")
setRelativeDirectory(".")


#run once only to prepare input files!
#prepares the final ECMWF And MERRA objects that can be later used for validation
setwd("ecmwf")
years<-2008:2016
a1<-convertAndSaveECMWF(years)
e<-ecmwfMakeObject(a1,years)
a_final<-convertAndSaveMERRA(years)
m<-merraMakeObject(a_final,years)
save(a1,file="ECMWF.RDat")
save(a_final,file="MERRA.dat")

