library(fields)
library(ff)
setOldClass("ff_array")

setRelativeDirectory(".")
stations<-read.table("stations_meta_data.csv",sep=";",header=T,stringsAsFactors=F)
setRelativeDirectory("code/copa")
source("000_classes.R")
source("000_functions.R")
setRelativeDirectory(".")

####run each time
setwd("ecmwf")
load("ECMWF.RDat")
e<-ecmwfMakeObject(a1,2008:2016)

setwd(".")
load(file="final_data.RDat")
#shorten to 2015
final_data<-final_data[1:70128,]
i<-INMET$new(final_data[,2:ncol(final_data)],stations[,c(3,2)],final_data[,1],1)
setwd("merra2")
load("MERRA.dat")
m<-merraMakeObject(a_final,2008:2016)


####run analysis on data
ecmwf_analysis<-finaldf(i,e,3,-1*3600*3)
merra_analysis<-finaldf(i,m,3,0)
merra_analysis1<-finaldf(i,m,1,0)
setwd(".")
#save(ecmwf_analysis,merra_analysis,merra_analysis1,file="analysis_ready.RDat")
#load("analysis_ready.RDat")

#join analysis in data.frame
ecmwf_analysis<-data.frame(ecmwf_analysis,run=1)
merra_analysis<-data.frame(merra_analysis,run=2)
merra_analysis1<-data.frame(merra_analysis1,run=3)

###create joint frame with all results
f<-function(x){round(mean(x),2)}
ddf_<-rbind(apply(ecmwf_analysis,2,f),apply(merra_analysis,2,f),apply(merra_analysis1,2,f))
ddf1<-data.frame(c("ECMWF","MERRA2","MERRA2-Hourly"),ddf_)

setwd(".")
load("quality_data.RDat")
fin_bind<-cbind(df_quality_data,rbind(ecmwf_analysis,merra_analysis,merra_analysis1))
fin_bind<-data.frame(fin_bind)

#create final data.frame with results of complete comparison
fin_bind_mo<-data.frame(fin_bind[,c(2,4:10,12)],diff_mean=(fin_bind[,c(12)]-fin_bind[,c(13)]),
                        fin_bind[,c(14:16)],
                        fin_bind[,c(20:22)],
                        maxext50diff=(fin_bind[,23]-fin_bind[,25]),
                        meanext50diff=(fin_bind[,24]-fin_bind[,26]),
                        maxext75diff=(fin_bind[,27]-fin_bind[,29]),
                        meanext75diff=(fin_bind[,28]-fin_bind[,30]),
                        maxext25diff=(fin_bind[,31]-fin_bind[,33]),
                        meanext25diff=(fin_bind[,32]-fin_bind[,34]),
                        cor.deseas=fin_bind$cor.deseas,run=fin_bind$run)

ddf<-NULL
for(jj in 1:80){
  filter<-fin_bind$n.m>jj
  ag<-aggregate(fin_bind_mo[filter,],by=list(fin_bind[filter,]$run),f)
  ag$n<-sum(filter)/3
  ddf<-rbind(ddf,ag)
  
}

#figure that shows how max and mean extreme values behave depending on subset of data
plotComparison(ddf,c("meanext25diff","maxext25diff"),c("mean ext 25 diff ","max ext 25 diff"))

#figure that shows how cor behaves depending on subset of data
plotComparison(ddf,c("cor.h","cor.m"),c("cor.h ","cor.m"))

#figure that shows how cor.deseas behaves depending on subset of data
plotComparison(ddf,c("cor.deseas","diff_mean"),c("cor.deseas ","diff.mean"))

#figure that shows how the humber of stations
plotComparison(ddf,c("n","diff_mean"),c("n","diff.mean"))

#figure that shows how the humber of stations
plotComparison(ddf,c("nrmse.h","nrmse.m"),c("nrmse.h","nrmse.m"))

filter<-fin_bind$n.m>68
sum(filter)/3
filt(fin_bind_mo,filter,stations,final_data,plot=FALSE)




#erroneous
filter<-!(c(1:481)%in%c(which(fin_bind_e$numbfullyears<3)))
####compare different data sources
compare_datasources(fin_bind_mo,filter,1,3)

aggregate(fin_bind,by=list(fin_bind$run),mean)
aggregate(fin_bind,by=list(fin_bind$run),mean)


fin_bind_e<-fin_bind[fin_bind$run==1,]
fin_bind_m<-fin_bind[fin_bind$run==2,]
apply(fin_bind_e,2,mean)


###stations to remove
#1: antarctica
#5: acre: strange pattern
#filter<-fin_bind_e$numbfullyears<3:no complete 3 years!
filter<-!(c(1:481)%in%c(which(fin_bind_e$numbfullyears<3)))
filt(fin_bind_e,filter,stations,final_data,plot=FALSE)
filter<-fin_bind_e$n.m>1
filt(fin_bind_e,filter,stations,final_data,plot=FALSE)
filt(fin_bind_m,filter,stations,final_data,plot=FALSE)


t<-data.frame(fin_bind[fin_bind$run==1,]["nmbnon_NAsaftint"],fin_bind[fin_bind$run==1,]["cor.h"])
par(mfrow=c(1,1))
plot(t)
fin_bind_e<-fin_bind[fin_bind$run==1,]



####run analysis for random samples
filter<-fin_bind[fin_bind$run==1,]$n.m>78
filt(fin_bind_mo,filter,stations,final_data,plot=FALSE)

final_data_reduced<-final_data[,2:ncol(final_data)]
final_data_reduced<-final_data[,filter]
i_reduced<-INMET$new(final_data_reduced,stations[,c(3,2)],final_data[,1],1)
setwd("D:/google drive/brasil/hourly_data")
source("000_classes.R")
i<-INMET$new(final_data_reduced,stations[,c(3,2)],final_data[,1],1)
n<-2
ss<-i$getSample(n)
e<-ecmwfMakeObject(a1,2008:2016)
ss1<-e$retSample(ss[[1]])

comp_single(1,2,i,e,3,-1*3600*3)
unlist(comp(88,i,e,3,-1*3600*3))



res<-mapply(comp_single,c(1:20),MoreArgs=list(jj,i_reduced,e,3,-1*3600*3),SIMPLIFY=FALSE)

ddf<-NULL
for(jj in seq(2,18,4)){
  print("round")
  print(jj)
  res<-mapply(comp_single,c(1:20),MoreArgs=list(jj,i_reduced,e,3,-1*3600*3),SIMPLIFY=FALSE)
  df <- data.frame(matrix(unlist(res), nrow=20, byrow=T))
  df<-cbind(jj,df)
  ddf<-rbind(ddf,df)  
}

r<-rep(seq(2,6,4),each=20,1)
ddf[,1]<-r
names(ddf)<-c("runn",TS_Meteo$new(c(1:10),c(as.POSIXct(100,origin="1970-01-01")),0)$getcompnames())
par(mfrow=c(1,1))
runn<-unlist(ddf$runn)
aggregate(ddf,by=list(runn),mean)
boxplot(unlist(ddf["cor h"])~runn)
boxplot(unlist(ddf["cor d"])~runn)
boxplot(unlist(ddf["cor m"])~runn)
boxplot(unlist(ddf["n d"])~runn)
boxplot(unlist(ddf["n m"])~runn)
boxplot(unlist(ddf["max ext 50 y"])~runn)

boxplot(ddf[,6]~ddf[,1])
boxplot(ddf[,7]~ddf[,1])
boxplot(ddf[,8]~ddf[,1])
boxplot(ddf[,9]~ddf[,1])
par(mfrow=c(1,3))
boxplot((ddf[,14]-ddf[,16])~ddf[,1],col="lightgreen",xlab="# locations",main="Diff in mean length in 75% of mean")
boxplot((ddf[,10]-ddf[,12])~ddf[,1],col="gold",xlab="# locations",main="Diff in mean length in 50% of mean")
boxplot((ddf[,18]-ddf[,20])~ddf[,1],col="darkblue",xlab="# locations",main="Diff in mean length in 25% of mean")

boxplot((ddf[,15]-ddf[,17])~ddf[,1],col="lightgreen")
boxplot((ddf[,11]-ddf[,13])~ddf[,1],add=TRUE,col="gold")
boxplot((ddf[,19]-ddf[,21])~ddf[,1],add=TRUE,col="darkblue")


###bind complete analysis


filt(ddf$numbfullyears>4,stations)

####selector
thres<-0.85
d_e<-ddf[ddf[,5]>thres,]
apply(d_e,2,mean)
plotStationsColorGradient(stations,ddf$numbfullyears)
