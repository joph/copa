library(zoo)
setRelativeDirectory(".")
stations<-read.table("stations_meta_data.csv",sep=";",header=T,stringsAsFactors=F)

df<-NULL

#preinitialize dates
load(paste("data/",stations$name[1],".RDat",sep=""))
date.start<-as.POSIXct("2008-01-01 00:00:00",tz="GMT")
dates<-final1[,1]
dates1<-dates[dates>=date.start]
date1 <- as.yearmon(dates1, "%b")
runner<-c(rep(1:12,8),1,2,3,4)

final_data<-data.frame(dates1)

#http://ascelibrary.org/doi/10.1061/%28ASCE%290733-9437%281996%29122%3A2%2897%29

for(station in 1:nrow(stations)){
  
 load(paste("data/",stations$name[station],".RDat",sep=""))
 #cut below 2008
 
 
 print(stations$name[station])
 rad<-final1[,6]
 
 rad<-rad[dates>=date.start]
 
 
 
 #correct 0
 rad[rad<0]<-0
 
 
 
 #correct to kWh
 rad<-rad*0.27777777777778
 
 #correct too high irradiation
 rad[rad>1378]<-NA
 
 #plot(rad,type="l")
 #w<-which(is.na(rad))
 #points(w,rep(500,length(w)),col="red",pch=3)
 
 
 #full years
 #interpolate NAs up to length 10
 rad_na<-na.approx(rad,na.rm=FALSE,maxgap=24)
 #plot(!is.na(rad_na))
 #remove months with too low mean
 ag<-aggregate(rad_na,by=list(date1),sum)
 mo_remov<-ag[,2]<72000 
 months_to_remove<-ag[ag[,2]<50000,1]
 months_to_remove<-c(months_to_remove,ag[is.na(ag[,2]),1])
 months_to_remove<-months_to_remove[!is.na(months_to_remove)]
 rad_na[which(date1%in%months_to_remove)]<-NA
 plot(rad_na,type="l")
 
 ag<-aggregate(rad_na,by=list(date1),sum)
 ag1<-aggregate(!is.na(ag[,2]),by=list(runner),sum)
 
 
 #length of non-NAs
 seq<-rle(is.na(rad_na))
 seq1<-seq$lengths[seq$values==FALSE]
 
 
 
 #calculate mean, variance
 vals<-c(sum(!is.na(rad)),sum(!is.na(rad_na)),mean(rad,na.rm=TRUE), mean(rad_na,na.rm=TRUE),var(rad_na,na.rm=TRUE),min(rad_na,na.rm=TRUE),max(rad_na,na.rm=TRUE),max(seq1),min(ag1[,2]))
 df<-rbind(df,vals)   
 
 final_data<-cbind(final_data,rad_na)
 
 #longest sequence of non-nas
 
 #number of non-NaNs

}

save(final_data,file="final_data.RDat")
load("final_data.RDat")
df_quality_data<-data.frame(df)
names(df_quality_data)<-c("nmbnonNAs","nmbnon_NAsaftint","mean","meanaftint","var","min","max","maxrle","numbfullyears")
setwd("d:/google drive/brasil/hourly_data")
save(df_quality_data,file="quality_data.RDat")

i<-INMET$new(final_data[,2:ncol(final_data)],stations[,c(3,2)],final_data[,1],1)
save(i,file="INMETobject.RDat")

hist(df$meanaftint)
hist(df$nmbnonNAs)
hist(df$var)
hist(df$numbfullyears)
sum(df$numbfullyears>4)
####here: define selection criteria




