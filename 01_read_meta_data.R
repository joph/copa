setRelativeDirectory(".")

res_table<-data.frame(cod=c(0),lat=c(0),lon=c(0),name=c(""))
r<-read.table("station_data.txt",sep="!",stringsAsFactors=FALSE)
for(i in seq(1,nrow(r),6)){
  cod<-substr(r[i,1],34,37)
  latlon<-substr(r[i+1,1],45,100)
  print(latlon)
  a<-strsplit(latlon,",")
  lat<-a[[1]][1]
  lon<-a[[1]][2]
  lon<-substr(lon,1,nchar(lon)-2)
  name<-substr(r[i+3,1],22,100)
  name<-gsub(";","",name)
  name<-gsub(" ","",name)
  res_table<-rbind(res_table,data.frame(cod=cod,lat=lat,lon=lon,name=name))
  
}
res_table<-res_table[-1,]
write.table(res_table,"stations_meta_data.csv",sep=";")
