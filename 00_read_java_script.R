library(RCurl)
library(jsonlite)
library(XML)
library(readr)
library(rstudioapi)
setwd(base_directory)

stations<-read.table("stations_meta_data.csv",sep=";",header=T,stringsAsFactors=F)
brazil_timezones<-read.table("brasil_timezones.csv",sep=";",header=F)
winter_shift<-as.POSIXct(brazil_timezones[,2],tz="GMT")+7200
summer_shift<-as.POSIXct(brazil_timezones[,6],tz="GMT")+7200

#execute if data is not available locally
#firstdownload()


par(mfrow=c(4,4))
#9
for(station in 436:nrow(stations)){

  s<-seq(ISOdate(1999,1,1), ISOdate(2015,3,20), "hour")
  final<-NULL
  print(paste("Dealing with",stations$name[station]))
  
  for(y in 1999:2016){
    final<-read_one_file(final)
  }
  
  date_start<-("1999-01-01 00:00:00")
  date_end<-("2016-04-20 00:00:00")
  date_seq<-data.frame(seq(as.POSIXct(date_start,tz="GMT"),as.POSIXct(date_end,tz="GMT"),by="hour")) 
  names(date_seq)<-c("d")
  final1<-merge(date_seq,final,by.x="d",by.y="date",all.x=TRUE)
  
  write.table(final1,paste("data/",stations$name[station],".csv",sep=""),sep=";")
  save(final1,file=paste("data/",stations$name[station],".RDat",sep=""))
  
  #ff<-final1[final1[,1]>as.POSIXct("2014-10-10")&final1[,1]<as.POSIXct("2014-10-30"),]
  
  
  #ff[is.na(ff)]<-0
  #plot(1:24,ff[1:24,6],type="l")
  #for(i in 1:10){lines(1:24,ff[(1+24*i):(24+24*i),6],col="green")}
  #for(i in 11:20){
  #  print(i)
  #  print(ff[(1+24*i):(24+24*i),6])
  #  lines(1:24,ff[(1+24*i):(24+24*i),6],col="red")}
  
  
  
  #final1[final1[,1]>as.POSIXct("2014-10-18")&final1[,1]<as.POSIXct("2014-10-20"),]
  #plot(final1[,6],type="l")
}




read_one_file<-function(final){
  print(y)
  df<-data.frame(date.time=rep(0,8784*8),val=rep(0,8784*8),type=rep("",8784*8),stringsAsFactors =FALSE)
  names(df)<-c("date.time","val","type")
  
  name<-paste("data/",stations$name[station],y,".txt",sep="")
  result<-read_file(name)
    
  if(substr(result[1],1,20)=="Registro Inexistente"){
    print(paste(stations$cod[station],y," not found"))
    return( final)  
  }
  
  txtvec <- strsplit(result,'\n')[[1]]
  txtvec<-txtvec[1:(length(txtvec)-619)]
  
  dates<-txtvec[seq(22,length(txtvec),3)]
  
  dates<-as.numeric(unlist(lapply(dates,extractdate)))/1000
  
  dates<-as.POSIXct(dates,origin="1970-01-01",tz="GMT")
  vals<-txtvec[seq(23,length(txtvec),3)]
  
  vals<-unlist(lapply(vals,extractval))
  vals[is.na(vals)]<--1
  
  types<-txtvec[seq(24,length(txtvec),3)]
  
  types<-unlist(lapply(types,extracttype))
  
  df_f<-data.frame(dates,vals,types)
 
  m<-df_f
 
  m[m[,1]<winter_shift[y-1998],1]<-m[m[,1]<winter_shift[y-1998],1]+3600
  m[m[,1]>summer_shift[y-1998],1]<-m[m[,1]>summer_shift[y-1998],1]+3600
  df_f<-m
  
  marker<-c("dados_temp","dados_umi.","dados_po.p","dados_pres","dados_rad.","dados_pres","dados_vdd.","dados_vvel")
    
  l<-lapply(X=marker,FUN=selector,df_f) 
  date_start<-paste(y,"-01-01 00:00:00",sep="")
  date_end<-paste(y+1,"-01-01 00:00:00",sep="")
  date_seq<-data.frame(seq(as.POSIXct(date_start,tz="GMT"),as.POSIXct(date_end,tz="GMT"),by="hour"))  
  names(date_seq)<-"d"
 
  m<-date_seq
 
  for(i in 1:length(l)){
    m<-merge(m,l[[i]][,1:2],by.x="d",by.y="dates")
   }
 
 
  names(m)<-c("date",marker)
 
  return(rbind(final,m))
}


selector<-function(mark,f){
  
  return(f[f$types==mark,])
  
}


extractdate<-function(d){ 
  
  date<-substr(d,11,100)
  date<-gsub("; \r","",date)
  return (date)
}



extractval<-function(v){
  val<-substr(v,11,100)
  val<-as.numeric(gsub(";\r","",val))
  return(val)
  
}


extracttype<-function(t){
  
  
  type<-substr(t,1,10)
  return(type)
}

latlontimezone<-function(lat,lon)
{
  
 require(XML)
  url <- paste('http://api.geonames.org/timezone?lat=',lat,'&lng=',lon,'&username=joph&date=2014-02-17')
  data <- xmlParse(url)
  xml_data <- xmlToList(data)
  return(xml_data$timezone$timezoneId)
  
}

firstdownload<-function(){
  
 
  
  for(station in 394:nrow(stations)){
    print("Downloading...")
    url<- "http://www.inmet.gov.br/projetos/grafico/ema_html_pg.php"
    
    print(paste("Dealing with",stations$name[station]))
    
    for(y in 1999:2016){
      print(y)
      result <- postForm(url, mRelEstacao=stations$cod[station],
                         mRelAno=y,
                         btnProcesso=" Gera ")
      name<-paste("data/",stations$name[station],y,".txt",sep="")
      write(result,file=name)
    }
  }
  
}




