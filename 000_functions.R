setRelativeDirectory<-function(x){
  setwd(paste(base_directory,"/",x,sep=""))
  }

finaldf<-function(i,m,timeres,shift,n=0){
  if(n<=0){
  n_<-i$getSize()
  }else{
    n_<-n
  }
  r<-mapply(comp,1:n_,MoreArgs=list(i,m,timeres,shift),SIMPLIFY=FALSE)
  df <- data.frame(matrix(unlist(r), nrow=n_, byrow=T))
  df<-cbind(1:nrow(df),df)
  names(df)<-c("ind","distance",m$minDistanceTS(0,0)$getcompnames())
  return(df)
}

compare_datasources<-function(df,filter, first, second){
  ag<-aggregate(df[filter,],by=list(df[filter,]$run),f)
  return(ag[first,]-ag[second,])
}

plotComparison<-function(df,vars,text){
  par(mfrow=c(1,2))
  for(jj in c(1:2)){
    matplot(data.frame(df[vars[jj]][seq(1,nrow(df),3),],df[vars[jj]][seq(2,nrow(df),3),]),type="l",main=paste(text[jj],"ECMWF vs. MERRA2"))
  }
}

filt<-function(ddf,filter,stations,final_data,plot=FALSE){
  print(sum(filter))
  print(apply(ddf[filter,],2,f))
  par(mfrow=c(1,1))
  plotSelectedStations(filter,stations)
  if(plot){
    print(stations[filter,])
    mapply(plot,final_data[,which(filter)])
  }
  
}



comp<-function(j,i,e,timeres,shift=0){
  print(j)
  ll<-(i$getIndexLonLat(j))
  
  inmet<-i$getIndex(j)
  comp_<-e$minDistanceTS(ll[1],ll[2])
  plotBrazil()
  points(ll[1],ll[2],col="red")
  
  
  comp_$shift(shift)
  
  inmet<-inmet$getHourly(timeres)
  comp_<-comp_$getHourly(timeres)
  
  r<-comp_$compare(inmet,comp_)
  return(c(e$getMinDistance(ll[1],ll[2]),r))
}

comp_single<-function(run,n,i,comp_,timeres,shift=0){
  print(run)
  nonas<-0
  while(nonas<(8760)){
    ss<-i$getSample(n)
    
    nonas<-ss[[3]]
    print(nonas)
  }
  
  ss1<-comp_$retSample(ss[[1]])
  
  ss1$shift(shift)
  
  a<-ss[[2]]$getHourly(timeres)
  b<-ss1$getHourly(timeres)
  
  r<-b$compare(a,b)
  return(r)
}




which_column=function(a,b){
  return(which(a==b))
}

convertAndSaveECMWF<-function(years){
  library(ncdf4)
  library(abind)
  
  n<-nc_open(paste("ecmwf",years[1],".nc",sep=""))  
  v1<-ncvar_get(n,"longitude")
  v2<-ncvar_get(n,"latitude")
  
  lonlat<-expand.grid(v2,v1)
  lonlat<-lonlat[,c(2,1)]
  
  #http://www.ecmwf.int/sites/default/files/radiation_in_mars.pdf
  l<-mapply(loadECMWF,years,"ssrd",SIMPLIFY=FALSE)
  
  a<-abind(l)
  a1<-apply(a,c(1:2),convertECMWFDiffWh)
  a1<-aperm(a1,c(2,3,1))
  
  save(a1,file="ECMWF.RDat")
  return(a1)
  ####TODO: SAVE LONLATFILE!!!
}

ecmwfMakeObject<-function(a1,years){
  library(ncdf4)
  library(abind)
  setRelativeDirectory("ecmwf")
  n<-nc_open(paste("ecmwf",2008,".nc",sep=""))  
  v1<-ncvar_get(n,"longitude")
  v2<-ncvar_get(n,"latitude")
  
  lonlat<-expand.grid(v2,v1)
  lonlat<-lonlat[,c(2,1)]
  
  l<-mapply(loadECMWF,years,"time",SIMPLIFY=FALSE)
  time<-abind(l)
  time<-as.POSIXct(time*3600,tz="GMT",origin="1900-01-01 00:00:00")
  a2<-as.ff(a1)
  e<-SpatialMetData$new(a2,lonlat,time,3)
  return(e)
  
  
}


convertAndSaveMERRA<-function(years){
  library(ncdf4)
  library(abind)
  library(ff)
  setRelativeDirectory("merra2")
  n<-nc_open(paste("svc_MERRA2_300.tavg1_2d_rad_Nx.20080101",".nc4",sep=""))
  v1<-ncvar_get(n,"lon")
  v2<-ncvar_get(n,"lat")
  
  lonlat<-expand.grid(v2,v1)
  lonlat<-lonlat[,c(2,1)]
  
  #http://www.ecmwf.int/sites/default/files/radiation_in_mars.pdf
  dates<-seq(as.POSIXlt(paste(years[1],"-01-01",sep=""),tz="GMT"),as.POSIXct(paste(years[length(years)],"-03-27",sep=""),tz="GMT"),"d")
  
  dates_str<-format(dates,format="%Y%m%d")
  l<-mapply(loadMERRA,dates_str,dates,"SWGDN",SIMPLIFY=FALSE)
  
  a_final <- ff(NA, dim=c(length(v1),length(v2),24*length(l)),vmode="double",filename="MERRA_FF.Dat",overwrite=TRUE) 
  
  for(cnt in 0:(length(l)-1))
    a_final[,,((cnt*24)+1):((cnt+1)*24)]<-l[[cnt+1]][,,]
  
  save(a_final,file="MERRA.dat")
  return(a_final)
}

merraMakeObject<-function(a_final,years){
  library(ncdf4)
  setRelativeDirectory("merra2")
  n<-nc_open(paste("svc_MERRA2_300.tavg1_2d_rad_Nx.20080101",".nc4",sep=""))
  v1<-ncvar_get(n,"lon")
  v2<-ncvar_get(n,"lat")
  
  lonlat<-expand.grid(v2,v1)
  lonlat<-lonlat[,c(2,1)]
  
  dates<-seq(as.POSIXlt(paste(years[1],"-01-01 00:00:00",sep=""),tz="GMT"),as.POSIXct(paste(years[length(years)],"-03-27 23:00:00",sep=""),tz="GMT"),"h")
  
  m<-SpatialMetData$new(a_final,lonlat,dates,1)
  save(m,file="MERRA_object.dat")
  return(m)
  
  
}





loadECMWF<-function(i,var){
  n<-nc_open(paste("ecmwf",i,".nc",sep=""))  
  v<-ncvar_get(n,var)
  return(v)
}

loadMERRA<-function(i,dates,var,retAsFF=TRUE){
  #var<-"SWGDN"
  print(dates)
  f<-paste("svc_MERRA2_300.tavg1_2d_rad_Nx.",i,".nc4",sep="")
  if(dates>as.POSIXlt("2010-12-31 23:59:00",tz="GMT")){
    f<-paste("svc_MERRA2_400.tavg1_2d_rad_Nx.",i,".nc4",sep="")
    
  }
  print(f)
  n<-nc_open(f)  
  v<-ncvar_get(n,var)
  nc_close(n)
  v1<-ff(v,dim=dim(v))
  if(retAsFF==TRUE){
    return(v1)}
  return(v)
}

getSubVecDiffWh<-function(c){
  b<-c(c[1],diff(c))
  b<-b*2.77778e-4
  return(b)
}

convertECMWFDiffWh<-function(c){
  #I<-I+1
  cat("|")
  par(mfrow=c(1,1))
  #plot(c[1:24],type="l")
  s<-split(c, rep(1:ceiling(length(c/8)), each=8)[1:length(c)])
  m<-mapply(getSubVecDiffWh,s,SIMPLIFY=FALSE)
  mm<-unlist(m)
  #lines(mm[1:24]/2.77778e-4,type="l",col="red")
  return(mm)
}



countExtremes<-function(t,ex){
  t<-t[!is.na(t[,1])&!is.na(t[,2]),]
  return(c(
    countExtreme(t[,1],ex),
    countExtreme(t[,2],ex)))
  
}

countExtreme<-function(t,ex){
  m<-mean(t,na.rm=TRUE)
  a<-(t<(m*ex))  
  r<-rle(a)
  dist<-r$lengths[r$values==TRUE]
  if(length(dist)==0){return(c(0,0))}
  return(c(max(dist,na.rm=TRUE),mean(dist,na.rm=TRUE)))
  
}


plotBrazil<-function(){
  
  library(maptools)
  
  #Load in the data file (could this be done from the downloaded zip file directly?
  gor=readShapeSpatial('D:/google drive/brasil/gis/gis-dataset-brasil-master/uf/shapefile/uf.shp')
  
  #I can plot the shapefile okay...
  plot(gor)
  
}

plotSelectedStations<-function(b,stations){
  plotBrazil()
  points(stations$lon,stations$lat)
  points(stations[b,]$lon,stations[b,]$lat,col="red",pch=16)
}

plotStationsColorGradient<-function(stations,col){
  #normalize to 0 - 1
  col1<-col-min(col)
  col1<-col1/max(col1)
  r<-rgb(col1,0,0)
  
  plotBrazil()
  points(stations$lon,stations$lat,col=r,pch=16)
  #text(stations$lon,stations$lat,labels=round(col,2),cex=0.6)
}
