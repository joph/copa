library(hydroGOF)

#Class TS_Meteo is a wrapper for meteorological time series
#it is able to aggregate the class hourly, to shift the time index, to deseasonalize the class,
#and most importantly, to compare that class to another class
TS_Meteo<- setRefClass("TS_Meteo",
                       fields = list(data = "vector",dates="POSIXct",timeres="numeric"),
                       
                       methods = list(                   
                         
                         ####constructor
                         ####necessary: a vector with the data, a vector with the corresponding dates
                         ####and an integer informing the timeresolution in hours (e.g. a value of 3 indicates
                         ####that there is a value every 3 hours)
                         initialize = function(data1,dates1,timeres1){
                           
                           ##set data
                           data<<-data1
                           dates<<-dates1
                           
                           timeres<<-timeres1
                           
                         },
                         
                         
                         ####returns a new TS_Meteo object
                         ####that has a time resolution according to n
                         ####precondition: n has to be equal or larger than the original
                         ####resolution (no downscaling implemented yet)
                         ####and n has to be a multiple of timeres
                         ####aggregation is done using the R function aggregate
                         getHourly = function(n) {
                           
                           if(timeres<n){
                             ###aggregate by sumation
                             if(n %% timeres != 0) {
                               stop("provided number is not multiple of time resolution")
                             }
                             
                             fact<-n/timeres
                             ag_list<-rep(1:((length(dates)/fact)),each=(fact))
                              
                             if(length(ag_list)<length(data)){
                               data<<-data[1:length(ag_list)]
                             }
                             if(length(ag_list)>length(data)){
                               ag_list<-ag_list[1:length(data)]
                             }
                             
                             
                             aa<-aggregate(data,by=list(ag_list),sum)
                             dates_new<-dates[seq(1,length(dates),fact)]
                             
                             return(TS_Meteo$new(aa[,2],dates_new,n))
                             
                           }else {
                             ####perhaps do later....
                             if(timeres==n){
                               return(TS_Meteo$new(data,dates,n))
                               
                             }
                             
                           }
                           
                           
                         },
                         
                         ####plots the timeseries
                         plotIt = function(){
                           
                           plot(data,type="l")
                         },
                         
                         ####shifts the date by by hours.
                         shift = function(by){
                           
                           dates<<-dates + by
                         },
                         
                         
                         ####compares two timeseries
                         ####returns a vector with different measures of comparison
                         
                         compare = function(x,y){
                           
                           
                           ###adjust timeseries to lower frequency
                           if(y$timeres>x$timeres){
                             
                             x<-x$getHourly(timeres)
                           }else{
                             if(y$timeres<x$timeres){
                               y<-y$getHourly(timeres)
                             }
                           }
                           
                           ###adjust start and end date
                           if(x$getStart()>y$getStart()){
                             y<-y$getPeriod(x$getStart(),y$getEnd())
                             
                           }
                           
                           if(x$getStart()<y$getStart()){
                             x<-x$getPeriod(y$getStart(),x$getEnd())
                             
                           }
                           
                           if(x$getEnd()>y$getEnd()){
                             x<-x$getPeriod(x$getStart(),y$getEnd())
                             
                           }
                           
                           if(x$getEnd()<y$getEnd()){
                             y<-y$getPeriod(y$getStart(),x$getEnd())
                             
                           }
                           
                          
                           #print(cor(x$data,y$data,use="pairwise.complete.obs"))
                           #par(mfrow=c(2,1))
                           ##hourly
                           h<-data.frame(x$data,y$data)
                           h1<-h[,c(2,1)]
                           plot(h1[!is.na(h1[,1])&!is.na(h1[,2]),])
                           #matplot(h[2400:2496,],type="l")
                           #plot(x$data,y$data)
                           d<-data.frame(x$getHourly(24)$data,y$getHourly(24)$data)
                           matplot(d,type="l")
                           
                           m<-data.frame(x$getHourly(24*30)$data,y$getHourly(24*30)$data)
                           matplot(m,type="l",ylim=c(0,max(m,na.rm=TRUE)))
                           
                           ####table
                           
                           cc1<-countExtremes(d,0.5)
                           cc2<-countExtremes(d,0.75)
                           cc3<-countExtremes(d,0.25)
                           
                           d1<-deseasonalize(x)
                           d2<-deseasonalize(y)
                           
                           
                             
                           filter<-!is.na(x$data)&!is.na(y$data)
                           filterd<-!is.na(d[,1])&!is.na(d[,2])
                           filterm<-!is.na(m[,1])&!is.na(m[,2])
                           
                           ret<-rbind(NULL,c(mean(x$data[filter],na.rm=TRUE),mean(y$data[filter],na.rm=TRUE),
                                             cor(h,use="pairwise.complete.obs")[1,2],
                                             cor(d,use="pairwise.complete.obs")[1,2],                                             
                                             cor(m,use="pairwise.complete.obs")[1,2],
                                             sum(filter),sum(filterd),sum(filterm),
                                             nrmse(h[,1],h[,2]),
                                             nrmse(d[,1],d[,2]),
                                             nrmse(m[,1],m[,2]),
                                             cc1,cc2,cc3,
                                             cor(d1[[1]],d2[[1]],use="pairwise.complete.obs"),
                                                 d1[[2]],d2[[2]],
                                             cor(x$getHourly(8760)$data,y$getHourly(8760)$data,use="pairwise.complete.obs")
                                             
                                      ))
                           ret<-data.frame(ret)
                           names(ret)<-getcompnames()
                           return(ret)
                           
                         },
                         
                         ###defines the names of the columns for the comparison
                         ###vector, i.e. this function indicates which quantiative criteria
                         ###are calcualted in the compare method
                         getcompnames=function(){
                           
                           return(c("mean x","mean y",
                                    "cor h","cor d","cor m",
                                    "n h","n d","n m",
                                    "nrmse h","nrmse d","nrmse m",
                                    "max ext 50 x","mean ext 50 x",
                                    "max ext 50 y","mean ext 50 y",
                                    "max ext 75 x","mean ext 75 x",
                                    "max ext 75 y","mean ext 75 y",
                                    "max ext 25 x","mean ext 25 x",
                                    "max ext 25 y","mean ext 25 y",
                                    "cor deseas",
                                    "x c1","x c2","x c3",
                                    "y c1","y c2","y c3",
                                    "annual cor"))
                           
                         },
                         
                        
                         ###get a subperiod of the timeseries
                         ###start and end should be POSICct objects in UTC(GMT)
                         ###returns a new TS_Meteo object
                         getPeriod=function(start,end){
                           part<-which(dates>=start&dates<=end)
                           
                           return(TS_Meteo$new(data[part],dates[part],timeres))
                           
                         },
                         
                      
                         
                         ###returns the start date of that timeseries
                         getStart=function(){
                           return(dates[1])
                         },
                         
                         ###returns the end date of that timeseries
                         getEnd=function(){
                           return(dates[length(dates)])
                         },
                         
                         ####deseasonalizes the given object
                         ####is aggregating the data to daily data
                         ####resturns a list that contains a vector with the deseasonalized
                         ####data and, as a second elements, the coefficients of the regression
                         ####used to deseasonalize. Here, a simple sinus-cosinus function is used
                         ####to depict seasons
                         deseasonalize=function(x){
                           dd<-x$getHourly(24)$data
                           
                           t<-1:length(dd)
                           omega=2*pi/(365)
                           lm.sin <- lm(dd~cos(omega*t)+sin(omega*t),na.action=na.exclude)
                           y.sin <- 0*dd+fitted(lm.sin)
                           des<-dd-y.sin
                           return(list(des,lm.sin$coefficients))
                           
                         },
                         
                         ####returns the list of dates
                         getDates=function(){
                           
                           return(dates)
                         }
                       ))





###class used to work with spatial time series
###the class uses as underlying data structure an ff_array
###which stores data on disk and loads part of the data to memory
###only when necessary
###the data is organized in a three dimensional array (for position in lon lat and timeseries)
SpatialMetData <- setRefClass("ECMWF",
                              
                             
                     fields = list(data = "ff_array",
                                   lonlat="data.frame",
                                   dates="POSIXct",
                                   map="data.frame",
                                   timeres="numeric"),
                     
                     methods = list(
                       
                       
                       ####constructor
                       ####data1 is the 3-dimensional array containing all data
                       ####lonlat1 is a list of all locations in the dataset,
                       ####as lon lat coordinates
                       ####dates1 is a vector with the dates for the timeseries (i.e. all timeseries for all points have to be equally long) 
                       ####timeres1 indicates the time resolution of the timeseries (e.g. 3 means that there is a value every 3 hours)
                       initialize = function(data1,lonlat1,dates1,timeres1){
                         
                         ##set data
                         data<<-data1
                         dates<<-dates1
                         lonlat<<-lonlat1
                         timeres<<-timeres1
                         index<-expand.grid(1:dim(data)[2],1:dim(data)[1])
                         index<-index[,c(2,1)]
                         map<<-data.frame(lonlat,index)
                         names(map)<<-c("lon","lat","ind1","ind2")
                         
                       },
                       
                       
                       ####get a particular timeseries associated with a longitude and latitude.
                       ####lon and lat have to _exactly_ match a longitude and latitude in the dataset
                       getatpos = function(lon,lat) {
                         a<-map[map[,1]==lon&map[,2]==lat,] 
                         if(nrow(a)>0){
                           dat<-unlist(data[a[,3],a[,4],])
                           dat[dat<0]<-0
                           dat<-TS_Meteo$new(dat,dates,timeres)
                           return(dat) 
                         }else{
                           return (NULL)
                         }
                       },
                       
                       ####get the timeseries at the location closest to lon lat
                       minDistanceTS = function(lon,lat){
                         d<-rdist.earth(lonlat,matrix(c(lon,lat),nrow=1)) 
                         l<-lonlat[which(min(d)==d[,1]),]
                         l<-unlist(l)
                         ts<-getatpos(l[1],l[2])
                         return(ts)
                       },
                       
                       ####get the minimium distance between the given lon lat
                       ####and a point in the dataset
                       getMinDistance = function(lon,lat){
                         d<-rdist.earth(lonlat,matrix(c(lon,lat),nrow=1)) 
                       
                         return(min(d))
                         
                         
                         
                       },
                       
                       ####return the dates object
                       getTime = function() {
                         
                         return(dates)
                       },
                       
                       ####for the given locations
                       ####which has to be a data.frame of lons and lats
                       ####this function returns a new timeseries
                       ####which aggregates the point, weighting every point equally
                       ####i.e. if x1(t) is the timeseries at the first point and x2(t)
                       ####is the timeseries at the second point,                      
                       ####the method returns (x1(t)+x2(t))/2
                       retSample=function(locations){
                          d<-rdist.earth(lonlat,locations) 
                          d<-data.frame(d)
                          locs<-mapply(which_column,d,apply(d,2,min))                         
                          locs<-unlist(locs)
                          lon<-lonlat[locs,1]
                          lat<-lonlat[locs,2]
                          #print(lon)
                          #print(lat)
                          #points(lon-360,lat,col="red")
                          df<-data.frame(lon,lat)
                          names(df)<-c("lon","lat")
                          
                          mm<-merge(map,df,by.x=c("lon","lat"),by.y=c("lon","lat"))
                          
                          
                          ts<-mergeAlg(mm)
                          return(ts)
                         },
                       
                       ####merges a three dimensional array
                       ####to one single timeseries, equally weighting all elements
                       mergeAlg=function(mm){
                         
                         c_fin<-rep(0,length(data[1,1,]))  
                         for(kk in 1:nrow(mm)){
                           #lines(data[mm[kk,3],mm[kk,4],140:280],col="darkblue")  
                           c_fin<-c_fin+data[mm[kk,3],mm[kk,4],]
                         }
                          c_fin<-c_fin/nrow(mm)
                          #lines(c_fin[140:280],lwd=2,col="blue")
                          return(TS_Meteo$new(c_fin,dates,timeres))
                         
                         
                       }
                       
                      
                       
                       
                       
                       
                       
                       
                       
                     )
)



####INMET class
####Is a container for data derived from INMET, the Brazilian meteorological office
INMET <- setRefClass("INMET",
                     fields = list(data = "data.frame",lonlat="data.frame",dates="POSIXct",timeres="numeric"),
                     #map="data.frame",
                     
                     methods = list(
                       
                       
                       ####constructor
                       ####data is a two-dimensional array with the data
                       ####lonlat1 is a data.frame with the corresponding lons and lats
                       ####dates1 is a vector with the dates
                       ####and timeres1 is the usual time resolution
                       initialize = function(data1,lonlat1,dates1,timeres1){
                         
                         ##set data
                         data<<-data1
                         dates<<-dates1
                         lonlat<<-lonlat1
                         timeres<<-timeres1
                         #index<-expand.grid(1:dim(data1)[2],1:dim(data1)[1])
                         #index<-index[,c(2,1)]
                         #map<<-data.frame(lonlat,index)
                         
                       },
                       
                       
                    
                       ####get the timeseries at the given index i
                       getIndex = function(i){
                         
                         ts<-TS_Meteo$new(unlist(data[,i]),dates,timeres)
                         
                         return(ts)
                       },
                       
                       ####get the lon lat at the given index
                       getIndexLonLat = function(i){
                         
                         return(unlist(lonlat[i,]))
                         
                       },
                       ####returns the dates vector
                       getTime = function() {
                         
                         return(dates)
                       },
                       
                       
                       ####tells how many stations are in the dataset
                       getSize = function() {
                         
                         return(ncol(data))
                       },
                       
                       ####derive a timeseries which is the result of
                       ####averaging over n random columns in the original data set
                       ####i.e. the result is the geographic average over n random locations 
                       getSample = function(n) {
                         
                         locations<-sample(1:ncol(data), n)
                         #14,28
                         #locations<-c(30,77)
                         ag<-aggregate(data[421:840,locations],by=list(rep(141:280,each=3)),sum)
                         #print(ag)
                         #matplot(ag[,2:ncol(ag)],type="l")
                         ts<-apply(data[,locations],1,mean)
                         
                         ssumnna<-sum(!is.na(ts))                         
                         tts<-TS_Meteo$new(ts,dates,timeres)
                         ag<-aggregate(ts[421:840],by=list(rep(141:280,each=3)),sum)
                         
                         #lines(ag[,2],col="red",lwd=2)
                         l<-list(lonlat[locations,],tts,ssumnna)
                         return(l)
                         
                       }
                       
                      
                     )
)


