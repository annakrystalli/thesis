#Set up
rm(list = ls())

#Packages
require(spatstat)
require(png)
require(tiff)
require(stringr)
require(maps)

#Files
#Load overlap dataframe names
load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/Overlap dataframe names.RData")
data.index<-read.csv("/Users/annakrystalli/Documents/SATELLITE/data type index.csv")
#___________________________________________________________________________________________________ 


x<-cpr.dat[(cpr.dat$lat>58.5) & (cpr.dat$lon>-3.0569099) & (cpr.dat$lon < -0.5849861) & (cpr.dat$year == 2000) & (cpr.dat$month == 5),]




  
  map("world", xlim=c(-3.4,0.2), ylim=c(58.5,61), fill=TRUE, resolution=0, 
      mar = c(4, 3, par("mar")[3], 1), las=1, font=2, ylab="latitude", 
      xlab="longitude")

  lines(c(x$lon, x$lon1[5]), c(x$lat, x$lat1[5]), lty=3)
arrows(x0=x$lon, y0=x$lat, x1=x$lon1, y1=x$lat1, lwd=2, 
       col=c(rep(grey(0.55), 3), "black", "black"), angle=90, length=0.07)
arrows(x0=x$lon, y0=x$lat, x1=x$lon2, y1=x$lat2, lwd=2, angle=90, length=0.07,
       col=c(rep(grey(0.55), 4), "black"))
points(x$lon,x$lat, cex=1, bg=c(rep(grey(0.55), 3), "black", "black"), pch=21,
       col=c(rep(grey(0.55), 3), "black", "black"))
abline(h=x$lat[4], lty=2)

box(lwd=3)
axis(1, font=2, at=-3:0, labels=-3:0)
axis(2, las=1, font=2, at=59:61)

#___________________________________________________________________________________________________    
#...process CPR - NOT REQUIRED IF LOADING FILE  

cpr.filename<-"/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/SeaWiFS CPR ordinal.csv"
#Clean CPR data
cpr.dat<-cleanCPR(cpr.filename)

#Geolocate full tow vectors
cpr.dat<-towGeolocate(cpr.spl=cpr.dat)

#Save processed CPR data
save(cpr.dat, file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/CPR extra clean & tow geoloc.RData")

#______________________________________________________________________________________________________________
#...clean CPR data and date locate

cleanCPR<-function(cpr.filename){
  cpr.dat<-read.csv(file=cpr.filename, header=TRUE)
  
  cpr.dat$date<-as.character(cpr.dat$date)
  
  #Correct for missing zeros in front of date
  dlength<-which(with(cpr.dat, nchar(as.character(date)))==7)
  cpr.dat$date[dlength]<-paste("0", cpr.dat$date[dlength], sep="")
  
  #Determine day and week of year
  cpr.dat$day<-strptime(as.character(cpr.dat$date) , "%d%m%Y")$yday+1
  cpr.dat$week<-ceiling(cpr.dat$day/8)
  
  #Get rid of NAs
  for(i in 10:14){
    cpr.dat[which(is.na(cpr.dat[,i])==TRUE),i]<-0   }
  
  return(cpr.dat)
  
}

#___________________________________________________________________________________________________    


towGeolocate<-function(cpr.spl){
  
  #Order dataset
  cpr.spl<-cpr.spl[order(cpr.spl$tow,cpr.spl$sample.no.),]
  
  #1/12 of a degree = 5 nautical miles = 1/2 a CPR spl
  cpr.length<-1/12*(pi/180)
  n<-length(cpr.spl$tow)
  cpr.spl$tow<-as.character(cpr.spl$tow)
  
  #Convert lat & lon to radians for enpoint calculations
  latr<-cpr.spl$lat*(pi/180)
  lonr<-cpr.spl$lon*(pi/180)
  
  require(argosfilter)
  
  #Use argosfilter package to calculate bearings (supply degrees & convert result to radians)
  theta1<-bearingTrack(cpr.spl$lat, cpr.spl$lon)*pi/180
  theta1<-c(theta1, theta1[length(theta1)])
  theta2<-rev(bearingTrack(rev(cpr.spl$lat), rev(cpr.spl$lon))*pi/180)
  theta2<-c(theta2[length(theta2)], theta2)
  
  #Assign spl slopes to ends of tows
  towplus<-c( cpr.spl$tow[-1], "x")
  towminus<-c("x", cpr.spl$tow[-n])
  
  theta1[which(cpr.spl$tow!=towplus)]<-((theta2[which(cpr.spl$tow!=towplus)])
                                        -sign(theta2[which(cpr.spl$tow!=towplus)]-pi)*pi)
  theta2[which(cpr.spl$tow!=towminus)]<-((theta1[which(cpr.spl$tow!=towminus)])
                                         -sign(theta1[which(cpr.spl$tow!=towminus)]-pi)*pi)
  
  
  
  #Calculate endpoints of tows (Distance (cpr.length) here is in radians)
  
  lat1<-(asin(sin(latr)*cos(cpr.length)+
                cos(latr)*sin(cpr.length)*cos(theta1)))*180/pi
  
  lon1<-(lonr + 
           atan2( sin(theta1) * sin(cpr.length)*cos(latr),
                  cos(cpr.length)-sin(latr)*sin(lat1*pi/180)))*180/pi
  
  lat2<-(asin(sin(latr)*cos(cpr.length)+
                cos(latr)*sin(cpr.length)*cos(theta2)))*180/pi
  
  lon2<-(lonr + 
           atan2( sin(theta2) * sin(cpr.length)*cos(latr),
                  cos(cpr.length)-sin(latr)*sin(lat2*pi/180)))*180/pi
  
  
  
  #Compile results
  cpr.spl<-cbind(cpr.spl, lat1,lon1, lat2, lon2)
  
  return(cpr.spl)
}

#____________________________________________________________________________________________________
#...Use spatstat to pixellate tow returning pixel matrix index, geolocation and % overlap of tow on pixel

pixellateTow<-function(tow, area, ypix, xpix){
  
  
  
  lon<-lonToPix(tow$lon,xpix)
  lon1<-lonToPix(tow$lon1,xpix)
  lon2<-lonToPix(tow$lon2,xpix)
  
  lat<-1114-latToPix(tow$lat,ypix)
  lat1<-1114-latToPix(tow$lat1,ypix)
  lat2<-1114-latToPix(tow$lat2,ypix)  
  
  tow.psp<-psp(c(lon,lon), c(lat,lat), 
               c(lon1,lon2), c(lat1,lat2), window=area)
  
  tow.img<-as.mask.psp(tow.psp, W=area)
  tow.img<-pixellate.psp(tow.psp, W=area)
  
  tow.matrix<-as.matrix(tow.img)
  
  tow.inds<-which(tow.matrix>0, arr.ind=TRUE)
  tow.coods<-geo.matrix[tow.inds[,1],tow.inds[,2],]
  tow.overlaps<-data.frame(cbind(diag(tow.coods[,,1]),diag(tow.coods[,,2]), tow.matrix[tow.inds]))
  names(tow.overlaps)<-c("lon", "lat","overlap")
  
  
  tow.dat<-cbind(tow.overlaps, tow[1:2],tow[6:16])
  
  tow.dat$loc.index<-paste(tow.inds[,1],tow.inds[,2], sep="-")
  tow.dat$w.index<-paste(tow.dat$year, tow.dat$week, sep="-")
  tow.dat$m.index<-paste(tow.dat$year, tow.dat$month, sep="-")
  
  return(tow.dat)
}


latToPix<-function(lat, ypix){if(min(ypix)<=lat && lat<61.001){
  y<-max(which(ypix<=lat))
  y<-y+((lat-ypix[y])/(ypix[y+1]-ypix[y]))}else{y<-0}
  return(y)}
lonToPix<-function(lon, xpix){if(min(xpix)<=lon && lon<11.006){
  x<-max(which(xpix<=lon))
  x<-x+((lon-xpix[x])/(xpix[x+1]-xpix[x]))}else{x<-0}
  return(x)}
#____________________________________________________________________________________________________
