#FUNCTIONS SHEET

#____________________________________________________________________________________________
#....Geolocate image pixels given area covered by image

geolocate<-function(X, minlon=-4, maxlon=10.996577398985, minlat=51, maxlat=61.000898928276, matrix=FALSE){
  
  rows<-nrow(X)
  cols<-ncol(X)
  
  require(aspace)
  
  #Calculate longitude
  lon.in <- matrix(rep(0:(cols-1), times=rows), nrow=rows, ncol=cols, byrow=TRUE)
  lonfract <- lon.in / (cols - 1)
  lon <- minlon + (lonfract * (maxlon - minlon))

  
  #Calculate latitude
  lat.in <- matrix(rep(0:(rows-1), times=cols), nrow=rows, ncol=cols)
  latfract = 1.0 - (lat.in / (rows - 1))
  
  
  
  Ymin = log (tan (as_radians (45.0 + (minlat / 2.0))))
  Ymax = log (tan (as_radians (45.0 + (maxlat / 2.0))))
  Yint = Ymin + (latfract * (Ymax - Ymin))
  
  lat = 2.0 * (atan (exp (Yint))*(180/pi) - 45.0)
  
  if (matrix==TRUE) {geo <- array(c(lon,lat),dim=c(rows,cols,2))}
  else {geo <- list(lon = lon, lat = lat)}
  
  
 
  
  return(geo)
}


#________________________________________________________________________________________________
# Function sets working directory for easy file retrival and extracts filenames, data.type and temporal index

batchOpen<-function(dir){
  
           setwd(dir) 
           
           data.index<-read.csv("/Users/annakrystalli/Documents/SATELLITE/data type index.csv")
          
           
           #load filenames
           filenames<-list.files(path=paste(dir,"images", sep="/"), all.files = FALSE)
           
           #prelims
           file.txt<-unlist(strsplit(dir,"/"))
           file.length<-length(file.txt)
           
           #determine map & data type
           map.type<-file.txt[file.length]
           data.type<-data.index[match(map.type,data.index[,1]),2]
           
           #determine output folder name
           file.txt[file.length]<-paste("PROCESSED/",map.type,"/", sep="")
           output.file=paste(file.txt, collapse="/")
           
           if (length(grep("weekly", dir))==1){temp<-"W"}else{temp<-"M"} 
           
           
           return(list(filenames=filenames, map.type=map.type, data.type=data.type, temp=temp,output.file=output.file))
}

#__________________________________________________________________________________________________
#...takes geolocated image, unrolls variable and geo data into dataframe, appends matrix location indices

unrollMap<-function(img ,geo){
  lat<-as.vector(geo$lat)
  lon<-as.vector(geo$lon)
  var<-as.vector(img)
  col.ind<-as.vector(matrix(1:ncol(img), length(img[,1]),ncol(img), byrow=TRUE))
  row.ind<-as.vector(matrix(1:nrow(img), length(img[1,]),nrow(img)))
  
  data.unrol<-data.frame(cbind(var,lat,lon, col.ind, row.ind ))
  
  return(data.unrol)
}


#____________________________________________________________________________________________________
#...Processes image to dataframe and saves as RData. To be used in prediction

imgProcess<-function(file.name, data.type, temp){
  
  load(file="/Users/annakrystalli/Documents/SATELLITE/month.index.RData")
  
  #Read in image#Read in image & flip matrix
  
  #...PNG
  if(length(grep(".png", file.name))==1){img<-readPNG(paste("images/",file.name, sep=""))
                                         if(length(grep("fcomp_", dir))==1){img<-img[,,1]}
                                         img<-img*255                                       
  }
  
  #...TIFF
  if(length(grep(".tif", file.name))==1){img<-readTIFF(paste("images/",file.name, sep=""), indexed=TRUE)
                                         if(length(grep("fcomp_", dir))==1){img<-img[,,1]}
                                         img<-img-1                                       
  }
  
  
  #Exctract day info  
  day.info<-gsub("^.|-.*$", "",file.name)
  
  #if weekly determine week
  if(temp=="W"){week<-((as.numeric(gsub("^....", "",day.info ))+7)/8)}
  year<-as.numeric(str_extract(day.info, "^...."))
  
  #Determine month
  if(length(grep("f", data.type))==1){
    month<-month.index[month.index[,2]==gsub("^.*fcomp...|..-.*$", "",file.name),1]}else{
      month<-month.index[month.index[,2]==gsub("^.*median...|..-.*$", "",file.name),1]}
  
  geo<-geolocate(img)
  
  data.unrol<-unrollMap(img, geo)
  data.unrol$pix.ind<-paste(data.unrol$row.ind, data.unrol$col.ind, sep="-")
  
  #Remove land and overcast pixels
  cln.satdat<-data.unrol[data.unrol$var!=1,]
  cln.satdat<-cln.satdat[cln.satdat$var!=0,]
  
  #Bind data and create spatio-temp index
  if(temp=="W"){cln.satdat<-cbind(cln.satdat,week=week, year=year, month=month);
                cln.satdat$match.index<-paste(cln.satdat$pix.ind, cln.satdat$year, cln.satdat$week, , sep="-");
                save(cln.satdat, file=paste( folder.info$output.file, year, "-", week,"-",file.name, ".RData", sep="" ))
                
  }else{cln.satdat<-cbind(cln.satdat, year=year, month=month);
        cln.satdat$match.index<-paste(cln.satdat$pix.ind, cln.satdat$year, cln.satdat$month,  sep="-");
        save(cln.satdat, file=paste(folder.info$output.file, "images/", year, "-", month,"-",file.name, ".RData", sep="" ))
  }
  
  
  
}


#________________________________________________________________________________________________
# Function renames sat data files with temp index and saves as Rdata matrices in subfolder "/r files/'

batchFolderRename<-function(dir){
  
  setwd(dir) 
  
  load(file="/Users/annakrystalli/Documents/SATELLITE/month.index.RData")
  
  #load filenames  
  filenames<-list.files(path=paste(dir,"images", sep="/"), all.files = FALSE)
  
  
  if (length(grep("weekly", dir))==1){temp<-"W"}else{temp<-"M"} 
  
  for (i in 1:length(filenames)){
    
    file.name<-filenames[i]    
    
    
    
    #Read in image#Read in image & flip matrix
    
    #...PNG
    if(length(grep(".png", file.name))==1){img<-readPNG(paste("images/",file.name, sep=""))
                                           if(length(grep("fcomp_", dir))==1){img<-img[,,1]}
                                           img<-img*255                                       
    }
    
    #...TIFF
    if(length(grep(".tif", file.name))==1){img<-readTIFF(paste("images/",file.name, sep=""), indexed=TRUE)
                                           if(length(grep("fcomp_", dir))==1){img<-img[,,1]}
                                           img<-img-1                                       
    }
    
    
    
    #Exctract day info  
    day.info<-gsub("^.|-.*$", "",file.name)
    
    #if weekly determine week
    if(temp=="W"){week<-floor((as.numeric(gsub("^....", "",day.info ))+7)/8)
                  t.ind<-week}else{
                    #Determine month
                    if(length(grep("fronts", dir))==1){
                      month<-month.index[month.index[,2]==gsub("^.*fcomp...|..-.*$", "",file.name),1]}else{
                              if(length(grep("fcomp", dir))==1){
                                month<-month.index[month.index[,2]==gsub("^.*fcomp...|..-.*$", "",file.name),1]}else{
                                          month<-month.index[month.index[,2]==gsub("^.*median...|..-.*$", "",file.name),1]}}
                    t.ind<-month
                  }
    
    year<-as.numeric(str_extract(day.info, "^...."))
    
    save(img, file=paste(dir,"/r files/",year,"-",t.ind,"-",file.name,".RData", sep=""))
  }
}


#______________________________________________________________________________________________________________
#CPR FUNCTIONS

#______________________________________________________________________________________________________________
#...clean CPR data and date locate
#cpr.filename<-"/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/SeaWiFS CPR ordinal.csv"

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
    
  
  #________________________________________________________________________________________________
  #...function uses tow centre locations to determine trajectory of tows and determine geo endpoints
  #...for each tow

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
#Test whether an object exists
testObject <- function(object)
{
  exists(as.character(substitute(object)))
}


#__________________________________________________________________________________________________
#...creates var (variable) column in master training dataset (X)

createVarCol<-function(X, s, match.folder.info){
  var<-matrix(NA,nrow=length(X[,1]), ncol=1 )
  var<-data.frame(var)
  
  if(match.folder.info$temp=="W"){names(var)<-paste(match.folder.info$data.type,".w",sep="")}else{
                                  names(var)<-paste(match.folder.info$data.type,".m",sep="")
                                  }
                    
  X<-cbind(X, var)
  return(X)}



#_____________________________________________________________________________________________________
#...creates empty data.frames from headers


empty.df<- function(header){
  df<-data.frame(matrix(matrix(rep(1,length(header)),1),1))
  colnames(df)<-header
  return(df[NULL,])}


#___________________________________________________________________________________________________
#...Match Satellite folder info (info required for matching procedure)

matchFolderInfo<-function(dir){
  
      setwd(dir) 
      
      data.index<-read.csv("/Users/annakrystalli/Documents/SATELLITE/data type index.csv")
      
      
      #load filenames
        filenames<-list.files(path=paste(dir,"r files", sep="/"), all.files = FALSE)
      
      #prelims
        file.txt<-unlist(strsplit(dir,"/"))
        file.length<-length(file.txt)
      
      #determine map & data type
        map.type<-file.txt[file.length]
        data.type<-data.index[match(map.type,data.index[,1]),2]
      
      #...set temporal resolution {temp}
      if (length(grep("weekly", dir))==1){temp<-"W"}else{temp<-"M"} 
      
      #...compile and return folder info {match.folder.info} (set it on call)
      return(list(filenames=filenames, map.type=map.type, data.type=data.type, temp=temp))
}


#_________________________________________________________________________________________________
  #...Match temporally match each sat map to relevant CPR pixels set to creat training data set

matchTemptoPixels<-function(cpr.data, temp.index, temp.id, match.folder.info, match.id){

  
  #Test whether sat image with temporal match exists - otherwise "NTP"
  if (length(
    try(
      sat.file<-match.folder.info$filenames[grep(temp.index, match.folder.info$filenames,
                                                 value=FALSE)] , 
      silent=TRUE)
  )!=0){load(paste(dir,"/r files/",sat.file, sep=""))}else{cpr.data[match.id,var.col]<-"NTP"} 
  
  
  #If sat.file created find space match. 1=land, 0=cloud, need to remove at end   
  if (testObject(sat.file)==TRUE){
   
    s<-img
    
    #...extract var values
    match.res<-diag(s[cpr.data$r.ind[match.id],cpr.data$c.ind[match.id]])
    
    #...remove sat.file    
    rm(sat.file)
 
    
  }

  match.res 
        
}

