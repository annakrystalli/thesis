#...Change map.res

map.res<-function(img, in.res, out.res ){
  X<-img
  
  x<-dim(X)[1]
  y<-dim(X)[2]
  
  
  s<-(in.res/out.res)
  rX<-matrix(0, x*s, y*s)
  
  rx<-dim(rX)[1]
  ry<-dim(rX)[2]
  
  i.in<-rep(1:x, each=s)
  j.in<-rep(1:y, each=s)
  
  for (k in 1:rx){
    for (l in 1:ry){
      
      i<-i.in[k]
      j<-j.in[l]
      
      rX[k,l]<-X[i,j]
      
      
      
      
    }
  }
  return(rX)      
  
}


#___________________________________________________________________________________________________
#Extract landmask to clean images
borderExtract<-function(file.name, change.res=FALSE,...){
  
  img<-readPNG(file.name)
  #prelims
  data.index<-read.csv("/Users/annakrystalli/Documents/SATELLITE/data type index.csv")
  
  #determine map & data type
  file.txt<-unlist(strsplit(file.name,"/"))
  data.type<-data.index[match(file.txt[length(file.txt)-1],data.index[,1]),2]
  
  
  if(change.res==TRUE) {img<-map.res(img, 4,1)}
  
  geo<-geolocate(img)
  
  data.unrol<-unrollMap(img, geo)
  
  
  border<-data.unrol[data.unrol[,1]==0,]
  border.index<-which(img==0,arr.ind=TRUE)
  border<-cbind(border,border.index)
  
  land<-data.unrol[data.unrol==img[384,5],]
  land.index<-which(img==img[384,5],arr.ind=TRUE)
  land<-cbind(land,land.index)
  
  white<-data.unrol[data.unrol[,1]==1,]
  white.index<-which(img==1,arr.ind=TRUE)
  white<-cbind(white,white.index)
  
  map.features<-list(border=border, land=land, white=white)
  save(map.features, file=paste(dir,"/r files/",data.type, " map features.RData", sep=""))
  
  clean.index<-(rbind(border[,1:3], land[,1:3], white[,1:3] ))
  clean.index$pix.ind<-paste(clean.index$lat, clean.index$lon, sep="-")
  
  save(clean.index, file=paste(dir,"/r files/",data.type, " clean index.RData", sep=""))
  
  setwd("/Users/annakrystalli/")
  
  return(clean.index)
}










#____________________________________________________________________________________________________
imgClean<-function(file.name, clean.index, change.res=FALSE, data.type){
  
  #Read in image
  img<-readPNG(file.name)
  
  #Exctract week info  
  file.info<-gsub("^.*[M]|-.*$", "",file.name)
  month<-((as.numeric(gsub("^....", "",file.info ))+7)/8)
  year<-as.numeric(str_extract(file.info, "^...."))
  
  
  if(change.res==TRUE) {img<-map.res(img, 4,1)}
  
  geo<-geolocate(img)
  
  data.unrol<-unrollMap(img, geo)
  data.unrol$pix.ind<-paste(data.unrol$lat, data.unrol$lon, sep="-")
  
  
  
  #Remove map features
  cln.dat<-data.unrol[data.unrol$pix.ind %in% clean.index$pix.ind==FALSE,]  
  
  #Remove NAs  
  load(file=paste(dir, "/r files/",data.type, " blank.RData", sep=""))
  cln.dat<-cln.dat[cln.dat[,1]!=blank,]
  
  cln.dat<-cbind(cln.dat,week=month, year=year)
  cln.dat$match.index<-paste(cln.dat$pix.index, cln.dat$year, cln.dat$month, , sep="-")
  
  return(cln.dat)
}








#_____________________________________________________________________________________________________

#TEMP (select an exaple layer from rgb)
selectLayer<-function(img=img,la=1){
  img<-matrix(as.numeric(paste(img[,,la])),nrow(img),ncol(img))
}

#___________________________________________________________________________________________________




pixellateDataset<-function(data=cpr.dat, xrange=c(-4,11), yrange=c(51,61), res=matrix(TRUE, 1112,926), 
                           start=1, end=end){
  
  
  #Load and assign empty data.frame
  load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/Overlap empty dataframe.RData")
  
  load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
  
  #Define area
  area<-owin(xrange=xrange, yrange=yrange, mask=res)
  
  
  #Loop pixellate function through rows
  
  for (i in start:end){
    
    if(i==start){output<-empty.overlap.dat}
    tow<-data[i,]
    
    try(tow.dat<-pixellateTow(tow=tow, area=area), silent=TRUE)
    
    if(testObject(tow.dat)==TRUE){output<-rbind(output,tow.dat)}else{next}
    
    
    rm(tow.dat)
    
    
    
    
  }
  
  
  return(output)
  
}


#___________________________________________________________________________________________________
fixPixIndex<-function(x){
  a<-strsplit(x, "--")
  l<-lapply(a,as.numeric)
  l<-lapply(l, round, digits=5)
  lx<-paste(sapply(l,"[", 1), sapply(l,"[", 2), sep="-")
  return(lx)}


fixLocIndex<-function(x){
  a<-strsplit(x, "-")
  l<-lapply(a,as.numeric)
  l<-lapply(l, round, digits=10)
  lx<-paste(sapply(l,"[", 1), sapply(l,"[", 2), sep="-")
  return(lx)}


towGeolocate<-function(cpr.spl){
  
  cpr.spl<-cpr.spl[order(cpr.spl$tow,cpr.spl$sample.no.),]
  
  #1/12 of a degree = 5 nautical miles = 1/2 a CPR spl
  cpr.length<-1/12
  n<-length(cpr.spl$tow)
  cpr.spl$tow<-as.character(cpr.spl$tow)
  
  slope1 = diff(cpr.spl$lat) / diff(cpr.spl$lon)
  slope2 = c( slope1[1], slope1)
  slope1 = c( slope1, slope1[length(slope1)])
  
  towplus<-c( cpr.spl$tow[-1], "x")
  towminus<-c("x", cpr.spl$tow[-n])
  
  slope1[which(cpr.spl$tow!=towplus)]<-slope2[which(cpr.spl$tow!=towplus)]
  slope2[which(cpr.spl$tow!=towminus)]<-slope1[which(cpr.spl$tow!=towminus)]
  
  
  dir.f<-diff(cpr.spl$lon)
  
  dir.f = c( dir.f, dir.f[length(dir.f)])
  
  dir.f[dir.f<0]<--1
  dir.f[dir.f>0]<-1
  dir.b<--dir.f
  
  #Distance (cpr.length)here is in radians
  lat1<-(cpr.spl$lat+(cpr.length*sin(atan(slope1)))*dir.f)
  lon1<-(cpr.spl$lon+(cpr.length*cos(atan(slope1)))*dir.f)
  
  lat2<-(cpr.spl$lat+(cpr.length*sin(atan(slope2)))*dir.b)
  lon2<-(cpr.spl$lon+(cpr.length*cos(atan(slope2)))*dir.b)
  
  
  cpr.spl<-cbind(cpr.spl, lat1,lon1, lat2, lon2)
  
  return(cpr.spl)
}




#...set tem.label for searching
if(match.folder.info$temp=="W"){ tem.label<-".w"}else{tem.label<-".m"}

#Determine number of CPR spls to be matched
n<-length(X[,1])

#
var.col<-which((colnames(X)==paste(match.folder.info$data.type, tem.label, sep=""))==TRUE)

pmt<- proc.time()
#___LOOP down CPR dataset            
for (i in 1:n){
  
  #Assign CPR spl to be matched
  Xi<-X[i,]
  
  #Test whether temporal match exists - otherwise "NTP"
  if (length(
    try(
      if (match.folder.info$temp=="W"){sat.file<-match.folder.info$filenames[grep(Xi$w.index, 
                                                                                  match.folder.info$filenames, 
                                                                                  value=FALSE)]}else{
                                                                                    sat.file<-match.folder.info$filenames[grep(Xi$m.index, match.folder.info$filenames, value=FALSE)]} , 
      silent=TRUE)
  )!=0){}else{
    X[i,var.col]<-"NTP"  
  }
  
  #If sat.file created attempt to find space match. No match results NULL and NA in column       
  try(
    if (testObject(sat.file)==TRUE){
      load(paste(dir,"/r files/",sat.file, sep=""))
      s<-img
      
      #...extract var value
      Xvar<-s[Xi$r.ind,Xi$c.ind]
      
      #...assign to training dataset    
      X[i,var.col]<-Xvar
      
      
      #...remove sat.file    
      rm(sat.file)
      
    }
    ,silent=TRUE)
  
  print(i)
  
}


proc.time()  - pmt






match.res<-matchTemptoPixels(cpr.data=X, 
                             temp.index=temp.index.i, 
                             temp.id=temp.id, 
                             match.folder.info=match.folder.info, 
                             match.id=match.id)