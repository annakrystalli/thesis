    #Set up
    rm(list = ls())
    
    #Packages
    require(spatstat)
    require(png)
    require(tiff)
    
    #Files
#Load overlap dataframe names
    load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/Overlap dataframe names.RData")
    
    
    
#___________________________________________________________________________________________________    
                  #...process CPR - NOT REQUIRED IF LOADING FILE  
                  
                      cpr.filename<-"/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/SeaWiFS CPR ordinal.csv"
                    #Clean CPR data
                      cpr.dat<-cleanCPR(cpr.filename)
                    #Geolocate full tow vectors
                      cpr.dat<-towGeolocate(cpr.spl=cpr.dat)
                    
                    #Save processed CPR data
                      save(cpr.dat, file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/CPR extra clean & tow geoloc.RData")
                     

    #Load processed CPR
    load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/CPR extra clean & tow geoloc.RData")
    
#______________________________________________________________________________________________________
#...Pixellate CPR dataset

    
  #...generate empty dataframe to collate tow pixellation
        empty.overlap.dat<-empty.df(names(cpr.dat))
    
        
                        #...produce geo.matrix - NOT REQUIRED IF LOADING FILE
                          geo.matrix<-geolocate(
                            readPNG("/Users/annakrystalli/Documents/SATELLITE/monthly/avhrr_sst/images/M1997001-1997031.uk.sstp.AVH.L3_median.01jan97-31jan97.v1.20122501533.rsg_grey.png"), 
                            matrix=TRUE)
    
                          ypix<-geo.matrix[,1,2]
                          ypix[1:1111]<-ypix[1:1111]+diff(ypix)/2
                          ypix[1112]<-ypix[1111]-(ypix[1111]-ypix[1112])*2
                          ypix<-sort(ypix)
    
                          xpix<-geo.matrix[1,,1]
                          xpix[1:925]<-xpix[1:925]-diff(xpix)/2
                          xpix[926]<-xpix[925]+(xpix[926]-xpix[925])*2/3
    
                          save(geo.matrix,ypix,xpix,
                               file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
    
    
       #Load geo.matrix
        load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
        
        #Define area
        area<-owin(xrange=c(1,927), yrange=c(1,1113), mask=matrix(TRUE, 1112,926), xy=list(x=1:926, y=1:1112))
        
        
    
    #Define extent of processing    
        start<-1
        end<-length(cpr.dat[,1])
    
        #Loop pixellate function through rows
        
    
        for (i in start:end){
          
            if(i==start){cpr.overlap.dat<-empty.overlap.dat
                         cpr.overlap.dat$tmp.w<-paste(cpr.overlap.dat$year, cpr.overlap.dat$week, sep="-")
                         cpr.overlap.dat$tmp.m<-paste(cpr.overlap.dat$year, cpr.overlap.dat$month, sep="-")}
            tow<-cpr.dat[i,]
            
            try(tow.dat<-pixellateTow(tow=tow, area=area, ypix=ypix, xpix=xpix), silent=TRUE)
            
            if(testObject(tow.dat)==TRUE){cpr.overlap.dat<-rbind(cpr.overlap.dat,tow.dat)}else{next}
            
            
            rm(tow.dat)
          
            
        }
        
        cpr.overlap.dat$r.ind<-as.numeric(sapply(strsplit(cpr.overlap.dat$loc.index,"-"), "[", 1))
        cpr.overlap.dat$c.ind<-as.numeric(sapply(strsplit(cpr.overlap.dat$loc.index,"-"), "[", 2))
        cpr.overlap.dat$w.index<-paste(cpr.overlap.dat$w.index,"-", sep="")
        cpr.overlap.dat$m.index<-paste(cpr.overlap.dat$m.index,"-", sep="")
    
        save(cpr.overlap.dat, file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/master cpr data.RData")
        
 