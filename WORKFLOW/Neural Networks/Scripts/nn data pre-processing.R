#Set up
rm(list = ls())

#Load latest trainind dataset
load(file="/Users/annakrystalli/Documents/TRAINING DATA/Latest training save.RData")
   
    X.clean<-X

#...Set all unusable satellite values (cloud, NTP, land) to NA

    #Clean NEODAAS var metrics
        clean.index<-X.clean[,c(1,3,6,13,15,18)+21]
        clean.index[clean.index==0]<-NA
        clean.index[clean.index==255]<-NA  
        clean.index[clean.index=="NTP"]<-NA
        X.clean[,c(1,3,6,13,15,18)+21]<-clean.index

    
    
    
    #Clean NEODAAS front metrics
        clean.index<-X.clean[,c(2,4,5,14,16,17)+21]
        clean.index[clean.index==254]<-NA
        clean.index[clean.index==255]<-NA
        clean.index[clean.index=="NTP"]<-NA
        X.clean[,c(2,4,5,14,16,17)+21]<-clean.index

    
    #Clean PM front metrics
        clean.index<-X.clean[,c(7:12, 19:24)+21]
        clean.index[clean.index==0]<-NA
        clean.index[clean.index=="NTP"]<-NA
        X.clean[,c(7:12, 19:24)+21]<-clean.index
        
        
        X.clean[ X.clean=="NTP"]<-NA


    #Separate into weekly and monthly training data sets
        X.clean.m<-X.clean[,c(1:21, seq(22,44, by=2), 46:48)]
        X.clean.w<-X.clean[,c(1:21, seq(23,45, by=2), 46:48)]
        X.clean.w<-X.clean.w[,c(-25,-26,-31:-33)]


        #...Sort time out
          X.clean.w$time<-as.numeric(strftime(strptime(X.clean.w$time, format="%H:%M:%S"),"%H"))
          X.clean.m$time<-as.numeric(strftime(strptime(X.clean.m$time, format="%H:%M:%S"),"%H"))

        #Save X.clean
          save(X.clean.m, X.clean.w, file="/Users/annakrystalli/Documents/TRAINING DATA/Clean NA training data.RData")



#...Remove NAs and calculate actual values

    load(file="/Users/annakrystalli/Documents/TRAINING DATA/Clean NA training data.RData")
    

      #Load converter file to calculate actual values from DNs
        DNconvert<-read.csv("~/Documents/SATELLITE/DN conversions.csv", header=TRUE)





    #...Monthly.....  
        master.m<-data.frame(X.clean.m[complete.cases(X.clean.m),])
        start=22
        end=length(master.m)-3
        
        for(i in start:end){
          
          j<-which(DNconvert[,1]==names(master.m[i]))
          
          if(DNconvert[j,2]=="lin"){master.m[i]<-(as.numeric(unlist(master.m[i]))*DNconvert$slope[j])+DNconvert$intercept[j]}else{
            master.m[i]<-10^((as.numeric(unlist(master.m[i]))*DNconvert$slope[j])+DNconvert$intercept[j])
          }
        }


    #...Weekly..... 
        master.w<-X.clean.w[complete.cases(X.clean.w),]
        
        start=22
        end=length(master.w)-3
        
        
        for(i in start:end){
          
          j<-which(DNconvert[,1]==names(master.w[i]))
          
          if(DNconvert[j,2]=="lin"){master.w[i]<-(as.numeric(unlist(master.w[i]))*DNconvert$slope[j])+DNconvert$intercept[j]}else{
            master.w[i]<-10^((as.numeric(unlist(master.w[i]))*DNconvert$slope[j])+DNconvert$intercept[j])
          }
        }

        
        #Remove Irish Sea
        master.m<-master.m[-which((master.m$lat<=55.5) & (master.m$lon<=-2)),]
        master.w<-master.w[-which((master.w$lat<=55.5) & (master.w$lon<=-2)),]
        
        master.m$tow.id<-as.factor(paste(master.m$tow, master.m$year, master.m$day, sep="-"))
        master.w$tow.id<-as.factor(paste(master.w$tow, master.w$year, master.w$day, sep="-"))

   #...Save master training datasets
        save( master.m,  master.w, file="/Users/annakrystalli/Documents/TRAINING DATA/training data.RData")


  load(file="/Users/annakrystalli/Documents/TRAINING DATA/training data.RData")



    # Mean normalise and Feature scale inputs
        

              #transform variable IDs
          varID.w<-c("lon", "lat", "tow", "sample.no." ,"overlap", "time","day" ,"year", 
                     "month", "week", "T.w", "Tf.w", "Sed.w", "Ch.w", "fdens.w", 
                     "fdist.w", "fside.w", "bath", "NAO", "wNAO")

          varID.m<-c("lon", "lat", "tow", "sample.no." ,"overlap", "time","day","year",  
                     "month",  "week","T.m", "Tf.m", "Sed.m", "Sedf.m", "Chf.m", 
                     "Ch.m", "fdens.m", "fdist.m", "fside.m", "fdens.c.m", "fdist.c.m", 
                     "fside.c.m", "bath", "NAO", "wNAO")



          norm.x.w<-master.w[,which(names(master.w)%in%varID.w, arr.ind=TRUE)]
          norm.x.m<-master.m[,which(names(master.m)%in%varID.m, arr.ind=TRUE)]



          y.w<-master.w[,10:14]
          y.m<-master.m[,10:14]
          save(y.w, y.m , file="/Users/annakrystalli/Documents/TRAINING DATA/y training data.RData")


#....NORMALIZE FUNCTION..........................................................................................                   
normaliseFeature<-function(x, name){
          load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
        
            if(name=="tow" | name=="sample.no."){return(x)}else{ j<-which(min.max.tab[,1]==name, arr.ind=TRUE)
            x<-(x-as.numeric(min.max.tab$min[j]))/(as.numeric(min.max.tab$max[j])-as.numeric(min.max.tab$min[j]))

            return(x)}
            }

#....NORMALIZE..........................................................................................
save(min.max.tab, file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")

      #...Normalise weekly
      
          data<-norm.x.w
          
                  for(i in 1:length(dimnames(data)[[2]])){
                  
                      x<-data[,i]
                      name<-dimnames(data)[[2]][i]
                      
                      data[,i]<-normaliseFeature(x, name)
                      
                  }
          norm.x.w<-data

      #...Normalise monthly
          data<-norm.x.m
          
                for(i in 1:length(dimnames(data)[[2]])){
                  
                  x<-data[,i]
                  name<-dimnames(data)[[2]][i]
                  
                  data[,i]<-normaliseFeature(x, name)
                  
                }
          norm.x.m<-data


save(norm.x.w, norm.x.m , file="/Users/annakrystalli/Documents/TRAINING DATA/normalized (min.max) x training data.RData")





                      # MIn/MAx normalisation look up set up...NOT REQUIRED JUST LOAD FILE
                      start=5
                      end=6
                      
                      for(i in start:end){
                        for(j in 1: length(DNconvert[,i])){
                          if(DNconvert[j,2]=="lin"){DNconvert[j,i+2]<-(DNconvert[j,i]*DNconvert$slope[j])+DNconvert$intercept[j]}else{
                            DNconvert[j,i+2]<-10^((DNconvert[j,i]*DNconvert$slope[j])+DNconvert$intercept[j])
                          }
                        }
                      }
                      
                      
                      
                      
                      min.max.tab<-DNconvert[,c(1,7,8)]
                      min.max.tab[,1]<-as.character(min.max.tab[,1])
                      
                      min.max.ind<-c("lon", "lat", "overlap","time", "year", 
                                     "month", "week", 
                                     "bath", "NAO", "wNAO")
                      
                      
                      for(i in 1:10){ 
                        j<-which(names(X)==min.max.ind[i], arr.ind=TRUE)
                        min.max.tab<-rbind(min.max.tab, c(min.max.ind[i], range(X[,j])))}
                      
                      names(min.max.tab)[2:3]<-c("min", "max")
                      min.max.tab<-as.data.frame(do.call(rbind, min.max.tab))
                      
                      min.max.tab[,3]<-as.numeric(min.max.tab[,3])
                      
                      save(min.max.tab, file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
                      load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
