#Set up
rm(list = ls())

input.folder<-"/Users/annakrystalli/Documents/SATELLITE"
aux.data.folder<-"/Users/annakrystalli/Documents/SATELLITE/aux.data"

.......#....NORMALIZE FUNCTION..........................................................................................                   
normaliseFeature<-function(x, name){
            load(file=paste(aux.data.folder,"/min.max.ind pre processing file.RData", sep=""))
            
            j<-which(min.max.tab[,1]==name, arr.ind=TRUE)
            x<-(x-min.max.tab$min[j])/(min.max.tab$max[j]-min.max.tab$min[j])
            
            return(x)
          }

.................................................................................................
compileAllPredDat<-function(input.folder=input.folder, 
                            aux.data.folder=aux.data.folder){
  
require(stringr)

          data.index<-read.csv(paste(aux.data.folder,"/data type index.csv", sep=""))
          #Load geo.matrix
          load(file=paste(aux.data.folder,"/geo.matrix.RData", sep=""))
          
          load(file=paste(aux.data.folder,"/month.index.RData", sep=""))
          
          
          load(file=paste(aux.data.folder,"/Bathymetry.RData", sep=""))
          load(file=paste(aux.data.folder,"/NAO.RData", sep=""))
          
          #Load converter file to calculate actual values from DNs
          DNconvert<-read.csv(paste(aux.data.folder,"/DN conversions.csv", sep=""), header=TRUE)



ntp.log<-NULL

t.indx<-paste(rep(1997:2010, each=12),"-",rep(1:12, times=14), "-",sep="")
t<-length(t.indx)






for (i in 1:t){

ntp.log<-try(compilePredDat(t.ind=t.indx[i], t.res="monthly", 
                            compile.var=data.index, ntp.log=ntp.log), 
             silent=TRUE)

save(ntp.log, file=paste(aux.data.folder, "/ntp.log.Rdata", sep=""))
            }

}

compilePredDat<-function(t.ind, t.res, compile.var=data.index, ntp.log=ntp.log){
              
                  n.var<-dim(compile.var)[1]
                  if(t.res=="weekly"){dim.names<-c("r", "c", "lat", "lon", "bath",
                                                   paste(as.character(compile.var$data.type), ".w", sep=""))}else{
                                                     dim.names<-c("r", "c", "lat", "lon", "bath",
                                                                  paste(as.character(compile.var$data.type), 
                                                                        ".m", sep=""))}
                  
              predict.var<-array(NA, dim=c(1112,926, n.var+5), dimnames(dim.names))
              predict.var[,,1]<-matrix(1:1112)
              predict.var[,,2]<-matrix(1:926, nrow=1112, ncol=926, byrow=TRUE)
              predict.var[,,3]<-geo.matrix[,,2]
              predict.var[,,4]<-geo.matrix[,,1]
              predict.var[,,5]<-bath.m
              
              for (i in 1:n.var){
                
                filenames<-list.files(path=paste(input.folder,
                                                 t.res, 
                                                 compile.var$map.type[i],
                                                 "r files", sep="/"), all.files = FALSE)
              
              
                    #Test whether sat image with temporal match exists - otherwise "NTP"
                      #and assign to appropriate array layer
                
                    if (length(
                      try(
                        sat.file<-filenames[grep(t.ind, filenames,value=FALSE)] , 
                        silent=TRUE)
                        )!=0){load(paste(input.folder,
                                     t.res, compile.var$map.type[i],"r files",
                                     sat.file, sep="/"))
                              
                              
                                  predict.var[,,5+i]<-img}else{predict.var[,,5+i]<-NA
                                                              ntp.log<-c(ntp.log, 
                                                                         paste(t.ind,
                                                                               compile.var$data.type[i], 
                                                                         sep=""))} 
              
                        if(i==1){#Exctract day info  
                          sat.file<-gsub(t.ind, "",sat.file)
                          day.info<-gsub("^.|-.*$", "",sat.file)
                          
                          #if weekly determine week
                          if(t.res=="weekly"){week<-((as.numeric(gsub("^....", "",day.info ))+7)/8)}
                          year<-as.numeric(str_extract(day.info, "^...."))
                          
                          #Determine month
              
                          month<-month.index[month.index[,2]==gsub("^.*median...|..-.*$", "",sat.file),1]}
              }
              
              
              
              X.pred<-as.data.frame(matrix(predict.var, nrow=1112*926, ncol=n.var+5))
              names(X.pred)<-dim.names
              
              
              rm(predict.var)
              
              
              NAOt.ind<-paste(year, "-", month, "-", sep="")
              
              X.pred$year<-year
              X.pred$month<-month
              if(t.res=="weekly"){X.pred$week<-week}
              X.pred$NAO<-NAO$NAO[which(NAO$m.index==NAOt.ind)]
              X.pred$wNAO<-NAO$wNAO[which(NAO$m.index==NAOt.ind)]
              
              save(X.pred, file=paste(input.folder,"/", t.res,"/PROCESSED/raw/",t.ind,".RData", sep=""))
              
              #Clean monthly
              if(t.res=="monthly"){
                    X.pred<-X.pred[-which((X.pred$lat<=55.5) & (X.pred$lon<=-2)),]
                    X.pred<-X.pred[X.pred$T.m!=0,]
                    X.pred$T.m[X.pred$T.m==255]<-NA
                    X.pred$Ch.m[X.pred$Ch.m==0]<-NA
                    X.pred$Tf.m[X.pred$Tf.m==254]<-NA
                    X.pred$Chf.m[X.pred$Chf.m==254]<-NA
                    X.pred$Sed.m[X.pred$Sed.m==0]<-NA
                    X.pred$Sedf.m[X.pred$Sedf.m==254]<-NA
                    X.pred$fside.m[X.pred$fside.m==0]<-NA
                    X.pred$fside.c.m[X.pred$fside.c.m==0]<-NA
                    X.pred$fdens.m[X.pred$fdens.m==254]<-NA
                    X.pred$fdens.c.m[X.pred$fdens.c.m==254]<-NA}else{
                                                            X.pred<-X.pred[X.pred$T.w!=0,]
                                                            X.pred<-X.pred[X.pred$T.w!=255,]
                                                            X.pred<-X.pred[X.pred$Ch.w!=0,]
                                                            X.pred<-X.pred[X.pred$Tf.w!=254,]
                                                            X.pred<-X.pred[X.pred$Chf.w!=254,]
                                                            X.pred<-X.pred[X.pred$Sed.w!=0,]
                                                            X.pred<-X.pred[X.pred$Sedf.w!=254,]
                                                            X.pred<-X.pred[X.pred$fside.w!=0,]
                                                            X.pred<-X.pred[X.pred$fside.c.w!=0,]}  


                for (i in 5+(1:n.var)){
                  j<-which(DNconvert$var==dim.names[i])
                  
                  if(DNconvert[j,2]=="lin"){X.pred[,i]<-(X.pred[,i]*DNconvert$slope[j])+DNconvert$intercept[j]}else{
                    X.pred[,i]<-10^((X.pred[,5+i]*DNconvert$slope[j])+DNconvert$intercept[j])}
                }
              
               
              save(X.pred, file=paste(input.folder,"/"
                                      ,t.res,"/PROCESSED/clean/",t.ind,".RData", sep=""))
              
              
              for (i in 3:dim(X.pred)[2]){

                X.pred[,i]<-normaliseFeature(x=X.pred[,i], name=names(X.pred)[i])
                
              }
              
              save(X.pred, file=paste(input.folder,"/"
                                      ,t.res,"/PROCESSED/normalised.na/",t.ind,".RData", sep=""))
             
              
              X.pred<-X.pred[-which(is.na(X.pred), arr.ind=TRUE)[,1],]
              
              if(dim(X.pred)[1]>0){save(X.pred, file=paste(input.folder,"/"
                                      ,t.res,"/PROCESSED/normalised/",t.ind,".RData", sep=""))}

             
return(ntp.log)

}

load(file=paste("/Users/annakrystalli/Documents/SATELLITE/"
                        ,"monthly","/PROCESSED/normalised/","2010-12-",".RData", sep=""))