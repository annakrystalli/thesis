#Set up
rm(list = ls())

require(nnet)


load(file="/Users/annakrystalli/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
load(file="/Users/annakrystalli/Documents/TRAINING DATA/y training data.RData")


load(file="~/Documents/TRAINING DATA/Dataset splits monthly.RData")



#......SPlit DATASET........................................................  

ice.dataPrep<-function(spp=y.m$calhel, spp.name="chel"){


                       
y.o.m<-spp

y.o.tr.m<-y.o.m[train.dat.m] 
y.a.tr.m<-y.o.tr.m[y.o.tr.m>=1]

y.o.v.m<-y.o.m[val.dat.m]
y.a.v.m<-y.o.v.m[y.o.v.m>=1]

y.o.ts.m<-y.o.m[test.dat.m]
y.a.ts.m<-y.o.ts.m[y.o.ts.m>=1]

#.......Monthly varnames............................................................
varnames.m<-c("lon", "lat", "time", "year", 
              "month",  "T.m",  "Tf.m",  "Sed.m", 
              "Sedf.m",  "Chf.m", "Ch.m", "fdens.m", 
              "fdist.m",  "fside.m",  "fdens.c.m", 
              "fdist.c.m",  "fside.c.m", 
              "bath", "NAO", "wNAO")

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

X.o.m<-selectX(norm.x.m, varnames.m)

X.o.tr.m<-X.o.m[train.dat.m,]
X.a.tr.m<-X.o.tr.m[y.o.tr.m>=1,]   

X.o.v.m<-X.o.m[val.dat.m,]
X.a.v.m<-X.o.v.m[y.o.v.m>=1,]                   

X.o.ts.m<-X.o.m[test.dat.m,]
X.a.ts.m<-X.o.ts.m[y.o.ts.m>=1,]                    



save(X.o.tr.m, X.o.v.m, X.o.ts.m,
     file="/Users/annakrystalli/Documents/TRAINING DATA/X_oc_data.RData")
save(y.o.tr.m, y.o.v.m, y.o.ts.m,
     file=paste("/Users/annakrystalli/Documents/TRAINING DATA/y_oc_", spp.name,"_data.RData", sep=""))
save(X.a.tr.m, X.a.v.m, X.a.ts.m,
     file="/Users/annakrystalli/Documents/TRAINING DATA/X_ac_data.RData")
save(y.a.tr.m, y.a.v.m, y.a.ts.m,
     file=paste("/Users/annakrystalli/Documents/TRAINING DATA/y_ac_", spp.name,"_data.RData", sep=""))
     }


ice.dataPrep(spp=y.m$calhel, spp.name="chel")
  
ice.boostPrep<-function(spp=y.m$calhel, spp.name="chel" ){
#.......varnames WITHOUT TIME...........................................................
  
  ice.dataPrep<-function(spp=y.m$calhel, spp.name="chel"){
    
    
    
    y.o.m<-spp
    
    y.o.tr.m<-y.o.m[train.dat.m]
    y.a.tr.m<-y.o.tr.m[y.o.tr.m>=1]
    
    y.o.v.m<-y.o.m[val.dat.m]
    y.a.v.m<-y.o.v.m[y.o.v.m>=1]
    
    y.o.ts.m<-y.o.m[test.dat.m]
    y.a.ts.m<-y.o.ts.m[y.o.ts.m>=1]
    
    #.......Monthly varnames............................................................
    varnames.m<-c("lon", "lat", "time", "year", 
                  "month",  "T.m",  "Tf.m",  "Sed.m", 
                  "Sedf.m",  "Chf.m", "Ch.m", "fdens.m", 
                  "fdist.m",  "fside.m",  "fdens.c.m", 
                  "fdist.c.m",  "fside.c.m", 
                  "bath", "NAO", "wNAO")
    
    selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
    
    X.o.m<-selectX(norm.x.m, varnames.m)
    
    X.o.tr.m<-X.o.m[train.dat.m,]
    X.a.tr.m<-X.o.tr.m[y.o.tr.m>=1,]   
    
    X.o.v.m<-X.o.m[val.dat.m,]
    X.a.v.m<-X.o.v.m[y.o.v.m>=1,]                   
    
    X.o.ts.m<-X.o.m[test.dat.m,]
    X.a.ts.m<-X.o.ts.m[y.o.ts.m>=1,] 
  
  

v1<-c("lon", "lat",  "year", 
               "month",  "T.m",  "Tf.m",  "Sed.m", 
               "Sedf.m",  "Chf.m", "Ch.m",
               "fdist.m",  "fside.m",
               "fdist.c.m",  "fside.c.m", 
               "bath", "NAO", "wNAO")

v2<-c("lon", "lat",  "year", 
               "month",  "T.m", "Sed.m", 
               "Sedf.m",  "Ch.m", "fdens.m", 
               "fdist.m",  "fside.m",  "fdens.c.m", 
               "fdist.c.m",  "fside.c.m", 
               "bath", "NAO", "wNAO")

ice.dataPrep(spp=y.m$calhel, spp.name="chel")
    
    
  
size<-rep(c(5,10,15,20,25,30,35,40, 45,50), times=24) 
decay<-rep(rep(c(0 , 0.01 , 0.02 , 0.04 , 0.08 , 0.16 , 0.32 , 0.64 , 1.28 , 2.56 , 5.12 , 10.24)/152217, times=2), each=10)          
varnames<-rep(list(v1=v1,v2=v2), each=120)
maxit<-500000

oc.dat<-list(x=X.o.tr.m,y=y.o.tr.m, x.val=X.o.v.m, y.val=y.o.v.m, size=size,
             decay=decay,  maxit=maxit, 
             varnames=varnames, file.ID=NULL)
    
    trainNN<-function(x,y, x.val, y.val, x.pred, size, presence=TRUE, decay=0.1,  maxit=300, 
                      varnames, weights, file.ID=NULL, plot=TRUE)
      
      
#Make SIZE datafiles
      makeSIZEdat<-function(sizes)