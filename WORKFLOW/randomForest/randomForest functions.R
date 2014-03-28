
require(randomForest)
require(png)

load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/y ", data, "training data.Rdata", sep="")) 



v2<-c("lon", "lat",  "year", 
      "month",  "T.m", "Sed.m", 
      "Sedf.m",  "Ch.m", "fdens.m", 
      "fdist.m",  "fside.m",  "fdens.c.m", 
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")

v3<-c("year", 
      "month",  "T.m", "Sed.m", 
      "Sedf.m",  "Ch.m", "fdens.m", 
      "fdist.m",  "fside.m",  "fdens.c.m", 
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")

v4<-c("year", "time",
      "month",  "T.m", "Sed.m", 
      "Sedf.m",  "Ch.m", "fdens.m", 
      "fdist.m",  "fside.m",  "fdens.c.m", 
      "fdist.c.m",  "fside.c.m", 
      "bath", "NAO", "wNAO")
#_________________________________________________________________________________________________________________________



rforDataprep<-function(resample=FALSE, tr.split=0.7, v.split=2/3){
  
  load(file=paste("/Users/annakrystalli/documents/TRAINING DATA/y training data.RData", sep=""))
  load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")
  
  if(resample==TRUE){n.m<-dim(y.m)[1]
                     all.dat.m<-1:n.m
                     
                     train.dat.m<-sample(all.dat.m, n.m*tr.split)
                     val.dat.m<-sample(setdiff(all.dat.m, train.dat.m), 
                                       v.split*length(setdiff(all.dat.m, train.dat.m)))
                     test.dat.m<-setdiff(all.dat.m, union(train.dat.m, val.dat.m))    
                     
                     save(val.dat.m,test.dat.m,train.dat.m, 
                          file="~/Documents/TRAINING DATA/Dataset splits monthly.RData")  
  }
  
  load("~/Documents/TRAINING DATA/Dataset splits monthly.RData")
  
  names.spp<-names(y.m)
  names.spp[3]<-"cfin"
  names.spp[4]<-"chel"
  names(y.m)<-names.spp
  spp.id<-which(names.spp%in%spp, arr.ind=TRUE)
  for(spp.id in 1:5){
    
    spp<-names.spp[spp.id]
    y.oc<-y.m[c(train.dat.m,val.dat.m), spp.id]
    y.oc[y.oc>=1]<-1
    y.toc<-y.m[test.dat.m, spp.id]
    y.toc[y.toc>=1]<-1
    x.oc<-norm.x.m[c(train.dat.m,val.dat.m),]
    x.toc<-norm.x.m[test.dat.m, ]
    
    y<-y.m[c(train.dat.m,val.dat.m), spp.id]
    y<-y[y.oc>=1]
    
    x<-norm.x.m[c(train.dat.m,val.dat.m),]
    x<-x[y.oc>=1,]
    
    y.t<-y.m[test.dat.m, spp.id]
    y.t<-y.t[y.toc>=1]
    
    x.t<-norm.x.m[test.dat.m, ]
    x.t<-x.t[y.toc>=1,]
    
    
    y.t<-factor(y.t, levels=sort(unique(y))) 
    y.toc<-factor(y.toc, levels=0:1) 
    y.oc<-factor(y.oc,levels=0:1)
    y<-factor(y, levels=sort(unique(y)))
    
    
    
    
    save(y, y.oc, y.t, y.toc, x, x.oc, x.t, x.toc,
         file=paste("~/Documents/TRAINING DATA/Models/randomForest/",spp, "_train_data.Rdata", sep=""))  
    
    
  }
}
#_______________________________________________________________________________________________________


selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
#____________________________________________

plotACimp<-function(rforAC, spp=spp, plot=TRUE, multi=F, an.ID){
  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forest plots/",
                                   spp, " ACimp.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  imp.ac<-importance(rforAC)
  var.names.ac<-rownames(imp.ac)
  
  FUN.c<-colorRampPalette(c( "thistle1",  "darkorchid3"), space="Lab")
  cols<-FUN.c(n=max(imp.ac))[imp.ac]
  
  
  if(multi==F){par(las=2, col.axis="darkslategray", 
      col.lab="darkslategray", 
      col.main="darkslategray",
      font.axis=2, family="Helvetica",
      font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2)) }
  barplot(imp.ac[order(imp.ac)], names.arg=var.names.ac[order(imp.ac)], horiz=TRUE, 
          col=cols[order(imp.ac)], cex.axis=0.8, cex.names=0.9, border="darkorchid4",
          main=paste(spp, "AC rfor variable importance"))
  
  if(plot==FALSE){dev.off()}}
#____________________________________________


plotOCimp<-function(rforOC, spp=spp, plot=TRUE, multi=F, an.ID){
  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/"
                                   ,an.ID,"forest plots/",
                                   spp, " OCimp.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  imp.oc<-importance(rforOC)
  var.names.oc<-rownames(imp.oc)
  
  FUN.c<-colorRampPalette(c( "honeydew",  "aquamarine4"), space="Lab")
  cols<-FUN.c(n=max(imp.oc))[imp.oc]
  
  
  if(multi==F){par(las=2, col.axis="darkslategray", 
      col.lab="darkslategray", 
      col.main="darkslategray",
      font.axis=2, family="Helvetica",
      font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2))} 
  barplot(imp.oc[order(imp.oc)], names.arg=var.names.oc[order(imp.oc)], horiz=TRUE, 
          col=cols[order(imp.oc)], cex.axis=0.8, cex.names=0.9, border="darkslategray4",
          main=paste(spp, "OC rfor variable importance"))
  
  if(plot==FALSE){dev.off()}}
#_________________________________________________________________________________________________________________________


growForest<-function(x=x, x.oc=x.oc, x.t=x.t, x.toc=x.toc, y=y, y.oc=y.oc,
                     y.t=y.t, y.toc=y.toc, ntree=200,
                     varnames=v2, plot=FALSE, set.seed=TRUE, an.ID, spp=spp){
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                  sep=""), 
             showWarnings = F)
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forest plots/",sep=""), 
             showWarnings = F)
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forests/",sep=""), 
             showWarnings = F)
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forests/",spp,sep=""), 
             showWarnings = F)
  
  
  x<-selectX(x, varnames)
  x.oc<-selectX(x.oc, varnames)
  x.t<-selectX(x.t, varnames)
  x.toc<-selectX(x.toc, varnames)
 
  
  if(set.seed==TRUE){set.seed(seed=1)}
  rforOC<-randomForest(x=x.oc, y=y.oc, xtest=x.toc, ytest=y.toc,
                       ntree=ntree,
                       replace=TRUE, classwt=NULL, 
                       strata=y.oc,
                       sampsize = as.vector(c(table(y.oc)[1]*.632, 
                                              table(y.oc)[2]*.632)))
  
  rforOC.test<-rforOC$test
  save(rforOC.test, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                          an.ID,"forests/",
                          spp, "/OCforest test.Rdata", sep=""))
  print(spp) 
  print("test")
  print(rforOC$test$confusion)
  
  rm(list=c("rforOC","rforOC.test"))
  
  if(set.seed==TRUE){set.seed(seed=1)}
  rforOC<-randomForest(x=x.oc, y=y.oc, 
                       keep.forest=TRUE, ntree=ntree,
                       replace=TRUE, classwt=NULL, 
                       strata=y.oc,
                       sampsize = as.vector(c(table(y.oc)[1]*.632, 
                                              table(y.oc)[2]*.632)))
  
  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                   an.ID,"forest plots/",
                                   spp, " OC error.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  plot(rforOC)
  
  if(plot==FALSE){dev.off()}
  
  
  plotOCimp(rforOC, spp=spp, plot=plot, an.ID=an.ID)
  
  print("training")
  print(rforOC$confusion)
  
  
  save(rforOC, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                          an.ID,"forests/",
                          spp, "/OCforest.Rdata", sep=""))
  
  rm(rforOC)
  
  #__________AC rfor
  
  if(set.seed==TRUE){set.seed(seed=1)}
  rforAC<-randomForest(x=x, y=y, xtest=x.t, ytest=y.t,
                       ntree=ntree,
                       replace=TRUE, classwt=NULL, 
                       strata=y,
                       sampsize = as.vector(table(y)*.632))
  
  rforAC.test<-rforAC$test
  save(rforAC.test, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                               an.ID,"forests/",
                               spp, "/ACforest test.Rdata", sep=""))
  print("spp") 
  print("test")
  print(rforAC$test$confusion)
  
  rm(list=c("rforAC","rforAC.test"))
  
  if(set.seed==TRUE){set.seed(seed=1)}
  rforAC<-randomForest(x=x, y=y, 
                       keep.forest=TRUE, ntree=ntree,
                       replace=TRUE, classwt=NULL, 
                       strata=y,
                       sampsize = as.vector(table(y)*.632))
  
  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/"
                                   ,an.ID,"forest plots/",
                                   spp, "AC error.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  plot(rforAC)
  
  
  
  if(plot==FALSE){dev.off()}
  
  print(rforAC$confusion)
  
  plotACimp(rforAC, spp=spp, plot=plot, an.ID=an.ID)
  
  save(rforAC, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                          an.ID,"forests/",
                          spp, "/ACforest.Rdata", sep=""))
  
  
}



plotImps<-function(plot=T, multi=T, pt=28, an.ID){
  
  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forest plots/all var imp.png",sep=""), 
      width=650*5, height=2*600, pointsize=pt)

par(mfcol=c(2,5),las=2, col.axis="darkslategray", 
                                    col.lab="darkslategray", 
                                    col.main="darkslategray",
                                    font.axis=2, family="Helvetica",
                                    font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2)) 

for(spp in  c("chel", "cfin","metrilu", "tem", "centrot")){
  
load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forests/",
                        spp, "/OCforest.Rdata", sep=""))

    plotOCimp(rforOC, spp=spp, plot=plot, multi=T, an.ID=an.ID)

      rm(rforOC)
 
load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forests/",
                spp, "/ACforest.Rdata", sep=""))

    plotACimp(rforAC, spp=spp, plot=plot, multi=T, an.ID=an.ID)

      rm(rforAC)

}
dev.off()}