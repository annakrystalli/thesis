
require(randomForest)
require(png)

set.seed(1)

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

varnames<-c("year", "time",
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")

load(file=paste("~/Documents/TRAINING DATA/y ", data, "training data.Rdata", sep="")) 

load("~/Documents/TRAINING DATA/normalized (min.max) x training data.RData")

norm.x.m$time[norm.x.m$time>=0.25 & norm.x.m$time<0.75]<-"day"
norm.x.m$time[norm.x.m$time!="day"]<-"night"
norm.x.m$time<-as.factor(norm.x.m$time)


all.samples<-paste(norm.x.m$tow,norm.x.m$sample.no., sep="-")

x.oc<-data.frame(spl.id=sort(all.samples), norm.x.m[order(all.samples),])
y.m<-y.m[order(all.samples),]
all.samples<-sort(all.samples)


y.oc<-y.m
y.oc[y.m!=0]<-1

rm(norm.x.m)

dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                 sep=""), 
           showWarnings = F)
dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                 "forest plots/",sep=""), 
           showWarnings = F)
dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                 "forests/",sep=""), 
           showWarnings = F)



rftune<-function(y, y.oc, x=x.oc, 
                 varnames, size=5000, names.spp){
  
  require(randomForest)
  
  selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}
  
  rftunes<-vector("list", length(names.spp)*2)
  
  for(spp.id in 1:length(names.spp)){
    
    spp<-names.spp[spp.id]
    set.seed(1)
    
    y.spp.oc<-y.oc[,spp.id]
    y.spp<-y[y.spp.oc==1,spp.id]
    
    s.oc<-unlist(mapply(FUN=sample, x=split(1:length(y.spp.oc), as.factor(y.spp.oc)),
                 size=table(y.spp.oc)/length(y.spp.oc)*size))

    s<-unlist(mapply(FUN=sample, x=split(1:length(y.spp), as.factor(y.spp)),
                 size=table(y.spp)/length(y.spp)*size))                 
    
    rftunes[[spp.id]]<-tuneRF(selectX(x[s.oc,], varnames),
                              as.factor(y.spp.oc[s.oc]), 
                              mtryStart=4, ntreeTry=100, stepFactor=2, 
                              improve=0.05, trace=TRUE, plot=F)  
    

    
    rftunes[[spp.id+5]]<-tuneRF(selectX(x[y.spp.oc==1,], varnames)[s,],
                                    as.factor(y.spp[s]), 
                                    mtryStart=4, ntreeTry=100, stepFactor=2, 
                                    improve=0.05, trace=TRUE, plot=F)} 
  
  rftune<-lapply(rftunes, function(x){min(x[which(x[,2]==min(x)),1])})
  rftune<-matrix(rftune, 5, 2, byrow=F, 
                 dimnames=list(names.spp, c("oc", "ac")))
  
  return(rftune)}


#_________________________________________________________________________________________________________________________

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


growForest<-function(x=x.oc, y.oc=y.oc, y=y.m,
                    ntree=200, tune, 
                     varnames, plot=FALSE, set.seed=TRUE, an.ID, names.spp){
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                  sep=""), 
             showWarnings = F)
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forest plots/",sep=""), 
             showWarnings = F)
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forests/",sep=""), 
             showWarnings = F)
  
  
  
  for(spp.id in 1:length(names.spp)){
    
    spp<-names.spp[spp.id]
    
    y.spp.oc<-y.oc[,spp.id]
    
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forests/",spp,sep=""), 
             showWarnings = F)
  
  
  if(set.seed==TRUE){set.seed(seed=1)}
  rforOC<-randomForest(x=selectX(x, varnames), y=as.factor(y.spp.oc),
                       ntree=ntree, mtry=unlist(tune[spp, 1]),
                       replace=TRUE, classwt=NULL, 
                       strata=as.factor(y.spp.oc),
                       sampsize = as.vector(table(y.spp.oc)*.632))
  

  

  
  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                   an.ID,"forest plots/",
                                   spp, " OC error.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  plot(rforOC)
  
  if(plot==FALSE){dev.off()}
  
  
  plotOCimp(rforOC, spp=spp, plot=plot, an.ID=an.ID)
  
  
  
  save(rforOC, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                          an.ID,"forests/",
                          spp, "/OCforest.Rdata", sep=""))
  
  rm(rforOC)
  
  #__________AC rfor
  
  if(set.seed==TRUE){set.seed(seed=1)}
    y.spp<-y[y.spp.oc==1, spp.id]
    
  rforAC<-randomForest(x=selectX(x, varnames)[y.spp.oc==1,], y=as.factor(y.spp), 
                       ntree=ntree, mtry=unlist(tune[spp, 2]),
                       replace=TRUE, classwt=NULL, 
                       strata=as.factor(y.spp),
                       sampsize = as.vector(table(y.spp)*.632))
  
  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/"
                                   ,an.ID,"forest plots/",
                                   spp, "AC error.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  plot(rforAC)
  
  
  
  if(plot==FALSE){dev.off()}
  
  
  plotACimp(rforAC, spp=spp, plot=plot, an.ID=an.ID)
  
  save(rforAC, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                          an.ID,"forests/",
                          spp, "/ACforest.Rdata", sep=""))}
  
  
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