
require(randomForest)
require(png)

an.ID="Final/"
thresh="PredPrev=Obs"
names.spp<-c( "chel","cfin" , "metrilu" ,  "centrot" ,"tem" )

set.seed(1)

selectX<-function(X, varnames){X[,which(names(X) %in% varnames)]}

varnames<-c("year", "time",
            "month",  "T.m", "Sed.m", 
            "Sedf.m",  "Ch.m", "fdens.m", 
            "fdist.m",  "fside.m",  "fdens.c.m", 
            "fdist.c.m",  "fside.c.m", 
            "bath", "NAO", "wNAO")


spp.labels<-c(expression(italic("C. helgolandicus")), 
              expression(italic("C. finmarchicus")),
              expression(italic("M. lucens")),
              expression(italic("C. typicus")),
              expression(italic("T. longicornis")))

names(spp.labels)<-names.spp


#______________________________________________________________________________________________________________________________________

#____________________________________________

plotACimp<-function(rforAC, spp=spp, plot=TRUE, multi=F, an.ID, ac.vrank=ac.vrank){


  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forest plots/",
                                   spp, " ACimp.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  imp.ac<-importance(rforAC)
  var.names.ac<-rownames(imp.ac)
  
  FUN.c<-colorRampPalette(c( "thistle1",  "darkorchid3"), space="Lab")
  cols<-FUN.c(n=max(imp.ac))[imp.ac]
  names(cols)<-rownames(imp.oc)
  
  if(multi==F){par(las=2, col.axis="darkslategray", 
                   col.lab="darkslategray", 
                   col.main="darkslategray",
                   font.axis=2, family="Helvetica",
                   font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2)) }
  barplot(imp.ac[rev(ac.vrank),], names.arg=rev(ac.vrank), horiz=TRUE, 
          col=cols[rev(ac.vrank)], cex.axis=0.8, cex.names=0.9, border="darkorchid4")
  
  if(plot==FALSE){dev.off()}}
#____________________________________________


plotOCimp<-function(rforOC, spp=spp, plot=TRUE, multi=F, an.ID, oc.vrank=oc.vrank, spp.labels){
  
  if(plot==FALSE){png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/"
                                   ,an.ID,"forest plots/",
                                   spp, " OCimp.png", sep=""), 
                      width=650, height=600, pointsize=12)}
  
  imp.oc<-importance(rforOC)
  
  FUN.c<-colorRampPalette(c( "honeydew",  "aquamarine4"), space="Lab")
  cols<-FUN.c(n=max(imp.oc))[imp.oc]
  names(cols)<-rownames(imp.oc)
  
  if(multi==F){par(las=2, col.axis="darkslategray", 
                   col.lab="darkslategray", 
                   col.main="darkslategray",
                   font.axis=2, family="Helvetica",
                   font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2))} 
  
  barplot(imp.oc[rev(oc.vrank),], names.arg=rev(oc.vrank), horiz=TRUE, 
          col=cols[rev(oc.vrank)], cex.axis=0.8, cex.names=0.9, border="darkslategray4",
          main=spp.labels[spp])
  
  if(plot==FALSE){dev.off()}}
#_________________________________________________________________________________________________________________________



plotImps<-function(plot=T, multi=T, pt=38, an.ID){
  
  oc.var.gini<-NULL
  ac.var.gini<-NULL
  
  for(spp in  names.spp){
    
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forests/",
                    spp, "/OCforest.Rdata", sep=""))
  
    oc.var.gini<-cbind(oc.var.gini,importance(rforOC))
    
    rm(rforOC)
    
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forests/",
                    spp, "/ACforest.Rdata", sep=""))
    
    ac.var.gini<-cbind(ac.var.gini,importance(rforAC))
    
    rm(rforAC)
  }
    
    oc.mean.gini<-rowSums(apply(oc.var.gini, FUN=rank, 2))
    ac.mean.gini<-rowSums(apply(ac.var.gini, FUN=rank, 2))
    oc.vrank<-rownames(oc.var.gini)[rev(order(oc.mean.gini))]
    ac.vrank<-rownames(ac.var.gini)[rev(order(ac.mean.gini))]
    
  
  png(file = paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forest plots/all var imp.png",sep=""), 
      width=650*5, height=2*600, pointsize=pt)
  
  par(mfcol=c(2,5),las=2, col.axis="darkslategray", 
      col.lab="darkslategray", 
      col.main="darkslategray",
      font.axis=2, family="Helvetica",
      font.main=4, oma=c(1,1,1.2,1), mar=c(3,4.2,1,2)) 
  
  for(spp in  names.spp){
    
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forests/",
                    spp, "/OCforest.Rdata", sep=""))
    
    plotOCimp(rforOC, spp=spp, plot=plot, multi=T, an.ID=an.ID, oc.vrank, spp.labels)
    
    rm(rforOC)
    
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,"forests/",
                    spp, "/ACforest.Rdata", sep=""))
    
    plotACimp(rforAC, spp=spp, plot=plot, multi=T, an.ID=an.ID, ac.vrank)
    
    rm(rforAC)
    
  }
  dev.off()

}