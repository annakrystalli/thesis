rm(list=ls())
cv.no=5
#Input correct an.ID for tuning and y data ("mod " for merged categories)
an.ID="model assess NC CV/"
an.IDCV="model assess NC CV"
thresh="PredPrev=Obs"

#data= " mod" for merged categories or NULL for original ACs
data=" mod"
model<-"rf_i"

names.spp<-c("chel", "centrot", "tem", "cfin",  "metrilu")
spp.labels<-c(expression(italic("C. helgolandicus")), 
              expression(italic("T. longicornis")),
              expression(italic("C. typicus")),   
              expression(italic("C. finmarchicus")),
              expression(italic("M. lucens")))


naAC<-list(NULL,NULL, NULL,NULL, 1:5)

ylabsA<-list("Frequency", NULL,NULL, NULL,NULL)
ylabsB<-list("Predicted AC", NULL,NULL, NULL,NULL)

png(filename = "~/Documents/THESIS/RF Chapter/Figures/Figure2.png", 
    pointsize=9.5, units = "in", res=300, width=8, height=2.8)

layout(matrix(1:10, 2,5, byrow=F), widths=1, heights=0.5)
par(oma=c(0.3, 2, 2.5, 0.2),  mgp=c(2,0.6,0))


for(spp.id in 1:length(names.spp)){

  par(mar=c(0.2, 3, 1.5, 0.5), las=0)
  
  spp<-names.spp[spp.id]
  
  yAC<-NULL
  prAC<-NULL
  
  for (i in 1:cv.no){ 
    
    an.ID<-paste(an.IDCV,i, "/", sep="") 
    
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                    an.ID,"forests/",spp,"/results.Rdata", sep=""))
    
    
    prAC<-c(prAC,res[[model]]$rf.p$AC$pr) 
    yAC<-c(yAC,res[[model]]$rf.p$AC$y.er)}
  
  

  
  
  yAC<-factor(yAC, levels=1:5)

  
  
  mAC<-cbind(y=hist(as.numeric(yAC), breaks=0:max(levels(yAC)), plot=F)$counts, 
             pr=hist(as.numeric(prAC), breaks=0:max(levels(yAC)), plot=F)$counts)
  
  barplot(t(mAC), beside=T, names.arg=NULL, xlim=c(-2,17),
          cex.lab=1.3,cex.main=1.4, xlab="",
          main=spp.labels[spp.id], ylab=ylabsA[[spp.id]],
          col=c("black", "grey"))
  
  if(spp.id==5){legend("topright", legend=c("y", "pr"), 
                       fill=c("black", "grey"), bty="n",
                       cex=0.9)}
  
  
  
  par(mar=c(4, 3, 0.2, 0.2), las=1, mgp=c(2,0.8,0))
  
  boxplot(prAC ~ as.factor(yAC), ylab=ylabsB[[spp.id]], xlab="Observed AC",
          cex.lab=1.3,cex.main=1.5, xlim=c(0,6), ylim=c(0,6), axes=F)
  
  box(which = "plot", lty = "solid")
  axis(1, at=unique(yAC), cex.axis=1.15, font=2)
  axis(2, at=1:5, cex.axis=1.15, font=2)
}

mtext(expression(bold(B)), side=2, at=0.4, line=0.8, outer=T)
mtext(expression(bold(A)), side=2, at=0.8, line=0.8, outer=T)

dev.off()



