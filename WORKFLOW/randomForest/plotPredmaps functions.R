
predMultiMap<-function(year, 
                       input.folder=paste("/Users/annakrystalli/Documents/PREDICTED/",
                                          an.ID,"normalised monthly maps/",sep=""),
                       output.folder=paste("~/Documents/PREDICTED/",an.ID,"images/",sep=""),
                       spp, spp.col, 
                       an.ID=NULL, an.IDCV, thresh="PredPrev=Obs"){
  
  require(png)
  require(stringr)
  
  
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                              an.IDCV,"1/forests/thresholds.Rdata", sep=""))
  
  dir.create(output.folder, 
             showWarnings = F)
  
  filenames<-grep(year, list.files(path=paste(input.folder, spp, "/night/",
                                              sep=""),all.files = FALSE), value=TRUE)
  
  img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")
  
  #Load geo.matrix
  load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
  lon<-c(geo.matrix[1,,1],11.01279)
  lat<-c(rev(geo.matrix[,1,2]), 61.00876)
  
  
  
  time.split<-matrix(as.numeric(unlist(strsplit(filenames, "-"))), 
                     nrow=length(filenames), ncol=3, byrow=TRUE)
  month.v<-sort(time.split[,2])
  
  dir.create(paste(output.folder,spp, sep=""), showWarnings = F)
  dir.create(paste(output.folder,spp,"/",thresh, sep=""), showWarnings = F)
  png(file = paste(output.folder,spp,"/",thresh, "/", year, ".png", sep=""), 
      width=150*8.3, height=150*11.7, pointsize=14)
  
  layout(matrix(data=c(1:12,13,13,13), nrow=5, ncol=3, byrow=TRUE), widths=c(1,1,1,1,1), 
         heights=c(10,10,10,10,1.5))
  
  
  par( las=1, mar=c(2, 3, 2, 2) + 0.1, col.axis="darkslategray", 
       family="Helvetica", font.axis=2, oma=c(0.2,0.2,3,0.2), mgp=c(4,0.6,0) )
  
  
  for (i in 1:12){  
    if(i%in%month.v){
      m<-which(time.split[,2]==i, arr.ind=TRUE)
      load(paste(input.folder,spp, "/night/",
                 filenames[m],sep=""))
      
      if(any(preds$pred.oc<0)){preds$pred.oc<-preds$pred.oc+1}
      
      preds$pred.oc<-as.numeric(preds$pred.oc>=THRESH.spp[[spp]][thresh])
      x.pred<-preds
      
      pred.oc<-preds$pred.oc
      
      x.pred.oc<-x.pred[pred.oc==0,1:2]  
      max.x<-max(x.pred$pred.ac*x.pred$pred.oc)
      pred.map<-matrix(max.x+1, ncol=926, nrow=1112)
      pred.map[img==1]<-max.x+2  
      pred.map[cbind(x.pred.oc$r,x.pred.oc$c)]<-0
      cols<-NULL
      
      if(sum(pred.oc)>0){
      preds<-preds$pred.ac 
      x.pred<-x.pred[pred.oc==1,1:2]
      y.pred<-preds[pred.oc==1]
      pred.map[cbind(x.pred$r,x.pred$c)]<-y.pred
      cols<-spp.col[which(1:5 %in% preds)]}else{cols<-spp.col[1]}
      
      cols<-c(cols, "white")
      cols<-c(cols, "black")
     
      
      image(z=t(pred.map[dim(pred.map)[1]:1,]),
            x=lon, y=lat, main=i, cex.main=1.8,
            col=cols, xlab="", ylab="", cex.axis=1)
      box("plot", lwd=1.5)}else{frame()}}
  
  
  
  K=5
  
  par(mar=c(2, 3, 0, 2) + 0.1, font.lab=2, mgp=c(4,0.6,0))
  
  scale<-matrix(rep(1:5, each=as.integer(926/K))+1, 
                nrow=30, 
                ncol=as.integer(926/K)*K,
                byrow=TRUE)
  

  cols<-spp.col[-1]
  
  image(z=t(scale),axes=FALSE,
        col=cols,xlab="", ylab="", cex.lab=1)
  box("plot", lwd=1.5)          
  
  
  
  axis(1, at=seq(0,1-(1/K), length.out=K)+1/(2*K),
       labels=1:K, tick=FALSE, cex.axis=2)
  
  mtext(paste(spp,year, an.ID),side=3,line=-0.2,cex=2,outer=TRUE)
  
  dev.off()
  
}

#_________________________________________________________________________________________________________________________

