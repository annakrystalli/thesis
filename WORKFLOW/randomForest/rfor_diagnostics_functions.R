
corData<-function(plot, an.ID, spp=NULL, dt, lm.diag, plot.only=F, log=F){
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID,sep=""), 
             showWarnings = T)
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forest diagnostics/",sep=""), 
             showWarnings = T)
  
  unNormalise<-function(x, var.id){
    for(var in var.id){
      x.v<-x[,var]
      min<-as.numeric(min.max.tab$min[min.max.tab$var==var])
      max<-as.numeric(min.max.tab$max[min.max.tab$var==var])
      x[,var]<-min+(x.v*(max-min))}
    
    return(x)}
  
  load(file="~/Documents/SATELLITE/Associated data/r files/min.max.ind pre processing file.RData")
  
  ave <- read.csv("~/Documents/CPR/DATA/RAW DATA/Accepted values.csv")
  tv<-c("tem", "centrot")
  ec<-c("chel", "cfin", "metrilu")

  if(is.null(spp)){spp.s<-c("chel", "cfin", "tem", "centrot", "metrilu")}else{spp.s<-spp}
  
 
  lm.s<-NULL
  
for(spp in  spp.s){
  
  dir.create(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.ID, 
                   "forest diagnostics/",spp,sep=""), 
             showWarnings = T)
  
  if(plot.only==F){                          
  load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",spp, 
                  "_train_data.Rdata", sep="")) 
  
  load(file=paste("~/Documents/PREDICTED/",an.ID,"AORS/data/",
                  spp," monthly AOR.Rdata",sep=""))


  if(dt=="oc"){
              levels(y.toc)[levels(y.toc)!="0"]<-"1"
              y.raw<-c(y.oc, y.toc)
              
              yraw<-c(0,1)[y.raw]  
              x.raw<-rbind(x.oc, x.toc) 
            
              xraw<-unNormalise(x.raw, var.id=setdiff(names(x.raw), c("tow", "tow.id")))
              
              id.r<-as.factor(paste(xraw$year,xraw$month, sep="-"))  
              
              raw<-tapply(yraw, INDEX=id.r, FUN=function(x){sum(x)/length(x)})
              
          save(yraw, xraw, raw, id.r, file=paste("~/Documents/PREDICTED/"
                                                 , an.ID,"AORS/data/",
                               spp," monthly raw oc.Rdata",sep=""))}else{
    
              y.raw<-c(y, y.t)
                
              x.raw<-rbind(x, x.t) 
              
              xraw<-unNormalise(x.raw, var.id=setdiff(names(x.raw), c("tow", "tow.id")))
              
              id.r<-as.factor(paste(xraw$year,xraw$month, sep="-"))  
              
              if(spp %in% tv){av<-ave$avt}else{av<-ave$ave}
              
              yraw<-av[y.raw]
              
              raw<-tapply(yraw, INDEX=id.r, FUN="mean") 
    
          save(yraw, xraw, raw, id.r, file=paste("~/Documents/PREDICTED/",
                                                 an.ID,"AORS/data/",
                                                 spp," monthly raw oc.Rdata",sep=""))}
 
  if(dt=="oc"){
          pred<-data.frame(oc=oc, m.id=as.character(paste(yr, mm, sep="-")), 
                             stringsAsFactors=FALSE)}else{
          pred<-data.frame(AB=AB, m.id=as.character(paste(yr, mm, sep="-")), 
                                                stringsAsFactors=FALSE)                      
                             }
  
  #Select only raw & pred overlapping months
    na.tp<-union(setdiff(id.r, pred$m.id),setdiff(pred$m.id, id.r ))
    pred<-pred[as.logical(1-pred$m.id %in% na.tp) ,]
  
    

  
  
  #LM
  
      for.lm<-lm(pred[,1]~raw)
  }
      
  if(plot.only==T){
    load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                           an.ID,"forest diagnostics/",spp,"/lm & pred ",
                                           dt," dat.RData",sep=""))
  }
 
  lm.line<-predict(for.lm, newdata=data.frame(raw=seq(min(raw),max(raw), 
                                                      length.out=20)))
  
      #Plotting parameters 
            ymax<-max(c(raw, pred[,1]))
            ymin<-min(c(raw, pred[,1]))
  
            cols.matrix<-matrix(c( "antiquewhite2",  "darkgoldenrod4",
                                   "honeydew1", "aquamarine4", 
                                   "seashell1","palevioletred4",
                                   "thistle1", "purple3",
                                   "whitesmoke","turquoise4"),
                                nrow=5, ncol=2, byrow=TRUE)
            
            spps=c("chel", "cfin", "tem", "centrot", "metrilu")
            spp.col<-cols.matrix[which(spps==spp, arr.ind=T),]
 #Lm plots 

          if(log==F){
            if(plot==F){png(filename=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                           an.ID,"forest diagnostics/",spp,"/lm plot ",
                                           dt,".png",sep=""),
                            width = 600, height = 600)}
            
            par(bg="black", col="white",font=1, fg="white", col.axis="white",
                col.main="white", col.lab="white")
            
                  plot(raw, pred[,1], col=spp.col[1], bg=spp.col[2], pch=21, cex=0.6,  
                       xlab=paste(dt, " traning set", sep="")
                       ,ylab=paste(dt, " forest output", sep=""),
                       ylim=c(0,ymax), xlim=c(0,ymax), main=spp)
                  abline(a=0, b=1, lty=3)
                  
                  points(seq(0,max(raw), length.out=20),lm.line , 
                         lty=1, type="l",lwd=1.6,col=spp.col[1] )}
  
      if(log==T){
        if(plot==F){png(filename=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                                       an.ID,"forest diagnostics/",spp,"/lm LOG plot ",
                                       dt,".png",sep=""),
                        width = 600, height = 600)}
        
        par(bg="black", col="white",font=1, fg="white", col.axis="white",
            col.main="white", col.lab="white")
        
        l.raw<-log(raw+0.01)
        l.pred<-log(pred[,1]+0.01)
        l.ymax<-max(c(l.raw, l.pred))
        l.ymin<-min(c(l.raw, l.pred))
        
                    plot(l.raw, l.pred, col=spp.col[1], bg=spp.col[2], pch=21, cex=0.6,  
                         xlab=paste(dt, " log traning set", sep="")
                         ,ylab=paste(dt, " log forest output", sep=""),
                         ylim=c(l.ymin,l.ymax), xlim=c(l.ymin,l.ymax), main=spp)
                    abline(a=0, b=1, lty=3)}
        if(plot==F){dev.off()}
  
  
  
  if(lm.diag==T){
  png(filename=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                     an.ID,"forest diagnostics/",spp,"/lm diagn ",
                     dt,".png",sep=""),
      width = 600, height = 600)
 
        par(mfrow=c(2,2))
 
            plot(for.lm)
  
      dev.off()}

  rsq<-cor(raw, pred[,1])  
  lm.s<-c(lm.s, for.lm$coeff,summary(for.lm)$adj.r.squared, rsq)
  
  
  if(plot.only==F){ 

  print(rsq)
  print(spp)
  print(summary(for.lm)$adj.r.squared) 
  
  
  save(for.lm,pred, raw, rsq, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                  an.ID,"forest diagnostics/",spp,"/lm & pred ",
                                    dt," dat.RData",sep=""))}}
  
  ls.tab<-matrix(lm.s,nrow=4, ncol=length(spp.s), 
         dimnames=list(c("a", "b", "adj.rsq", "cor.rsq"), spp.s))

  
  if(plot.only==F){return(ls.tab)}
  }



#_________________________________________________________________________


plotALLcor<-function(an.ID=NULL, plot.only=T, log=F){
  
  
  png(filename=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                     an.ID,"forest diagnostics/all.lm plot.png",sep=""),
      width = 600*5, height = 600*2, pointsize = 28)
  
  par(mfrow=c(2,5))
  
  if(plot.only==F){
    lm.oc<-corData(plot=T, an.ID=an.ID, spp=NULL, dt="oc", lm.diag=F, plot.only=plot.only, 
                   log=log)
    lm.ac<-corData(plot=T, an.ID=an.ID, spp=NULL, dt="ac", lm.diag=F, plot.only=plot.only, 
                   log=log)
    dev.off()
    
    lm.tab<-list(oc=lm.oc, ac=lm.ac)
    
    save(lm.tab, file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                            an.ID,"forest diagnostics/lm table.RData",sep=""))
    
    print(lm.tab)}
  
  if(plot.only==T){
    if(log==F){
    png(filename=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"forest diagnostics/all.lm plot.png",sep=""),
        width = 600*5, height = 600*2, pointsize = 28)
    
    par(mfrow=c(2,5))
    
    
    corData(plot=T, an.ID=an.ID, spp=NULL, dt="oc", lm.diag=F, plot.only=plot.only, log=log)
    corData(plot=T, an.ID=an.ID, spp=NULL, dt="ac", lm.diag=F, plot.only=plot.only, log=log)
    dev.off()}
  
    if(log==T){png(filename=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                       an.ID,"forest diagnostics/all LOG lm plot.png",sep=""),
        width = 600*5, height = 600*2, pointsize = 28)
    
    par(mfrow=c(2,5))
    
    
    corData(plot=T, an.ID=an.ID, spp=NULL, dt="oc", lm.diag=F, plot.only=plot.only, log=F)
    corData(plot=T, an.ID=an.ID, spp=NULL, dt="ac", lm.diag=F, plot.only=plot.only, log=log)
    dev.off()}
  
}}

#_________________________________________________________________________


