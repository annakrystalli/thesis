#Set up
rm(list = ls())
set.seed(seed=1)

require(randomForest)
require(png)

spp<-"metrilu"


#_______________________________________________________________________________________________________

                                  load("~/Documents/TRAINING DATA/Dataset tow splits monthly.RData")
                                

                                  sampleTows()


                                  rforOCSMP<-sampCombineForest(x=x.oc, y=y.oc, m=30)

                                  pred.t<-predict(rforOCSMP, newdata=selectX(x.toc, varnames=v2))
                                  print(table(y.toc, pred.t))


source('~/Documents/WORKFLOW/randomForest/rf_tow_functions.R', chdir = TRUE)

load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",spp, "_train_tow_data.Rdata", sep="")) 

  varnames<-v2
  
  x<-selectX(x, varnames)
  x.oc<-selectX(x.oc, varnames)
  x.t<-selectX(x.t, varnames)
  x.toc<-selectX(x.toc, varnames)
  
  set.seed(seed=1)
  rforOCtow<-randomForest(x=x.oc, y=y.oc, 
                       xtest=x.toc, ytest=y.toc, ntree=200,
                       replace=TRUE, classwt=c(1,1), 
                       sampsize = ceiling(.632*nrow(x)))
  
              cf.m<-rforOCtow$test$confusion
              err.rate<-rforOCtow$test$err.rate[rforOCtow$ntree,1]

              print(rforOCtow$err.rate[rforOCtow$ntree,1])
              print(rforOCtow$confusion)
              print(err.rate)
              print(cf.m) 


                plot(rforOCtow)
                plotOCimp(rforOCtow, spp=spp, plot=TRUE)


rm(rforOCtow)
set.seed(seed=1)
rforOCtowT<-randomForest(x=x.oc, y=y.oc, 
                         keep.forest=TRUE, ntree=200,
                         replace=TRUE, classwt=c(1,1), 
                         sampsize = ceiling(.632*nrow(x)))

            save(rforOCtowT, cf.m, err.rate, file=paste("~/Documents/TRAINING DATA/Models/randomForest/forests/",
                                    spp, "/OCtowforest.Rdata", sep=""))
            rm(rforOCtowT)





  levels(y.t)<-1:max(as.numeric(levels(y)))  
  set.seed(seed=1)
  rforACtow<-randomForest(x=x, y=y, xtest=x.t, ytest=y.t,
                        ntree=200,
                       replace=TRUE, classwt=NULL, 
                       sampsize = ceiling(.632*nrow(x)))
                      
                      cf.mAC<-rforACtow$test$confusion
                      err.rateAC<-rforACtow$test$err.rate[rforACtow$ntree,1]
                      
                      print(rforACtow$err.rate[rforACtow$ntree,1])
                      rforACtow$confusion
                      print(err.rateAC)
                      print(cf.mAC)
                      
                      
                      plot(rforACtow)
                      plotACimp(rforACtow, spp=spp, plot=TRUE)

                      rm(rforACtow)


  set.seed(seed=1)
  rforACtowT<-randomForest(x=x, y=y, keep.forest=TRUE,
                          ntree=200,
                          replace=TRUE, classwt=NULL, 
                          sampsize = ceiling(.632*nrow(x)))
  

save(rforACtowT,  cf.mAC, err.rateAC, file=paste("~/Documents/TRAINING DATA/Models/randomForest/forests/",
                                                 spp, "/ACtowforest.Rdata", sep=""))


rm(rforACtowT)
  
  

  #(classification only) vector error rates of the prediction on the input data, 
  #the i-th element being the (OOB) error rate for all trees up to the i-th.
  
  

  
  rm(list=ls())
  
  
  
  
  
  predMultiMap<-function(year, 
                         input.folder="/Users/annakrystalli/Documents/PREDICTED/normalised monthly maps/",
                         output.folder="~/Documents/PREDICTED/images/",
                         spp, spp.col=c( "antiquewhite2",  "darkgoldenrod4")){
    
    require(png)
    require(stringr)
    
    filenames<-grep(year, list.files(path=paste(input.folder, spp,
                                                sep=""),all.files = FALSE), value=TRUE)
    
    img<-readPNG("~/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")
    
    #Load geo.matrix
    load(file="/Users/annakrystalli/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
    lon<-c(geo.matrix[1,,1],11.01279)
    lat<-c(rev(geo.matrix[,1,2]), 61.00876)
    
    
    
    time.split<-matrix(as.numeric(unlist(strsplit(filenames, "-"))), 
                       nrow=length(filenames), ncol=3, byrow=TRUE)
    month.v<-sort(time.split[,2])
    
    dir.create(paste(output.folder,spp, sep=""))
    png(file = paste(output.folder,spp, "/", year, ".png", sep=""), 
        width=150*8.3, height=150*11.7, pointsize=14)
    
    layout(matrix(data=c(1:12,13,13,13), nrow=5, ncol=3, byrow=TRUE), widths=c(1,1,1,1,1), 
           heights=c(10,10,10,10,1.5))
    
    
    par( las=1, mar=c(2, 3, 2, 2) + 0.1, col.axis="darkslategray", 
         family="Helvetica", font.axis=2, oma=c(0.2,0.2,3,0.2), mgp=c(4,0.6,0) )
    
    
    for (i in 1:12){  
      if(i%in%month.v){
        m<-which(time.split[,2]==i, arr.ind=TRUE)
        load(paste(input.folder,spp, "/",
                   filenames[m],sep=""))
        
        x.pred<-preds
        
        pred.oc<-preds$pred.oc
        preds<-preds$pred.ac 
        
        x.pred.oc<-x.pred[pred.oc==0,1:2]        
        x.pred<-x.pred[pred.oc==1,1:2]
        y.pred<-preds[pred.oc==1]
        
        pred.map<-matrix(13, ncol=926, nrow=1112)
        pred.map[img==1]<-14
        
        pred.map[cbind(x.pred$r,x.pred$c)]<-y.pred
        pred.map[cbind(x.pred.oc$r,x.pred.oc$c)]<-15
        
        K<-12
        FUN.c<-colorRampPalette(spp.col, space="Lab")
        
        cols<-FUN.c(n=12)
        cols<-c(cols, "white")
        cols<-c(cols, "black")
        cols<-c(cols, "cadetblue3")
        
        image(z=t(pred.map[dim(pred.map)[1]:1,]),
              x=lon, y=lat, main=i, cex.main=1.8,
              col=cols, xlab="", ylab="", cex.axis=1)
        box("plot", lwd=1.5)}else{frame()}}
    
    
    
    
    
    par(mar=c(2, 3, 0, 2) + 0.1, font.lab=2, mgp=c(4,0.6,0))
    
    scale<-matrix(rep(1:12, each=as.integer(926/K))+1, 
                  nrow=30, 
                  ncol=as.integer(926/K)*K,
                  byrow=TRUE)
    
    cols<-FUN.c(n=K)
    
    image(z=t(scale),axes=FALSE,
          col=cols,xlab="", ylab="", cex.lab=1)
    box("plot", lwd=1.5)          
    
    
    
    axis(1, at=seq(0,1-(1/K), length.out=K)+1/(2*K),
         labels=1:K, tick=FALSE, cex.axis=2)
    
    mtext(year,side=3,line=-0.2,cex=2,outer=TRUE)
    
    dev.off()
    
  }
  
  
  spp.v<-"chel"
  
  #Pred mMAP........................................................................
  year.s<-1997:2010
  spp.v<-c("cfin","centrot", "tem")
  cols.matrix<-matrix(c("honeydew1", "aquamarine4", 
                        "seashell1","palevioletred4",
                        "thistle1", "purple3"),
                      nrow=3, ncol=2, byrow=TRUE)
  
  for(j in 1:3){
    for(i in 1:length(year.s)){
      
      predMultiMap(year=year.s[i], spp=spp.v[j], spp.col=cols.matrix[j,])
    }}
  